#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Monitoring 1 — Extract Sample Information (Patched: Option B for submitdates)
# Authors: Saskia Wilken (saskia.wilken@uni-hamburg.de, saskia.a.wilken@gmail.com)
# Created: 2025-10-29
# Patch date: 2026-11-03
#
# What this does
# - Reads the 3-sheet ID completeness report create by mon_01:
#   1) complete datasets
#   2) datasets missing questionnaire data (=> cognitive present)
#   3) datasets missing cognitive test data (=> questionnaires present)
# - Writes overview and per-project completeness workbooks.
# - Project 6 has infants, children/parents, and adults. Adults require cognitive
#   tests only: their cogtest IDs are complete without questionnaire data; their
#   dates come from the cogtest submitdates sheet.
#
# Patch summary (Option B):
# - When attaching submit dates to IDs, pick the LAST non-NA value among duplicates
#   for the same ID within a (sample|project) key. This prevents an early NA (e.g.,
#   from cogtest) from masking a valid questionnaire date.
# - Slightly more robust submitdate-sheet reading (tries "submitdates", then
#   "submitdate", then sheet index 2).
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clean up R environment -------------------------------------------------------
rm(list = ls())
cat("\014")

# Install/load packages --------------------------------------------------------
suppressPackageStartupMessages({
  if (!require("dplyr")) { install.packages("dplyr") }; library(dplyr)
  if (!require("tidyr")) { install.packages("tidyr") }; library(tidyr)
  if (!require("readxl")) { install.packages("readxl") }; library(readxl)
  if (!require("openxlsx")) { install.packages("openxlsx") }; library(openxlsx)
  if (!require("purrr")) { install.packages("purrr") }; library(purrr)
  if (!require("stringr")) { install.packages("stringr") }; library(stringr)
})

# -------------------- Helpers & Config --------------------
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  i <- grep("^--file=", cmd_args)
  if (length(i)) return(dirname(normalizePath(sub("^--file=", "", cmd_args[i]))))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- try(rstudioapi::getSourceEditorContext(), silent = TRUE)
    if (!inherits(ctx, "try-error") && nzchar(ctx$path)) return(dirname(normalizePath(ctx$path)))
  }
  normalizePath(getwd())
}

SCRIPT_DIR <- get_script_dir()
DATA_DIR   <- file.path(SCRIPT_DIR, "private_information", "ids_in_all_projects")
PROJ_ROOT  <- file.path(SCRIPT_DIR, "01_project_data")

if (!dir.exists(DATA_DIR)) stop("Directory not found: ", DATA_DIR)
if (!dir.exists(PROJ_ROOT)) dir.create(PROJ_ROOT, recursive = TRUE, showWarnings = FALSE)

TYPES       <- c("cogtest", "questionnaire", "questionnaire_p6")
FILENAME_RE <- "^(.*?)_ids_in_all_projects_(cogtest|questionnaire|questionnaire_p6)\\.xlsx$"
RE_COG      <- "^(?:psytool_info|cogtest)_([A-Za-z0-9_]+)_([1-9])$"
RE_Q        <- "^(?:dat|questionnaire)_([A-Za-z0-9_]+)_([1-9])$"
RE_QP6      <- "^(?:dat|questionnaire)_([A-Za-z0-9_]+)_p([1-9])$"

normalize_sample <- function(s) {
  s <- tolower(trimws(s))
  # NOTE: This maps children_parents/parents to "children" on purpose so that
  # questionnaire (children) and cogtest (children_parents) data for project 6
  # share the same key and can be joined.
  if (s %in% c("children_parents", "parents")) return("children")
  s
}

parse_date_tag <- function(tag) {
  fmts <- c("%Y-%m-%d", "%Y%m%d", "%d-%m-%Y", "%Y.%m.%d")
  for (fmt in fmts) {
    dt <- try(as.POSIXct(tag, format = fmt, tz = "UTC"), silent = TRUE)
    if (!inherits(dt, "try-error") && !is.na(dt)) {
      return(list(key = as.numeric(dt), norm = format(dt, "%Y-%m-%d")))
    }
  }
  if (grepl("^[0-9]+$", tag)) return(list(key = as.numeric(tag), norm = tag))
  list(key = NA_real_, norm = tag)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# -------------------- Discover latest files --------------------
files <- list.files(DATA_DIR, pattern = "_ids_in_all_projects_.*\\.xlsx$", full.names = TRUE)
if (!length(files)) stop("No matching files in ", DATA_DIR)

meta <- tibble(path = files, name = basename(files)) %>%
  mutate(m = str_match(name, FILENAME_RE)) %>%
  filter(!is.na(m[,1])) %>%
  transmute(path, dtype = tolower(m[,3]), date_tag = m[,2]) %>%
  rowwise() %>% mutate(parsed = list(parse_date_tag(date_tag)),
                       date_key = parsed$key, date_norm = parsed$norm) %>%
  ungroup()

coverage <- meta %>%
  group_by(date_norm) %>%
  summarize(types_present = n_distinct(dtype),
            max_key = max(coalesce(date_key, -Inf)), .groups = "drop") %>%
  arrange(desc(types_present), desc(max_key))

best_norm <- coverage$date_norm[1]

chosen <- meta %>%
  filter(date_norm == best_norm) %>%
  group_by(dtype) %>%
  slice_max(order_by = coalesce(date_key, -Inf), with_ties = FALSE) %>%
  ungroup()

if (!"cogtest" %in% chosen$dtype) {
  newest_cog <- meta %>% filter(dtype == "cogtest") %>%
    slice_max(order_by = coalesce(date_key, -Inf), with_ties = FALSE)
  chosen <- bind_rows(chosen, newest_cog)
}

paths <- setNames(chosen$path, chosen$dtype)

cog_date_tag <- chosen %>% filter(dtype == "cogtest") %>% pull(date_tag) %>% .[1]
if (is.na(cog_date_tag)) cog_date_tag <- chosen %>% filter(dtype == "cogtest") %>% pull(date_norm) %>% .[1]

# -------------------- Read IDs and submitdates --------------------
read_if <- function(dtype) if (dtype %in% names(paths)) readxl::read_excel(paths[[dtype]]) else NULL

# PATCH: more robust reading of submit date sheet
read_submit_if <- function(dtype) {
  if (!(dtype %in% names(paths))) return(NULL)
  p <- paths[[dtype]]
  # Try common sheet names first
  for (nm in c("submitdates", "submitdate")) {
    df <- try(readxl::read_excel(p, sheet = nm), silent = TRUE)
    if (!inherits(df, "try-error") && !is.null(df)) return(df)
  }
  # Fallback to sheet index 2
  df <- try(readxl::read_excel(p, sheet = 2), silent = TRUE)
  if (inherits(df, "try-error")) return(NULL)
  df
}

collect_ids_from_cols <- function(df, pattern, map_project = identity, sample_normalizer = normalize_sample) {
  if (is.null(df)) return(list())
  out <- list()
  for (cn in names(df)) {
    m <- str_match(cn, pattern)
    if (is.na(m[1,1])) next
    sample  <- sample_normalizer(m[1,2])
    project <- as.integer(map_project(m[1,3]))
    ids <- df[[cn]] %>% as.character() %>% trimws() %>% .[!is.na(.) & . != ""]
    key <- paste(sample, project, sep = "|")
    if (!length(ids)) next
    out[[key]] <- unique(c(out[[key]] %||% character(), ids))
  }
  out
}

collect_dates_from_cols <- function(ids_df, dates_df, pattern, map_project = identity, sample_normalizer = normalize_sample) {
  if (is.null(ids_df) || is.null(dates_df)) return(list())
  out <- list()
  cols <- intersect(names(ids_df), names(dates_df))
  for (cn in cols) {
    m <- str_match(cn, pattern)
    if (is.na(m[1,1])) next
    sample  <- sample_normalizer(m[1,2])
    project <- as.integer(map_project(m[1,3]))
    ids_col   <- ids_df[[cn]] %>% as.character()
    dates_col <- dates_df[[cn]]
    # Normalize dates to YYYY-MM-DD if possible
    norm <- function(x) {
      if (inherits(x, "Date"))    return(format(x, "%Y-%m-%d"))
      if (inherits(x, "POSIXt"))  return(format(as.Date(x), "%Y-%m-%d"))
      if (is.numeric(x)) {
        d <- try(as.Date(x, origin = "1899-12-30"), silent = TRUE)
        if (!inherits(d, "try-error")) return(format(d, "%Y-%m-%d"))
      }
      as.character(x)
    }
    dates_chr <- vapply(dates_col, function(x){
      if (is.na(x)) return(NA_character_)
      norm(x)
    }, FUN.VALUE = character(1))
    valid <- !is.na(ids_col) & ids_col != ""
    ids_ok <- ids_col[valid]; dts_ok <- dates_chr[valid]
    key <- paste(sample, project, sep = "|")
    names(dts_ok) <- ids_ok
    out[[key]] <- c(out[[key]] %||% c(), dts_ok)
  }
  out
}

cog_df  <- read_if("cogtest")
q_df    <- read_if("questionnaire")
qp6_df  <- read_if("questionnaire_p6")

cog_sub <- read_submit_if("cogtest")
q_sub   <- read_submit_if("questionnaire")
qp6_sub <- read_submit_if("questionnaire_p6")

cog_map <- collect_ids_from_cols(cog_df, RE_COG)
q_map   <- collect_ids_from_cols(q_df, RE_Q)
qp6_map <- collect_ids_from_cols(qp6_df, RE_QP6)

cog_dates  <- collect_dates_from_cols(cog_df,  cog_sub,  RE_COG)
q_dates    <- collect_dates_from_cols(q_df,    q_sub,    RE_Q)
qp6_dates  <- collect_dates_from_cols(qp6_df,  qp6_sub,  RE_QP6)

merge_maps <- function(a, b) {
  keys <- union(names(a), names(b))
  out <- vector("list", length(keys)); names(out) <- keys
  for (k in keys) out[[k]] <- union(a[[k]] %||% character(), b[[k]] %||% character())
  out
}

merge_date_maps <- function(a, b) {
  keys <- union(names(a), names(b))
  out <- setNames(vector("list", length(keys)), keys)
  for (k in keys) {
    out[[k]] <- c(a[[k]] %||% c(), b[[k]] %||% c())
  }
  out
}

q_all    <- merge_maps(q_map, qp6_map)
dates_all <- merge_date_maps(cog_dates, merge_date_maps(q_dates, qp6_dates))

valid_key <- function(k) {
  parts <- strsplit(k, "\\|")[[1]]
  if (length(parts) != 2) return(FALSE)
  p <- suppressWarnings(as.integer(parts[2]))
  !is.na(p) && p >= 1 && p <= 9
}

all_keys <- union(names(cog_map), names(q_all))
all_keys <- all_keys[vapply(all_keys, valid_key, logical(1))]
all_keys <- sort(all_keys)

# PATCH (Option B): choose the LAST non-NA date where multiple entries exist
attach_dates <- function(key, ids_vec) {
  dmap <- dates_all[[key]] %||% c()
  if (!length(dmap)) return(rep(NA_character_, length(ids_vec)))
  # Normalize possible string artifacts like "NA" and whitespace
  nm <- names(dmap)
  vv <- as.character(dmap)
  vv[is.na(vv)] <- NA_character_
  vv <- trimws(vv)
  keep <- !is.na(vv) & vv != "" & tolower(vv) != "na" & tolower(vv) != "nan"
  nm <- nm[keep]; vv <- vv[keep]
  vapply(ids_vec, function(idv) {
    idx <- which(nm == idv)
    if (!length(idx)) return(NA_character_)
    as.character(tail(vv[idx], 1))
  }, FUN.VALUE = character(1))
}

rows_complete   <- list(); rows_missing_q <- list(); rows_missing_c <- list()
for (k in all_keys) {
  parts   <- strsplit(k, "\\|")[[1]]
  sample  <- parts[1]; project <- as.integer(parts[2])
  cog_ids <- cog_map[[k]] %||% character()
  q_ids   <- q_all[[k]] %||% character()
  
  # Completeness follows the data types required by each subproject.
  # p6 adults collect cognitive tests only; questionnaires are not expected.
  cognitive_only <- project == 6L && sample == "adults"
  complete   <- if (cognitive_only) cog_ids else intersect(cog_ids, q_ids)
  missing_q  <- if (cognitive_only) character() else setdiff(cog_ids, q_ids)
  missing_c  <- setdiff(q_ids, cog_ids)
  
  if (length(complete))
    rows_complete[[k]] <- tibble(sample, project,
                                 id = sort(unique(complete)),
                                 submitdate = attach_dates(k, sort(unique(complete))))
  if (length(missing_q))
    rows_missing_q[[k]] <- tibble(sample, project,
                                  id = sort(unique(missing_q)),
                                  submitdate = attach_dates(k, sort(unique(missing_q))))
  if (length(missing_c))
    rows_missing_c[[k]] <- tibble(sample, project,
                                  id = sort(unique(missing_c)),
                                  submitdate = attach_dates(k, sort(unique(missing_c))))
}

df_complete <- if (length(rows_complete)) bind_rows(rows_complete) else tibble(sample=character(), project=integer(), id=character(), submitdate=character())
df_miss_q   <- if (length(rows_missing_q)) bind_rows(rows_missing_q) else tibble(sample=character(), project=integer(), id=character(), submitdate=character())
df_miss_c   <- if (length(rows_missing_c)) bind_rows(rows_missing_c) else tibble(sample=character(), project=integer(), id=character(), submitdate=character())

# -------------------- Write OVERVIEW workbook (includes submitdate) --------------------
overview_name <- sprintf("%s_id_completeness_report.xlsx", cog_date_tag)
overview_path <- file.path(DATA_DIR, overview_name)

openxlsx::write.xlsx(
  list(
    complete = df_complete,
    missing_questionnaire = df_miss_q,
    missing_cogtest = df_miss_c
  ),
  file = overview_path,
  overwrite = TRUE
)

# -------------------- Write per-project workbooks (NO submitdate) --------------------
write_project_workbook <- function(prj, df_complete, df_miss_q, df_miss_c) {
  if (prj == 1) return(invisible(NULL))
  sub_complete <- df_complete %>% filter(project == prj) %>% select(-submitdate)
  sub_miss_q   <- df_miss_q   %>% filter(project == prj) %>% select(-submitdate)
  sub_miss_c   <- df_miss_c   %>% filter(project == prj) %>% select(-submitdate)
  
  prj_dir  <- file.path(PROJ_ROOT, sprintf("%d_backbone", prj))
  if (!dir.exists(prj_dir)) dir.create(prj_dir, recursive = TRUE, showWarnings = FALSE)
  prj_name <- sprintf("%s_project_%d_id_completeness.xlsx", cog_date_tag, prj)
  prj_path <- file.path(prj_dir, prj_name)
  
  wb <- createWorkbook()
  addWorksheet(wb, "complete")
  addWorksheet(wb, "missing_questionnaire")
  addWorksheet(wb, "missing_cogtest")
  writeData(wb, "complete", sub_complete)
  writeData(wb, "missing_questionnaire", sub_miss_q)
  writeData(wb, "missing_cogtest", sub_miss_c)
  saveWorkbook(wb, prj_path, overwrite = TRUE)
  invisible(prj_path)
}

invisible(lapply(1:9, write_project_workbook, df_complete=df_complete, df_miss_q=df_miss_q, df_miss_c=df_miss_c))

cat("Overview (with submitdate):", overview_path, "\n")

# -------------------- Optional: drop obvious placeholder IDs before writing ----
# If you decide to exclude placeholder/test IDs (e.g., 0, 1, 1001, 69999), you
# can uncomment the lines below before writing the workbooks.
# is_placeholder_id <- function(x) {
#   # Treat non-digits or known sentinel-like numbers as placeholders
#   !grepl("^[0-9]+$", x) | x %in% c("0","1","1001","69999")
# }
# df_complete <- df_complete %>% filter(!is_placeholder_id(id))
# df_miss_q   <- df_miss_q   %>% filter(!is_placeholder_id(id))
# df_miss_c   <- df_miss_c   %>% filter(!is_placeholder_id(id))
