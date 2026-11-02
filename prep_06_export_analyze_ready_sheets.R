# --- prep_06_export_analyze_ready_sheets.R ----------------------------------------------
# FOR: Export HiTOP-mapped + COMPLETE questionnaire-only inputs for factor analysis
# Date: 2026-10-09
#
# Assumes:
#   - Item-level clean master file in: ROOT/02_cleaned/<sample>/
#   - Keys in:                        ROOT/02_cleaned/keys/
#   - Item Information in:           ROOT/information/
#   - Internal consistency helper in:
#         distribution_of_backbone_scores_and_internal_consistency/
#
# Exports:
#   C) <input-date>_<samples>_stratification_info.xlsx
#      - project, age, gender, group (from clean master)
#
#   HiTOP (Item Information-based):
#   A) <input-date>_<samples>_HiTOP_items.xlsx
#      - ONLY item columns that have ANY HiTOP mapping in Item Information
#   B) <input-date>_<samples>_HiTOP_subscales.xlsx
#      - existing score_* columns created by prep05 for the SAME HiTOP-mapped scales
#      - CAPE positive Schlier2015 factors: both _sum and _mean columns
#        (frequency only; 0-3 item coding)
#
#   COMPLETE (questionnaire-only, explicitly configured below):
#   D) <input-date>_<samples>_complete_items.xlsx
#      - ALL item columns belonging to CFG$questionnaire_scales, including FHS
#        plus fhs_*/qc_fhs_* outputs (direct-name/contact/free-text FHS fields
#        are privacy-filtered)
#   E) <input-date>_<samples>_complete_subscales.xlsx
#      - existing score_* columns plus the fhs_* and qc_fhs_* outputs from prep05
#   F) <input-date>_<samples>_complete_subscales_enriched.xlsx
#      - complete analysis input containing questionnaire items, score_* columns,
#        z-scores, demographics, project/group information and FHS/QC outputs
#      - copied unchanged to
#        ROOT/01_project_data/all_projects_backbone/derivatives/questionnaire/
#      - split into one adults or adolescents workbook per project-specific
#        questionnaire folder
#
# Optional filtered exports:
#   same filenames with suffix _lt020 (or whatever threshold tag applies)
#   -> items removed based on poor factor loadings
#   -> scale/subscale scores recomputed from filtered item set
#   -> CAPE positive Schlier2015 factors retain their fixed literature item sets
#
# Run:
#   Rscript export_hitop_analysis_inputs.R --samples=adults,adolescents
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
cat("\014")

# ---- Packages ----------------------------------------------------------------
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readxl", "readr", "writexl", "janitor", "dplyr", "stringr", "tibble",
  "purrr", "fs", "glue", "rstudioapi", "jsonlite"
))

# ---- CONFIG ------------------------------------------------------------------
CFG <- list(
  samples = c("adults", "adolescents"),
  export_hitop = TRUE,
  export_complete = TRUE,
  export_loading_filtered = TRUE,
  export_enriched = TRUE,
  export_questionnaire_copies = TRUE,
  questionnaire_copy_projects = 2:9,
  loading_threshold = 0.30,   # make more aggressive by increasing this, e.g. 0.30 / 0.35
  
  questionnaire_scales = c(
    "IDAS", "CAPE", "AQ", "SUQ", "ASRS", "BISBAS", "IUS", "APS",
    "TICS", "CTQ", "MAP-SR", "FHSfamilytree"
  ),
  # FHS contains a mixture of numeric confidence ratings, Y/N fields and text.
  # Preserve its source types in COMPLETE item exports instead of coercing all
  # fields to numeric.
  non_numeric_item_scales = c("FHSfamilytree"),
  # Direct names/contact fields and unstructured free text are not appropriate
  # in analysis-ready workbooks. Structured FHS items and derived FHS fields
  # remain available.
  complete_excluded_item_patterns = c(
    "^parents(001|002)$", "names", "contact", "^relativesinfo$",
    "^fhsopentext$", "^ownpsychdiagnother$"
  ),
  complete_auxiliary_column_pattern = paste0(
    "^(fhs_(any_response|num_|parents_|siblings_|children_|relatives_|",
    "prop_|all_diagnoses_|treated_diagnoses_)|qc_fhs_)"
  ),
  enrichment_demographics_scale = "demographics",
  enrichment_date_candidates = c("date", "datum", "startdate", "start_date", "submit_date", "submitted_at", "enddate", "end_date"),
  enrichment_group_candidates = c("group", "paper_group", "clinical_group", "participant_group", "case_control", "patient_hc"),
  clean_master = NULL,
  item_info = NULL,
  score_total_override = c(),
  # Derived score columns that do not correspond one-to-one to an Item
  # Information subscale and therefore cannot be inferred from the keys.
  derived_score_specs = data.frame(
    scale = "SUQ",
    subscale = "illegal-drugs",
    col = "score_suq__illegal_drugs",
    stringsAsFactors = FALSE
  )
)

# ---- Helpers -----------------------------------------------------------------
add_z_score_columns <- function(df,
                                score_pattern = "^score_",
                                z_prefix = "z_") {
  score_cols <- grep(score_pattern, names(df), value = TRUE)
  score_cols <- score_cols[!startsWith(score_cols, z_prefix)]
  
  if (!length(score_cols)) return(df)
  
  for (cc in score_cols) {
    x <- suppressWarnings(as.numeric(df[[cc]]))
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    
    z_col <- paste0(z_prefix, cc)
    df[[z_col]] <- if (is.finite(s) && s > 0) {
      (x - m) / s
    } else {
      NA_real_
    }
  }
  
  df
}

make_export_sheets <- function(x, combined_sheet = "combined", z_scores = FALSE) {
  x <- x[!vapply(x, is.null, logical(1))]
  
  if (isTRUE(z_scores)) {
    x <- purrr::map(x, add_z_score_columns)
  }
  
  combined <- dplyr::bind_rows(x)
  
  if (isTRUE(z_scores)) {
    combined <- add_z_score_columns(combined)
  }
  
  x[[combined_sheet]] <- combined
  x
}

`%||%` <- function(x, y) if (!is.null(x) && !is.na(x) && nzchar(x)) x else y

script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    filepath <- sub(file_arg, "", args[grep(file_arg, args)])
    if (length(filepath) == 1) return(normalizePath(dirname(filepath), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

parse_args <- function(args) {
  out <- list()
  for (a in args) {
    if (grepl("^--", a)) {
      kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
      key <- kv[1]
      val <- if (length(kv) >= 2) kv[2] else NA_character_
      out[[key]] <- val
    }
  }
  out
}

normalize_sample_case <- function(sample) {
  stringr::str_replace_all(stringr::str_to_title(sample), "_", " ")
}

extract_date <- function(x) {
  d <- stringr::str_match(x, "(\\d{4}-\\d{2}-\\d{2})")[,2]
  suppressWarnings(as.Date(d))
}

latest_file_by_pattern <- function(dir, pattern, recurse = FALSE) {
  files_all <- fs::dir_ls(dir, type = "file", recurse = recurse, fail = FALSE)
  files <- files_all[grepl(pattern, basename(files_all))]
  if (!length(files)) return(NA_character_)
  dts <- extract_date(basename(files))
  if (all(is.na(dts))) {
    info <- file.info(files)
    return(files[order(info$mtime, decreasing = TRUE)][1])
  }
  files[order(dts, decreasing = TRUE)][1]
}

latest_clean_master_for_sample <- function(root, sample) {
  folder <- fs::path(root, "02_cleaned", sample)
  patt <- paste0(
    "^\\d{4}-\\d{2}-\\d{2}_",
    sample,
    "_clean_master\\.xlsx$"
  )

  files_all <- fs::dir_ls(folder, type = "file", recurse = FALSE, fail = FALSE)
  files <- files_all[grepl(patt, basename(files_all))]
  if (!length(files)) return(NA_character_)

  dts <- extract_date(basename(files))
  valid <- !is.na(dts)
  files <- files[valid]
  dts <- dts[valid]
  if (!length(files)) return(NA_character_)

  newest_date <- max(dts)
  candidates <- files[dts == newest_date]
  if (length(candidates) == 1L) return(candidates)

  info <- file.info(candidates)
  candidates[which.max(info$mtime)]
}

dated_master_input_date <- function(path) {
  date_value <- extract_date(basename(path))
  if (length(date_value) != 1L || is.na(date_value)) {
    stop(
      "Could not extract a YYYY-MM-DD input date from master file: ",
      path,
      call. = FALSE
    )
  }
  format(date_value, "%Y-%m-%d")
}

latest_scored_master_for_sample <- function(root, sample, input_date, suffix = NULL) {
  folder <- fs::path(root, "02_cleaned", sample)
  
  fname <- if (is.null(suffix) || !nzchar(suffix)) {
    paste0(input_date, "_", sample, "_clean_master_scored.xlsx")
  } else {
    paste0(input_date, "_", sample, "_clean_master_scored_", suffix, ".xlsx")
  }
  
  cand <- fs::path(folder, fname)
  if (fs::file_exists(cand)) return(cand)
  
  NA_character_
}

latest_iteminfo_for_sample <- function(root, sample) {
  info_dir <- fs::path(root, "information")
  SampleCap <- normalize_sample_case(sample)
  patt <- glue::glue("^\\d{{4}}-\\d{{2}}-\\d{{2}}_Item_Information_{SampleCap}\\.xlsx$")
  latest_file_by_pattern(info_dir, patt)
}

normalize_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+", "") %>%
    stringr::str_replace_all("\\[|\\]", "") %>%
    stringr::str_replace_all("[^a-z0-9_]+", "")
}

safe_score_name <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}

is_nonempty <- function(x) {
  x0 <- trimws(as.character(x))
  !is.na(x0) & nzchar(x0) & tolower(x0) != "na"
}

pick_first <- function(cands, nms) {
  hit <- cands[cands %in% nms]
  if (length(hit)) hit[1] else NA_character_
}

resolve_iteminfo_with_fallback <- function(root, sample, fallback_sample = "adults") {
  ii_path <- latest_iteminfo_for_sample(root, sample)
  if (!is.na(ii_path) && fs::file_exists(ii_path)) return(ii_path)
  
  fb <- latest_iteminfo_for_sample(root, fallback_sample)
  if (!is.na(fb) && fs::file_exists(fb)) {
    message("ItemInfo missing for sample '", sample, "'. Falling back to '", fallback_sample, "': ", fb)
    return(fb)
  }
  NA_character_
}

make_threshold_tag <- function(x) {
  paste0("lt", gsub("\\.", "", formatC(x, format = "f", digits = 2)))
}

sample_tag <- function(samples) {
  paste(samples, collapse = "_")
}

read_master_robust <- function(master_path, default_delim = ";", clean_column_names = TRUE) {
  extension <- tolower(fs::path_ext(master_path))

  if (identical(extension, "xlsx")) {
    out <- suppressMessages(readxl::read_excel(master_path)) %>%
      tibble::as_tibble()
    # Step 05 writes subscale names with a double underscore (score_scale__subscale).
    # janitor::clean_names() collapses that delimiter, so preserve scored headers.
    if (isTRUE(clean_column_names)) out <- janitor::clean_names(out)
    return(out)
  }

  if (!identical(extension, "csv")) {
    stop("Unsupported master file type: ", master_path, call. = FALSE)
  }

  master_csv <- master_path
  first_line <- readr::read_lines(master_csv, n_max = 1)
  
  delim <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) {
    sub("^sep=", "", first_line, ignore.case = TRUE)
  } else {
    default_delim
  }
  
  skip_n <- if (length(first_line) && grepl("^sep=", first_line, ignore.case = TRUE)) 1L else 0L
  
  suppressMessages(
    readr::read_delim(
      master_csv,
      delim = delim,
      skip = skip_n,
      show_col_types = FALSE,
      locale = readr::locale(
        encoding = "UTF-8",
        decimal_mark = if (identical(delim, ";")) "," else ".",
        grouping_mark = if (identical(delim, ";")) "." else ","
      )
    )
  ) %>%
    tibble::as_tibble()
}

# ---- Keys / scoring helpers --------------------------------------------------

is_cape_positive_schlier2015_subscale <- function(scale, subscale) {
  !is.na(scale) & toupper(trimws(as.character(scale))) == "CAPE" &
    !is.na(subscale) & startsWith(tolower(trimws(as.character(subscale))),
                                 "positive_schlier2015_")
}

drop_flagged_items_from_keys <- function(keys, flagged_tbl) {
  if (is.null(flagged_tbl) || !nrow(flagged_tbl)) return(keys)
  
  flags <- flagged_tbl %>%
    dplyr::mutate(
      level_norm = tolower(trimws(as.character(.data$level))),
      scale_norm = toupper(trimws(as.character(.data$scale))),
      subscale_norm = tolower(trimws(as.character(.data$subscale))),
      item_norm = normalize_id(.data$item)
    )
  
  remove_flagged <- function(items, level, scale, subscale = NA_character_) {
    items <- as.character(items)
    # Match Step 05: literature-defined factors keep all their frequency items.
    if (identical(level, "subscale") &&
        is_cape_positive_schlier2015_subscale(scale, subscale)) return(items)
    
    relevant_flags <- flags %>%
      dplyr::filter(
        .data$level_norm == tolower(level),
        .data$scale_norm == toupper(trimws(as.character(scale)))
      )
    
    if (identical(level, "subscale")) {
      sub_norm <- tolower(trimws(as.character(subscale)))
      
      relevant_flags <- relevant_flags %>%
        dplyr::filter(
          !is.na(.data$subscale_norm),
          .data$subscale_norm == sub_norm
        )
    }
    
    drop_norm <- unique(relevant_flags$item_norm)
    
    items[!(normalize_id(items) %in% drop_norm)]
  }
  
  if (!is.null(keys$items_by_scale) && nrow(keys$items_by_scale)) {
    keys$items_by_scale <- keys$items_by_scale %>%
      dplyr::mutate(
        items = purrr::map2(
          .data$items,
          .data$scale,
          ~ remove_flagged(
            items = .x,
            level = "scale",
            scale = .y
          )
        )
      )
  }
  
  if (!is.null(keys$items_by_subscale) && nrow(keys$items_by_subscale)) {
    keys$items_by_subscale <- keys$items_by_subscale %>%
      dplyr::mutate(
        items = purrr::pmap(
          list(.data$items, .data$scale, .data$subscale),
          function(items, scale, subscale) {
            remove_flagged(
              items = items,
              level = "subscale",
              scale = scale,
              subscale = subscale
            )
          }
        )
      )
  }
  
  keys
}

# ---- Flag-helper handling ----------------------------------------------------
latest_flag_helper <- function(root, combined_label, threshold_tag, input_date) {
  base_dir <- fs::path(
    root, "out", "internal_data_analysis",
    "distribution_of_backbone_scores_and_internal_consistency",
    paste0(input_date, "_", combined_label)
  )
  patt <- paste0("^", combined_label, "_flagged_items_", threshold_tag, "\\.xlsx$")
  latest_file_by_pattern(base_dir, patt, recurse = FALSE)
}

read_flagged_items <- function(path, dataset_label, threshold_value) {
  if (is.na(path) || !fs::file_exists(path)) return(tibble::tibble())
  
  x <- suppressMessages(readxl::read_excel(path, sheet = "flagged_items")) %>%
    janitor::clean_names()
  
  req <- c("dataset", "level", "scale", "subscale", "item", "loading")
  if (!all(req %in% names(x))) return(tibble::tibble())
  
  to_logical_flag <- function(z) {
    if (is.logical(z)) return(z)
    if (is.numeric(z)) return(z != 0)
    z0 <- tolower(trimws(as.character(z)))
    z0 %in% c("true", "t", "1", "yes", "y")
  }
  
  x <- x %>%
    dplyr::mutate(
      dataset = as.character(.data$dataset),
      level = as.character(.data$level),
      scale = as.character(.data$scale),
      subscale = as.character(.data$subscale),
      item = as.character(.data$item),
      loading = suppressWarnings(as.numeric(.data$loading))
    )
  
  if ("flagged_for_removal" %in% names(x)) {
    x$flagged_for_removal <- to_logical_flag(x$flagged_for_removal)
  } else {
    x$flagged_for_removal <- !is.na(x$loading) & abs(x$loading) < threshold_value
  }
  
  x %>%
    dplyr::filter(
      .data$dataset == dataset_label,
      .data$flagged_for_removal %in% TRUE
    ) %>%
    dplyr::distinct(.data$dataset, .data$level, .data$scale, .data$subscale, .data$item, .keep_all = TRUE)
}

# ---- Main --------------------------------------------------------------------
ROOT <- script_dir()
ARGS <- parse_args(commandArgs(trailingOnly = TRUE))

if (is.null(ARGS$samples) && !is.null(ARGS$sample) && nzchar(ARGS$sample)) {
  ARGS$samples <- ARGS$sample
}

OUT_DIR <- fs::path(ROOT, "03_analysis_input")
fs::dir_create(OUT_DIR)

samples_cli <- ARGS$samples %||% NA_character_
samples <- if (!is.na(samples_cli) && nzchar(samples_cli)) {
  strsplit(samples_cli, ",", fixed = TRUE)[[1]] |> trimws() |> tolower()
} else {
  CFG$samples |> tolower()
}

combined_label <- sample_tag(samples)
threshold_tag  <- make_threshold_tag(CFG$loading_threshold)

message("Samples:    ", paste(samples, collapse = ", "))
message("Sample tag: ", combined_label)
message("Output dir: ", OUT_DIR)
message("COMPLETE questionnaire scales: ", paste(CFG$questionnaire_scales, collapse = ", "))

ITEM_INFO <- (ARGS$item_info %||% CFG$item_info) %||%
  resolve_iteminfo_with_fallback(ROOT, "adults", fallback_sample = "adults")
stopifnot(!is.na(ITEM_INFO), fs::file_exists(ITEM_INFO))
message("ItemInfo:   ", ITEM_INFO)

ii <- readxl::read_excel(ITEM_INFO) |>
  janitor::clean_names()

if (!"item" %in% names(ii))  stop("Item Information is missing an 'item' column after clean_names().")
if (!"scale" %in% names(ii)) stop("Item Information is missing a 'scale' column after clean_names().")
if (!"subscale" %in% names(ii)) ii$subscale <- NA_character_

ii <- ii |>
  dplyr::mutate(
    item     = as.character(.data$item),
    scale    = as.character(.data$scale),
    subscale = as.character(.data$subscale)
  ) |>
  dplyr::filter(is_nonempty(.data$item), is_nonempty(.data$scale))

# -----------------------------
# Per-sample loader
# -----------------------------
read_one_sample_master <- function(sample) {
  cm <- (ARGS$clean_master %||% CFG$clean_master)
  master_path <- if (!is.null(cm) && !is.na(cm) && nzchar(cm)) {
    cm
  } else {
    latest_clean_master_for_sample(ROOT, sample)
  }
  
  if (is.na(master_path) || !fs::file_exists(master_path)) {
    stop(
      "Missing dated Step 03 clean master for sample '", sample,
      "'. Expected YYYY-MM-DD_", sample,
      "_clean_master.xlsx in ", fs::path(ROOT, "02_cleaned", sample),
      call. = FALSE
    )
  }

  input_date <- dated_master_input_date(master_path)
  keys_path <- fs::path(
    ROOT,
    "02_cleaned",
    "keys",
    paste0(input_date, "_", sample, "_keys.rds")
  )
  if (!fs::file_exists(keys_path)) {
    stop(
      "Missing Step 03 keys matching input date ", input_date,
      " for sample '", sample, "'. Looked for: ", keys_path,
      call. = FALSE
    )
  }
  
  d <- read_master_robust(master_path)
  
  scored_full_path <- latest_scored_master_for_sample(
    ROOT,
    sample,
    input_date = input_date
  )
  if (is.na(scored_full_path) || !fs::file_exists(scored_full_path)) {
    stop(
      "Missing Step 05 unfiltered scored master matching input date ",
      input_date, " for sample '", sample, "'. Looked for: ",
      scored_full_path, ". Run Step 05 first.",
      call. = FALSE
    )
  }
  
  scored_filtered_path <- latest_scored_master_for_sample(
    ROOT,
    sample,
    input_date = input_date,
    suffix = threshold_tag
  )
  
  scored_combined_suffix <- paste0(threshold_tag, "_combined")
  scored_combined_path <- latest_scored_master_for_sample(
    ROOT,
    sample,
    input_date = input_date,
    suffix = scored_combined_suffix
  )
  
  d_scored_full <- read_master_robust(scored_full_path, clean_column_names = FALSE)
  
  d_scored_filtered <- if (!is.na(scored_filtered_path) && fs::file_exists(scored_filtered_path)) {
    read_master_robust(scored_filtered_path, clean_column_names = FALSE)
  } else {
    NULL
  }
  
  d_scored_combined <- if (!is.na(scored_combined_path) && fs::file_exists(scored_combined_path)) {
    read_master_robust(scored_combined_path, clean_column_names = FALSE)
  } else {
    NULL
  }
  
  id_col <- pick_first(c("vp_id", "vp", "vpid", "participant_id", "participantid", "id"), names(d))
  if (is.na(id_col)) {
    stop("Could not find an ID column in master for sample '", sample, "'.", call. = FALSE)
  }
  
  list(
    sample = sample,
    input_date = input_date,
    master_path = master_path,
    keys_path = keys_path,
    d = d,
    id_col = id_col,
    keys = readRDS(keys_path),
    d_scored_full = d_scored_full,
    d_scored_filtered = d_scored_filtered,
    d_scored_combined = d_scored_combined,
    scored_full_path = scored_full_path,
    scored_filtered_path = scored_filtered_path,
    scored_combined_path = scored_combined_path
  )
}

masters <- purrr::map(samples, read_one_sample_master)

master_dates <- suppressWarnings(as.Date(
  purrr::map_chr(masters, "input_date")
))
if (any(is.na(master_dates))) {
  stop("Could not determine the input date for every selected sample.", call. = FALSE)
}
output_date <- format(max(master_dates), "%Y-%m-%d")
message("Output date: ", output_date)

# -----------------------------
# Build stratification tables
# -----------------------------
normalize_project_number <- function(x) {
  x_norm <- x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace("^project[ _-]*", "") %>%
    stringr::str_replace("^p[ _-]*", "")

  x_num <- suppressWarnings(as.numeric(x_norm))
  out <- suppressWarnings(as.integer(x_num))
  out[is.na(x_num) | x_num != out | !(out %in% 2:9)] <- NA_integer_
  out
}

normalize_group_label <- function(group, project) {
  group_chr <- stringr::str_trim(as.character(group))
  group_chr[!is.na(group_chr) & !nzchar(group_chr)] <- NA_character_
  group_norm <- group_chr %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")

  out <- dplyr::case_when(
    group_norm %in% c("hc", "healthy_control", "healthy_controls", "control", "controls") ~ "HC",
    group_norm %in% c("patient", "patients", "pat", "clinical", "case", "cases", "pp", "chr", "ar", "at_risk", "with_diagnosis") ~ "with_diagnosis",
    TRUE ~ group_chr
  )

  project_number <- normalize_project_number(project)
  out[is.na(out) & !is.na(project_number) & project_number %in% c(3L, 4L, 5L, 6L)] <- "HC"
  out
}

make_strat <- function(obj) {
  d <- obj$d; id_col <- obj$id_col; sample <- obj$sample
  proj_col   <- pick_first(c("project", "p"), names(d))
  age_col    <- pick_first(c("age_years", "age"), names(d))
  gender_col <- pick_first(c("gender"), names(d))
  group_col  <- pick_first(CFG$enrichment_group_candidates, names(d))
  project <- if (!is.na(proj_col)) as.character(d[[proj_col]]) else rep(NA_character_, nrow(d))
  group <- if (!is.na(group_col)) as.character(d[[group_col]]) else rep(NA_character_, nrow(d))
  
  tibble::tibble(
    sample  = sample,
    !!id_col := as.character(d[[id_col]]),
    project = project,
    age     = if (!is.na(age_col))    suppressWarnings(as.numeric(d[[age_col]])) else NA_real_,
    gender  = if (!is.na(gender_col)) as.character(d[[gender_col]]) else NA_character_,
    group   = normalize_group_label(group, project)
  )
}

strat_list <- purrr::map(masters, make_strat)
strat_combined <- dplyr::bind_rows(strat_list)

out_strat <- fs::path(OUT_DIR, glue::glue("{output_date}_{combined_label}_stratification_info.xlsx"))
writexl::write_xlsx(list(strat_all = strat_combined), out_strat)
message("Wrote: ", out_strat)

# -----------------------------
# Export builders
# -----------------------------
export_items_tbl <- function(d, id_col, ii_rows,
                             include_complete_auxiliary = FALSE,
                             dataset_label = NA_character_) {
  col_map <- tibble::tibble(orig = names(d), item_norm = normalize_id(names(d))) |>
    dplyr::distinct(.data$item_norm, .keep_all = TRUE)
  
  items_keep_norm <- unique(normalize_id(ii_rows$item))
  item_cols <- col_map |>
    dplyr::filter(.data$item_norm %in% items_keep_norm) |>
    dplyr::pull(.data$orig) |>
    unique()
  
  if (!length(item_cols)) return(NULL)

  non_numeric_items_norm <- ii_rows %>%
    dplyr::filter(.data$scale %in% CFG$non_numeric_item_scales) %>%
    dplyr::pull("item") %>%
    normalize_id() %>%
    unique()

  out <- tibble::tibble(!!id_col := as.character(d[[id_col]]))
  for (column in item_cols) {
    if (normalize_id(column) %in% non_numeric_items_norm) {
      out[[column]] <- d[[column]]
    } else {
      out[[column]] <- suppressWarnings(as.numeric(d[[column]]))
    }
  }

  if (isTRUE(include_complete_auxiliary)) {
    auxiliary_cols <- grep(
      CFG$complete_auxiliary_column_pattern,
      names(d),
      value = TRUE
    )
    required_fhs_cols <- c(
      "fhs_any_response",
      "fhs_num_diagnoses_self",
      "fhs_num_treated_diagnoses_self",
      "fhs_parents_with_diagnosis",
      "fhs_siblings_with_diagnosis",
      "fhs_children_with_diagnosis",
      "fhs_relatives_total",
      "fhs_relatives_with_diagnosis",
      "fhs_all_diagnoses_self",
      "fhs_treated_diagnoses_self"
    )
    missing_fhs_cols <- setdiff(required_fhs_cols, names(d))
    if (length(missing_fhs_cols)) {
      stop(
        "COMPLETE item export is missing required FHS output columns",
        if (!is.na(dataset_label)) paste0(" [", dataset_label, "]") else "",
        ": ", paste(missing_fhs_cols, collapse = ", "),
        ". Run the updated prep_05_Score_Scales.R first.",
        call. = FALSE
      )
    }
    auxiliary_cols <- setdiff(auxiliary_cols, names(out))
    if (length(auxiliary_cols)) {
      out <- dplyr::bind_cols(
        out,
        d %>% dplyr::select(dplyr::all_of(auxiliary_cols))
      )
    }
  }
  out
}

expected_score_cols_from_keys <- function(
    keys_obj,
    scales,
    total_override = c(),
    derived_score_specs = CFG$derived_score_specs) {
  scales <- unique(as.character(scales))
  scales <- scales[is_nonempty(scales)]
  
  expected_total <- tibble::tibble()
  if (!is.null(keys_obj$items_by_scale) && nrow(keys_obj$items_by_scale)) {
    expected_total <- keys_obj$items_by_scale %>%
      # FHS is exported through its derived fhs_* fields, not a score_fhsfamilytree total.
      dplyr::filter(.data$scale %in% scales, .data$scale != "FHSfamilytree") %>%
      dplyr::mutate(
        level = "scale",
        subscale = NA_character_,
        col = if (length(total_override)) {
          dplyr::if_else(
            .data$scale %in% names(total_override),
            unname(total_override[.data$scale]),
            paste0("score_", safe_score_name(.data$scale))
          )
        } else {
          paste0("score_", safe_score_name(.data$scale))
        }
      ) %>%
      dplyr::select(.data$level, .data$scale, .data$subscale, .data$col)
  }
  
  expected_sub <- tibble::tibble()
  if (!is.null(keys_obj$items_by_subscale) && nrow(keys_obj$items_by_subscale)) {
    expected_sub <- keys_obj$items_by_subscale %>%
      dplyr::filter(.data$scale %in% scales) %>%
      dplyr::filter(is_nonempty(.data$subscale)) %>%
      dplyr::mutate(
        level = "subscale",
        col = paste0("score_", safe_score_name(.data$scale), "__", safe_score_name(.data$subscale))
      ) %>%
      dplyr::select(.data$level, .data$scale, .data$subscale, .data$col) %>%
      dplyr::distinct()

    # Step 05 writes explicit sum and mean columns for the additional CAPE
    # factors, without an unsuffixed score column. Classical scores keep their
    # existing names.
    cape_rows <- is_cape_positive_schlier2015_subscale(
      expected_sub$scale, expected_sub$subscale
    )
    if (any(cape_rows)) {
      expected_cape <- expected_sub[cape_rows, , drop = FALSE]
      expected_sub <- dplyr::bind_rows(
        expected_sub[!cape_rows, , drop = FALSE],
        expected_cape %>% dplyr::mutate(col = paste0(.data$col, "_sum")),
        expected_cape %>% dplyr::mutate(col = paste0(.data$col, "_mean"))
      )
    }
  }
  
  expected_derived <- tibble::tibble()
  if (!is.null(derived_score_specs) && nrow(derived_score_specs)) {
    expected_derived <- tibble::as_tibble(derived_score_specs) %>%
      dplyr::filter(.data$scale %in% scales) %>%
      dplyr::filter(
        is_nonempty(.data$scale),
        is_nonempty(.data$subscale),
        is_nonempty(.data$col)
      ) %>%
      dplyr::mutate(level = "derived_subscale") %>%
      dplyr::select(.data$level, .data$scale, .data$subscale, .data$col)
  }

  dplyr::bind_rows(expected_total, expected_sub, expected_derived) %>%
    dplyr::distinct()
}

export_existing_scores_tbl <- function(d_scored, id_col, keys_obj, scales,
                                       total_override = c(),
                                       dataset_label = NA_character_,
                                       include_complete_auxiliary = FALSE) {
  if (is.null(d_scored)) {
    stop("Score export requested, but scored data is NULL", call. = FALSE)
  }
  
  expected <- expected_score_cols_from_keys(
    keys_obj = keys_obj,
    scales = scales,
    total_override = total_override
  )

  auxiliary_cols <- if (isTRUE(include_complete_auxiliary)) {
    grep(
      CFG$complete_auxiliary_column_pattern,
      names(d_scored),
      value = TRUE
    )
  } else {
    character(0)
  }

  if (isTRUE(include_complete_auxiliary)) {
    required_fhs_cols <- c(
      "fhs_any_response",
      "fhs_num_diagnoses_self",
      "fhs_num_treated_diagnoses_self",
      "fhs_parents_with_diagnosis",
      "fhs_siblings_with_diagnosis",
      "fhs_children_with_diagnosis",
      "fhs_relatives_total",
      "fhs_relatives_with_diagnosis",
      "fhs_all_diagnoses_self",
      "fhs_treated_diagnoses_self"
    )
    missing_fhs_cols <- setdiff(required_fhs_cols, names(d_scored))
    if (length(missing_fhs_cols)) {
      stop(
        "COMPLETE score export is missing required FHS output columns",
        if (!is.na(dataset_label)) paste0(" [", dataset_label, "]") else "",
        ": ", paste(missing_fhs_cols, collapse = ", "),
        ". Run the updated prep_05_Score_Scales.R first.",
        call. = FALSE
      )
    }
  }

  if (!nrow(expected) && !length(auxiliary_cols)) return(NULL)
  
  present <- intersect(expected$col, names(d_scored))
  
  missing_not_created <- expected %>%
    dplyr::filter(!(.data$col %in% present))

  missing_cape <- missing_not_created %>%
    dplyr::filter(is_cape_positive_schlier2015_subscale(.data$scale, .data$subscale))
  if (nrow(missing_cape)) {
    stop(
      "Score export is missing required CAPE Schlier2015 sum/mean columns",
      if (!is.na(dataset_label)) paste0(" [", dataset_label, "]") else "",
      ": ", paste(missing_cape$col, collapse = ", "),
      ". Run the updated prep_05_Score_Scales.R first.",
      call. = FALSE
    )
  }
  
  if (nrow(missing_not_created)) {
    for (i in seq_len(nrow(missing_not_created))) {
      lbl <- if (missing_not_created$level[i] == "scale") {
        as.character(missing_not_created$scale[i])
      } else {
        paste0(missing_not_created$scale[i], " / ", missing_not_created$subscale[i])
      }
      message(
        "Score export: expected ", missing_not_created$level[i], " '", lbl,
        "' was not present in the scored master and will be omitted",
        if (!is.na(dataset_label)) paste0(" [", dataset_label, "]") else "",
        "."
      )
    }
  }
  
  present_nonempty <- character(0)
  for (cc in present) {
    z <- suppressWarnings(as.numeric(d_scored[[cc]]))
    if (!all(is.na(z))) present_nonempty <- c(present_nonempty, cc)
  }
  
  present_nonempty <- unique(present_nonempty)

  if (!length(present_nonempty) && !length(auxiliary_cols)) {
    message(
      "Score export: no non-missing score columns remained",
      if (!is.na(dataset_label)) paste0(" [", dataset_label, "]") else "",
      "."
    )
    return(
      d_scored %>%
        dplyr::transmute(!!id_col := as.character(.data[[id_col]]))
    )
  }
  
  out <- d_scored %>%
    dplyr::transmute(!!id_col := as.character(.data[[id_col]]))

  if (length(present_nonempty)) {
    out <- dplyr::bind_cols(
      out,
      d_scored %>%
        dplyr::transmute(
          dplyr::across(
            dplyr::all_of(present_nonempty),
            ~ suppressWarnings(as.numeric(.x))
          )
        )
    )
  }

  if (length(auxiliary_cols)) {
    out <- dplyr::bind_cols(
      out,
      d_scored %>% dplyr::select(dplyr::all_of(auxiliary_cols))
    )
  }

  out
}


# -----------------------------
# Enrichment helpers
# -----------------------------
add_enriched_suffix <- function(path) {
  sub("\\.xlsx$", "_enriched.xlsx", path)
}

pick_first_col_norm <- function(cands, nms) {
  cand_norm <- normalize_id(cands)
  nms_norm <- normalize_id(nms)
  idx <- match(cand_norm, nms_norm)
  idx <- idx[!is.na(idx)]
  if (length(idx)) nms[idx[1]] else NA_character_
}

make_enrichment_tbl <- function(obj, ii_rows,
                                demographics_scale = CFG$enrichment_demographics_scale) {
  d <- obj$d
  id_col <- obj$id_col
  nms <- names(d)
  col_map <- tibble::tibble(orig = nms, item_norm = normalize_id(nms)) %>%
    dplyr::distinct(.data$item_norm, .keep_all = TRUE)
  
  demo_items_norm <- ii_rows %>%
    dplyr::filter(tolower(trimws(as.character(.data$scale))) == tolower(trimws(demographics_scale))) %>%
    dplyr::pull(.data$item) %>%
    normalize_id() %>%
    unique()
  
  demo_cols <- col_map %>%
    dplyr::filter(.data$item_norm %in% demo_items_norm) %>%
    dplyr::pull(.data$orig) %>%
    unique()
  
  age_col <- pick_first_col_norm(c("age_years", "age"), nms)
  date_col <- pick_first_col_norm(CFG$enrichment_date_candidates, nms)
  project_col <- pick_first_col_norm(c("project", "p"), nms)
  group_col <- pick_first_col_norm(CFG$enrichment_group_candidates, nms)
  project <- if (!is.na(project_col)) as.character(d[[project_col]]) else rep(NA_character_, nrow(d))
  group <- if (!is.na(group_col)) as.character(d[[group_col]]) else rep(NA_character_, nrow(d))
  
  out <- tibble::tibble(
    !!id_col := as.character(d[[id_col]]),
    age_years = if (!is.na(age_col)) suppressWarnings(as.numeric(d[[age_col]])) else NA_real_,
    date = if (!is.na(date_col)) as.character(d[[date_col]]) else NA_character_,
    project = project,
    group = normalize_group_label(group, project)
  )
  
  # Keep demographics item columns from Item Information, but avoid duplicate canonical fields.
  blocked_norm <- normalize_id(c(id_col, "age", "age_years", "date", "datum", "project", "p", "group"))
  demo_cols <- demo_cols[!(normalize_id(demo_cols) %in% blocked_norm)]
  demo_cols <- setdiff(demo_cols, names(out))
  
  if (length(demo_cols)) {
    out <- dplyr::bind_cols(
      out,
      d %>% dplyr::select(dplyr::all_of(demo_cols))
    )
  }
  
  out
}

add_enrichment_to_tbl <- function(tab, obj, ii_rows) {
  if (is.null(tab) || !nrow(tab)) return(tab)
  enrich <- make_enrichment_tbl(obj, ii_rows)
  by_col <- obj$id_col
  
  if (!(by_col %in% names(tab))) {
    stop("Cannot enrich table for sample '", obj$sample, "': ID column '", by_col, "' is missing.", call. = FALSE)
  }
  
  # Do not create .x/.y duplicates if an export already contains one of these columns.
  duplicate_cols <- setdiff(intersect(names(enrich), names(tab)), by_col)
  if (length(duplicate_cols)) {
    enrich <- enrich %>% dplyr::select(-dplyr::all_of(duplicate_cols))
  }

  # Tables created from the same master retain identical row order. Bind those
  # rows directly so duplicate participant IDs cannot cause a many-to-many join.
  if (nrow(tab) == nrow(enrich)) {
    tab_ids <- as.character(tab[[by_col]])
    enrich_ids <- as.character(enrich[[by_col]])
    same_ids <- (is.na(tab_ids) & is.na(enrich_ids)) |
      (!is.na(tab_ids) & !is.na(enrich_ids) & tab_ids == enrich_ids)

    if (all(same_ids)) {
      enrichment_cols <- setdiff(names(enrich), by_col)
      return(
        dplyr::bind_cols(
          tab,
          enrich %>% dplyr::select(dplyr::all_of(enrichment_cols))
        )
      )
    }
  }

  if (anyDuplicated(enrich[[by_col]])) {
    stop(
      "Cannot safely enrich sample '", obj$sample,
      "': enrichment IDs are duplicated and rows are not aligned.",
      call. = FALSE
    )
  }

  tab %>% dplyr::left_join(enrich, by = by_col)
}

make_enriched_sample_sheets <- function(x, masters, ii_rows,
                                        combined_sheet = "combined",
                                        z_scores = FALSE) {
  if (!isTRUE(CFG$export_enriched)) return(NULL)
  
  enriched <- list()
  for (obj in masters) {
    if (!is.null(x[[obj$sample]])) {
      enriched[[obj$sample]] <- add_enrichment_to_tbl(x[[obj$sample]], obj, ii_rows)
    }
  }
  
  make_export_sheets(enriched, combined_sheet = combined_sheet, z_scores = z_scores)
}

combine_items_and_scores <- function(items, scores, id_col, dataset_label) {
  if (is.null(items) || is.null(scores)) {
    stop(
      "Cannot build complete analysis input for '", dataset_label,
      "': item or score table is missing.",
      call. = FALSE
    )
  }

  if (nrow(items) != nrow(scores)) {
    stop(
      "Cannot build complete analysis input for '", dataset_label,
      "': item and score tables have different row counts.",
      call. = FALSE
    )
  }

  if (!(id_col %in% names(items)) || !(id_col %in% names(scores))) {
    stop(
      "Cannot build complete analysis input for '", dataset_label,
      "': ID column '", id_col, "' is missing.",
      call. = FALSE
    )
  }

  item_ids <- as.character(items[[id_col]])
  score_ids <- as.character(scores[[id_col]])
  same_ids <- (is.na(item_ids) & is.na(score_ids)) |
    (!is.na(item_ids) & !is.na(score_ids) & item_ids == score_ids)

  if (!all(same_ids)) {
    stop(
      "Cannot build complete analysis input for '", dataset_label,
      "': item and score rows are not aligned by ID.",
      call. = FALSE
    )
  }

  score_only_cols <- setdiff(names(scores), names(items))
  if (!length(score_only_cols)) return(items)

  dplyr::bind_cols(
    items,
    scores %>% dplyr::select(dplyr::all_of(score_only_cols))
  )
}

make_complete_analysis_tables <- function(items_list, scores_list, masters) {
  out <- list()

  for (obj in masters) {
    sample <- obj$sample
    out[[sample]] <- combine_items_and_scores(
      items = items_list[[sample]],
      scores = scores_list[[sample]],
      id_col = obj$id_col,
      dataset_label = sample
    )
  }

  out
}

write_questionnaire_copies <- function(enriched_sheets,
                                       source_workbook,
                                       root,
                                       projects = 2:9) {
  if (!isTRUE(CFG$export_questionnaire_copies)) return(invisible(NULL))

  if (is.null(enriched_sheets) || !length(enriched_sheets)) {
    stop("Questionnaire copies requested, but enriched sheets are missing.", call. = FALSE)
  }

  if (!fs::file_exists(source_workbook)) {
    stop(
      "Questionnaire copies requested, but source workbook is missing: ",
      source_workbook,
      call. = FALSE
    )
  }

  project_data_root <- fs::path(root, "01_project_data")
  if (!fs::dir_exists(project_data_root)) {
    stop(
      "Cannot create questionnaire copies because the project-data directory ",
      "does not exist: ", project_data_root,
      call. = FALSE
    )
  }

  sample_names <- intersect(c("adults", "adolescents"), names(enriched_sheets))

  if (all(c("adults", "adolescents") %in% sample_names)) {
    all_projects_dir <- fs::path(
      project_data_root,
      "all_projects_backbone",
      "derivatives",
      "questionnaire"
    )
    fs::dir_create(all_projects_dir)
    all_projects_copy <- fs::path(all_projects_dir, basename(source_workbook))
    fs::file_copy(source_workbook, all_projects_copy, overwrite = TRUE)
    message("Copied: ", all_projects_copy)
  } else {
    message(
      "Skipped all-projects questionnaire copy because this run does not ",
      "contain both adults and adolescents."
    )
  }

  for (sample in sample_names) {
    sample_tbl <- enriched_sheets[[sample]]

    if (is.null(sample_tbl) || !nrow(sample_tbl)) next
    if (!("project" %in% names(sample_tbl))) {
      stop(
        "Cannot create project questionnaire copies for sample '", sample,
        "': enriched table has no 'project' column.",
        call. = FALSE
      )
    }

    project_number <- normalize_project_number(sample_tbl$project)
    unresolved <- is.na(project_number)
    if (any(unresolved)) {
      unresolved_values <- unique(as.character(sample_tbl$project[unresolved]))
      unresolved_values[is.na(unresolved_values)] <- "<NA>"
      stop(
        "Cannot create project questionnaire copies for sample '", sample,
        "': ", sum(unresolved), " row(s) have an unresolved project value: ",
        paste(unresolved_values, collapse = ", "),
        call. = FALSE
      )
    }

    for (project in projects) {
      project_tbl <- sample_tbl[project_number == project, , drop = FALSE]
      if (!nrow(project_tbl)) next

      project_dir <- fs::path(
        project_data_root,
        paste0(project, "_backbone"),
        "derivatives",
        "questionnaire"
      )
      fs::dir_create(project_dir)

      project_file <- fs::path(
        project_dir,
        paste0(
          dated_master_input_date(source_workbook),
          "_",
          sample,
          "_complete_subscales_enriched.xlsx"
        )
      )
      writexl::write_xlsx(stats::setNames(list(project_tbl), sample), project_file)
      message("Wrote: ", project_file)
    }
  }

  invisible(NULL)
}

make_enriched_existing_sheets <- function(x, masters, ii_rows,
                                          combined_sheet = "combined") {
  if (!isTRUE(CFG$export_enriched)) return(NULL)
  
  out <- list()
  for (obj in masters) {
    if (!is.null(x[[obj$sample]])) {
      out[[obj$sample]] <- add_enrichment_to_tbl(x[[obj$sample]], obj, ii_rows)
    }
  }
  
  if (!is.null(x[[combined_sheet]])) {
    combined_parts <- list()
    for (obj in masters) {
      tab <- x[[combined_sheet]]
      if ("sample" %in% names(tab)) {
        tab <- tab %>% dplyr::filter(.data$sample == obj$sample)
      }
      combined_parts[[obj$sample]] <- add_enrichment_to_tbl(tab, obj, ii_rows)
    }
    out[[combined_sheet]] <- dplyr::bind_rows(combined_parts)
  }
  
  out
}

matches_any_pattern <- function(x, patterns) {
  x <- as.character(x)
  
  patterns <- as.character(patterns)
  patterns <- patterns[
    !is.na(patterns) &
      nzchar(trimws(patterns))
  ]
  
  if (length(patterns) == 0L) {
    return(rep(FALSE, length(x)))
  }
  
  matched <- stringr::str_detect(
    x,
    stringr::regex(
      paste0("(", paste(patterns, collapse = ")|("), ")"),
      ignore_case = TRUE
    )
  )
  
  dplyr::coalesce(matched, FALSE)
}

# -----------------------------
# HiTOP selection
# -----------------------------
hitop_cols <- grep("hi[_-]?top|hitop", names(ii), ignore.case = TRUE, value = TRUE)
ii_hitop <- NULL
if (isTRUE(CFG$export_hitop) && length(hitop_cols)) {
  mapped_rows <- apply(ii[, hitop_cols, drop = FALSE], 1, function(r) any(is_nonempty(r)))
  ii_hitop <- ii[mapped_rows, , drop = FALSE]
}

# -----------------------------
# COMPLETE selection
# -----------------------------
ii_complete <- NULL
if (isTRUE(CFG$export_complete)) {
  ii_complete_candidates <- ii |>
    dplyr::filter(.data$scale %in% CFG$questionnaire_scales)

  excluded_complete_items <- ii_complete_candidates |>
    dplyr::filter(
      matches_any_pattern(.data$item, CFG$complete_excluded_item_patterns)
    )

  ii_complete <- ii_complete_candidates |>
    dplyr::filter(
      !matches_any_pattern(.data$item, CFG$complete_excluded_item_patterns)
    )

  if (nrow(excluded_complete_items)) {
    message(
      "COMPLETE privacy filter excluded ", nrow(excluded_complete_items),
      " direct-name/contact/free-text item(s): ",
      paste(unique(excluded_complete_items$item), collapse = ", ")
    )
  }
  if (!nrow(ii_complete)) {
    stop("COMPLETE export: No Item Information rows match CFG$questionnaire_scales.", call. = FALSE)
  }
}

# -----------------------------
# Unfiltered exports
# -----------------------------
if (isTRUE(CFG$export_hitop) && !is.null(ii_hitop) && nrow(ii_hitop)) {
  items_list  <- list()
  scores_list <- list()
  scales_hitop <- unique(ii_hitop$scale)
  
  for (obj in masters) {
    tab_items  <- export_items_tbl(obj$d, obj$id_col, ii_hitop)
    tab_scores <- export_existing_scores_tbl(
      obj$d_scored_full, obj$id_col, obj$keys, scales_hitop,
      total_override = CFG$score_total_override,
      dataset_label = paste0(obj$sample, " HiTOP")
    )
    
    if (!is.null(tab_items)) {
      tab_items <- tab_items %>% dplyr::mutate(sample = obj$sample, .before = 1)
      items_list[[obj$sample]] <- tab_items
    }
    if (!is.null(tab_scores)) {
      tab_scores <- tab_scores %>% dplyr::mutate(sample = obj$sample, .before = 1)
      scores_list[[obj$sample]] <- tab_scores
    }
  }
  
  out_items <- fs::path(OUT_DIR, glue::glue("{output_date}_{combined_label}_HiTOP_items.xlsx"))
  writexl::write_xlsx(make_export_sheets(items_list), out_items)
  message("Wrote: ", out_items)
  
  if (isTRUE(CFG$export_enriched)) {
    out_items_enriched <- add_enriched_suffix(out_items)
    writexl::write_xlsx(make_enriched_sample_sheets(items_list, masters, ii), out_items_enriched)
    message("Wrote: ", out_items_enriched)
  }
  
  out_scores <- fs::path(OUT_DIR, glue::glue("{output_date}_{combined_label}_HiTOP_subscales.xlsx"))
  writexl::write_xlsx(make_export_sheets(scores_list, z_scores = TRUE), out_scores)
  message("Wrote: ", out_scores)
  
  if (isTRUE(CFG$export_enriched)) {
    out_scores_enriched <- add_enriched_suffix(out_scores)
    writexl::write_xlsx(make_enriched_sample_sheets(scores_list, masters, ii, z_scores = TRUE), out_scores_enriched)
    message("Wrote: ", out_scores_enriched)
  }
}

if (isTRUE(CFG$export_complete) && !is.null(ii_complete) && nrow(ii_complete)) {
  items_list  <- list()
  scores_list <- list()
  scales_all <- unique(ii_complete$scale)
  
  for (obj in masters) {
    # Use the scored master here so reviewed FHS corrections and the
    # existence-aware confidence recoding from prep05 are reflected in the
    # COMPLETE item export as well as in the derived FHS columns.
    tab_items <- export_items_tbl(
      obj$d_scored_full,
      obj$id_col,
      ii_complete,
      include_complete_auxiliary = TRUE,
      dataset_label = paste0(obj$sample, " COMPLETE")
    )
    tab_scores <- export_existing_scores_tbl(
      obj$d_scored_full,
      obj$id_col,
      obj$keys,
      scales_all,
      total_override = CFG$score_total_override,
      dataset_label = paste0(obj$sample, " COMPLETE"),
      include_complete_auxiliary = TRUE
    )
    
    if (!is.null(tab_items)) {
      tab_items <- tab_items %>% dplyr::mutate(sample = obj$sample, .before = 1)
      items_list[[obj$sample]] <- tab_items
    }
    if (!is.null(tab_scores)) {
      tab_scores <- tab_scores %>% dplyr::mutate(sample = obj$sample, .before = 1)
      scores_list[[obj$sample]] <- tab_scores
    }
  }
  
  out_items_all <- fs::path(OUT_DIR, glue::glue("{output_date}_{combined_label}_complete_items.xlsx"))
  writexl::write_xlsx(make_export_sheets(items_list), out_items_all)
  message("Wrote: ", out_items_all)
  
  if (isTRUE(CFG$export_enriched)) {
    out_items_all_enriched <- add_enriched_suffix(out_items_all)
    writexl::write_xlsx(make_enriched_sample_sheets(items_list, masters, ii), out_items_all_enriched)
    message("Wrote: ", out_items_all_enriched)
  }
  
  out_scores_all <- fs::path(OUT_DIR, glue::glue("{output_date}_{combined_label}_complete_subscales.xlsx"))
  writexl::write_xlsx(make_export_sheets(scores_list, z_scores = TRUE), out_scores_all)
  message("Wrote: ", out_scores_all)
  
  if (isTRUE(CFG$export_enriched)) {
    out_scores_all_enriched <- add_enriched_suffix(out_scores_all)
    complete_analysis_tables <- make_complete_analysis_tables(
      items_list = items_list,
      scores_list = scores_list,
      masters = masters
    )
    complete_analysis_sheets <- make_enriched_sample_sheets(
      complete_analysis_tables,
      masters,
      ii,
      z_scores = TRUE
    )
    writexl::write_xlsx(complete_analysis_sheets, out_scores_all_enriched)
    message("Wrote: ", out_scores_all_enriched)

    write_questionnaire_copies(
      enriched_sheets = complete_analysis_sheets,
      source_workbook = out_scores_all_enriched,
      root = ROOT,
      projects = CFG$questionnaire_copy_projects
    )
  }
}

# -----------------------------
# Filtered exports:
# - sample sheets use sample-specific flags + sample-specific filtered scored masters
# - combined sheet uses combined flags + combined-filtered scored masters
# -----------------------------
if (isTRUE(CFG$export_loading_filtered)) {
  flag_helper <- latest_flag_helper(
    ROOT,
    combined_label,
    threshold_tag,
    input_date = output_date
  )
  
  if (is.na(flag_helper) || !fs::file_exists(flag_helper)) {
    message("No flagged-item helper found for filtered exports. Skipping filtered outputs.")
  } else {
    message("Flag helper: ", flag_helper)
    
    # -------------------------------------------------------------------------
    # HiTOP filtered exports
    # -------------------------------------------------------------------------
    if (isTRUE(CFG$export_hitop) && !is.null(ii_hitop) && nrow(ii_hitop)) {
      items_list  <- list()
      scores_list <- list()
      scales_hitop <- unique(ii_hitop$scale)
      
      # sample-specific sheets
      for (obj in masters) {
        if (is.null(obj$d_scored_filtered)) {
          stop(
            "Missing sample-specific filtered scored master for sample '",
            obj$sample, "'. Run Step 05 with export_filtered_scores = TRUE.",
            call. = FALSE
          )
        }
        
        flagged_tbl <- read_flagged_items(
          flag_helper,
          dataset_label = obj$sample,
          threshold_value = CFG$loading_threshold
        )
        
        keys_filt <- drop_flagged_items_from_keys(obj$keys, flagged_tbl)
        
        flagged_norm <- unique(normalize_id(flagged_tbl$item))
        ii_hitop_filt <- if (length(flagged_norm)) {
          ii_hitop %>% dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm))
        } else {
          ii_hitop
        }
        
        tab_items <- export_items_tbl(obj$d, obj$id_col, ii_hitop_filt)
        tab_scores <- export_existing_scores_tbl(
          obj$d_scored_filtered,
          obj$id_col,
          keys_filt,
          scales_hitop,
          total_override = CFG$score_total_override,
          dataset_label = paste0(obj$sample, " HiTOP_", threshold_tag)
        )
        
        if (!is.null(tab_items)) {
          items_list[[obj$sample]] <- tab_items %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
        if (!is.null(tab_scores)) {
          scores_list[[obj$sample]] <- tab_scores %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
      }
      
      # combined sheet
      combined_items_parts <- list()
      combined_scores_parts <- list()
      
      for (obj in masters) {
        if (is.null(obj$d_scored_combined)) {
          stop(
            "Missing combined-filtered scored master for sample '",
            obj$sample, "'. Run Step 05 with export_combined_filtered_scores = TRUE.",
            call. = FALSE
          )
        }
        
        flagged_tbl_combined <- read_flagged_items(
          flag_helper,
          dataset_label = combined_label,
          threshold_value = CFG$loading_threshold
        )
        
        keys_combined_filt <- drop_flagged_items_from_keys(obj$keys, flagged_tbl_combined)
        
        flagged_norm_combined <- unique(normalize_id(flagged_tbl_combined$item))
        ii_hitop_combined_filt <- if (length(flagged_norm_combined)) {
          ii_hitop %>% dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm_combined))
        } else {
          ii_hitop
        }
        
        tab_items_combined <- export_items_tbl(obj$d, obj$id_col, ii_hitop_combined_filt)
        tab_scores_combined <- export_existing_scores_tbl(
          obj$d_scored_combined,
          obj$id_col,
          keys_combined_filt,
          scales_hitop,
          total_override = CFG$score_total_override,
          dataset_label = paste0(combined_label, " HiTOP_", threshold_tag)
        )
        
        if (!is.null(tab_items_combined)) {
          combined_items_parts[[obj$sample]] <- tab_items_combined %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
        if (!is.null(tab_scores_combined)) {
          combined_scores_parts[[obj$sample]] <- tab_scores_combined %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
      }
      
      items_list[["combined"]]  <- dplyr::bind_rows(combined_items_parts)
      scores_list[["combined"]] <- dplyr::bind_rows(combined_scores_parts)
      
      out_items_f <- fs::path(OUT_DIR, glue::glue("{output_date}_{combined_label}_HiTOP_items_{threshold_tag}.xlsx"))
      writexl::write_xlsx(items_list, out_items_f)
      message("Wrote: ", out_items_f)
      
      if (isTRUE(CFG$export_enriched)) {
        out_items_f_enriched <- add_enriched_suffix(out_items_f)
        writexl::write_xlsx(make_enriched_existing_sheets(items_list, masters, ii), out_items_f_enriched)
        message("Wrote: ", out_items_f_enriched)
      }
      
      out_scores_f <- fs::path(OUT_DIR, glue::glue("{output_date}_{combined_label}_HiTOP_subscales_{threshold_tag}.xlsx"))
      scores_list <- purrr::map(scores_list, add_z_score_columns)
      writexl::write_xlsx(scores_list, out_scores_f)
      message("Wrote: ", out_scores_f)
      
      if (isTRUE(CFG$export_enriched)) {
        out_scores_f_enriched <- add_enriched_suffix(out_scores_f)
        writexl::write_xlsx(make_enriched_existing_sheets(scores_list, masters, ii), out_scores_f_enriched)
        message("Wrote: ", out_scores_f_enriched)
      }
    }
    
    # -------------------------------------------------------------------------
    # COMPLETE filtered exports
    # -------------------------------------------------------------------------
    if (isTRUE(CFG$export_complete) && !is.null(ii_complete) && nrow(ii_complete)) {
      items_list  <- list()
      scores_list <- list()
      scales_all <- unique(ii_complete$scale)
      
      # sample-specific sheets
      for (obj in masters) {
        if (is.null(obj$d_scored_filtered)) {
          stop(
            "Missing sample-specific filtered scored master for sample '",
            obj$sample, "'. Run Step 05 with export_filtered_scores = TRUE.",
            call. = FALSE
          )
        }
        
        flagged_tbl <- read_flagged_items(
          flag_helper,
          dataset_label = obj$sample,
          threshold_value = CFG$loading_threshold
        )
        
        keys_filt <- drop_flagged_items_from_keys(obj$keys, flagged_tbl)
        
        flagged_norm <- unique(normalize_id(flagged_tbl$item))
        ii_complete_filt <- if (length(flagged_norm)) {
          ii_complete %>% dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm))
        } else {
          ii_complete
        }
        
        tab_items <- export_items_tbl(
          obj$d_scored_filtered,
          obj$id_col,
          ii_complete_filt,
          include_complete_auxiliary = TRUE,
          dataset_label = paste0(obj$sample, " COMPLETE_", threshold_tag)
        )
        tab_scores <- export_existing_scores_tbl(
          obj$d_scored_filtered,
          obj$id_col,
          keys_filt,
          scales_all,
          total_override = CFG$score_total_override,
          dataset_label = paste0(obj$sample, " COMPLETE_", threshold_tag),
          include_complete_auxiliary = TRUE
        )
        
        if (!is.null(tab_items)) {
          items_list[[obj$sample]] <- tab_items %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
        if (!is.null(tab_scores)) {
          scores_list[[obj$sample]] <- tab_scores %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
      }
      
      # combined sheet
      combined_items_parts <- list()
      combined_scores_parts <- list()
      
      for (obj in masters) {
        if (is.null(obj$d_scored_combined)) {
          stop(
            "Missing combined-filtered scored master for sample '",
            obj$sample, "'. Run Step 05 with export_combined_filtered_scores = TRUE.",
            call. = FALSE
          )
        }
        
        flagged_tbl_combined <- read_flagged_items(
          flag_helper,
          dataset_label = combined_label,
          threshold_value = CFG$loading_threshold
        )
        
        keys_combined_filt <- drop_flagged_items_from_keys(obj$keys, flagged_tbl_combined)
        
        flagged_norm_combined <- unique(normalize_id(flagged_tbl_combined$item))
        ii_complete_combined_filt <- if (length(flagged_norm_combined)) {
          ii_complete %>% dplyr::filter(!(normalize_id(.data$item) %in% flagged_norm_combined))
        } else {
          ii_complete
        }
        
        tab_items_combined <- export_items_tbl(
          obj$d_scored_combined,
          obj$id_col,
          ii_complete_combined_filt,
          include_complete_auxiliary = TRUE,
          dataset_label = paste0(combined_label, " COMPLETE_", threshold_tag)
        )
        tab_scores_combined <- export_existing_scores_tbl(
          obj$d_scored_combined,
          obj$id_col,
          keys_combined_filt,
          scales_all,
          total_override = CFG$score_total_override,
          dataset_label = paste0(combined_label, " COMPLETE_", threshold_tag),
          include_complete_auxiliary = TRUE
        )
        
        if (!is.null(tab_items_combined)) {
          combined_items_parts[[obj$sample]] <- tab_items_combined %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
        if (!is.null(tab_scores_combined)) {
          combined_scores_parts[[obj$sample]] <- tab_scores_combined %>%
            dplyr::mutate(sample = obj$sample, .before = 1)
        }
      }
      
      items_list[["combined"]]  <- dplyr::bind_rows(combined_items_parts)
      scores_list[["combined"]] <- dplyr::bind_rows(combined_scores_parts)
      
      out_items_all_f <- fs::path(OUT_DIR, glue::glue("{output_date}_{combined_label}_complete_items_{threshold_tag}.xlsx"))
      writexl::write_xlsx(items_list, out_items_all_f)
      message("Wrote: ", out_items_all_f)
      
      if (isTRUE(CFG$export_enriched)) {
        out_items_all_f_enriched <- add_enriched_suffix(out_items_all_f)
        writexl::write_xlsx(make_enriched_existing_sheets(items_list, masters, ii), out_items_all_f_enriched)
        message("Wrote: ", out_items_all_f_enriched)
      }
      
      out_scores_all_f <- fs::path(OUT_DIR, glue::glue("{output_date}_{combined_label}_complete_subscales_{threshold_tag}.xlsx"))
      scores_list <- purrr::map(scores_list, add_z_score_columns)
      writexl::write_xlsx(scores_list, out_scores_all_f)
      message("Wrote: ", out_scores_all_f)
      
      if (isTRUE(CFG$export_enriched)) {
        out_scores_all_f_enriched <- add_enriched_suffix(out_scores_all_f)
        writexl::write_xlsx(make_enriched_existing_sheets(scores_list, masters, ii), out_scores_all_f_enriched)
        message("Wrote: ", out_scores_all_f_enriched)
      }
    }
  }
}

compact_column_name <- function(x) {
  gsub("[^a-z0-9]", "", tolower(as.character(x)))
}

matches_any_pattern <- function(x, patterns) {
  compact <- compact_column_name(x)
  vapply(
    compact,
    function(one) any(vapply(patterns, grepl, logical(1), x = one)),
    logical(1)
  )
}
