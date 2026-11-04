#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Separate Backbone Data by Project  — Cleaning & Export Pipeline
# Authors: Saskia Wilken (saskia.wilken@uni-hamburg.de, saskia.a.wilken@gmail.com) &
#          Antonia Bott (antonia.bott@uni-hamburg.de)
# First edited: 2025-08-08 (SW)
#
# Description:
# This script reads questionnaire data (LimeSurvey, PsyToolkit), fixes known
# VP-ID issues, removes test/empty entries, separates data per project, exports
# cleaned datasets (and discarded rows) to disk, and manages logging.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Clean up R environment -------------------------------------------------------
rm(list = ls())
cat("\014")

# Install/load packages --------------------------------------------------------
if (!require("dplyr"))     { install.packages("dplyr")     }; library(dplyr)
if (!require("tidyr"))     { install.packages("tidyr")     }; library(tidyr)
if (!require("writexl"))   { install.packages("writexl")   }; library(writexl)
if (!require("rstudioapi")){ install.packages("rstudioapi")}; library(rstudioapi)
if (!require("readxl"))    { install.packages("readxl")}; library(readxl)
if (!require("purrr")){ install.packages("purrr")}; library(purrr)
if (!require("stringr")){ install.packages("stringr")}; library(stringr)
if (!require("rlang")){ install.packages("rlang")}; library(rlang)
if (!require("readr")){ install.packages("readr")}; library(readr)
if (!require("lubridate")){ install.packages("lubridate")}; library(lubridate)
if (!require("openxlsx")){ install.packages("openxlsx")}; library(openxlsx)
if (!require("jsonlite")){ install.packages("jsonlite")}; library(jsonlite)


# Ensure proper number display -------------------------------------------------
options(scipen = 999)  # disable scientific notation globally

# Get Today -------------------------------------------------------------------
today <- format(Sys.Date(), "%Y-%m-%d")

# Set working directory -----------------------------------------------
whoami <- Sys.info()[["nodename"]]
if (is.null(whoami) || is.na(whoami)) whoami <- Sys.info()[[4]]
print(whoami)

# If you are not in this list, a sensible default is used
name <- switch(
  whoami,
  "KOPSY-D-033080" = "K:/vol2011/bba3488/",
  "UN-LAP-015977"  = {
    if (rstudioapi::isAvailable()) {
      dirname(rstudioapi::getSourceEditorContext()$path)
    } else {
      getwd()
    }
  },
  # default fallback
  getwd()
)

setwd(name)

in_path           <- file.path("raw_data")
project_data_path <- file.path("01_project_data")
out_path          <- project_data_path
function_path     <- file.path("functions")
psytool_path      <- file.path("raw_data", "psytoolkit")
private_info_path <- file.path("private_information")

# IMPORTANT: raw_data and derivatives belong INSIDE every project folder, e.g.
# 01_project_data/3_backbone/raw_data and
# 01_project_data/3_backbone/derivatives.
# Nothing is moved into 01_project_data/raw_data or 01_project_data/derivatives.
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

# Specify the folder path
discarded_path <- file.path(out_path, "discarded")

# Check if the folder exists
if (!dir.exists(discarded_path)) {
  dir.create(discarded_path, recursive = TRUE)
}

# ---------- shared helpers ----------
# robust CSV reader for both ';' (questionnaires) and ',' (PsyToolkit)
safe_read_csv <- function(path, tz = "Europe/Berlin") {
  stopifnot(file.exists(path))
  first <- readr::read_lines(path, n_max = 1)
  delim <- ifelse(stringr::str_count(first, ";") > stringr::str_count(first, ","), ";", ",")
  df <- readr::read_delim(
    path, delim = delim,
    na = c("", "NA", "NaN", "null"),
    locale = readr::locale(tz = tz, decimal_mark = ".", grouping_mark = ","),
    trim_ws = TRUE, show_col_types = FALSE
  )
  # strip BOM + normalize names
  nm <- names(df); nm[1] <- sub("^\ufeff", "", nm[1]); nm <- gsub("\\s+", "_", trimws(nm)); names(df) <- nm
  # IMPORTANT: PsyToolkit TIME_start/TIME_end are intentionally parsed here.
  # Raw PsyToolkit values are usually strings like "2025-09-24-09-34".
  # We interpret them as Europe/Berlin time (Hamburg project time) and store them
  # as POSIXct. When R later prints or exports the same POSIXct value in UTC, it
  # can look shifted by the Berlin UTC offset (usually -2 h during summer time),
  # e.g. "2025-09-24-09-34" -> "2025-09-24 07:34:00 UTC". That is expected.
  # All manual exact-time checks below should therefore compare Berlin time
  # explicitly, using .fmt_time_berlin() or .date_berlin().
  parse_hyphen_dt <- function(x) {
    y <- trimws(as.character(x)); y[y %in% c("", "NA","NaN","null")] <- NA
    suppressWarnings(as.POSIXct(y, format = "%Y-%m-%d-%H-%M", tz = tz))
  }
  if ("TIME_start" %in% names(df)) {
    df$TIME_start <- parse_hyphen_dt(df$TIME_start)
  }
  if ("TIME_end" %in% names(df)) {
    df$TIME_end <- parse_hyphen_dt(df$TIME_end)
  }
  df
}

.normalize_sample <- function(x) {
  x <- tolower(x)
  if (grepl("parents_p6", x)) return("parents_p6")
  if (grepl("children_p6", x)) return("children_p6")
  if (grepl("children_parents", x)) return("children_parents")
  if (grepl("(?:^|[_\\.-])(children)(?:$|[_\\.-])", x)) return("children_parents")
  if (grepl("(?:^|[_\\.-])(adolescents)(?:$|[_\\.-])", x)) return("adolescents")
  if (grepl("(?:^|[_\\.-])(adults)(?:$|[_\\.-])", x)) return("adults")
  "unknown"
}

pick_newest_matching_dir <- function(parent_dir, pattern) {
  hits <- dir(parent_dir, pattern = pattern, full.names = TRUE)
  
  if (length(hits) == 0) {
    return(NA_character_)
  }
  
  info <- file.info(hits)
  hits[which.max(info$mtime)]
}

.ensure_dir <- function(path, dry_run) if (!dir.exists(path) && !dry_run) dir.create(path, recursive = TRUE, showWarnings = FALSE)
.is_empty_df <- function(x) is.null(x) || nrow(x) == 0 || all(vapply(x, function(col) all(is.na(col)), logical(1)))

# --- Robust POSIXct parser (works for your LimeSurvey + PsyToolkit formats) ---
as_time_safely <- function(x, tz = "Europe/Berlin") {
  if (inherits(x, "POSIXct")) return(x)
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA","NaN","null")] <- NA
  
  fmts <- c(
    "%Y-%m-%d %H:%M:%S %Z",
    "%Y-%m-%d %H:%M:%S %z",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y-%m-%d-%H-%M",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d",
    "%d.%m.%Y %H:%M:%S",
    "%d.%m.%Y %H:%M",
    "%d.%m.%Y"
  )
  
  out <- rep(as.POSIXct(NA, tz = tz), length(x))
  for (f in fmts) {
    miss <- is.na(out) & !is.na(x)
    if (!any(miss)) break
    parsed <- suppressWarnings(as.POSIXct(x[miss], format = f, tz = tz))
    out[miss] <- parsed
  }
  out
}

.fmt_time_berlin <- function(x, fmt = "%Y-%m-%d %H:%M:%S") {
  format(as_time_safely(x, tz = "Europe/Berlin"), fmt, tz = "Europe/Berlin")
}

.date_berlin <- function(x) {
  as.Date(as_time_safely(x, tz = "Europe/Berlin"), tz = "Europe/Berlin")
}

# Match an exact questionnaire session time supplied in UTC against the
# available LimeSurvey time columns. Questionnaire timestamps are interpreted
# as Europe/Berlin during import, but may later be displayed in UTC.
.questionnaire_time_matches_utc <- function(df,
                                            timestamp_utc,
                                            time_cols = c("startdate", "submitdate", "datestamp")) {
  target <- as.POSIXct(
    timestamp_utc,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )

  if (is.na(target)) {
    stop("Invalid UTC questionnaire timestamp: ", timestamp_utc)
  }

  available_time_cols <- intersect(time_cols, names(df))
  if (!length(available_time_cols)) {
    stop(
      "Questionnaire data contain none of the expected time columns: ",
      paste(time_cols, collapse = ", ")
    )
  }

  matched <- rep(FALSE, nrow(df))
  for (time_col in available_time_cols) {
    values <- as_time_safely(df[[time_col]], tz = "Europe/Berlin")
    matched <- matched | (!is.na(values) & values == target)
  }

  matched
}

# --- ID remap: 30xxx -> 32xxx (keep last 3 digits); keep existing 32xxx untouched ---
make_p3_exp2_id <- function(id_vec) {
  x <- suppressWarnings(as.integer(as.character(id_vec)))
  out <- x
  already_32 <- !is.na(x) & x >= 32000L & x < 33000L
  idx <- !is.na(x) & !already_32
  out[idx] <- 32000L + (x[idx] %% 1000L)
  out
}

# =============================================================================
# Project 3 exp split EXACTLY as you specified
# Step 1: extract start time of 30128
# Step 2: any row with starttime > boundary gets renamed to 32xxx
# Step 3: exp-2 are the rows whose (final) id starts with 32xxx
# =============================================================================
p3_split_after_30128 <- function(df,
                                 id_col,
                                 project_col,
                                 start_col,
                                 boundary_id = 30128L,
                                 tz = "Europe/Berlin",
                                 verbose = TRUE) {
  
  stopifnot(id_col %in% names(df), project_col %in% names(df), start_col %in% names(df))
  
  out <- df
  
  ids   <- suppressWarnings(as.integer(as.character(out[[id_col]])))
  proj  <- suppressWarnings(as.integer(as.character(out[[project_col]])))
  start <- as_time_safely(out[[start_col]], tz = tz)
  
  is_p3 <- !is.na(proj) & proj == 3L
  
  # Step 1: boundary time = start time of ID 30128 (if multiple, take the latest one)
  boundary_candidates <- start[is_p3 & !is.na(ids) & ids == boundary_id]
  if (!length(boundary_candidates) || all(is.na(boundary_candidates))) {
    stop(sprintf("Could not find a parseable %s for P3 %s == %d.", start_col, id_col, boundary_id))
  }
  boundary_time <- max(boundary_candidates, na.rm = TRUE)
  
  # Step 2: rename any row strictly AFTER boundary_time
  idx_after <- which(is_p3 & !is.na(start) & start > boundary_time)
  if (length(idx_after)) {
    out[[id_col]][idx_after] <- make_p3_exp2_id(out[[id_col]][idx_after])
  }
  
  # Step 3: exp-2 = IDs that start with 32xxx (after renaming)
  ids_new <- suppressWarnings(as.integer(as.character(out[[id_col]])))
  exp2_mask <- is_p3 & !is.na(ids_new) & ids_new >= 32000L & ids_new < 33000L
  exp1_mask <- is_p3 & !is.na(ids_new) & ids_new >= 30000L & ids_new < 32000L
  
  if (isTRUE(verbose)) {
    message(sprintf(
      "P3 split: boundary_id=%d at %s | renamed=%d | exp-1=%d | exp-2=%d | NA starttimes in P3=%d",
      boundary_id,
      format(boundary_time, "%Y-%m-%d %H:%M:%S"),
      length(idx_after),
      sum(exp1_mask, na.rm=TRUE),
      sum(exp2_mask, na.rm=TRUE),
      sum(is_p3 & is.na(start), na.rm=TRUE)
    ))
  }
  
  list(
    data = out,
    boundary_time = boundary_time,
    exp_1 = out[exp1_mask, , drop = FALSE],
    exp_2 = out[exp2_mask, , drop = FALSE]
  )
}

write_project3_exp_split_exports <- function(exp1_df,
                                             exp2_df,
                                             out_path,
                                             sample_label = "adults",
                                             data_type = c("questionnaires", "experiment_data"),
                                             today = format(Sys.Date(), "%Y-%m-%d")) {
  
  data_type <- match.arg(data_type)
  type_stub <- if (identical(data_type, "questionnaires")) "questionnaire" else "cogtests"
  
  roots <- c(
    file.path(out_path, "3_backbone", "raw_data", data_type),
    file.path(out_path, "all_projects_backbone", "raw_data", data_type)
  )
  
  f_exp1 <- sprintf("3_%s_%s_%s_exp-1.xlsx", today, sample_label, type_stub)
  f_exp2 <- sprintf("3_%s_%s_%s_exp-2.xlsx", today, sample_label, type_stub)
  
  for (root in roots) {
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    writexl::write_xlsx(exp1_df, file.path(root, f_exp1))
    writexl::write_xlsx(exp2_df, file.path(root, f_exp2))
  }
}
# ---- FORCE STABLE MASTER SCHEMA ---------------------------------------------

# ---- Deterministic ID schema helpers -----------------------------------------

normalize_questionnaire_ids <- function(df, sample) {
  sample <- match.arg(
    sample,
    c("adults", "adolescents", "children_parents", "children_p6", "parents_p6")
  )
  
  # Participant ID column is fixed by data source.
  participant_id_col <- switch(
    sample,
    adults           = "vpid",
    adolescents      = "vpid",
    children_parents = "vpid",
    children_p6      = "VPCode",
    parents_p6       = "VPCode"
  )
  
  if (!participant_id_col %in% names(df)) {
    stop(sprintf(
      "normalize_questionnaire_ids(): sample '%s' is missing participant ID column '%s'.",
      sample, participant_id_col
    ))
  }
  
  # LimeSurvey's internal response id is not the participant ID.
  # Rename in place, preserving column order.
  if ("id" %in% names(df) && !"limesurvey_id" %in% names(df)) {
    names(df)[names(df) == "id"] <- "limesurvey_id"
  }
  
  # Export convention: participant ID column is called vp_id.
  # Rename the known participant ID column in place, preserving column order.
  if (participant_id_col != "vp_id") {
    names(df)[names(df) == participant_id_col] <- "vp_id"
  }
  
  df$vp_id <- suppressWarnings(as.integer(as.character(df$vp_id)))
  
  # For P6 files only: if there is truly no project column, append project = 6.
  # This is the only intentional new column.
  if (!"project" %in% names(df)) {
    if ("Projekt." %in% names(df)) {
      names(df)[names(df) == "Projekt."] <- "project"
    } else if (sample %in% c("children_p6", "parents_p6")) {
      df$project <- 6L
    }
  }
  
  df
}

normalize_cogtest_ids <- function(df) {
  # PsyToolkit receives the participant ID from LimeSurvey in column 'id'.
  # There is no separate internal LimeSurvey-like row id here.
  if (!"id" %in% names(df)) {
    stop("normalize_cogtest_ids(): missing participant ID column 'id'.")
  }
  
  df$id <- suppressWarnings(as.integer(as.character(df$id)))
  df
}


## Source required functions ---------------------------------------------------
source(file.path(function_path, "separate_by_project.R"))
source(file.path(function_path, "remove_test_rows.R"))
source(file.path(function_path, "copy_psytool_files.R"))
source(file.path(function_path, "extract_pilot_by_vpid.R"))
source(file.path(function_path, "resolve_duplicates.R"))
source(file.path(function_path, "correct_child_vpids.R"))
source(file.path(function_path, "check_vpid_forms.R"))
source(file.path(function_path, "find_pilot_ids.R"))
source(file.path(function_path, "compare_vpcodes.R"))
source(file.path(function_path, "partition_empty_obs_psytoolkit.R"))
source(file.path(function_path, "collect_ids_to_excel.R"))
source(file.path(function_path, "move_old_backbones.R"))
source(file.path(function_path, "setup_logging.R"))
source(file.path(function_path, "qc_ranges_and_missing.R"))
source(file.path(function_path, "aggregate_scales.R"))
source(file.path(function_path, "extract_scales.R"))
source(file.path(function_path, "prepare_project_slices.R"))
source(file.path(function_path, "write_project_slices.R"))


## Move old Data ---------------------------------------------------------------
move_old_backbones(out_path, dry_run = FALSE)

## Create the clean target layout for this run ---------------------------------
initialize_backbone_layout <- function(project_data_path) {
  backbone_names <- c(paste0(2:9, "_backbone"), "all_projects_backbone")
  standard_subdirs <- c(
    file.path("raw_data", "questionnaires"),
    file.path("raw_data", "experiment_data"),
    "derivatives"
  )

  for (backbone_name in backbone_names) {
    for (subdir in standard_subdirs) {
      dir.create(
        file.path(project_data_path, backbone_name, subdir),
        recursive = TRUE,
        showWarnings = FALSE
      )
    }
  }

  for (subdir in c("questionnaires", "experiment_data")) {
    dir.create(
      file.path(project_data_path, "9_backbone", "raw_data", "pilot_data", subdir),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  invisible(TRUE)
}

initialize_backbone_layout(out_path)

## Setup Logging ---------------------------------------------------------------
dir.create(file.path(out_path, "logs"), recursive = TRUE, showWarnings = FALSE)
all_action_points_log <- file.path(out_path, "logs", "all_action_points.log")
logger <- setup_logging(all_action_points_log)

## Backbone surveys ------------------------------------------------------------
file_adults            <- "results-survey564757_remids_translated.csv"
file_adolescents       <- "results-survey585676.csv"
file_children_parents  <- "results-survey798916_remids_translated.csv"
file_parents_p6        <- "results-survey191355.csv"
file_children_p6       <- "results-survey518972.csv"

## Load data -------------------------------------------------------------------
# Questionnaires
dat_adults           <- safe_read_csv(file.path(name, in_path, file_adults))
dat_adolescents      <- safe_read_csv(file.path(name, in_path, file_adolescents))
dat_children_parents <- safe_read_csv(file.path(name, in_path, file_children_parents))
dat_parents_p6       <- safe_read_csv(file.path(name, in_path, file_parents_p6))
dat_children_p6      <- safe_read_csv(file.path(name, in_path, file_children_p6))

# --- Debug: track whether adolescent VPIDs get truncated -----------------------

track_adolescent_vpids <- function(df, label) {
  if (!"vpid" %in% names(df)) {
    message(label, ": no vpid column")
    return(invisible(df))
  }
  
  x_chr <- trimws(as.character(df$vpid))
  x_num <- suppressWarnings(as.integer(x_chr))
  
  bad_short <- !is.na(x_num) & x_num > 0L & x_num < 1000L
  
  message(
    label,
    " | n=", nrow(df),
    " | min=", suppressWarnings(min(x_num, na.rm = TRUE)),
    " | max=", suppressWarnings(max(x_num, na.rm = TRUE)),
    " | short_ids=", sum(bad_short, na.rm = TRUE)
  )
  
  if (any(bad_short, na.rm = TRUE)) {
    print(
      df[bad_short, intersect(c("vpid", "project", "submitdate", "startdate", "id"), names(df)), drop = FALSE] %>%
        utils::head(20)
    )
    stop("Adolescent vpid got shortened before/at: ", label)
  }
  
  invisible(df)
}

dat_adolescents <- track_adolescent_vpids(dat_adolescents, "after import")

# Get metadata
quest_info <- file.info(file.path(name, in_path, file_adults))
quest_info$sample <- "adults"
quest_info[2, ]   <- c(file.info(file.path(name, in_path, file_adolescents)),      "adolescents")
quest_info[3, ]   <- c(file.info(file.path(name, in_path, file_children_parents)), "children_parents")
quest_info[4, ]   <- c(file.info(file.path(name, in_path, file_parents_p6)),       "parents_p6")
quest_info[5, ]   <- c(file.info(file.path(name, in_path, file_children_p6)),      "children_p6")

# Data Overview
file_general <- "results-survey415148.csv"
dat_general  <- safe_read_csv(file.path(name, in_path, file_general))

# PsyToolkit Tests
file_psytool_info <- "data.csv"

psytool_dir_adults <- pick_newest_matching_dir(
  file.path(name, psytool_path),
  "adults_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}$"
)

psytool_dir_adolescents <- pick_newest_matching_dir(
  file.path(name, psytool_path),
  "adolescents_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}$"
)

psytool_dir_children <- pick_newest_matching_dir(
  file.path(name, psytool_path),
  "children_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}$"
)

cog_paths <- c(
  adults = if (!is.na(psytool_dir_adults)) file.path(psytool_dir_adults, file_psytool_info) else NA_character_,
  adolescents = if (!is.na(psytool_dir_adolescents)) file.path(psytool_dir_adolescents, file_psytool_info) else NA_character_,
  children_parents = if (!is.na(psytool_dir_children)) file.path(psytool_dir_children, file_psytool_info) else NA_character_
)

# Get metadata
cogtest_info <- file.info(cog_paths)
cogtest_info$sample <- c("adults","adolescents","children_parents")[seq_len(nrow(cogtest_info))]

psytool_info_adults      <- safe_read_csv(cog_paths["adults"])
psytool_info_adolescents <- safe_read_csv(cog_paths["adolescents"])
psytool_info_children    <- safe_read_csv(cog_paths["children_parents"])

# Get item info

scoring_info <- read_excel(file.path("information", "2025-10-28_Scoring.xlsx"))
item_info_adults <- read_excel(file.path("information", "2025-10-28_Item_Information_Adults.xlsx"))

# Remove Test Datasets from all Project data ----------------------------------
dat_adults           <- remove_test_rows(dat_adults,           "Adults",      dat_general)
dat_adolescents      <- remove_test_rows(dat_adolescents,      "Adolescents", dat_general)
dat_children_parents <- remove_test_rows(dat_children_parents, "Children",    dat_general)
dat_parents_p6       <- remove_test_rows(dat_parents_p6,       "Parents",     dat_general)
dat_children_p6      <- remove_test_rows(dat_children_p6,      "Children",    dat_general)

psytool_info_adults      <- remove_test_rows(psytool_info_adults,      "Adults",      dat_general)
psytool_info_adolescents <- remove_test_rows(psytool_info_adolescents, "Adolescents", dat_general)
psytool_info_children    <- remove_test_rows(psytool_info_children,    "Children",    dat_general)


dat_adolescents <- track_adolescent_vpids(dat_adolescents, "after remove_test_rows")

################################################################################
## Data Cleaning for Questionnaire Data ----------------------------------------
################################################################################


# ---- 1) read schema once ----
library(jsonlite)
schema <- read_json(file.path("information", "recommended_schema.json"), simplifyVector = TRUE)

# small converters
as_num_safely <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = FALSE)))
as_int_safely <- function(x) suppressWarnings(as.integer(as_num_safely(x)))


coerce_by_schema <- function(df, schema) {
  if (is.null(df) || !ncol(df)) return(df)
  for (nm in intersect(names(df), names(schema))) {
    tgt <- schema[[nm]]
    if (is.null(tgt)) next
    # skip if already the right general class
    cur <- class(df[[nm]])[1]
    # perform coercion
    df[[nm]] <- switch(
      tgt,
      "integer"  = as_int_safely(df[[nm]]),
      "double"   = as_num_safely(df[[nm]]),
      "datetime" = as_time_safely(df[[nm]]),
      "string"   = as.character(df[[nm]]),
      df[[nm]]
    )
  }
  df
}

# =============================================================================
# ID change audit helpers
# =============================================================================

id_change_audit <- list()

.audit_col <- function(df, idx, candidates) {
  hit <- intersect(candidates, names(df))
  if (!length(hit)) return(rep(NA_character_, length(idx)))
  as.character(df[[hit[1]]][idx])
}

.audit_project <- function(df, idx, project_col = NULL) {
  if (!is.null(project_col) && project_col %in% names(df)) {
    return(as.character(df[[project_col]][idx]))
  }
  if ("project" %in% names(df)) return(as.character(df$project[idx]))
  if ("p" %in% names(df)) return(as.character(df$p[idx]))
  rep(NA_character_, length(idx))
}

.audit_add <- function(rows) {
  if (!is.null(rows) && nrow(rows) > 0) {
    id_change_audit[[length(id_change_audit) + 1L]] <<- rows
  }
}

audit_id_change <- function(df,
                            idx,
                            id_col,
                            new_id,
                            project_col = NULL,
                            sample,
                            data_type,
                            criterion,
                            action = "id_changed") {
  idx <- which(idx)
  if (!length(idx)) return(df)
  
  if (!id_col %in% names(df)) {
    stop("audit_id_change(): missing id_col: ", id_col)
  }
  
  old_id <- suppressWarnings(as.integer(as.character(df[[id_col]][idx])))
  
  if (length(new_id) == 1L) {
    new_id_vec <- rep(new_id, length(idx))
  } else {
    new_id_vec <- new_id
  }
  
  new_id_vec <- suppressWarnings(as.integer(as.character(new_id_vec)))
  
  changed <- !(is.na(old_id) & is.na(new_id_vec)) & old_id != new_id_vec
  if (!any(changed)) return(df)
  
  idx_changed <- idx[changed]
  
  rows <- data.frame(
    action        = action,
    project       = .audit_project(df, idx_changed, project_col),
    sample        = sample,
    data_type     = data_type,
    id_col        = id_col,
    old_id        = old_id[changed],
    new_id        = new_id_vec[changed],
    criterion     = criterion,
    limesurvey_id = .audit_col(df, idx_changed, c("limesurvey_id", "id")),
    startdate     = .audit_col(df, idx_changed, c("startdate")),
    submitdate    = .audit_col(df, idx_changed, c("submitdate")),
    datestamp     = .audit_col(df, idx_changed, c("datestamp")),
    TIME_start    = .audit_col(df, idx_changed, c("TIME_start")),
    TIME_end      = .audit_col(df, idx_changed, c("TIME_end")),
    gender        = .audit_col(df, idx_changed, c("gender", "sex", "Geschlecht")),
    age_years     = .audit_col(df, idx_changed, c("age_years", "age", "Alter")),
    stringsAsFactors = FALSE
  )
  
  .audit_add(rows)
  df[[id_col]][idx_changed] <- new_id_vec[changed]
  df
}

audit_row_action <- function(df,
                             idx,
                             id_col,
                             project_col = NULL,
                             sample,
                             data_type,
                             criterion,
                             action = "deleted",
                             new_id = NA_integer_) {
  idx <- which(idx)
  if (!length(idx)) return(df)
  
  rows <- data.frame(
    action        = action,
    project       = .audit_project(df, idx, project_col),
    sample        = sample,
    data_type     = data_type,
    id_col        = id_col,
    old_id        = suppressWarnings(as.integer(as.character(df[[id_col]][idx]))),
    new_id        = suppressWarnings(as.integer(new_id)),
    criterion     = criterion,
    limesurvey_id = .audit_col(df, idx, c("limesurvey_id", "id")),
    startdate     = .audit_col(df, idx, c("startdate")),
    submitdate    = .audit_col(df, idx, c("submitdate")),
    datestamp     = .audit_col(df, idx, c("datestamp")),
    TIME_start    = .audit_col(df, idx, c("TIME_start")),
    TIME_end      = .audit_col(df, idx, c("TIME_end")),
    gender        = .audit_col(df, idx, c("gender", "sex", "Geschlecht")),
    age_years     = .audit_col(df, idx, c("age_years", "age", "Alter")),
    stringsAsFactors = FALSE
  )
  
  .audit_add(rows)
  
  if (identical(action, "deleted")) {
    return(df[-idx, , drop = FALSE])
  }
  
  df
}

audit_snapshot <- function(df) {
  if (!".__audit_row_id" %in% names(df)) {
    df$.__audit_row_id <- seq_len(nrow(df))
  }
  df
}

audit_id_diff_by_row <- function(before,
                                 after,
                                 id_col,
                                 project_col = NULL,
                                 sample,
                                 data_type,
                                 criterion,
                                 action = "id_changed") {
  if (!".__audit_row_id" %in% names(before) || !".__audit_row_id" %in% names(after)) {
    stop("audit_id_diff_by_row(): missing .__audit_row_id. Use audit_snapshot() before the change.")
  }
  
  if (!id_col %in% names(before) || !id_col %in% names(after)) {
    stop("audit_id_diff_by_row(): missing id_col: ", id_col)
  }
  
  pos_before <- match(after$.__audit_row_id, before$.__audit_row_id)
  valid <- !is.na(pos_before)
  if (!any(valid)) return(after)
  
  after_idx <- which(valid)
  before_idx <- pos_before[valid]
  
  old_id <- suppressWarnings(as.integer(as.character(before[[id_col]][before_idx])))
  new_id <- suppressWarnings(as.integer(as.character(after[[id_col]][after_idx])))
  
  changed <- !(is.na(old_id) & is.na(new_id)) & old_id != new_id
  if (!any(changed)) return(after)
  
  after_changed_idx <- after_idx[changed]
  before_changed_idx <- before_idx[changed]
  
  rows <- data.frame(
    action        = action,
    project       = .audit_project(after, after_changed_idx, project_col),
    sample        = sample,
    data_type     = data_type,
    id_col        = id_col,
    old_id        = old_id[changed],
    new_id        = new_id[changed],
    criterion     = criterion,
    limesurvey_id = .audit_col(before, before_changed_idx, c("limesurvey_id", "id")),
    startdate     = .audit_col(before, before_changed_idx, c("startdate")),
    submitdate    = .audit_col(before, before_changed_idx, c("submitdate")),
    datestamp     = .audit_col(before, before_changed_idx, c("datestamp")),
    TIME_start    = .audit_col(before, before_changed_idx, c("TIME_start")),
    TIME_end      = .audit_col(before, before_changed_idx, c("TIME_end")),
    gender        = .audit_col(before, before_changed_idx, c("gender", "sex", "Geschlecht")),
    age_years     = .audit_col(before, before_changed_idx, c("age_years", "age", "Alter")),
    stringsAsFactors = FALSE
  )
  
  .audit_add(rows)
  after
}

drop_audit_row_id <- function(df) {
  if (".__audit_row_id" %in% names(df)) {
    df$.__audit_row_id <- NULL
  }
  df
}

write_id_change_audit <- function(private_info_path, today) {
  audit_dir <- file.path(private_info_path, "id_change_audit")
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
  
  audit_file <- file.path(
    audit_dir,
    sprintf("%s_id_change_audit.xlsx", today)
  )
  
  if (!length(id_change_audit)) {
    empty <- data.frame(
      action = character(),
      project = character(),
      sample = character(),
      data_type = character(),
      id_col = character(),
      old_id = integer(),
      new_id = integer(),
      criterion = character(),
      limesurvey_id = character(),
      startdate = character(),
      submitdate = character(),
      datestamp = character(),
      TIME_start = character(),
      TIME_end = character(),
      gender = character(),
      age_years = character()
    )
    writexl::write_xlsx(list(id_changes = empty), audit_file)
    message("No ID changes logged. Wrote empty audit file: ", audit_file)
    return(invisible(empty))
  }
  
  audit <- dplyr::bind_rows(id_change_audit) %>%
    dplyr::mutate(
      project_num = suppressWarnings(as.integer(project))
    ) %>%
    dplyr::arrange(project_num, data_type, sample, old_id, new_id, action) %>%
    dplyr::select(-project_num)
  
  writexl::write_xlsx(list(id_changes = audit), audit_file)
  message("Wrote ID change audit: ", audit_file)
  invisible(audit)
}


# Set column name variables ----------------------------------------------------
vp_col     <- "vpid"
project_col<- "project"
last_page  <- "lastpage"
link_col   <- "comp"
id_col     <- "id"         # careful - psytoolkit: vpid; questionnaires: unique counter
submit_col <- "submitdate"

# --- Normalize ID column types (questionnaires) ---
for (nm in c("dat_adults","dat_adolescents","dat_children_parents","dat_children_p6","dat_parents_p6")) {
  if (exists(nm)) {
    df <- get(nm)
    if ("vpid"   %in% names(df)) df$vpid   <- suppressWarnings(as.integer(as.character(df$vpid)))
    if ("VPCode" %in% names(df)) df$VPCode <- suppressWarnings(as.integer(as.character(df$VPCode)))
    assign(nm, df)
  }
}

# --- Normalize ID column types (cogtests / PsyToolkit) ---
for (nm in c("psytool_info_adults","psytool_info_adolescents","psytool_info_children")) {
  if (exists(nm)) {
    df <- get(nm)
    if ("id" %in% names(df)) df$id <- suppressWarnings(as.integer(as.character(df$id)))
    assign(nm, df)
  }
}


# Fix issues with project assignment ------------------------------------------
# assuming this is project 2 since project 1 does not collect data and the id starts with a 2
dat_adults[[project_col]][which(dat_adults[[vp_col]] == 2048)]  <- 2
# assuming this is project 9 since project 1 does not collect data and the id starts with a 9.
# also project 9 IDs are actually consecutive and 17 is missing.
dat_adults[[project_col]][which(dat_adults[[vp_col]] == 99017)] <- 9


# Remove empty Rows ------------------------------------------------------------
# ---------- NA & factor SAFE helpers ----------
.as_num <- function(x) {
  if (is.factor(x)) {
    suppressWarnings(as.numeric(as.character(x)))
  } else {
    suppressWarnings(as.numeric(x))
  }
}

.pid_eq <- function(df, project_col, PROJECT) {
  if (!project_col %in% names(df)) return(rep(FALSE, nrow(df)))
  lab <- df[[project_col]]
  if (is.factor(lab)) lab <- as.character(lab)
  lab <- trimws(as.character(lab))
  dig <- gsub("\\D+", "", lab)
  dig == as.character(PROJECT)
}

.comp_is_cogn <- function(df, link_col) {
  if (!link_col %in% names(df)) return(rep(FALSE, nrow(df)))
  comp <- df[[link_col]]
  if (is.factor(comp)) comp <- as.character(comp)
  comp <- tolower(trimws(as.character(comp)))
  !is.na(comp) & comp == "cogn"
}

.lp_is_below <- function(df, last_page, thr) {
  if (!last_page %in% names(df)) return(rep(FALSE, nrow(df)))
  lp <- .as_num(df[[last_page]])
  !is.na(lp) & lp < thr
}

# Build mask: project == PROJECT & (comp == "cogn" OR lastpage < thr)
.drop_mask <- function(df, PROJECT, project_col, link_col, last_page, thr) {
  .pid_eq(df, project_col, PROJECT) &
    ( .comp_is_cogn(df, link_col) | .lp_is_below(df, last_page, thr) )
}

# tiny diag helper
.diag <- function(tag, df, mask) {
  cat(sprintf("%s: n=%d | drop=%d | keep=%d\n",
              tag, nrow(df), sum(mask, na.rm = TRUE), nrow(df) - sum(mask, na.rm = TRUE)))
}

# ------- coerce table types to same format ---------------#
# Example on your loaded tables:
dat_adults           <- coerce_by_schema(dat_adults,           schema)
dat_adolescents      <- coerce_by_schema(dat_adolescents,      schema)
dat_children_parents <- coerce_by_schema(dat_children_parents, schema)
dat_parents_p6       <- coerce_by_schema(dat_parents_p6,       schema)
dat_children_p6      <- coerce_by_schema(dat_children_p6,      schema)
dat_general          <- coerce_by_schema(dat_general,          schema)

dat_adolescents <- track_adolescent_vpids(dat_adolescents, "after coerce_by_schema")

# --- ALSO coerce PsyToolkit tables to the same schema --- #
psytool_info_adults      <- coerce_by_schema(psytool_info_adults,      schema)
psytool_info_adolescents <- coerce_by_schema(psytool_info_adolescents, schema)
psytool_info_children    <- coerce_by_schema(psytool_info_children,    schema)

# Be explicit for project + id to remove any ambiguity
if ("p"  %in% names(psytool_info_adults))      psytool_info_adults$p      <- as_int_safely(psytool_info_adults$p)
if ("p"  %in% names(psytool_info_adolescents)) psytool_info_adolescents$p <- as_int_safely(psytool_info_adolescents$p)
if ("p"  %in% names(psytool_info_children))    psytool_info_children$p    <- as_int_safely(psytool_info_children$p)

if ("id" %in% names(psytool_info_adults))      psytool_info_adults$id      <- as_int_safely(psytool_info_adults$id)
if ("id" %in% names(psytool_info_adolescents)) psytool_info_adolescents$id <- as_int_safely(psytool_info_adolescents$id)
if ("id" %in% names(psytool_info_children))    psytool_info_children$id    <- as_int_safely(psytool_info_children$id)

# --- Hard questionnaire exclusion: comp == "cogn" ---------------------------
# Rows with comp == "cogn" are routing records for cognitive testing only.
# They must never enter questionnaire outputs, even if administrative fields
# such as VPID, project, timestamps or lastpage are populated. The earlier
# project-specific empty-row rules did not cover Project 2, which allowed such
# rows (for example the cognitive-only row for VPID 20118) to survive until the
# duplicate check.
exclude_cogn_questionnaire_rows <- function(df,
                                            sample) {
  mask <- .comp_is_cogn(df, link_col)
  dropped <- df[mask, , drop = FALSE]

  if (nrow(dropped)) {
    dropped$.__reason__ <- "comp_cogn_cognitive_only"
    df <- df[!mask, , drop = FALSE]
  }

  message(
    "Questionnaire comp exclusion for ", sample, ": removed ",
    nrow(dropped), " row(s) with comp == 'cogn'."
  )

  list(clean = df, dropped = dropped)
}

cogn_exclusion_adults <- exclude_cogn_questionnaire_rows(
  dat_adults, "adults"
)
dat_adults <- cogn_exclusion_adults$clean

cogn_exclusion_adolescents <- exclude_cogn_questionnaire_rows(
  dat_adolescents, "adolescents"
)
dat_adolescents <- cogn_exclusion_adolescents$clean

cogn_exclusion_children_parents <- exclude_cogn_questionnaire_rows(
  dat_children_parents, "children_parents"
)
dat_children_parents <- cogn_exclusion_children_parents$clean

cogn_exclusion_children_p6 <- exclude_cogn_questionnaire_rows(
  dat_children_p6, "children_p6"
)
dat_children_p6 <- cogn_exclusion_children_p6$clean

cogn_exclusion_parents_p6 <- exclude_cogn_questionnaire_rows(
  dat_parents_p6, "parents_p6"
)
dat_parents_p6 <- cogn_exclusion_parents_p6$clean

# --- Project 4 Special-case Probanden-Fix: TIME_end (UTC) -> id = 40016 ----------------

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults$TIME_end == as.POSIXct("2025-10-23 06:39:00", tz = "UTC"),
  id_col = "id",
  new_id = 40016L,
  project_col = "p",
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 4 cogtests: row identified by exact TIME_end 2025-10-23 06:39:00 UTC corrected to ID 40016."
)

# ---------- Remove empty Rows ----------
LAST_P_EMPTY <- 7

## Project 3
PROJECT <- 3
mask_ad_3 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_3 <- dat_adults[ which(mask_ad_3), , drop = FALSE ]
dat_adults <- dat_adults[!mask_ad_3, , drop = FALSE]
.diag("P3 adults", dat_adults, mask_ad_3)

mask_ch_3 <- .drop_mask(dat_children_parents, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ch_3 <- dat_children_parents[ which(mask_ch_3), , drop = FALSE ]
dat_children_parents <- dat_children_parents[ !mask_ch_3, , drop = FALSE ]
.diag("P3 children", dat_children_parents, mask_ch_3)

## Project 4
PROJECT <- 4
mask_ch_4 <- .drop_mask(dat_children_parents, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ch_4 <- dat_children_parents[ which(mask_ch_4), , drop = FALSE ]
dat_children_parents <- dat_children_parents[ !mask_ch_4, , drop = FALSE ]
.diag("P4 children", dat_children_parents, mask_ch_4)

mask_ad_4 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_4 <- dat_adults[ which(mask_ad_4), , drop = FALSE ]
dat_adults <- dat_adults[ !mask_ad_4, , drop = FALSE ]
.diag("P4 adults", dat_adults, mask_ad_4)

## Project 5
PROJECT <- 5
mask_ad_5 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_5 <- dat_adults[ which(mask_ad_5), , drop = FALSE ]
dat_adults <- dat_adults[ !mask_ad_5, , drop = FALSE ]
.diag("P5 adults", dat_adults, mask_ad_5)

## Project 6 (different questionnaire; no project split for _p6 forms)
LAST_P_EMPTY <- 3
mask_ch_6 <- .lp_is_below(dat_children_p6, last_page, LAST_P_EMPTY)
empty_ch_6 <- dat_children_p6[ which(mask_ch_6), , drop = FALSE ]
dat_children_p6 <- dat_children_p6[ !mask_ch_6, , drop = FALSE ]
.diag("P6 children_p6", dat_children_p6, mask_ch_6)

mask_p_6 <- .lp_is_below(dat_parents_p6, last_page, LAST_P_EMPTY)
empty_p_6 <- dat_parents_p6[ which(mask_p_6), , drop = FALSE ]
dat_parents_p6 <- dat_parents_p6[ !mask_p_6, , drop = FALSE ]
.diag("P6 parents_p6", dat_parents_p6, mask_p_6)

## Project 7
PROJECT <- 7
LAST_P_EMPTY <- 7

mask_ad_7 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_7 <- dat_adults[ which(mask_ad_7), , drop = FALSE ]
dat_adults <- dat_adults[ !mask_ad_7, , drop = FALSE ]
.diag("P7 adults", dat_adults, mask_ad_7)

mask_adlsc_7 <- .drop_mask(dat_adolescents, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_adlsc_7 <- dat_adolescents[ which(mask_adlsc_7), , drop = FALSE ]
dat_adolescents <- dat_adolescents[ !mask_adlsc_7, , drop = FALSE ]
.diag("P7 adolescents", dat_adolescents, mask_adlsc_7)

## Project 8
PROJECT <- 8
mask_ad_8 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_8 <- dat_adults[ which(mask_ad_8), , drop = FALSE ]
dat_adults <- dat_adults[ !mask_ad_8, , drop = FALSE ]
.diag("P8 adults", dat_adults, mask_ad_8)

mask_ch_8 <- .drop_mask(dat_children_parents, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ch_8 <- dat_children_parents[ which(mask_ch_8), , drop = FALSE ]
dat_children_parents <- dat_children_parents[ !mask_ch_8, , drop = FALSE ]
.diag("P8 children", dat_children_parents, mask_ch_8)

## Project 9
PROJECT <- 9
mask_ad_9 <- .drop_mask(dat_adults, PROJECT, project_col, link_col, last_page, LAST_P_EMPTY)
empty_ad_9 <- dat_adults[ which(mask_ad_9), , drop = FALSE ]
dat_adults <- dat_adults[ !mask_ad_9, , drop = FALSE ]
.diag("P9 adults", dat_adults, mask_ad_9)
# ---- collect "empty" rows in the old shape so the next code stays flush ----

# helper: an empty data.frame with the same columns as a template df
.empty_like <- function(df) df[0, , drop = FALSE]

# Ensure each per-project "empty_*" object exists (even if 0 rows)
if (!exists("empty_ad_3"))      empty_ad_3      <- .empty_like(dat_adults)
if (!exists("empty_ch_3"))      empty_ch_3      <- .empty_like(dat_children_parents)
if (!exists("empty_ch_4"))      empty_ch_4      <- .empty_like(dat_children_parents)
if (!exists("empty_ad_4"))      empty_ad_4      <- .empty_like(dat_adults)
if (!exists("empty_ad_5"))      empty_ad_5      <- .empty_like(dat_adults)
if (!exists("empty_ch_6"))      empty_ch_6      <- .empty_like(dat_children_p6)
if (!exists("empty_p_6"))       empty_p_6       <- .empty_like(dat_parents_p6)
if (!exists("empty_ad_7"))      empty_ad_7      <- .empty_like(dat_adults)
if (!exists("empty_adlsc_7"))   empty_adlsc_7   <- .empty_like(dat_adolescents)
if (!exists("empty_ad_8"))      empty_ad_8      <- .empty_like(dat_adults)
if (!exists("empty_ch_8"))      empty_ch_8      <- .empty_like(dat_children_parents)
if (!exists("empty_ad_9"))      empty_ad_9      <- .empty_like(dat_adults)

# Recreate the legacy lists the downstream code expects
ads <- list(empty_ad_3, empty_ad_4, empty_ad_5, empty_ad_7, empty_ad_8, empty_ad_9)
ch  <- list(empty_ch_3, empty_ch_4, empty_ch_8)

# Keep only the non-empty ones
ads_non_empty <- ads[vapply(ads, nrow, integer(1)) > 0]
chs_non_empty <- ch[vapply(ch,  nrow, integer(1)) > 0]

# Bind them together (preserve columns; if none, create a truly empty like-template)
all_empty_ad <- if (length(ads_non_empty) > 0) {
  dplyr::bind_rows(ads_non_empty)
} else {
  .empty_like(dat_adults)
}

all_empty_ch <- if (length(chs_non_empty) > 0) {
  dplyr::bind_rows(chs_non_empty)
} else {
  .empty_like(dat_children_parents)
}

# Tag reason so later write-out works the same
all_empty_ad$.__reason__.   <- "empty"
all_empty_ch$.__reason__.   <- "empty"
empty_adlsc_7$.__reason__.  <- "empty"

# Retain the globally excluded cognitive-only routing rows in the discarded
# workbooks while preserving their more specific exclusion reason.
all_empty_ad <- dplyr::bind_rows(
  all_empty_ad,
  cogn_exclusion_adults$dropped
)
all_empty_ch <- dplyr::bind_rows(
  all_empty_ch,
  cogn_exclusion_children_parents$dropped,
  cogn_exclusion_children_p6$dropped,
  cogn_exclusion_parents_p6$dropped
)
empty_adlsc_7 <- dplyr::bind_rows(
  empty_adlsc_7,
  cogn_exclusion_adolescents$dropped
)

# Fix ID naming issues ---------------------------------------------------------
# All manual ID changes in this block are audited in id_change_audit.

# Project 2 --------------------------------------------------------------------
PROJECT <- 2

dat_adults <- audit_id_change(
  dat_adults,
  idx = dat_adults[[vp_col]] == 20035,
  id_col = vp_col,
  new_id = 20036,
  project_col = project_col,
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 2 questionnaire: known wrong VPID entry 20035 corrected to 20036."
)

dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 4    & dat_adults[[project_col]] == PROJECT, vp_col, 20004, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing project prefix; VPID 4 corrected to 20004.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 6    & dat_adults[[project_col]] == PROJECT, vp_col, 20006, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing project prefix; VPID 6 corrected to 20006.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 15   & dat_adults[[project_col]] == PROJECT, vp_col, 20015, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing project prefix; VPID 15 corrected to 20015.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 2023 & dat_adults[[project_col]] == PROJECT, vp_col, 20023, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing zero/project-prefix format; VPID 2023 corrected to 20023.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 26   & dat_adults[[project_col]] == PROJECT, vp_col, 20026, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing project prefix; VPID 26 corrected to 20026.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 35   & dat_adults[[project_col]] == PROJECT, vp_col, 20035, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing project prefix; VPID 35 corrected to 20035.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 2041 & dat_adults[[project_col]] == PROJECT, vp_col, 20041, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing zero/project-prefix format; VPID 2041 corrected to 20041.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 2044 & dat_adults[[project_col]] == PROJECT, vp_col, 20044, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing zero/project-prefix format; VPID 2044 corrected to 20044.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 2046 & dat_adults[[project_col]] == PROJECT, vp_col, 20046, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing zero/project-prefix format; VPID 2046 corrected to 20046.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 2048 & dat_adults[[project_col]] == PROJECT, vp_col, 20048, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing zero/project-prefix format; VPID 2048 corrected to 20048.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 2051 & dat_adults[[project_col]] == PROJECT, vp_col, 20051, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing zero/project-prefix format; VPID 2051 corrected to 20051.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 2052 & dat_adults[[project_col]] == PROJECT, vp_col, 20052, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing zero/project-prefix format; VPID 2052 corrected to 20052.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 2083 & dat_adults[[project_col]] == PROJECT, vp_col, 20083, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing zero/project-prefix format; VPID 2083 corrected to 20083.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 2084 & dat_adults[[project_col]] == PROJECT, vp_col, 20084, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing zero/project-prefix format; VPID 2084 corrected to 20084.")

# Project 2 questionnaire: 2092 appears twice.
# Earlier 2092 -> 20092; later 2092 -> 20093.
idx_2092_q <- which(dat_adults[[vp_col]] == 2092L & dat_adults[[project_col]] == PROJECT)
if (length(idx_2092_q) == 1L) {
  mask_2092_q <- rep(FALSE, nrow(dat_adults))
  mask_2092_q[idx_2092_q] <- TRUE
  dat_adults <- audit_id_change(dat_adults, mask_2092_q, vp_col, 20092, project_col, "adults", "questionnaire", "Project 2 questionnaire: VPID 2092 corrected to 20092.")
  rm(mask_2092_q)
} else if (length(idx_2092_q) > 1L) {
  time_2092_q <- as_time_safely(dat_adults$startdate[idx_2092_q])
  later_2092_q <- idx_2092_q[which.max(time_2092_q)]
  earlier_2092_q <- setdiff(idx_2092_q, later_2092_q)
  
  mask_2092_earlier_q <- rep(FALSE, nrow(dat_adults))
  mask_2092_earlier_q[earlier_2092_q] <- TRUE
  dat_adults <- audit_id_change(dat_adults, mask_2092_earlier_q, vp_col, 20092, project_col, "adults", "questionnaire", "Project 2 questionnaire: earlier VPID 2092 corrected to 20092.")
  rm(mask_2092_earlier_q)
  
  mask_2092_later_q <- rep(FALSE, nrow(dat_adults))
  mask_2092_later_q[later_2092_q] <- TRUE
  dat_adults <- audit_id_change(dat_adults, mask_2092_later_q, vp_col, 20093, project_col, "adults", "questionnaire", "Project 2 questionnaire: later VPID 2092 corrected to 20093.")
  rm(mask_2092_later_q)
}
rm(idx_2092_q)
if (exists("time_2092_q")) rm(time_2092_q)
if (exists("later_2092_q")) rm(later_2092_q)
if (exists("earlier_2092_q")) rm(earlier_2092_q)

dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 2097 & dat_adults[[project_col]] == PROJECT, vp_col, 20097, project_col, "adults", "questionnaire", "Project 2 questionnaire: missing zero/project-prefix format; VPID 2097 corrected to 20097.")


# Project 2: FHS-Antworten von 29135 gehören zu 20135.
# Nur die FHS-Items werden übertragen; anschließend wird die Zeile 29135 gelöscht.

if (!all(c("Item", "Scale") %in% names(item_info_adults))) {
  stop("FHS transfer: item_info_adults muss die Spalten 'Item' und 'Scale' enthalten.")
}

fhs_item_names <- trimws(as.character(item_info_adults$Item))
fhs_scale_names <- trimws(as.character(item_info_adults$Scale))

is_fhs_item <- !is.na(fhs_item_names) &
  (
    (!is.na(fhs_scale_names) & grepl("FHS", fhs_scale_names, ignore.case = TRUE)) |
      grepl("^FHS", fhs_item_names, ignore.case = TRUE)
  )

fhs_items <- unique(fhs_item_names[is_fhs_item])
fhs_items <- fhs_items[nzchar(fhs_items)]

if (!length(fhs_items)) {
  stop("FHS transfer: In item_info_adults wurden keine FHS-Items gefunden.")
}

missing_fhs_cols <- setdiff(fhs_items, names(dat_adults))
if (length(missing_fhs_cols)) {
  stop(
    "FHS transfer: Diese gemappten FHS-Items fehlen in dat_adults: ",
    paste(missing_fhs_cols, collapse = ", ")
  )
}

project_num_adults <- suppressWarnings(
  as.integer(as.character(dat_adults[[project_col]]))
)

idx_29135_fhs <- which(
  dat_adults[[vp_col]] == 29135L &
    project_num_adults == 2L
)

idx_20135_fhs <- which(
  dat_adults[[vp_col]] == 20135L &
    project_num_adults == 2L
)


# 20135 brach beim Ausfüllen ab. Der Neustart unter 29135 wurde erst ab dem
# FHS ernsthaft beantwortet. Daher alle FHS-Antworten vollständig überschreiben,
# auch wenn 20135 bereits Werte enthält oder 29135 einzelne fehlende Werte hat.
# Bei nicht vorhandener Quellzeile gibt es nichts zu übertragen/löschen.
if (length(idx_29135_fhs) > 0L) {
  if (length(idx_29135_fhs) != 1L || length(idx_20135_fhs) != 1L) {
    stop(
      "FHS transfer: expected exactly one source row 29135 and one target row 20135 in Project 2; found ",
      length(idx_29135_fhs), " source row(s) and ", length(idx_20135_fhs),
      " target row(s). No FHS values transferred and no source row deleted."
    )
  }

  for (item in fhs_items) {
    dat_adults[[item]][idx_20135_fhs] <- dat_adults[[item]][idx_29135_fhs]
  }

  dat_adults <- audit_row_action(
    dat_adults,
    idx = seq_len(nrow(dat_adults)) == idx_20135_fhs,
    id_col = vp_col,
    project_col = project_col,
    sample = "adults",
    data_type = "questionnaire",
    criterion = paste0(
      "Project 2 questionnaire: after questionnaire 20135 crashed, session 29135 was answered seriously only from FHS onward. All mapped FHS responses in 20135 were overwritten with the responses from 29135 (including missing values): ",
      paste(fhs_items, collapse = ", "), "."
    ),
    action = "fhs_overwritten",
    new_id = 20135L
  )

  # Die Quellzeile ausschließlich nach erfolgreichem Transfer entfernen.
  dat_adults <- audit_row_action(
    dat_adults,
    idx = seq_len(nrow(dat_adults)) == idx_29135_fhs,
    id_col = vp_col,
    project_col = project_col,
    sample = "adults",
    data_type = "questionnaire",
    criterion = paste0(
      "Project 2 questionnaire: FHS items (",
      paste(fhs_items, collapse = ", "),
      ") were transferred from VPID 29135 to VPID 20135; source row 29135 deleted."
    ),
    action = "deleted"
  )
}

# Project 3 --------------------------------------------------------------------
PROJECT <- 3

dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 1   & dat_adults[[project_col]] == PROJECT, vp_col, 30001, project_col, "adults", "questionnaire", "Project 3 questionnaire: missing project prefix; VPID 1 corrected to 30001.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 3   & dat_adults[[project_col]] == PROJECT, vp_col, 30003, project_col, "adults", "questionnaire", "Project 3 questionnaire: missing project prefix; VPID 3 corrected to 30003.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 8   & dat_adults[[project_col]] == PROJECT, vp_col, 30008, project_col, "adults", "questionnaire", "Project 3 questionnaire: missing project prefix; VPID 8 corrected to 30008.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 9   & dat_adults[[project_col]] == PROJECT, vp_col, 30009, project_col, "adults", "questionnaire", "Project 3 questionnaire: missing project prefix; VPID 9 corrected to 30009.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 104 & dat_adults[[project_col]] == PROJECT, vp_col, 30104, project_col, "adults", "questionnaire", "Project 3 questionnaire: missing project prefix; VPID 104 corrected to 30104.")

dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 10002 & dat_adults[[project_col]] == PROJECT, vp_col, 30002, project_col, "adults", "questionnaire", "Project 3 questionnaire: wrong initial project digit; VPID 10002 corrected to 30002.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 10005 & dat_adults[[project_col]] == PROJECT, vp_col, 30005, project_col, "adults", "questionnaire", "Project 3 questionnaire: wrong initial project digit; VPID 10005 corrected to 30005.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 10006 & dat_adults[[project_col]] == PROJECT, vp_col, 30006, project_col, "adults", "questionnaire", "Project 3 questionnaire: wrong initial project digit; VPID 10006 corrected to 30006.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 10007 & dat_adults[[project_col]] == PROJECT, vp_col, 30007, project_col, "adults", "questionnaire", "Project 3 questionnaire: wrong initial project digit; VPID 10007 corrected to 30007.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 40019 & dat_adults[[project_col]] == PROJECT, vp_col, 30019, project_col, "adults", "questionnaire", "Project 3 questionnaire: wrong initial project digit; VPID 40019 corrected to 30019.")

dat_adults <- audit_id_change(
  dat_adults,
  idx = dat_adults[[vp_col]] == 31016L &
    dat_adults[[project_col]] == PROJECT,
  id_col = vp_col,
  new_id = 32016L,
  project_col = project_col,
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 3 questionnaire: known wrong VPID 31016 corrected to 32016."
)

# Resolve the original 32015 session BEFORE assigning 30016 to 32015.
# The supplied questionnaire timestamp can be displayed in Berlin time or UTC.
# Refuse an ambiguous match rather than changing two different sessions.
idx_32015_p3_questionnaire_20241023 <- dat_adults[[vp_col]] == 32015L &
  dat_adults[[project_col]] == PROJECT &
  (
    .fmt_time_berlin(dat_adults$startdate) == "2024-10-23 11:24:16" |
      .questionnaire_time_matches_utc(
        dat_adults,
        "2024-10-23 11:24:16",
        time_cols = "startdate"
      )
  )

if (sum(idx_32015_p3_questionnaire_20241023, na.rm = TRUE) > 1L) {
  stop("Project 3 questionnaire: ambiguous match for original VPID 32015 at 2024-10-23 11:24:16 (Berlin/UTC).")
}

dat_adults <- audit_id_change(
  dat_adults,
  idx = idx_32015_p3_questionnaire_20241023,
  id_col = vp_col,
  new_id = 30015L,
  project_col = project_col,
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 3 questionnaire: original VPID 32015 at 2024-10-23 11:24:16 (Berlin or UTC display) corrected to 30015 before the separate 30016-to-32015 correction."
)
rm(idx_32015_p3_questionnaire_20241023)

dat_adults <- audit_id_change(
  dat_adults,
  idx = dat_adults[[vp_col]] == 30016L &
    dat_adults[[project_col]] == PROJECT,
  id_col = vp_col,
  new_id = 32015L,
  project_col = project_col,
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 3 questionnaire: known wrong VPID 30016 corrected to 32015."
)

dat_adults <- audit_id_change(
  dat_adults,
  idx = dat_adults[[vp_col]] == 30078L &
    dat_adults[[project_col]] == PROJECT &
    .date_berlin(dat_adults$startdate) == as.Date("2026-02-19"),
  id_col = vp_col,
  new_id = 32074L,
  project_col = project_col,
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 3 questionnaire: VPID 30078 with startdate 2026-02-19 corrected directly to 32074 before the general exp-2 relabeling."
)

# Two Project 3 questionnaire records from 2026-07-20 would otherwise both
# resolve to VPID 32026. Keep the earlier record as 32026 and assign 32027 to
# the record with the later start time before the general exp-2 relabeling.
idx_32026_questionnaire_20260720 <- which(
  dat_adults[[vp_col]] %in% c(30026L, 32026L) &
    dat_adults[[project_col]] == PROJECT &
    .date_berlin(dat_adults$startdate) == as.Date("2026-07-20")
)
time_32026_questionnaire_20260720 <- NULL
later_32026_questionnaire_idx <- NULL

if (length(idx_32026_questionnaire_20260720) >= 2L) {
  if (length(idx_32026_questionnaire_20260720) != 2L) {
    stop(
      "Project 3 questionnaire: expected exactly two records resolving to ID 32026 on 2026-07-20, found ",
      length(idx_32026_questionnaire_20260720),
      "."
    )
  }

  time_32026_questionnaire_20260720 <- as_time_safely(
    dat_adults$startdate[idx_32026_questionnaire_20260720],
    tz = "Europe/Berlin"
  )

  if (
    any(is.na(time_32026_questionnaire_20260720)) ||
      length(unique(time_32026_questionnaire_20260720)) != 2L
  ) {
    stop(
      "Project 3 questionnaire: the two records resolving to ID 32026 on 2026-07-20 need two valid, distinct start times."
    )
  }

  later_32026_questionnaire_idx <- idx_32026_questionnaire_20260720[
    which.max(time_32026_questionnaire_20260720)
  ]

  dat_adults <- audit_id_change(
    dat_adults,
    idx = seq_len(nrow(dat_adults)) == later_32026_questionnaire_idx,
    id_col = vp_col,
    new_id = 32027L,
    project_col = project_col,
    sample = "adults",
    data_type = "questionnaire",
    criterion = "Project 3 questionnaire: of the two records on 2026-07-20 that would resolve to VPID 32026, the record with the later startdate was corrected to 32027 before the general exp-2 relabeling."
  )
}

rm(
  idx_32026_questionnaire_20260720,
  time_32026_questionnaire_20260720,
  later_32026_questionnaire_idx
)

dat_adults <- audit_id_change(dat_adults, dat_adults[[id_col]] == 227 & dat_adults[[project_col]] == PROJECT, vp_col, 30047, project_col, "adults", "questionnaire", "Project 3 questionnaire: LimeSurvey response id 227 known to belong to VPID 30047.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[id_col]] == 316 & dat_adults[[project_col]] == PROJECT, vp_col, 30057, project_col, "adults", "questionnaire", "Project 3 questionnaire: LimeSurvey response id 316 known to belong to VPID 30057.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[id_col]] == 579 & dat_adults[[project_col]] == PROJECT, vp_col, 30100, project_col, "adults", "questionnaire", "Project 3 questionnaire: LimeSurvey response id 579 known to belong to VPID 30100.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[id_col]] == 606 & dat_adults[[project_col]] == PROJECT, vp_col, 30101, project_col, "adults", "questionnaire", "Project 3 questionnaire: LimeSurvey response id 606 known to belong to VPID 30101.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[id_col]] == 708 & dat_adults[[project_col]] == PROJECT, vp_col, 30112, project_col, "adults", "questionnaire", "Project 3 questionnaire: LimeSurvey response id 708 known to belong to VPID 30112.")

# Project 3 exp-2 relabeling: 30xxx -> 32xxx after boundary participant 30128.
dat_adults <- audit_snapshot(dat_adults)
before_p3_questionnaire_split <- dat_adults

p3_questionnaire_split <- p3_split_after_30128(
  df = dat_adults,
  id_col = "vpid",
  project_col = "project",
  start_col = "startdate",
  boundary_id = 30128L
)

dat_adults <- audit_id_diff_by_row(
  before = before_p3_questionnaire_split,
  after = p3_questionnaire_split$data,
  id_col = "vpid",
  project_col = "project",
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 3 questionnaire exp-2 split: rows with startdate later than boundary participant 30128 relabeled from 30xxx to 32xxx using 32000 + old_id %% 1000."
)

dat_adults <- drop_audit_row_id(dat_adults)

if ("vp_id" %in% names(dat_adults)) {
  dat_adults$vp_id <- dat_adults$vpid
}


# Project 4 --------------------------------------------------------------------
PROJECT <- 4

dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 4001 & dat_adults[[project_col]] == PROJECT, vp_col, 40001, project_col, "adults", "questionnaire", "Project 4 questionnaire: missing zero/project-prefix format; VPID 4001 corrected to 40001.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 4002 & dat_adults[[project_col]] == PROJECT, vp_col, 40002, project_col, "adults", "questionnaire", "Project 4 questionnaire: missing zero/project-prefix format; VPID 4002 corrected to 40002.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 4003 & dat_adults[[project_col]] == PROJECT, vp_col, 40003, project_col, "adults", "questionnaire", "Project 4 questionnaire: missing zero/project-prefix format; VPID 4003 corrected to 40003.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[vp_col]] == 4033 & dat_adults[[project_col]] == PROJECT, vp_col, 40033, project_col, "adults", "questionnaire", "Project 4 questionnaire: missing zero/project-prefix format; VPID 4033 corrected to 40033.")
dat_adults <- audit_id_change(dat_adults, dat_adults[[id_col]] == 630 & dat_adults[[project_col]] == PROJECT, vp_col, 40016, project_col, "adults", "questionnaire", "Project 4 questionnaire: LimeSurvey response id 630 known to belong to VPID 40016.")


# Project 7 --------------------------------------------------------------------
PROJECT <- 7

# VPID 70299 from 2026-05-13 is a mistyped questionnaire ID. Project 7 is
# collected in both samples, so apply the same narrowly dated correction to
# whichever questionnaire source contains the row.
dat_adults <- audit_id_change(
  dat_adults,
  idx = dat_adults[[vp_col]] == 70299L &
    dat_adults[[project_col]] == PROJECT &
    .date_berlin(dat_adults$startdate) == as.Date("2026-05-13"),
  id_col = vp_col,
  new_id = 70229L,
  project_col = project_col,
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 7 questionnaire: VPID 70299 with startdate 2026-05-13 corrected to 70229."
)

dat_adolescents <- audit_id_change(
  dat_adolescents,
  idx = dat_adolescents[[vp_col]] == 70299L &
    dat_adolescents[[project_col]] == PROJECT &
    .date_berlin(dat_adolescents$startdate) == as.Date("2026-05-13"),
  id_col = vp_col,
  new_id = 70229L,
  project_col = project_col,
  sample = "adolescents",
  data_type = "questionnaire",
  criterion = "Project 7 questionnaire: VPID 70299 with startdate 2026-05-13 corrected to 70229."
)

# Project 8 --------------------------------------------------------------------
PROJECT <- 8

# agecalc is not applicable to A-forms in the children/parents sample.
# Use R's standard missing value, consistent with the imported/exported data.
if (!all(c("project", "form", "agecalc") %in% names(dat_children_parents))) {
  stop("Project 8 agecalc correction: expected columns project, form and agecalc in children_parents.")
}
idx_p8_form_a_agecalc <- which(
  dat_children_parents$project == PROJECT &
    dat_children_parents$form == "A"
)
dat_children_parents$agecalc[idx_p8_form_a_agecalc] <- NA
rm(idx_p8_form_a_agecalc)

dat_children_parents <- audit_id_change(
  dat_children_parents,
  idx = dat_children_parents[[vp_col]] == 80418,
  id_col = vp_col,
  new_id = 80518,
  project_col = project_col,
  sample = "children_parents",
  data_type = "questionnaire",
  criterion = "Project 8 questionnaire children/parents: known wrong VPID 80418 corrected to 80518."
)

dat_children_parents <- audit_id_change(
  dat_children_parents,
  idx = dat_children_parents[[vp_col]] == 80553 &
    dat_children_parents[[project_col]] == PROJECT,
  id_col = vp_col,
  new_id = 80533L,
  project_col = project_col,
  sample = "children_parents",
  data_type = "questionnaire",
  criterion = "Project 8 questionnaire children/parents: known wrong VPID 80553 corrected to 80533."
)

dat_children_parents <- audit_id_change(
  dat_children_parents,
  idx = dat_children_parents[[vp_col]] == 80752L &
    dat_children_parents[[project_col]] == PROJECT &
    .date_berlin(dat_children_parents$startdate) < as.Date("2026-09-01"),
  id_col = vp_col,
  new_id = 80751L,
  project_col = project_col,
  sample = "children_parents",
  data_type = "questionnaire",
  criterion = "Project 8 questionnaire children/parents: known wrong VPID 80752 corrected to 80751 only for rows with startdate before 2026-09-01."
)

dat_children_parents <- audit_snapshot(dat_children_parents)
before_p8_child_questionnaire_mapping <- dat_children_parents

dat_children_parents <- correct_child_vpids(
  dat_children_parents,
  vpid_col = "vpid",
  project_col = "project",
  startdate_col = "startdate",
  mapping_file = file.path("information", "2025-08-19_Neuzuordnung_VP-IDs_Kinder-Sample_Projekt_8.xlsx")
)

dat_children_parents <- audit_id_diff_by_row(
  before = before_p8_child_questionnaire_mapping,
  after = dat_children_parents,
  id_col = "vpid",
  project_col = "project",
  sample = "children_parents",
  data_type = "questionnaire",
  criterion = "Project 8 questionnaire children/parents: VPID remapped using 2025-08-19_Neuzuordnung_VP-IDs_Kinder-Sample_Projekt_8.xlsx."
)

dat_children_parents <- drop_audit_row_id(dat_children_parents)


# Project 9 --------------------------------------------------------------------
PROJECT <- 9

dat_adults <- audit_id_change(
  dat_adults,
  idx = dat_adults[[vp_col]] == 9901,
  id_col = vp_col,
  new_id = 99001,
  project_col = project_col,
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 9 questionnaire: missing zero/project-prefix format; VPID 9901 corrected to 99001."
)

dat_adults <- audit_id_change(
  dat_adults,
  idx = dat_adults[[vp_col]] == 90696L,
  id_col = vp_col,
  new_id = 90027L,
  project_col = project_col,
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 9 questionnaire: known wrong VPID 90696 corrected to 90027."
)

# Exclude participant without data-use permission ------------------------------
# Participant 20080 did not allow use of their data.
# Therefore, neither questionnaire nor cogtest data may be used/exported.

dat_adults <- audit_row_action(
  dat_adults,
  idx = dat_adults$vpid == 20080L,
  id_col = "vpid",
  project_col = "project",
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Participant 20080 did not allow use of their data; questionnaire data deleted before pilot extraction/export.",
  action = "deleted"
)

# Gather Pilot Participant IDs -------------------------------------------------
pilots_ad_auto <- find_pilot_ids(dat_general, dat_adults)
pilots_asc_auto <- find_pilot_ids(dat_general, dat_adolescents)
pilots_ch_auto <- find_pilot_ids(dat_general, dat_children_parents)

# Confirmed main-study exception: 70084 is not a pilot even if the general
# participant overview causes find_pilot_ids() to classify it as one.
confirmed_main_asc_7 <- c(70084L)
auto_pilot_overlap_asc_7 <- intersect(
  suppressWarnings(as.integer(pilots_asc_auto)),
  confirmed_main_asc_7
)

if (length(auto_pilot_overlap_asc_7) > 0L) {
  dat_adolescents <- audit_row_action(
    dat_adolescents,
    idx = dat_adolescents$vpid %in% auto_pilot_overlap_asc_7,
    id_col = "vpid",
    project_col = "project",
    sample = "adolescents",
    data_type = "questionnaire",
    criterion = "Project 7 questionnaire adolescents: VPID 70084 was automatically classified as a pilot but is confirmed as a main-study participant; kept in the main-study data.",
    action = "pilot_status_corrected",
    new_id = 70084L
  )
}

pilots_asc_auto <- setdiff(
  suppressWarnings(as.integer(pilots_asc_auto)),
  confirmed_main_asc_7
)
rm(confirmed_main_asc_7, auto_pilot_overlap_asc_7)

pilot_ad_2 <- c(20004)
pilot_ad_9 <- c()
pilot_ad_8 <- c(80350)
pilot_asc_7 <- c()
pilot_ch_6 <- c(62973, 62980, 62998, 62992, 62987, 62989, 62994, 62970)
pilot_ch_8 <- c(80350)

pilot_ad_all <- c(pilot_ad_2, pilot_ad_9, pilot_ad_8, pilots_ad_auto)
pilot_asc_all <- c(pilots_asc_auto)
pilots_ch_all <- c(pilots_ch_auto, pilot_ch_6, pilot_ch_8)

# --- SAVE P9 (and all) pilot rows BEFORE extracting them out ------------------
pilot_quest_adults  <- dplyr::filter(dat_adults, .data[[vp_col]] %in% pilot_ad_all)

# Move to separate file and from original dataset -----------------------------
dat_adults <- extract_pilot_by_vpid(
  dat_adults, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilot_ad_all, sample = "adults", vpid_col = "vpid"
)
dat_adolescents <- extract_pilot_by_vpid(
  dat_adolescents, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilot_asc_all, sample = "adolescents", vpid_col = "vpid"
)
dat_children_parents <- extract_pilot_by_vpid(
  dat_children_parents, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilots_ch_all, sample = "children_parents", vpid_col = "vpid"
)
dat_children_p6 <- extract_pilot_by_vpid(
  dat_children_p6, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilots_ch_all, sample = "children_p6", vpid_col = "VPCode"
)
dat_parents_p6 <- extract_pilot_by_vpid(
  dat_parents_p6, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilots_ch_all, sample = "parents_p6", vpid_col = "VPCode"
)

# Handle duplicate IDs ---------------------------------------------------------
# Delete not needed, incomplete or faulty datasets

vp_col      <- "vpid"
project_col <- "project"
id_col      <- "id"

# Project 3 --------------------------------------------------------------------
# Hendrik said these can be deleted as incomplete/faulty.
# Additional Exp-1 questionnaire row:
#   VP_id 30100; LimeSurvey id 579; row 90; 2025-09-24 10:41:00
del_id_ad <- c(59L, 80L, 579L)

dat_adults <- audit_row_action(
  dat_adults,
  idx = dat_adults[[project_col]] == 3L & dat_adults[[id_col]] %in% del_id_ad,
  id_col = "vpid",
  project_col = "project",
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 3 questionnaire: LimeSurvey response id in c(59, 80, 579) was specified as incomplete/faulty and deleted.",
  action = "deleted"
)


# Project 6 --------------------------------------------------------------------
keep_row_id <- dat_children_parents %>%
  dplyr::mutate(start_dt = as.POSIXct(startdate), .row = dplyr::row_number()) %>%
  dplyr::filter(vpid == 62128, form == "C") %>%
  dplyr::arrange(start_dt, .row) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::pull(.row)

dat_children_parents <- dat_children_parents %>%
  dplyr::mutate(.row = dplyr::row_number())

dat_children_parents <- audit_row_action(
  dat_children_parents,
  idx = dat_children_parents$vpid == 62128 & dat_children_parents$form == "C" & dat_children_parents$.row != keep_row_id,
  id_col = "vpid",
  project_col = "project",
  sample = "children_parents",
  data_type = "questionnaire",
  criterion = "Project 6 questionnaire children/parents: duplicate C-form for VPID 62128; kept earliest row by startdate and row order, deleted the other row(s).",
  action = "deleted"
)

dat_children_parents <- dat_children_parents %>%
  dplyr::select(-.row)


# Project 7 --------------------------------------------------------------------
# VPID 70084: only the second adolescent questionnaire from 2025-07-04 is
# invalid. Retain the earlier valid questionnaire and the main-study status.
dat_adolescents <- audit_row_action(
  dat_adolescents,
  idx = dat_adolescents[[project_col]] == 7L &
    dat_adolescents[[vp_col]] == 70084L &
    .date_berlin(dat_adolescents$startdate) == as.Date("2025-07-04"),
  id_col = "vpid",
  project_col = "project",
  sample = "adolescents",
  data_type = "questionnaire",
  criterion = "Project 7 questionnaire adolescents: invalid second recording for VPID 70084 with startdate on 2025-07-04 deleted; the earlier valid session was retained.",
  action = "deleted"
)

# VPID 70088: keep the first adolescent session from 2025-06-24. The later
# session was an accidental restart of the questionnaire.
dat_adolescents <- audit_row_action(
  dat_adolescents,
  idx = dat_adolescents[[project_col]] == 7L &
    dat_adolescents[[vp_col]] == 70088L &
    .questionnaire_time_matches_utc(
      dat_adolescents,
      "2025-07-04 07:59:31"
    ),
  id_col = "vpid",
  project_col = "project",
  sample = "adolescents",
  data_type = "questionnaire",
  criterion = "Project 7 questionnaire adolescents: VPID 70088 session at 2025-07-04 07:59:31 UTC deleted because the questionnaire was accidentally started again; the earlier session from 2025-06-24 was retained.",
  action = "deleted"
)

# VPID 70091 belongs to the adolescent session from 2025-09-05. Delete the
# later adult record created after the ID had been reassigned.
dat_adults <- audit_row_action(
  dat_adults,
  idx = dat_adults[[project_col]] == 7L &
    dat_adults[[vp_col]] == 70091L &
    .questionnaire_time_matches_utc(
      dat_adults,
      "2026-04-09 12:59:24"
    ),
  id_col = "vpid",
  project_col = "project",
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 7 questionnaire adults: VPID 70091 session at 2026-04-09 12:59:24 UTC deleted because the valid assignment is the adolescent session from 2025-09-05.",
  action = "deleted"
)

# VPID 70164 belongs to the adult session from 2026-04-07. Delete the earlier
# adolescent record because the ID was subsequently reassigned to an adult.
dat_adolescents <- audit_row_action(
  dat_adolescents,
  idx = dat_adolescents[[project_col]] == 7L &
    dat_adolescents[[vp_col]] == 70164L &
    .questionnaire_time_matches_utc(
      dat_adolescents,
      "2026-03-02 12:12:00"
    ),
  id_col = "vpid",
  project_col = "project",
  sample = "adolescents",
  data_type = "questionnaire",
  criterion = "Project 7 questionnaire adolescents: VPID 70164 session at 2026-03-02 12:12:00 UTC deleted because the valid assignment is the adult session from 2026-04-07.",
  action = "deleted"
)

# Participants filled out questionnaires twice.
drop_ids_p7 <- c(70076L, 70072L, 70062L)

dat_adolescents <- dat_adolescents %>%
  dplyr::group_by(vpid, project) %>%
  dplyr::arrange(submitdate, .by_group = TRUE) %>%
  dplyr::mutate(.row_in_group = dplyr::row_number(), .n_in_group = dplyr::n()) %>%
  dplyr::ungroup()

dat_adolescents <- audit_row_action(
  dat_adolescents,
  idx = dat_adolescents$project == 7L &
    dat_adolescents$vpid %in% drop_ids_p7 &
    dat_adolescents$.n_in_group > 1L &
    dat_adolescents$.row_in_group == dat_adolescents$.n_in_group,
  id_col = "vpid",
  project_col = "project",
  sample = "adolescents",
  data_type = "questionnaire",
  criterion = "Project 7 questionnaire adolescents: duplicate questionnaire entries for VPIDs 70076, 70072, 70062; deleted the later entry by submitdate within vpid/project.",
  action = "deleted"
)

dat_adolescents <- dat_adolescents %>%
  dplyr::select(-.row_in_group, -.n_in_group)

dat_adolescents <- track_adolescent_vpids(dat_adolescents, "after project 7 duplicate filter")

# Project 7 adolescents: LimeSurvey response id 301 is a wrong duplicate/faulty entry.
dat_adolescents <- audit_row_action(
  dat_adolescents,
  idx = dat_adolescents[[project_col]] == 7L & dat_adolescents[[id_col]] == 301L,
  id_col = "vpid",
  project_col = "project",
  sample = "adolescents",
  data_type = "questionnaire",
  criterion = "Project 7 questionnaire adolescents: LimeSurvey response id 301 deleted.",
  action = "deleted"
)

# Project 7 adults: LimeSurvey response id 965 is the wrong duplicate entry.
dat_adults <- audit_row_action(
  dat_adults,
  idx = dat_adults[[project_col]] == 7L & dat_adults[[id_col]] == 965L,
  id_col = "vpid",
  project_col = "project",
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 7 questionnaire adults: LimeSurvey response id 965 is the wrong duplicate entry and was deleted.",
  action = "deleted"
)


# Project 8 --------------------------------------------------------------------
dat_children_parents <- dat_children_parents %>%
  dplyr::mutate(startdate_date = as.Date(startdate))

dat_children_parents <- audit_row_action(
  dat_children_parents,
  idx = dat_children_parents$vpid == 80505 &
    dat_children_parents$form == "P" &
    dat_children_parents$startdate_date == max(dat_children_parents$startdate_date[dat_children_parents$vpid == 80505 & dat_children_parents$form == "P"], na.rm = TRUE),
  id_col = "vpid",
  project_col = "project",
  sample = "children_parents",
  data_type = "questionnaire",
  criterion = "Project 8 questionnaire children/parents: duplicate P-form for VPID 80505; deleted the row with the latest startdate.",
  action = "deleted"
)

dat_children_parents <- dat_children_parents %>%
  dplyr::select(-startdate_date)

# Manual resolution of known duplicate form conflicts:
# 80521: keep first complete duplicate for form C and P
# 80529: keep first complete duplicate for form C
# 80523: for incomplete duplicate C-forms, keep the one with highest lastpage

dat_children_parents <- dat_children_parents %>%
  dplyr::mutate(
    .row = dplyr::row_number(),
    .lastpage_num = suppressWarnings(as.numeric(as.character(lastpage)))
  ) %>%
  dplyr::group_by(vpid, form) %>%
  dplyr::mutate(.row_in_group = dplyr::row_number()) %>%
  dplyr::ungroup()

dat_children_parents <- audit_row_action(
  dat_children_parents,
  idx = dat_children_parents$vpid == 80521 &
    dat_children_parents$form %in% c("C", "P") &
    dat_children_parents$.row_in_group > 1L,
  id_col = "vpid",
  project_col = "project",
  sample = "children_parents",
  data_type = "questionnaire",
  criterion = "Project 8 questionnaire children/parents: VPID 80521 duplicate C/P forms; kept first complete duplicate, deleted later duplicate(s).",
  action = "deleted"
)

dat_children_parents <- dat_children_parents %>%
  dplyr::group_by(vpid, form) %>%
  dplyr::mutate(.row_in_group = dplyr::row_number()) %>%
  dplyr::ungroup()

dat_children_parents <- audit_row_action(
  dat_children_parents,
  idx = dat_children_parents$vpid == 80529 &
    dat_children_parents$form == "C" &
    dat_children_parents$.row_in_group > 1L,
  id_col = "vpid",
  project_col = "project",
  sample = "children_parents",
  data_type = "questionnaire",
  criterion = "Project 8 questionnaire children/parents: VPID 80529 duplicate C-form; kept first complete duplicate, deleted later duplicate(s).",
  action = "deleted"
)

keep_80523_c_row <- dat_children_parents %>%
  dplyr::filter(vpid == 80523, form == "C") %>%
  dplyr::arrange(dplyr::desc(tidyr::replace_na(.lastpage_num, -Inf)), .row) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::pull(.row)

dat_children_parents <- audit_row_action(
  dat_children_parents,
  idx = dat_children_parents$vpid == 80523 &
    dat_children_parents$form == "C" &
    dat_children_parents$.row != keep_80523_c_row,
  id_col = "vpid",
  project_col = "project",
  sample = "children_parents",
  data_type = "questionnaire",
  criterion = "Project 8 questionnaire children/parents: VPID 80523 duplicate incomplete C-forms; kept row with highest lastpage, deleted other row(s).",
  action = "deleted"
)

dat_children_parents <- dat_children_parents %>%
  dplyr::select(-.row, -.lastpage_num, -.row_in_group)

# Leo says they are not complete and cannot be salvaged:
dat_adults <- audit_row_action(dat_adults, dat_adults$vpid == 80018 & dat_adults$project == 8L, "vpid", "project", "adults", "questionnaire", "Project 8 questionnaire adults: VPID 80018 not complete and cannot be salvaged; deleted.", "deleted")
dat_adults <- audit_row_action(dat_adults, dat_adults$vpid == 80009 & dat_adults$project == 8L, "vpid", "project", "adults", "questionnaire", "Project 8 questionnaire adults: VPID 80009 not complete and cannot be salvaged; deleted.", "deleted")
dat_adults <- audit_row_action(dat_adults, dat_adults$vpid == 80011 & dat_adults$project == 8L, "vpid", "project", "adults", "questionnaire", "Project 8 questionnaire adults: VPID 80011 not complete and cannot be salvaged; deleted.", "deleted")


# Project 9 --------------------------------------------------------------------
PROJECT <- 9

dat_adults <- audit_row_action(
  dat_adults,
  idx = dat_adults$vpid == 90002 & dat_adults$lastpage == 11,
  id_col = "vpid",
  project_col = "project",
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 9 questionnaire adults: VPID 90002 row with lastpage == 11 deleted.",
  action = "deleted"
)

dat_adults <- dat_adults %>%
  dplyr::group_by(vpid, project) %>%
  dplyr::arrange(dplyr::coalesce(submitdate, as.POSIXct("1900-01-01", tz = "Europe/Berlin")), .by_group = TRUE) %>%
  dplyr::mutate(.row_in_group = dplyr::row_number(), .n_in_group = dplyr::n()) %>%
  dplyr::ungroup()

dat_adults <- audit_row_action(
  dat_adults,
  idx = dat_adults$project == 9L &
    dat_adults$vpid == 90004L &
    dat_adults$.n_in_group > 1L &
    dat_adults$.row_in_group == dat_adults$.n_in_group,
  id_col = "vpid",
  project_col = "project",
  sample = "adults",
  data_type = "questionnaire",
  criterion = "Project 9 questionnaire adults: duplicate VPID 90004; deleted the later row by submitdate.",
  action = "deleted"
)

dat_adults <- dat_adults %>%
  dplyr::select(-.row_in_group, -.n_in_group)


# Remove all rows with zero non-admin answers ----------------------------------
drop_answer_empty <- function(df, admin_like = c(
  "vpid", "vp_id", "id", "project", "p", "proj", "comp", "submitdate", "startdate",
  "datestamp", "remid", "remidcheck", "warning", "consent", "end", "seed", "startlanguage"
)) {
  is_admin <- tolower(names(df)) %in% tolower(admin_like)
  sub <- df[, !is_admin, drop = FALSE]
  if (!ncol(sub)) return(df)
  non_empty <- rowSums(!vapply(sub, function(x) {
    if (is.character(x)) is.na(x) | trimws(x) == "" else is.na(x)
  }, logical(nrow(df)))) > 0
  df[non_empty, , drop = FALSE]
}

dat_adults            <- drop_answer_empty(dat_adults)
dat_adolescents       <- drop_answer_empty(dat_adolescents)
dat_children_parents  <- drop_answer_empty(dat_children_parents)
dat_children_p6       <- drop_answer_empty(dat_children_p6)
dat_parents_p6        <- drop_answer_empty(dat_parents_p6)


# Move to other sample ---------------------------------------------------------
# Adults -> Adolescents (Questionnaires + Cogtests)
move_ids_adults_to_adolescents <- c(70090L, 70153L)

audit_row_action(
  dat_adults,
  idx = dat_adults$vpid %in% move_ids_adults_to_adolescents,
  id_col = "vpid",
  project_col = "project",
  sample = "adults_to_adolescents",
  data_type = "questionnaire",
  criterion = "Questionnaire sample correction: participant belongs to adolescents sample, not adults sample.",
  action = "sample_moved",
  new_id = NA_integer_
)

audit_row_action(
  psytool_info_adults,
  idx = psytool_info_adults$id %in% move_ids_adults_to_adolescents,
  id_col = "id",
  project_col = "p",
  sample = "adults_to_adolescents",
  data_type = "experiment_data",
  criterion = "Cogtest sample correction: participant belongs to adolescents sample, not adults sample.",
  action = "sample_moved",
  new_id = NA_integer_
)

ids <- move_ids_adults_to_adolescents

# Questionnaires (key: vpid)
dat_adolescents <- dplyr::bind_rows(dat_adolescents, dplyr::filter(dat_adults, vpid %in% ids))
dat_adults      <- dplyr::filter(dat_adults, !vpid %in% ids)

# Cogtests / PsyToolkit (key: id)
psytool_info_adolescents <- dplyr::bind_rows(psytool_info_adolescents, dplyr::filter(psytool_info_adults, id %in% ids))
psytool_info_adults      <- dplyr::filter(psytool_info_adults, !id %in% ids)

# Auto-remove and check for remaining duplicates
# Adults
res_adults <- resolve_duplicates(dat_adults, vp_col, submit_col,
                                 dataset_name = "adults", data_type = "questionnaire",
                                 project_col, logger = logger)
dat_adults    <- res_adults$cleaned
trash_adults  <- res_adults$trash_bin

# insert manually merged project 9 subject
manual_90012 <- readxl::read_excel(
  file.path(name, private_info_path, "Backbone_90012.xlsx")
)

manual_90012_row <- manual_90012[2, , drop = FALSE]
manual_90012_row$vpid <- 90012L
manual_90012_row$project <- 9L

for (nm in setdiff(names(dat_adults), names(manual_90012_row))) {
  manual_90012_row[[nm]] <- NA
}

manual_90012_row <- manual_90012_row[, names(dat_adults), drop = FALSE]
manual_90012_row <- coerce_by_schema(manual_90012_row, schema)

# force exact class match to dat_adults
for (nm in intersect(names(manual_90012_row), names(dat_adults))) {
  target <- dat_adults[[nm]]
  
  if (inherits(target, "POSIXct")) {
    manual_90012_row[[nm]] <- as_time_safely(manual_90012_row[[nm]])
    
  } else if (is.integer(target)) {
    manual_90012_row[[nm]] <- as_int_safely(manual_90012_row[[nm]])
    
  } else if (is.numeric(target)) {
    manual_90012_row[[nm]] <- as_num_safely(manual_90012_row[[nm]])
    
  } else if (is.character(target)) {
    manual_90012_row[[nm]] <- as.character(manual_90012_row[[nm]])
  }
}

dat_adults <- dat_adults %>%
  dplyr::filter(!(vpid == 90012 & project == 9)) %>%
  dplyr::bind_rows(manual_90012_row)

# Adolescents
res_adolescents <- resolve_duplicates(dat_adolescents, vp_col, submit_col,
                                      dataset_name = "adolescents", data_type = "questionnaire",
                                      project_col, logger = logger)
dat_adolescents   <- res_adolescents$cleaned
trash_adolescents <- res_adolescents$trash_bin

dat_adolescents <- track_adolescent_vpids(dat_adolescents, "after resolve_duplicates adolescents")

# Project 7 is collected in both adults and adolescents. The ordinary duplicate
# pass runs within each sample, so explicitly flag IDs that remain in both
# samples. Do not remove either record automatically because the correct sample
# assignment requires manual review.
flag_cross_sample_duplicates(
  df_left = dat_adults,
  df_right = dat_adolescents,
  vp_col = "vpid",
  project_col = "project",
  project = 7L,
  sample_left = "adults",
  sample_right = "adolescents",
  data_type = "questionnaire",
  logger = logger
)

# Children/Parents
res_children_parents <- resolve_duplicates(
  dat_children_parents,
  vp_col,
  submit_col,
  dataset_name = "children_parents",
  data_type = "questionnaire",
  project_col = project_col,
  logger = logger,
  suppress_project8_form_warnings = TRUE
)
dat_children_parents  <- res_children_parents$cleaned
trash_children_parents<- res_children_parents$trash_bin

# Project 6 children parents
vp_col <- "VPCode"
res_children_p6 <- resolve_duplicates(dat_children_p6, vp_col, submit_col,
                                      dataset_name = "children_p6", data_type = "questionnaire",
                                      project_col, lastpage_threshold = 13, logger = logger)
dat_children_p6 <- res_children_p6$cleaned
trash_children_p6 <- res_children_p6$trash_bin

res_parents_p6 <- resolve_duplicates(dat_parents_p6, vp_col, submit_col,
                                     dataset_name = "parents_p6", data_type = "questionnaire",
                                     project_col, lastpage_threshold = 13, logger = logger)
dat_parents_p6 <- res_parents_p6$cleaned
trash_parents_p6 <- res_parents_p6$trash_bin

# Questionnaire quality flags and exclusions (rushing and straightlining) are
# calculated together in Step 03, using the raw questionnaire exports below.

# Special Case Project 8: Check C, P and A entries -----------------------------
check_vpid_forms(dat_children_parents, logger = logger)

# Save the Trash just to be safe -----------------------------------------------
all_trash_adults      <- dplyr::bind_rows(all_empty_ad,  trash_adults)
all_trash_children    <- dplyr::bind_rows(all_empty_ch,  trash_children_parents)
all_trash_adolescents <- dplyr::bind_rows(empty_adlsc_7, trash_adolescents)

write_xlsx(all_trash_adults,      file.path(out_path, "discarded", sprintf("deleted-rows_%s_adults.xlsx", today)))
write_xlsx(all_trash_children,    file.path(out_path, "discarded", sprintf("deleted-rows_%s_children.xlsx", today)))
write_xlsx(all_trash_adolescents, file.path(out_path, "discarded", sprintf("deleted-rows_%s_adolescents.xlsx", today)))

# Separate the data by project and store on disk -------------------------------

samples <- list(
  adults           = dat_adults,
  adolescents      = dat_adolescents,
  children_parents = dat_children_parents,
  children_p6      = dat_children_p6,
  parents_p6       = dat_parents_p6
)

# Final hard stop immediately before questionnaire slicing/export. This guards
# against a later processing step accidentally reintroducing cognitive-only
# routing rows.
remaining_cogn_rows <- vapply(
  samples,
  function(df) sum(.comp_is_cogn(df, link_col), na.rm = TRUE),
  integer(1)
)
if (any(remaining_cogn_rows > 0L)) {
  stop(
    "Questionnaire export blocked: comp == 'cogn' rows remain in sample(s): ",
    paste(
      paste0(names(remaining_cogn_rows)[remaining_cogn_rows > 0L],
             " (n=", remaining_cogn_rows[remaining_cogn_rows > 0L], ")"),
      collapse = ", "
    )
  )
}
message("Questionnaire export integrity check passed: no comp == 'cogn' rows remain.")

# ---- Deterministic questionnaire ID schema before slicing/export --------------

samples <- lapply(names(samples), function(s) {
  normalize_questionnaire_ids(samples[[s]], sample = s)
})
names(samples) <- c("adults", "adolescents", "children_parents", "children_p6", "parents_p6")

# 1) prepare all preps first
preps <- lapply(names(samples), function(s) {
  prepare_project_slices(samples[[s]], out_path = out_path, sample = s,
                         data_type = "questionnaires", metadata_info = quest_info)
})

names(preps) <- names(samples)

# Write raw questionnaire exports. Step 03 computes all quality flags.
lapply(names(preps), function(s) write_project_slices(preps[[s]]))

# -------------------------------------------------------------------------
# Additional Project 3 questionnaire exports: exp_1 vs exp_2
# -------------------------------------------------------------------------

# Prefer the prepared questionnaire export if available.
p3_questionnaire_export <- if (exists("data_adults_p_3_questionnaire", envir = .GlobalEnv)) {
  get("data_adults_p_3_questionnaire", envir = .GlobalEnv)
} else {
  normalize_questionnaire_ids(
    dplyr::filter(dat_adults, project == 3),
    sample = "adults"
  )
}

p3_questionnaire_export$vp_id <- suppressWarnings(as.integer(as.character(p3_questionnaire_export$vp_id)))

data_adults_exp_1_p_3_questionnaire <- p3_questionnaire_export %>%
  dplyr::filter(vp_id >= 30000L, vp_id <= 30128L)

data_adults_exp_2_p_3_questionnaire <- p3_questionnaire_export %>%
  dplyr::filter(vp_id >= 32000L, vp_id < 33000L)

write_project3_exp_split_exports(
  exp1_df = data_adults_exp_1_p_3_questionnaire,
  exp2_df = data_adults_exp_2_p_3_questionnaire,
  out_path = out_path,
  sample_label = "adults",
  data_type = "questionnaires"
)


##########################################################################
## Data Cleaning for Cognitive Test Data ---------------------------------
##########################################################################

# Set column name variables ----------------------------------------------------
vp_col     <- "id"
project_col<- "p"
# last_page <- "lastpage"
link_col   <- "comp"
id_col     <- NA    # psytoolkit info sheets: vpid; questionnaires: unique increment counter
submit_col <- "TIME_end"
start_col  <- "TIME_start"

# Fix issues with project assignment ------------------------------------------
psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 2048)]  <- 2
psytool_info_adults[[project_col]][which(psytool_info_adults[[vp_col]] == 99017)] <- 9

# Project 6 adults: resolve the two confirmed cognitive-test ID conflicts.
# Match the supplied UTC instants explicitly: TIME_start is stored as POSIXct
# after importing PsyToolkit's Berlin-local timestamps. Apply these corrections
# before generic empty-row/duplicate handling so their reasons are audited.
p6_adult_cogtest_start_utc <- format(
  as_time_safely(psytool_info_adults$TIME_start, tz = "Europe/Berlin"),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults$id == 62224L &
    psytool_info_adults$p == 6L &
    p6_adult_cogtest_start_utc == "2026-06-12 08:14:00",
  id_col = "id",
  new_id = 62204L,
  project_col = "p",
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 6 adult cogtests: ID conflict; session 62224 at 2026-06-12 08:14:00 UTC belongs to 62204. The separate session at 2026-09-10 13:10:00 UTC retains ID 62224."
)

psytool_info_adults <- audit_row_action(
  psytool_info_adults,
  idx = psytool_info_adults$id == 62206L &
    psytool_info_adults$p == 6L &
    p6_adult_cogtest_start_utc == "2026-06-15 09:17:00",
  id_col = "id",
  project_col = "p",
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 6 adult cogtests: unused preparation for ID 62206 at 2026-06-15 09:17:00 UTC deleted after a last-minute rescheduling. The actual session at 2026-06-15 13:10:00 UTC is retained.",
  action = "deleted"
)
rm(p6_adult_cogtest_start_utc)

# Remove empty Rows ------------------------------------------------------------
list_output           <- partition_empty_obs_psytoolkit(psytool_info_adults)
psytool_info_adults   <- list_output$kept
no_id_ad              <- list_output$no_id
empty_rows_ad         <- list_output$empty
# TODO: need to understand how it is possible to generate entries without ID - and possibly reconstruct?

list_output               <- partition_empty_obs_psytoolkit(psytool_info_adolescents)
psytool_info_adolescents  <- list_output$kept
no_id_adlsc               <- list_output$no_id
empty_rows_adlsc          <- list_output$empty

list_output            <- partition_empty_obs_psytoolkit(psytool_info_children)
psytool_info_children  <- list_output$kept
no_id_ch               <- list_output$no_id
empty_rows_ch          <- list_output$empty

# Fix ID naming issues ---------------------------------------------------------
# All manual ID changes in this block are audited in id_change_audit.

# Project 2 --------------------------------------------------------------------
PROJECT <- 2

psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 20035, vp_col, 20036, project_col, "adults", "experiment_data", "Project 2 cogtests: known wrong VPID entry 20035 corrected to 20036.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 4    & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20004, project_col, "adults", "experiment_data", "Project 2 cogtests: missing project prefix; ID 4 corrected to 20004.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 6    & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20006, project_col, "adults", "experiment_data", "Project 2 cogtests: missing project prefix; ID 6 corrected to 20006.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 15   & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20015, project_col, "adults", "experiment_data", "Project 2 cogtests: missing project prefix; ID 15 corrected to 20015.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 2023 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20023, project_col, "adults", "experiment_data", "Project 2 cogtests: missing zero/project-prefix format; ID 2023 corrected to 20023.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 26   & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20026, project_col, "adults", "experiment_data", "Project 2 cogtests: missing project prefix; ID 26 corrected to 20026.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 35   & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20035, project_col, "adults", "experiment_data", "Project 2 cogtests: missing project prefix; ID 35 corrected to 20035.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 2041 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20041, project_col, "adults", "experiment_data", "Project 2 cogtests: missing zero/project-prefix format; ID 2041 corrected to 20041.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 2044 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20044, project_col, "adults", "experiment_data", "Project 2 cogtests: missing zero/project-prefix format; ID 2044 corrected to 20044.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 2046 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20046, project_col, "adults", "experiment_data", "Project 2 cogtests: missing zero/project-prefix format; ID 2046 corrected to 20046.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 2048 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20048, project_col, "adults", "experiment_data", "Project 2 cogtests: missing zero/project-prefix format; ID 2048 corrected to 20048.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 2051 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20051, project_col, "adults", "experiment_data", "Project 2 cogtests: missing zero/project-prefix format; ID 2051 corrected to 20051.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 2052 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20052, project_col, "adults", "experiment_data", "Project 2 cogtests: missing zero/project-prefix format; ID 2052 corrected to 20052.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 2083 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20083, project_col, "adults", "experiment_data", "Project 2 cogtests: missing zero/project-prefix format; ID 2083 corrected to 20083.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 2084 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20084, project_col, "adults", "experiment_data", "Project 2 cogtests: missing zero/project-prefix format; ID 2084 corrected to 20084.")

# Project 2 cogtests: 2092 appears twice.
# Earlier 2092 -> 20092; later 2092 -> 20093.
idx_2092_cog <- which(psytool_info_adults[[vp_col]] == 2092L & psytool_info_adults[[project_col]] == PROJECT)
if (length(idx_2092_cog) == 1L) {
  mask_2092_cog <- rep(FALSE, nrow(psytool_info_adults))
  mask_2092_cog[idx_2092_cog] <- TRUE
  psytool_info_adults <- audit_id_change(psytool_info_adults, mask_2092_cog, vp_col, 20092, project_col, "adults", "experiment_data", "Project 2 cogtests: ID 2092 corrected to 20092.")
  rm(mask_2092_cog)
} else if (length(idx_2092_cog) > 1L) {
  time_2092_cog <- as_time_safely(psytool_info_adults[[start_col]][idx_2092_cog])
  later_2092_cog <- idx_2092_cog[which.max(time_2092_cog)]
  earlier_2092_cog <- setdiff(idx_2092_cog, later_2092_cog)
  
  mask_2092_earlier_cog <- rep(FALSE, nrow(psytool_info_adults))
  mask_2092_earlier_cog[earlier_2092_cog] <- TRUE
  psytool_info_adults <- audit_id_change(psytool_info_adults, mask_2092_earlier_cog, vp_col, 20092, project_col, "adults", "experiment_data", "Project 2 cogtests: earlier ID 2092 corrected to 20092.")
  rm(mask_2092_earlier_cog)
  
  mask_2092_later_cog <- rep(FALSE, nrow(psytool_info_adults))
  mask_2092_later_cog[later_2092_cog] <- TRUE
  psytool_info_adults <- audit_id_change(psytool_info_adults, mask_2092_later_cog, vp_col, 20093, project_col, "adults", "experiment_data", "Project 2 cogtests: later ID 2092 corrected to 20093.")
  rm(mask_2092_later_cog)
}
rm(idx_2092_cog)
if (exists("time_2092_cog")) rm(time_2092_cog)
if (exists("later_2092_cog")) rm(later_2092_cog)
if (exists("earlier_2092_cog")) rm(earlier_2092_cog)

psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 2097 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 20097, project_col, "adults", "experiment_data", "Project 2 cogtests: missing zero/project-prefix format; ID 2097 corrected to 20097.")

psytool_info_adults <- psytool_info_adults %>%
  dplyr::group_by(.data[[vp_col]]) %>%
  dplyr::mutate(.change_20069_to_20066 = .data[[vp_col]] == 20069 & .data[[start_col]] == max(.data[[start_col]])) %>%
  dplyr::ungroup()

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults$.change_20069_to_20066,
  id_col = vp_col,
  new_id = 20066,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 2 cogtests: for duplicate/misassigned ID 20069, row with latest TIME_start corrected to 20066."
)

psytool_info_adults$.change_20069_to_20066 <- NULL


# Project 3 --------------------------------------------------------------------
PROJECT <- 3

psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 1 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 30001, project_col, "adults", "experiment_data", "Project 3 cogtests: missing project prefix; ID 1 corrected to 30001.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 3 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 30003, project_col, "adults", "experiment_data", "Project 3 cogtests: missing project prefix; ID 3 corrected to 30003.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 8 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 30008, project_col, "adults", "experiment_data", "Project 3 cogtests: missing project prefix; ID 8 corrected to 30008.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 9 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 30009, project_col, "adults", "experiment_data", "Project 3 cogtests: missing project prefix; ID 9 corrected to 30009.")

psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 10002 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 30002, project_col, "adults", "experiment_data", "Project 3 cogtests: wrong initial project digit; ID 10002 corrected to 30002.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 10005 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 30005, project_col, "adults", "experiment_data", "Project 3 cogtests: wrong initial project digit; ID 10005 corrected to 30005.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 10006 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 30006, project_col, "adults", "experiment_data", "Project 3 cogtests: wrong initial project digit; ID 10006 corrected to 30006.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 10007 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 30007, project_col, "adults", "experiment_data", "Project 3 cogtests: wrong initial project digit; ID 10007 corrected to 30007.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 40019 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 30019, project_col, "adults", "experiment_data", "Project 3 cogtests: wrong initial project digit; ID 40019 corrected to 30019.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 104 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 30104, project_col, "adults", "experiment_data", "Project 3 cogtests: missing project prefix; ID 104 corrected to 30104.")

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults[[vp_col]] == 31016L &
    psytool_info_adults[[project_col]] == PROJECT,
  id_col = vp_col,
  new_id = 32016L,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 3 cogtests: known wrong ID 31016 corrected to 32016."
)

# Match the original 32015 cogtest on the questionnaire's collection date.
# Apply this first so the separate 30016-to-32015 correction is preserved.
psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults$id == 32015L &
    psytool_info_adults$p == PROJECT &
    .date_berlin(psytool_info_adults$TIME_start) == as.Date("2024-10-23"),
  id_col = "id",
  new_id = 30015L,
  project_col = "p",
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 3 cogtests: ID 32015 with TIME_start on 2024-10-23 corrected to 30015."
)

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults[[vp_col]] == 30016L &
    psytool_info_adults[[project_col]] == PROJECT,
  id_col = vp_col,
  new_id = 32015L,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 3 cogtests: known wrong ID 30016 corrected to 32015."
)

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults[[vp_col]] == 30078L &
    psytool_info_adults[[project_col]] == PROJECT &
    .date_berlin(psytool_info_adults$TIME_start) == as.Date("2026-02-19"),
  id_col = vp_col,
  new_id = 32074L,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 3 cogtests: ID 30078 with TIME_start 2026-02-19 corrected directly to 32074 before the general exp-2 relabeling."
)

# Apply the same time-based distinction to the Project 3 cogtests: of the two
# records from 2026-07-20 that would otherwise resolve to ID 32026, the later
# one becomes 32027 before the general exp-2 relabeling.
idx_32026_cogtest_20260720 <- which(
  psytool_info_adults[[vp_col]] %in% c(30026L, 32026L) &
    psytool_info_adults[[project_col]] == PROJECT &
    .date_berlin(psytool_info_adults$TIME_start) == as.Date("2026-07-20")
)
time_32026_cogtest_20260720 <- NULL
later_32026_cogtest_idx <- NULL

if (length(idx_32026_cogtest_20260720) >= 2L) {
  if (length(idx_32026_cogtest_20260720) != 2L) {
    stop(
      "Project 3 cogtests: expected exactly two records resolving to ID 32026 on 2026-07-20, found ",
      length(idx_32026_cogtest_20260720),
      "."
    )
  }

  time_32026_cogtest_20260720 <- as_time_safely(
    psytool_info_adults$TIME_start[idx_32026_cogtest_20260720],
    tz = "Europe/Berlin"
  )

  if (
    any(is.na(time_32026_cogtest_20260720)) ||
      length(unique(time_32026_cogtest_20260720)) != 2L
  ) {
    stop(
      "Project 3 cogtests: the two records resolving to ID 32026 on 2026-07-20 need two valid, distinct TIME_start values."
    )
  }

  later_32026_cogtest_idx <- idx_32026_cogtest_20260720[
    which.max(time_32026_cogtest_20260720)
  ]

  psytool_info_adults <- audit_id_change(
    psytool_info_adults,
    idx = seq_len(nrow(psytool_info_adults)) == later_32026_cogtest_idx,
    id_col = vp_col,
    new_id = 32027L,
    project_col = project_col,
    sample = "adults",
    data_type = "experiment_data",
    criterion = "Project 3 cogtests: of the two records on 2026-07-20 that would resolve to ID 32026, the record with the later TIME_start was corrected to 32027 before the general exp-2 relabeling."
  )
}

rm(
  idx_32026_cogtest_20260720,
  time_32026_cogtest_20260720,
  later_32026_cogtest_idx
)

psytool_info_adults$id <- suppressWarnings(as.integer(psytool_info_adults$id))

psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults$id == 30048L & psytool_info_adults$p == 3L & psytool_info_adults$TIME_start == max(psytool_info_adults$TIME_start[psytool_info_adults$id == 30048L & psytool_info_adults$p == 3L], na.rm = TRUE), "id", 30047, "p", "adults", "experiment_data", "Project 3 cogtests: falsely named dataset; ID 30048 row with latest TIME_start corrected to 30047.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults$id == 30058L & psytool_info_adults$p == 3L & psytool_info_adults$TIME_start == max(psytool_info_adults$TIME_start[psytool_info_adults$id == 30058L & psytool_info_adults$p == 3L], na.rm = TRUE), "id", 30057, "p", "adults", "experiment_data", "Project 3 cogtests: falsely named dataset; ID 30058 row with latest TIME_start corrected to 30057.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults$id == 30099L & psytool_info_adults$p == 3L & psytool_info_adults$TIME_start == min(psytool_info_adults$TIME_start[psytool_info_adults$id == 30099L & psytool_info_adults$p == 3L], na.rm = TRUE), "id", 30100, "p", "adults", "experiment_data", "Project 3 cogtests: falsely named dataset; ID 30099 row with earliest TIME_start corrected to 30100.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults$id == 30111L & psytool_info_adults$p == 3L & psytool_info_adults$TIME_start == min(psytool_info_adults$TIME_start[psytool_info_adults$id == 30111L & psytool_info_adults$p == 3L], na.rm = TRUE), "id", 30112, "p", "adults", "experiment_data", "Project 3 cogtests: falsely named dataset; ID 30111 row with earliest TIME_start corrected to 30112.")

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults[[vp_col]] == 219 & psytool_info_adults[[project_col]] == PROJECT,
  id_col = vp_col,
  new_id = 30002,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 3 cogtests: ID 219 in project 3 corrected to 30002."
)

# Project 3 exp-2 relabeling: 30xxx -> 32xxx after boundary participant 30128.
psytool_info_adults <- audit_snapshot(psytool_info_adults)
before_p3_cogtest_split <- psytool_info_adults

p3_cogtest_split <- p3_split_after_30128(
  df = psytool_info_adults,
  id_col = "id",
  project_col = "p",
  start_col = "TIME_start",
  boundary_id = 30128L
)

psytool_info_adults <- audit_id_diff_by_row(
  before = before_p3_cogtest_split,
  after = p3_cogtest_split$data,
  id_col = "id",
  project_col = "p",
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 3 cogtests exp-2 split: rows with TIME_start later than boundary participant 30128 relabeled from 30xxx to 32xxx using 32000 + old_id %% 1000."
)

psytool_info_adults <- drop_audit_row_id(psytool_info_adults)


# Project 4 --------------------------------------------------------------------
PROJECT <- 4

psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 4001 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 40001, project_col, "adults", "experiment_data", "Project 4 cogtests: missing zero/project-prefix format; ID 4001 corrected to 40001.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 4002 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 40002, project_col, "adults", "experiment_data", "Project 4 cogtests: missing zero/project-prefix format; ID 4002 corrected to 40002.")
psytool_info_adults <- audit_id_change(psytool_info_adults, psytool_info_adults[[vp_col]] == 4003 & psytool_info_adults[[project_col]] == PROJECT, vp_col, 40003, project_col, "adults", "experiment_data", "Project 4 cogtests: missing zero/project-prefix format; ID 4003 corrected to 40003.")


# Project 8 cogtests: correct only the mistyped ID for this dated session.
# The existing project assignment p = 8 is correct and stays unchanged.
psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults[[vp_col]] == 50056L &
    psytool_info_adults[[project_col]] == 8L &
    .date_berlin(psytool_info_adults$TIME_start) == as.Date("2026-02-03"),
  id_col = vp_col,
  new_id = 80056L,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 8 cogtests: ID 50056 with TIME_start on 2026-02-03 corrected to 80056; the correct project assignment p = 8 was retained."
)

# Project 8 --------------------------------------------------------------------
PROJECT <- 8

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults[[vp_col]] == 800028 & psytool_info_adults[[project_col]] == PROJECT,
  id_col = vp_col,
  new_id = 80028,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 8 cogtests: typo/extra zero; ID 800028 corrected to 80028."
)

# Project 8: wrong cogtest IDs
psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults[[vp_col]] == 80144L &
    psytool_info_adults[[project_col]] == PROJECT,
  id_col = vp_col,
  new_id = 80090L,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 8 cogtests adults: known wrong ID 80144 corrected to 80090."
)

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults[[vp_col]] == 80114L &
    psytool_info_adults[[project_col]] == PROJECT &
    .date_berlin(psytool_info_adults$TIME_start) == as.Date("2026-06-25"),
  id_col = vp_col,
  new_id = 80090L,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 8 cogtests adults: ID 80114 with TIME_start 2026-06-25 corrected to 80090."
)

psytool_info_children <- audit_id_change(
  psytool_info_children,
  idx = psytool_info_children[[vp_col]] == 80418L &
    psytool_info_children[[project_col]] == PROJECT,
  id_col = vp_col,
  new_id = 80518L,
  project_col = project_col,
  sample = "children_parents",
  data_type = "experiment_data",
  criterion = "Project 8 cogtests children/parents: known wrong ID 80418 corrected to 80518."
)

psytool_info_children <- audit_id_change(
  psytool_info_children,
  idx = psytool_info_children[[vp_col]] == 80553L &
    psytool_info_children[[project_col]] == PROJECT,
  id_col = vp_col,
  new_id = 80533L,
  project_col = project_col,
  sample = "children_parents",
  data_type = "experiment_data",
  criterion = "Project 8 cogtests children/parents: known wrong ID 80553 corrected to 80533."
)

psytool_info_children <- audit_id_change(
  psytool_info_children,
  idx = psytool_info_children[[vp_col]] == 80752L &
    psytool_info_children[[project_col]] == PROJECT &
    .date_berlin(psytool_info_children$TIME_start) < as.Date("2026-09-01"),
  id_col = vp_col,
  new_id = 80751L,
  project_col = project_col,
  sample = "children_parents",
  data_type = "experiment_data",
  criterion = "Project 8 cogtests children/parents: known wrong ID 80752 corrected to 80751 only for rows with TIME_start before 2026-09-01."
)

psytool_info_children <- audit_snapshot(psytool_info_children)
before_p8_child_cogtest_mapping <- psytool_info_children

psytool_info_children <- correct_child_vpids(
  psytool_info_children,
  vpid_col = "id",
  project_col = "p",
  startdate_col = "TIME_start",
  mapping_file = file.path("information", "2025-08-19_Neuzuordnung_VP-IDs_Kinder-Sample_Projekt_8.xlsx")
)

psytool_info_children <- audit_id_diff_by_row(
  before = before_p8_child_cogtest_mapping,
  after = psytool_info_children,
  id_col = "id",
  project_col = "p",
  sample = "children_parents",
  data_type = "experiment_data",
  criterion = "Project 8 cogtests children/parents: VPID remapped using 2025-08-19_Neuzuordnung_VP-IDs_Kinder-Sample_Projekt_8.xlsx."
)

psytool_info_children <- drop_audit_row_id(psytool_info_children)


# Project 9 --------------------------------------------------------------------
PROJECT <- 9

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults[[vp_col]] == 9901,
  id_col = vp_col,
  new_id = 99001,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 9 cogtests: missing zero/project-prefix format; ID 9901 corrected to 99001."
)

psytool_info_adults <- audit_id_change(
  psytool_info_adults,
  idx = psytool_info_adults[[vp_col]] == 90696L,
  id_col = vp_col,
  new_id = 90027L,
  project_col = project_col,
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 9 cogtests: known wrong ID 90696 corrected to 90027."
)

# Exclude participant without data-use permission ------------------------------
# Participant 20080 did not allow use of their data.
# Therefore, neither questionnaire nor cogtest data may be used/exported.

psytool_info_adults <- audit_row_action(
  psytool_info_adults,
  idx = psytool_info_adults$id == 20080L,
  id_col = "id",
  project_col = "p",
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Participant 20080 did not allow use of their data; cogtest data deleted before pilot extraction/export.",
  action = "deleted"
)


# Gather Pilot Participant IDs -------------------------------------------------
pilots_ad_auto  <- find_pilot_ids(dat_general, psytool_info_adults,      vpid_col_df2 = vp_col)
pilots_asc_auto <- find_pilot_ids(dat_general, psytool_info_adolescents, vpid_col_df2 = vp_col)
pilots_ch_auto  <- find_pilot_ids(dat_general, psytool_info_children,    vpid_col_df2 = vp_col)

# Confirmed main-study exception: 70084 is not a pilot even if the general
# participant overview causes find_pilot_ids() to classify it as one.
confirmed_main_asc_7 <- c(70084L)
auto_pilot_overlap_asc_7 <- intersect(
  suppressWarnings(as.integer(pilots_asc_auto)),
  confirmed_main_asc_7
)

if (length(auto_pilot_overlap_asc_7) > 0L) {
  psytool_info_adolescents <- audit_row_action(
    psytool_info_adolescents,
    idx = psytool_info_adolescents$id %in% auto_pilot_overlap_asc_7,
    id_col = "id",
    project_col = "p",
    sample = "adolescents",
    data_type = "experiment_data",
    criterion = "Project 7 cogtests adolescents: ID 70084 was automatically classified as a pilot but is confirmed as a main-study participant; kept in the main-study data.",
    action = "pilot_status_corrected",
    new_id = 70084L
  )
}

pilots_asc_auto <- setdiff(
  suppressWarnings(as.integer(pilots_asc_auto)),
  confirmed_main_asc_7
)
rm(confirmed_main_asc_7, auto_pilot_overlap_asc_7)

pilot_ad_2  <- c(20004)
pilot_ad_9  <- c()
pilot_ad_8  <- c(80350)
pilot_asc_7 <- c()
pilot_ch_6  <- c(62973, 62980, 62998, 62992, 62987, 62989, 62994, 62970)
pilot_ch_8  <- c(80350)

pilot_ad_all  <- c(pilot_ad_2, pilot_ad_9, pilot_ad_8, pilots_ad_auto)
pilot_asc_all <- c(pilots_asc_auto)
pilots_ch_all <- c(pilots_ch_auto, pilot_ch_6, pilot_ch_8)

# --- save P9 (and all) pilot rows before extracting them out ------------------
pilot_psytool_adults <- dplyr::filter(psytool_info_adults, .data[[vp_col]] %in% pilot_ad_all)

# Move to separate file and from original dataset -----------------------------
psytool_info_adults <- extract_pilot_by_vpid(
  psytool_info_adults, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilot_ad_all, sample = "psytool_adults", vpid_col = vp_col
)
psytool_info_adolescents <- extract_pilot_by_vpid(
  psytool_info_adolescents, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilot_asc_all, sample = "psytool_adolescents", vpid_col = vp_col
)
psytool_info_children <- extract_pilot_by_vpid(
  psytool_info_children, out_path = file.path(out_path, "pilots"), export_csv = FALSE,
  pilot_ids = pilots_ch_all, sample = "psytool_children", vpid_col = vp_col
)

# Handle duplicate IDs ---------------------------------------------------------
# Delete not needed, incomplete or faulty datasets -----------------------------

# Project 3 — delete exact faulty cogtest rows -------------------------------
# These checks use interpreted Hamburg/Berlin time, not UTC display time.
time_start_berlin_p3 <- .fmt_time_berlin(psytool_info_adults[[start_col]])

drop_p3_cog_mask <- psytool_info_adults[[project_col]] == 3L & (
  (
    psytool_info_adults[[vp_col]] == 30100L &
      time_start_berlin_p3 == "2025-09-24 09:34:00"
  ) | (
    psytool_info_adults[[vp_col]] == 30102L &
      time_start_berlin_p3 == "2025-10-13 09:21:00"
  )
)

psytool_info_adults <- audit_row_action(
  psytool_info_adults,
  idx = drop_p3_cog_mask,
  id_col = "id",
  project_col = "p",
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 3 cogtests: duplicate IDs 30100 and 30102 deleted by exact Berlin TIME_start.",
  action = "deleted"
)

rm(drop_p3_cog_mask, time_start_berlin_p3)

# Project 3: after deleting the known faulty 30102 row above, the earliest
# remaining 30102 cogtest from 2025-10-13 belongs to participant 30101.
idx_30102_p3_remaining <- which(
  psytool_info_adults[[project_col]] == 3L &
    psytool_info_adults[[vp_col]] == 30102L &
    .date_berlin(psytool_info_adults[[start_col]]) == as.Date("2025-10-13")
)

if (length(idx_30102_p3_remaining) > 0L) {
  time_30102_p3_remaining <- as_time_safely(
    psytool_info_adults[[start_col]][idx_30102_p3_remaining],
    tz = "Europe/Berlin"
  )

  if (all(is.na(time_30102_p3_remaining))) {
    stop("Project 3 cogtests: remaining ID 30102 rows from 2025-10-13 have no valid TIME_start.")
  }

  earliest_30102_p3_idx <- idx_30102_p3_remaining[
    which.min(time_30102_p3_remaining)
  ]
  mask_30102_to_30101 <- seq_len(nrow(psytool_info_adults)) == earliest_30102_p3_idx

  psytool_info_adults <- audit_id_change(
    psytool_info_adults,
    idx = mask_30102_to_30101,
    id_col = "id",
    new_id = 30101L,
    project_col = "p",
    sample = "adults",
    data_type = "experiment_data",
    criterion = "Project 3 cogtests: after deletion of the known faulty 30102 row, the earliest remaining ID 30102 row from 2025-10-13 was corrected to 30101."
  )

  rm(time_30102_p3_remaining, earliest_30102_p3_idx, mask_30102_to_30101)
}
rm(idx_30102_p3_remaining)

# Project 7
psytool_info_adolescents <- audit_row_action(
  psytool_info_adolescents,
  idx = psytool_info_adolescents[[project_col]] == 7L &
    psytool_info_adolescents[[vp_col]] == 70176L &
    .date_berlin(psytool_info_adolescents[[start_col]]) == as.Date("2026-04-27"),
  id_col = "id",
  project_col = "p",
  sample = "adolescents",
  data_type = "experiment_data",
  criterion = "Project 7 adolescent cogtests: duplicate ID 70176 deleted.",
  action = "deleted"
)

# Project 8
# The corresponding questionnaire records are unusable and are already removed
# above. Remove their cognitive-test records as well so the modalities remain
# consistent.
psytool_info_adults <- audit_row_action(
  psytool_info_adults,
  idx = psytool_info_adults$id %in% c(80009L, 80011L) &
    psytool_info_adults$p == 8L,
  id_col = "id",
  project_col = "p",
  sample = "adults",
  data_type = "experiment_data",
  criterion = "Project 8 cogtests adults: IDs 80009 and 80011 deleted because their corresponding questionnaire records are incomplete and cannot be salvaged.",
  action = "deleted"
)

# Adults
res_adults <- resolve_duplicates(psytool_info_adults, vp_col, submit_col,
                                 dataset_name = "adults", data_type = "experiment_data",
                                 project_col, logger = logger)
psytool_info_adults <- res_adults$cleaned
trash_adults        <- res_adults$trash_bin

# Adolescents
res_adolescents <- resolve_duplicates(psytool_info_adolescents, vp_col, submit_col,
                                      dataset_name = "adolescents", data_type = "experiment_data",
                                      project_col, logger = logger)
psytool_info_adolescents <- res_adolescents$cleaned
trash_adolescents        <- res_adolescents$trash_bin

# Apply the same Project 7 cross-sample check to the cognitive-test data after
# the ordinary within-sample duplicate passes.
flag_cross_sample_duplicates(
  df_left = psytool_info_adults,
  df_right = psytool_info_adolescents,
  vp_col = "id",
  project_col = "p",
  project = 7L,
  sample_left = "adults",
  sample_right = "adolescents",
  data_type = "experiment_data",
  logger = logger
)

# Children
res_children <- resolve_duplicates(psytool_info_children, vp_col, submit_col,
                                   dataset_name = "children", data_type = "experiment_data",
                                   project_col, logger = logger)
psytool_info_children <- res_children$cleaned
trash_children        <- res_children$trash_bin

# Separate the data by project and store on disk -------------------------------
# Cognitive Tests

samples <- list(
  adults           = normalize_cogtest_ids(psytool_info_adults),
  adolescents      = normalize_cogtest_ids(psytool_info_adolescents),
  children_parents = normalize_cogtest_ids(psytool_info_children)
)

# Cogtests
variable_output_paths = lapply(names(samples), function(s) {
  prep <- prepare_project_slices(samples[[s]], out_path = out_path, sample = s, data_type = "experiment_data", metadata_info = cogtest_info)
  write_project_slices(prep)
})
names(variable_output_paths) <- names(samples)

# -------------------------------------------------------------------------
# Additional Project 3 cogtest exports: exp_1 vs exp_2
# -------------------------------------------------------------------------

p3_cogtest_export <- if (exists("data_adults_p_3_cogtest", envir = .GlobalEnv)) {
  get("data_adults_p_3_cogtest", envir = .GlobalEnv)
} else {
  dplyr::filter(psytool_info_adults, p == 3)
}

p3_cogtest_export$id <- suppressWarnings(as.integer(as.character(p3_cogtest_export$id)))

data_adults_exp_1_p_3_cogtest <- p3_cogtest_export %>%
  dplyr::filter(id >= 30000L, id <= 30128L)

data_adults_exp_2_p_3_cogtest <- p3_cogtest_export %>%
  dplyr::filter(id >= 32000L, id < 33000L)

write_project3_exp_split_exports(
  exp1_df = data_adults_exp_1_p_3_cogtest,
  exp2_df = data_adults_exp_2_p_3_cogtest,
  out_path = out_path,
  sample_label = "adults",
  data_type = "experiment_data"
)

# ================= Pilot exception for Project 9 ==============================
# Save pilot rows that belong to project 9 into
# '<pid>_backbone/raw_data/pilot_data/'
# using separate_by_project(..., pilot_mode = TRUE).

write_p9_pilots <- function(df, sample_label, data_type, metadata_info) {
  if (is.null(df) || !NROW(df)) return(invisible(NULL))
  # Detect the project column robustly
  proj_candidates <- c("project", "Projekt...Project", "Projekt.", "Projekt",
                       "projekt", "p", "proj", "Proj")
  lower_names <- tolower(names(df))
  hit <- match(tolower(proj_candidates), lower_names)
  hit <- hit[!is.na(hit)][1]
  if (is.na(hit)) return(invisible(NULL))
  proj_col <- names(df)[hit]
  sub <- df[trimws(as.character(df[[proj_col]])) %in% c("9","P9","Project 9","9."), , drop = FALSE]
  if (!NROW(sub)) return(invisible(NULL))
  
  separate_by_project(
    sub,
    out_path      = out_path,
    sample        = sample_label,
    export_csv    = FALSE,
    data_type     = data_type,
    metadata_info = metadata_info,
    pilot_mode    = TRUE,
    verbose       = TRUE
  )
}

# --- Questionnaires (use quest_info) -----------------------------------------
write_p9_pilots(pilot_quest_adults, "adults", "questionnaires", quest_info)

# --- PsyToolkit (cognitive; use cogtest_info) --------------------------------
write_p9_pilots(pilot_psytool_adults, "adults", "experiment_data", cogtest_info)


# Build logger dest map robustly from the file paths we just wrote
build_dest_dirs <- function(paths_vec) {
  # Flatten + drop empties
  paths_vec <- as.character(unlist(paths_vec, use.names = FALSE))
  paths_vec <- paths_vec[nzchar(paths_vec)]
  if (!length(paths_vec)) stop("No output paths to derive per-project destinations from.")
  
  find_backbone_root <- function(p) {
    p <- normalizePath(p, winslash = "/", mustWork = FALSE)
    if (!nzchar(p)) return(NA_character_)
    # if it's a file, start at its directory
    cur <- if (file.exists(p) && !dir.exists(p)) dirname(p) else p
    # climb until we hit X_backbone
    for (i in 1:10) {
      base <- basename(cur)
      if (grepl("^[0-9]+_backbone$", base)) return(cur)
      parent <- dirname(cur)
      if (identical(parent, cur)) break
      cur <- parent
    }
    NA_character_
  }
  
  roots <- vapply(paths_vec, find_backbone_root, character(1))
  roots <- unique(roots[!is.na(roots)])
  if (!length(roots)) stop("No per-project folders (<digits>_backbone) found among the given paths.")
  
  proj_keys <- sub("^([0-9]+).*", "\\1", basename(roots))
  stats::setNames(file.path(roots, "raw_data"), proj_keys)
}


# Use adults (any sample works; they all produce the same project set)
dest_dirs <- build_dest_dirs(variable_output_paths$adults)

# (Optional) clear old .log files in those project folders
for (dir in dest_dirs) {
  if (dir.exists(dir)) {
    log_files <- list.files(dir, pattern = "\\.log$", full.names = TRUE)
    if (length(log_files)) {
      message("Removing existing log files in: ", dir)
      file.remove(log_files)
    }
  }
}

# Split the main log into the per-project folders
logger$split(dest_dirs)

# --- REPLACE up to here --------------------------------------------------------


# 4️⃣ Close when done
logger$close()

# Keep the complete, unsplit log with the combined raw-data exports as well.
all_projects_log_dir <- file.path(
  out_path,
  "all_projects_backbone",
  "raw_data"
)
dir.create(all_projects_log_dir, recursive = TRUE, showWarnings = FALSE)

all_projects_log_file <- file.path(
  all_projects_log_dir,
  "all_action_points.log"
)

log_copy_ok <- file.copy(
  from = all_action_points_log,
  to = all_projects_log_file,
  overwrite = TRUE
)

if (!isTRUE(log_copy_ok)) {
  stop(
    "Could not copy the complete action-points log to: ",
    all_projects_log_file
  )
}

message("Wrote complete action-points log: ", all_projects_log_file)

## Get the Experimental Data Sets Associated with the project ------------------

# copy TXT files for all per-project cogtests AND mirror into all_projects_backbone
log_copy <- copy_psytool_files(
  env_objects       = ls(pattern = "^data_.*_p_[0-9]+_cogtest$"),
  cogtest_out_path  = out_path,          # "01_project_data"
  meta_env_name     = "cogtest_info",
  test_cols         = NULL,              # let it auto-detect
  allowed_projects  = as.character(2:9),
  middle_subdir     = NULL,
  purge_old_dated   = TRUE,              # delete old *_cogtest_data before copying
  write_all_projects= TRUE               # also write ALL_<date>_*_cogtest_data
)

# Additional Project 3 exp-specific cogtest TXT exports ------------------------

copy_psytool_files(
  env_objects       = "data_adults_exp_1_p_3_cogtest",
  cogtest_out_path  = out_path,
  meta_env_name     = "cogtest_info",
  test_cols         = NULL,
  allowed_projects  = "3",
  middle_subdir     = "exp-1",
  purge_old_dated   = FALSE,
  write_all_projects= TRUE
)

copy_psytool_files(
  env_objects       = "data_adults_exp_2_p_3_cogtest",
  cogtest_out_path  = out_path,
  meta_env_name     = "cogtest_info",
  test_cols         = NULL,
  allowed_projects  = "3",
  middle_subdir     = "exp-2",
  purge_old_dated   = FALSE,
  write_all_projects= TRUE
)

# `write_p9_pilots()` creates the Project-9-only pilot object under the distinct
# name `data_adults_p_9_cogtest_pilot`. Keeping this separate is essential:
# overwriting `data_adults_p_9_cogtest` here would replace the main P9 slice.
if (exists("data_adults_p_9_cogtest_pilot", envir = .GlobalEnv, inherits = FALSE) &&
    NROW(get("data_adults_p_9_cogtest_pilot", envir = .GlobalEnv))) {
  log_copy_p9_pilot <- copy_psytool_files(
    env_objects        = "data_adults_p_9_cogtest_pilot",
    cogtest_out_path   = out_path,
    meta_env_name      = "cogtest_info",
    allowed_projects   = "9",
    middle_subdir      = NULL,
    project_subdir     = "pilot_data",
    folder_label       = "PILOT",
    purge_old_dated    = TRUE,
    write_all_projects = FALSE
  )
}

## Export ID change/deletion/sample-move audit ---------------------------------

write_id_change_audit(private_info_path, today)

## Print the final list of IDs to Disk -----------------------------------------

collect_ids_to_excel(
  meta_data = quest_info,
  dat_adults,
  dat_adolescents,
  dat_children_parents,
  dat_children_p6,
  dat_parents_p6,
  submit_date_col = "startdate",
  id_col = "vpid",
  project_col = "project",
  data_type = "questionnaire"
)

collect_ids_to_excel(
  meta_data = quest_info,
  dat_children_p6,
  dat_parents_p6,
  submit_date_col = "startdate",
  id_col = "VPCode",
  project_col = NULL,
  data_type = "questionnaire_p6"
)

collect_ids_to_excel(
  meta_data = cogtest_info,
  psytool_info_adults,
  psytool_info_adolescents,
  psytool_info_children,
  submit_date_col = "TIME_start",
  id_col = "id",
  project_col = "p",
  data_type = "cogtest"
)


## Data Sanity Check -----------------------------------------



## ------------- helpers -------------

# Canonical key: lowercase + remove all non-alphanumerics
.canon_key <- function(x) {
  x <- tolower(x)
  gsub("[^a-z0-9]", "", x, perl = TRUE)
}

# Normalize item/scale mapping to columns: item, scale, item_key
.normalize_item_info <- function(item_info_adults) {
  ii <- item_info_adults %>%
    rename_with(~ "item",  dplyr::matches("(?i)^item$")) %>%
    rename_with(~ "scale", dplyr::matches("(?i)^scale$")) %>%
    mutate(
      item     = as.character(item),
      scale    = as.character(scale),
      item_key = .canon_key(item)
    ) %>%
    distinct(item, scale, item_key)
  
  # warn if different item names collapse to same key
  dup_keys <- ii %>% count(item_key) %>% filter(n > 1)
  if (nrow(dup_keys) > 0) {
    warning("Multiple Item names collapse to the same canonical key: ",
            paste0(dup_keys$item_key, collapse = ", "),
            ". Disambiguate Item names if this is unintended.")
  }
  ii
}

# Normalize scoring to columns: scale, min, max
.normalize_scoring_info <- function(scoring_info) {
  scoring_info %>%
    rename_with(~ "scale", dplyr::matches("(?i)^scale$")) %>%
    rename_with(~ "min",   dplyr::matches("(?i)^min$")) %>%
    rename_with(~ "max",   dplyr::matches("(?i)^max$")) %>%
    mutate(scale = as.character(scale)) %>%
    dplyr::select(scale, min, max)
}

# Link mapping items to actual data columns (handles brackets/dots via canonical keys)
# Returns: list(link, present, missing)
.build_item_link <- function(dat, item_info_adults) {
  ii <- .normalize_item_info(item_info_adults) %>%
    mutate(item_key = .canon_key(item))
  
  dat_cols <- tibble(
    data_col = names(dat),
    data_key = .canon_key(names(dat))
  )
  
  # join item_key (mapping) -> data_key (actual columns)
  link <- ii %>%
    left_join(dat_cols, by = c("item_key" = "data_key")) %>%
    dplyr::select(item, scale, item_key, data_col)
  
  # for QC we only care about true scales, not admin fields
  present <- link %>% filter(!is.na(data_col), !is.na(scale))
  missing <- link %>% filter(is.na(data_col), !is.na(scale))
  
  # warn if data columns canonicalize to the same key (rare)
  dup_dat_keys <- dat_cols %>% count(data_key) %>% filter(n > 1)
  if (nrow(dup_dat_keys) > 0) {
    warning("Multiple data columns collapse to the same canonical key: ",
            paste0(dup_dat_keys$data_key, collapse = ", "),
            ". Consider cleaning column names to avoid ambiguity.")
  }
  
  list(link = link, present = present, missing = missing)
}


# expect objects named data_adults_p_1_questionnaire ... _p_9_questionnaire
dataset_names <- sprintf("data_adults_p_%d_questionnaire", 1:9)
dataset_names <- dataset_names[dataset_names %in% ls(envir = .GlobalEnv)]
datasets <- mget(dataset_names, envir = .GlobalEnv)

# run QC on each dataset
qc_results <- imap(datasets, function(dat, nm) {
  message("\n--- QC for ", nm, " ---")
  
  # quick mapping counts (optional; useful for sanity)
  lp <- .build_item_link(dat, item_info_adults)
  message("Mapped items: ", nrow(lp$present), "  |  Unmapped (in mapping but not in data): ", nrow(lp$missing))
  
  res <- qc_ranges_and_missing(
    dat                  = dat,
    item_info_adults     = item_info_adults,
    scoring_info         = scoring_info,
    id_col               = "vpid",                 # this attaches vpids to outputs
    all_or_nothing_scales= c("FHSfamilytree", "CAPE", "SUQ", "health", "demographics", "times"),
    exclude_scales_from_qc = c("id")               # <-- ignore the ID scale in QC
  )
  
  
  # concise console output
  message("Range violations: ", nrow(res$violations))
  if (nrow(res$per_scale_summary) > 0) {
    message("Scales with any FULL missing participants:")
    print(res$per_scale_summary %>% filter(n_full_missing > 0) %>% arrange(desc(n_full_missing)))
    message("Scales with any PARTIAL missing participants (excl. all-or-nothing):")
    print(res$per_scale_summary %>% filter(n_partial_missing > 0) %>% arrange(desc(n_partial_missing)))
  } else {
    message("No mapped scales found (skipped).")
  }
  
  invisible(res)
})


# Final project-folder validation ----------------------------------------------
# Writers now target the final raw_data paths directly. Nothing is moved here;
# this check only prevents a silently mixed old/new layout from going unnoticed.
validate_project_backbone_layout <- function(project_data_path) {
  backbone_names <- c(paste0(2:9, "_backbone"), "all_projects_backbone")
  required_relative_paths <- c(
    file.path("raw_data", "questionnaires"),
    file.path("raw_data", "experiment_data"),
    "derivatives"
  )

  required_paths <- unlist(
    lapply(
      backbone_names,
      function(backbone_name) {
        file.path(project_data_path, backbone_name, required_relative_paths)
      }
    ),
    use.names = FALSE
  )
  required_paths <- c(
    required_paths,
    file.path(
      project_data_path,
      "9_backbone",
      "raw_data",
      "pilot_data",
      c("questionnaires", "experiment_data")
    )
  )

  missing_paths <- required_paths[!dir.exists(required_paths)]
  if (length(missing_paths)) {
    stop(
      "Backbone layout is incomplete; these required folders are missing:\n",
      paste(missing_paths, collapse = "\n")
    )
  }

  legacy_paths <- unlist(
    lapply(
      backbone_names,
      function(backbone_name) {
        file.path(
          project_data_path,
          backbone_name,
          c("questionnaires", "experiment_data", "pilot_data")
        )
      }
    ),
    use.names = FALSE
  )
  legacy_paths <- legacy_paths[dir.exists(legacy_paths)]
  if (length(legacy_paths)) {
    warning(
      "Legacy output folders remain outside raw_data because one or more old ",
      "files could not be archived. Current outputs still use raw_data, so ",
      "preprocessing will continue:\n",
      paste(legacy_paths, collapse = "\n")
    )
  }

  message(
    "Backbone layout validated for current outputs: raw_data and derivatives ",
    "folders exist."
  )
  invisible(TRUE)
}

validate_project_backbone_layout(out_path)
