# --- prep03_BAckbone_Questionnaire_Data_Cleaning.R -----------------------------------------------
# FOR: Separate Backbone Data by Project — ITEM-LEVEL Cleaning & Export Pipeline
# Authors: Saskia Wilken
# Split version: 2026-03-16
#
# Description:
# Reads questionnaire data, filters bad rows, reverse-codes items, applies
# questionnaire-specific item-level preprocessing, enriches demographics/health,
# writes item-level clean masters, per-project exports, sanity logs, and keys.
# Per-project clean files go to 01_project_data/<project>_backbone/derivatives/
# questionnaires; combined masters, keys and logs keep their existing locations.
# Previous 02_cleaned outputs are archived under 02_cleaned/old as before.
# Adds fixed, frequency-only CAPE positive subfactor keys from Schlier et al.
# (2015), Table 2, alongside the existing CAPE keys. Run steps 03 -> 04 -> 05.
# Grouping revision 2026-10-09: group labels With_Diagnosis / No_Diagnosis,
# plus Diagnosis_Group in all clean and discarded exports. Fixed groups for
# With_Diagnosis participants: P2 psychosis; P8 anxiety; P9 obsession.
# Run Tool_01_P7_diagnosis_extraction.R first to refresh the P7 grouping sheet.
# Revision 2026-10-12: all P3/P4/P5 participants are forced to No_Diagnosis,
# with an empty Diagnosis_Group, independently of grouping-workbook entries.
#
# IMPORTANT:
# - This script does NOT compute score_* columns.
# - Scoring is moved to a later script so internal consistency can run in between.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
cat("\014")

# Clean up R environment -------------------------------------------------------
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readxl", "janitor", "stringr", "dplyr", 
  "purrr", "lubridate", "tibble", "glue", "fs", "jsonlite", "writexl",
  "forcats"
))

# ---- Paths & Logging ---------------------------------------------------------
script_dir <- function() {
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    filepath <- sub(file_arg, "", args[grep(file_arg, args)])
    if (length(filepath) == 1) return(normalizePath(dirname(filepath), winslash = "/"))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

ROOT <- script_dir()

DIR_PROJECT_DATA    <- fs::path(ROOT, "01_project_data")
DIR_QUESTIONNAIRES  <- fs::path(
  DIR_PROJECT_DATA, "all_projects_backbone", "raw_data", "questionnaires"
)
DIR_INFO            <- fs::path(ROOT, "information")
DIR_EXPORT          <- fs::path(ROOT, "02_cleaned")
DIR_KEYS            <- fs::path(DIR_EXPORT, "keys")
DIR_LOGS            <- fs::path(ROOT, "logs")
DIR_PRIVATE         <- fs::path(ROOT, "private_information")
DIR_FUNCTIONS       <- fs::path(ROOT, "functions")

fs::dir_create(DIR_EXPORT)
fs::dir_create(DIR_LOGS)

logfile <- NULL
SANITY_LOG_FILE <- NULL

log_msg <- function(..., .sep = "", .newline = TRUE) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = .sep))
  if (.newline) msg <- paste0(msg, "\n")
  cat(msg)
  cat(msg, file = logfile, append = TRUE)
}

# ---- Source required functions -----------------------------------------------
source(file.path(DIR_FUNCTIONS, "normalize_id.R"))
source(file.path(DIR_FUNCTIONS, "get_project_col.R"))

# --- Normalizers --------------------------------------------------------------
normalize_project_key <- function(x) {
  x0  <- x %>% as.character() %>% stringr::str_trim() %>% stringr::str_to_lower()
  num <- stringr::str_match(x0, "p?\\s*(\\d+)")[,2]
  ifelse(is.na(num), NA_character_, paste0("p", num))
}

norm_token <- function(x) gsub("[^a-z]", "", tolower(paste(x)))

normalize_sample_case <- function(sample) {
  stringr::str_replace_all(stringr::str_to_title(sample), "_", " ")
}

extract_date <- function(x) {
  d <- stringr::str_match(x, "(\\d{4}-\\d{2}-\\d{2})")[,2]
  suppressWarnings(lubridate::ymd(d))
}

input_date_prefix <- function(filepath) {
  date_value <- extract_date(basename(filepath))
  if (length(date_value) != 1L || is.na(date_value)) {
    stop(
      "Could not extract a YYYY-MM-DD date from questionnaire input file: ",
      filepath
    )
  }
  format(date_value, "%Y-%m-%d")
}

initialize_run_logs <- function(input_dates) {
  valid_dates <- suppressWarnings(as.Date(input_dates))
  run_date <- if (all(is.na(valid_dates))) {
    format(Sys.Date(), "%Y-%m-%d")
  } else {
    format(max(valid_dates, na.rm = TRUE), "%Y-%m-%d")
  }
  run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  logfile <<- fs::path(
    DIR_LOGS,
    glue::glue("{run_date}_clean_questionnaires_items_only_{run_stamp}.log")
  )
  SANITY_LOG_FILE <<- fs::path(
    DIR_LOGS,
    glue::glue("{run_date}_sanity_check_backbone_data_{run_stamp}.txt")
  )
}

archive_existing_clean_outputs <- function(export_dir = DIR_EXPORT) {
  fs::dir_create(export_dir)
  old_root <- fs::path(export_dir, "old")
  fs::dir_create(old_root)

  entries <- fs::dir_ls(
    export_dir,
    recurse = FALSE,
    type = "any",
    all = TRUE,
    fail = FALSE
  )
  entries <- entries[fs::path_file(entries) != "old"]

  if (!length(entries)) {
    log_msg("No previous files found in ", export_dir, "; nothing to archive.")
    return(invisible(NULL))
  }

  archive_stamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  archive_dir <- fs::path(old_root, archive_stamp)
  suffix <- 1L
  while (fs::dir_exists(archive_dir) || fs::file_exists(archive_dir)) {
    archive_dir <- fs::path(old_root, paste0(archive_stamp, "_", suffix))
    suffix <- suffix + 1L
  }
  fs::dir_create(archive_dir)

  moved <- character(0)
  for (src in entries) {
    dst <- fs::path(archive_dir, fs::path_file(src))
    move_error <- tryCatch(
      {
        fs::file_move(src, dst)
        NULL
      },
      error = function(e) e
    )

    if (!is.null(move_error)) {
      rollback_errors <- character(0)
      for (old_src in rev(moved)) {
        old_dst <- fs::path(archive_dir, fs::path_file(old_src))
        rollback_error <- tryCatch(
          {
            if (fs::file_exists(old_dst) || fs::dir_exists(old_dst)) {
              fs::file_move(old_dst, old_src)
            }
            NULL
          },
          error = function(e) conditionMessage(e)
        )
        if (!is.null(rollback_error)) {
          rollback_errors <- c(rollback_errors, rollback_error)
        }
      }

      stop(
        "Could not archive previous clean outputs before writing new files. ",
        "Failed while moving '", src, "': ", conditionMessage(move_error),
        if (length(rollback_errors)) {
          paste0(" Rollback also reported: ", paste(rollback_errors, collapse = " | "))
        } else {
          " All earlier moves from this attempt were rolled back."
        }
      )
    }
    moved <- c(moved, src)
  }

  log_msg(
    "Archived ", length(moved), " previous top-level output item(s) to: ",
    archive_dir
  )
  invisible(archive_dir)
}

latest_file_by_pattern <- function(dir, pattern) {
  files_all <- fs::dir_ls(dir, type = "file", fail = FALSE)
  files <- files_all[grepl(pattern, basename(files_all))]
  if (!length(files)) return(NA_character_)
  dts <- extract_date(basename(files))
  files[order(dts, decreasing = TRUE)][1]
}

latest_questionnaire_for_sample_local <- function(sample) {
  patt <- glue::glue("^ALL_\\d{{4}}-\\d{{2}}-\\d{{2}}_{sample}_questionnaire\\.xlsx$")
  latest_file_by_pattern(DIR_QUESTIONNAIRES, patt)
}

latest_iteminfo_for_sample <- function(sample) {
  SampleCap <- normalize_sample_case(sample)
  patt <- glue::glue("^\\d{{4}}-\\d{{2}}-\\d{{2}}_Item_Information_{SampleCap}\\.xlsx$")
  latest_file_by_pattern(DIR_INFO, patt)
}

latest_scoring <- function() {
  patt <- "^\\d{4}-\\d{2}-\\d{2}_Scoring\\.xlsx$"
  latest_file_by_pattern(DIR_INFO, patt)
}

latest_groupings_file <- function() {
  patt <- "^\\d{4}-\\d{2}-\\d{2}_groupings\\.xlsx$"
  latest_file_by_pattern(DIR_PRIVATE, patt)
}

normalize_vpid <- function(x) {
  x %>% as.character() %>% stringr::str_to_lower() %>% stringr::str_replace_all("[^a-z0-9]", "")
}

# ---- IO helpers --------------------------------------------------------------
read_questionnaire <- function(filepath) {
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  log_msg("Reading questionnaire: ", filepath)
  df <- suppressMessages(readxl::read_excel(filepath))
  df <- janitor::remove_empty(df, which = c("rows", "cols"))
  tibble::as_tibble(df)
}

read_item_info <- function(filepath) {
  if (is.null(filepath) || is.na(filepath)) {
    log_msg("Item Information file not found for this sample — continuing without it.")
    return(NULL)
  }
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  log_msg("Reading Item Information: ", filepath)
  suppressMessages(readxl::read_excel(filepath)) |>
    janitor::clean_names() |>
    dplyr::mutate(
      item_norm = normalize_id(.data$item),
      reverse_coded = as.logical(.data$reverse_coded)
    )
}

read_scoring <- function(filepath) {
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  log_msg("Reading Scoring file: ", filepath)
  suppressMessages(readxl::read_excel(filepath)) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(c(min, max), as.numeric))
}

# ---- Groupings helpers -------------------------------------------------------
# Revision 2026-10-09: With_Diagnosis / No_Diagnosis and Diagnosis_Group.
# In list-style grouping sheets, Diagnosis_Group is aligned ONLY with the
# With_Diagnosis ID on that row. The No_Diagnosis list is independent.
standardize_group <- function(x) {
  x0 <- gsub("[^a-z0-9]", "", tolower(trimws(as.character(x))))
  dplyr::case_when(
    x0 %in% c("withdiagnosis", "patient", "patients", "pt", "case", "clinical") ~ "With_Diagnosis",
    x0 %in% c("nodiagnosis", "hc", "healthy", "control", "controls", "healthycontrol") ~ "No_Diagnosis",
    TRUE ~ NA_character_
  )
}

normalize_diagnosis_group <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_
  token <- gsub("[^a-z]", "", tolower(x))
  labels <- c("psychosis", "anxiety", "obsession", "depression", "substance", "ptsd", "rest")
  for (label in labels) {
    x[token %in% c(label, paste0(label, "group"))] <- paste0(label, "-group")
  }
  x
}

apply_project_diagnosis_groups <- function(df, project_keys) {
  if (!"group" %in% names(df)) df$group <- rep(NA_character_, nrow(df))
  if (!"Diagnosis_Group" %in% names(df)) df$Diagnosis_Group <- rep(NA_character_, nrow(df))
  df$group <- standardize_group(df$group)
  # Psychiatric diagnoses were exclusion criteria in screened projects 3, 4 and 5.
  # Force every participant to No_Diagnosis, including unmatched or missing IDs.
  screened_no_diagnosis <- project_keys %in% c("p3", "p4", "p5")
  df$group[screened_no_diagnosis] <- "No_Diagnosis"
  df$Diagnosis_Group <- normalize_diagnosis_group(df$Diagnosis_Group)
  with_diagnosis <- !is.na(df$group) & df$group == "With_Diagnosis"
  df$Diagnosis_Group[!with_diagnosis] <- NA_character_
  df$Diagnosis_Group[with_diagnosis & project_keys %in% "p2"] <- "psychosis-group"
  df$Diagnosis_Group[with_diagnosis & project_keys %in% "p8"] <- "anxiety-group"
  df$Diagnosis_Group[with_diagnosis & project_keys %in% "p9"] <- "obsession-group"
  df
}

read_groupings_for_sample <- function(sample, filepath = NULL) {
  # Tool_01 updates this exact workbook; use it even if another dated copy
  # exists in private_information.
  if (is.null(filepath)) filepath <- fs::path(DIR_PRIVATE, "2025-10-15_groupings.xlsx")
  if (is.null(filepath) || is.na(filepath) || !fs::file_exists(filepath)) {
    log_msg("No groupings file found — skipping group mapping for sample '", sample, "'.")
    return(NULL)
  }
  log_msg("Reading groupings workbook: ", filepath)
  sheets <- readxl::excel_sheets(filepath)
  parsed <- purrr::map_dfr(sheets, function(sh) {
    m <- stringr::str_match(tolower(sh), "^\\s*(p?\\s*(\\d+))\\s*([\\s_-].*)?$")
    if (all(is.na(m))) return(NULL)
    tibble::tibble(
      sheet = sh, .p_join = paste0("p", m[, 3]),
      suffix_norm = norm_token(ifelse(is.na(m[, 4]), "", m[, 4]))
    )
  })
  if (!nrow(parsed)) {
    log_msg("Groupings workbook has no 'p#' sheets — skipping.")
    return(NULL)
  }
  sample_norm <- norm_token(sample)
  parsed <- parsed %>%
    dplyr::mutate(score = dplyr::case_when(
      suffix_norm == "" ~ 1L,
      grepl(sample_norm, suffix_norm, fixed = TRUE) ~ 2L,
      TRUE ~ 0L
    )) %>%
    dplyr::filter(.data$score > 0L)
  id_candidates <- c("vp", "vp id", "vpid", "vp_id", "vp-id", "participant", "participant id",
                     "participant_id", "participantid", "subject", "subject id", "subject_id", "vpid_clean", "id")
  group_candidates <- c("group", "grp", "status", "diagnosis", "dx", "health_status", "arm")
  all_rows <- purrr::map_dfr(parsed$sheet, function(sh) {
    meta <- parsed[parsed$sheet == sh, , drop = FALSE]
    df_sh <- suppressMessages(readxl::read_excel(filepath, sheet = sh)) %>% janitor::clean_names()
    dx_col <- intersect("diagnosis_group", names(df_sh)) |> purrr::pluck(1, .default = NA_character_)
    dx <- if (!is.na(dx_col)) normalize_diagnosis_group(df_sh[[dx_col]]) else rep(NA_character_, nrow(df_sh))
    # Prefer the new names; keep legacy input names readable during migration.
    with_col <- intersect(c("with_diagnosis", "patient", "patients"), names(df_sh)) |>
      purrr::pluck(1, .default = NA_character_)
    no_col <- intersect(c("no_diagnosis", "control", "controls", "hc"), names(df_sh)) |>
      purrr::pluck(1, .default = NA_character_)
    if (!is.na(with_col) || !is.na(no_col)) {
      rows_with <- if (!is.na(with_col)) tibble::tibble(
        vp_join = normalize_vpid(df_sh[[with_col]]), group = "With_Diagnosis", Diagnosis_Group = dx
      ) else tibble::tibble()
      rows_no <- if (!is.na(no_col)) tibble::tibble(
        vp_join = normalize_vpid(df_sh[[no_col]]), group = "No_Diagnosis", Diagnosis_Group = NA_character_
      ) else tibble::tibble()
      out <- dplyr::bind_rows(rows_with, rows_no)
    } else {
      id_col <- intersect(names(df_sh), janitor::make_clean_names(id_candidates)) |>
        purrr::pluck(1, .default = NA_character_)
      grp_col <- intersect(names(df_sh), janitor::make_clean_names(group_candidates)) |>
        purrr::pluck(1, .default = NA_character_)
      if (is.na(id_col) || is.na(grp_col)) {
        log_msg("Sheet '", sh, "' has no recognizable ID/group or With_Diagnosis/No_Diagnosis columns — skipped.")
        return(NULL)
      }
      out <- tibble::tibble(
        vp_join = normalize_vpid(df_sh[[id_col]]),
        group = standardize_group(df_sh[[grp_col]]), Diagnosis_Group = dx
      )
    }
    out <- out %>% dplyr::filter(!is.na(vp_join) & vp_join != "" & !is.na(group))
    out$.p_join <- rep(meta$.p_join[1], nrow(out))
    out$.score <- rep(meta$score[1], nrow(out))
    out
  })
  if (!nrow(all_rows)) {
    log_msg("Groupings for '", sample, "' contained no usable rows after cleaning.")
    return(NULL)
  }
  # A sample-specific sheet takes precedence over a generic project sheet.
  best_rows <- all_rows %>%
    dplyr::group_by(.p_join, vp_join) %>%
    dplyr::filter(.score == max(.score)) %>%
    dplyr::ungroup()
  best_rows <- apply_project_diagnosis_groups(best_rows, best_rows$.p_join)
  conflicts <- best_rows %>%
    dplyr::group_by(.p_join, vp_join) %>%
    dplyr::summarise(
      status_count = dplyr::n_distinct(group),
      diagnosis_count = dplyr::n_distinct(Diagnosis_Group, na.rm = TRUE), .groups = "drop"
    ) %>%
    dplyr::filter(status_count > 1L | diagnosis_count > 1L)
  if (nrow(conflicts)) stop(
    "Conflicting grouping entries for project/ID: ",
    paste(paste(conflicts$.p_join, conflicts$vp_join, sep = "/"), collapse = ", "),
    ". Resolve these entries in the grouping workbook before exporting."
  )
  out <- best_rows %>%
    dplyr::arrange(dplyr::desc(!is.na(Diagnosis_Group))) %>%
    dplyr::distinct(.p_join, vp_join, .keep_all = TRUE) %>%
    dplyr::select(.p_join, vp_join, group, Diagnosis_Group)
  log_msg("Loaded ", nrow(out), " group mappings for sample '", sample, "'.")
  out
}

attach_groupings <- function(df, sample,
                             id_guess = c("vp", "vp id", "vpid", "vp_id", "vp-id",
                                          "participant", "participant id", "participant_id", "participantid",
                                          "subject", "subject_id", "subject id", "vpid_clean"),
                             groupings = read_groupings_for_sample(sample)) {
  if (!"group" %in% names(df)) df$group <- rep(NA_character_, nrow(df))
  if (!"Diagnosis_Group" %in% names(df)) {
    df$Diagnosis_Group <- if ("diagnosis_group" %in% names(df)) {
      as.character(df$diagnosis_group)
    } else rep(NA_character_, nrow(df))
  }
  df <- dplyr::select(df, -dplyr::any_of("diagnosis_group"))
  proj_col <- get_project_col(df)
  if (is.null(proj_col)) {
    log_msg("No project column ('project' or 'p') — group mapping cannot be attached.")
    return(apply_project_diagnosis_groups(df, rep(NA_character_, nrow(df))))
  }
  project_keys <- normalize_project_key(df[[proj_col]])
  if (!nrow(df) || is.null(groupings)) return(apply_project_diagnosis_groups(df, project_keys))
  df$.p_join <- project_keys
  grp <- groupings %>% dplyr::rename(
    .mapped_group = group, .mapped_diagnosis_group = Diagnosis_Group
  )
  df_nms_norm <- gsub("[^a-z0-9]+", "", tolower(names(df)))
  cand_norm <- gsub("[^a-z0-9]+", "", tolower(id_guess))
  id_in_df <- names(df)[df_nms_norm %in% cand_norm]
  best <- NULL
  best_n <- -1L
  best_id <- NA_character_
  for (id_col in id_in_df) {
    tmp <- df %>%
      dplyr::mutate(.vp_join = normalize_vpid(.data[[id_col]])) %>%
      dplyr::left_join(grp, by = c(".p_join", ".vp_join" = "vp_join"))
    nmatch <- sum(!is.na(tmp$.mapped_group))
    if (nmatch > best_n) {
      best <- tmp
      best_n <- nmatch
      best_id <- id_col
    }
  }
  # Assign by project + participant ID only. A project-only fallback would
  # misclassify unmatched IDs and could duplicate rows when a project has both
  # With_Diagnosis and No_Diagnosis participants.
  if (is.null(best)) {
    df_out <- df
    matched <- 0L
    log_msg("No recognizable participant ID column for sample '", sample, "'; workbook mappings were not assigned.")
  } else {
    matched <- sum(!is.na(best$.mapped_group))
    df_out <- best %>% dplyr::mutate(
      group = dplyr::coalesce(.mapped_group, standardize_group(group)),
      Diagnosis_Group = ifelse(!is.na(.mapped_group), .mapped_diagnosis_group, Diagnosis_Group)
    )
  }
  df_out <- apply_project_diagnosis_groups(df_out, df_out$.p_join) %>%
    dplyr::select(-dplyr::any_of(c(".p_join", ".vp_join", ".mapped_group", ".mapped_diagnosis_group")))
  stopifnot(nrow(df_out) == nrow(df))
  log_msg(glue::glue("Grouping for '{sample}': matched {matched}/{nrow(df_out)} rows",
                    if (!is.na(best_id)) glue::glue(" (via '{best_id}')") else ""))
  df_out
}

# ---- Validation & keys -------------------------------------------------------
check_header_match <- function(q_df, item_info) {
  q_norm  <- normalize_id(names(q_df))
  ii_norm <- unique(item_info$item_norm)
  
  not_in_q  <- setdiff(ii_norm, q_norm)
  not_in_ii <- setdiff(q_norm, ii_norm)
  
  exempt <- c("p", "project", "rushingflag", "rushing_flag", "rushingmethod",
              "idasinvariantresponseflag", "idas_invariant_response_flag",
              "participant", "participantid", "vp", "vpid", "id", "submitdate",
              "startdate", "datestamp", "end", "consent", "startlanguage",
              "seed", "refurl", "lastpage")
  not_in_ii <- setdiff(not_in_ii, exempt)
  
  if (length(not_in_q) || length(not_in_ii)) {
    msg <- paste0(
      "⚠️  Header / Item Information mismatch detected.\n",
      if (length(not_in_q))
        paste0("  • Items expected but NOT found in questionnaire: ",
               paste(not_in_q, collapse = ", "), "\n") else "",
      if (length(not_in_ii))
        paste0("  • Columns in questionnaire NOT present in Item Information: ",
               paste(not_in_ii, collapse = ", "), "\n") else ""
    )
    warning(msg, call. = FALSE)
    log_msg(msg)
  } else {
    log_msg("Header check: questionnaire columns align with Item Information (filtered to scored scales).")
  }
  invisible(TRUE)
}

# ---- Additional CAPE positive factors: Schlier et al. (2015) ------------------
# Source: Schizophrenia Research 169, 274-279, Table 2 (p. 276).
# DOI: 10.1016/j.schres.2015.10.034
# These are the five POSITIVE first-order factors of the nine-factor model.
# The paper analyses frequency (0-3), not distress. The Backbone asks about the
# last four weeks, whereas the validation paper used lifetime frequency.
# Keep the existing CAPE frequency/distress keys and item preprocessing intact.
CAPE_POSITIVE_SCHLIER2015_ITEMS <- list(
  bizarre_experiences = c(5L, 17L, 24L, 26L, 28L, 30L, 31L),
  hallucinations = c(33L, 34L, 41L, 42L),
  paranoia = c(2L, 6L, 7L, 10L, 22L),
  grandiosity = c(11L, 13L),
  magical_thinking = c(15L, 20L)
)

add_cape_positive_schlier2015_keys <- function(keys) {
  subs <- keys$items_by_subscale
  is_cape <- !is.na(subs$scale) & toupper(trimws(subs$scale)) == "CAPE"
  if (!any(is_cape)) return(keys)

  freq_rows <- subs[is_cape & !is.na(subs$subscale) &
                      tolower(trimws(subs$subscale)) == "frequency", , drop = FALSE]
  freq_items <- unique(as.character(unlist(freq_rows$items, use.names = FALSE)))
  freq_numbers <- suppressWarnings(as.integer(stringr::str_extract(freq_items, "[0-9]+$")))
  expected <- unlist(CAPE_POSITIVE_SCHLIER2015_ITEMS, use.names = FALSE)
  if (anyNA(freq_numbers) || anyDuplicated(freq_numbers) ||
      !all(expected %in% freq_numbers)) {
    stop(
      "CAPE positive Schlier2015 keys need unambiguous frequency items for all ",
      "20 positive CAPE-42 item numbers. Missing numbers: ",
      paste(setdiff(expected, freq_numbers), collapse = ", "),
      ". Check the CAPE frequency rows in Item Information."
    )
  }

  extra <- tibble::tibble(
    scale = "CAPE",
    subscale = paste0("positive_schlier2015_", names(CAPE_POSITIVE_SCHLIER2015_ITEMS)),
    items = unname(lapply(CAPE_POSITIVE_SCHLIER2015_ITEMS, function(nums) {
      freq_items[match(nums, freq_numbers)]
    }))
  )
  keys$items_by_subscale <- dplyr::bind_rows(subs, extra)

  # Keep the flat, higher-order and nested key representations consistent.
  ho_label <- "CAPE_positive_Schlier2015"
  keys$items_by_higher_order <- dplyr::bind_rows(
    keys$items_by_higher_order,
    tibble::tibble(
      higher_order_subscale = ho_label,
      items = list(unique(unlist(extra$items, use.names = FALSE)))
    )
  )
  cape_idx <- which(toupper(trimws(keys$nested$scale)) == "CAPE")
  if (length(cape_idx) == 1L) {
    keys$nested$higher_order[[cape_idx]] <- dplyr::bind_rows(
      keys$nested$higher_order[[cape_idx]],
      tibble::tibble(
        higher_order_subscale = ho_label,
        subscales = list(dplyr::select(extra, subscale, items))
      )
    )
  }

  # Step 03 retains its existing 1-4 frequency coding. Step 05 subtracts one
  # only for these additional indices, then exports both unweighted sums and
  # item means on the paper's 0-3 metric. No CFA weights or cutoffs are inferred.
  keys$cape_positive_schlier2015 <- list(
    reference = "Schlier et al. (2015), doi:10.1016/j.schres.2015.10.034, Table 2",
    response_dimension = "frequency",
    clean_frequency_min = 1,
    clean_frequency_max = 4,
    item_score_min = 0,
    item_score_max = 3,
    paper_timeframe = "lifetime",
    backbone_timeframe = "last four weeks",
    factors = tibble::tibble(
      subscale = extra$subscale,
      n_items = lengths(CAPE_POSITIVE_SCHLIER2015_ITEMS),
      item_numbers = unname(CAPE_POSITIVE_SCHLIER2015_ITEMS),
      items = extra$items
    )
  )
  log_msg("Added five fixed CAPE positive frequency subfactor keys (Schlier2015; 7/4/5/2/2 items).")
  keys
}

build_keys <- function(item_info) {
  ii <- item_info
  if (!"item_norm" %in% names(ii)) ii$item_norm <- normalize_id(ii$item)
  if (!"scale" %in% names(ii)) ii$scale <- NA_character_
  if (!"subscale" %in% names(ii)) ii$subscale <- NA_character_
  if (!"higher_order_subscale" %in% names(ii)) ii$higher_order_subscale <- NA_character_
  
  ii <- ii %>%
    dplyr::mutate(
      scale = as.character(scale),
      subscale = as.character(subscale),
      higher_order_subscale = as.character(higher_order_subscale)
    )
  
  items_by_scale <- ii %>% dplyr::group_by(scale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  items_by_sub <- ii %>% dplyr::group_by(scale, subscale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  items_by_ho <- ii %>% dplyr::group_by(higher_order_subscale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  leaf <- ii %>% dplyr::group_by(scale, higher_order_subscale, subscale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  ho_level <- leaf %>% dplyr::group_by(scale, higher_order_subscale) %>%
    dplyr::summarise(
      subscales = list(tibble::tibble(subscale = subscale, items = items)),
      .groups = "drop"
    )
  
  nested <- ho_level %>% dplyr::group_by(scale) %>%
    dplyr::summarise(
      higher_order = list(tibble::tibble(
        higher_order_subscale = higher_order_subscale,
        subscales = subscales
      )),
      .groups = "drop"
    )
  
  keys <- list(
    items_by_scale = items_by_scale,
    items_by_subscale = items_by_sub,
    items_by_higher_order = items_by_ho,
    nested = nested
  )
  add_cape_positive_schlier2015_keys(keys)
}

# ---- Row filtering -----------------------------------------------------------
find_participant_id_col <- function(df) {
  candidates <- c(
    "vpid", "vp_id", "vp", "participant_id", "participantid",
    "participant", "subject_id", "subjectid", "subject", "id"
  )
  name_norm <- normalize_id(names(df))
  candidate_norm <- normalize_id(candidates)
  hit <- match(candidate_norm, name_norm)
  hit <- hit[!is.na(hit)]
  if (!length(hit)) return(NULL)
  names(df)[hit[1]]
}

make_participant_exclusion_key <- function(df, id_col = find_participant_id_col(df)) {
  if (is.null(id_col) || !id_col %in% names(df)) {
    return(rep(NA_character_, nrow(df)))
  }

  id_value <- normalize_vpid(df[[id_col]])
  id_value[is.na(id_value) | id_value == ""] <- NA_character_

  project_col <- get_project_col(df)
  if (is.null(project_col)) return(id_value)

  project_value <- normalize_project_key(df[[project_col]])
  ifelse(
    is.na(id_value),
    NA_character_,
    paste0(dplyr::coalesce(project_value, "project_unknown"), "::", id_value)
  )
}

remove_flagged_rows <- function(q_df, sample) {
  as_flag_logical <- function(x) {
    if (is.logical(x)) return(dplyr::coalesce(x, FALSE))
    if (is.numeric(x)) return(!is.na(x) & x != 0)
    tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
  }

  rushing_col <- intersect(
    names(q_df),
    c("rushing_flag", "rushingflag")
  ) |> purrr::pluck(1, .default = NA_character_)

  idas_col <- intersect(
    names(q_df),
    c("idas_invariant_response_flag", "idasinvariantresponseflag")
  ) |> purrr::pluck(1, .default = NA_character_)

  rushing_flag <- if (is.na(rushing_col)) {
    rep(FALSE, nrow(q_df))
  } else {
    as_flag_logical(q_df[[rushing_col]])
  }

  idas_flag <- if (is.na(idas_col)) {
    rep(FALSE, nrow(q_df))
  } else {
    as_flag_logical(q_df[[idas_col]])
  }

  directly_flagged <- rushing_flag | idas_flag
  id_col <- find_participant_id_col(q_df)
  participant_key <- make_participant_exclusion_key(q_df, id_col)
  flagged_participant_keys <- unique(
    participant_key[directly_flagged & !is.na(participant_key)]
  )

  # Exclusion is participant-based, not merely row-based. This matters when a
  # participant has duplicate questionnaire rows but only one row is flagged.
  same_flagged_participant <- !is.na(participant_key) &
    participant_key %in% flagged_participant_keys
  remove_row <- directly_flagged | same_flagged_participant

  discarded <- q_df[remove_row, , drop = FALSE]
  if (nrow(discarded)) {
    discarded$.__reason__ <- dplyr::case_when(
      directly_flagged[remove_row] & rushing_flag[remove_row] & idas_flag[remove_row] ~
        "rushing_flag_and_same_response_on_all_raw_IDAS_items",
      directly_flagged[remove_row] & rushing_flag[remove_row] ~ "rushing_flag",
      directly_flagged[remove_row] & idas_flag[remove_row] ~
        "same_response_on_all_raw_IDAS_items",
      same_flagged_participant[remove_row] ~
        "same_participant_as_rushing_or_IDAS_flagged_row",
      TRUE ~ "flagged_for_exclusion"
    )
  }

  clean <- q_df[!remove_row, , drop = FALSE]

  log_msg(glue::glue(
    "Sample '{sample}': removed {sum(remove_row)} row(s) belonging to "
  ),
  length(flagged_participant_keys), " identified flagged participant(s) in prep03 ",
  "(directly flagged rows: ", sum(directly_flagged),
  "; additional duplicate rows from the same participants: ",
  sum(remove_row & !directly_flagged),
  "; rushing: ", sum(rushing_flag),
  "; invariant IDAS responding: ", sum(idas_flag),
  "; both: ", sum(rushing_flag & idas_flag), ").")

  if (any(directly_flagged & is.na(participant_key))) {
    log_msg(
      "Sample '", sample, "': ",
      sum(directly_flagged & is.na(participant_key)),
      " directly flagged row(s) had no usable participant ID and were therefore ",
      "excluded row-wise."
    )
  }

  list(
    clean = clean,
    discarded = discarded,
    excluded_participant_keys = flagged_participant_keys,
    participant_id_col = id_col
  )
}

assert_flagged_participants_absent <- function(clean_df, dropped, sample) {
  excluded_keys <- dropped$excluded_participant_keys
  if (is.null(excluded_keys) || !length(excluded_keys)) {
    log_msg("Exclusion check for sample '", sample, "': no identified flagged participants.")
    return(invisible(TRUE))
  }

  clean_keys <- make_participant_exclusion_key(
    clean_df,
    id_col = dropped$participant_id_col
  )
  overlap <- intersect(unique(clean_keys[!is.na(clean_keys)]), excluded_keys)

  if (length(overlap)) {
    stop(
      "Exclusion integrity check failed for sample '", sample, "': ",
      length(overlap),
      " participant key(s) flagged for rushing or invariant IDAS responding ",
      "are still present in the cleaned master. No outputs were written for ",
      "this sample. Affected key(s): ", paste(overlap, collapse = ", ")
    )
  }

  log_msg(
    "Exclusion check for sample '", sample, "': all ",
    length(excluded_keys),
    " identified participant(s) flagged for rushing or invariant IDAS ",
    "responding are absent from the cleaned master."
  )
  invisible(TRUE)
}

# ---- Wide-only preprocessing helpers -----------------------------------------
split_id_and_item_columns <- function(q_df, item_info, scoring) {
  valid_scales <- unique(toupper(as.character(scoring$scale)))
  
  item_info_valid <- item_info %>%
    dplyr::filter(!is.na(scale) & scale != "") %>%
    dplyr::filter(toupper(.data$scale) %in% valid_scales | toupper(.data$scale) == "SUQ") %>%
    dplyr::mutate(item_norm = normalize_id(item))
  
  valid_ids <- unique(item_info_valid$item_norm)
  col_norm  <- normalize_id(names(q_df))
  is_item   <- col_norm %in% valid_ids
  
  id_cols   <- names(q_df)[!is_item]
  item_cols <- names(q_df)[is_item]
  
  log_msg("Using items whose Scale is present in the Scoring sheet (+ always SUQ).")
  log_msg("Detected ", length(item_cols), " item columns and ",
          length(id_cols), " meta/ID columns for this sample.")
  
  list(
    id_cols   = id_cols,
    item_cols = item_cols,
    item_info_valid = item_info_valid
  )
}

reverse_code_wide <- function(df, item_cols, col_meta) {
  min_by_col <- col_meta$min; names(min_by_col) <- col_meta$orig
  max_by_col <- col_meta$max; names(max_by_col) <- col_meta$orig
  rev_by_col <- col_meta$reverse_coded; names(rev_by_col) <- col_meta$orig
  
  df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(item_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(item_cols),
        ~ {
          val <- .x
          col <- dplyr::cur_column()
          if (isTRUE(rev_by_col[[col]])) min_by_col[[col]] + max_by_col[[col]] - val else val
        }
      )
    )
}

NON_SCORABLE_SCALES <- c(
  "FHSfamilytree", "health", "demographics", "times",
  "date", "id", "project"
)
# "Non-scorable" means excluded from reverse coding, missing-item handling and
# psychometric scoring. It does not mean that these columns are removed.

normalize_scale_chr <- function(x) toupper(trimws(as.character(x)))

get_item_col_map <- function(df, item_info) {
  ii <- item_info
  if (!"item_norm" %in% names(ii)) {
    ii <- ii %>% dplyr::mutate(item_norm = normalize_id(.data$item))
  }
  
  tibble::tibble(
    orig      = names(df),
    item_norm = normalize_id(names(df))
  ) %>%
    dplyr::distinct(.data$item_norm, .keep_all = TRUE) %>%
    dplyr::inner_join(
      ii %>%
        dplyr::mutate(
          item = as.character(.data$item),
          scale = as.character(.data$scale),
          subscale = as.character(.data$subscale),
          item_norm = normalize_id(.data$item)
        ) %>%
        dplyr::select(item, scale, subscale, item_norm),
      by = "item_norm"
    )
}

ensure_suq_columns <- function(df, item_info) {
  if (is.null(item_info) || !all(c("scale", "item") %in% names(item_info))) return(df)
  
  ii <- item_info
  if (!"item_norm" %in% names(ii)) {
    ii <- ii %>% dplyr::mutate(item_norm = normalize_id(.data$item))
  }
  
  ii_suq <- ii %>%
    dplyr::mutate(
      scale_chr = normalize_scale_chr(.data$scale),
      item_chr  = trimws(as.character(.data$item)),
      item_norm = normalize_id(.data$item)
    ) %>%
    dplyr::filter(.data$scale_chr == "SUQ", .data$item_chr != "", .data$item_norm != "") %>%
    dplyr::distinct(.data$item_chr, .data$item_norm)
  
  if (!nrow(ii_suq)) return(df)
  
  col_norm <- normalize_id(names(df))
  n_created <- 0L
  
  for (i in seq_len(nrow(ii_suq))) {
    if (!(ii_suq$item_norm[i] %in% col_norm)) {
      df[[ii_suq$item_chr[i]]] <- NA_real_
      n_created <- n_created + 1L
    }
  }
  
  if (n_created > 0L) {
    log_msg("SUQ preprocessing: created ", n_created, " absent SUQ columns as NA before branching cleanup.")
  }
  
  df
}

recode_cape_range_to_1_4 <- function(df, item_info, scoring_df) {
  if (is.null(item_info) || !all(c("scale", "item") %in% names(item_info))) return(df)
  
  cmap <- get_item_col_map(df, item_info) %>%
    dplyr::filter(normalize_scale_chr(.data$scale) == "CAPE")
  
  if (!nrow(cmap)) return(df)
  
  cape_vals <- unlist(
    lapply(cmap$orig, function(cc) suppressWarnings(as.numeric(df[[cc]]))),
    use.names = FALSE
  )
  cape_vals <- cape_vals[is.finite(cape_vals)]
  
  if (!length(cape_vals)) {
    log_msg("CAPE range recode: skipped because no finite CAPE values were found.")
    return(df)
  }
  
  data_min <- min(cape_vals, na.rm = TRUE)
  data_max <- max(cape_vals, na.rm = TRUE)
  data_has_zero <- any(cape_vals == 0, na.rm = TRUE)
  data_looks_0_3 <- isTRUE(data_has_zero) && data_min >= 0 && data_max <= 3
  
  if (!data_looks_0_3) {
    log_msg(
      "CAPE range recode: skipped. Observed CAPE range is ",
      data_min, "–", data_max,
      "; data do not look like 0–3."
    )
    return(df)
  }
  
  for (cc in cmap$orig) {
    x <- suppressWarnings(as.numeric(df[[cc]]))
    idx <- !is.na(x)
    x[idx] <- x[idx] + 1
    df[[cc]] <- x
  }
  
  log_msg(
    "CAPE range recode: shifted ", length(cmap$orig),
    " CAPE item columns from 0–3 to 1–4. Observed pre-shift range was ",
    data_min, "–", data_max, "."
  )
  
  df
}

impute_cape_distress_from_frequency <- function(df,
                                                item_info,
                                                scoring_df,
                                                distress_missing_value = 0) {
  if (is.null(item_info) || !all(c("scale", "subscale", "item") %in% names(item_info))) return(df)
  
  cmap <- get_item_col_map(df, item_info) %>%
    dplyr::filter(normalize_scale_chr(.data$scale) == "CAPE")
  
  if (!nrow(cmap)) return(df)
  
  freq_tbl <- cmap %>%
    dplyr::filter(tolower(as.character(.data$subscale)) == "frequency") %>%
    dplyr::mutate(code = stringr::str_extract(.data$item, "\\[(\\d+)\\]"))
  
  distr_tbl <- cmap %>%
    dplyr::filter(tolower(as.character(.data$subscale)) == "distress") %>%
    dplyr::mutate(code = stringr::str_extract(.data$item, "\\[(\\d+)\\]"))
  
  pairs <- freq_tbl %>%
    dplyr::select(code, freq_col = orig) %>%
    dplyr::inner_join(
      distr_tbl %>% dplyr::select(code, distr_col = orig),
      by = "code"
    )
  
  if (!nrow(pairs)) return(df)
  
  cape_scoring <- scoring_df %>%
    dplyr::filter(normalize_scale_chr(.data$scale) == "CAPE") %>%
    dplyr::slice(1)
  
  cape_min <- if (
    nrow(cape_scoring) &&
    "min" %in% names(cape_scoring) &&
    !is.na(cape_scoring$min[1])
  ) {
    as.numeric(cape_scoring$min[1])
  } else {
    1
  }
  
  n_structural_filled <- 0L
  n_left_for_median_imputation <- 0L
  n_frequency_missing <- 0L
  
  for (i in seq_len(nrow(pairs))) {
    fcol <- pairs$freq_col[i]
    dcol <- pairs$distr_col[i]
    
    df[[fcol]] <- suppressWarnings(as.numeric(df[[fcol]]))
    df[[dcol]] <- suppressWarnings(as.numeric(df[[dcol]]))
    
    idx_structural_missing <- !is.na(df[[fcol]]) &
      df[[fcol]] == cape_min &
      is.na(df[[dcol]])
    
    if (any(idx_structural_missing)) {
      df[[dcol]][idx_structural_missing] <- distress_missing_value
      n_structural_filled <- n_structural_filled + sum(idx_structural_missing)
    }
    
    idx_frequency_positive_distress_missing <- !is.na(df[[fcol]]) &
      df[[fcol]] > cape_min &
      is.na(df[[dcol]])
    
    n_left_for_median_imputation <- n_left_for_median_imputation +
      sum(idx_frequency_positive_distress_missing)
    
    idx_frequency_missing_distress_missing <- is.na(df[[fcol]]) & is.na(df[[dcol]])
    n_frequency_missing <- n_frequency_missing + sum(idx_frequency_missing_distress_missing)
  }
  
  log_msg(
    "CAPE structural distress cleanup: filled ", n_structural_filled,
    " missing distress values with ", distress_missing_value,
    " where matching frequency was the lowest option (", cape_min, "); left ",
    n_left_for_median_imputation,
    " missing distress values for later median imputation because matching frequency was > ",
    cape_min,
    "; left ", n_frequency_missing,
    " distress values missing because matching frequency was also missing."
  )
  
  df
}

preprocess_suq_wide <- function(df, item_info) {
  if (is.null(item_info) || !"scale" %in% names(item_info)) return(df)
  
  if (!"item_norm" %in% names(item_info)) {
    item_info <- item_info %>% dplyr::mutate(item_norm = normalize_id(.data$item))
  }
  if (!"subscale" %in% names(item_info) || !"item" %in% names(item_info)) return(df)
  
  ii_suq <- item_info %>%
    dplyr::filter(toupper(.data$scale) == "SUQ") %>%
    dplyr::mutate(
      subscale = as.character(.data$subscale),
      item_num = suppressWarnings(as.integer(stringr::str_extract(.data$item, "\\d+$"))),
      item_norm = normalize_id(.data$item)
    ) %>%
    dplyr::filter(!is.na(.data$item_num), .data$item_num %in% c(1L, 2L, 3L),
                  !is.na(.data$subscale), .data$subscale != "")
  
  if (!nrow(ii_suq)) return(df)
  
  col_map <- tibble::tibble(
    orig      = names(df),
    item_norm = normalize_id(names(df))
  )
  
  mapped <- ii_suq %>%
    dplyr::inner_join(col_map, by = "item_norm") %>%
    dplyr::select(subscale, item_num, col = orig) %>%
    dplyr::distinct()
  
  wide_map <- mapped %>%
    dplyr::group_by(.data$subscale) %>%
    dplyr::summarise(
      q1 = dplyr::first(.data$col[.data$item_num == 1]),
      q2 = dplyr::first(.data$col[.data$item_num == 2]),
      q3 = dplyr::first(.data$col[.data$item_num == 3]),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(.data$q1), !is.na(.data$q2), !is.na(.data$q3))
  
  if (!nrow(wide_map)) return(df)
  
  n_q2_imputed_yes <- 0L
  n_q3_forced_zero <- 0L
  n_remaining_na <- 0L
  
  for (i in seq_len(nrow(wide_map))) {
    c1 <- wide_map$q1[i]
    c2 <- wide_map$q2[i]
    c3 <- wide_map$q3[i]
    
    q1 <- suppressWarnings(as.numeric(df[[c1]]))
    q2 <- suppressWarnings(as.numeric(df[[c2]]))
    q3 <- suppressWarnings(as.numeric(df[[c3]]))
    
    idx_yes_q2_missing <- !is.na(q1) & q1 == 1 & is.na(q2)
    if (any(idx_yes_q2_missing)) {
      q2[idx_yes_q2_missing] <- 1
      n_q2_imputed_yes <- n_q2_imputed_yes + sum(idx_yes_q2_missing)
    }
    
    idx_no <- !is.na(q1) & q1 == 0
    q2[idx_no] <- 0
    q3[idx_no] <- 0
    
    idx_yes_q2_one <- !is.na(q1) & q1 == 1 & !is.na(q2) & q2 == 1
    if (any(idx_yes_q2_one)) {
      n_q3_forced_zero <- n_q3_forced_zero + sum(is.na(q3[idx_yes_q2_one]) | q3[idx_yes_q2_one] != 0)
      q3[idx_yes_q2_one] <- 0
    }
    
    n_remaining_na <- n_remaining_na + sum(is.na(q1)) + sum(is.na(q2)) + sum(is.na(q3))
    q1[is.na(q1)] <- 0
    q2[is.na(q2)] <- 0
    q3[is.na(q3)] <- 0
    
    df[[c1]] <- q1
    df[[c2]] <- q2
    df[[c3]] <- q3
  }
  
  log_msg("SUQ preprocessing: filled ", n_q2_imputed_yes,
          " missing Q2 cells with 1 where Q1 == 1; forced ",
          n_q3_forced_zero,
          " Q3 cells to 0 where Q1 == 1 and Q2 == 1; filled ",
          n_remaining_na, " remaining missing SUQ cells with 0.")
  
  df
}

filter_and_impute_global_scored_items <- function(df,
                                                  item_info,
                                                  scoring_df,
                                                  sample,
                                                  min_prop_items = 0.50,
                                                  exclude_scales = NON_SCORABLE_SCALES) {
  if (is.null(item_info) || is.null(scoring_df)) {
    return(list(clean = df, discarded = tibble::tibble(), imputation_log = tibble::tibble()))
  }
  
  scoring_scales <- unique(normalize_scale_chr(scoring_df$scale))
  exclude_scales_norm <- normalize_scale_chr(exclude_scales)
  
  cmap <- get_item_col_map(df, item_info) %>%
    dplyr::mutate(scale_norm = normalize_scale_chr(.data$scale)) %>%
    dplyr::filter(
      .data$scale_norm %in% scoring_scales | .data$scale_norm == "SUQ",
      !(.data$scale_norm %in% exclude_scales_norm)
    ) %>%
    dplyr::distinct(.data$orig, .keep_all = TRUE)
  
  item_cols <- cmap$orig
  if (!length(item_cols)) {
    log_msg("Global missingness filter: no scorable item columns found for sample '", sample, "'.")
    return(list(clean = df, discarded = tibble::tibble(), imputation_log = tibble::tibble()))
  }
  
  df[item_cols] <- lapply(df[item_cols], function(z) suppressWarnings(as.numeric(z)))
  
  M <- as.matrix(df[, item_cols, drop = FALSE])
  n_nonmiss <- rowSums(!is.na(M))
  n_total <- length(item_cols)
  min_required <- ceiling(min_prop_items * n_total)
  
  keep <- n_nonmiss >= min_required
  
  discarded <- df[!keep, , drop = FALSE]
  if (nrow(discarded)) {
    discarded$.__reason__ <- paste0(
      "global_missingness: fewer than ", min_required, "/",
      n_total, " scorable items present"
    )
  }
  
  clean <- df[keep, , drop = FALSE]
  
  log_msg("Global missingness filter for sample '", sample, "': kept ",
          nrow(clean), "/", nrow(df), " participants; threshold = ",
          min_required, "/", n_total, " scorable items.")
  
  if (!nrow(clean)) {
    stop("Global missingness filter removed all rows for sample '", sample, "'.")
  }
  
  imputation_log <- purrr::map_dfr(item_cols, function(cc) {
    x <- suppressWarnings(as.numeric(clean[[cc]]))
    n_missing <- sum(is.na(x))
    
    if (n_missing == 0L) {
      clean[[cc]] <<- x
      return(tibble::tibble(item = cc, n_imputed = 0L, impute_value = NA_real_))
    }
    
    imp <- suppressWarnings(stats::median(x, na.rm = TRUE))
    if (is.na(imp)) {
      stop("Cannot impute item column '", cc, "' because all retained values are missing.")
    }
    
    x[is.na(x)] <- imp
    clean[[cc]] <<- x
    
    tibble::tibble(item = cc, n_imputed = n_missing, impute_value = imp)
  })
  
  n_imp <- sum(imputation_log$n_imputed, na.rm = TRUE)
  log_msg("Global item imputation for sample '", sample, "': imputed ",
          n_imp, " remaining missing scorable item cells by item median.")
  
  list(clean = clean, discarded = discarded, imputation_log = imputation_log)
}

# ---- Export helpers ----------------------------------------------------------
normalize_master_item_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+", "") %>%
    stringr::str_replace_all("\\[|\\]", "") %>%
    stringr::str_replace_all("[^a-z0-9_]+", "")
}

is_item_like_col <- function(nm) {
  grepl("\\[\\d+\\]$", nm)
}

rename_master_item_columns_inplace <- function(df, item_info = NULL, preserve_scales = NON_SCORABLE_SCALES) {
  old <- names(df)
  new <- old
  
  preserve_item_norm <- character(0)
  if (!is.null(item_info) && all(c("item", "scale") %in% names(item_info))) {
    ii_preserve <- item_info
    if (!"item_norm" %in% names(ii_preserve)) {
      ii_preserve <- ii_preserve %>% dplyr::mutate(item_norm = normalize_id(.data$item))
    }
    preserve_item_norm <- ii_preserve %>%
      dplyr::mutate(scale_norm = normalize_scale_chr(.data$scale)) %>%
      dplyr::filter(.data$scale_norm %in% normalize_scale_chr(preserve_scales)) %>%
      dplyr::pull("item_norm") %>%
      unique()
  }
  
  old_norm <- normalize_id(old)
  idx <- vapply(old, is_item_like_col, logical(1)) &
    !(old_norm %in% preserve_item_norm)
  
  new[idx] <- vapply(old[idx], normalize_master_item_id, character(1))
  new <- make.unique(new, sep = "_dup")
  map_tbl <- tibble::tibble(old = old, new = new, renamed = old != new)
  names(df) <- new
  list(df = df, map = map_tbl)
}

export_per_project <- function(df_clean,
                               df_discard,
                               sample,
                               input_date,
                               item_info = NULL) {
  out_dir <- fs::path(DIR_EXPORT, sample)
  fs::dir_create(out_dir)
  
  ren <- rename_master_item_columns_inplace(df_clean, item_info = item_info)
  df_clean <- ren$df
  map_tbl  <- ren$map
  
  map_path <- fs::path(
    out_dir,
    glue::glue("{input_date}_rename_map_master_items_{sample}.xlsx")
  )
  writexl::write_xlsx(map_tbl, map_path)
  log_msg("Wrote rename map: ", map_path, "  (renamed n=", sum(map_tbl$renamed), ")")
  
  master_path <- fs::path(
    out_dir,
    glue::glue("{input_date}_{sample}_clean_master.xlsx")
  )
  disc_path <- fs::path(
    out_dir,
    glue::glue("{input_date}_{sample}_discarded.xlsx")
  )
  
  writexl::write_xlsx(df_clean, master_path)
  writexl::write_xlsx(df_discard, disc_path)
  
  log_msg("Wrote master (items only, no score columns): ", master_path)
  log_msg("Wrote discarded: ", disc_path)
  
  proj_col <- get_project_col(df_clean)
  if (is.null(proj_col)) {
    log_msg("No project column ('project' or 'p') — skipping per-project split.")
    return(invisible(NULL))
  }
  
  df_split <- split(df_clean, df_clean[[proj_col]])
  purrr::iwalk(df_split, function(dd, proj) {
    project_key <- normalize_project_key(proj)
    if (is.na(project_key) || !grepl("^p[0-9]+$", project_key)) {
      warning("Cannot assign project-specific export to a project folder: ", proj,
              ". These rows remain in the combined clean master.", call. = FALSE)
      return(invisible(NULL))
    }
    project_number <- sub("^p", "", project_key)
    project_out_dir <- fs::path(
      DIR_PROJECT_DATA, paste0(project_number, "_backbone"),
      "derivatives", "questionnaires"
    )
    fs::dir_create(project_out_dir)
    safe_proj <- gsub("[^A-Za-z0-9_-]+", "_", proj)
    filepath <- fs::path(
      project_out_dir,
      glue::glue("{input_date}_{sample}_project-{safe_proj}_clean.xlsx")
    )
    writexl::write_xlsx(dd, filepath)
    log_msg("Wrote project-specific clean questionnaire file: ", filepath)
  })
  
  log_msg(glue::glue("Exported {length(df_split)} per-project XLSX files for '{sample}'."))
}

save_keys <- function(keys, sample, input_date) {
  fs::dir_create(DIR_KEYS)
  path_rds <- fs::path(
    DIR_KEYS,
    glue::glue("{input_date}_{sample}_keys.rds")
  )
  path_json <- fs::path(
    DIR_KEYS,
    glue::glue("{input_date}_{sample}_keys.json")
  )
  saveRDS(keys, path_rds)
  jsonlite::write_json(keys, path_json, pretty = TRUE, auto_unbox = TRUE)
  log_msg(glue::glue("Saved keys for '{sample}' to: {path_rds} and {path_json}"))
}

# ---- FHS family-tree consistency cleanup --------------------------------------
clean_duplicate_parent_gender <- function(df, sample) {
  # The FHS asks separately for the gender of parent 1 and parent 2.
  # If both non-missing entries are identical, the pair is internally
  # inconsistent for the intended mother/father assignment. Because it is not
  # possible to determine which entry is incorrect, both values are set to NA.
  target_norm <- normalize_id(c("parentsgender[001]", "parentsgender[002]"))
  col_norm    <- normalize_id(names(df))
  parent_cols <- names(df)[match(target_norm, col_norm)]

  if (any(is.na(parent_cols))) {
    log_msg(
      "FHS parent-gender cleanup for sample '", sample,
      "': skipped because parentsgender[001] and/or parentsgender[002] was not found."
    )
    return(df)
  }

  g1 <- stringr::str_to_lower(stringr::str_trim(as.character(df[[parent_cols[1]]])))
  g2 <- stringr::str_to_lower(stringr::str_trim(as.character(df[[parent_cols[2]]])))

  g1[g1 == ""] <- NA_character_
  g2[g2 == ""] <- NA_character_

  duplicate_gender <- !is.na(g1) & !is.na(g2) & g1 == g2
  n_duplicate <- sum(duplicate_gender)

  if (n_duplicate > 0L) {
    df[[parent_cols[1]]][duplicate_gender] <- NA
    df[[parent_cols[2]]][duplicate_gender] <- NA
  }

  log_msg(
    "FHS parent-gender cleanup for sample '", sample, "': set both ",
    parent_cols[1], " and ", parent_cols[2], " to NA for ",
    n_duplicate, " participant(s) with duplicate non-missing gender entries."
  )

  df
}

assert_fhs_columns_retained <- function(input_df, output_df, item_info, sample) {
  if (is.null(item_info) || !all(c("item", "scale") %in% names(item_info))) {
    log_msg(
      "FHS retention check for sample '", sample,
      "': skipped because Item Information is unavailable."
    )
    return(invisible(TRUE))
  }

  fhs_item_norm <- item_info %>%
    dplyr::mutate(
      item_norm = normalize_id(.data$item),
      scale_norm = normalize_scale_chr(.data$scale)
    ) %>%
    dplyr::filter(.data$scale_norm == normalize_scale_chr("FHSfamilytree")) %>%
    dplyr::pull("item_norm") %>%
    unique()

  fhs_input_cols <- names(input_df)[normalize_id(names(input_df)) %in% fhs_item_norm]
  if (!length(fhs_input_cols)) {
    log_msg(
      "FHS retention check for sample '", sample,
      "': no FHS columns were present in the questionnaire input."
    )
    return(invisible(TRUE))
  }

  output_norm <- normalize_id(names(output_df))
  missing_cols <- fhs_input_cols[!(normalize_id(fhs_input_cols) %in% output_norm)]
  if (length(missing_cols)) {
    stop(
      "FHS retention check failed for sample '", sample,
      "'. The following input columns disappeared during cleaning: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  log_msg(
    "FHS retention check for sample '", sample, "': passed; all ",
    length(fhs_input_cols), " FHS input column(s) remain in the cleaned master."
  )
  invisible(TRUE)
}

# ---- Demographics / health ---------------------------------------------------
label_demographics_health <- function(df) {
  out <- df
  
  # Only recode the actual participant-level gender column here.
  # Family-tree columns such as parentsgender[001] must remain raw FHS data.
  gender_cols <- intersect("gender", names(out))
  if (length(gender_cols)) {
    out <- out %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(gender_cols),
          ~ factor(as.character(.x),
                   levels = c("1","2","3","0","-oth-"),
                   labels = c("female","male","nonbinary","no gender","other"))
        )
      )
  }
  
  if ("education" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        education = factor(as.character(education),
                           levels = c("0","1","2","3","4","5","6","7","8","9","10","-oth-"),
                           labels = c("no school diploma",
                                      "primary school",
                                      "Lower Secondary School Certificate",
                                      "Intermediate Secondary School Certificate",
                                      "General Qualification for University Entrance",
                                      "apprentice",
                                      "applied science (Fachhochschulabschluss)",
                                      "diploma","bachelor","master","PhD","other"))
      ) %>%
      dplyr::mutate(
        education = forcats::fct_relevel(
          education,
          "no school diploma","primary school",
          "Lower Secondary School Certificate","Intermediate Secondary School Certificate",
          "General Qualification for University Entrance","apprentice",
          "applied science (Fachhochschulabschluss)",
          "bachelor","diploma","master","PhD","other"
        ),
        education = factor(education, levels = levels(education), ordered = TRUE)
      )
  }
  
  if ("maritalstat" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        maritalstat = factor(as.character(maritalstat),
                             levels = c("0","1","2","3","4"),
                             labels = c("single","relationship","married","divorced","widowed"))
      )
  }
  
  sens_cols <- intersect(c("eyesight","hearing"), names(out))
  if (length(sens_cols)) {
    out <- out %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(sens_cols),
          ~ factor(as.character(.x),
                   levels = c("0","1","2"),
                   labels = c("normal","corrected","not corrected"))
        )
      )
  }
  
  bin_candidates <- c("psychomedication","othermedication","ownpsychdisorder")
  contact_cols <- grep("contact", names(out), value = TRUE, ignore.case = TRUE)
  contact_cols <- contact_cols[
    !grepl("^(parents|siblings|children|childrengender|cildrengender)",
           contact_cols, ignore.case = TRUE)
  ]
  bin_candidates <- union(bin_candidates, contact_cols)
  bin_cols <- intersect(bin_candidates, names(out))
  if (length(bin_cols)) {
    out <- out %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(bin_cols),
                      ~ factor(as.character(.x), levels = c("0","1"), labels = c("no","yes")))
      )
  }
  
  out
}

log_sanity <- function(sample, df_rows, var, rule_desc) {
  if (nrow(df_rows) == 0) return(invisible(NULL))
  apply(df_rows, 1, function(r) {
    msg <- glue::glue("🔺 vpid {r[['vp_id']]} (p={r[['p']]}, sample={sample}) has {var}={r[['value']]} — {rule_desc}.")
    log_msg(msg)
    cat(paste0(msg, "\n"), file = SANITY_LOG_FILE, append = TRUE)
  })
  invisible(TRUE)
}

enrich_demographics_and_health_fields <- function(df, sample) {
  d <- label_demographics_health(df)
  
  parse_date_safely <- function(x) suppressWarnings(as.Date(x))
  get_start_like_date <- function(z) {
    cand <- intersect(c("startdate", "datestamp"), names(z))
    if (!length(cand)) return(rep(as.Date(NA), nrow(z)))
    parse_date_safely(z[[cand[1]]])
  }
  looks_like_years_vec <- function(x) {
    xn <- suppressWarnings(as.numeric(x))
    if (all(is.na(xn))) return(FALSE)
    stats::median(xn, na.rm = TRUE) <= 120
  }
  
  dob   <- if ("age" %in% names(d)) parse_date_safely(d$age) else as.Date(NA)
  start <- get_start_like_date(d)
  age_num_raw <- if ("age" %in% names(d)) suppressWarnings(as.numeric(d$age)) else rep(NA_real_, nrow(d))
  treat_as_years <- looks_like_years_vec(d$age)
  
  d <- d %>%
    dplyr::mutate(
      age_years = dplyr::case_when(
        !is.na(dob) & !is.na(start) ~ as.integer(floor(lubridate::interval(dob, start) / lubridate::years(1))),
        treat_as_years ~ as.integer(floor(age_num_raw)),
        TRUE ~ NA_integer_
      ),
      height = suppressWarnings(as.numeric(.data[["height"]])),
      weight = suppressWarnings(as.numeric(.data[["weight"]]))
    )
  
  get_id_vec <- function(z) {
    if ("vp_id" %in% names(z)) z$vp_id
    else if ("vp" %in% names(z)) z$vp
    else if ("participantid" %in% names(z)) z$participantid
    else rep(NA_character_, nrow(z))
  }
  proj_col <- get_project_col(d)
  id_vec <- get_id_vec(d)
  
  rep_rows <- function(idx, var, values, rule) {
    if (any(idx, na.rm = TRUE)) {
      tibble::tibble(
        vp_id = id_vec[idx],
        p    = if (!is.null(proj_col)) d[[proj_col]][idx] else NA_character_,
        value = values[idx]
      ) %>% log_sanity(sample, ., var, rule)
    }
  }
  
  if (tolower(sample) == "adults") {
    age_bad    <- !is.na(d$age_years) & d$age_years < 18L
    height_bad <- !is.na(d$height)    & d$height > 250
    weight_bad <- !is.na(d$weight)    & d$weight < 35
    
    rep_rows(age_bad,    "age",    d$age_years, "below sensible threshold 18 years (adults)")
    rep_rows(height_bad, "height", d$height,    "above sensible threshold 250 cm")
    rep_rows(weight_bad, "weight", d$weight,    "below sensible threshold 35 kg")
    
    d <- d %>%
      dplyr::mutate(
        age_years = dplyr::if_else(age_bad, as.integer(NA), age_years),
        height    = dplyr::if_else(height_bad, NA_real_, height),
        weight    = dplyr::if_else(weight_bad, NA_real_, weight)
      )
  } else if (tolower(sample) == "adolescents") {
    age_bad    <- !is.na(d$age_years) & d$age_years < 5L
    height_bad <- !is.na(d$height)    & d$height > 250
    weight_bad <- !is.na(d$weight)    & d$weight < 35
    
    rep_rows(age_bad,    "age",    d$age_years, "below sensible threshold 5 years")
    rep_rows(height_bad, "height", d$height,    "above sensible threshold 250 cm")
    rep_rows(weight_bad, "weight", d$weight,    "below sensible threshold 35 kg")
    
    d <- d %>%
      dplyr::mutate(
        age_years = dplyr::if_else(age_bad, as.integer(NA), age_years),
        height    = dplyr::if_else(height_bad, NA_real_, height),
        weight    = dplyr::if_else(weight_bad, NA_real_, weight)
      )
  }
  
  if (any(!is.na(id_vec))) {
    idx_manual <- as.character(id_vec) %in% c("30102")
    if (any(idx_manual, na.rm = TRUE)) {
      tibble::tibble(
        vp_id = id_vec[idx_manual],
        p    = if (!is.null(proj_col)) d[[proj_col]][idx_manual] else NA_character_,
        value = d$age_years[idx_manual]
      ) %>% log_sanity(sample, ., "age", "manually set to NA due to implausible DOB (would be ~5 years in adults)")
      d$age_years[idx_manual] <- NA_integer_
    }
  }
  
  if ("gender" %in% names(d)) {
    g <- as.character(d$gender)
    g[!(g %in% c("female","male"))] <- NA_character_
    d$gender <- factor(g, levels = c("female","male"))
  }
  if ("education" %in% names(d)) {
    e <- as.character(d$education)
    e[e %in% c("other","missing")] <- NA_character_
    lev <- setdiff(levels(d$education), c("other","missing"))
    d$education <- factor(e, levels = lev, ordered = TRUE)
  }
  if ("maritalstat" %in% names(d)) {
    m <- as.character(d$maritalstat)
    ord <- c("single","relationship","married","divorced","widowed")
    d$maritalstat <- factor(m, levels = ord, ordered = TRUE)
  }
  
  d
}

# ---- One-sample driver -------------------------------------------------------
process_sample <- function(sample,
                           questionnaire_path = NULL,
                           iteminfo_path = NULL,
                           scoring_df = NULL,
                           drop_criteria_fun = remove_flagged_rows) {
  log_msg("\n--- Processing sample: ", sample, " ---")
  
  if (is.null(questionnaire_path)) questionnaire_path <- latest_questionnaire_for_sample_local(sample)
  if (is.na(questionnaire_path)) {
    log_msg("No questionnaire file found for sample '", sample, "'. Skipping.")
    return(invisible(NULL))
  }
  if (is.null(iteminfo_path)) iteminfo_path <- latest_iteminfo_for_sample(sample)
  input_date <- input_date_prefix(questionnaire_path)
  
  if (is.null(iteminfo_path) || is.na(iteminfo_path) || !fs::file_exists(iteminfo_path)) {
    fallback <- latest_iteminfo_for_sample("adults")
    if (!is.na(fallback) && fs::file_exists(fallback)) {
      log_msg("Item Information missing for sample '", sample,
              "'. Falling back to adults Item Information: ", fallback)
      iteminfo_path <- fallback
    }
  }
  
  q_raw  <- read_questionnaire(questionnaire_path)
  ii_all <- read_item_info(iteminfo_path)
  
  if (is.null(ii_all)) {
    log_msg("Item Information missing even after fallback; cannot proceed. Skipping sample '", sample, "'.")
    return(invisible(NULL))
  }
  
  dropped   <- drop_criteria_fun(q_raw, sample)
  q_clean0  <- dropped$clean
  q_discard <- dropped$discarded
  
  q_clean0 <- ensure_suq_columns(q_clean0, ii_all)
  
  parts   <- split_id_and_item_columns(q_clean0, ii_all, scoring_df)
  id_cols <- parts$id_cols
  ii      <- parts$item_info_valid
  
  check_header_match(q_clean0, ii)
  
  # Only columns that are truly scorable may be coerced to numeric/reverse-coded.
  # FHSfamilytree and other pass-through scales can contain text (names, Y/N, etc.)
  # and must therefore be kept out of reverse_code_wide().
  scoring_scales_norm <- unique(normalize_scale_chr(scoring_df$scale))
  exclude_scales_norm <- normalize_scale_chr(NON_SCORABLE_SCALES)
  
  col_map <- tibble::tibble(
    orig      = names(q_clean0),
    item_norm = normalize_id(names(q_clean0))
  ) %>%
    dplyr::inner_join(ii %>% dplyr::select(item_norm, scale, reverse_coded), by = "item_norm") %>%
    dplyr::mutate(scale_norm = normalize_scale_chr(.data$scale)) %>%
    dplyr::filter(
      (.data$scale_norm %in% scoring_scales_norm | .data$scale_norm == "SUQ"),
      !(.data$scale_norm %in% exclude_scales_norm)
    ) %>%
    dplyr::distinct(orig, .keep_all = TRUE)
  
  minmax <- scoring_df %>% dplyr::select(scale, min, max)
  col_meta <- col_map %>%
    dplyr::left_join(minmax, by = "scale") %>%
    dplyr::select(orig, reverse_coded, min, max)
  
  q_final <- reverse_code_wide(q_clean0, unique(col_map$orig), col_meta)
  q_final <- recode_cape_range_to_1_4(q_final, ii, scoring_df)
  q_final <- impute_cape_distress_from_frequency(
    q_final,
    ii,
    scoring_df,
    distress_missing_value = 0
  )
  q_final <- preprocess_suq_wide(q_final, ii_all)
  global_miss <- filter_and_impute_global_scored_items(
    df = q_final,
    item_info = ii_all,
    scoring_df = scoring_df,
    sample = sample,
    min_prop_items = 0.50,
    exclude_scales = NON_SCORABLE_SCALES
  )
  
  q_final <- global_miss$clean
  
  if (nrow(global_miss$discarded)) {
    q_discard <- dplyr::bind_rows(
      q_discard %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)),
      global_miss$discarded %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    )
  }
  q_final <- q_final %>% dplyr::select(dplyr::any_of(id_cols), dplyr::everything())
  q_final <- clean_duplicate_parent_gender(q_final, sample)
  grouping_map <- read_groupings_for_sample(sample)
  q_final <- attach_groupings(q_final, sample, groupings = grouping_map)
  q_discard <- attach_groupings(q_discard, sample, groupings = grouping_map)
  
  q_ready <- enrich_demographics_and_health_fields(q_final, sample)

  # FHS is deliberately excluded from internal-consistency preparation, but
  # every raw FHS column must remain available for the later FHS scoring step.
  assert_fhs_columns_retained(q_clean0, q_ready, ii_all, sample)
  assert_flagged_participants_absent(q_ready, dropped, sample)
  
  keys <- build_keys(ii)
  save_keys(keys, sample, input_date)
  
  export_per_project(
    q_ready,
    q_discard,
    sample,
    input_date = input_date,
    item_info = ii_all
  )
  
  log_msg("--- Done sample: ", sample, " ---\n")
  invisible(list(
    data_clean = q_ready,
    data_discarded = q_discard,
    keys = keys
  ))
}

# ---- Main --------------------------------------------------------------------
SAMPLES_TO_PROCESS <- c("adults", "adolescents")

QUESTIONNAIRE_PATHS <- stats::setNames(
  purrr::map_chr(SAMPLES_TO_PROCESS, latest_questionnaire_for_sample_local),
  SAMPLES_TO_PROCESS
)

missing_questionnaires <- names(QUESTIONNAIRE_PATHS)[
  is.na(QUESTIONNAIRE_PATHS) | !fs::file_exists(QUESTIONNAIRE_PATHS)
]
if (length(missing_questionnaires)) {
  stop(
    "No dated questionnaire input was found for sample(s): ",
    paste(missing_questionnaires, collapse = ", "),
    ". Expected ALL_YYYY-MM-DD_<sample>_questionnaire.xlsx in '",
    DIR_QUESTIONNAIRES, "'. Existing clean outputs were not archived."
  )
}

INPUT_DATES <- purrr::map_chr(QUESTIONNAIRE_PATHS, input_date_prefix)
initialize_run_logs(INPUT_DATES)

purrr::iwalk(QUESTIONNAIRE_PATHS, function(path, sample) {
  log_msg("Selected questionnaire input for '", sample, "': ", path)
})

SCORING_PATH <- latest_scoring()
if (is.na(SCORING_PATH)) {
  stop(glue::glue(
    "No scoring file found in '{DIR_INFO}'. Expected something like 'YYYY-MM-DD_Scoring.xlsx'."
  ))
}
SCORING <- read_scoring(SCORING_PATH)

ITEMINFO_PATHS <- stats::setNames(
  purrr::map_chr(SAMPLES_TO_PROCESS, function(sample) {
    path <- latest_iteminfo_for_sample(sample)
    if (!is.na(path) && fs::file_exists(path)) return(path)

    fallback <- latest_iteminfo_for_sample("adults")
    if (!is.na(fallback) && fs::file_exists(fallback)) return(fallback)
    NA_character_
  }),
  SAMPLES_TO_PROCESS
)

missing_iteminfo <- names(ITEMINFO_PATHS)[
  is.na(ITEMINFO_PATHS) | !fs::file_exists(ITEMINFO_PATHS)
]
if (length(missing_iteminfo)) {
  stop(
    "No Item Information workbook was found for sample(s): ",
    paste(missing_iteminfo, collapse = ", "),
    ". Existing clean outputs were not archived."
  )
}

# Archive the complete previous Step-03 output tree only after all required
# input files have been located successfully and before any new file is written.
archive_existing_clean_outputs(DIR_EXPORT)

results <- purrr::imap(
  QUESTIONNAIRE_PATHS,
  ~ process_sample(
    sample = .y,
    questionnaire_path = .x,
    iteminfo_path = ITEMINFO_PATHS[[.y]],
    scoring_df = SCORING
  )
)
