#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Backbone cognitive-test scoring (BACS, WCST, LNS)
# Authors: Artem Chramow, Michel Wrede, Saskia Wilken
# Adapted for the RU5389 Backbone pipeline
#
# Input (per project):
#   <script_dir>/01_project_data/<project>_backbone/raw_data/experiment_data/
# Legacy fallback (per project):
#   <script_dir>/01_project_data/<project>_backbone/experiment_data/
# The _cogtests.xlsx overview files list participant IDs and raw test filenames.
# Children's overview files use children_parents; their raw folders use children.
#
# Output (per project):
#   <script_dir>/01_project_data/<project>_backbone/derivatives/experiment_data/
# The project folder, internal structure, and every input date stamp are retained.
# Exported source-file references contain filenames only for RU-wide sharing.
# Children's trial folders use children. Projects 6/8
# collected WCST only for children; BACS/LNS are labelled "Nicht erhoben".
#
# Methodological note:
# The FHS questionnaire block from the internship script is intentionally not
# part of this cognitive-test pipeline. It is scored in prep_05_Score_Scales.R.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
options(scipen = 999)

required_packages <- c("dplyr", "readxl", "tibble", "writexl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_packages)) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Install them once before running this pipeline."
  )
}

# Methodological switches ------------------------------------------------------
# The internship script's BACS training exclusion was ineffective (STATUS was
# recoded before STATUS == 2 was counted, and > 7 cannot trigger with 7 training
# trials). It therefore remains OFF until a cutoff has been approved.
config <- list(
  bacs_exclude_training_failures = FALSE,
  bacs_training_error_cutoff = 7L,
  bacs_trial_rt_filter = FALSE,
  bacs_trial_rt_absolute_min_ms = 400,
  bacs_trial_rt_sd_lower = 3,
  bacs_trial_rt_sd_upper = 3,
  bacs_participant_rt_filter = FALSE,
  bacs_participant_rt_sd_lower = 3,
  bacs_participant_rt_sd_upper = 3
)

script_directory <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(active)) return(dirname(normalizePath(active, mustWork = FALSE)))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

script_dir <- script_directory()
project_data_dir <- file.path(script_dir, "01_project_data")

if (!dir.exists(project_data_dir)) {
  stop(
    "Project data directory does not exist: ",
    normalizePath(project_data_dir, winslash = "/", mustWork = FALSE)
  )
}
input_root <- project_data_dir

# Generic helpers --------------------------------------------------------------
empty_string <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

as_number <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

safe_mean <- function(x) {
  x <- as_number(x)
  if (!any(is.finite(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  x <- as_number(x)
  if (sum(is.finite(x)) < 2L) return(NA_real_)
  stats::sd(x, na.rm = TRUE)
}

safe_max <- function(x) {
  x <- as_number(x)
  if (!any(is.finite(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

normalize_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

export_filename <- function(path) {
  # Handle both Windows and forward-slash paths; preserve missing values.
  basename(gsub("\\\\", "/", as.character(path)))
}

relative_path <- function(path, root) {
  root <- sub("/+$", "", normalize_path(root))
  prefix <- paste0(root, "/")
  vapply(
    normalize_path(path),
    function(one_path) {
      if (startsWith(one_path, prefix)) {
        substring(one_path, nchar(prefix) + 1L)
      } else {
        basename(one_path)
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

first_existing_column <- function(df, candidates, required = FALSE, label = NULL) {
  hit <- match(tolower(candidates), tolower(names(df)))
  hit <- hit[!is.na(hit)]
  if (length(hit)) return(names(df)[hit[1]])
  if (required) {
    stop(
      "Required column not found",
      if (!is.null(label)) paste0(" (", label, ")") else "",
      ". Accepted names: ", paste(candidates, collapse = ", ")
    )
  }
  NA_character_
}

value_from_column <- function(df, column, row) {
  if (is.na(column)) return(NA_character_)
  value <- df[[column]][row]
  if (!length(value) || empty_string(value)) NA_character_ else trimws(as.character(value))
}

read_trial_file <- function(path, column_names) {
  if (is.na(path) || !file.exists(path)) {
    return(list(data = NULL, error = "Fehlt im Ordner", original_ncol = 0L))
  }
  out <- tryCatch(
    utils::read.table(
      path,
      header = FALSE,
      fill = TRUE,
      stringsAsFactors = FALSE,
      quote = "",
      comment.char = "",
      na.strings = c("NA", "NaN", "null"),
      check.names = FALSE
    ),
    error = function(e) e
  )
  if (inherits(out, "error") || is.null(out) || !nrow(out)) {
    detail <- if (inherits(out, "error")) conditionMessage(out) else "Datei leer"
    detail <- gsub(path, export_filename(path), detail, fixed = TRUE)
    return(list(data = NULL, error = paste0("Datei leer/Lesefehler: ", detail), original_ncol = 0L))
  }
  original_ncol <- ncol(out)
  assigned <- min(original_ncol, length(column_names))
  names(out)[seq_len(assigned)] <- column_names[seq_len(assigned)]
  for (nm in setdiff(column_names, names(out))) out[[nm]] <- NA
  list(data = out, error = NULL, original_ncol = original_ncol)
}

find_file_case_insensitive <- function(directory, filename) {
  if (is.na(directory) || is.na(filename) || !dir.exists(directory)) return(NA_character_)
  direct <- file.path(directory, basename(filename))
  if (file.exists(direct)) return(direct)
  entries <- list.files(directory, full.names = TRUE, recursive = FALSE)
  hit <- entries[tolower(basename(entries)) == tolower(basename(filename))]
  if (length(hit)) hit[1] else NA_character_
}

extract_date_from_name <- function(x) {
  match <- regmatches(x, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", x))
  date <- suppressWarnings(as.Date(match))
  ifelse(is.na(date), 0, as.numeric(date))
}

parse_master_name <- function(path) {
  filename <- basename(path)
  rx <- "^((?:ALL|[2-9]))_([0-9]{4}-[0-9]{2}-[0-9]{2})(?:_(PILOT))?_(adults|adolescents|children_parents)_cogtests(?:_(exp-[12]))?\\.xlsx$"
  match <- regexec(rx, filename, ignore.case = TRUE, perl = TRUE)
  parts <- regmatches(filename, match)[[1]]
  if (!length(parts)) return(NULL)
  scope <- toupper(parts[2])
  date_stamp <- parts[3]
  pilot <- length(parts) >= 4L && !is.na(parts[4]) && nzchar(parts[4])
  sample <- tolower(parts[5])
  exp_label <- if (length(parts) >= 6L && nzchar(parts[6])) tolower(parts[6]) else NA_character_
  if (!is.na(exp_label) && scope != "3") {
    stop("Only Project 3 may contain exp-1/exp-2 input files: ", filename)
  }
  list(
    path = path,
    filename = filename,
    scope = scope,
    date_stamp = date_stamp,
    pilot = pilot,
    sample = sample,
    exp_label = exp_label
  )
}

discover_master_files <- function(root) {
  # Start at each project's experiment_data folder, rather than relying on
  # recursive traversal from the common 01_project_data parent directory.
  project_folders <- paste0(c(as.character(2:9), "all_projects"), "_backbone")
  search_directories <- c(
    file.path(root, project_folders, "raw_data", "experiment_data"),
    file.path(root, project_folders, "experiment_data")
  )
  existing_directories <- search_directories[dir.exists(search_directories)]
  candidates <- unique(unlist(lapply(existing_directories, function(directory) {
    list.files(
      directory,
      pattern = "_cogtests(_exp-[12])?\\.xlsx$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  }), use.names = FALSE))
  if (is.null(candidates)) candidates <- character()
  normalized <- normalize_path(candidates)
  candidates <- candidates[!grepl(
    "(^|/)(old_data|discarded|derivatives)(/|$)",
    normalized,
    ignore.case = TRUE
  )]
  candidates <- candidates[!startsWith(basename(candidates), "~$")]
  parsed <- lapply(candidates, parse_master_name)
  parsed <- parsed[!vapply(parsed, is.null, logical(1))]
  if (!length(parsed)) {
    seen <- unique(unlist(lapply(existing_directories, function(directory) {
      list.files(directory, pattern = "\\.xlsx$", recursive = TRUE,
                 full.names = TRUE, ignore.case = TRUE)
    }), use.names = FALSE))
    if (is.null(seen)) seen <- character()
    seen <- relative_path(utils::head(seen, 20L), root)
    stop(
      "No cognitive-test overview Excel file found in the project experiment_data folders.\n",
      "Expected names such as 6_2026-10-06_children_parents_cogtests.xlsx.\n",
      "Existing folders searched directly:\n",
      if (length(existing_directories)) {
        paste(normalize_path(existing_directories), collapse = "\n")
      } else {
        paste0("(none found below ", normalize_path(root), ")")
      }, "\n",
      "Excel files seen there (first 20):\n",
      if (length(seen)) paste(seen, collapse = "\n") else "(none)"
    )
  }
  parsed
}

candidate_data_directories <- function(master, root) {
  master_dir <- dirname(master$path)
  fallback <- c(
    file.path(root, "all_projects_backbone", "raw_data", "experiment_data"),
    file.path(root, "all_projects_backbone", "experiment_data")
  )
  if (master$sample == "children_parents" && master$scope == "ALL") {
    # A combined children's master can reference raw WCST files stored only in
    # the individual projects. Keep the separate Project 6/8 data in place.
    child_projects <- paste0(c("6", "8"), "_backbone")
    fallback <- c(
      fallback,
      file.path(root, child_projects, "raw_data", "experiment_data"),
      file.path(root, child_projects, "experiment_data")
    )
  }
  search_roots <- unique(c(master_dir, fallback[file.exists(fallback)]))
  directories <- unique(unlist(lapply(search_roots, function(x) {
    list.dirs(x, recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE))
  directories <- directories[grepl("_cogtest_data", basename(directories), ignore.case = TRUE)]
  # Fixed PsyToolkit naming: children_parents in the overview, children in the
  # raw-data folder. Adults and adolescents keep their respective sample name.
  data_sample <- if (master$sample == "children_parents") {
    "children"
  } else {
    master$sample
  }
  directories <- directories[
    grepl(
      tolower(paste0("_", data_sample, "_cogtest_data")),
      tolower(basename(directories)),
      fixed = TRUE
    )
  ]

  has_exp <- grepl("exp[-_/]?[12]", normalize_path(directories), ignore.case = TRUE)
  if (is.na(master$exp_label)) {
    directories <- directories[!has_exp]
  } else {
    exp_number <- sub("exp-", "", master$exp_label, fixed = TRUE)
    directories <- directories[
      grepl(
        paste0("exp[-_/]?", exp_number, "(?:/|$)"),
        normalize_path(directories),
        ignore.case = TRUE,
        perl = TRUE
      )
    ]
  }

  is_pilot <- grepl("pilot", normalize_path(directories), ignore.case = TRUE)
  directories <- if (master$pilot) directories[is_pilot] else directories[!is_pilot]
  if (!length(directories)) return(character())

  info <- file.info(directories)
  order_key <- order(
    extract_date_from_name(basename(directories)),
    as.numeric(info$mtime),
    decreasing = TRUE,
    na.last = TRUE
  )
  directories[order_key]
}

resolve_trial_path <- function(filename, directories) {
  if (is.na(filename)) return(NA_character_)
  if (file.exists(filename)) return(normalize_path(filename))
  for (directory in directories) {
    hit <- find_file_case_insensitive(directory, filename)
    if (!is.na(hit)) return(hit)
  }
  NA_character_
}

# BACS ------------------------------------------------------------------------
bacs_columns <- c(
  "project", "vp_id_raw", "block", "TRIALCOUNT", "NumberCorrect", "STATUS",
  "answer", "RT", "correct_count", "error_count", "time_trialstart"
)

empty_bacs_result <- function(status, training_errors = NA_integer_) {
  tibble::tibble(
    Status_BACS = status,
    score_BACS_correct = NA_integer_,
    score_BACS_errors = NA_integer_,
    score_BACS_total_trials = NA_integer_,
    score_BACS_mean_rt = NA_real_,
    qc_BACS_training_errors = training_errors,
    qc_BACS_trials_removed_rt = NA_integer_
  )
}

score_bacs_single <- function(path) {
  parsed <- read_trial_file(path, bacs_columns)
  if (!is.null(parsed$error)) return(empty_bacs_result(parsed$error))
  df <- parsed$data
  required <- c("block", "NumberCorrect", "STATUS", "answer", "RT")
  if (any(vapply(df[required], function(x) all(is.na(x)), logical(1)))) {
    return(empty_bacs_result("Dateiformat unvollständig"))
  }

  df$NumberCorrect <- as_number(df$NumberCorrect)
  df$STATUS_raw <- as_number(df$STATUS)
  df$RT <- as_number(df$RT)
  df$answer <- trimws(as.character(df$answer))

  # PsyToolkit can record Shift+number answers as keyboard symbols. The original
  # script intended to mark these as correct but removed the quote character
  # before testing it; this version keeps and evaluates the actual character.
  symbol_key <- c(
    "1" = "!", "2" = "\"", "3" = "§", "4" = "$", "5" = "%",
    "6" = "&", "7" = "/", "8" = "(", "9" = ")", "0" = "="
  )
  expected_symbol <- unname(symbol_key[as.character(df$NumberCorrect)])
  symbol_correct <- !is.na(expected_symbol) & !is.na(df$answer) & df$answer == expected_symbol

  df$STATUS_scored <- ifelse(
    df$STATUS_raw == 1, 1,
    ifelse(df$STATUS_raw %in% c(0, 2), 0, NA_real_)
  )
  df$STATUS_scored[symbol_correct] <- 1

  training <- tolower(trimws(as.character(df$block))) == "training"
  training[is.na(training)] <- FALSE
  training_errors <- sum(training & df$STATUS_raw %in% c(0, 2), na.rm = TRUE)
  training_failed <- training_errors > config$bacs_training_error_cutoff
  if (isTRUE(config$bacs_exclude_training_failures) && training_failed) {
    return(empty_bacs_result(
      paste0("Ausgeschlossen: ", training_errors, " Trainingsfehler"),
      training_errors = training_errors
    ))
  }

  exp_data <- df[!training, , drop = FALSE]
  if (!nrow(exp_data)) {
    return(empty_bacs_result("Keine Experiment-Trials", training_errors = training_errors))
  }

  removed_rt <- 0L
  if (isTRUE(config$bacs_trial_rt_filter)) {
    rt_mean <- safe_mean(exp_data$RT)
    rt_sd <- safe_sd(exp_data$RT)
    finite_rt <- is.finite(exp_data$RT)
    rt_outlier <- finite_rt & exp_data$RT < config$bacs_trial_rt_absolute_min_ms
    if (is.finite(rt_mean) && is.finite(rt_sd) && rt_sd > 0) {
      rt_outlier <- rt_outlier | finite_rt & (
        exp_data$RT < rt_mean - config$bacs_trial_rt_sd_lower * rt_sd |
          exp_data$RT > rt_mean + config$bacs_trial_rt_sd_upper * rt_sd
      )
    }
    removed_rt <- sum(rt_outlier, na.rm = TRUE)
    exp_data <- exp_data[!rt_outlier, , drop = FALSE]
  }

  if (!nrow(exp_data)) {
    out <- empty_bacs_result("Keine Trials nach RT-Filter", training_errors)
    out$qc_BACS_trials_removed_rt <- removed_rt
    return(out)
  }

  tibble::tibble(
    Status_BACS = if (training_failed) "Erfolgreich; Trainingsflag" else "Erfolgreich",
    score_BACS_correct = sum(exp_data$STATUS_scored == 1, na.rm = TRUE),
    score_BACS_errors = sum(exp_data$STATUS_scored == 0, na.rm = TRUE),
    score_BACS_total_trials = nrow(exp_data),
    score_BACS_mean_rt = safe_mean(exp_data$RT),
    qc_BACS_training_errors = training_errors,
    qc_BACS_trials_removed_rt = removed_rt
  )
}

apply_bacs_participant_filter <- function(results) {
  results$qc_BACS_participant_rt_outlier <- FALSE
  if (!isTRUE(config$bacs_participant_rt_filter)) return(results)
  eligible <- grepl("^Erfolgreich", results$Status_BACS) &
    is.finite(results$score_BACS_mean_rt)
  grand_mean <- safe_mean(results$score_BACS_mean_rt[eligible])
  grand_sd <- safe_sd(results$score_BACS_mean_rt[eligible])
  if (!is.finite(grand_mean) || !is.finite(grand_sd) || grand_sd <= 0) return(results)
  outlier <- eligible & (
    results$score_BACS_mean_rt < grand_mean - config$bacs_participant_rt_sd_lower * grand_sd |
      results$score_BACS_mean_rt > grand_mean + config$bacs_participant_rt_sd_upper * grand_sd
  )
  results$qc_BACS_participant_rt_outlier[outlier] <- TRUE
  results$Status_BACS[outlier] <- "Ausgeschlossen: mittlere RT außerhalb Cutoff"
  score_cols <- c(
    "score_BACS_correct", "score_BACS_errors", "score_BACS_total_trials",
    "score_BACS_mean_rt"
  )
  results[outlier, score_cols] <- NA
  results
}

# WCST ------------------------------------------------------------------------
wcst_columns <- c(
  "project", "vpid", "card", "ShapeCorrect", "NumberCorrect", "ColorCorrect",
  "RT", "STATUS", "answer", "anyerror", "perseverationerror",
  "nonperseverationerror", "correct_count", "block_count", "trial_count",
  "time_trialstart"
)

empty_wcst_result <- function(status) {
  tibble::tibble(
    Status_WCST = status,
    score_WCST_correct = NA_integer_,
    score_WCST_errors = NA_integer_,
    score_WCST_pers_resp = NA_integer_,
    score_WCST_pers_err = NA_integer_,
    score_WCST_nonpers_err = NA_integer_,
    score_WCST_cat_comp = NA_integer_,
    score_WCST_trials_first_cat = NA_integer_,
    score_WCST_fail_maintain = NA_integer_,
    score_WCST_conceptual_resp = NA_integer_,
    score_WCST_L2L = NA_real_,
    qc_WCST_NA_trials = NA_integer_
  )
}

score_wcst_single <- function(path) {
  parsed <- read_trial_file(path, wcst_columns)
  if (!is.null(parsed$error)) return(empty_wcst_result(parsed$error))
  df <- parsed$data
  needed <- c(
    "ShapeCorrect", "NumberCorrect", "ColorCorrect", "answer", "anyerror",
    "correct_count", "block_count"
  )
  if (any(vapply(df[needed], function(x) all(is.na(x)), logical(1)))) {
    return(empty_wcst_result("Dateiformat unvollständig"))
  }

  numeric_names <- c(
    "ShapeCorrect", "NumberCorrect", "ColorCorrect", "answer", "anyerror",
    "correct_count", "block_count"
  )
  df[numeric_names] <- lapply(df[numeric_names], as_number)
  if (!any(df$anyerror %in% c(0, 1), na.rm = TRUE)) {
    return(empty_wcst_result("Keine auswertbaren Trials"))
  }

  df$match_shape <- !is.na(df$answer) & !is.na(df$ShapeCorrect) & df$answer == df$ShapeCorrect
  df$match_number <- !is.na(df$answer) & !is.na(df$NumberCorrect) & df$answer == df$NumberCorrect
  df$match_color <- !is.na(df$answer) & !is.na(df$ColorCorrect) & df$answer == df$ColorCorrect
  df$match_total <- as.integer(df$match_shape) + as.integer(df$match_number) + as.integer(df$match_color)
  df$is_unambiguous <- df$match_total == 1L
  df$is_ambiguous <- df$match_total > 1L
  df$is_pers_resp <- FALSE
  df$pers_principle_active <- NA_character_

  current_principle <- NA_character_
  candidate_principle <- NA_character_
  candidate_indices <- integer(0)
  rule_of_block <- c(
    "1" = "color", "2" = "shape", "3" = "number",
    "4" = "color", "5" = "shape", "6" = "number"
  )
  block_key <- ifelse(is.na(df$block_count), "__NA__", as.character(df$block_count))
  current_block <- block_key[1]

  for (i in seq_len(nrow(df))) {
    if (!identical(block_key[i], current_block)) {
      # The just-completed category's rule becomes the perseverated principle
      # on the first trial of the new category.
      previous_rule <- unname(rule_of_block[current_block])
      current_principle <- if (length(previous_rule) && !is.na(previous_rule)) {
        previous_rule
      } else {
        NA_character_
      }
      candidate_principle <- NA_character_
      candidate_indices <- integer(0)
      current_block <- block_key[i]
    }

    is_error <- !is.na(df$anyerror[i]) && df$anyerror[i] == 1
    is_correct <- !is.na(df$anyerror[i]) && df$anyerror[i] == 0
    unambiguous <- isTRUE(df$is_unambiguous[i])
    matched_dim <- NA_character_
    if (unambiguous) {
      if (df$match_shape[i]) matched_dim <- "shape"
      if (df$match_number[i]) matched_dim <- "number"
      if (df$match_color[i]) matched_dim <- "color"
    }

    if (is_error && unambiguous) {
      if (is.na(current_principle)) {
        current_principle <- matched_dim
      } else if (identical(matched_dim, current_principle)) {
        df$is_pers_resp[i] <- TRUE
        candidate_principle <- NA_character_
        candidate_indices <- integer(0)
      } else {
        if (is.na(candidate_principle) || !identical(candidate_principle, matched_dim)) {
          candidate_principle <- matched_dim
          candidate_indices <- i
        } else {
          candidate_indices <- c(candidate_indices, i)
        }
        if (length(candidate_indices) >= 3L) {
          # Error 1 establishes the new principle; errors 2 and 3 count
          # retrospectively as perseverative responses.
          df$is_pers_resp[candidate_indices[2:3]] <- TRUE
          df$pers_principle_active[candidate_indices[2:3]] <- candidate_principle
          current_principle <- candidate_principle
          candidate_principle <- NA_character_
          candidate_indices <- integer(0)
        }
      }
    } else if (is_correct && unambiguous) {
      candidate_principle <- NA_character_
      candidate_indices <- integer(0)
    }
    df$pers_principle_active[i] <- current_principle
  }

  # Ambiguous responses can be perseverative when bracketed by unambiguous
  # perseverative responses using the same principle within the same block.
  if (nrow(df) >= 3L) {
    for (i in 2:(nrow(df) - 1L)) {
      principle <- df$pers_principle_active[i]
      if (!isTRUE(df$is_ambiguous[i]) || is.na(principle)) next
      match_active <- switch(
        principle,
        shape = df$match_shape[i],
        number = df$match_number[i],
        color = df$match_color[i],
        FALSE
      )
      if (!isTRUE(match_active)) next
      previous_indices <- seq_len(i - 1L)
      next_indices <- (i + 1L):nrow(df)
      previous_candidates <- previous_indices[
        df$is_unambiguous[previous_indices] & block_key[previous_indices] == block_key[i]
      ]
      next_candidates <- next_indices[
        df$is_unambiguous[next_indices] & block_key[next_indices] == block_key[i]
      ]
      if (!length(previous_candidates) || !length(next_candidates)) next
      previous <- max(previous_candidates)
      next_one <- min(next_candidates)
      if (
        isTRUE(df$is_pers_resp[previous]) &&
          isTRUE(df$is_pers_resp[next_one]) &&
          identical(df$pers_principle_active[previous], principle) &&
          identical(df$pers_principle_active[next_one], principle)
      ) {
        df$is_pers_resp[i] <- TRUE
      }
    }
  }

  score_correct <- sum(df$anyerror == 0, na.rm = TRUE)
  score_errors <- sum(df$anyerror == 1, na.rm = TRUE)
  score_pers_resp <- sum(df$is_pers_resp, na.rm = TRUE)
  score_pers_err <- sum(df$is_pers_resp & df$anyerror == 1, na.rm = TRUE)
  score_nonpers_err <- score_errors - score_pers_err
  score_cat_comp <- sum(df$correct_count == 10, na.rm = TRUE)
  completed <- which(df$correct_count == 10)
  trial_first_cat <- if (length(completed)) min(completed) else 65L

  score_failure_maintain <- 0L
  if (nrow(df) >= 2L) {
    for (i in 2:nrow(df)) {
      previous <- df$correct_count[i - 1L]
      current <- df$correct_count[i]
      if (
        !is.na(previous) && !is.na(current) &&
          previous >= 5 && previous < 10 && current == 0
      ) {
        score_failure_maintain <- score_failure_maintain + 1L
      }
    }
  }

  correct_flag <- !is.na(df$anyerror) & df$anyerror == 0
  runs <- rle(correct_flag)
  score_conceptual <- sum(runs$lengths[runs$values & runs$lengths >= 3L])

  cat_stats <- df |>
    dplyr::group_by(.data$block_count) |>
    dplyr::summarise(
      trials = dplyr::n(),
      errors = sum(.data$anyerror == 1, na.rm = TRUE),
      max_correct = safe_max(.data$correct_count),
      .groups = "drop"
    ) |>
    dplyr::filter(
      (!is.na(.data$max_correct) & .data$max_correct >= 10) | .data$trials >= 10
    ) |>
    dplyr::mutate(pes = (.data$errors / .data$trials) * 100)
  score_l2l <- if (nrow(cat_stats) >= 3L) mean(-diff(cat_stats$pes)) else NA_real_

  tibble::tibble(
    Status_WCST = "Erfolgreich",
    score_WCST_correct = score_correct,
    score_WCST_errors = score_errors,
    score_WCST_pers_resp = score_pers_resp,
    score_WCST_pers_err = score_pers_err,
    score_WCST_nonpers_err = score_nonpers_err,
    score_WCST_cat_comp = score_cat_comp,
    score_WCST_trials_first_cat = trial_first_cat,
    score_WCST_fail_maintain = score_failure_maintain,
    score_WCST_conceptual_resp = score_conceptual,
    score_WCST_L2L = score_l2l,
    qc_WCST_NA_trials = sum(is.na(df$answer))
  )
}

# LNS -------------------------------------------------------------------------
lns_columns <- c(
  "project", "vpid", "block_type", "block_num", "itemCorrect",
  "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9",
  "rt", "seq_correct", "correct_count", "span_error", "time_start"
)

empty_lns_result <- function(status) {
  tibble::tibble(
    Status_LNS = status,
    score_LNS_correct = NA_integer_,
    score_LNS_errors = NA_integer_,
    score_LNS_max_span = NA_integer_,
    score_LNS_mean_rt_correct = NA_real_,
    qc_LNS_NA_trials = NA_integer_,
    qc_LNS_trials_scored = NA_integer_
  )
}

score_lns_single <- function(path) {
  parsed <- read_trial_file(path, lns_columns)
  if (!is.null(parsed$error)) return(empty_lns_result(parsed$error))
  df <- parsed$data
  required <- c("block_type", "itemCorrect", "rt", "seq_correct", "span_error")
  if (any(vapply(df[required], function(x) all(is.na(x)), logical(1)))) {
    return(empty_lns_result("Dateiformat unvollständig"))
  }
  experiment_trial <- tolower(trimws(as.character(df$block_type))) == "experiment"
  experiment_trial[is.na(experiment_trial)] <- FALSE
  exp_data <- df[experiment_trial, , drop = FALSE]
  if (!nrow(exp_data)) return(empty_lns_result("Keine Experiment-Trials"))

  exp_data$seq_correct <- as_number(exp_data$seq_correct)
  exp_data$span_error <- as_number(exp_data$span_error)
  exp_data$rt <- as_number(exp_data$rt)
  stop_index <- which(!is.na(exp_data$span_error) & exp_data$span_error >= 4)
  if (length(stop_index)) exp_data <- exp_data[seq_len(stop_index[1]), , drop = FALSE]

  # Retains the internship script's definition: sequence length equals the
  # character count of itemCorrect. This assumes the PsyToolkit encoding uses
  # one character per item and should be validated against the task definition.
  exp_data$span_length <- nchar(as.character(exp_data$itemCorrect))
  correct_trials <- exp_data[!is.na(exp_data$seq_correct) & exp_data$seq_correct == 1, , drop = FALSE]
  max_span <- if (nrow(correct_trials)) safe_max(correct_trials$span_length) else 0
  if (is.na(max_span)) max_span <- 0

  tibble::tibble(
    Status_LNS = "Erfolgreich",
    score_LNS_correct = sum(exp_data$seq_correct == 1, na.rm = TRUE),
    score_LNS_errors = sum(exp_data$seq_correct == 0, na.rm = TRUE),
    score_LNS_max_span = as.integer(max_span),
    score_LNS_mean_rt_correct = round(safe_mean(correct_trials$rt), 2),
    qc_LNS_NA_trials = sum(is.na(exp_data$seq_correct)),
    qc_LNS_trials_scored = nrow(exp_data)
  )
}

# Master processing and export -------------------------------------------------
status_summary <- function(scores) {
  status_columns <- c("Status_BACS", "Status_WCST", "Status_LNS")
  pieces <- lapply(status_columns, function(column) {
    values <- as.character(scores[[column]])
    values[is.na(values)] <- "NA"
    counts <- as.data.frame(table(values), stringsAsFactors = FALSE)
    names(counts) <- c("status", "n")
    counts$test <- sub("^Status_", "", column)
    counts[, c("test", "status", "n")]
  })
  dplyr::bind_rows(pieces)
}

output_path_for_master <- function(master) {
  relative_master <- relative_path(master$path, project_data_dir)
  parts <- strsplit(relative_master, "/", fixed = TRUE)[[1]]
  project_index <- which(grepl(
    "^([2-9]|all_projects)_backbone$",
    parts,
    ignore.case = TRUE
  ))[1]
  if (is.na(project_index)) {
    stop("Input master is not inside a project backbone folder: ", master$path)
  }
  inside_project <- parts[(project_index + 1L):length(parts)]
  if (length(inside_project) && tolower(inside_project[1]) == "raw_data") {
    inside_project <- inside_project[-1]
  }
  if (!length(inside_project)) {
    stop("Cannot derive an output path from input master: ", master$path)
  }
  output_name <- sub("_cogtests", "_cognitive_scores", basename(master$path), fixed = TRUE)
  output_subdir <- dirname(paste(inside_project, collapse = "/"))
  output_dir <- file.path(
    project_data_dir,
    parts[project_index],
    "derivatives",
    output_subdir
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(output_dir, output_name)
}

process_master <- function(master) {
  message("Scoring: ", normalize_path(master$path))
  master_df <- readxl::read_excel(master$path)
  if (!nrow(master_df)) stop("Master Excel is empty: ", master$path)

  id_column <- first_existing_column(
    master_df, c("id", "vp_id", "vpid"), required = TRUE, label = "participant ID"
  )
  bacs_column <- first_existing_column(master_df, c("BACS"))
  wcst_column <- first_existing_column(master_df, c("WCST"))
  lns_column <- first_existing_column(master_df, c("LNS"))
  # Projects 6 and 8 collected WCST only in their children's sample.
  children_wcst_only <- rep(
    master$sample == "children_parents" && master$scope %in% c("6", "8"),
    nrow(master_df)
  )
  if (master$sample == "children_parents" && master$scope == "ALL") {
    project_column <- first_existing_column(master_df, c("p", "project"))
    if (!is.na(project_column)) {
      project_numbers <- gsub("[^0-9]", "", as.character(master_df[[project_column]]))
      children_wcst_only <- project_numbers %in% c("6", "8")
    }
  }
  directories <- candidate_data_directories(master, input_root)
  if (!length(directories)) {
    warning("No matching cogtest_data directory found for ", master$filename)
  }

  records <- vector("list", nrow(master_df))
  for (row in seq_len(nrow(master_df))) {
    bacs_path <- if (children_wcst_only[row]) NA_character_ else
      resolve_trial_path(value_from_column(master_df, bacs_column, row), directories)
    wcst_path <- resolve_trial_path(value_from_column(master_df, wcst_column, row), directories)
    lns_path <- if (children_wcst_only[row]) NA_character_ else
      resolve_trial_path(value_from_column(master_df, lns_column, row), directories)
    records[[row]] <- dplyr::bind_cols(
      tibble::tibble(
        source_row = row,
        sample = master$sample,
        score_vp_id = as.character(master_df[[id_column]][row]),
        source_BACS_file = export_filename(bacs_path),
        source_WCST_file = export_filename(wcst_path),
        source_LNS_file = export_filename(lns_path)
      ),
      if (children_wcst_only[row]) empty_bacs_result("Nicht erhoben") else
        if (is.na(bacs_column)) empty_bacs_result("Nicht in Master angegeben") else score_bacs_single(bacs_path),
      if (is.na(wcst_column)) empty_wcst_result("Nicht in Master angegeben") else score_wcst_single(wcst_path),
      if (children_wcst_only[row]) empty_lns_result("Nicht erhoben") else
        if (is.na(lns_column)) empty_lns_result("Nicht in Master angegeben") else score_lns_single(lns_path)
    )
  }

  results <- dplyr::bind_rows(records)
  results <- apply_bacs_participant_filter(results)
  results$all_available_tests_successful <-
    (results$Status_BACS %in% c("Nicht in Master angegeben", "Nicht erhoben") | grepl("^Erfolgreich", results$Status_BACS)) &
    ((results$Status_WCST == "Nicht in Master angegeben" & !children_wcst_only) | results$Status_WCST == "Erfolgreich") &
    (results$Status_LNS %in% c("Nicht in Master angegeben", "Nicht erhoben") | results$Status_LNS == "Erfolgreich")

  # Keep full paths internally for scoring; share only filenames in the export.
  master_export <- as.data.frame(master_df, check.names = FALSE)
  file_columns <- c(bacs_column, wcst_column, lns_column)
  for (column in file_columns[!is.na(file_columns)]) {
    master_export[[column]] <- export_filename(master_export[[column]])
  }
  scores <- dplyr::bind_cols(
    master_export,
    # Keep the input's original ID only; duplicate diagnostics live elsewhere.
    results[, setdiff(names(results), c("source_row", "score_vp_id")), drop = FALSE]
  )
  run_info <- tibble::tibble(
    field = c(
      "input_file", "input_date_stamp", "input_root", "output_created_utc",
      "sample", "project_scope", "experiment_split", "pilot",
      "bacs_training_exclusion", "bacs_training_error_cutoff",
      "bacs_trial_rt_filter", "bacs_participant_rt_filter"
    ),
    value = c(
      export_filename(master$path),
      master$date_stamp,
      export_filename(input_root),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      master$sample,
      master$scope,
      ifelse(is.na(master$exp_label), "", master$exp_label),
      as.character(master$pilot),
      as.character(config$bacs_exclude_training_failures),
      as.character(config$bacs_training_error_cutoff),
      as.character(config$bacs_trial_rt_filter),
      as.character(config$bacs_participant_rt_filter)
    )
  )

  output_path <- output_path_for_master(master)
  writexl::write_xlsx(
    list(
      cognitive_scores = scores,
      qc_status = status_summary(scores),
      run_info = run_info
    ),
    output_path
  )
  message("Written: ", normalize_path(output_path))
  output_path
}

masters <- discover_master_files(input_root)
outputs <- lapply(masters, process_master)

master_scopes <- vapply(masters, `[[`, character(1), "scope")
master_samples <- vapply(masters, `[[`, character(1), "sample")
combined_samples <- unique(master_samples[master_scopes == "ALL"])
missing_combined <- setdiff(unique(master_samples), combined_samples)

if (length(missing_combined)) {
  warning(
    "No all-project cognitive master was found for sample(s): ",
    paste(missing_combined, collapse = ", "),
    ". Their per-project outputs were written, but no combined output could be created."
  )
} else {
  message(
    "Combined all-project cognitive output written for sample(s): ",
    paste(combined_samples, collapse = ", ")
  )
}

message(
  "Finished. ", length(outputs), " derivative Excel file(s) written:\n",
  paste(vapply(outputs, normalize_path, character(1)), collapse = "\n")
)
