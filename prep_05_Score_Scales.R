# --- prep05_Score_Scales.R -----------------------------------------------
# FOR: Add questionnaire score columns to already cleaned Backbone masters
# Authors: Saskia Wilken, Michel Wrede
# New split step: 2026-03-16
#
# Description:
# Reads item-level clean masters from 02_cleaned/<sample>/, reads keys + scoring,
# optionally removes low-loading items flagged by analyze_backbone_scales.R,
# computes score_* columns plus FHS family-history outputs, and writes scored
# masters without dropping participants who have no FHS response.
# Reviewed manual FHS item corrections are already applied by Step 03.
# Also exports fixed CAPE positive frequency subfactor sums and means from
# Schlier et al. (2015), Table 2, alongside the original CAPE scores.
# Run the updated Step 03 first to regenerate matching keys.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
cat("\014")

ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readxl", "readr", "janitor", "dplyr", "stringr", "tibble",
  "purrr", "fs", "glue", "rprojroot", "writexl"
))

CFG <- list(
  samples = c("adults", "adolescents"),
  export_filtered_scores = TRUE,
  export_combined_filtered_scores = TRUE,
  loading_threshold = 0.30,
  combined_label = "adults_adolescents",
  min_prop_items_default = 0.30,
  add_z_scores_to_scored_masters = FALSE,
  fhs_confidence_cutoff = 1,
  # For reduced/loading-filtered score variants, use means so scores remain
  # interpretable when the number of retained items differs from the original scale.
  force_filtered_scores_to_mean = TRUE,
  # SUQ total is normally a sum of substance-specific Q2*Q3 scores.
  # In filtered/reduced variants, use the mean of retained substance scores instead.
  suq_filtered_total_agg = "mean",
  # Additional derived SUQ subscale:
  # combines every SUQ substance except Alcohol, Tobacco, Cannabis and Medication.
  # With the current Item Information this includes Stimulants, Opioids,
  # Hallucinogens, Inhalants and Other.
  suq_illegal_drugs_label = "illegal-drugs",
  suq_illegal_drugs_exclude = c("Alcohol", "Tobacco", "Cannabis", "Medication")
)
NON_SCORABLE_SCALES <- c(
  "FHSfamilytree", "health", "demographics", "times",
  "date", "id", "project"
)

script_dir <- function() {
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    filepath <- sub(file_arg, "", args[grep(file_arg, args)])
    if (length(filepath) == 1) return(normalizePath(dirname(filepath), winslash = "/"))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

project_root <- function() {
  env <- Sys.getenv("BACKBONE_ROOT", unset = NA)
  if (!is.na(env) && dir.exists(env)) return(normalizePath(env, winslash = "/"))
  sd <- script_dir(); if (dir.exists(sd)) return(sd)
  root <- tryCatch(
    rprojroot::find_root(rprojroot::is_rstudio_project | rprojroot::is_git_root),
    error = function(e) NA
  )
  if (!is.na(root)) return(normalizePath(root, winslash = "/"))
  normalizePath(getwd(), winslash = "/")
}

ROOT <- project_root()
DIR_EXPORT     <- fs::path(ROOT, "02_cleaned")
DIR_KEYS       <- fs::path(DIR_EXPORT, "keys")
DIR_INFO       <- fs::path(ROOT, "information")
DIR_LOGS       <- fs::path(ROOT, "logs")
DIR_INTCONS    <- fs::path(ROOT, "out", "internal_data_analysis", "distribution_of_backbone_scores_and_internal_consistency")

fs::dir_create(DIR_LOGS)

logfile <- fs::path(DIR_LOGS, glue::glue("add_scores_to_clean_master_{format(Sys.time(), '%Y-%m-%d_%H%M%S')}.log"))
log_msg <- function(..., .sep = "", .newline = TRUE) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = .sep))
  if (.newline) msg <- paste0(msg, "\n")
  cat(msg)
  cat(msg, file = logfile, append = TRUE)
}

# ---- helpers -----------------------------------------------------------------
latest_file_by_pattern <- function(dir, pattern) {
  files_all <- fs::dir_ls(dir, type = "file", recurse = TRUE, fail = FALSE)
  files <- files_all[grepl(pattern, basename(files_all))]
  if (!length(files)) return(NA_character_)
  info <- file.info(files)
  files[order(info$mtime, decreasing = TRUE)][1]
}

latest_scoring <- function() {
  patt <- "^\\d{4}-\\d{2}-\\d{2}_Scoring\\.xlsx$"
  latest_file_by_pattern(DIR_INFO, patt)
}

latest_iteminfo_for_sample <- function(sample) {
  sample_label <- sample %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_title()
  patt <- paste0(
    "^\\d{4}-\\d{2}-\\d{2}_Item_Information_",
    sample_label,
    "\\.xlsx$"
  )
  latest_file_by_pattern(DIR_INFO, patt)
}

extract_date_string <- function(path) {
  hit <- stringr::str_match(
    basename(path),
    "^(\\d{4}-\\d{2}-\\d{2})_"
  )[, 2]
  if (length(hit) != 1L || is.na(hit)) {
    stop("Could not extract the leading YYYY-MM-DD date from: ", path)
  }
  hit
}

latest_dated_master_for_sample <- function(sample) {
  sample_dir <- fs::path(DIR_EXPORT, sample)
  patt <- paste0(
    "^\\d{4}-\\d{2}-\\d{2}_",
    sample,
    "_clean_master\\.xlsx$"
  )

  files_all <- fs::dir_ls(
    sample_dir,
    type = "file",
    recurse = FALSE,
    fail = FALSE
  )
  files <- files_all[grepl(patt, basename(files_all))]
  if (!length(files)) return(NA_character_)

  dates <- suppressWarnings(as.Date(vapply(files, extract_date_string, character(1))))
  valid <- !is.na(dates)
  files <- files[valid]
  dates <- dates[valid]
  if (!length(files)) return(NA_character_)

  newest_date <- max(dates)
  candidates <- files[dates == newest_date]
  if (length(candidates) == 1L) return(candidates)

  info <- file.info(candidates)
  candidates[which.max(info$mtime)]
}

resolve_sample_inputs <- function(sample) {
  master_path <- latest_dated_master_for_sample(sample)
  if (is.na(master_path) || !fs::file_exists(master_path)) {
    stop(
      "No dated Step 03 master found for sample '", sample, "' in '",
      fs::path(DIR_EXPORT, sample), "'. Expected a file named ",
      "YYYY-MM-DD_", sample, "_clean_master.xlsx."
    )
  }

  input_date <- extract_date_string(master_path)
  keys_path <- fs::path(
    DIR_KEYS,
    glue::glue("{input_date}_{sample}_keys.rds")
  )
  if (!fs::file_exists(keys_path)) {
    stop(
      "The Step 03 key matching the selected master date is missing for sample '",
      sample, "': ", keys_path
    )
  }

  list(
    master_path = master_path,
    keys_path = keys_path,
    input_date = input_date
  )
}

read_master_robust <- function(master_path, default_delim = ";") {
  extension <- tolower(fs::path_ext(master_path))

  if (identical(extension, "xlsx")) {
    return(
      suppressMessages(readxl::read_excel(master_path)) %>%
        janitor::clean_names()
    )
  }

  if (!identical(extension, "csv")) {
    stop("Unsupported master file type: ", master_path)
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
    janitor::clean_names()
}

read_scoring <- function(filepath) {
  suppressMessages(readxl::read_excel(filepath)) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(c(min, max), as.numeric))
}

normalize_id_local <- function(x) {
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

is_cape_positive_schlier2015_subscale <- function(scale, subscale) {
  !is.na(scale) & toupper(trimws(as.character(scale))) == "CAPE" &
    !is.na(subscale) & startsWith(tolower(trimws(as.character(subscale))),
                                 "positive_schlier2015_")
}

validate_cape_positive_schlier2015_keys <- function(keys) {
  subs <- keys$items_by_subscale
  if (is.null(subs) || !nrow(subs) ||
      !any(toupper(trimws(subs$scale)) == "CAPE", na.rm = TRUE)) return(invisible(TRUE))
  meta <- keys$cape_positive_schlier2015
  extra <- subs[is_cape_positive_schlier2015_subscale(subs$scale, subs$subscale), , drop = FALSE]
  if (is.null(meta) || is.null(meta$factors) || nrow(extra) != 5L ||
      !setequal(extra$subscale, meta$factors$subscale)) {
    stop("CAPE Schlier2015 keys are missing or incomplete. Rerun the updated Step 03 before Step 05.")
  }
  invisible(TRUE)
}

make_threshold_tag <- function(x) {
  paste0("lt", gsub("\\.", "", formatC(x, format = "f", digits = 2)))
}

latest_flag_helper <- function(combined_label, threshold_tag) {
  patt <- paste0("^", combined_label, "_flagged_items_", threshold_tag, "\\.xlsx$")
  latest_file_by_pattern(DIR_INTCONS, patt)
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
  
  get_suq_item_num_local <- function(z) {
    suppressWarnings(as.integer(stringr::str_extract(normalize_id_local(z), "[123]$")))
  }
  
  get_suq_pair_key_local <- function(z) {
    stringr::str_remove(normalize_id_local(z), "[123]$")
  }
  
  x <- x %>%
    dplyr::mutate(
      dataset = as.character(.data$dataset),
      level = as.character(.data$level),
      scale = as.character(.data$scale),
      subscale = as.character(.data$subscale),
      item = as.character(.data$item),
      loading = suppressWarnings(as.numeric(.data$loading)),
      flagged_for_removal = if ("flagged_for_removal" %in% names(.)) {
        to_logical_flag(.data$flagged_for_removal)
      } else {
        !is.na(.data$loading) & abs(.data$loading) < threshold_value
      }
    ) %>%
    dplyr::filter(
      .data$dataset == dataset_label,
      .data$flagged_for_removal %in% TRUE
    ) %>%
    dplyr::mutate(
      scale_norm = toupper(trimws(as.character(.data$scale))),
      suq_item_num = get_suq_item_num_local(.data$item),
      suq_pair_key = get_suq_pair_key_local(.data$item)
    )
  
  # SUQ rule:
  # - ignore Q1 completely
  # - if Q2 or Q3 is flagged, remove both Q2 and Q3
  x <- x %>%
    dplyr::filter(!(.data$scale_norm == "SUQ" & .data$suq_item_num == 1L))
  
  suq_pair_mates <- x %>%
    dplyr::filter(
      .data$scale_norm == "SUQ",
      .data$suq_item_num %in% c(2L, 3L)
    ) %>%
    dplyr::mutate(
      item = dplyr::if_else(
        .data$suq_item_num == 2L,
        paste0(.data$suq_pair_key, "3"),
        paste0(.data$suq_pair_key, "2")
      ),
      loading = NA_real_,
      flagged_for_removal = TRUE
    )
  
  out <- dplyr::bind_rows(x, suq_pair_mates) %>%
    dplyr::select(-dplyr::any_of(c("scale_norm", "suq_item_num", "suq_pair_key"))) %>%
    dplyr::distinct(.data$dataset, .data$level, .data$scale, .data$subscale, .data$item, .keep_all = TRUE)
  
  log_msg(
    "Read ", nrow(out), " flagged items for dataset '", dataset_label,
    "' from helper file."
  )
  
  out
}

drop_flagged_items_from_keys <- function(keys, flagged_tbl) {
  if (is.null(flagged_tbl) || !nrow(flagged_tbl)) return(keys)
  
  flags <- flagged_tbl %>%
    dplyr::mutate(
      level_norm = tolower(trimws(as.character(.data$level))),
      scale_norm = toupper(trimws(as.character(.data$scale))),
      subscale_norm = tolower(trimws(as.character(.data$subscale))),
      item_norm = normalize_id_local(.data$item)
    )
  
  remove_flagged <- function(items, level, scale, subscale = NA_character_) {
    items <- as.character(items)
    # Literature-defined factors retain their complete Table 2 membership in
    # every output variant. Filtering still applies to the original CAPE scores.
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
    
    items[!(normalize_id_local(items) %in% drop_norm)]
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

get_scale_scoring_mode <- function(scoring_df, scale, default = "mean", force_mean = FALSE) {
  if (isTRUE(force_mean)) return("mean")
  if (toupper(trimws(scale)) == "SUQ") return("sum")
  if (is.null(scoring_df) || !"scale" %in% names(scoring_df)) return(default)
  
  cand_cols <- intersect(
    names(scoring_df),
    c("mode","scoring","method","aggregation","score_type","score_method","compute")
  )
  if (!length(cand_cols)) return(default)
  
  sc_key <- toupper(trimws(scale))
  s_key  <- toupper(trimws(as.character(scoring_df$scale)))
  idx    <- which(s_key == sc_key)
  if (!length(idx)) return(default)
  
  for (cc in cand_cols) {
    v <- scoring_df[[cc]][idx[1]]
    if (is.na(v)) next
    v0 <- tolower(trimws(as.character(v)))
    if (grepl("sum|total", v0)) return("sum")
    if (grepl("mean|avg|average", v0)) return("mean")
  }
  
  default
}

make_col_map <- function(df) {
  tibble::tibble(orig = names(df), item_norm = normalize_id_local(names(df))) %>%
    dplyr::distinct(item_norm, .keep_all = TRUE)
}

score_items_wide <- function(d, items, col_map, agg = c("mean","sum")) {
  agg <- match.arg(agg)
  
  items_norm <- normalize_id_local(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (!length(keep_norm)) return(rep(NA_real_, nrow(d)))
  
  orig_cols <- col_map$orig[match(keep_norm, col_map$item_norm)]
  orig_cols <- orig_cols[!is.na(orig_cols)]
  if (!length(orig_cols)) return(rep(NA_real_, nrow(d)))
  
  M <- d[, orig_cols, drop = FALSE]
  M[] <- lapply(M, function(z) suppressWarnings(as.numeric(z)))
  
  n_nonmiss <- rowSums(!is.na(as.matrix(M)))
  out <- if (agg == "sum") rowSums(M, na.rm = TRUE) else rowMeans(M, na.rm = TRUE)
  out[n_nonmiss == 0] <- NA_real_
  out
}

score_cape_positive_schlier2015_wide <- function(d, items, col_map, subscale, metadata) {
  definition <- metadata$factors[metadata$factors$subscale == subscale, , drop = FALSE]
  nums <- suppressWarnings(as.integer(stringr::str_extract(as.character(items), "[0-9]+$")))
  if (nrow(definition) != 1L || length(items) != definition$n_items[1] ||
      anyNA(nums) || anyDuplicated(nums) ||
      !setequal(nums, unlist(definition$item_numbers[[1]], use.names = FALSE)) ||
      !setequal(normalize_id_local(items), normalize_id_local(definition$items[[1]]))) {
    stop("CAPE Schlier2015 item membership differs from Table 2 for '", subscale, "'.")
  }

  cols <- col_map$orig[match(normalize_id_local(items), col_map$item_norm)]
  if (anyNA(cols)) {
    stop("CAPE Schlier2015 frequency item columns missing from the clean master: ",
         paste(items[is.na(cols)], collapse = ", "))
  }
  # Frequency only. Never mix frequency with distress or replace structural
  # distress zeros. Original item columns and CAPE scores keep their 1-4 coding.
  M <- d[, cols, drop = FALSE]
  M[] <- lapply(M, function(x) suppressWarnings(as.numeric(as.character(x))))
  values <- as.matrix(M)
  if (any(!is.na(values) & (!is.finite(values) |
                           values < metadata$clean_frequency_min |
                           values > metadata$clean_frequency_max))) {
    stop("CAPE Schlier2015 expects cleaned frequency items coded 1-4. Check Step 03 for '", subscale, "'.")
  }
  values <- values - metadata$clean_frequency_min + metadata$item_score_min
  # Require every factor item. Step 03 already handles missing items globally;
  # do not silently produce a partial sum when a required response is missing.
  list(sum = rowSums(values), mean = rowMeans(values))
}

# CTQ-SF items 10, 16 and 22 form a separate 0-3 Minimization/Denial
# indicator. They must never contribute to the CTQ maltreatment total.
is_ctq_md_subscale <- function(scale, subscale) {
  if (!identical(toupper(trimws(as.character(scale))), "CTQ")) return(FALSE)
  label <- safe_score_name(subscale)
  isTRUE(grepl("minimi[sz]|denial|verleugn|bagatellis|^m_d$|^md$", label))
}

is_ctq_md_item_number <- function(items) {
  compact <- gsub("[^a-z0-9]", "", normalize_id_local(items))
  grepl("^(ctq)?0?(10|16|22)$", compact)
}

get_ctq_md_definition <- function(keys) {
  scale_rows <- keys$items_by_scale
  if (is.null(scale_rows) || !nrow(scale_rows)) return(NULL)
  ctq_rows <- scale_rows[!is.na(scale_rows$scale) &
                            toupper(trimws(as.character(scale_rows$scale))) == "CTQ", , drop = FALSE]
  if (!nrow(ctq_rows)) return(NULL)

  subs <- keys$items_by_subscale
  md_rows <- if (!is.null(subs) && nrow(subs)) {
    subs[vapply(seq_len(nrow(subs)), function(i) {
      is_ctq_md_subscale(subs$scale[i], subs$subscale[i])
    }, logical(1)), , drop = FALSE]
  } else NULL

  # Prefer the explicit subscale key, allowing dataset-specific item names.
  # Fall back to the three standard CTQ-SF item numbers in the CTQ scale key.
  items <- if (!is.null(md_rows) && nrow(md_rows)) {
    unique(unlist(md_rows$items, use.names = FALSE))
  } else {
    ctq_items <- unique(unlist(ctq_rows$items, use.names = FALSE))
    ctq_items[is_ctq_md_item_number(ctq_items)]
  }
  items <- items[!is.na(items) & nzchar(trimws(as.character(items)))]
  if (length(unique(normalize_id_local(items))) != 3L) {
    stop("CTQ Minimization/Denial needs exactly three item keys (items 10, 16, 22); found ",
         length(unique(normalize_id_local(items))), ". Check the CTQ keys before scoring.")
  }

  label <- if (!is.null(md_rows) && nrow(md_rows)) {
    safe_score_name(md_rows$subscale[1])
  } else "minimization_denial"
  list(items = items, label = label)
}

add_ctq_md_score <- function(df, md_definition, prefix = "score_") {
  if (is.null(md_definition)) return(df)
  col_map <- make_col_map(df)
  item_norm <- normalize_id_local(md_definition$items)
  cols <- col_map$orig[match(item_norm, col_map$item_norm)]
  if (anyNA(cols)) {
    stop("CTQ Minimization/Denial item columns missing from clean master: ",
         paste(md_definition$items[is.na(cols)], collapse = ", "))
  }

  responses <- df[, cols, drop = FALSE]
  responses[] <- lapply(responses, function(x) suppressWarnings(as.numeric(as.character(x))))
  values <- as.matrix(responses)
  if (any(!is.na(values) & !(as.vector(values) %in% 1:5))) {
    stop("CTQ Minimization/Denial responses must be coded from 1 to 5.")
  }
  score <- rowSums(values == 5, na.rm = TRUE)
  score[rowSums(is.na(values)) > 0L] <- NA_real_
  new_col <- paste0(prefix, "ctq__", md_definition$label)
  df[[new_col]] <- score
  log_msg("Scored CTQ Minimization/Denial -> ", new_col,
          " (three items: response 5 = 1, responses 1-4 = 0; range 0-3)")
  df
}

add_z_score_columns <- function(df,
                                score_pattern = "^score_",
                                z_prefix = "z_",
                                dataset_label = NA_character_) {
  score_cols <- grep(score_pattern, names(df), value = TRUE)
  score_cols <- score_cols[!startsWith(score_cols, z_prefix)]
  
  if (!length(score_cols)) return(df)
  
  for (cc in score_cols) {
    x <- suppressWarnings(as.numeric(df[[cc]]))
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    
    z_col <- paste0(z_prefix, cc)
    
    if (!is.finite(s) || s <= 0) {
      df[[z_col]] <- NA_real_
      log_msg(
        "Z-scoring skipped for '", cc, "'",
        if (!is.na(dataset_label)) paste0(" [", dataset_label, "]") else "",
        " because SD is zero or not finite."
      )
    } else {
      df[[z_col]] <- (x - m) / s
    }
  }
  
  log_msg(
    "Added ", length(score_cols), " z-scored score columns",
    if (!is.na(dataset_label)) paste0(" [", dataset_label, "]") else "",
    "."
  )
  
  df
}

get_suq_q2_q3_cols <- function(items, col_map) {
  items_chr <- as.character(items)
  
  items_tbl <- tibble::tibble(
    item = items_chr,
    item_norm = normalize_id_local(items_chr),
    item_num = suppressWarnings(as.integer(stringr::str_extract(items_chr, "[123]$")))
  )
  
  mapped <- items_tbl %>%
    dplyr::inner_join(col_map, by = "item_norm") %>%
    dplyr::filter(.data$item_num %in% c(2L, 3L))
  
  q2_col <- mapped$orig[mapped$item_num == 2L][1]
  q3_col <- mapped$orig[mapped$item_num == 3L][1]
  
  ok <- !is.na(q2_col) &&
    !is.na(q3_col) &&
    q2_col %in% col_map$orig &&
    q3_col %in% col_map$orig
  
  list(ok = ok, q2_col = q2_col, q3_col = q3_col)
}

can_score_suq_subscale_wide <- function(items, col_map) {
  pair <- get_suq_q2_q3_cols(items, col_map)
  isTRUE(pair$ok)
}

has_scorable_suq_subscales <- function(keys, col_map) {
  if (is.null(keys$items_by_subscale) || !nrow(keys$items_by_subscale)) {
    return(FALSE)
  }
  
  suq_subs <- keys$items_by_subscale %>%
    dplyr::filter(toupper(trimws(as.character(.data$scale))) == "SUQ") %>%
    dplyr::filter(!is.na(.data$subscale), .data$subscale != "")
  
  if (!nrow(suq_subs)) return(FALSE)
  
  any(vapply(
    seq_len(nrow(suq_subs)),
    function(i) can_score_suq_subscale_wide(suq_subs$items[[i]], col_map),
    logical(1)
  ))
}

score_suq_subscale_wide <- function(d, items, col_map) {
  pair <- get_suq_q2_q3_cols(items, col_map)
  
  if (!isTRUE(pair$ok)) {
    return(rep(NA_real_, nrow(d)))
  }
  
  q2 <- suppressWarnings(as.numeric(d[[pair$q2_col]]))
  q3 <- suppressWarnings(as.numeric(d[[pair$q3_col]]))
  
  q2 * q3
}

score_suq_total_wide <- function(d, keys, col_map, agg = c("sum", "mean")) {
  agg <- match.arg(agg)
  if (is.null(keys$items_by_subscale) || !nrow(keys$items_by_subscale)) {
    return(rep(NA_real_, nrow(d)))
  }
  
  suq_subs <- keys$items_by_subscale %>%
    dplyr::filter(toupper(trimws(as.character(.data$scale))) == "SUQ") %>%
    dplyr::filter(!is.na(.data$subscale), .data$subscale != "")
  
  if (!nrow(suq_subs)) return(rep(NA_real_, nrow(d)))
  
  can_score <- vapply(
    seq_len(nrow(suq_subs)),
    function(i) can_score_suq_subscale_wide(suq_subs$items[[i]], col_map),
    logical(1)
  )
  
  suq_subs <- suq_subs[can_score, , drop = FALSE]
  
  if (!nrow(suq_subs)) {
    return(rep(NA_real_, nrow(d)))
  }
  
  S <- purrr::map_dfc(seq_len(nrow(suq_subs)), function(i) {
    nm <- safe_score_name(suq_subs$subscale[i])
    tibble::tibble(
      !!nm := score_suq_subscale_wide(d, suq_subs$items[[i]], col_map)
    )
  })
  
  n_nonmiss <- rowSums(!is.na(as.matrix(S)))
  out <- if (agg == "sum") rowSums(S, na.rm = TRUE) else rowMeans(S, na.rm = TRUE)
  out[n_nonmiss == 0] <- NA_real_
  out
}

score_suq_composite_wide <- function(d, keys, col_map,
                                     exclude_subscales = character(0),
                                     agg = c("sum", "mean")) {
  agg <- match.arg(agg)

  if (is.null(keys$items_by_subscale) || !nrow(keys$items_by_subscale)) {
    return(NULL)
  }

  exclude_norm <- tolower(trimws(as.character(exclude_subscales)))

  suq_subs <- keys$items_by_subscale %>%
    dplyr::filter(toupper(trimws(as.character(.data$scale))) == "SUQ") %>%
    dplyr::filter(!is.na(.data$subscale), .data$subscale != "") %>%
    dplyr::mutate(.subscale_norm = tolower(trimws(as.character(.data$subscale)))) %>%
    dplyr::filter(!(.data$.subscale_norm %in% exclude_norm))

  if (!nrow(suq_subs)) return(NULL)

  can_score <- vapply(
    seq_len(nrow(suq_subs)),
    function(i) can_score_suq_subscale_wide(suq_subs$items[[i]], col_map),
    logical(1)
  )

  suq_subs <- suq_subs[can_score, , drop = FALSE]
  if (!nrow(suq_subs)) return(NULL)

  S <- purrr::map_dfc(seq_len(nrow(suq_subs)), function(i) {
    nm <- safe_score_name(suq_subs$subscale[i])
    tibble::tibble(
      !!nm := score_suq_subscale_wide(d, suq_subs$items[[i]], col_map)
    )
  })

  n_nonmiss <- rowSums(!is.na(as.matrix(S)))
  out <- if (agg == "sum") rowSums(S, na.rm = TRUE) else rowMeans(S, na.rm = TRUE)
  out[n_nonmiss == 0] <- NA_real_

  list(
    score = out,
    included_subscales = as.character(suq_subs$subscale)
  )
}

add_suq_illegal_drugs_score <- function(
    df,
    keys,
    prefix = "score_",
    label = CFG$suq_illegal_drugs_label,
    exclude_subscales = CFG$suq_illegal_drugs_exclude,
    agg = c("sum", "mean")) {

  agg <- match.arg(agg)
  col_map <- make_col_map(df)

  composite <- score_suq_composite_wide(
    d = df,
    keys = keys,
    col_map = col_map,
    exclude_subscales = exclude_subscales,
    agg = agg
  )

  new_col <- paste0(
    prefix,
    safe_score_name("SUQ"),
    "__",
    safe_score_name(label)
  )

  if (is.null(composite)) {
    log_msg(
      "Skipped derived SUQ subscale '", label, "' -> ", new_col,
      " because no complete eligible SUQ Q2/Q3 pairs remained."
    )
    return(df)
  }

  df[[new_col]] <- composite$score

  log_msg(
    "Scored derived SUQ subscale '", label, "' -> ", new_col,
    " (mode=", agg, " of: ",
    paste(composite$included_subscales, collapse = ", "),
    ")."
  )

  df
}

add_scale_scores <- function(df, keys, scoring_df,
                             prefix = "score_",
                             default_min_prop = CFG$min_prop_items_default,
                             exclude_scales = character(0),
                             force_mean_scores = FALSE,
                             suq_total_agg = "sum",
                             ctq_md_definition = NULL) {
  if (is.null(keys) || is.null(keys$items_by_scale) || !nrow(keys$items_by_scale)) return(df)
  
  col_map <- make_col_map(df)
  
  scales_tbl <- keys$items_by_scale %>%
    dplyr::filter(!is.na(scale), scale != "") %>%
    dplyr::filter(!(toupper(scale) %in% toupper(exclude_scales)))
  
  if (!nrow(scales_tbl)) return(df)
  
  for (i in seq_len(nrow(scales_tbl))) {
    sc    <- as.character(scales_tbl$scale[i])
    items <- scales_tbl$items[[i]]
    if (toupper(trimws(sc)) == "CTQ" && !is.null(ctq_md_definition)) {
      md_norm <- normalize_id_local(ctq_md_definition$items)
      items <- items[!(normalize_id_local(items) %in% md_norm | is_ctq_md_item_number(items))]
    }
    if (length(items) < 1L) next
    
    new_col <- paste0(prefix, safe_score_name(sc))
    
    if (toupper(trimws(sc)) == "SUQ") {
      if (!has_scorable_suq_subscales(keys, col_map)) {
        log_msg(
          "Skipped scale '", sc, "' -> ", new_col,
          " because no complete SUQ Q2/Q3 subscale pairs remained after filtering."
        )
        next
      }
      
      df[[new_col]] <- score_suq_total_wide(df, keys, col_map, agg = suq_total_agg)
      log_msg("Scored scale '", sc, "' -> ", new_col, " (mode=", suq_total_agg, " of SUQ Q2*Q3 subscale scores)")
      next
    }
    
    mode_i <- get_scale_scoring_mode(scoring_df, sc, default = "mean", force_mean = force_mean_scores)
    mp_i <- default_min_prop

    df[[new_col]] <- score_items_wide(df, items, col_map, agg = mode_i)
    
    log_msg("Scored scale '", sc, "' -> ", new_col, " (mode=", mode_i, ", min_prop=", mp_i, ")")
  }
  
  df
}

add_subscale_scores <- function(df, keys, scoring_df,
                                prefix = "score_",
                                default_min_prop = CFG$min_prop_items_default,
                                exclude_scales = character(0),
                                force_mean_scores = FALSE) {
  if (is.null(keys) || is.null(keys$items_by_subscale) || !nrow(keys$items_by_subscale)) return(df)
  
  col_map <- make_col_map(df)
  
  subs_tbl <- keys$items_by_subscale %>%
    dplyr::filter(!is.na(scale), scale != "") %>%
    dplyr::filter(!is.na(subscale), subscale != "") %>%
    dplyr::filter(!(toupper(scale) %in% toupper(exclude_scales))) %>%
    dplyr::distinct(scale, subscale, .keep_all = TRUE)
  
  if (!nrow(subs_tbl)) return(df)
  
  for (i in seq_len(nrow(subs_tbl))) {
    sc    <- as.character(subs_tbl$scale[i])
    sub   <- as.character(subs_tbl$subscale[i])
    items <- subs_tbl$items[[i]]
    if (is_ctq_md_subscale(sc, sub)) next  # scored separately from the original three items
    if (length(items) < 1L) {
      log_msg(
        "Skipped subscale '", sc, " / ", sub,
        "' because no items remained after level-specific filtering."
      )
      next
    }    
    new_col <- paste0(prefix, safe_score_name(sc), "__", safe_score_name(sub))

    if (is_cape_positive_schlier2015_subscale(sc, sub)) {
      extra <- score_cape_positive_schlier2015_wide(
        df, items, col_map, sub, keys$cape_positive_schlier2015
      )
      df[[paste0(new_col, "_sum")]] <- extra$sum
      df[[paste0(new_col, "_mean")]] <- extra$mean
      log_msg("Scored fixed CAPE Schlier2015 factor '", sub,
              "' (frequency only; 0-3 item coding; sum + mean; ", length(items), " items)")
      next
    }
    
    if (toupper(trimws(sc)) == "SUQ") {
      if (!can_score_suq_subscale_wide(items, col_map)) {
        log_msg(
          "Skipped subscale '", sc, " / ", sub, "' -> ", new_col,
          " because the SUQ Q2/Q3 pair was incomplete after filtering."
        )
        next
      }
      
      df[[new_col]] <- score_suq_subscale_wide(df, items, col_map)
      log_msg("Scored subscale '", sc, " / ", sub, "' -> ", new_col,
              " (mode=SUQ Q2*Q3)")
      next
    }
    
    mode_i <- get_scale_scoring_mode(scoring_df, sc, default = "mean", force_mean = force_mean_scores)
    mp_i <- default_min_prop

    df[[new_col]] <- score_items_wide(df, items, col_map, agg = mode_i)
    
    log_msg("Scored subscale '", sc, " / ", sub, "' -> ", new_col,
            " (mode=", mode_i, ", min_prop=", mp_i, ")")
  }
  
  df
}


assert_no_missing_scores <- function(df, sample, suffix = NULL) {
  score_cols <- grep("^score_", names(df), value = TRUE)
  if (!length(score_cols)) {
    log_msg("No score columns found for sample '", sample, "'.")
    return(invisible(TRUE))
  }
  
  miss <- vapply(score_cols, function(cc) sum(is.na(df[[cc]])), integer(1))
  miss <- miss[miss > 0]
  
  label <- if (is.null(suffix) || !nzchar(suffix)) "unfiltered" else suffix
  
  if (length(miss)) {
    stop(
      "Missing values detected in scored output for sample '", sample,
      "' (", label, "): ",
      paste(names(miss), miss, sep = "=", collapse = ", ")
    )
  }
  
  log_msg("Missing-score check for sample '", sample, "' (", label, "): passed.")
  invisible(TRUE)
}

# ---- FHS family-history scoring ----------------------------------------------
fhs_compact_name <- function(x) {
  gsub("[^a-z0-9]", "", tolower(as.character(x)))
}

fhs_value_present <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

fhs_as_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

read_fhs_item_info <- function(sample) {
  path <- latest_iteminfo_for_sample(sample)
  if (is.na(path) || !fs::file_exists(path)) {
    fallback <- latest_iteminfo_for_sample("adults")
    if (!is.na(fallback) && fs::file_exists(fallback)) path <- fallback
  }
  if (is.na(path) || !fs::file_exists(path)) {
    log_msg(
      "No Item Information found for FHS scoring in sample '", sample,
      "'. Falling back to column-name detection."
    )
    return(NULL)
  }
  log_msg("Using Item Information for FHS scoring: ", path)
  suppressMessages(readxl::read_excel(path)) %>% janitor::clean_names()
}

get_fhs_source_columns <- function(df, item_info = NULL) {
  df_compact <- fhs_compact_name(names(df))

  if (!is.null(item_info) && all(c("item", "scale") %in% names(item_info))) {
    fhs_items <- item_info %>%
      dplyr::filter(fhs_compact_name(.data$scale) == "fhsfamilytree") %>%
      dplyr::pull("item") %>%
      fhs_compact_name() %>%
      unique()
    matched <- names(df)[df_compact %in% fhs_items]
    if (length(matched)) return(matched)
  }

  # Schema-drift fallback. The Item Information route above is preferred.
  is_fhs <- grepl(
    "^[a-z0-9]+confidence(self|parent[1-9]|sibling[1-9]|child[1-9])$",
    df_compact
  ) |
    grepl("^ownpsychdiagn", df_compact) |
    df_compact %in% c(
      "siblings", "children", "relativesinfo", "fhsopentext",
      "parents001", "parents002", "parentsgender001", "parentsgender002"
    )
  names(df)[is_fhs]
}

first_fhs_column <- function(df, compact_targets) {
  compact <- fhs_compact_name(names(df))
  for (target in compact_targets) {
    hit <- which(compact == fhs_compact_name(target))
    if (length(hit)) return(names(df)[hit[1]])
  }
  NA_character_
}

fhs_numeric_matrix <- function(df, columns) {
  if (!length(columns)) {
    return(matrix(numeric(0), nrow = nrow(df), ncol = 0L))
  }
  do.call(cbind, lapply(columns, function(column) fhs_as_numeric(df[[column]])))
}

fhs_character_matrix <- function(df, columns) {
  if (!length(columns)) {
    return(matrix(character(0), nrow = nrow(df), ncol = 0L))
  }
  do.call(cbind, lapply(columns, function(column) as.character(df[[column]])))
}

fhs_diagnosis_list <- function(values, diagnoses, cutoff, none_label) {
  if (!nrow(values)) return(character(0))
  apply(values, 1L, function(one_row) {
    if (all(is.na(one_row))) return(NA_character_)
    present <- unique(diagnoses[!is.na(one_row) & one_row >= cutoff])
    if (!length(present)) none_label else paste(present, collapse = ", ")
  })
}

add_fhs_scores <- function(df, sample, item_info = NULL) {
  input_n <- nrow(df)
  source_cols <- get_fhs_source_columns(df, item_info)
  if (!length(source_cols)) {
    stop(
      "No FHSfamilytree source columns found for sample '", sample,
      "'. Refusing to write scored masters without the requested FHS output."
    )
  }

  work <- df
  id_col <- first_fhs_column(work, c("vp_id", "vpid", "id"))

  present_matrix <- do.call(
    cbind,
    lapply(source_cols, function(column) fhs_value_present(work[[column]]))
  )
  fhs_answered <- rowSums(present_matrix) > 0L

  duplicate_id <- rep(FALSE, nrow(work))
  if (!is.na(id_col)) {
    ids <- trimws(as.character(work[[id_col]]))
    valid_id <- !is.na(ids) & nzchar(ids)
    duplicate_id <- valid_id & (duplicated(ids) | duplicated(ids, fromLast = TRUE))
  }

  open_text_col <- first_fhs_column(work, "FHSOpenText")
  own_other_col <- first_fhs_column(work, "ownpsychdiagnother")
  open_text_review <- if (is.na(open_text_col)) {
    rep(FALSE, nrow(work))
  } else {
    fhs_value_present(work[[open_text_col]])
  }
  own_other_review <- if (is.na(own_other_col)) {
    rep(FALSE, nrow(work))
  } else {
    fhs_value_present(work[[own_other_col]])
  }

  compact <- fhs_compact_name(names(work))
  confidence_match <- stringr::str_match(
    compact,
    "^(.+?)confidence(self|parent|sibling|child)([1-9]?)$"
  )
  confidence_rows <- which(!is.na(confidence_match[, 1]))
  if (!length(confidence_rows)) {
    stop(
      "FHS columns were found for sample '", sample,
      "', but no diagnosis-confidence columns matched the expected schema."
    )
  }

  confidence_meta <- tibble::tibble(
    column = names(work)[confidence_rows],
    diagnosis = confidence_match[confidence_rows, 2],
    relation = confidence_match[confidence_rows, 3],
    index = confidence_match[confidence_rows, 4]
  )
  confidence_meta$index[confidence_meta$index == ""] <- "0"

  siblings_col <- first_fhs_column(work, "siblings")
  children_col <- first_fhs_column(work, "children")
  siblings_n <- if (is.na(siblings_col)) rep(NA_real_, nrow(work)) else fhs_as_numeric(work[[siblings_col]])
  children_n <- if (is.na(children_col)) rep(NA_real_, nrow(work)) else fhs_as_numeric(work[[children_col]])

  # Missing confidence means zero only when the person/relative exists. For
  # people with no FHS response, values and all derived FHS fields stay NA.
  for (i in seq_len(nrow(confidence_meta))) {
    column <- confidence_meta$column[i]
    relation <- confidence_meta$relation[i]
    relative_index <- suppressWarnings(as.integer(confidence_meta$index[i]))
    values <- fhs_as_numeric(work[[column]])
    relative_exists <- switch(
      relation,
      self = rep(TRUE, nrow(work)),
      parent = rep(TRUE, nrow(work)),
      sibling = !is.na(siblings_n) & relative_index <= siblings_n,
      child = !is.na(children_n) & relative_index <= children_n,
      rep(FALSE, nrow(work))
    )
    replace_zero <- fhs_answered & relative_exists & is.na(values)
    values[replace_zero] <- 0
    work[[column]] <- values
  }

  work$fhs_any_response <- fhs_answered
  work$qc_fhs_duplicate_vp_id <- duplicate_id
  work$qc_fhs_open_text_requires_review <- open_text_review
  work$qc_fhs_own_diagnosis_other_requires_review <- own_other_review

  cutoff <- CFG$fhs_confidence_cutoff
  self_meta <- confidence_meta[confidence_meta$relation == "self", , drop = FALSE]
  self_values <- fhs_numeric_matrix(work, self_meta$column)
  self_count <- if (ncol(self_values)) {
    rowSums(self_values >= cutoff, na.rm = TRUE)
  } else {
    rep(NA_real_, nrow(work))
  }
  self_count[!fhs_answered] <- NA_real_
  work$fhs_num_diagnoses_self <- as.integer(self_count)

  own_match <- stringr::str_match(compact, "^ownpsychdiagn(.+)$")
  own_rows <- which(!is.na(own_match[, 1]) & own_match[, 2] != "other")
  own_meta <- tibble::tibble(
    column = names(work)[own_rows],
    diagnosis = own_match[own_rows, 2]
  )
  own_values <- fhs_character_matrix(work, own_meta$column)
  own_yes <- matrix(FALSE, nrow = nrow(work), ncol = ncol(own_values))
  if (ncol(own_values)) {
    own_yes <- matrix(
      toupper(trimws(as.character(own_values))) == "Y",
      nrow = nrow(work),
      ncol = ncol(own_values)
    )
    own_yes[is.na(own_yes)] <- FALSE
  }
  treated_count <- if (ncol(own_yes)) rowSums(own_yes) else rep(NA_real_, nrow(work))
  treated_count[!fhs_answered] <- NA_real_
  work$fhs_num_treated_diagnoses_self <- as.integer(treated_count)

  entity_meta <- confidence_meta %>%
    dplyr::filter(.data$relation != "self") %>%
    dplyr::distinct(.data$relation, .data$index)
  entity_counts <- list()
  entity_info <- tibble::tibble(
    key = character(), relation = character(), index = character()
  )

  for (i in seq_len(nrow(entity_meta))) {
    relation <- entity_meta$relation[i]
    relative_index <- entity_meta$index[i]
    columns <- confidence_meta$column[
      confidence_meta$relation == relation & confidence_meta$index == relative_index
    ]
    diagnoses <- confidence_meta$diagnosis[
      confidence_meta$relation == relation & confidence_meta$index == relative_index
    ]
    values <- fhs_numeric_matrix(work, columns)
    has_observation <- rowSums(!is.na(values)) > 0L
    count <- ifelse(
      has_observation,
      rowSums(values >= cutoff, na.rm = TRUE),
      NA_real_
    )
    count[!fhs_answered] <- NA_real_
    key <- paste(relation, relative_index, sep = "_")
    output_name <- paste0("fhs_num_diagnoses_", key)
    work[[output_name]] <- as.integer(count)
    entity_counts[[key]] <- count
    entity_info <- dplyr::bind_rows(
      entity_info,
      tibble::tibble(key = key, relation = relation, index = relative_index)
    )

    diagnosis_list <- fhs_diagnosis_list(values, diagnoses, cutoff, "nodiagn")
    diagnosis_list[!fhs_answered] <- NA_character_
    work[[paste0("fhs_all_diagnoses_", key)]] <- diagnosis_list
  }

  entity_count_matrix <- if (length(entity_counts)) {
    do.call(cbind, entity_counts)
  } else {
    matrix(numeric(0), nrow = nrow(work), ncol = 0L)
  }

  diagnosed_by_relation <- function(relation) {
    keys <- entity_info$key[entity_info$relation == relation]
    if (!length(keys)) return(rep(NA_integer_, nrow(work)))
    values <- entity_count_matrix[, keys, drop = FALSE]
    has_relative <- rowSums(!is.na(values)) > 0L
    result <- ifelse(
      has_relative,
      rowSums(values >= 1, na.rm = TRUE),
      NA_real_
    )
    result[!fhs_answered] <- NA_real_
    as.integer(result)
  }

  work$fhs_parents_with_diagnosis <- diagnosed_by_relation("parent")
  work$fhs_siblings_with_diagnosis <- diagnosed_by_relation("sibling")
  work$fhs_children_with_diagnosis <- diagnosed_by_relation("child")

  relatives_total <- siblings_n + children_n + 2
  relatives_total[!fhs_answered] <- NA_real_
  work$fhs_relatives_total <- as.integer(relatives_total)

  relatives_with_diagnosis <- if (ncol(entity_count_matrix)) {
    rowSums(entity_count_matrix >= 1, na.rm = TRUE)
  } else {
    rep(NA_real_, nrow(work))
  }
  relatives_with_diagnosis[!fhs_answered] <- NA_real_
  work$fhs_relatives_with_diagnosis <- as.integer(relatives_with_diagnosis)

  relative_diagnoses <- unique(
    confidence_meta$diagnosis[confidence_meta$relation != "self"]
  )
  for (diagnosis in relative_diagnoses) {
    columns <- confidence_meta$column[
      confidence_meta$relation != "self" & confidence_meta$diagnosis == diagnosis
    ]
    values <- fhs_numeric_matrix(work, columns)
    diagnosis_count <- rowSums(values >= cutoff, na.rm = TRUE)
    diagnosis_count[!fhs_answered] <- NA_real_
    safe_diagnosis <- safe_score_name(diagnosis)
    count_name <- paste0("fhs_relatives_with_", safe_diagnosis)
    prop_name <- paste0("fhs_prop_relatives_with_", safe_diagnosis)
    work[[count_name]] <- as.integer(diagnosis_count)
    proportion <- diagnosis_count / relatives_total
    proportion[!is.finite(proportion)] <- NA_real_
    work[[prop_name]] <- proportion
  }

  self_diagnosis_list <- if (ncol(self_values)) {
    fhs_diagnosis_list(self_values, self_meta$diagnosis, cutoff, "nodiagn")
  } else {
    rep(NA_character_, nrow(work))
  }
  self_diagnosis_list[!fhs_answered] <- NA_character_
  work$fhs_all_diagnoses_self <- self_diagnosis_list

  treated_diagnosis_list <- rep(NA_character_, nrow(work))
  if (ncol(own_yes)) {
    treated_diagnosis_list <- apply(own_yes, 1L, function(one_row) {
      present <- unique(own_meta$diagnosis[one_row])
      if (!length(present)) "nodiagntreated" else paste(present, collapse = ", ")
    })
  }
  treated_diagnosis_list[!fhs_answered] <- NA_character_
  work$fhs_treated_diagnoses_self <- treated_diagnosis_list

  required_fhs_output <- c(
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
  missing_output <- setdiff(required_fhs_output, names(work))
  if (length(missing_output)) {
    stop(
      "FHS scoring failed to create required output columns for sample '",
      sample, "': ", paste(missing_output, collapse = ", ")
    )
  }
  if (nrow(work) != input_n) {
    stop(
      "FHS scoring changed the number of rows for sample '", sample,
      "' (", input_n, " -> ", nrow(work), ")."
    )
  }

  log_msg(
    "FHS scoring for sample '", sample, "': retained all ", nrow(work),
    " row(s); ", sum(fhs_answered), " had at least one FHS response and ",
    sum(!fhs_answered), " retained row(s) received NA for derived FHS fields."
  )
  if (sum(open_text_review | own_other_review) > 0L) {
    log_msg(
      "FHS free-text review flag for sample '", sample, "': ",
      sum(open_text_review | own_other_review), " participant(s) require review."
    )
  }
  work
}

write_master_variant <- function(df, sample, input_date, suffix = NULL) {
  out_dir <- fs::path(DIR_EXPORT, sample)
  fs::dir_create(out_dir)
  
  fname <- if (is.null(suffix) || !nzchar(suffix)) {
    glue::glue("{input_date}_{sample}_clean_master_scored.xlsx")
  } else {
    glue::glue("{input_date}_{sample}_clean_master_scored_{suffix}.xlsx")
  }
  
  out_path <- fs::path(out_dir, fname)
  writexl::write_xlsx(df, out_path)
  log_msg("Wrote scored master: ", out_path)
  out_path
}

process_sample <- function(sample, scoring_df, flag_helper_path = NA_character_) {
  log_msg("\n--- Scoring sample: ", sample, " ---")
  
  sample_inputs <- resolve_sample_inputs(sample)
  master_path <- sample_inputs$master_path
  keys_path <- sample_inputs$keys_path
  input_date <- sample_inputs$input_date

  log_msg("Using dated Step 03 master: ", master_path)
  log_msg("Using matching Step 03 key: ", keys_path)

  df   <- read_master_robust(master_path)
  keys <- readRDS(keys_path)
  validate_cape_positive_schlier2015_keys(keys)
  ctq_md_definition <- get_ctq_md_definition(keys)

  # FHS remains outside the internal-consistency/item-loading workflow, but its
  # derived family-history variables belong in every scored master variant.
  fhs_item_info <- read_fhs_item_info(sample)
  df <- add_fhs_scores(df, sample, item_info = fhs_item_info)
  
  # round 1: unfiltered scores
  df_scored <- df
  df_scored <- add_scale_scores(
    df_scored, keys, scoring_df,
    prefix = "score_",
    exclude_scales = NON_SCORABLE_SCALES,
    ctq_md_definition = ctq_md_definition
  )
  df_scored <- add_subscale_scores(
    df_scored, keys, scoring_df,
    prefix = "score_",
    exclude_scales = NON_SCORABLE_SCALES
  )
  df_scored <- add_ctq_md_score(df_scored, ctq_md_definition)
  df_scored <- add_suq_illegal_drugs_score(
    df_scored,
    keys,
    prefix = "score_",
    agg = "sum"
  )
  assert_no_missing_scores(df_scored, sample)
  
  if (isTRUE(CFG$add_z_scores_to_scored_masters)) {
    df_scored <- add_z_score_columns(
      df_scored,
      dataset_label = paste0(sample, " unfiltered")
    )
  }
  
  write_master_variant(df_scored, sample, input_date = input_date)

  # round 2: filtered scores
  if (isTRUE(CFG$export_filtered_scores)) {
    if (!is.na(flag_helper_path) && fs::file_exists(flag_helper_path)) {
      flagged_tbl <- read_flagged_items(
        path = flag_helper_path,
        dataset_label = sample,
        threshold_value = CFG$loading_threshold
      )
      
      log_msg("Applying filtered scoring for sample '", sample, "' with ", nrow(flagged_tbl), " flagged items.")
      keys_filt <- drop_flagged_items_from_keys(keys, flagged_tbl)
      
      df_scored_f <- df
      df_scored_f <- add_scale_scores(
        df_scored_f, keys_filt, scoring_df,
        prefix = "score_",
        exclude_scales = NON_SCORABLE_SCALES,
        force_mean_scores = CFG$force_filtered_scores_to_mean,
        suq_total_agg = CFG$suq_filtered_total_agg,
        ctq_md_definition = ctq_md_definition
      )
      df_scored_f <- add_subscale_scores(
        df_scored_f, keys_filt, scoring_df,
        prefix = "score_",
        exclude_scales = NON_SCORABLE_SCALES,
        force_mean_scores = CFG$force_filtered_scores_to_mean
      )
      df_scored_f <- add_ctq_md_score(df_scored_f, ctq_md_definition)
      df_scored_f <- add_suq_illegal_drugs_score(
        df_scored_f,
        keys_filt,
        prefix = "score_",
        agg = CFG$suq_filtered_total_agg
      )
      
      suffix_tag <- make_threshold_tag(CFG$loading_threshold)
      assert_no_missing_scores(df_scored_f, sample, suffix = suffix_tag)
      
      if (isTRUE(CFG$add_z_scores_to_scored_masters)) {
        df_scored_f <- add_z_score_columns(
          df_scored_f,
          dataset_label = paste0(sample, " ", suffix_tag)
        )
      }
      
      write_master_variant(
        df_scored_f,
        sample,
        input_date = input_date,
        suffix = suffix_tag
      )
    } else {
      log_msg("No flagged-item helper found. Skipping filtered scored master.")
    }
  }
  # round 3: combined-filtered scores for Step 4 analysis-input exports
  # These use the pooled/combined loading flags, but are written sample-wise,
  # so Step 4 can combine adults + adolescents without recomputing scores.
  if (isTRUE(CFG$export_filtered_scores) && isTRUE(CFG$export_combined_filtered_scores)) {
    if (!is.na(flag_helper_path) && fs::file_exists(flag_helper_path)) {
      flagged_tbl_combined <- read_flagged_items(
        path = flag_helper_path,
        dataset_label = CFG$combined_label,
        threshold_value = CFG$loading_threshold
      )
      
      log_msg(
        "Applying combined-filtered scoring for sample '", sample,
        "' using dataset label '", CFG$combined_label,
        "' with ", nrow(flagged_tbl_combined), " flagged items."
      )
      
      keys_combined_filt <- drop_flagged_items_from_keys(keys, flagged_tbl_combined)
      
      df_scored_combined_f <- df
      df_scored_combined_f <- add_scale_scores(
        df_scored_combined_f, keys_combined_filt, scoring_df,
        prefix = "score_",
        exclude_scales = NON_SCORABLE_SCALES,
        force_mean_scores = CFG$force_filtered_scores_to_mean,
        suq_total_agg = CFG$suq_filtered_total_agg,
        ctq_md_definition = ctq_md_definition
      )
      df_scored_combined_f <- add_subscale_scores(
        df_scored_combined_f, keys_combined_filt, scoring_df,
        prefix = "score_",
        exclude_scales = NON_SCORABLE_SCALES,
        force_mean_scores = CFG$force_filtered_scores_to_mean
      )
      df_scored_combined_f <- add_ctq_md_score(df_scored_combined_f, ctq_md_definition)
      df_scored_combined_f <- add_suq_illegal_drugs_score(
        df_scored_combined_f,
        keys_combined_filt,
        prefix = "score_",
        agg = CFG$suq_filtered_total_agg
      )
      
      suffix_tag_combined <- paste0(make_threshold_tag(CFG$loading_threshold), "_combined")
      
      assert_no_missing_scores(df_scored_combined_f, sample, suffix = suffix_tag_combined)
      
      if (isTRUE(CFG$add_z_scores_to_scored_masters)) {
        df_scored_combined_f <- add_z_score_columns(
          df_scored_combined_f,
          dataset_label = paste0(sample, " ", suffix_tag_combined)
        )
      }
      
      write_master_variant(
        df_scored_combined_f,
        sample,
        input_date = input_date,
        suffix = suffix_tag_combined
      )
    } else {
      log_msg("No flagged-item helper found. Skipping combined-filtered scored master.")
    }
  }
  log_msg("--- Done scoring sample: ", sample, " ---\n")
}

# ---- Main --------------------------------------------------------------------
SCORING_PATH <- latest_scoring()
if (is.na(SCORING_PATH)) {
  stop("No scoring file found in information/.")
}
SCORING <- read_scoring(SCORING_PATH)

threshold_tag <- make_threshold_tag(CFG$loading_threshold)
FLAG_HELPER <- latest_flag_helper(CFG$combined_label, threshold_tag)

if (!is.na(FLAG_HELPER)) {
  log_msg("Using flagged-item helper: ", FLAG_HELPER)
} else {
  log_msg("No flagged-item helper found. Filtered scored masters may be skipped.")
}

purrr::walk(CFG$samples, ~ process_sample(.x, scoring_df = SCORING, flag_helper_path = FLAG_HELPER))
