# --- prep04_internal_consistency_check.R -----------------------------------------------
# INTERNAL CONSISTENCY + LOADINGS:
# - Computes McDonald's omega (ωt) for all scales + subscales
# - Runs for adults, adolescents, and optionally a combined dataset
# - Exports one XLSX with separate sheets per dataset:
#     * omega_<dataset>
#     * load_<dataset>
# - Optionally performs a second omega round after dropping items with
#   factor loadings < threshold from round 1
# - Exports a helper XLSX listing all flagged items
# - Logs skipped participants (too many missing items) to ROOT/logs/<DATE>_data_analysis_log.txt
#
# NOTE:
# - Loadings are 1-factor FA loadings (minres), computed overall per dataset
# - Filtered rerun affects omega only (not the original loadings sheets)
# - Separate datasets keep separate filtering logic
# - Combined dataset uses combined filtering logic
# - Includes fixed, frequency-only CAPE positive factors from Schlier et al.
#   (2015), Table 2. Rerun updated Step 03 first to regenerate the keys.
# - The two-item CAPE factors use inter-item r + Spearman-Brown. Omega and
#   one-factor loadings are not estimated for these underidentified factors.
# - Literature-defined CAPE factors keep all their items in the filtered round.
# ------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE)); invisible(gc())
try(cat("\014"), silent = TRUE)

# ===== Setup =====
ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "jsonlite","tibble","dplyr","tidyr","purrr","stringr",
  "fs","glue","janitor","psych","rprojroot","readxl","openxlsx"
))

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ===== CONFIG =====
CFG <- list(
  samples = c("adults", "adolescents"),
  export_combined = TRUE,
  combined_label = "adults_adolescents",
  
  omega_timeout_sec = 20,
  
  export_loading_filtered = TRUE,
  loading_threshold = 0.30,
  
  # Speed / relevance controls
  exclude_scales = c(
    "FHSfamilytree", "health", "demographics", "times",
    "date", "id", "project"
  ),
  skip_na_subscales = TRUE,
  compute_group_omega = FALSE,
  compute_project_omega = FALSE,
  verbose_item_matching = TRUE
)

# ===== Path helpers =====
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
  sd <- script_dir()
  if (dir.exists(sd)) return(sd)
  root <- tryCatch(
    rprojroot::find_root(rprojroot::is_rstudio_project | rprojroot::is_git_root),
    error = function(e) NA
  )
  if (!is.na(root)) return(normalizePath(root, winslash = "/"))
  normalizePath(getwd(), winslash = "/")
}

ROOT <- project_root()
DIR_INFO           <- fs::path(ROOT, "information")
DIR_QUESTIONNAIRES <- fs::path(
  ROOT, "01_project_data", "all_projects_backbone", "raw_data", "questionnaires"
)
DIR_EXPORT         <- fs::path(ROOT, "02_cleaned")
DIR_KEYS           <- fs::path(DIR_EXPORT, "keys")
DIR_FUNCTIONS      <- fs::path(ROOT, "functions")
DIR_LOGS           <- fs::path(ROOT, "logs")

# ===== Source helpers =====
source(file.path(DIR_FUNCTIONS, "normalize_id.R"))
source(file.path(DIR_FUNCTIONS, "get_project_col.R"))
source(file.path(DIR_FUNCTIONS, "extract_date_string.R"))
source(file.path(DIR_FUNCTIONS, "latest_questionnaire_for_sample.R"))
source(file.path(DIR_FUNCTIONS, "setup_logging.R"))

# ===== General helpers =====
sanitize_sheet_name <- function(x) {
  x <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", x)
  substr(x, 1, 31)
}

make_threshold_tag <- function(x) {
  paste0("lt", gsub("\\.", "", formatC(x, format = "f", digits = 2)))
}

read_master_xlsx <- function(master_xlsx) {
  suppressMessages(readxl::read_excel(master_xlsx)) %>%
    janitor::clean_names()
}

normalize_group_col <- function(df) {
  if ("group" %in% names(df)) {
    df$group <- as.character(df$group)
    df$group[is.na(df$group) | !nzchar(df$group)] <- "HC"
    df$group <- factor(df$group, levels = c("HC", "patient"))
  }
  df
}

get_id_vec <- function(z) {
  if ("vpid" %in% names(z)) z$vpid
  else if ("vp" %in% names(z)) z$vp
  else if ("participantid" %in% names(z)) z$participantid
  else if ("participant_id" %in% names(z)) z$participant_id
  else if ("id" %in% names(z)) z$id
  else rep(NA_character_, nrow(z))
}

is_cape_positive_schlier2015_subscale <- function(scale, subscale) {
  !is.na(scale) & toupper(trimws(as.character(scale))) == "CAPE" &
    !is.na(subscale) & startsWith(tolower(trimws(as.character(subscale))),
                                 "positive_schlier2015_")
}

flatten_keys <- function(keys_json) {
  keys_raw <- jsonlite::read_json(keys_json, simplifyVector = FALSE)
  
  items_by_scale <- tibble::tibble(
    scale = purrr::map_chr(keys_raw$items_by_scale, ~ .x$scale %||% NA_character_),
    items = purrr::map(keys_raw$items_by_scale, ~ as.character(.x$items %||% character(0)))
  )
  
  items_by_subscale <- tibble::tibble(
    scale    = purrr::map_chr(keys_raw$items_by_subscale, ~ .x$scale %||% NA_character_),
    subscale = purrr::map_chr(keys_raw$items_by_subscale, ~ .x$subscale %||% NA_character_),
    items    = purrr::map(keys_raw$items_by_subscale, ~ as.character(.x$items %||% character(0)))
  )

  if (any(toupper(trimws(items_by_scale$scale)) == "CAPE", na.rm = TRUE) &&
      (is.null(keys_raw$cape_positive_schlier2015) ||
       sum(is_cape_positive_schlier2015_subscale(
         items_by_subscale$scale, items_by_subscale$subscale
       )) != 5L)) {
    stop("CAPE Schlier2015 keys are missing or incomplete. Rerun the updated Step 03 before Step 04.")
  }
  
  .as_chr0 <- function(x) {
    if (is.null(x)) return(character(0))
    if (length(x) == 1 && is.na(x)) return(character(0))
    as.character(x)
  }
  
  scale_from_subscales <- items_by_subscale %>%
    dplyr::filter(!is.na(.data$scale)) %>%
    dplyr::group_by(.data$scale) %>%
    dplyr::summarise(
      items_sub = list(unique(unlist(.data$items, use.names = FALSE))),
      .groups = "drop"
    )
  
  items_by_scale_all <- items_by_scale %>%
    dplyr::rename(items_scale = .data$items) %>%
    dplyr::full_join(scale_from_subscales, by = "scale") %>%
    dplyr::mutate(
      items = purrr::map2(.data$items_scale, .data$items_sub, ~ {
        a <- .as_chr0(.x)
        b <- .as_chr0(.y)
        if (length(a)) a else b
      }),
      total_source = dplyr::case_when(
        purrr::map_int(.data$items_scale, ~ length(.as_chr0(.x))) > 0 ~ "keys_scale",
        TRUE ~ "union_subscales"
      )
    ) %>%
    dplyr::select(.data$scale, .data$items, .data$total_source) %>%
    dplyr::arrange(.data$scale)
  
  list(
    scale_tbl = items_by_scale_all,
    sub_tbl   = items_by_subscale
  )
}

merge_scale_tables <- function(bundles) {
  dplyr::bind_rows(purrr::map(bundles, "scale_tbl")) %>%
    dplyr::group_by(.data$scale) %>%
    dplyr::summarise(
      items = list(unique(unlist(.data$items, use.names = FALSE))),
      total_source = paste(unique(.data$total_source), collapse = "|"),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$scale)
}

merge_subscale_tables <- function(bundles) {
  dplyr::bind_rows(purrr::map(bundles, "sub_tbl")) %>%
    dplyr::group_by(.data$scale, .data$subscale) %>%
    dplyr::summarise(
      items = list(unique(unlist(.data$items, use.names = FALSE))),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$scale, .data$subscale)
}

make_col_map <- function(df) {
  tibble::tibble(orig = names(df), item_norm = normalize_id(names(df))) %>%
    dplyr::distinct(.data$item_norm, .keep_all = TRUE)
}

is_excluded_scale <- function(x) {
  toupper(trimws(as.character(x))) %in% toupper(trimws(CFG$exclude_scales))
}

is_blank_or_na_subscale <- function(x) {
  x_chr <- trimws(as.character(x))
  is.na(x) | x_chr == "" | toupper(x_chr) == "NA"
}

filter_key_tables <- function(key_tbls) {
  key_tbls$scale_tbl <- key_tbls$scale_tbl %>%
    dplyr::filter(!is_excluded_scale(.data$scale))
  
  key_tbls$sub_tbl <- key_tbls$sub_tbl %>%
    dplyr::filter(!is_excluded_scale(.data$scale))
  
  if (isTRUE(CFG$skip_na_subscales)) {
    key_tbls$sub_tbl <- key_tbls$sub_tbl %>%
      dplyr::filter(!is_blank_or_na_subscale(.data$subscale))
  }
  
  key_tbls
}

read_sample_bundle <- function(sample) {
  q_file <- latest_questionnaire_for_sample(sample, DIR_QUESTIONNAIRES)
  if (length(q_file) != 1L || is.na(q_file) || !fs::file_exists(q_file)) {
    stop(
      "No dated questionnaire input found for sample '", sample,
      "' in '", DIR_QUESTIONNAIRES, "'."
    )
  }

  input_date <- extract_date_string(q_file)
  master_xlsx <- fs::path(
    DIR_EXPORT,
    sample,
    glue::glue("{input_date}_{sample}_clean_master.xlsx")
  )
  keys_json <- fs::path(
    DIR_KEYS,
    glue::glue("{input_date}_{sample}_keys.json")
  )

  required_files <- c(master_xlsx, keys_json)
  missing_files <- required_files[!fs::file_exists(required_files)]
  if (length(missing_files)) {
    stop(
      "Step 03 outputs matching questionnaire date ", input_date,
      " are missing for sample '", sample, "': ",
      paste(missing_files, collapse = ", ")
    )
  }
  
  df <- read_master_xlsx(master_xlsx) %>%
    normalize_group_col()
  
  key_tbls <- flatten_keys(keys_json)
  key_tbls <- filter_key_tables(key_tbls)
  
  list(
    sample    = sample,
    df        = df,
    col_map   = make_col_map(df),
    proj_col  = get_project_col(df),
    scale_tbl = key_tbls$scale_tbl,
    sub_tbl   = key_tbls$sub_tbl,
    q_file    = q_file,
    master_xlsx = master_xlsx,
    keys_json  = keys_json
  )
}

make_dataset_defs <- function(sample_bundles) {
  out <- purrr::map(sample_bundles, function(b) {
    list(
      label     = b$sample,
      df        = b$df,
      col_map   = b$col_map,
      proj_col  = b$proj_col,
      scale_tbl = b$scale_tbl,
      sub_tbl   = b$sub_tbl
    )
  })
  
  if (isTRUE(CFG$export_combined)) {
    df_combined <- purrr::imap_dfr(sample_bundles, function(b, nm) {
      b$df %>% dplyr::mutate(sample_source = b$sample, .before = 1)
    }) %>%
      normalize_group_col()
    
    out[[CFG$combined_label]] <- list(
      label     = CFG$combined_label,
      df        = df_combined,
      col_map   = make_col_map(df_combined),
      proj_col  = get_project_col(df_combined),
      scale_tbl = merge_scale_tables(sample_bundles),
      sub_tbl   = merge_subscale_tables(sample_bundles)
    )
  }
  
  out
}

# ===== Matching / filtering helpers =====
drop_flagged_items <- function(items, flagged_tbl = NULL,
                               level = NULL, scale = NULL, subscale = NULL) {
  if (identical(level, "subscale") &&
      isTRUE(is_cape_positive_schlier2015_subscale(scale, subscale))) return(items)
  if (is.null(flagged_tbl) || !nrow(flagged_tbl)) return(items)
  
  flagged_here <- flagged_tbl %>%
    dplyr::filter(
      .data$level == !!level,
      .data$scale == !!scale,
      (is.na(.data$subscale) & is.na(!!subscale)) | (.data$subscale == !!subscale)
    )
  
  if (!nrow(flagged_here)) return(items)
  
  drop_norm <- unique(normalize_id(flagged_here$item))
  keep_idx  <- !(normalize_id(items) %in% drop_norm)
  items[keep_idx]
}

match_items <- function(col_map, items) {
  items_norm <- normalize_id(items)
  hit <- intersect(items_norm, col_map$item_norm)
  list(
    n_in_key = length(items_norm),
    n_found  = length(hit),
    found    = hit
  )
}

# ===== Omega =====
compute_omega_safe <- function(items_df, timeout_sec = CFG$omega_timeout_sec,
                               min_items = 2L) {
  items_df[] <- lapply(items_df, function(z) suppressWarnings(as.numeric(z)))
  
  keep <- vapply(items_df, function(z) {
    z <- z[is.finite(z)]
    length(z) > 0 && length(unique(z)) > 1
  }, logical(1))
  items_df <- items_df[, keep, drop = FALSE]
  
  if (ncol(items_df) < min_items || nrow(items_df) < 10L) return(NA_real_)
  
  setTimeLimit(elapsed = timeout_sec, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
  
  out <- tryCatch({
    fa_out <- suppressWarnings(suppressMessages(
      psych::fa(items_df, nfactors = 1, rotate = "none", fm = "minres", warnings = FALSE)
    ))
    
    loadings <- as.numeric(fa_out$loadings[, 1])
    uniqueness <- as.numeric(fa_out$uniquenesses)
    
    if (!length(loadings) || !length(uniqueness)) return(NA_real_)
    if (any(!is.finite(loadings)) || any(!is.finite(uniqueness))) return(NA_real_)
    
    numerator <- sum(loadings)^2
    denominator <- numerator + sum(uniqueness)
    
    if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
    
    numerator / denominator
  }, error = function(e) NA_real_)
  
  as.numeric(out)
}

omega_for <- function(d, col_map, items, logger,
                      scale_label = NULL, sub_label = NULL) {
  fixed_cape_factor <- isTRUE(is_cape_positive_schlier2015_subscale(scale_label, sub_label))
  if (fixed_cape_factor && length(items) == 2L) return(NA_real_)
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (length(keep_norm) < 2L) return(NA_real_)
  
  orig_cols <- col_map$orig[match(keep_norm, col_map$item_norm)]
  X <- d[, orig_cols, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  
  has_any_item <- rowSums(!is.na(as.matrix(X))) > 0
  X <- X[has_any_item, , drop = FALSE]
  
  if (nrow(X) < 10L) return(NA_real_)
  compute_omega_safe(X, min_items = if (fixed_cape_factor) 3L else 2L)
}

# A two-indicator unrestricted one-factor model is underidentified. Report a
# transparent two-item reliability summary instead of fitting that model.
# This is an additional diagnostic; the paper does not prescribe this index.
cape_two_item_reliability <- function(d, col_map, items) {
  empty <- list(r = NA_real_, spearman_brown = NA_real_, n_used = 0L)
  cols <- col_map$orig[match(normalize_id(items), col_map$item_norm)]
  if (length(cols) != 2L || anyNA(cols)) return(empty)
  X <- d[, cols, drop = FALSE]
  X[] <- lapply(X, function(x) suppressWarnings(as.numeric(x)))
  X <- X[is.finite(X[[1]]) & is.finite(X[[2]]), , drop = FALSE]
  empty$n_used <- nrow(X)
  if (nrow(X) < 10L || stats::sd(X[[1]]) <= 0 || stats::sd(X[[2]]) <= 0) return(empty)
  r <- stats::cor(X[[1]], X[[2]])
  list(
    r = r,
    spearman_brown = if (is.finite(r) && 1 + r > sqrt(.Machine$double.eps)) {
      2 * r / (1 + r)
    } else NA_real_,
    n_used = nrow(X)
  )
}

cape_two_item_summary <- function(d, col_map, items) {
  all <- cape_two_item_reliability(d, col_map, items)
  hc <- patient <- list(r = NA_real_, spearman_brown = NA_real_, n_used = NA_integer_)
  if (isTRUE(CFG$compute_group_omega) && "group" %in% names(d)) {
    hc <- cape_two_item_reliability(dplyr::filter(d, .data$group %in% "HC"), col_map, items)
    patient <- cape_two_item_reliability(dplyr::filter(d, .data$group %in% "patient"), col_map, items)
  }
  tibble::tibble(
    interitem_r_all = all$r,
    spearman_brown_all = all$spearman_brown,
    n_used_two_items_all = all$n_used,
    interitem_r_HC = hc$r,
    spearman_brown_HC = hc$spearman_brown,
    interitem_r_patient = patient$r,
    spearman_brown_patient = patient$spearman_brown,
    reliability_note = "Two-item factor: inter-item r and Spearman-Brown; omega not estimated"
  )
}

# ===== Loadings =====
compute_loadings_safe <- function(items_df, timeout_sec = CFG$omega_timeout_sec,
                                 min_items = 2L) {
  items_df[] <- lapply(items_df, function(z) suppressWarnings(as.numeric(z)))
  
  keep <- vapply(items_df, function(z) {
    z <- z[is.finite(z)]
    length(z) > 0 && length(unique(z)) > 1
  }, logical(1))
  items_df <- items_df[, keep, drop = FALSE]
  
  if (ncol(items_df) < min_items || nrow(items_df) < 20L) return(NULL)
  
  setTimeLimit(elapsed = timeout_sec, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
  
  fa_out <- tryCatch(
    suppressWarnings(suppressMessages(psych::fa(items_df, nfactors = 1, rotate = "none", fm = "minres"))),
    error = function(e) NULL
  )
  if (is.null(fa_out)) return(NULL)
  
  L <- as.matrix(fa_out$loadings)
  if (!ncol(L)) return(NULL)
  
  tibble::tibble(
    item = rownames(L),
    loading = as.numeric(L[, 1]),
    communality = if (!is.null(fa_out$communality)) as.numeric(fa_out$communality) else NA_real_,
    uniqueness  = if (!is.null(fa_out$uniquenesses)) as.numeric(fa_out$uniquenesses) else NA_real_,
    n_used = nrow(items_df),
    method = "fa(minres, 1)"
  )
}

loadings_for <- function(d, col_map, items,
                         level = "scale",
                         scale = NA_character_,
                         subscale = NA_character_,
                         total_source = NA_character_) {
  items_norm <- normalize_id(items)
  keep_norm  <- intersect(items_norm, col_map$item_norm)
  if (length(keep_norm) < 2L) return(tibble::tibble())
  
  orig_cols <- col_map$orig[match(keep_norm, col_map$item_norm)]
  X <- d[, orig_cols, drop = FALSE]
  X[] <- lapply(X, function(z) suppressWarnings(as.numeric(z)))
  
  has_any_item <- rowSums(!is.na(as.matrix(X))) > 0
  X <- X[has_any_item, , drop = FALSE]

  fixed_cape_factor <- isTRUE(is_cape_positive_schlier2015_subscale(scale, subscale))
  if (fixed_cape_factor && length(items) == 2L) {
    return(tibble::tibble(
      level = level, scale = scale, subscale = subscale,
      total_source = total_source, item = orig_cols,
      loading = NA_real_, communality = NA_real_, uniqueness = NA_real_,
      n_used = sum(is.finite(X[[1]]) & is.finite(X[[2]])),
      method = "not estimated: two-item factor (underidentified)"
    ))
  }
  L <- compute_loadings_safe(X, min_items = if (fixed_cape_factor) 3L else 2L)
  if (is.null(L) || !nrow(L)) return(tibble::tibble())
  
  dplyr::mutate(
    L,
    level = level,
    scale = scale,
    subscale = subscale,
    total_source = total_source,
    .before = 1
  )
}

# ===== Summary builders =====
omega_block <- function(dset, items, level, scale, subscale = NA_character_,
                        total_source = NA_character_, logger) {
  df       <- dset$df
  col_map  <- dset$col_map
  proj_col <- dset$proj_col
  
  name <- if (identical(level, "scale")) scale else paste(scale, subscale, sep = " / ")
  
  overall_all <- omega_for(df, col_map, items, logger, scale_label = scale, sub_label = if (level == "subscale") subscale else NULL)
  
  overall_hc <- overall_patient <- NA_real_
  if (isTRUE(CFG$compute_group_omega) &&
      "group" %in% names(df) &&
      any(df$group %in% c("HC", "patient"), na.rm = TRUE)) {
    overall_hc <- omega_for(
      dplyr::filter(df, .data$group %in% "HC"),
      col_map, items, logger,
      scale_label = scale,
      sub_label   = if (level == "subscale") paste0(subscale, " / HC") else "HC"
    )
    overall_patient <- omega_for(
      dplyr::filter(df, .data$group %in% "patient"),
      col_map, items, logger,
      scale_label = scale,
      sub_label   = if (level == "subscale") paste0(subscale, " / patient") else "patient"
    )
  }
  
  out <- tibble::tibble(
    level = level,
    scale = scale,
    subscale = subscale,
    name = name,
    total_source = total_source,
    project = "OVERALL",
    omega_all = as.numeric(overall_all),
    omega_HC = as.numeric(overall_hc),
    omega_patient = as.numeric(overall_patient)
  )
  
  if (isTRUE(CFG$compute_project_omega) && !is.null(proj_col)) {
    proj_vals <- unique(df[[proj_col]])
    proj_vals <- proj_vals[!is.na(proj_vals)]
    
    proj_rows <- purrr::map_dfr(proj_vals, function(pv) {
      d_proj <- df[df[[proj_col]] == pv, , drop = FALSE]
      
      om_all <- omega_for(d_proj, col_map, items, logger,
                          scale_label = scale,
                          sub_label = if (level == "subscale") subscale else NULL)
      
      om_hc <- om_pat <- NA_real_
      if (isTRUE(CFG$compute_group_omega) &&
          "group" %in% names(d_proj) &&
          any(d_proj$group %in% c("HC", "patient"), na.rm = TRUE)) {
        d_hc  <- dplyr::filter(d_proj, .data$group %in% "HC")
        d_pat <- dplyr::filter(d_proj, .data$group %in% "patient")
        
        if (nrow(d_hc)) {
          om_hc <- omega_for(d_hc, col_map, items, logger,
                             scale_label = scale,
                             sub_label = if (level == "subscale") paste0(subscale, " / HC") else "HC")
        }
        if (nrow(d_pat)) {
          om_pat <- omega_for(d_pat, col_map, items, logger,
                              scale_label = scale,
                              sub_label = if (level == "subscale") paste0(subscale, " / patient") else "patient")
        }
      }
      
      tibble::tibble(
        level = level,
        scale = scale,
        subscale = subscale,
        name = name,
        total_source = total_source,
        project = as.character(pv),
        omega_all = as.numeric(om_all),
        omega_HC = as.numeric(om_hc),
        omega_patient = as.numeric(om_pat)
      )
    })
    
    out <- dplyr::bind_rows(out, proj_rows)
  }

  if (isTRUE(is_cape_positive_schlier2015_subscale(scale, subscale)) &&
      length(items) == 2L) {
    extra <- purrr::map_dfr(out$project, function(pv) {
      d_block <- if (identical(pv, "OVERALL")) df else {
        df[!is.na(df[[proj_col]]) & as.character(df[[proj_col]]) == pv, , drop = FALSE]
      }
      cape_two_item_summary(d_block, col_map, items)
    })
    out <- dplyr::bind_cols(out, extra)
  }
  
  out
}

build_omega_rows <- function(dset, logger, flagged_tbl = NULL) {
  scale_rows <- purrr::pmap_dfr(
    dset$scale_tbl %>% dplyr::select(scale, items, total_source),
    function(scale, items, total_source) {
      items_use <- drop_flagged_items(
        items = unlist(items, use.names = FALSE),
        flagged_tbl = flagged_tbl,
        level = "scale",
        scale = scale,
        subscale = NA_character_
      )
      
      mchk <- match_items(dset$col_map, items_use)
      if (isTRUE(CFG$verbose_item_matching)) {
        message(glue::glue("[{dset$label}] SCALE {scale}: {mchk$n_found}/{length(items_use)} items found after filtering"))
      }
      if (mchk$n_found < 2) {
        return(tibble::tibble())
      }
      
      omega_block(
        dset = dset,
        items = items_use,
        level = "scale",
        scale = scale,
        subscale = NA_character_,
        total_source = total_source,
        logger = logger
      )
    }
  )
  
  sub_rows <- purrr::pmap_dfr(
    dset$sub_tbl %>% dplyr::select(scale, subscale, items),
    function(scale, subscale, items) {
      items_use <- drop_flagged_items(
        items = unlist(items, use.names = FALSE),
        flagged_tbl = flagged_tbl,
        level = "subscale",
        scale = scale,
        subscale = subscale
      )
      
      mchk <- match_items(dset$col_map, items_use)
      if (isTRUE(CFG$verbose_item_matching)) {
        message(glue::glue("[{dset$label}] SUBSCALE {scale} / {subscale}: {mchk$n_found}/{length(items_use)} items found after filtering"))
      }
      if (mchk$n_found < 2) {
        return(tibble::tibble())
      }
      
      omega_block(
        dset = dset,
        items = items_use,
        level = "subscale",
        scale = scale,
        subscale = subscale,
        total_source = "subscale",
        logger = logger
      )
    }
  )
  
  dplyr::bind_rows(scale_rows, sub_rows)
}

build_loadings_rows <- function(dset) {
  scale_rows <- purrr::pmap_dfr(
    dset$scale_tbl %>% dplyr::select(scale, items, total_source),
    function(scale, items, total_source) {
      loadings_for(
        d = dset$df,
        col_map = dset$col_map,
        items = unlist(items, use.names = FALSE),
        level = "scale",
        scale = scale,
        subscale = NA_character_,
        total_source = total_source
      )
    }
  )
  
  sub_rows <- purrr::pmap_dfr(
    dset$sub_tbl %>% dplyr::select(scale, subscale, items),
    function(scale, subscale, items) {
      loadings_for(
        d = dset$df,
        col_map = dset$col_map,
        items = unlist(items, use.names = FALSE),
        level = "subscale",
        scale = scale,
        subscale = subscale,
        total_source = "subscale"
      )
    }
  )
  
  dplyr::bind_rows(scale_rows, sub_rows)
}

# ===== Main =====
sample_bundles <- purrr::map(CFG$samples, read_sample_bundle)
names(sample_bundles) <- CFG$samples

all_dates <- purrr::map_chr(sample_bundles, ~ {
  qf <- .x$q_file
  if (length(qf) == 1 && !is.na(qf)) extract_date_string(qf) else NA_character_
})
all_dates_as_date <- suppressWarnings(as.Date(all_dates))
date_str <- if (all(is.na(all_dates_as_date))) format(Sys.Date(), "%Y-%m-%d") else format(max(all_dates_as_date, na.rm = TRUE), "%Y-%m-%d")

OUT_BASE <- fs::path(
  ROOT, "out", "internal_data_analysis",
  "distribution_of_backbone_scores_and_internal_consistency",
  paste0(date_str, "_", CFG$combined_label)
)
fs::dir_create(OUT_BASE)
fs::dir_create(DIR_LOGS)

log_path <- fs::path(DIR_LOGS, glue::glue("{date_str}_data_analysis_log.txt"))
logger   <- setup_logging(log_path, append = TRUE, with_timestamp = TRUE, enforce_code = FALSE)

dataset_defs <- make_dataset_defs(sample_bundles)

omega_round1 <- list()
loadings_all <- list()

message("== ROUND 1: omega + loadings ==")
for (nm in names(dataset_defs)) {
  dset <- dataset_defs[[nm]]
  message(glue::glue("---- Dataset: {nm} ----"))
  omega_round1[[nm]] <- build_omega_rows(dset, logger = logger)
  loadings_all[[nm]] <- build_loadings_rows(dset) %>%
    dplyr::mutate(dataset = nm, .before = 1)
}

loadings_combined <- dplyr::bind_rows(loadings_all)

threshold_tag <- make_threshold_tag(CFG$loading_threshold)
flagged_items <- tibble::tibble()

omega_round2 <- list()
if (isTRUE(CFG$export_loading_filtered)) {
  get_suq_item_num <- function(x) {
    suppressWarnings(as.integer(stringr::str_extract(normalize_id(x), "[123]$")))
  }
  
  get_suq_pair_key <- function(x) {
    stringr::str_remove(normalize_id(x), "[123]$")
  }
  
  make_flagged_items <- function(loadings_tbl, threshold) {
    x <- loadings_tbl %>%
      dplyr::mutate(
        scale_norm = toupper(trimws(as.character(.data$scale))),
        subscale_join = dplyr::if_else(
          is.na(.data$subscale),
          "__NA__",
          as.character(.data$subscale)
        ),
        suq_item_num = get_suq_item_num(.data$item),
        suq_pair_key = get_suq_pair_key(.data$item),
        abs_loading = abs(.data$loading)
      )
    
    non_suq_flagged <- x %>%
      dplyr::filter(
        .data$scale_norm != "SUQ",
        !is_cape_positive_schlier2015_subscale(.data$scale, .data$subscale),
        !is.na(.data$loading),
        .data$abs_loading < threshold
      )
    
    suq_low_pairs <- x %>%
      dplyr::filter(
        .data$scale_norm == "SUQ",
        .data$suq_item_num %in% c(2L, 3L),
        !is.na(.data$loading),
        .data$abs_loading < threshold
      ) %>%
      dplyr::distinct(
        .data$dataset,
        .data$level,
        .data$scale,
        .data$subscale_join,
        .data$suq_pair_key
      )
    
    suq_paired_flagged <- x %>%
      dplyr::filter(
        .data$scale_norm == "SUQ",
        .data$suq_item_num %in% c(2L, 3L)
      ) %>%
      dplyr::semi_join(
        suq_low_pairs,
        by = c("dataset", "level", "scale", "subscale_join", "suq_pair_key")
      )
    
    dplyr::bind_rows(non_suq_flagged, suq_paired_flagged) %>%
      dplyr::mutate(
        loading_threshold = threshold,
        flagged_for_removal = TRUE
      ) %>%
      dplyr::distinct(.data$dataset, .data$level, .data$scale, .data$subscale, .data$item, .keep_all = TRUE) %>%
      dplyr::select(
        -dplyr::any_of(c("scale_norm", "subscale_join", "suq_item_num", "suq_pair_key"))
      ) %>%
      dplyr::arrange(.data$dataset, .data$level, .data$scale, .data$subscale, .data$item)
  }
  
  flagged_items <- make_flagged_items(loadings_combined, CFG$loading_threshold)
  
  message(glue::glue("== ROUND 2: omega after dropping items with loading < {CFG$loading_threshold} =="))
  for (nm in names(dataset_defs)) {
    if (identical(nm, CFG$combined_label)) {
      dset_flags <- flagged_items %>%
        dplyr::filter(.data$dataset == CFG$combined_label)
      message(glue::glue("---- Filtered dataset: {nm} | using pooled flags ({nrow(dset_flags)} items) ----"))
    } else {
      dset_flags <- flagged_items %>%
        dplyr::filter(.data$dataset == nm)
      message(glue::glue("---- Filtered dataset: {nm} | using sample-specific flags ({nrow(dset_flags)} items) ----"))
    }
    
    omega_round2[[nm]] <- build_omega_rows(
      dataset_defs[[nm]],
      logger = logger,
      flagged_tbl = dset_flags
    )
  }
}

# ===== Export =====
xlsx_path <- fs::path(OUT_BASE, glue::glue("{CFG$combined_label}_omega_and_loadings.xlsx"))

wb <- openxlsx::createWorkbook()

for (nm in names(dataset_defs)) {
  sh_omega <- sanitize_sheet_name(paste0("omega_", nm))
  sh_load  <- sanitize_sheet_name(paste0("load_", nm))
  
  openxlsx::addWorksheet(wb, sh_omega)
  openxlsx::writeDataTable(wb, sh_omega, omega_round1[[nm]], withFilter = TRUE)
  
  openxlsx::addWorksheet(wb, sh_load)
  openxlsx::writeDataTable(wb, sh_load, loadings_all[[nm]], withFilter = TRUE)
  
  if (isTRUE(CFG$export_loading_filtered)) {
    sh_omega_f <- sanitize_sheet_name(paste0("omega_", nm, "_", threshold_tag))
    openxlsx::addWorksheet(wb, sh_omega_f)
    openxlsx::writeDataTable(wb, sh_omega_f, omega_round2[[nm]], withFilter = TRUE)
  }
}

openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)

helper_path <- NULL
if (isTRUE(CFG$export_loading_filtered)) {
  helper_path <- fs::path(
    OUT_BASE,
    glue::glue("{CFG$combined_label}_flagged_items_{threshold_tag}.xlsx")
  )
  
  helper_wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(helper_wb, "flagged_items")
  openxlsx::writeDataTable(helper_wb, "flagged_items", flagged_items, withFilter = TRUE)
  
  openxlsx::addWorksheet(helper_wb, "all_loadings")
  openxlsx::writeDataTable(helper_wb, "all_loadings", loadings_combined, withFilter = TRUE)
  
  summary_tbl <- flagged_items %>%
    dplyr::count(.data$dataset, .data$level, .data$scale, .data$subscale, name = "n_flagged_items") %>%
    dplyr::arrange(.data$dataset, .data$level, .data$scale, .data$subscale)
  
  openxlsx::addWorksheet(helper_wb, "summary")
  openxlsx::writeDataTable(helper_wb, "summary", summary_tbl, withFilter = TRUE)
  
  openxlsx::saveWorkbook(helper_wb, helper_path, overwrite = TRUE)
}

message("== Summary ==")
message("Omega + loadings workbook: ", xlsx_path)
if (!is.null(helper_path)) message("Flagged-items helper:      ", helper_path)
message("Log file:                  ", log_path)

try(logger$close(), silent = TRUE)
