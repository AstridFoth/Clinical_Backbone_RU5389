#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Monitoring 3 — Timeline of Data Collection (with projections + ideal state)
# Authors: Saskia Wilken (+ modifications)
# Created: 2025-11-10
# Updated: 2026-11-03
#
# What this does
# - Reads the newest *_id_completeness_report.xlsx (by leading YYYY-MM-DD tag) and the Target_Sample_Size_Projects.xlsx
# - Uses the "submitdate" alongside id + sample to build per-project timelines
# - Produces two kinds of timeline plots (absolute & percent as stacked subplots):
#     1) Vanilla layout (one line per project p2..p9)
#     2) Split layout (p6 children/adults and p8 children/adults separated;
#        p6 infants remain hidden in this layout as before)
# - For each layout it saves:
#     A) Empirical timeline only
#     B) Empirical timeline + per-project linear regression with 95% CI (Newey–West) and horizon to 31.12.2027
# - NEW: Ideal state plot (split layout):
#     * For late series only: a dashed "ideal" line that STARTS at the LAST OBSERVED point
#       and reaches target by 31.12.2026, then stays flat out to 31.12.2027
#     * X-axis stays identical to the prediction plot (runs to 31.12.2027)
# - NEW: Weekly rate table + required weekly rate from 01.02.2026 to meet 31.12.2026 (blank if already on time)
# - NEW: All output numbers rounded to 2 decimals (Excel exports)
#
# Notes
# - Each project/group has its own color (legend).
# - X axis spans from fixed origin 01.08.2024 to 31.12.2027 for prediction/ideal plots.
# - p6 adults collect cognitive tests only and are complete per Mon_01.
# - Percent uses the appropriate target. p6 adults require their own "p6 adults"
#   row in the split target sheet; no fallback to the existing p6 target is used.
#   Until that target is supplied, their absolute timeline is shown, while
#   percentages and target-dependent estimates remain NA (also for aggregated p6).
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clean up R environment -------------------------------------------------------
rm(list = ls())
cat("\014")

# ---- Locate ROOT ----
if (requireNamespace("here", quietly = TRUE)) {
  here::i_am("Mon_03_Timeline_of_data_collection.R")
  ROOT <- here::here()
} else if (requireNamespace("rprojroot", quietly = TRUE)) {
  ROOT <- rprojroot::find_root(
    rprojroot::has_file(".Rproj") | rprojroot::has_file(".here") | rprojroot::has_dir(".git")
  )
} else {
  ROOT <- getwd()
}

# ---- Packages ----
auto_install <- FALSE
pkgs <- c("readxl","dplyr","tidyr","stringr","ggplot2","forcats","lubridate",
          "writexl","purrr","scales","colorspace","broom","patchwork","tools",
          "sandwich","lmtest","tibble")
if (auto_install) {
  to_get <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if (length(to_get)) install.packages(to_get)
}
invisible(lapply(pkgs, require, character.only = TRUE))
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
library(patchwork)
if (!requireNamespace("sandwich", quietly = TRUE)) install.packages("sandwich")
library(sandwich)

# ---- Config ----
png_width <- 12; png_height <- 8; png_dpi <- 300
out_dir   <- file.path(ROOT, "out", "timeline")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Ideal-state config ----
ideal_cut_date  <- as.Date("2026-02-01")  # used for Excel rate computations
ideal_goal_date <- as.Date("2026-12-31")  # deadline
ref_date_2027   <- as.Date("2027-12-31")  # keep plots on the same horizon

# ---- Output rounding config ----
round_digits <- 2

# Target table path
target_xlsx <- file.path(ROOT, "information", "2025-11-10_Target_Sample_Size_Projects.xlsx")
if (!file.exists(target_xlsx)) {
  alt <- "/mnt/data/2025-11-10_Target_Sample_Size_Projects.xlsx"
  if (file.exists(alt)) target_xlsx <- alt
}
if (!file.exists(target_xlsx)) stop("Target table not found: ", target_xlsx)

# ---- Targets (Vanilla + Split) ----
read_targets_vanilla <- function(path){
  df <- suppressMessages(readxl::read_excel(path, sheet = "Vanilla"))
  names(df) <- tolower(names(df))
  df %>%
    dplyr::transmute(
      project = suppressWarnings(as.integer(stringr::str_extract(as.character(project), "\\d+"))),
      target  = suppressWarnings(as.numeric(target))
    ) %>%
    dplyr::filter(!is.na(project), !is.na(target), project >= 2, project <= 9)
}
read_targets_split <- function(path){
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  sheet_name <- if ("P8_Split_P6.2_only" %in% sheets) "P8_Split_P6.2_only"
  else if ("Proj_8_Split" %in% sheets) "Proj_8_Split" else "P8_Split_P6.2_only"
  df <- suppressMessages(readxl::read_excel(path, sheet = sheet_name))
  names(df) <- tolower(names(df))
  tibble::tibble(project_raw = as.character(df$project), target = suppressWarnings(as.numeric(df$target))) %>%
    dplyr::mutate(
      prj_lower = tolower(project_raw),
      project   = suppressWarnings(as.integer(stringr::str_extract(prj_lower, "\\d+"))),
      sample    = dplyr::case_when(
        stringr::str_detect(prj_lower, "child") ~ "children",
        stringr::str_detect(prj_lower, "adult") ~ "adults",
        TRUE                                    ~ "ALL"
      )
    ) %>%
    dplyr::filter(!is.na(project), !is.na(target)) %>%
    dplyr::select(project, sample, target)
}
targets_vanilla <- read_targets_vanilla(target_xlsx)
total_targets   <- setNames(targets_vanilla$target, paste0("p", targets_vanilla$project))
targets_split   <- read_targets_split(target_xlsx)

max_target_vanilla <- suppressWarnings(max(as.numeric(total_targets), na.rm = TRUE))
if (!is.finite(max_target_vanilla)) max_target_vanilla <- NA_real_
max_target_split <- {
  tt <- suppressWarnings(as.numeric(targets_split$target))
  proj <- suppressWarnings(as.numeric(total_targets))
  suppressWarnings(max(c(tt, proj), na.rm = TRUE))
}
if (!is.finite(max_target_split)) max_target_split <- max_target_vanilla

# ---- Read newest ID completeness report (ONLY 'complete') ----
priv_dir <- file.path(ROOT, "private_information", "ids_in_all_projects")

get_newest_completeness_report <- function(dir_candidates){
  all_files <- unlist(lapply(dir_candidates, function(d){
    if (!dir.exists(d)) return(character())
    list.files(d, pattern = "_id_completeness_report\\.xlsx$", full.names = TRUE)
  }), use.names = FALSE)
  
  if (!length(all_files)) return(list(path = NA_character_, datestamp = NA_character_))
  
  meta <- tibble::tibble(
    path = all_files,
    base = basename(all_files),
    date_tag = stringr::str_match(basename(all_files), "^(\\d{4}-\\d{2}-\\d{2})")[,2],
    date = suppressWarnings(as.Date(date_tag)),
    mtime = as.POSIXct(file.info(all_files)$mtime)
  )
  
  with_date <- meta %>% dplyr::filter(!is.na(date))
  if (nrow(with_date) > 0) {
    best <- with_date %>%
      dplyr::arrange(dplyr::desc(date), dplyr::desc(mtime)) %>%
      dplyr::slice(1)
    return(list(path = best$path[[1]], datestamp = best$date_tag[[1]]))
  }
  
  best <- meta %>%
    dplyr::arrange(dplyr::desc(mtime)) %>%
    dplyr::slice(1)
  
  ds <- sub("_id_completeness_report\\.xlsx$", "", best$base[[1]])
  return(list(path = best$path[[1]], datestamp = ds))
}

cand_dirs <- c(priv_dir, "/mnt/data")
best_report <- get_newest_completeness_report(cand_dirs)
report_path <- best_report$path
datestamp_last <- best_report$datestamp

if (is.na(report_path) || !file.exists(report_path)) {
  stop("No *_id_completeness_report.xlsx found in expected folders: ",
       paste(cand_dirs, collapse = ", "))
}

read_sheet_timeline <- function(path, sheet){
  df <- suppressMessages(readxl::read_excel(path, sheet = sheet))
  if (!is.data.frame(df) || !nrow(df)) return(tibble::tibble())
  nms <- tolower(names(df)); names(df) <- nms
  
  pid_col <- names(df)[stringr::str_detect(names(df), "^participant\\s*id$")]
  if (!length(pid_col)) {
    pid_col <- names(df)[stringr::str_detect(names(df), "id")]
    if (!length(pid_col)) pid_col <- names(df)[ncol(df)]
  }
  submit_col <- names(df)[stringr::str_detect(names(df), "submit")]
  if (!length(submit_col)) submit_col <- names(df)[stringr::str_detect(names(df), "date")]
  if (!length(submit_col)) return(tibble::tibble())
  
  tibble::tibble(
    project    = suppressWarnings(as.integer(df[["project"]])),
    sample     = tolower(if ("sample" %in% names(df)) df[["sample"]] else ""),
    pid        = df[[pid_col[1]]],
    submitdate = suppressWarnings(lubridate::as_date(df[[submit_col[1]]]))
  ) %>% dplyr::filter(!is.na(project), project >= 2, project <= 9, !is.na(submitdate))
}

sheet_complete <- read_sheet_timeline(report_path, "complete")
if (!nrow(sheet_complete)) stop("No rows in 'complete' sheet with submitdate found.")
# Run Mon_01 first: p6 adults enter complete with their cognitive-test dates;
# questionnaires are not required for this subproject.

timeline_raw <- sheet_complete %>%
  dplyr::mutate(sample = ifelse(is.na(sample) | sample == "", "ALL", sample)) %>%
  dplyr::group_by(project, sample, pid) %>%
  dplyr::summarise(submitdate = min(submitdate, na.rm = TRUE), .groups="drop") %>%
  dplyr::filter(!is.na(submitdate))

# A missing adult target also leaves the combined p6 target unknown.
p6_adults_target_missing <- any(timeline_raw$project == 6 & timeline_raw$sample == "adults") &&
  !any(targets_split$project == 6 & targets_split$sample == "adults" &
         is.finite(targets_split$target) & targets_split$target > 0)

# ---- Colors ----
vanilla_colors <- c(
  "p2"="#1B9E77","p3"="#D95F02","p4"="#7570B3",
  "p5"="#E7298A","p6"="#66A61E","p7"="#E6AB02",
  "p8"="#000000","p9"="#7A7A7A"
)
color_map <- vanilla_colors

color_map_split <- c(
  color_map,
  "p8 children"="#000000",
  "p8 adults"  ="#A6761D",
  "p6 children"= unname(vanilla_colors["p6"]),
  "p6 adults"  = "#17BECF"
)
names(color_map_split) <- stringr::str_squish(names(color_map_split))

# ---- Helpers: cumulative and percent ----
cum_by_date <- function(df, group_vars, date_col = "submitdate"){
  df %>%
    dplyr::mutate(date = lubridate::as_date(.data[[date_col]])) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)), date) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    tidyr::complete(
      date = seq(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = "day"),
      fill = list(n = 0)
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(group_vars)), date) %>%
    dplyr::mutate(cum_n = cumsum(n)) %>%
    dplyr::ungroup()
}

add_percent_vanilla <- function(df_cum){
  df_cum %>%
    dplyr::mutate(
      project_id = paste0("p", project),
      target = suppressWarnings(as.numeric(total_targets[project_id])),
      target = dplyr::if_else(project == 6 & p6_adults_target_missing, NA_real_, target),
      pct = ifelse(is.na(target) | target <= 0, NA_real_, pmin(100, 100*cum_n/target))
    )
}

add_percent_split <- function(df_cum){
  df_cum %>%
    dplyr::mutate(project_id = paste0("p", project)) %>%
    dplyr::left_join(targets_split, by = c("project","sample")) %>%
    dplyr::mutate(
      proj_target   = suppressWarnings(as.numeric(total_targets[project_id])),
      sample_target = dplyr::if_else(project == 6 & sample == "adults",
                                     target, dplyr::coalesce(target, proj_target))
    ) %>%
    dplyr::select(-target) %>%
    dplyr::mutate(
      pct    = ifelse(is.na(sample_target) | sample_target <= 0, NA_real_, pmin(100, 100*cum_n/sample_target)),
      target = sample_target
    )
}

# ---- Build series ----
vanilla_cum <- timeline_raw %>%
  dplyr::mutate(sample = "ALL") %>%
  cum_by_date(group_vars = c("project", "sample")) %>%
  add_percent_vanilla() %>%
  dplyr::mutate(label = paste0("p", project))

split_cum <- timeline_raw %>%
  dplyr::mutate(
    sample = stringr::str_squish(tolower(sample)),
    sample = dplyr::case_when(
      project == 8 & stringr::str_detect(sample, "adult") ~ "adults",
      project == 8 & stringr::str_detect(sample, "child") ~ "children",
      project == 6 & stringr::str_detect(sample, "child") ~ "children",
      project == 6 & sample == "adults"                  ~ "adults",
      project == 6                                        ~ NA_character_,
      TRUE                                                ~ "ALL"
    )
  ) %>%
  dplyr::filter(!is.na(sample)) %>%
  cum_by_date(group_vars = c("project", "sample")) %>%
  add_percent_split() %>%
  dplyr::mutate(label = dplyr::case_when(
    project == 8 & sample == "children" ~ "p8 children",
    project == 8 & sample == "adults"   ~ "p8 adults",
    project == 6 & sample == "children" ~ "p6 children",
    project == 6 & sample == "adults"   ~ "p6 adults",
    TRUE                                ~ paste0("p", project)
  ),
  label = stringr::str_squish(label))

# ---- Regression (linear) + 95% Newey–West CI (mean fit) ----
fit_project_lm <- function(df_grp){
  if (nrow(df_grp) < 2 || max(df_grp$cum_n, na.rm = TRUE) == 0) return(NULL)
  df_grp <- df_grp %>% dplyr::mutate(tnum = as.numeric(date))
  mod <- try(stats::lm(cum_n ~ tnum, data = df_grp), silent = TRUE)
  if (inherits(mod, "try-error")) return(NULL)
  
  bw  <- try(sandwich::bwNeweyWest(mod), silent = TRUE)
  if (inherits(bw, "try-error") || !is.finite(bw)) bw <- 0
  Vnw <- sandwich::NeweyWest(mod, lag = bw, prewhite = FALSE, adjust = TRUE)
  
  list(model = mod, Vnw = Vnw, df = stats::df.residual(mod), bw = bw)
}

predict_lm_ci_nw <- function(fit, date_seq){
  newd <- tibble::tibble(date = date_seq, tnum = as.numeric(date_seq))
  Xnew <- stats::model.matrix(~ tnum, data = newd)
  beta <- stats::coef(fit$model)
  fitv <- as.numeric(Xnew %*% beta)
  
  Vnw  <- fit$Vnw
  XV   <- Xnew %*% Vnw
  se_m <- sqrt(pmax(0, diag(XV %*% t(Xnew))))
  tval <- stats::qt(0.975, df = fit$df)
  
  tibble::tibble(
    date = date_seq,
    fit  = pmax(0, fitv),
    lwr  = pmax(0, fitv - tval * se_m),
    upr  = pmax(0, fitv + tval * se_m)
  )
}

# ---- X-axis ticks: fixed origin (01.08.2024), 4-month steps ----
mk_four_month_breaks <- function(start_origin = as.Date("2024-08-01"),
                                 last_needed_date) {
  s <- as.Date(start_origin)
  e <- as.Date(last_needed_date)
  seq(s, e, by = "4 months")
}

# ---- Ideal-state helpers (rates + plotting) ----
get_cum_at_or_before <- function(df, ref_date){
  df <- df %>% dplyr::arrange(date)
  sub <- df %>% dplyr::filter(date <= ref_date)
  if (!nrow(sub)) return(0)
  sub$cum_n[nrow(sub)]
}

estimate_finish_date <- function(last_date, last_cum, target, rate_per_day){
  if (is.na(target) || target <= 0) return(as.Date(NA))
  if (!is.na(last_cum) && last_cum >= target) return(as.Date(last_date))
  if (is.na(rate_per_day) || !is.finite(rate_per_day) || rate_per_day <= 0) return(as.Date(NA))
  days_needed <- (target - last_cum) / rate_per_day
  as.Date(last_date + ceiling(days_needed))
}

compute_rates_weekly_with_ideal <- function(df_cum, id_cols = c("label"),
                                            cut_date = ideal_cut_date,
                                            goal_date = ideal_goal_date){
  df_cum %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::group_modify(~{
      df <- .x %>% dplyr::arrange(date)
      
      tgt <- dplyr::last(df$target)
      last_obs_date <- dplyr::last(df$date)
      last_obs_cum  <- dplyr::last(df$cum_n)
      
      if (nrow(df) < 2) {
        return(tibble::tibble(
          target = tgt,
          last_obs_date = last_obs_date,
          last_obs_cum  = last_obs_cum,
          current_rate_per_week = NA_real_,
          finish_date_current = as.Date(NA),
          cum_at_cut = get_cum_at_or_before(df %>% dplyr::select(date, cum_n), cut_date),
          required_rate_per_week_from_2026_02_01 = NA_real_,
          delta_rate_per_week = NA_real_
        ))
      }
      
      df_lm <- df %>% dplyr::mutate(tnum = as.numeric(date))
      fit <- try(stats::lm(cum_n ~ tnum, data = df_lm), silent = TRUE)
      rate_per_day <- if (inherits(fit, "try-error")) NA_real_ else as.numeric(stats::coef(fit)[["tnum"]])
      current_rate_per_week <- rate_per_day * 7
      
      finish_date_current <- estimate_finish_date(last_obs_date, last_obs_cum, tgt, rate_per_day)
      
      cum_at_cut <- get_cum_at_or_before(df %>% dplyr::select(date, cum_n), cut_date)
      if (!is.na(tgt) && is.finite(tgt)) cum_at_cut <- pmin(cum_at_cut, tgt)
      
      remaining <- if (is.na(tgt)) NA_real_ else pmax(0, tgt - cum_at_cut)
      weeks_window <- as.numeric(goal_date - cut_date) / 7
      
      required_rate_per_week <- if (is.na(tgt) || tgt <= 0 || !is.finite(weeks_window) || weeks_window <= 0) {
        NA_real_
      } else {
        remaining / weeks_window
      }
      
      on_time <- FALSE
      if (!is.na(finish_date_current)) {
        on_time <- (finish_date_current <= goal_date)
      } else {
        if (!is.na(tgt) && tgt > 0 && last_obs_cum < tgt) on_time <- FALSE
      }
      
      tibble::tibble(
        target = tgt,
        last_obs_date = last_obs_date,
        last_obs_cum  = last_obs_cum,
        current_rate_per_week = current_rate_per_week,
        finish_date_current = finish_date_current,
        cum_at_cut = cum_at_cut,
        required_rate_per_week_from_2026_02_01 = ifelse(on_time, NA_real_, required_rate_per_week),
        delta_rate_per_week = ifelse(on_time, NA_real_, required_rate_per_week - current_rate_per_week)
      )
    }) %>%
    dplyr::ungroup()
}

# ---- Plot helper (empirical + prediction OR ideal) ----
plot_timeline <- function(df, layout = c("vanilla","split"),
                          with_pred = FALSE,
                          with_ideal = FALSE,
                          abs_cap = c("max","zoom"),
                          pred_end = as.Date(NA),
                          ideal_cut = ideal_cut_date,
                          ideal_goal = ideal_goal_date){
  
  layout    <- match.arg(layout)
  abs_cap   <- match.arg(abs_cap)
  has_pred  <- isTRUE(with_pred)
  has_ideal <- isTRUE(with_ideal)
  
  # colors + y ceiling
  if (layout == "vanilla") {
    df <- df %>% dplyr::mutate(col_lab = label)
    col_map <- color_map
    y_abs_limit <- if (abs_cap=="max") max_target_vanilla else {
      df %>% dplyr::group_by(col_lab) %>% dplyr::slice_max(date, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>% dplyr::summarise(mx = max(cum_n, na.rm = TRUE)) %>% dplyr::pull(mx)
    }
    legend_order <- c("p2","p3","p4","p5","p6","p7","p8","p9")
    legend_order <- legend_order[legend_order %in% unique(df$col_lab)]
  } else {
    df <- df %>% dplyr::mutate(col_lab = label)
    col_map <- color_map_split
    y_abs_limit <- if (abs_cap=="max") max_target_split else {
      df %>% dplyr::group_by(col_lab) %>% dplyr::slice_max(date, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>% dplyr::summarise(mx = max(cum_n, na.rm = TRUE)) %>% dplyr::pull(mx)
    }
    legend_order <- c("p2","p3","p4","p5","p6 children","p6 adults","p7","p8 adults","p8 children","p9")
    legend_order <- legend_order[legend_order %in% unique(df$col_lab)]
  }
  # Unknown targets must not clip observed counts in the absolute panel.
  y_abs_limit <- max(c(y_abs_limit, df$cum_n), na.rm = TRUE)
  if (!is.finite(y_abs_limit) || is.na(y_abs_limit)) y_abs_limit <- NA_real_
  
  # X range and breaks (IMPORTANT: prediction + ideal plots go to 31.12.2027)
  dmax_emp <- max(df$date, na.rm = TRUE)
  
  dmax <- dmax_emp
  if (!is.na(pred_end)) dmax <- max(dmax, pred_end)
  if (has_pred || has_ideal) dmax <- max(dmax, ref_date_2027)
  
  dmin <- as.Date("2024-08-01")
  breaks <- mk_four_month_breaks(dmin, dmax)
  
  # Prediction series (truncate at first target/100% hit)
  pred_df <- NULL; balls_abs <- NULL; balls_pct <- NULL
  if (has_pred) {
    # Keep observed p6 data visible; preserve the existing aggregate-fit exclusion.
    pred_source <- if (layout == "vanilla") dplyr::filter(df, col_lab != "p6") else df
    pred_df <- pred_source %>% dplyr::group_by(col_lab) %>% dplyr::group_split()
    pred_df <- purrr::map_dfr(pred_df, function(grp){
      lab <- unique(grp$col_lab)
      dsub <- grp %>% dplyr::select(date, cum_n, target) %>% dplyr::distinct()
      if (!nrow(dsub) || all(is.na(dsub$cum_n))) return(NULL)
      fitinfo <- fit_project_lm(dsub); if (is.null(fitinfo)) return(NULL)
      end_date <- dmax
      dseq <- seq(min(dsub$date, na.rm = TRUE), end_date, by = "day")
      preds <- predict_lm_ci_nw(fitinfo, dseq) %>%
        dplyr::mutate(target = dsub$target[1], col_lab = lab)
      
      tgt <- preds$target[1]
      hit_idx_abs <- if (!is.na(tgt) && is.finite(tgt) && tgt > 0) which(preds$fit >= tgt)[1] else NA_integer_
      if (!is.na(hit_idx_abs)) preds <- preds[1:hit_idx_abs, , drop = FALSE]
      
      preds <- preds %>%
        dplyr::mutate(
          pct_fit = ifelse(is.na(target) | target <= 0, NA_real_, pmin(100, 100*fit/target)),
          pct_lwr = ifelse(is.na(target) | target <= 0, NA_real_, pmax(0, pmin(100, 100*lwr/target))),
          pct_upr = ifelse(is.na(target) | target <= 0, NA_real_, pmin(100, 100*upr/target))
        )
      
      preds$finished_abs <- FALSE
      if (!is.na(hit_idx_abs)) preds$finished_abs[nrow(preds)] <- TRUE
      
      hit_idx_pct <- which(preds$pct_fit >= 100)[1]
      preds$finished_pct <- FALSE
      if (!is.na(hit_idx_pct)) preds$finished_pct[hit_idx_pct] <- TRUE
      
      preds
    })
    balls_abs <- if (nrow(pred_df)) dplyr::filter(pred_df, finished_abs) else NULL
    balls_pct <- if (nrow(pred_df)) dplyr::filter(pred_df, finished_pct) else NULL
  }
  
  # Ideal series (VISUAL ONLY):
  # - Only for series currently late (finish_date_current > ideal_goal)
  # - Starts at LAST OBSERVED point (last date, last cum_n)
  # - Reaches target by ideal_goal, then stays flat out to 31.12.2027
  ideal_df <- NULL; balls_abs_ideal <- NULL; balls_pct_ideal <- NULL
  if (has_ideal) {
    ideal_df <- df %>%
      dplyr::group_by(col_lab) %>%
      dplyr::group_split() %>%
      purrr::map_dfr(function(grp){
        lab <- unique(grp$col_lab)
        grp <- grp %>% dplyr::arrange(date)
        
        tgt <- dplyr::last(grp$target)
        if (is.na(tgt) || !is.finite(tgt) || tgt <= 0) return(NULL)
        
        last_obs_date <- dplyr::last(grp$date)
        last_obs_cum  <- dplyr::last(grp$cum_n)
        
        # If already finished, do not plot ideal line
        if (!is.na(last_obs_cum) && last_obs_cum >= tgt) return(NULL)
        
        # If last observation is after the deadline, skip (cannot draw sensible "reach by deadline")
        if (!is.na(last_obs_date) && last_obs_date >= ideal_goal) return(NULL)
        
        # Determine if they are "late" at current speed (LM over observed history)
        finish_date_current <- as.Date(NA)
        if (nrow(grp) >= 2) {
          grp_lm <- grp %>% dplyr::mutate(tnum = as.numeric(date))
          fit <- try(stats::lm(cum_n ~ tnum, data = grp_lm), silent = TRUE)
          rate_per_day <- if (inherits(fit, "try-error")) NA_real_ else as.numeric(stats::coef(fit)[["tnum"]])
          finish_date_current <- estimate_finish_date(last_obs_date, last_obs_cum, tgt, rate_per_day)
        }
        
        # If already on-time (<= ideal_goal), do NOT draw ideal line (avoid "encouraging slowdown")
        if (!is.na(finish_date_current) && finish_date_current <= ideal_goal) return(NULL)
        
        # Visual slope: from last observed point to hit target by ideal_goal
        days_to_goal <- as.numeric(ideal_goal - last_obs_date)
        if (!is.finite(days_to_goal) || days_to_goal <= 0) return(NULL)
        
        slope_per_day <- (tgt - last_obs_cum) / days_to_goal
        
        dseq <- seq(last_obs_date, ref_date_2027, by = "day")
        fitv <- last_obs_cum + slope_per_day * as.numeric(dseq - last_obs_date)
        fitv <- pmax(0, fitv)
        
        # Cap at target; once hit, stay flat through 2027
        hit_idx <- which(fitv >= tgt)[1]
        if (!is.na(hit_idx)) {
          fitv[hit_idx:length(fitv)] <- tgt
        } else {
          # If numerical issues prevent reaching, cap at target at the end anyway
          fitv[length(fitv)] <- min(tgt, fitv[length(fitv)])
        }
        
        tibble::tibble(
          date = dseq,
          fit  = pmin(tgt, fitv),
          target = tgt,
          col_lab = lab,
          pct_fit = pmin(100, 100 * pmin(tgt, fitv) / tgt),
          finished_abs = date == dseq[ ifelse(is.na(hit_idx), length(dseq), hit_idx) ],
          finished_pct = date == dseq[ ifelse(is.na(hit_idx), length(dseq), hit_idx) ]
        )
      })
    
    balls_abs_ideal <- if (nrow(ideal_df)) dplyr::filter(ideal_df, finished_abs) else NULL
    balls_pct_ideal <- if (nrow(ideal_df)) dplyr::filter(ideal_df, finished_pct) else NULL
  }
  
  # Aesthetics
  # Approximately one third thinner than before (old: 1.1 / 0.9 / 0.3)
  line_size           <- 0.73
  pred_size           <- 0.60
  ref_line_size       <- 0.20
  ball_size           <- 2.8
  rib_alpha           <- 0.18
  middle_subtitle_size <- 8
  middle_subtitle_wrap <- 105
  
  # Titles / captions
  main_title <- "Sample size over time via complete backbone datasets"
  perc_sub_emp  <- paste0(
    "Top: absolute | Bottom: percent of integration target sample size",
    if (any(df$project == 6 & df$sample == "adults")) " • p6 adults: cognitive tests only" else "",
    if (any(is.na(df$target))) " • Unknown targets: no percentage" else ""
  )
  perc_sub_pred <- paste0(
    perc_sub_emp,
    " \u2022 Shaded: 95% Newey\u2013West CI = \u0177\u0302 \u00B1 t\u2080.\u2089\u2087\u2085,df \u221A(x\u2032 V\u2099\u2093 x)"
  )
  perc_sub_ideal <- paste0(
    perc_sub_emp,
    " \u2022 Ideal visualization: from last observed point to reach target by ",
    format(ideal_goal, "%d.%m.%Y"), "."
  )
  
  # Reference lines
  vline_ref_abs  <- if (has_pred || has_ideal)
    ggplot2::geom_vline(xintercept = as.numeric(ref_date_2027),
                        linetype = "dotted", linewidth = ref_line_size, color = "grey40") else NULL
  vline_ref_pct  <- if (has_pred || has_ideal)
    ggplot2::geom_vline(xintercept = as.numeric(ref_date_2027),
                        linetype = "dotted", linewidth = ref_line_size, color = "grey40") else NULL
  
  vline_goal_abs <- if (has_ideal)
    ggplot2::geom_vline(xintercept = as.numeric(ideal_goal),
                        linetype = "dotted", linewidth = ref_line_size, color = "grey40") else NULL
  vline_goal_pct <- if (has_ideal)
    ggplot2::geom_vline(xintercept = as.numeric(ideal_goal),
                        linetype = "dotted", linewidth = ref_line_size, color = "grey40") else NULL
  
  vline_cut <- if (has_ideal)
    ggplot2::geom_vline(xintercept = as.numeric(ideal_cut),
                        linetype = "dashed", linewidth = ref_line_size, color = "grey40") else NULL
  
  # Show a newly started series even when it contains only one observation date.
  single_date <- df %>% dplyr::group_by(col_lab) %>%
    dplyr::filter(dplyr::n_distinct(date) == 1L) %>% dplyr::ungroup()

  # Absolute panel
  y_top <- y_abs_limit
  if ((has_pred || has_ideal) && is.finite(y_top)) y_top <- y_top + 1
  
  p_abs <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df, ggplot2::aes(x = date, y = cum_n, color = col_lab), linewidth = line_size) +
    ggplot2::geom_point(data = single_date, ggplot2::aes(x = date, y = cum_n, color = col_lab), size = 1.8) +
    vline_cut + vline_goal_abs + vline_ref_abs +
    ggplot2::scale_x_date(breaks = breaks, labels = scales::label_date(format = "%d.%m.%Y"),
                          expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::scale_y_continuous(limits = c(0, y_top), expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::labs(x = NULL, y = "Cumulative sample size per project", title = main_title) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "right", legend.box = "vertical") +
    ggplot2::scale_color_manual(values = col_map, name = NULL,
                                limits = legend_order, breaks = legend_order, drop = FALSE,
                                guide = ggplot2::guide_legend(ncol = 1))
  
  if (has_pred && !is.null(pred_df) && nrow(pred_df)) {
    p_abs <- p_abs +
      ggplot2::geom_ribbon(data = pred_df, ggplot2::aes(x = date, ymin = lwr, ymax = upr, fill = col_lab),
                           alpha = rib_alpha, inherit.aes = FALSE) +
      ggplot2::geom_line(data = pred_df, ggplot2::aes(x = date, y = fit, color = col_lab),
                         linewidth = pred_size, linetype = "solid") +
      ggplot2::scale_fill_manual(values = col_map, guide = "none",
                                 limits = legend_order, breaks = legend_order, drop = FALSE)
    if (!is.null(balls_abs) && nrow(balls_abs)) {
      p_abs <- p_abs + ggplot2::geom_point(data = balls_abs,
                                           ggplot2::aes(x = date, y = fit, color = col_lab), size = ball_size)
    }
  }
  
  if (has_ideal && !is.null(ideal_df) && nrow(ideal_df)) {
    p_abs <- p_abs +
      ggplot2::geom_line(data = ideal_df,
                         ggplot2::aes(x = date, y = fit, color = col_lab),
                         linewidth = pred_size, linetype = "dashed")
    if (!is.null(balls_abs_ideal) && nrow(balls_abs_ideal)) {
      p_abs <- p_abs + ggplot2::geom_point(data = balls_abs_ideal,
                                           ggplot2::aes(x = date, y = fit, color = col_lab),
                                           size = ball_size)
    }
  }
  
  # Percent panel
  p_pct <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df, ggplot2::aes(x = date, y = pct, color = col_lab), linewidth = line_size) +
    ggplot2::geom_point(data = single_date, ggplot2::aes(x = date, y = pct, color = col_lab), size = 1.8, na.rm = TRUE) +
    vline_cut + vline_goal_pct + vline_ref_pct +
    ggplot2::scale_x_date(breaks = breaks, labels = scales::label_date(format = "%d.%m.%Y"),
                          expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0,25,50,75,100)) +
    ggplot2::labs(
      x = NULL,
      y = "Percent of integration target sample size",
      subtitle = stringr::str_wrap(
        if (has_pred) perc_sub_pred else if (has_ideal) perc_sub_ideal else perc_sub_emp,
        width = middle_subtitle_wrap
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      legend.box = "vertical",
      plot.subtitle = ggplot2::element_text(
        size = middle_subtitle_size,
        lineheight = 0.95,
        margin = ggplot2::margin(b = 4)
      )
    ) +
    ggplot2::scale_color_manual(values = col_map, name = NULL,
                                limits = legend_order, breaks = legend_order, drop = FALSE,
                                guide = ggplot2::guide_legend(ncol = 1))
  
  if (has_pred && !is.null(pred_df) && nrow(pred_df)) {
    p_pct <- p_pct +
      ggplot2::geom_ribbon(data = pred_df, ggplot2::aes(x = date, ymin = pct_lwr, ymax = pct_upr, fill = col_lab),
                           alpha = rib_alpha, inherit.aes = FALSE) +
      ggplot2::geom_line(data = pred_df, ggplot2::aes(x = date, y = pct_fit, color = col_lab),
                         linewidth = pred_size, linetype = "solid") +
      ggplot2::scale_fill_manual(values = col_map, guide = "none",
                                 limits = legend_order, breaks = legend_order, drop = FALSE)
    if (!is.null(balls_pct) && nrow(balls_pct)) {
      p_pct <- p_pct + ggplot2::geom_point(data = balls_pct,
                                           ggplot2::aes(x = date, y = pct_fit, color = col_lab), size = ball_size)
    }
  }
  
  if (has_ideal && !is.null(ideal_df) && nrow(ideal_df)) {
    p_pct <- p_pct +
      ggplot2::geom_line(data = ideal_df,
                         ggplot2::aes(x = date, y = pct_fit, color = col_lab),
                         linewidth = pred_size, linetype = "dashed")
    if (!is.null(balls_pct_ideal) && nrow(balls_pct_ideal)) {
      p_pct <- p_pct + ggplot2::geom_point(data = balls_pct_ideal,
                                           ggplot2::aes(x = date, y = pct_fit, color = col_lab),
                                           size = ball_size)
    }
  }
  
  (p_abs + ggplot2::theme(legend.position = "right")) /
    (p_pct + ggplot2::theme(legend.position = "none")) +
    patchwork::plot_layout(heights = c(1,1), guides = "collect")
}

# ---- Exports ----
vanilla_export <- vanilla_cum %>%
  dplyr::transmute(
    date, project, label,
    cum_n = cum_n,
    target = target,
    pct = round(pct, round_digits)
  )

split_export <- split_cum %>%
  dplyr::transmute(
    date, project, sample, label,
    cum_n = cum_n,
    target = target,
    pct = round(pct, round_digits)
  )

file_base_vanilla <- file.path(out_dir, sprintf("%s_timeline_data_collection", datestamp_last))
file_base_split   <- file.path(out_dir, sprintf("%s_timeline_data_collection_split", datestamp_last))

# Empirical (zoomed) only
p_vanilla_zoom <- plot_timeline(vanilla_cum, layout = "vanilla", with_pred = FALSE, with_ideal = FALSE, abs_cap = "zoom")
ggplot2::ggsave(paste0(file_base_vanilla, "_zoomed.png"), p_vanilla_zoom, width = png_width, height = png_height, dpi = png_dpi)
writexl::write_xlsx(list(timeline_vanilla = vanilla_export), path = paste0(file_base_vanilla, "_zoomed.xlsx"))

p_split_zoom <- plot_timeline(split_cum, layout = "split", with_pred = FALSE, with_ideal = FALSE, abs_cap = "zoom")
ggplot2::ggsave(paste0(file_base_split, "_zoomed.png"), p_split_zoom, width = png_width, height = png_height, dpi = png_dpi)
writexl::write_xlsx(list(timeline_split = split_export), path = paste0(file_base_split, "_zoomed.xlsx"))

# Prediction (max-cap) only, horizon to 31.12.2027
pred_end_date <- ref_date_2027

p_vanilla_pred <- plot_timeline(vanilla_cum, layout = "vanilla", with_pred = TRUE, with_ideal = FALSE,
                                abs_cap = "max", pred_end = pred_end_date)
ggplot2::ggsave(paste0(file_base_vanilla, "_and-prediction.png"), p_vanilla_pred,
                width = png_width, height = png_height, dpi = png_dpi)

p_split_pred <- plot_timeline(split_cum, layout = "split", with_pred = TRUE, with_ideal = FALSE,
                              abs_cap = "max", pred_end = pred_end_date)
ggplot2::ggsave(paste0(file_base_split, "_and-prediction.png"), p_split_pred,
                width = png_width, height = png_height, dpi = png_dpi)

# Ideal-state plot (split): visualization (start at last observed point; reach target by 31.12.2026; flat to 31.12.2027)
p_split_ideal <- plot_timeline(split_cum, layout = "split",
                               with_pred = FALSE, with_ideal = TRUE,
                               abs_cap = "max",
                               pred_end = pred_end_date,
                               ideal_cut = ideal_cut_date,
                               ideal_goal = ideal_goal_date)

ggplot2::ggsave(paste0(file_base_split, "_ideal-to-2026.png"),
                p_split_ideal, width = png_width, height = png_height, dpi = png_dpi)

# ---- Weekly rate summary + required weekly rate (Excel numbers stay based on Feb 1 onward) ----
rates_vanilla_week <- compute_rates_weekly_with_ideal(vanilla_cum, id_cols = c("label")) %>%
  dplyr::rename(project = label) %>%
  dplyr::mutate(
    current_rate_per_week = round(current_rate_per_week, round_digits),
    required_rate_per_week_from_2026_02_01 = round(required_rate_per_week_from_2026_02_01, round_digits),
    delta_rate_per_week = round(delta_rate_per_week, round_digits)
  )

rates_split_week <- compute_rates_weekly_with_ideal(split_cum, id_cols = c("label")) %>%
  dplyr::rename(series = label) %>%
  dplyr::mutate(
    current_rate_per_week = round(current_rate_per_week, round_digits),
    required_rate_per_week_from_2026_02_01 = round(required_rate_per_week_from_2026_02_01, round_digits),
    delta_rate_per_week = round(delta_rate_per_week, round_digits)
  )

rate_summary <- rates_vanilla_week %>%
  dplyr::summarise(
    mean_current_rate_per_week = round(mean(current_rate_per_week, na.rm = TRUE), round_digits),
    sd_current_rate_per_week   = round(sd(current_rate_per_week, na.rm = TRUE), round_digits)
  )

slowest <- rates_vanilla_week %>%
  dplyr::filter(is.finite(current_rate_per_week)) %>%
  dplyr::slice_min(current_rate_per_week, n = 1, with_ties = FALSE)

fastest <- rates_vanilla_week %>%
  dplyr::filter(is.finite(current_rate_per_week)) %>%
  dplyr::slice_max(current_rate_per_week, n = 1, with_ties = FALSE)

# ---- Forecast table: expected recruited participants by 31.12.2026 ----
forecast_end_date <- ideal_goal_date   # = 2026-12-31

make_forecast_table <- function(df_cum, id_cols = c("label"),
                                forecast_date = forecast_end_date,
                                cap_at_target = TRUE) {
  df_cum %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::group_modify(~{
      grp <- .x %>% dplyr::arrange(date)
      
      tgt <- dplyr::last(grp$target)
      last_obs_date <- dplyr::last(grp$date)
      last_obs_cum  <- dplyr::last(grp$cum_n)
      
      fitinfo <- fit_project_lm(grp %>% dplyr::select(date, cum_n))
      
      if (is.null(fitinfo)) {
        pred_raw <- last_obs_cum
        lwr_raw  <- NA_real_
        upr_raw  <- NA_real_
        method   <- "last_observed"
      } else {
        pr <- predict_lm_ci_nw(fitinfo, as.Date(forecast_date))
        pred_raw <- pr$fit[1]
        lwr_raw  <- pr$lwr[1]
        upr_raw  <- pr$upr[1]
        method   <- "linear_projection"
      }
      
      pred_use <- pred_raw
      lwr_use  <- lwr_raw
      upr_use  <- upr_raw
      
      if (isTRUE(cap_at_target) && !is.na(tgt) && is.finite(tgt) && tgt > 0) {
        pred_use <- pmin(pred_use, tgt)
        lwr_use  <- pmin(lwr_use, tgt)
        upr_use  <- pmin(upr_use, tgt)
      }
      
      tibble::tibble(
        target = tgt,
        last_obs_date = last_obs_date,
        last_obs_cum = last_obs_cum,
        forecast_date = as.Date(forecast_date),
        predicted_cum_n = pred_use,
        predicted_lwr = lwr_use,
        predicted_upr = upr_use,
        additional_expected_until_year_end = pmax(0, pred_use - last_obs_cum),
        predicted_pct_of_target = ifelse(
          is.na(tgt) | tgt <= 0,
          NA_real_,
          100 * pred_use / tgt
        ),
        method = method
      )
    }) %>%
    dplyr::ungroup()
}

forecast_vanilla_2026 <- make_forecast_table(
  vanilla_cum,
  id_cols = c("label"),
  forecast_date = forecast_end_date,
  cap_at_target = TRUE
) %>%
  dplyr::rename(project = label) %>%
  dplyr::left_join(
    vanilla_cum %>% dplyr::select(label, project_num = project) %>% dplyr::distinct(),
    by = c("project" = "label")
  ) %>%
  dplyr::select(
    project_num, project, target,
    last_obs_date, last_obs_cum,
    forecast_date, predicted_cum_n, predicted_lwr, predicted_upr,
    additional_expected_until_year_end, predicted_pct_of_target, method
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(target, last_obs_cum, predicted_cum_n, predicted_lwr, predicted_upr,
        additional_expected_until_year_end, predicted_pct_of_target),
      ~ round(.x, round_digits)
    )
  ) %>%
  dplyr::arrange(project_num)

forecast_split_2026 <- make_forecast_table(
  split_cum,
  id_cols = c("label"),
  forecast_date = forecast_end_date,
  cap_at_target = TRUE
) %>%
  dplyr::rename(series = label) %>%
  dplyr::left_join(
    split_cum %>% dplyr::select(label, project, sample) %>% dplyr::distinct(),
    by = c("series" = "label")
  ) %>%
  dplyr::select(
    project, sample, series, target,
    last_obs_date, last_obs_cum,
    forecast_date, predicted_cum_n, predicted_lwr, predicted_upr,
    additional_expected_until_year_end, predicted_pct_of_target, method
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(target, last_obs_cum, predicted_cum_n, predicted_lwr, predicted_upr,
        additional_expected_until_year_end, predicted_pct_of_target),
      ~ round(.x, round_digits)
    )
  ) %>%
  dplyr::arrange(project, sample, series)


writexl::write_xlsx(
  list(
    rates_vanilla_weekly = rates_vanilla_week,
    rates_split_weekly   = rates_split_week,
    forecast_vanilla_2026 = forecast_vanilla_2026,
    forecast_split_2026   = forecast_split_2026,
    summary              = rate_summary,
    slowest              = slowest,
    fastest              = fastest
  ),
  path = file.path(out_dir, sprintf("%s_data_collection_rates_weekly_with_ideal.xlsx", datestamp_last))
)

message("Saved outputs in: ", normalizePath(out_dir))
