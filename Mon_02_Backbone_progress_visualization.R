#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Monitoring 2 — Visualize Backbone Progress
# Authors: Saskia Wilken (saskia.wilken@uni-hamburg.de, saskia.a.wilken@gmail.com)
# Updated: 2026-11-04
#
# RUN MON_01 first!
#
# What this does
#  - Reads the 3-sheet ID completeness report create by mon_01:
#       1) complete datasets
#       2) datasets missing questionnaire data  (=> cognitive present)
#       3) datasets missing cognitive test data (=> questionnaires present)
#  - Produces a progress plot (PNG) and a summary XLSX.
#  - p6 adults require cognitive tests only (classified as complete by Mon_01).
#    Their confirmed cohort-specific target is N = 44 (2026-11-04).
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clean up R environment -------------------------------------------------------
rm(list = ls())
cat("\014")

#------------ Read in packages ---------------------

if (requireNamespace("here", quietly = TRUE)) {
  # one-time in this file: declare where this script lives (relative to project root)
  here::i_am("Mon_02_Backbone_progress_visualization.R")
  ROOT <- here::here()
} else if (requireNamespace("rprojroot", quietly = TRUE)) {
  ROOT <- rprojroot::find_root(
    rprojroot::has_file(".Rproj") | rprojroot::has_file(".here") | rprojroot::has_dir(".git")
  )
} else {
  ROOT <- getwd()
}

if (!require("colorspace")){ install.packages("colorspace")}; library(colorspace)

# ---- Setup ----
auto_install <- FALSE
pkgs <- c("readxl","dplyr","stringr","tidyr","ggplot2","forcats","purrr","writexl","tools")
if (auto_install) {
  to_get <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if (length(to_get)) install.packages(to_get)
}
invisible(lapply(pkgs, require, character.only = TRUE))

# ---- Config ----
out_dir  <- file.path(ROOT, "out")
png_width <- 10; png_height <- 5; png_dpi <- 300

# Toggle: one bar per project (default) vs per-sample bars
per_sample_bars <- FALSE
max_samples_per_project <- 3

# Aesthetics
outline_color <- "black"
refline_color <- "grey60"
axis_labels <- c(p2="p2", p3="p3", p4="p4", p5="p5", p6="p6", p7="p7", p8="p8", p9="p9")

# ---- Targets from XLSX (replaces hardcoded total_targets) ----
target_xlsx <- file.path(ROOT, "information", "2025-11-10_Target_Sample_Size_Projects.xlsx")
if (!file.exists(target_xlsx)) stop("Target table not found: ", target_xlsx)

read_targets_vanilla <- function(path){
  df <- suppressMessages(readxl::read_excel(path, sheet = "Vanilla"))
  names(df) <- tolower(names(df))
  df %>%
    dplyr::transmute(
      project = as.integer(stringr::str_extract(as.character(project), "\\d+")),
      target  = as.numeric(target)
    ) %>%
    dplyr::filter(!is.na(project))
}
targets_vanilla <- read_targets_vanilla(target_xlsx)
# Named vector keyed by "p2","p3",...
total_targets <- setNames(targets_vanilla$target, paste0("p", targets_vanilla$project))

read_targets_split <- function(path){
  df <- suppressMessages(readxl::read_excel(path, sheet = "P8_Split_P6.2_only"))
  names(df) <- tolower(names(df))
  tibble::tibble(project_raw = as.character(df$project), target = as.numeric(df$target)) %>%
    dplyr::mutate(
      prj_lower = tolower(project_raw),
      project   = as.integer(stringr::str_extract(prj_lower, "\\d+")),
      sample    = dplyr::case_when(
        stringr::str_detect(prj_lower, "child") ~ "children",
        stringr::str_detect(prj_lower, "adult") ~ "adults",
        TRUE                                    ~ "ALL"
      )
    ) %>%
    dplyr::filter(!is.na(project)) %>%
    dplyr::select(project, sample, target)
}
targets_split <- read_targets_split(target_xlsx)
# Confirmed by Saskia on 2026-11-04: p6 adults target N = 44.
# Apply to this cohort only, including when the workbook has no adult row yet.
targets_split <- targets_split %>%
  dplyr::filter(!(project == 6 & sample == "adults")) %>%
  dplyr::bind_rows(tibble::tibble(project = 6L, sample = "adults", target = 44))


# ---- Read latest report ----
priv_dir <- file.path(ROOT, "private_information", "ids_in_all_projects")
report_files <- list.files(priv_dir, pattern = "_id_completeness_report\\.xlsx$", full.names = TRUE)
if (!length(report_files)) stop("No *_id_completeness_report.xlsx found in 'private_information'.")
report_path <- sort(report_files, decreasing = TRUE)[1]
datestamp <- sub("_id_completeness_report\\.xlsx$", "", basename(report_path))  # used for caption + filenames

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
output_png  <- file.path(out_dir, sprintf("%s_project_progress.png", datestamp))
output_xlsx <- file.path(out_dir, sprintf("%s_project_progress_summary.xlsx", datestamp))

read_sheet_safe <- function(path, sheet){
  df <- suppressMessages(readxl::read_excel(path, sheet = sheet))
  if (!is.data.frame(df) || !nrow(df)) return(tibble::tibble())
  names(df) <- tolower(names(df))
  pid_col <- names(df)[stringr::str_detect(names(df), "^participant\\s*id$")]
  if (!length(pid_col)) {
    pid_col <- names(df)[stringr::str_detect(names(df), "id")]
    if (!length(pid_col)) pid_col <- names(df)[ncol(df)]
  }
  tibble::tibble(
    project = suppressWarnings(as.integer(df[["project"]])),
    sample  = tolower(if ("sample" %in% names(df)) df[["sample"]] else ""),
    pid     = df[[pid_col[1]]]
  ) %>%
    dplyr::filter(!is.na(project), project >= 2, project <= 9)
}

sheet_complete <- read_sheet_safe(report_path, 1)   # fully complete
sheet_miss_q   <- read_sheet_safe(report_path, 2)   # missing questionnaire  (=> cognitive present)
sheet_miss_c   <- read_sheet_safe(report_path, 3)   # missing cognitive      (=> questionnaires present)

count_by_ps <- function(df){
  if (!nrow(df)) tibble::tibble(project=integer(), sample=character(), n=integer())
  else dplyr::count(df, project, sample, name="n")
}
n_complete <- count_by_ps(sheet_complete)
n_miss_q   <- count_by_ps(sheet_miss_q)
n_miss_c   <- count_by_ps(sheet_miss_c)

# A new adult cohort must not inherit the target of the existing p6 cohorts.
p6_adults_target <- targets_split %>%
  dplyr::filter(project == 6, sample == "adults", is.finite(target), target > 0) %>%
  dplyr::pull(target) %>% .[1]
p6_adults_present <- any(n_complete$project == 6 & n_complete$sample == "adults")

# ---- Build counts (project vs sample mode) ----
if (per_sample_bars) {
  all_ps <- dplyr::bind_rows(n_complete[,1:2], n_miss_q[,1:2], n_miss_c[,1:2]) %>%
    dplyr::distinct() %>% dplyr::arrange(project, sample) %>%
    dplyr::group_by(project) %>% dplyr::slice_head(n = max_samples_per_project) %>% dplyr::ungroup()
  
  progress_counts <- all_ps %>%
    dplyr::left_join(n_complete, by=c("project","sample")) %>% dplyr::rename(n_complete=n) %>%
    dplyr::left_join(n_miss_q, by=c("project","sample"))   %>% dplyr::rename(n_miss_q =n) %>%
    dplyr::left_join(n_miss_c, by=c("project","sample"))   %>% dplyr::rename(n_miss_c =n) %>%
    dplyr::mutate(dplyr::across(c(n_complete,n_miss_q,n_miss_c), ~tidyr::replace_na(.,0L)))
} else {
  # aggregate per project first
  progress_counts <- dplyr::full_join(
    n_complete %>% dplyr::group_by(project) %>% dplyr::summarise(n_complete=sum(n), .groups="drop"),
    n_miss_q   %>% dplyr::group_by(project) %>% dplyr::summarise(n_miss_q  =sum(n), .groups="drop"),
    by="project"
  ) %>%
    dplyr::full_join(
      n_miss_c %>% dplyr::group_by(project) %>% dplyr::summarise(n_miss_c=sum(n), .groups="drop"),
      by="project"
    ) %>%
    dplyr::right_join(tibble::tibble(project=2:9), by="project") %>%
    dplyr::mutate(dplyr::across(c(n_complete,n_miss_q,n_miss_c), ~tidyr::replace_na(.,0L)),
                  sample="ALL")
}

# ---- Layer definitions (correct mapping) ----
# "cognitive tests missing"  := complete + MISSING COGNITIVE   (has questionnaires)  -> ORANGE
# "questionnaires missing"   := complete + MISSING QUESTIONNAIRE (has cognitive)      -> BLUE
progress_counts <- progress_counts %>%
  dplyr::mutate(
    n_has_questionnaire = n_complete + n_miss_c,  # <- used for "cognitive tests missing" overlay
    n_has_cognitive     = n_complete + n_miss_q,  # <- used for "questionnaires missing" overlay
    project_id          = paste0("p", project),
    proj_target         = as.numeric(total_targets[project_id]),
    sample_target       = dplyr::case_when(
      project == 6 & sample == "adults" ~ p6_adults_target,
      project == 6 & sample == "ALL" & p6_adults_present & is.na(p6_adults_target) ~ NA_real_,
      TRUE ~ proj_target
    )
  )

# ---- Special rule: p6 forced visual 33% + label "TBA / ?" ----
progress_counts <- progress_counts %>%
  dplyr::mutate(force_33 = project == 6 & sample != "adults")

# ---- Percent-of-target (base for label position is COMPLETE) ----
pctify <- function(n, target) ifelse(is.na(target) | target <= 0, NA_real_, pmin(100, 100*n/target))

progress_pct <- progress_counts %>%
  dplyr::mutate(
    pct_complete = pctify(n_complete,         sample_target),
    pct_qmiss    = pctify(n_has_questionnaire, sample_target),  # cognitive tests missing (orange)
    pct_cmiss    = pctify(n_has_cognitive,     sample_target)   # questionnaires missing (blue)
  ) %>%
  dplyr::mutate(
    # For p6: force all layers to 33% visually; we still don't show % in label.
    pct_complete = dplyr::if_else(force_33, 33, pct_complete),
    pct_qmiss    = dplyr::if_else(force_33, 33, pct_qmiss),
    pct_cmiss    = dplyr::if_else(force_33, 33, pct_cmiss)
  )

# ---- Plot data ----
plot_df <- progress_pct %>%
  dplyr::mutate(
    project_lbl = factor(paste0("p", project),
                         levels=paste0("p",2:9),
                         labels=axis_labels[paste0("p",2:9)])
  ) %>%
  dplyr::arrange(project)

plot_long <- plot_df %>%
  tidyr::pivot_longer(
    cols = c(pct_qmiss, pct_cmiss, pct_complete),
    names_to = "layer", values_to = "pct"
  ) %>%
  dplyr::mutate(
    x = factor(as.character(project_lbl), levels = axis_labels[paste0("p",2:9)]),
    layer = factor(layer,
                   levels = c("pct_qmiss","pct_cmiss","pct_complete"),
                   labels = c("cognitive tests missing","questionnaires missing","complete"))
  )

# 40% opacity fills (hex alpha '66')
layer_fills <- c(
  "cognitive tests missing" = "#F7C97F",  # orange
  "questionnaires missing"  = "#8CAED0",  # blue
  "complete"                = "#6CAB5C"   # green
)

p <- ggplot(plot_long, aes(x=x, y=pct)) +
  # 100% outlines
  geom_col(data = plot_long %>% dplyr::distinct(x), aes(x=x, y=100),
           inherit.aes=FALSE, fill=NA, color=outline_color, linewidth=0.9, width=0.7) +
  # overlays
  geom_col(aes(fill=layer), width=0.7, position=position_identity()) +
  # guides
  geom_hline(yintercept=c(25,50,75), linetype="dashed", linewidth=0.3, color=refline_color) +
  coord_flip(clip="off") +
  scale_fill_manual(values=layer_fills, name=NULL) +
  scale_y_continuous(limits=c(0,110), breaks=c(0,25,50,75,100),
                     labels=c("0","25","50","75","100"),
                     expand=expansion(mult=c(0.02,0.12))) +
  labs(
    x = NULL,
    y = 'Percent of target sample size',
    title = 'Completed backbone datasets',
    caption = paste0("Status: ", datestamp)   # lower-left caption
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.background       = element_rect(fill="white", color=NA),
    plot.background        = element_rect(fill="white", color=NA),
    axis.title.y           = element_blank(),
    legend.position        = "bottom",
    plot.caption.position  = "plot",
    plot.caption           = element_text(hjust = 0, size = 8)  # left-aligned, smaller
  )

# ---- Numeric labels inside bars (based on COMPLETE) ----
label_df <- plot_df %>%
  dplyr::mutate(
    label_total_complete = ifelse(force_33, NA_integer_, n_complete),
    pct_for_label        = ifelse(force_33, NA_real_, pctify(n_complete, sample_target))
  ) %>%
  dplyr::transmute(
    x = factor(as.character(project_lbl), levels = axis_labels[paste0("p",2:9)]),
    # place label slightly to the RIGHT of the green (complete) fill
    pct_pos = dplyr::case_when(
      force_33             ~ 35,                              # p6: near 33% fill
      is.na(pct_for_label) ~ 5,                               # unknown target -> near left edge
      TRUE                 ~ pmin(98, pmax(5, pct_for_label + 2))
    ),
    label = dplyr::case_when(
      force_33             ~ sprintf("%d / ?", n_complete),
      is.na(sample_target) ~ sprintf("%d / ?", n_complete),
      TRUE                 ~ sprintf("%d / %d (%.0f%%)", n_complete, sample_target, pct_for_label)
    )
  )

p <- p + geom_text(data = label_df, aes(x=x, y=pct_pos, label=label),
                   inherit.aes = FALSE, hjust = 0, size = 3.4)

# ---- Save plot ----
ggsave(output_png, p, width = png_width, height = png_height, dpi = png_dpi)
message("Saved plot: ", normalizePath(output_png))

# ---- Summary table (printed + XLSX) ----
summary_tbl <- progress_pct %>%
  dplyr::transmute(
    project_id = paste0("p", project),
    n_complete,
    n_questionnaires_missing = n_has_cognitive - n_complete,    # actually missing questionnaires
    n_cognitive_missing      = n_has_questionnaire - n_complete,# actually missing cognitive tests
    target = dplyr::if_else(is.na(sample_target), "?", as.character(sample_target)),
    pct_complete = ifelse(is.na(sample_target) | force_33, NA, round(pctify(n_complete, sample_target),1)),
    label_for_plot = dplyr::case_when(
      force_33             ~ sprintf("%d / ?", n_complete),
      is.na(sample_target) ~ sprintf("%d / ?", n_complete),
      TRUE                 ~ sprintf("%d / %d (%.0f%%)", n_complete, sample_target, pctify(n_complete, sample_target))
    )
  ) %>% dplyr::arrange(project_id)

print(summary_tbl, n = Inf)
writexl::write_xlsx(list(progress_summary = summary_tbl), path = output_xlsx)
message("Saved table: ", normalizePath(output_xlsx))



# ---- Second plot: split p6 children/adults (infants hidden) AND p8 children/adults ----
# Output name remains "_split_p6_p8"; p6 infants stay hidden as before.
output_png_split <- file.path(out_dir, sprintf("%s_project_progress_split_p6_p8.png", datestamp))

# Helper: for split view
# - p8 => keep "children"/"adults" using flexible matching
# - p6 => keep "children" and "adults"; hide/drop infants as before
# - all others => "ALL"
collapse_with_split_p6_p8 <- function(df_counts){
  if (!nrow(df_counts)) return(tibble::tibble(project=integer(), sample=character(), n=integer()))
  df_counts %>%
    dplyr::mutate(
      sl = stringr::str_squish(tolower(as.character(sample))),
      sample = dplyr::case_when(
        project == 8 & stringr::str_detect(sl, "adult") ~ "adults",
        project == 8 & stringr::str_detect(sl, "child") ~ "children",
        project == 6 & sl == "children"                 ~ "children",
        project == 6 & sl == "adults"                   ~ "adults",
        project == 6                                    ~ NA_character_,
        TRUE                                            ~ "ALL"
      )
    ) %>%
    dplyr::filter(!is.na(sample)) %>%
    dplyr::group_by(project, sample) %>%
    dplyr::summarise(n = sum(n), .groups = "drop")
}

n_complete_s <- collapse_with_split_p6_p8(n_complete)
n_miss_q_s   <- collapse_with_split_p6_p8(n_miss_q)
n_miss_c_s   <- collapse_with_split_p6_p8(n_miss_c)

# Ensure rows for:
# p2..p5, p7, p9 => "ALL"
# p6 => "children" and "adults"; infants remain hidden in this split plot
# p8 => "children" and "adults"
base_rows <- tibble::tibble(
  project = c(2:5, 6, 6, 7, 8, 8, 9),
  sample  = c(rep("ALL", 4), "children", "adults", "ALL", "children", "adults", "ALL")
)

progress_counts_split <- base_rows %>%
  dplyr::left_join(n_complete_s, by=c("project","sample")) %>% dplyr::rename(n_complete = n) %>%
  dplyr::left_join(n_miss_q_s,   by=c("project","sample")) %>% dplyr::rename(n_miss_q  = n) %>%
  dplyr::left_join(n_miss_c_s,   by=c("project","sample")) %>% dplyr::rename(n_miss_c  = n) %>%
  dplyr::mutate(dplyr::across(c(n_complete, n_miss_q, n_miss_c), ~tidyr::replace_na(., 0L))) %>%
  dplyr::mutate(
    project_id          = paste0("p", project),
    # Project-level targets (used as fallback)
    proj_target         = as.numeric(total_targets[project_id]),
    # Layer bases (same logic as original)
    n_has_questionnaire = n_complete + n_miss_c,
    n_has_cognitive     = n_complete + n_miss_q
  ) %>%
  # Join per-(project,sample) targets from the P8_Split_P6.2_only sheet
  dplyr::left_join(targets_split, by = c("project","sample")) %>%
  dplyr::rename(sample_target = target) %>%
  # p6 adults use ONLY their own confirmed target (N = 44).
  # Preserve the existing project-target fallback for all other samples.
  dplyr::mutate(
    sample_target = dplyr::if_else(project == 6 & sample == "adults",
                                  sample_target, dplyr::coalesce(sample_target, proj_target)),
    force_33 = FALSE
  )

# Percent-of-targets
pctify <- function(n, target) ifelse(is.na(target) | target <= 0, NA_real_, pmin(100, 100*n/target))

progress_pct_split <- progress_counts_split %>%
  dplyr::mutate(
    pct_complete = pctify(n_complete,          sample_target),
    pct_qmiss    = pctify(n_has_questionnaire, sample_target),  # cognitive tests missing (orange)
    pct_cmiss    = pctify(n_has_cognitive,     sample_target)   # questionnaires missing (blue)
  ) %>%
  dplyr::mutate(
    pct_complete = dplyr::if_else(force_33, 33, pct_complete),
    pct_qmiss    = dplyr::if_else(force_33, 33, pct_qmiss),
    pct_cmiss    = dplyr::if_else(force_33, 33, pct_cmiss)
  )

# Axis labels include the new p6 adults subproject.
axis_levels_split <- c(
  axis_labels[paste0("p", 2:5)],
  "p6 children", "p6 adults",
  axis_labels["p7"],
  "p8 children", "p8 adults",
  axis_labels["p9"]
) |> as.character()

plot_df2 <- progress_pct_split %>%
  dplyr::mutate(
    project_lbl = dplyr::case_when(
      project == 6 & sample == "children" ~ "p6 children",
      project == 6 & sample == "adults"   ~ "p6 adults",
      project == 8 & sample == "children" ~ "p8 children",
      project == 8 & sample == "adults"   ~ "p8 adults",
      TRUE                                ~ axis_labels[paste0("p", project)]
    ) |> as.character()
  ) %>%
  dplyr::mutate(project_lbl = factor(project_lbl, levels = axis_levels_split)) %>%
  dplyr::arrange(project, sample)

plot_long2 <- plot_df2 %>%
  tidyr::pivot_longer(
    cols = c(pct_qmiss, pct_cmiss, pct_complete),
    names_to = "layer", values_to = "pct"
  ) %>%
  dplyr::mutate(
    x = factor(as.character(project_lbl), levels = axis_levels_split),
    layer = factor(layer,
                   levels = c("pct_qmiss","pct_cmiss","pct_complete"),
                   labels = c("cognitive tests missing","questionnaires missing","complete"))
  )

p2 <- ggplot(plot_long2, aes(x = x, y = pct)) +
  geom_col(data = plot_long2 %>% dplyr::distinct(x), aes(x = x, y = 100),
           inherit.aes = FALSE, fill = NA, color = outline_color, linewidth = 0.9, width = 0.7) +
  geom_col(aes(fill = layer), width = 0.7, position = position_identity()) +
  geom_hline(yintercept = c(25, 50, 75), linetype = "dashed", linewidth = 0.3, color = refline_color) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = layer_fills, name = NULL) +
  scale_y_continuous(limits = c(0, 110), breaks = c(0,25,50,75,100),
                     labels = c("0","25","50","75","100"),
                     expand = expansion(mult = c(0.02, 0.12))) +
  labs(
    x = NULL,
    y = "Percent of target sample size",
    title = "Completed backbone datasets",
    caption = paste0("Status: ", datestamp)
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.background      = element_rect(fill="white", color=NA),
    plot.background       = element_rect(fill="white", color=NA),
    axis.title.y          = element_blank(),
    legend.position       = "bottom",
    plot.caption.position = "plot",
    plot.caption          = element_text(hjust = 0, size = 8)
  )

# Labels based on COMPLETE
label_df2 <- plot_df2 %>%
  dplyr::mutate(
    label_total_complete = ifelse(force_33, NA_integer_, n_complete),
    pct_for_label        = ifelse(force_33, NA_real_, pctify(n_complete, sample_target))
  ) %>%
  dplyr::transmute(
    x = factor(as.character(project_lbl), levels = axis_levels_split),
    pct_pos = dplyr::case_when(
      force_33             ~ 35,
      is.na(pct_for_label) ~ 5,
      TRUE                 ~ pmin(98, pmax(5, pct_for_label + 2))
    ),
    label = dplyr::case_when(
      force_33             ~ sprintf("%d / ?", n_complete),
      is.na(sample_target) ~ sprintf("%d / ?", n_complete),
      TRUE                 ~ sprintf("%d / %d (%.0f%%)", n_complete, sample_target, pct_for_label)
    )
  )

p2 <- p2 + geom_text(data = label_df2, aes(x = x, y = pct_pos, label = label),
                     inherit.aes = FALSE, hjust = 0, size = 3.4)

ggsave(output_png_split, p2, width = png_width, height = png_height, dpi = png_dpi)
message("Saved plot (p6 children/adults + p8 split): ", normalizePath(output_png_split))
