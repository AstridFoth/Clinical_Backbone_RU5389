#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Demographic plots from CLEANED backbone exports only
# Description:
# Reads the latest dated clean-master XLSX exports from prep03 and produces
#  demographic plots (age density, education density, % women,
# % single, height density, weight density).
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
cat("\014")

ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "dplyr", "forcats", "fs", "glue", "stringr", "ggplot2", "readxl"
))

# ---- Paths -------------------------------------------------------------------

# Resolve the data repository independently of the script's own subfolder.
# BACKBONE_ROOT can override the established K: drive location.
script_directory <- function() {
  source_files <- vapply(sys.frames(), function(frame) {
    p <- frame$ofile
    if (is.null(p) || !length(p) || is.na(p[1])) "" else as.character(p[1])
  }, character(1))
  source_files <- source_files[nzchar(source_files)]
  if (length(source_files)) {
    return(dirname(normalizePath(tail(source_files, 1), winslash = "/", mustWork = FALSE)))
  }
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (length(p) == 1L && !is.na(p) && nzchar(p)) {
      return(dirname(normalizePath(p, winslash = "/", mustWork = FALSE)))
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

resolve_backbone_root <- function(start = script_directory()) {
  override <- Sys.getenv("BACKBONE_ROOT", unset = "")
  if (nzchar(override)) {
    if (!dir.exists(override)) stop("BACKBONE_ROOT does not exist: ", override)
    return(normalizePath(override, winslash = "/", mustWork = TRUE))
  }
  for (location in unique(c(start, getwd()))) {
    location <- normalizePath(location, winslash = "/", mustWork = FALSE)
    repeat {
      if (dir.exists(file.path(location, "01_project_data")) &&
          any(dir.exists(file.path(location, c("02_cleaned", "03_analysis_input"))))) {
        return(location)
      }
      parent <- dirname(location)
      if (identical(parent, location)) break
      location <- parent
    }
  }
  default_root <- "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389"
  if (dir.exists(default_root)) return(default_root)
  stop("Cannot locate Clinical_Backbone_RU5389. Set BACKBONE_ROOT to its root folder.")
}

input_date_from_path <- function(paths) {
  vapply(paths, function(path) {
    hit <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", basename(path))
    if (hit[1] < 0L) return(NA_character_)
    stamp <- regmatches(basename(path), hit)
    parsed <- suppressWarnings(as.Date(stamp, format = "%Y-%m-%d"))
    if (is.na(parsed) || format(parsed, "%Y-%m-%d") != stamp) NA_character_ else stamp
  }, character(1), USE.NAMES = FALSE)
}

latest_dated_file <- function(directory, pattern, input_date = "") {
  files <- list.files(directory, pattern = pattern, full.names = TRUE,
                      recursive = FALSE, ignore.case = TRUE)
  files <- files[!startsWith(basename(files), "~$")]
  dates <- input_date_from_path(files)
  keep <- !is.na(dates)
  if (nzchar(input_date)) keep <- keep & dates == input_date
  files <- files[keep]
  dates <- dates[keep]
  if (!length(files)) {
    stop("No matching dated input in ", directory, ": ", pattern,
         if (nzchar(input_date)) paste0(" (date ", input_date, ")") else "")
  }
  newest <- max(dates)
  hits <- files[dates == newest]
  if (length(hits) != 1L) stop("Ambiguous input for ", newest, ": ", paste(hits, collapse = "; "))
  hits[1]
}

ROOT <- resolve_backbone_root()
INPUT_DATE <- Sys.getenv("BACKBONE_INPUT_DATE", unset = "")

DIR_EXPORT        <- fs::path(ROOT, "02_cleaned")
DIR_ANALYSIS_BASE <- fs::path(ROOT, "out", "internal_data_analysis", "demographics_and_health")
fs::dir_create(DIR_ANALYSIS_BASE)

# Project colors (same as in cleaning script)
vanilla_colors <- c(
  "p2"="#1B9E77","p3"="#D95F02","p4"="#7570B3", "p5"="#E7298A",
  "p6"="#66A61E", "p6 children"="#66A61E","p7"="#E6AB02", "p8 children"="#000000",
  "p8 adults" ="#A6761D", "p9"="#7A7A7A"
)

# ---- Helpers ----------------------------------------------------------------

get_project_col <- function(df) {
  if ("project" %in% names(df)) return("project")
  if ("p" %in% names(df)) return("p")
  NULL
}

# Robust: accepts "8", "p8", "P8 adults", etc.
to_palette_key <- function(x, sample) {
  x0  <- as.character(x)
  x0  <- stringr::str_trim(stringr::str_to_lower(x0))
  num <- stringr::str_match(x0, "p?\\s*(\\d+)")[,2]
  key <- ifelse(is.na(num), NA_character_, paste0("p", num))
  
  # Match your original intent: special handling for p8 + children projects
  is_children_sample <- grepl("child", tolower(sample))
  
  key <- dplyr::case_when(
    key == "p8" & is_children_sample ~ "p8 children",
    key == "p8" & !is_children_sample ~ "p8 adults",
    key == "p6" & is_children_sample ~ "p6 children",
    TRUE ~ key
  )
  
  key
}

coerce_gender <- function(x) {
  x0 <- stringr::str_trim(stringr::str_to_lower(as.character(x)))
  x1 <- dplyr::case_when(
    x0 %in% c("1","female","f","woman","w") ~ "female",
    x0 %in% c("2","male","m","man") ~ "male",
    TRUE ~ NA_character_
  )
  factor(x1, levels = c("female","male"))
}

coerce_education <- function(x) {
  # supports either numeric codes ("0".."10") OR already-labeled text
  lev_labels <- c(
    "no school diploma",
    "primary school",
    "Lower Secondary School Certificate",
    "Intermediate Secondary School Certificate",
    "General Qualification for University Entrance",
    "apprentice",
    "applied science (Fachhochschulabschluss)",
    "diploma",
    "bachelor",
    "master",
    "PhD",
    "other"
  )
  
  x_chr <- stringr::str_trim(as.character(x))
  
  # numeric-coded?
  x_num <- suppressWarnings(as.integer(x_chr))
  if (!all(is.na(x_num))) {
    out <- factor(as.character(x_chr),
                  levels = as.character(0:10) |> c("-oth-"),
                  labels = lev_labels
    )
  } else {
    out <- factor(x_chr, levels = lev_labels)
  }
  
  # match your ordering and remove "other"
  out <- forcats::fct_relevel(
    out,
    "no school diploma","primary school",
    "Lower Secondary School Certificate","Intermediate Secondary School Certificate",
    "General Qualification for University Entrance","apprentice",
    "applied science (Fachhochschulabschluss)",
    "bachelor","diploma","master","PhD","other"
  )
  out <- factor(out, levels = levels(out), ordered = TRUE)
  
  # in your pipeline you drop "other/missing"
  out_chr <- as.character(out)
  out_chr[out_chr %in% c("other","missing","")] <- NA_character_
  factor(out_chr, levels = setdiff(levels(out), "other"), ordered = TRUE)
}

coerce_marital <- function(x) {
  x_chr <- stringr::str_trim(stringr::str_to_lower(as.character(x)))
  # allow numeric codes 0..4 OR labels
  x_chr <- dplyr::case_when(
    x_chr == "0" ~ "single",
    x_chr == "1" ~ "relationship",
    x_chr == "2" ~ "married",
    x_chr == "3" ~ "divorced",
    x_chr == "4" ~ "widowed",
    TRUE ~ x_chr
  )
  ord <- c("single","relationship","married","divorced","widowed")
  factor(x_chr, levels = ord, ordered = TRUE)
}

prepare_demographics_for_plotting <- function(df, sample) {
  out <- df
  
  if (!"age_years" %in% names(out) && "age" %in% names(out)) out$age_years <- out$age
  for (column in intersect(c("age_years", "height", "weight"), names(out))) {
    out[[column]] <- suppressWarnings(as.numeric(gsub(",", ".", as.character(out[[column]]), fixed = TRUE)))
  }
  
  if ("gender" %in% names(out))    out$gender    <- coerce_gender(out$gender)
  if ("education" %in% names(out)) out$education <- coerce_education(out$education)
  if ("maritalstat" %in% names(out)) out$maritalstat <- coerce_marital(out$maritalstat)
  
  proj_col <- get_project_col(out)
  if (!is.null(proj_col)) {
    out$palette_key <- to_palette_key(out[[proj_col]], sample)
  } else {
    out$palette_key <- NA_character_
  }
  
  out
}

percent_nonmissing <- function(x) {
  n <- length(x); nn <- sum(!is.na(x))
  if (n == 0) return(NA_real_)
  100 * nn / n
}

# ---- Plotting ----------------------------------------------------------------

plot_simple_demographics_from_cleaned <- function(df_clean, sample, run_tag = format(Sys.Date(), "%Y-%m-%d")) {
  df_ready <- prepare_demographics_for_plotting(df_clean, sample)
  proj_col <- get_project_col(df_ready)
  if (is.null(proj_col)) {
    message("Simple plots skipped: no project column ('project' or 'p').")
    return(invisible(NULL))
  }
  
  out_dir <- fs::path(DIR_ANALYSIS_BASE, paste0(run_tag, "_", sample), "simple_plots")
  fs::dir_create(out_dir)
  
  # coverage report
  present <- intersect(c("age_years","education","gender"), names(df_ready))
  if (length(present)) {
    message("Non-missing coverage (%):")
    for (v in present) message("  • ", v, ": ", sprintf("%.1f", percent_nonmissing(df_ready[[v]])), "%")
  }
  
  # manual palette if possible
  keys_used <- unique(df_ready$palette_key)
  pal <- vanilla_colors[keys_used]
  use_manual <- any(!is.na(pal))
  
  # 1) Age density overlay
  if ("age_years" %in% names(df_ready)) {
    d_age <- df_ready %>% dplyr::filter(!is.na(age_years), !is.na(.data[[proj_col]]))
    if (nrow(d_age)) {
      p_age <- ggplot2::ggplot(d_age, ggplot2::aes(x = age_years, color = palette_key, fill = palette_key)) +
        ggplot2::geom_density(ggplot2::aes(y = after_stat(density * 100)), alpha = 0.2, adjust = 1) +
        ggplot2::labs(title = paste0(sample, ": Age distribution by project"), x = "Age (years)", y = "Percentage") +
        { if (use_manual) ggplot2::scale_color_manual(values = vanilla_colors, drop = FALSE) else ggplot2::scale_color_discrete() } +
        { if (use_manual) ggplot2::scale_fill_manual(values = vanilla_colors, drop = FALSE) else ggplot2::scale_fill_discrete() } +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.title = ggplot2::element_blank())
      ggplot2::ggsave(fs::path(out_dir, "age_density_percentage.png"), p_age, width = 8, height = 5.2, dpi = 150)
    }
  }
  
  # 2) Education density overlay
  if ("education" %in% names(df_ready)) {
    d_edu <- df_ready %>% dplyr::filter(!is.na(education), !is.na(.data[[proj_col]]))
    if (nrow(d_edu)) {
      drop_levels <- c("no school diploma", "primary school")
      d_edu <- d_edu %>%
        dplyr::mutate(education = droplevels(forcats::fct_drop(education, only = drop_levels))) %>%
        dplyr::filter(!is.na(education))
      if (nrow(d_edu)) {
        edu_levels <- levels(d_edu$education)
        d_edu <- d_edu %>% dplyr::mutate(edu_rank = as.integer(education))
        
        p_edu_den <- ggplot2::ggplot(d_edu, ggplot2::aes(x = edu_rank, color = palette_key, fill = palette_key)) +
          ggplot2::geom_density(ggplot2::aes(y = after_stat(density * 100)), alpha = 0.2, adjust = 1) +
          ggplot2::scale_x_continuous(
            breaks = seq_along(edu_levels),
            labels = edu_levels,
            expand = ggplot2::expansion(mult = c(0.02, 0.02))
          ) +
          ggplot2::labs(title = paste0(sample, ": Education (ordinal) density by project"), x = "Education", y = "Percentage") +
          { if (use_manual) ggplot2::scale_color_manual(values = vanilla_colors, drop = FALSE) else ggplot2::scale_color_discrete() } +
          { if (use_manual) ggplot2::scale_fill_manual(values = vanilla_colors, drop = FALSE) else ggplot2::scale_fill_discrete() } +
          ggplot2::theme_bw(base_size = 13) +
          ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
            legend.title = ggplot2::element_blank()
          )
        ggplot2::ggsave(fs::path(out_dir, "education_density_percentage.png"), p_edu_den, width = 9.5, height = 6.4, dpi = 150)
      }
    }
  }
  
  # 3) % Women bars
  if ("gender" %in% names(df_ready)) {
    d_g <- df_ready %>% dplyr::filter(!is.na(.data[[proj_col]])) %>%
      dplyr::mutate(gender = as.character(gender)) %>%
      dplyr::filter(gender %in% c("female","male"))
    if (nrow(d_g)) {
      women_pct <- d_g %>%
        dplyr::group_by(.data[[proj_col]]) %>%
        dplyr::summarise(
          N = dplyr::n(),
          n_female = sum(gender == "female"),
          pct_female = ifelse(N > 0, 100 * n_female / N, NA_real_),
          palette_key = dplyr::first(palette_key),
          .groups = "drop"
        )
      
      women_pct[[proj_col]] <- factor(women_pct[[proj_col]], levels = women_pct[[proj_col]])
      
      p_w <- ggplot2::ggplot(women_pct, ggplot2::aes(x = .data[[proj_col]], y = pct_female, fill = palette_key)) +
        ggplot2::geom_col(width = 0.75) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", pct_female)), vjust = -0.3, size = 3) +
        ggplot2::labs(title = paste0(sample, ": % Women by project"), x = "Project", y = "Percentage women") +
        { if (use_manual) ggplot2::scale_fill_manual(values = vanilla_colors, drop = FALSE) else ggplot2::scale_fill_discrete() } +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "none")
      
      ggplot2::ggsave(fs::path(out_dir, "women_percentage_by_project.png"), p_w, width = 8, height = 5.2, dpi = 150)
    }
  }
  
  # 4) % Single bars (divorced → single)
  if ("maritalstat" %in% names(df_ready)) {
    d_m <- df_ready %>% dplyr::filter(!is.na(.data[[proj_col]]), !is.na(maritalstat))
    if (nrow(d_m)) {
      d_m <- d_m %>%
        dplyr::mutate(
          mstat = as.character(maritalstat),
          mstat = dplyr::if_else(mstat == "divorced", "single", mstat),
          is_single = mstat == "single"
        )
      
      single_pct <- d_m %>%
        dplyr::group_by(.data[[proj_col]]) %>%
        dplyr::summarise(
          N = dplyr::n(),
          n_single = sum(is_single, na.rm = TRUE),
          pct_single = ifelse(N > 0, 100 * n_single / N, NA_real_),
          palette_key = dplyr::first(palette_key),
          .groups = "drop"
        )
      
      single_pct[[proj_col]] <- factor(single_pct[[proj_col]], levels = single_pct[[proj_col]])
      
      p_s <- ggplot2::ggplot(single_pct, ggplot2::aes(x = .data[[proj_col]], y = pct_single, fill = palette_key)) +
        ggplot2::geom_col(width = 0.75) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", pct_single)), vjust = -0.3, size = 3) +
        ggplot2::labs(title = paste0(sample, ": % Single ('divorced' + 'single') by project"), x = "Project", y = "Percentage single") +
        { if (use_manual) ggplot2::scale_fill_manual(values = vanilla_colors, drop = FALSE) else ggplot2::scale_fill_discrete() } +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "none")
      
      ggplot2::ggsave(fs::path(out_dir, "single_percentage_by_project.png"), p_s, width = 8, height = 5.2, dpi = 150)
    }
  }
  
  # 5) Height density overlay
  if ("height" %in% names(df_ready)) {
    d_h <- df_ready %>% dplyr::filter(!is.na(height), !is.na(.data[[proj_col]]))
    if (nrow(d_h)) {
      p_h <- ggplot2::ggplot(d_h, ggplot2::aes(x = height, color = palette_key, fill = palette_key)) +
        ggplot2::geom_density(ggplot2::aes(y = after_stat(density * 100)), alpha = 0.2, adjust = 1) +
        ggplot2::labs(title = paste0(sample, ": Height distribution by project"), x = "Height (cm)", y = "Percentage") +
        { if (use_manual) ggplot2::scale_color_manual(values = vanilla_colors, drop = FALSE) else ggplot2::scale_color_discrete() } +
        { if (use_manual) ggplot2::scale_fill_manual(values = vanilla_colors, drop = FALSE) else ggplot2::scale_fill_discrete() } +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.title = ggplot2::element_blank())
      ggplot2::ggsave(fs::path(out_dir, "height_density_percentage.png"), p_h, width = 8, height = 5.2, dpi = 150)
    }
  }
  
  # 6) Weight density overlay
  if ("weight" %in% names(df_ready)) {
    d_w <- df_ready %>% dplyr::filter(!is.na(weight), !is.na(.data[[proj_col]]))
    if (nrow(d_w)) {
      p_wt <- ggplot2::ggplot(d_w, ggplot2::aes(x = weight, color = palette_key, fill = palette_key)) +
        ggplot2::geom_density(ggplot2::aes(y = after_stat(density * 100)), alpha = 0.2, adjust = 1) +
        ggplot2::labs(title = paste0(sample, ": Weight distribution by project"), x = "Weight (kg)", y = "Percentage") +
        { if (use_manual) ggplot2::scale_color_manual(values = vanilla_colors, drop = FALSE) else ggplot2::scale_color_discrete() } +
        { if (use_manual) ggplot2::scale_fill_manual(values = vanilla_colors, drop = FALSE) else ggplot2::scale_fill_discrete() } +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.title = ggplot2::element_blank())
      ggplot2::ggsave(fs::path(out_dir, "weight_density_percentage.png"), p_wt, width = 8, height = 5.2, dpi = 150)
    }
  }
  
  message("Saved plots to: ", out_dir)
  invisible(TRUE)
}

read_clean_master <- function(sample) {
  folder <- file.path(DIR_EXPORT, sample)
  if (!dir.exists(folder)) stop("Cleaned sample folder not found: ", folder)
  pattern <- paste0("^[0-9]{4}-[0-9]{2}-[0-9]{2}_", sample, "_clean_master\\.xlsx$")
  path <- latest_dated_file(folder, pattern, input_date = INPUT_DATE)
  message("Reading clean master: ", path)
  data <- as.data.frame(readxl::read_excel(path, .name_repair = "minimal"))
  attr(data, "input_file") <- path
  attr(data, "input_date") <- input_date_from_path(path)
  data
}

# ---- Main --------------------------------------------------------------------

SAMPLES_TO_PLOT <- c("adults")   # e.g. c("adults", "adolescents", "children_parents")

for (sample in SAMPLES_TO_PLOT) {
  data <- read_clean_master(sample)
  plot_simple_demographics_from_cleaned(data, sample, run_tag = attr(data, "input_date"))
}
