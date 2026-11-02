# ==============================================================================
# Exploratory Data Analysis & Sanity Check Visualization Script (FHS & CTQ)
# ==============================================================================

# 1. Load required libraries
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  readxl,
  dplyr,
  tidyr,
  ggplot2,
  stringr,
  readr,
  purrr
)

# ==============================================================================
# 2. DEFINE FILE PATHS
# ==============================================================================

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

base_path <- resolve_backbone_root()
analysis_dir <- file.path(base_path, "03_analysis_input")
INPUT_DATE <- Sys.getenv("BACKBONE_INPUT_DATE", unset = "")

# FHS and CTQ use the full COMPLETE exports, before loading-based filtering.
# Match both workbooks to the same export date; never mix pipeline runs.
input_items <- latest_dated_file(
  analysis_dir,
  "^[0-9]{4}-[0-9]{2}-[0-9]{2}_adults_adolescents_complete_items\\.xlsx$",
  input_date = INPUT_DATE
)
RUN_TAG <- input_date_from_path(input_items)
input_sub <- latest_dated_file(
  analysis_dir,
  "^[0-9]{4}-[0-9]{2}-[0-9]{2}_adults_adolescents_complete_subscales\\.xlsx$",
  input_date = RUN_TAG
)
output_dir <- file.path(
  base_path, "out", "internal_data_analysis", "risk_factors",
  paste0(RUN_TAG, "_adults_adolescents")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 3. LOAD AND JOIN DATA
# ==============================================================================

read_combined_export <- function(path) {
  sheets <- readxl::excel_sheets(path)
  combined <- sheets[tolower(sheets) == "combined"]
  if (length(combined) == 1L) {
    data <- readxl::read_excel(path, sheet = combined, .name_repair = "minimal")
  } else {
    # If a workbook has only sample sheets, combine them once.
    sample_sheets <- sheets[tolower(sheets) %in% c("adults", "adolescents")]
    if (!length(sample_sheets)) stop("No combined or sample sheets in: ", path)
    data <- dplyr::bind_rows(lapply(sample_sheets, function(sheet) {
      part <- readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
      if (!"sample" %in% names(part)) part$sample <- tolower(sheet)
      part
    }))
  }
  if (!"sample" %in% names(data)) stop("Missing sample column in: ", path)
  if (anyDuplicated(names(data))) stop("Duplicate column names in: ", path)
  message("Read ", nrow(data), " rows from: ", path)
  data
}

message("Loading COMPLETE workbooks from ", RUN_TAG, "...")
df_items <- read_combined_export(input_items)
df_sub <- read_combined_export(input_sub)

# Identify the participant ID column explicitly
id_candidates <- c(
  "vp_id",
  "vp",
  "vpid",
  "participant_id",
  "participant",
  "id"
)

id_col <- id_candidates[
  id_candidates %in% names(df_items) &
    id_candidates %in% names(df_sub)
][1]

if (is.na(id_col)) {
  stop(
    "No shared participant ID column was found. Expected one of: ",
    paste(id_candidates, collapse = ", ")
  )
}

# Include sample in the join if it exists in both files
join_keys <- id_col

if ("sample" %in% names(df_items) &&
    "sample" %in% names(df_sub)) {
  join_keys <- c("sample", id_col)
}
if ("project" %in% names(df_items) && "project" %in% names(df_sub)) {
  join_keys <- c(join_keys, "project")
}

message(
  "Using join key(s): ",
  paste(join_keys, collapse = ", ")
)

# Harmonize join-key types
df_items <- df_items %>%
  mutate(across(all_of(join_keys), as.character))

df_sub <- df_sub %>%
  mutate(across(all_of(join_keys), as.character))

# Count the number of observations per participant
key_counts_items <- df_items %>%
  count(
    across(all_of(join_keys)),
    name = "n_items"
  )

key_counts_sub <- df_sub %>%
  count(
    across(all_of(join_keys)),
    name = "n_subscales"
  )

duplicate_report <- full_join(
  key_counts_items,
  key_counts_sub,
  by = join_keys
) %>%
  mutate(
    n_items = replace_na(n_items, 0L),
    n_subscales = replace_na(n_subscales, 0L)
  ) %>%
  filter(n_items > 1 | n_subscales > 1)

if (nrow(duplicate_report) > 0) {
  message("Repeated participant IDs found:")
  print(duplicate_report, n = Inf)
  readr::write_csv(duplicate_report, file.path(output_dir, "duplicate_join_keys.csv"))
}

has_duplicates <- nrow(duplicate_report) > 0

# Check whether both files contain the same participant rows
# in exactly the same order
same_key_order <-
  nrow(df_items) == nrow(df_sub) &&
  identical(
    as.data.frame(df_items[, join_keys, drop = FALSE]),
    as.data.frame(df_sub[, join_keys, drop = FALSE])
  )

# Keep all item-level columns, including the FHS variables.
# Add only genuinely new columns from the subscale-level file.
sub_new_cols <- setdiff(
  names(df_sub),
  names(df_items)
)

if (!has_duplicates) {
  
  # Standard one-to-one join
  df <- full_join(
    df_items,
    df_sub %>%
      select(
        all_of(join_keys),
        all_of(sub_new_cols)
      ),
    by = join_keys
  )
  
} else if (same_key_order) {
  
  # Repeated IDs occur in both exports in the same row order.
  # Preserve all observations and pair corresponding source rows.
  message(
    "Repeated IDs are aligned across both files. ",
    "Joining corresponding source rows."
  )
  
  df <- df_items %>%
    mutate(.source_row = row_number()) %>%
    left_join(
      df_sub %>%
        mutate(.source_row = row_number()) %>%
        select(
          .source_row,
          all_of(join_keys),
          all_of(sub_new_cols)
        ),
      by = c(".source_row", join_keys)
    ) %>%
    select(-.source_row)
  
} else {
  
  stop(
    "Repeated participant IDs were found, but the participant rows are not ",
    "aligned across the two files. A further shared identifier, such as a ",
    "submission timestamp, is required to match these observations safely."
  )
}

message(
  "Joined dataset contains ",
  nrow(df),
  " rows and ",
  ncol(df),
  " columns."
)

# ==============================================================================
# 4. CLEAN DATA
# ==============================================================================

clean_num <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  
  suppressWarnings(
    as.numeric(
      str_replace_all(
        as.character(x),
        ",",
        "."
      )
    )
  )
}

# Replace textual missing-value codes without coercing numeric columns
df <- df %>%
  mutate(
    across(
      where(is.character),
      ~ replace(
        .x,
        .x %in% c("nodiagn", "nodiagntreated"),
        NA_character_
      )
    )
  ) %>%
  mutate(
    across(
      matches(
        "(?i)(num_|prop_|relatives|siblings|children|score|ctq)"
      ),
      clean_num
    )
  )

# ==============================================================================
# 5. PLOT THEME
# ==============================================================================

theme_pres <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "gray30"
    ),
    axis.title = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(
      face = "bold"
    )
  )

# ==============================================================================
# 6. PLOT GENERATION
# ==============================================================================

# ------------------------------------------------------------------------------
# PLOT 1: FAMILY DEMOGRAPHICS AND QC FLAGS
# ------------------------------------------------------------------------------

p1_cols <- intersect(
  c(
    "siblings",
    "children",
    "ownpsychdisorder",
    "qc_fhs_duplicate_vp_id",
    "qc_fhs_open_text_requires_review",
    "qc_fhs_own_diagnosis_other_requires_review"
  ),
  names(df)
)

if (length(p1_cols) == 0) {
  
  message(
    "Plot 1 skipped: no demographic or FHS quality-control variables were found."
  )
  
} else {
  
  message(
    "Plot 1 uses: ",
    paste(p1_cols, collapse = ", ")
  )
  
  p1_data <- df %>%
    select(all_of(p1_cols)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    drop_na(Value)
  
  if (nrow(p1_data) == 0) {
    
    message(
      "Plot 1 skipped: the selected variables contain no observations."
    )
    
  } else {
    
    p1 <- ggplot(
      p1_data,
      aes(
        x = factor(Value),
        fill = Variable
      )
    ) +
      geom_bar(
        show.legend = FALSE,
        fill = "#2c3e50"
      ) +
      facet_wrap(
        ~ Variable,
        scales = "free",
        ncol = 3
      ) +
      labs(
        title = "Demographics & Quality Control Flags Overview",
        subtitle = "Counts of categories and flag indicators across the dataset",
        x = "Category / Response Value",
        y = "Count (N)"
      ) +
      theme_pres
    
    ggsave(
      file.path(
        output_dir,
        "01_demographics_and_qc_flags.png"
      ),
      plot = p1,
      width = 10,
      height = 6,
      dpi = 300
    )
  }
}

# ------------------------------------------------------------------------------
# PLOT 2: FAMILY HISTORY SCREEN – RELATIVES OVERVIEW
# ------------------------------------------------------------------------------

p2_cols <- c(
  "fhs_parents_with_diagnosis",
  "fhs_siblings_with_diagnosis",
  "fhs_children_with_diagnosis",
  "fhs_relatives_with_diagnosis"
)

missing_p2_cols <- setdiff(
  p2_cols,
  names(df)
)

if (length(missing_p2_cols) > 0) {
  
  message(
    "Plot 2 skipped. The following required FHS columns are missing: ",
    paste(missing_p2_cols, collapse = ", ")
  )
  
  available_fhs_cols <- names(df)[
    str_detect(
      names(df),
      regex("fhs", ignore_case = TRUE)
    )
  ]
  
  if (length(available_fhs_cols) > 0) {
    message(
      "Available FHS columns: ",
      paste(available_fhs_cols, collapse = ", ")
    )
  }
  
} else {
  
  p2_data <- df %>%
    select(all_of(p2_cols)) %>%
    mutate(across(everything(), clean_num)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Relative_Group",
      values_to = "Count"
    ) %>%
    drop_na(Count) %>%
    mutate(
      Relative_Group = str_replace_all(
        Relative_Group,
        "fhs_|_with_diagnosis",
        ""
      ) %>%
        str_to_title()
    )
  
  if (nrow(p2_data) == 0) {
    
    message(
      "Plot 2 skipped: the selected FHS variables contain no numeric observations."
    )
    
  } else {
    
    p2 <- ggplot(
      p2_data,
      aes(
        x = Relative_Group,
        y = Count,
        fill = Relative_Group
      )
    ) +
      geom_boxplot(
        alpha = 0.7,
        outlier.color = "red",
        show.legend = FALSE
      ) +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 18,
        size = 3,
        color = "darkred"
      ) +
      scale_fill_brewer(
        palette = "Set2"
      ) +
      labs(
        title = "Affected Relatives by Family Degree",
        subtitle = "Boxplots with mean (red diamond) and outliers (red dots)",
        x = "Family Group",
        y = "Number of Affected Relatives"
      ) +
      theme_pres
    
    ggsave(
      file.path(
        output_dir,
        "02_affected_relatives_by_degree.png"
      ),
      plot = p2,
      width = 8,
      height = 5,
      dpi = 300
    )
  }
}

# ------------------------------------------------------------------------------
# PLOT 3: PROPORTION OF RELATIVES WITH SPECIFIC DISORDERS
# ------------------------------------------------------------------------------

prop_cols <- names(df)[
  str_detect(
    names(df),
    "^fhs_prop_relatives_with_"
  )
]

if (length(prop_cols) == 0) {
  
  message(
    "Plot 3 skipped: no FHS disorder-proportion variables were found."
  )
  
} else {
  
  message(
    "Plot 3 uses: ",
    paste(prop_cols, collapse = ", ")
  )
  
  p3_data <- df %>%
    select(all_of(prop_cols)) %>%
    mutate(across(everything(), clean_num)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Disorder",
      values_to = "Proportion"
    ) %>%
    drop_na(Proportion) %>%
    mutate(
      Disorder = str_replace(
        Disorder,
        "fhs_prop_relatives_with_",
        ""
      ) %>%
        str_replace_all("_", " ") %>%
        str_to_title()
    ) %>%
    group_by(Disorder) %>%
    summarise(
      Mean_Prop = mean(Proportion, na.rm = TRUE),
      SE = sd(Proportion, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  if (nrow(p3_data) == 0) {
    
    message(
      "Plot 3 skipped: the selected variables contain no numeric observations."
    )
    
  } else {
    
    p3 <- ggplot(
      p3_data,
      aes(
        x = reorder(Disorder, Mean_Prop),
        y = Mean_Prop
      )
    ) +
      geom_col(
        fill = "#3498db",
        alpha = 0.85,
        width = 0.7
      ) +
      geom_errorbar(
        aes(
          ymin = Mean_Prop - SE,
          ymax = Mean_Prop + SE
        ),
        width = 0.2,
        color = "black"
      ) +
      coord_flip() +
      labs(
        title = "Proportion of Relatives Affected by Disorder Type",
        subtitle = "Mean proportion across sample (error bars: ± SE)",
        x = "Disorder Type",
        y = "Mean Proportion of Affected Relatives"
      ) +
      theme_pres
    
    ggsave(
      file.path(
        output_dir,
        "03_proportions_by_disorder.png"
      ),
      plot = p3,
      width = 8,
      height = 6,
      dpi = 300
    )
  }
}

# ------------------------------------------------------------------------------
# PLOT 4: SELF-REPORTED DIAGNOSES AND TREATMENT COUNTS
# ------------------------------------------------------------------------------

p4_cols <- intersect(
  c(
    "fhs_num_diagnoses_self",
    "fhs_num_treated_diagnoses_self"
  ),
  names(df)
)

if (length(p4_cols) == 0) {
  
  message(
    "Plot 4 skipped: no self-diagnosis count variables were found."
  )
  
} else {
  
  message(
    "Plot 4 uses: ",
    paste(p4_cols, collapse = ", ")
  )
  
  p4_data <- df %>%
    select(all_of(p4_cols)) %>%
    mutate(across(everything(), clean_num)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Type",
      values_to = "Count"
    ) %>%
    drop_na(Count) %>%
    mutate(
      Type = if_else(
        str_detect(Type, "treated"),
        "Treated Diagnoses",
        "Total Diagnoses"
      )
    )
  
  if (nrow(p4_data) == 0) {
    
    message(
      "Plot 4 skipped: the selected variables contain no numeric observations."
    )
    
  } else {
    
    p4 <- ggplot(
      p4_data,
      aes(
        x = factor(Count),
        fill = Type
      )
    ) +
      geom_bar(
        position = "dodge",
        alpha = 0.85
      ) +
      scale_fill_manual(
        values = c(
          "Total Diagnoses" = "#8e44ad",
          "Treated Diagnoses" = "#16a085"
        )
      ) +
      labs(
        title = "Participant Self-Diagnoses Count Distribution",
        subtitle = "Comparison of total self-reported and treated diagnoses",
        x = "Number of Diagnoses",
        y = "Participant Count (N)",
        fill = "Category"
      ) +
      theme_pres
    
    ggsave(
      file.path(
        output_dir,
        "04_self_diagnoses_distribution.png"
      ),
      plot = p4,
      width = 8,
      height = 5,
      dpi = 300
    )
  }
}

# ------------------------------------------------------------------------------
# PLOT 5A: CTQ SUBSCALE MEANS
# ------------------------------------------------------------------------------

# Select scored subscales from the subscale workbook, never CTQ item columns.
ctq_sub_cols <- names(df_sub)[
  str_detect(names(df_sub), "(?i)^score_ctq__") &
    !str_detect(names(df_sub), "(?i)^score_ctq__total(?:_(sum|mean))?$")
]
# If both metrics exist, show the sum once.
ctq_sub_cols <- ctq_sub_cols[!vapply(ctq_sub_cols, function(column) {
  grepl("_mean$", column, ignore.case = TRUE) &&
    sub("_mean$", "_sum", column, ignore.case = TRUE) %in% ctq_sub_cols
}, logical(1))]

if (length(ctq_sub_cols) == 0) {
  
  message(
    "Plot 5A skipped: no CTQ subscale variables were found."
  )
  
} else {
  
  message(
    "Plot 5A uses: ",
    paste(ctq_sub_cols, collapse = ", ")
  )
  
  p5a_data <- df %>%
    select(all_of(ctq_sub_cols)) %>%
    mutate(across(everything(), clean_num)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Subscale",
      values_to = "Score"
    ) %>%
    drop_na(Score) %>%
    mutate(
      Subscale = str_replace_all(
        Subscale,
        "(?i)^score_ctq__|_(sum|mean)$",
        ""
      ) %>%
        str_replace_all("_", " ") %>%
        str_to_title()
    ) %>%
    group_by(Subscale) %>%
    summarise(
      Mean_Score = mean(Score, na.rm = TRUE),
      SD = sd(Score, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(p5a_data) == 0) {
    
    message(
      "Plot 5A skipped: the selected variables contain no numeric observations."
    )
    
  } else {
    
    p5a <- ggplot(
      p5a_data,
      aes(
        x = reorder(Subscale, Mean_Score),
        y = Mean_Score,
        fill = Subscale
      )
    ) +
      geom_col(
        alpha = 0.85,
        show.legend = FALSE
      ) +
      geom_errorbar(
        aes(
          ymin = Mean_Score - SD,
          ymax = Mean_Score + SD
        ),
        width = 0.2
      ) +
      geom_text(
        aes(
          label = round(Mean_Score, 1)
        ),
        vjust = -0.5,
        fontface = "bold"
      ) +
      scale_fill_brewer(
        palette = "Spectral"
      ) +
      labs(
        title = "Childhood Trauma Questionnaire (CTQ) Subscale Means",
        subtitle = "Mean scores across subscales (error bars: ± SD)",
        x = "CTQ Subscale",
        y = "Mean Score"
      ) +
      theme_pres
    
    ggsave(
      file.path(
        output_dir,
        "05a_ctq_subscale_means.png"
      ),
      plot = p5a,
      width = 8,
      height = 5,
      dpi = 300
    )
  }
}

# ------------------------------------------------------------------------------
# PLOT 5B: CTQ TOTAL SCORE DISTRIBUTION
# ------------------------------------------------------------------------------

ctq_total_col <- names(df)[
  str_detect(
    names(df),
    "(?i)^score_ctq$|^score_ctq__total(?:_sum)?$|^ctq_total$"
  )
]

if (length(ctq_total_col) == 0) {
  
  message(
    "Plot 5B skipped: no CTQ total-score variable was found."
  )
  
} else {
  
  selected_ctq_total <- ctq_total_col[1]
  
  message(
    "Plot 5B uses: ",
    selected_ctq_total
  )
  
  p5b_data <- df %>%
    transmute(
      CTQ_Total = clean_num(.data[[selected_ctq_total]])
    ) %>%
    drop_na(CTQ_Total)
  
  if (nrow(p5b_data) == 0) {
    
    message(
      "Plot 5B skipped: the CTQ total-score variable contains no numeric observations."
    )
    
  } else {
    
    p5b <- ggplot(
      p5b_data,
      aes(x = CTQ_Total)
    ) +
      geom_histogram(
        binwidth = 3,
        fill = "#e67e22",
        color = "white",
        alpha = 0.8
      ) +
      geom_density(
        aes(y = after_stat(count) * 3),
        color = "black",
        linewidth = 1
      ) +
      labs(
        title = "CTQ Total Score Distribution",
        subtitle = "Histogram overlaid with density line for total trauma load",
        x = "CTQ Total Score",
        y = "Frequency"
      ) +
      theme_pres
    
    ggsave(
      file.path(
        output_dir,
        "05b_ctq_total_score_distribution.png"
      ),
      plot = p5b,
      width = 8,
      height = 5,
      dpi = 300
    )
  }
}

message(
  "Execution complete. All available descriptive plots were saved to: ",
  output_dir
)