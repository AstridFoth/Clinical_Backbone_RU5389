# Tool_01_P7_diagnosis_extraction.R
# Project 7: current DIPS diagnoses, exclusive RU groups and grouping IDs.
# Revision: 2026-10-09_skip_empty_ids
# Run this script BEFORE prep_03_Backbone_Questionnaire_Data_Cleaning.R.
#
# Current diagnosis = 1 or 3. Lifetime only (2) does not establish a current
# diagnosis. Per the requested rule, a row with only missing values also belongs
# to No_Diagnosis; missingness is retained in Participant_assignments.
# Priority: psychosis -> anxiety -> obsession -> depression -> substance ->
#           PTSD -> rest. The priority is a study-specific grouping rule.
# Diagnosis_Group in the grouping workbook belongs to the With_Diagnosis ID
# on the SAME row. No_Diagnosis is an independent, compact list of IDs.
# Duplicate source IDs are combined using any current diagnosis across rows.
# No individual is counted twice. All original source row numbers are retained.
# Rows with an empty ID (including whitespace-only cells) are skipped before
# diagnosis values are parsed, regardless of the contents of those cells.

ROOT <- "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389"
DIAGNOSIS_SHEET <- NULL  # NULL = detect the unique sheet with ID + ICD headers.

GROUP_PRIORITY <- c(
  "psychosis-group", "anxiety-group", "obsession-group", "depression-group",
  "substance-group", "ptsd-group", "rest-group"
)

ensure_p7_packages <- function() {
  needed <- c("readxl", "openxlsx", "ggplot2")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
  unavailable <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(unavailable)) stop("Required R packages are unavailable: ", paste(unavailable, collapse = ", "))
}

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(gsub("[[:space:]\u00a0]+", " ", x))
}

header_token <- function(x) gsub("[^a-z0-9]", "", tolower(clean_text(x)))

normalize_p7_id <- function(x) {
  x <- clean_text(x)
  sub("\\.0+$", "", x)
}

read_sheet_matrix <- function(filepath, sheet, n_max = Inf) {
  # Anchor at A1 so row numbers and column positions are actual Excel positions,
  # even when the workbook starts with empty rows or columns.
  last_row <- if (is.finite(n_max)) as.integer(n_max) else NA_integer_
  as.matrix(suppressMessages(readxl::read_excel(
    filepath, sheet = sheet, col_names = FALSE, col_types = "text",
    .name_repair = "minimal", na = character(0),
    range = cellranger::cell_limits(c(1L, 1L), c(last_row, NA_integer_))
  )))
}

icd_code <- function(x) {
  z <- toupper(gsub("[[:space:]\u00a0]+", "", clean_text(x)))
  z[!grepl("^F[0-9]{2}(\\.[0-9]{1,3})?$", z)] <- NA_character_
  z
}

detect_dips_header <- function(raw) {
  if (!nrow(raw) || !ncol(raw)) return(NULL)
  scan_rows <- seq_len(min(100L, nrow(raw)))
  id_tokens <- c("id", "vpid", "participantid", "subjectid")
  hits <- lapply(scan_rows, function(r) {
    ids <- which(header_token(raw[r, ]) %in% id_tokens)
    codes <- sum(!is.na(icd_code(raw[r, ])))
    if (length(ids) == 1L && codes >= 8L) list(row = r, id_col = ids, codes = codes) else NULL
  })
  hits <- Filter(Negate(is.null), hits)
  if (!length(hits)) return(NULL)
  best <- which.max(vapply(hits, function(x) x$codes, integer(1)))
  hits[[best]]
}

select_dips_sheet <- function(filepath, requested = NULL) {
  sheets <- readxl::excel_sheets(filepath)
  if (!is.null(requested)) {
    if (!requested %in% sheets) stop("Diagnosis sheet not found: ", requested)
    sheets <- requested
  }
  headers <- lapply(sheets, function(sh) detect_dips_header(read_sheet_matrix(filepath, sh, 100L)))
  suitable <- which(!vapply(headers, is.null, logical(1)))
  if (!length(suitable)) stop("No sheet contains an ID header and at least eight ICD diagnosis headers.")
  if (length(suitable) > 1L) stop(
    "Several diagnosis sheets were found: ", paste(sheets[suitable], collapse = ", "),
    ". Set DIAGNOSIS_SHEET explicitly to prevent silently omitting a sheet."
  )
  list(sheet = sheets[suitable], header = headers[[suitable]])
}

english_diagnosis_name <- function(code, group) {
  if (is.na(code)) return("Psychotic symptoms")
  fixed <- c(
    "F41.0" = "Panic disorder", "F40.00" = "Agoraphobia",
    "F40.10" = "Social anxiety disorder", "F40.2" = "Specific phobia",
    "F40.218" = "Specific phobia: animals", "F40.228" = "Specific phobia: natural environment",
    "F40.230" = "Specific phobia: blood", "F40.231" = "Specific phobia: injections",
    "F40.232" = "Specific phobia: medical procedures", "F40.233" = "Specific phobia: injury",
    "F40.248" = "Specific phobia: situational", "F40.298" = "Specific phobia: other",
    "F41.1" = "Generalized anxiety disorder", "F93.0" = "Separation anxiety disorder",
    "F31.81" = "Bipolar II disorder", "F34.1" = "Dysthymia",
    "F42" = "Obsessive-compulsive disorder", "F45.22" = "Body dysmorphic disorder",
    "F43.10" = "Post-traumatic stress disorder", "F43.0" = "Acute stress disorder",
    "F45.1" = "Somatic symptom disorder", "F45.21" = "Illness anxiety disorder",
    "F50.01" = "Anorexia nervosa: restrictive", "F50.02" = "Anorexia nervosa: binge/purge",
    "F50.2" = "Bulimia nervosa", "F50.8" = "Binge-eating disorder",
    "F51.01" = "Insomnia", "F51.11" = "Hypersomnia", "F63.0" = "Gambling disorder",
    "F90.2" = "ADHD: combined", "F90.0" = "ADHD: inattentive",
    "F90.1" = "ADHD: hyperactive/impulsive",
    # Source label is Borderline. Preserve that label rather than reinterpret
    # the supplied F63.3 code; this column belongs to rest-group either way.
    "F63.3" = "Borderline (source label)"
  )
  if (code %in% names(fixed)) return(unname(fixed[code]))
  if (grepl("^F31\\.", code)) return(paste0(
    if (group == "psychosis-group") "Bipolar I: manic episode" else "Bipolar I: depressive episode",
    " (", code, ")"
  ))
  if (grepl("^F32\\.", code)) return(paste0("Depression: single episode (", code, ")"))
  if (grepl("^F33\\.", code)) return(paste0("Depression: recurrent episodes (", code, ")"))
  substance <- c(
    "F10" = "Alcohol", "F11" = "Opioids", "F12" = "Cannabis",
    "F13" = "Sedatives, hypnotics or anxiolytics", "F14" = "Stimulants (F14 section)",
    "F15" = "Caffeine (source section)", "F16" = "Hallucinogens",
    "F18" = "Inhalants", "F19" = "Other substances"
  )
  if (substr(code, 1L, 3L) %in% names(substance)) return(paste0(
    unname(substance[substr(code, 1L, 3L)]), " (", code, ")"
  ))
  paste0("Other diagnosis (", code, ")")
}

build_diagnosis_mapping <- function(raw, header, sheet) {
  codes <- icd_code(raw[header$row, ])
  header_rows <- seq.int(max(1L, header$row - 3L), header$row)
  source_headers <- vapply(seq_len(ncol(raw)), function(j) {
    paste(unique(clean_text(raw[header_rows, j])[nzchar(clean_text(raw[header_rows, j]))]), collapse = " | ")
  }, character(1))
  psych <- grepl("psychoticsymptoms|psychotischesymptome", header_token(source_headers))
  columns <- which(!is.na(codes) | psych)
  if (!any(psych)) stop("The 'Psychotic symptoms' column was not found in the diagnosis headers.")
  manic_codes <- c("F31.11", "F31.12", "F31.13", "F31.2", "F31.73", "F31.74")
  depressive_codes <- c("F31.31", "F31.32", "F31.4", "F31.5", "F31.75", "F31.76", "F31.81", "F34.1")
  groups <- rep("rest-group", ncol(raw))
  groups[codes %in% manic_codes | psych] <- "psychosis-group"
  groups[!is.na(codes) & grepl("^F4[01]([.]|$)", codes) | codes %in% "F93.0"] <- "anxiety-group"
  groups[codes %in% "F42" | !is.na(codes) & grepl("^F42[.]", codes)] <- "obsession-group"
  groups[codes %in% depressive_codes | !is.na(codes) & grepl("^F3[23]([.]|$)", codes)] <- "depression-group"
  groups[!is.na(codes) & grepl("^F1[0-9]([.]|$)", codes) | codes %in% "F63.0"] <- "substance-group"
  groups[codes %in% c("F43.1", "F43.10")] <- "ptsd-group"
  # F31.9 appears twice: one unspecified BP-I entry in EACH episode section.
  # Use the nearest preceding, unambiguous BP-I code to resolve the duplicate.
  for (j in which(codes %in% "F31.9")) {
    anchors <- which(seq_along(codes) < j & codes %in% c(manic_codes, depressive_codes[1:6]))
    if (!length(anchors)) stop("Cannot determine the episode section for F31.9 in Excel column ", j, ".")
    anchor <- tail(anchors, 1L)
    groups[j] <- if (codes[anchor] %in% manic_codes) "psychosis-group" else "depression-group"
  }
  data.frame(
    Source_Sheet = sheet, Excel_Column = openxlsx::int2col(columns), Column_Index = columns,
    ICD_Code = codes[columns],
    Diagnosis_Name = mapply(english_diagnosis_name, codes[columns], groups[columns], USE.NAMES = FALSE),
    Diagnosis_Group = groups[columns], Source_Header = source_headers[columns],
    stringsAsFactors = FALSE
  )
}

extract_p7_assignments <- function(filepath, diagnosis_sheet = NULL) {
  selection <- select_dips_sheet(filepath, diagnosis_sheet)
  raw <- read_sheet_matrix(filepath, selection$sheet)
  header <- selection$header
  mapping <- build_diagnosis_mapping(raw, header, selection$sheet)
  if (header$row >= nrow(raw)) stop("The diagnosis sheet contains no participant rows.")
  source_rows <- seq.int(header$row + 1L, nrow(raw))
  ids <- normalize_p7_id(raw[source_rows, header$id_col])
  has_id <- nzchar(ids)
  if (!any(has_id)) stop("No participant IDs were found below the diagnosis header.")
  if (any(!has_id)) message("Skipped ", sum(!has_id), " row(s) with an empty ID.")
  source_rows <- source_rows[has_id]
  ids <- ids[has_id]
  text_values <- matrix(clean_text(raw[source_rows, mapping$Column_Index, drop = FALSE]),
                        nrow = length(source_rows), ncol = nrow(mapping))
  # Empty cells are missing, rather than diagnosis code 0.
  missing <- text_values %in% c("", "NA", "N/A")
  dim(missing) <- dim(text_values)
  values <- matrix(suppressWarnings(as.numeric(gsub(",", ".", text_values, fixed = TRUE))),
                   nrow = nrow(text_values), ncol = ncol(text_values))
  has_values <- rowSums(!missing) > 0L
  valid_id <- grepl("^[0-9]+$", ids)
  invalid_id <- which(!valid_id & has_values)
  if (length(invalid_id)) stop(
    "Diagnosis values without a valid numeric participant ID in sheet '", selection$sheet,
    "', row(s): ", paste(source_rows[invalid_id], collapse = ", "), ". No grouping file was changed."
  )
  keep <- valid_id
  if (!any(keep)) stop("No participant IDs were found below the diagnosis header.")
  ids <- ids[keep]
  source_rows <- source_rows[keep]
  values <- values[keep, , drop = FALSE]
  text_values <- text_values[keep, , drop = FALSE]
  missing <- missing[keep, , drop = FALSE]
  invalid <- !missing & (is.na(values) | !(values %in% 0:3))
  dim(invalid) <- dim(values)
  if (any(invalid)) {
    bad <- which(invalid, arr.ind = TRUE)
    examples <- apply(head(bad, 12L), 1L, function(ij) paste0(
      mapping$Excel_Column[ij[2]], source_rows[ij[1]], "='", text_values[ij[1], ij[2]], "'"
    ))
    stop("Unexpected diagnosis codes; expected 0, 1, 2, 3 or empty cells. Sheet '",
         selection$sheet, "': ", paste(examples, collapse = "; "), ". No grouping file was changed.")
  }
  values[missing] <- NA_real_
  unique_ids <- unique(ids)
  unique_ids <- unique_ids[order(as.numeric(unique_ids), unique_ids)]
  indices <- lapply(unique_ids, function(id) which(ids == id))
  combine <- function(predicate) do.call(rbind, lapply(indices, function(ix) {
    apply(values[ix, , drop = FALSE], 2L, predicate)
  }))
  current <- combine(function(x) any(x %in% c(1, 3)))
  lifetime <- combine(function(x) any(x %in% c(2, 3)))
  assessed <- combine(function(x) any(!is.na(x)))
  memberships <- do.call(cbind, lapply(GROUP_PRIORITY, function(g) {
    rowSums(current[, mapping$Diagnosis_Group == g, drop = FALSE]) > 0L
  }))
  colnames(memberships) <- GROUP_PRIORITY
  num_groups <- rowSums(memberships)
  primary <- apply(memberships, 1L, function(x) {
    if (any(x)) GROUP_PRIORITY[which(x)[1L]] else NA_character_
  })
  all_groups <- apply(memberships, 1L, function(x) paste(GROUP_PRIORITY[x], collapse = "; "))
  all_current <- apply(current, 1L, function(x) paste(mapping$Diagnosis_Name[x], collapse = "; "))
  participants <- data.frame(
    ID = unique_ids,
    group = ifelse(num_groups > 0L, "With_Diagnosis", "No_Diagnosis"),
    Diagnosis_Group = primary, All_Diagnosis_Groups = all_groups,
    Number_Of_Diagnosis_Groups = as.integer(num_groups),
    Current_Diagnosis_Count = as.integer(rowSums(current)),
    Lifetime_Only_Diagnosis_Count = as.integer(rowSums(lifetime & !current)),
    Assessed_Diagnosis_Count = as.integer(rowSums(assessed)),
    Missing_Diagnosis_Count = as.integer(ncol(assessed) - rowSums(assessed)),
    Current_Diagnoses = all_current, Source_Row_Count = lengths(indices),
    Source_Rows = vapply(indices, function(ix) paste(source_rows[ix], collapse = "; "), character(1)),
    stringsAsFactors = FALSE
  )
  labels <- c(GROUP_PRIORITY, "No_Diagnosis")
  exclusive <- ifelse(is.na(primary), "No_Diagnosis", primary)
  frequencies <- data.frame(
    Diagnosis_Group = labels, Count = as.integer(table(factor(exclusive, levels = labels))),
    stringsAsFactors = FALSE
  )
  occurrence <- data.frame(
    Diagnosis_Group = GROUP_PRIORITY, Count_Any_Current = as.integer(colSums(memberships)),
    Count_Exclusive = frequencies$Count[seq_along(GROUP_PRIORITY)], stringsAsFactors = FALSE
  )
  stopifnot(sum(frequencies$Count) == nrow(participants), !anyDuplicated(participants$ID))
  list(
    Frequencies = frequencies, Group_occurrence = occurrence,
    Participant_assignments = participants,
    Multiple_groups = participants[participants$Number_Of_Diagnosis_Groups > 1L, , drop = FALSE],
    Duplicate_IDs = participants[participants$Source_Row_Count > 1L, , drop = FALSE],
    Diagnosis_columns = mapping
  )
}

update_p7_groupings <- function(filepath, participants, sheet = "p7_adults_adolescents") {
  if (!file.exists(filepath)) stop("Grouping workbook not found: ", filepath)
  wb <- openxlsx::loadWorkbook(filepath)
  sheet_exists <- sheet %in% names(wb)
  if (sheet_exists) {
    old <- read_sheet_matrix(filepath, sheet)
  } else {
    openxlsx::addWorksheet(wb, sheet)
    old <- matrix(character(0), nrow = 0L, ncol = 0L)
  }
  wanted <- c("With_Diagnosis", "No_Diagnosis", "Diagnosis_Group")
  aliases <- list(c("withdiagnosis", "patient", "patients"),
                  c("nodiagnosis", "control", "controls", "hc"), "diagnosisgroup")
  if (!length(old)) {
    header_row <- 1L
    target_cols <- 1:3
  } else {
    matches <- vapply(seq_len(min(50L, nrow(old))), function(r) {
      sum(header_token(old[r, ]) %in% unlist(aliases))
    }, integer(1))
    if (!any(matches > 0L)) stop("No grouping headers found on sheet '", sheet, "'. No cells were overwritten.")
    header_row <- which.max(matches)
    tokens <- header_token(old[header_row, ])
    target_cols <- integer(3L)
    next_col <- ncol(old)
    for (j in seq_along(aliases)) {
      found <- which(tokens %in% aliases[[j]])
      if (length(found) > 1L) stop("Ambiguous columns for ", wanted[j], " on sheet '", sheet, "'.")
      if (length(found)) target_cols[j] <- found else {
        next_col <- next_col + 1L
        target_cols[j] <- next_col
      }
    }
  }
  with <- participants[participants$group == "With_Diagnosis", , drop = FALSE]
  without <- participants$ID[participants$group == "No_Diagnosis"]
  lists <- list(with$ID, without, with$Diagnosis_Group)
  data_start <- header_row + 1L
  data_end <- max(nrow(old), header_row + nrow(with), header_row + length(without))
  for (j in seq_along(target_cols)) {
    if (data_end >= data_start) openxlsx::deleteData(
      wb, sheet, cols = target_cols[j], rows = seq.int(data_start, data_end), gridExpand = TRUE
    )
    openxlsx::writeData(wb, sheet, wanted[j], startCol = target_cols[j], startRow = header_row, colNames = FALSE)
    if (length(lists[[j]])) openxlsx::writeData(
      wb, sheet, as.character(lists[[j]]), startCol = target_cols[j], startRow = data_start, colNames = FALSE
    )
  }
  staged <- tempfile(pattern = "p7_groupings_", tmpdir = dirname(filepath), fileext = ".xlsx")
  on.exit(unlink(staged), add = TRUE)
  openxlsx::saveWorkbook(wb, staged, overwrite = TRUE)
  # Verify the ID-to-group alignment before replacing the existing workbook.
  checked <- read_sheet_matrix(staged, sheet)
  for (j in seq_along(target_cols)) {
    stopifnot(clean_text(checked[header_row, target_cols[j]]) == wanted[j])
    actual <- if (nrow(checked) > header_row) clean_text(checked[seq.int(data_start, nrow(checked)), target_cols[j]]) else character(0)
    expected <- c(as.character(lists[[j]]), rep("", max(0L, length(actual) - length(lists[[j]]))))
    if (!identical(actual, expected)) stop("Grouping verification failed for ", wanted[j], ". Original workbook unchanged.")
  }
  backup_dir <- file.path(dirname(filepath), "backups")
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  backup <- tempfile(
    pattern = paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "_", tools::file_path_sans_ext(basename(filepath)), "_"),
    tmpdir = backup_dir, fileext = ".xlsx"
  )
  if (!file.copy(filepath, backup, overwrite = FALSE)) stop("Could not back up the grouping workbook; no replacement attempted.")
  if (!file.copy(staged, filepath, overwrite = TRUE)) stop(
    "Could not update the grouping workbook. Close it in Excel and rerun. Backup: ", backup
  )
  message("Updated grouping sheet: ", sheet, " (", nrow(with), " With_Diagnosis; ", length(without), " No_Diagnosis).")
  invisible(backup)
}

write_p7_summary <- function(tables, filepath) {
  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#173B57", textDecoration = "bold", wrapText = TRUE)
  for (sh in names(tables)) {
    tab <- tables[[sh]]
    openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, tab, withFilter = nrow(tab) > 0L, keepNA = FALSE)
    openxlsx::addStyle(wb, sh, header_style, rows = 1L, cols = seq_len(ncol(tab)), gridExpand = TRUE)
    openxlsx::setRowHeights(wb, sh, rows = 1L, heights = 32)
    openxlsx::freezePane(wb, sh, firstRow = TRUE)
    widths <- vapply(tab, function(x) {
      max(nchar(as.character(x)), 0L, na.rm = TRUE)
    }, numeric(1))
    widths <- pmin(55, pmax(16, widths + 2L, nchar(names(tab)) + 2L))
    openxlsx::setColWidths(wb, sh, cols = seq_len(ncol(tab)), widths = widths)
  }
  openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
}

write_p7_plot <- function(frequencies, filepath, participant_count) {
  plot_data <- frequencies
  plot_data$Diagnosis_Group <- factor(plot_data$Diagnosis_Group, levels = rev(frequencies$Diagnosis_Group))
  colors <- c(
    "psychosis-group" = "#A364BB", "anxiety-group" = "#E5A339",
    "obsession-group" = "#50A6B5", "depression-group" = "#5B81C4",
    "substance-group" = "#BA725C", "ptsd-group" = "#8C9B52",
    "rest-group" = "#9199A5", "No_Diagnosis" = "#63A583"
  )
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Diagnosis_Group, y = Count, fill = Diagnosis_Group)) +
    ggplot2::geom_col(width = 0.72, colour = "#293544", linewidth = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = Count), hjust = -0.25, size = 5, fontface = "bold") +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_manual(values = colors, drop = FALSE) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(1, max(plot_data$Count)) * 1.2),
      breaks = function(limits) unique(round(pretty(limits))), expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = "Project 7: current diagnosis groups",
      subtitle = paste0("N = ", participant_count, " unique participants | Exclusive assignment by group priority"),
      x = NULL, y = "Number of participants",
      caption = "Current diagnoses: codes 1 or 3. Lifetime-only diagnoses (2) do not establish a current diagnosis."
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      legend.position = "none", panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 21),
      plot.subtitle = ggplot2::element_text(size = 13), plot.caption = ggplot2::element_text(size = 10),
      plot.margin = ggplot2::margin(15, 30, 15, 15)
    )
  ggplot2::ggsave(filepath, p, width = 12, height = 7, dpi = 300, bg = "white")
}

run_p7_diagnosis_extraction <- function(root = ROOT, diagnosis_sheet = DIAGNOSIS_SHEET) {
  ensure_p7_packages()
  input <- file.path(root, "private_information", "P7_DIPS-diagnosis-only.xlsx")
  groupings <- file.path(root, "private_information", "2025-10-15_groupings.xlsx")
  out <- file.path(root, "out", "p7_diagnoses")
  if (!file.exists(input)) stop("Diagnosis workbook not found: ", input)
  if (!file.exists(groupings)) stop("Grouping workbook not found: ", groupings)
  result <- extract_p7_assignments(input, diagnosis_sheet)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  prefix <- format(Sys.Date(), "%Y-%m-%d")
  table_path <- file.path(out, paste0(prefix, "_p7_diagnosis_summary.xlsx"))
  plot_path <- file.path(out, paste0(prefix, "_p7_diagnosis_frequencies.png"))
  write_p7_summary(result, table_path)
  write_p7_plot(result$Frequencies, plot_path, nrow(result$Participant_assignments))
  update_p7_groupings(groupings, result$Participant_assignments)
  message("Frequency table and participant assignments: ", table_path)
  message("Frequency plot: ", plot_path)
  message("Participants in more than one group: ", nrow(result$Multiple_groups), " (see Multiple_groups).")
  message("Duplicate IDs combined across source rows: ", nrow(result$Duplicate_IDs), ".")
  invisible(result)
}

run_p7_diagnosis_extraction()
