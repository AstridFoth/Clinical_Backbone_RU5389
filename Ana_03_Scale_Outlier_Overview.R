# Sample-wise high/extreme values for every existing scale/subscale score.
# Run after prep_05_Score_Scales.R. No item rescoring and no exclusions.
# Inputs: 02_cleaned/<sample>/YYYY-MM-DD_<sample>_clean_master_scored.xlsx
# Labels: matching 02_cleaned/keys/YYYY-MM-DD_<sample>_keys.rds (when present).
# Output: 01_project_data/all_projects_backbone/derivatives/questionnaire_extreme_values/
#         <run timestamp>/ : workbook, heatmaps, paginated raincloud PNGs + PDF.
# Set BACKBONE_ROOT or CFG$root if this script is stored outside the repository.
# To load functions without running: options(backbone.extreme_values.autorun = FALSE).

CFG <- list(
  root = NULL,
  samples = NULL,                 # NULL = discover all samples with scored masters
  score_suffix = "",             # "" = full scores; e.g. "lt030" for filtered scores
  high_percentile = .95,          # high: at/above this sample-wise cutoff
  extreme_percentile = .99,       # extreme: at/above this cutoff, counted once
  min_reference_n = 4L,           # fewer observations: not assessed, never coded 0
  small_reference_n = 100L,      # flag limited precision of upper percentiles
  quantile_type = 7L,            # R sample quantile interpolation
  missing_numeric_codes = c(-99, -999, -9999),
  heatmap_top_n = 50L,
  highlight_top_n = 10L,           # people with most high/extreme scores, per sample
  raincloud_panels_per_page = 12L, # all scores, spread over readable overview pages
  install_missing_packages = TRUE,
  # Exact overrides if needed; example:
  # field_overrides = list(adults = c(survey_day = "startdate", gender = "gender"))
  field_overrides = list(),
  output_dir = NULL
)

norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(trimws(as.character(x))))
safe_score_name <- function(x) {
  x <- gsub("[^a-z0-9]+", "_", tolower(trimws(as.character(x))))
  gsub("^_+|_+$", "", x)
}
empty_text <- function(x) {
  y <- trimws(as.character(x))
  y[is.na(x) | tolower(y) %in% c("", "na", "nan", "null", "n/a")] <- NA_character_
  y
}
numeric_values <- function(x, cfg, label, strict = FALSE) {
  y <- empty_text(x)
  z <- suppressWarnings(as.numeric(gsub(",", ".", y, fixed = TRUE)))
  if (strict && any(!is.na(y) & is.na(z))) {
    stop("Non-numeric values in ", label, ": ",
         paste(head(unique(y[!is.na(y) & is.na(z)]), 5L), collapse = ", "))
  }
  z[!is.finite(z) | z %in% cfg$missing_numeric_codes] <- NA_real_
  z
}
bind_tables <- function(x) {
  if (!length(x)) return(data.frame())
  out <- do.call(rbind, x)
  rownames(out) <- NULL
  out
}

script_location <- function() {
  # source() / RStudio / Rscript all supported.
  frames <- sys.frames()
  paths <- vapply(frames, function(f) {
    if (is.null(f$ofile)) "" else as.character(f$ofile)[1L]
  }, character(1))
  paths <- paths[nzchar(paths)]
  if (length(paths)) return(dirname(normalizePath(tail(paths, 1L), mustWork = FALSE)))
  arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(arg)) return(dirname(normalizePath(sub("^--file=", "", arg[1L]), mustWork = FALSE)))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) return(dirname(p))
  }
  getwd()
}
resolve_root <- function(cfg) {
  explicit <- c(cfg$root, Sys.getenv("BACKBONE_ROOT", ""))
  explicit <- explicit[!is.na(explicit) & nzchar(explicit)]
  if (length(explicit)) {
    root <- explicit[1L]
    if (!dir.exists(file.path(root, "02_cleaned"))) stop("No 02_cleaned directory under ", root)
    return(normalizePath(root, winslash = "/"))
  }
  for (start in unique(c(script_location(), getwd()))) {
    p <- normalizePath(start, winslash = "/", mustWork = FALSE)
    repeat {
      if (dir.exists(file.path(p, "02_cleaned"))) return(p)
      parent <- dirname(p)
      if (identical(p, parent)) break
      p <- parent
    }
  }
  stop("Cannot find repository root. Set CFG$root or BACKBONE_ROOT.")
}
ensure_packages <- function(cfg) {
  pkgs <- c("readxl", "openxlsx")
  available <- function(pk) requireNamespace(pk, quietly = TRUE)
  missing <- pkgs[!vapply(pkgs, available, logical(1))]
  if (length(missing) && isTRUE(cfg$install_missing_packages)) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  missing <- pkgs[!vapply(pkgs, available, logical(1))]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
}
read_master <- function(path) {
  d <- as.data.frame(readxl::read_excel(path, .name_repair = "minimal"),
                     check.names = FALSE, stringsAsFactors = FALSE)
  names(d) <- trimws(names(d))
  # Preserve double underscores in score_<scale>__<subscale>.
  if (anyDuplicated(names(d))) stop("Duplicate column names in ", path)
  d
}
resolve_master <- function(root, sample, cfg) {
  folder <- file.path(root, "02_cleaned", sample)
  tail_name <- paste0("_", sample, "_clean_master_scored",
                      if (nzchar(cfg$score_suffix)) paste0("_", cfg$score_suffix), ".xlsx")
  files <- list.files(folder, full.names = TRUE)
  files <- files[endsWith(basename(files), tail_name) &
                   grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}_", basename(files)) &
                   !startsWith(basename(files), "~$")]
  if (!length(files)) return(NULL)
  dates <- suppressWarnings(as.Date(substr(basename(files), 1L, 10L)))
  files <- files[!is.na(dates)]
  dates <- dates[!is.na(dates)]
  if (!length(files)) return(NULL)
  files <- files[dates == max(dates)]
  if (length(files) != 1L) stop("Ambiguous newest scored master for ", sample)
  files[1L]
}

FIELD_CANDIDATES <- list(
  id = c("vpid", "vp_id", "participant_id", "participantid", "subject_id", "id"),
  project = c("project", "p", "project_id"),
  group = c("group", "paper_group", "clinical_group", "participant_group"),
  diagnosis_group = c("Diagnosis_Group", "diagnosis_group"),
  survey_day = c("startdate", "start_date", "datestamp", "survey_date", "date", "datum",
                 "submitdate", "submit_date", "enddate"),
  age_years = c("age_years", "ageyears", "agecalc", "age"),
  gender = c("gender", "sex", "geschlecht")
)
field_column <- function(d, field, sample, cfg) {
  override <- cfg$field_overrides[[sample]]
  if (!is.null(override) && field %in% names(override)) {
    cc <- unname(override[field])
    if (!cc %in% names(d)) stop("Missing overridden ", field, " column: ", cc)
    return(cc)
  }
  hit <- match(norm_name(FIELD_CANDIDATES[[field]]), norm_name(names(d)))
  hit <- hit[!is.na(hit)]
  if (length(hit)) names(d)[hit[1L]] else NA_character_
}
parse_survey_day <- function(x) {
  if (inherits(x, "Date")) return(as.character(x))
  # readxl represents Excel's local wall-clock timestamps as UTC POSIXct.
  # Preserve that stored calendar date instead of shifting it around midnight.
  if (inherits(x, "POSIXt")) return(as.character(as.Date(x, tz = "UTC")))
  y <- empty_text(x)
  out <- rep(NA_character_, length(y))
  for (i in which(!is.na(y))) {
    z <- y[i]
    day <- as.Date(NA)
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", z)) {
      day <- suppressWarnings(as.Date(substr(z, 1L, 10L), format = "%Y-%m-%d"))
    } else if (grepl("^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}", z)) {
      day <- suppressWarnings(as.Date(sub(" .*", "", z), format = "%d.%m.%Y"))
    } else if (grepl("^[0-9]+(\\.[0-9]+)?$", z)) {
      serial <- suppressWarnings(as.numeric(z))
      if (is.finite(serial) && serial >= 20000 && serial <= 80000) {
        day <- as.Date(floor(serial), origin = "1899-12-30")
      }
    }
    out[i] <- as.character(day)
  }
  out
}
project_key <- function(x) {
  z <- norm_name(x)
  z <- sub("^(project|projekt|p)", "", z)
  numeric <- grepl("^[0-9]+$", z)
  z[numeric] <- as.character(as.integer(z[numeric]))
  z[is.na(x) | z == ""] <- NA_character_
  z
}
person_key <- function(id, project) {
  z <- norm_name(id)
  z[is.na(id) | z == ""] <- NA_character_
  p <- project_key(project)
  out <- paste(p, z, sep = "::")
  out[is.na(p) | is.na(z)] <- NA_character_
  out
}
make_metadata <- function(d, sample, input_date, cfg) {
  fields <- names(FIELD_CANDIDATES)
  cols <- setNames(vapply(fields, function(f) field_column(d, f, sample, cfg), character(1)), fields)
  if (is.na(cols["id"]) || is.na(cols["project"])) {
    stop("Participant ID and project columns are required in sample ", sample)
  }
  values <- lapply(cols, function(cc) if (is.na(cc)) rep(NA_character_, nrow(d)) else empty_text(d[[cc]]))
  m <- as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
  m$age_years <- numeric_values(m$age_years, cfg, "age_years")
  # Never interpret birth dates or implausible numeric dates as age in years.
  m$age_years[m$age_years < 0 | m$age_years > 120] <- NA_real_
  date_col <- cols["survey_day"]
  m$survey_day <- if (is.na(date_col)) rep(NA_character_, nrow(d)) else parse_survey_day(d[[date_col]])
  m$survey_day_source <- ifelse(is.na(m$survey_day), NA_character_, paste0("scored master: ", date_col))
  m$sample <- rep(sample, nrow(d))
  m$input_date <- rep(input_date, nrow(d))
  m$person_key <- person_key(m$id, m$project)
  if (anyNA(m$person_key)) stop("Missing/invalid project or ID in sample ", sample)
  keep_rows <- which(!duplicated(m$person_key))
  if (length(keep_rows) < nrow(m)) message("Sample ", sample, ": skipping ",
    nrow(m) - length(keep_rows), " duplicate row(s); keeping the first project/ID occurrence.")
  m <- m[keep_rows, , drop = FALSE]
  m <- m[, c("id", "group", "diagnosis_group", "project", "survey_day", "age_years", "gender",
              "sample", "survey_day_source", "input_date", "person_key"), drop = FALSE]
  list(metadata = m, columns = cols, keep_rows = keep_rows)
}
recover_survey_dates <- function(m, root, sample, input_date, cfg) {
  if (!anyNA(m$survey_day)) return(m)
  raw <- file.path(root, "01_project_data", "all_projects_backbone", "raw_data", "questionnaires",
                   paste0("ALL_", input_date, "_", sample, "_questionnaire.xlsx"))
  if (!file.exists(raw)) {
    warning("Missing survey days in ", sample, "; matching source not found: ", raw, call. = FALSE)
    return(m)
  }
  d <- read_master(raw)
  cols <- vapply(c("id", "project", "survey_day"), function(f) field_column(d, f, sample, cfg), character(1))
  if (anyNA(cols)) {
    warning("Cannot recover survey dates: missing source columns in ", basename(raw), call. = FALSE)
    return(m)
  }
  k <- person_key(d[[cols[1L]]], d[[cols[2L]]])
  day <- parse_survey_day(d[[cols[3L]]])
  for (i in which(is.na(m$survey_day))) {
    dates <- unique(day[!is.na(k) & k == m$person_key[i] & !is.na(day)])
    if (length(dates) == 1L) {
      m$survey_day[i] <- dates
      m$survey_day_source[i] <- paste0(basename(raw), ": ", cols[3L])
    } else if (length(dates) > 1L) {
      m$survey_day_source[i] <- "ambiguous source dates; review required"
    }
  }
  m
}

score_dictionary <- function(d, keys = NULL) {
  cols <- grep("^score_", names(d), value = TRUE)
  if (!length(cols)) stop("No score_* columns: run Step 05 first.")
  # CAPE fixed factors have both sum and mean. One factor must count once.
  excluded <- cols[grepl("^score_cape__positive_schlier2015_.*_mean$", cols) &
                     sub("_mean$", "_sum", cols) %in% cols]
  cols <- setdiff(cols, excluded)
  dict <- data.frame(score_column = cols, scale = NA_character_, subscale = NA_character_,
                     level = NA_character_, label_source = "score column", stringsAsFactors = FALSE)
  for (j in seq_along(cols)) {
    cc <- cols[j]
    body <- sub("^score_", "", cc)
    parts <- strsplit(body, "__", fixed = TRUE)[[1L]]
    dict$scale[j] <- toupper(parts[1L])
    dict$level[j] <- if (length(parts) > 1L) "subscale" else "scale"
    if (length(parts) > 1L) dict$subscale[j] <- paste(parts[-1L], collapse = "__")
    if (!is.null(keys)) {
      if (dict$level[j] == "scale" && !is.null(keys$items_by_scale)) {
        tab <- keys$items_by_scale
        hit <- which(paste0("score_", safe_score_name(tab$scale)) == cc)
      } else if (dict$level[j] == "subscale" && !is.null(keys$items_by_subscale)) {
        tab <- keys$items_by_subscale
        expected <- paste0("score_", safe_score_name(tab$scale), "__", safe_score_name(tab$subscale))
        cape <- !is.na(tab$scale) & !is.na(tab$subscale) & grepl("^positive_schlier2015_", tab$subscale) &
          toupper(tab$scale) == "CAPE"
        expected[cape] <- paste0(expected[cape], if (endsWith(cc, "_mean")) "_mean" else "_sum")
        hit <- which(expected == cc)
      } else hit <- integer()
      if (length(hit)) {
        h <- hit[1L]
        dict$scale[j] <- as.character(tab$scale[h])
        if (dict$level[j] == "subscale") dict$subscale[j] <- as.character(tab$subscale[h])
        dict$label_source[j] <- "matching Step 03 key"
      }
    }
  }
  dict$label <- ifelse(dict$level == "scale", paste0(dict$scale, " / total"),
                        paste(dict$scale, dict$subscale, sep = " / "))
  dict <- dict[order(tolower(dict$scale), dict$level != "scale", tolower(dict$subscale)), , drop = FALSE]
  rownames(dict) <- NULL
  attr(dict, "excluded_redundant_columns") <- excluded
  dict
}

assess_score <- function(x, cfg) {
  good <- is.finite(x)
  n <- sum(good)
  q <- if (n) as.numeric(stats::quantile(x[good],
    c(.25, .5, .75, cfg$high_percentile, cfg$extreme_percentile), type = cfg$quantile_type)) else rep(NA_real_, 5L)
  n_unique <- length(unique(x[good]))
  eligible <- n >= cfg$min_reference_n && n_unique > 1L
  minimum <- if (n) min(x[good]) else NA_real_
  # Include tied scores at the cutoff. A cutoff coinciding with the sample
  # minimum needs a strict comparison: the lowest observed value cannot be
  # interpreted as elevated merely because a floor mass makes P95/P99 equal it.
  high_at_floor <- n > 0L && q[4L] == minimum
  extreme_at_floor <- n > 0L && q[5L] == minimum
  code <- rep(NA_integer_, length(x))
  if (eligible) {
    code[good] <- 0L
    high <- if (high_at_floor) good & x > q[4L] else good & x >= q[4L]
    extreme <- if (extreme_at_floor) good & x > q[5L] else good & x >= q[5L]
    code[high] <- 1L
    code[extreme] <- 2L
  }
  status <- if (n < cfg$min_reference_n) "insufficient_n" else if (n_unique <= 1L) "constant_not_assessed" else
    if (high_at_floor || extreme_at_floor) "cutoff_at_floor_strict_comparison" else "assessed"
  list(code = code, stats = data.frame(
    n_valid = n, n_missing = length(x) - n, n_unique = n_unique,
    q1 = q[1L], median = q[2L], q3 = q[3L],
    high_percentile = cfg$high_percentile, extreme_percentile = cfg$extreme_percentile,
    high_cutoff = q[4L], extreme_cutoff = q[5L],
    high_comparison = if (high_at_floor) ">" else ">=",
    extreme_comparison = if (extreme_at_floor) ">" else ">=",
    coincident_cutoffs = isTRUE(q[4L] == q[5L]),
    n_at_high_cutoff = sum(x[good] == q[4L]), n_at_extreme_cutoff = sum(x[good] == q[5L]),
    assessed = eligible, reference_status = status, small_reference = n < cfg$small_reference_n,
    n_elevated = sum(abs(code) >= 1L, na.rm = TRUE),
    n_high = sum(code == 1L, na.rm = TRUE), n_extreme = sum(code == 2L, na.rm = TRUE),
    pct_elevated = if (eligible && n) 100 * sum(abs(code) >= 1L, na.rm = TRUE) / n else NA_real_,
    stringsAsFactors = FALSE))
}
analyse_sample <- function(d, m, dict, cfg) {
  codes <- matrix(NA_integer_, nrow(d), nrow(dict), dimnames = list(NULL, dict$score_column))
  values <- matrix(NA_real_, nrow(d), nrow(dict), dimnames = list(NULL, dict$score_column))
  threshold_parts <- detail_parts <- vector("list", nrow(dict))
  for (j in seq_len(nrow(dict))) {
    x <- numeric_values(d[[dict$score_column[j]]], cfg, dict$score_column[j], strict = TRUE)
    values[, j] <- x
    a <- assess_score(x, cfg)
    codes[, j] <- a$code
    tt <- cbind(data.frame(sample = m$sample[1L], input_date = m$input_date[1L]), dict[j, , drop = FALSE], a$stats)
    threshold_parts[[j]] <- tt
    idx <- which(!is.na(a$code) & a$code != 0L)
    # Keep an empty typed detail table for runs with no elevated values too.
    detail_parts[[j]] <- cbind(m[idx, , drop = FALSE],
      dict[rep(j, length(idx)), , drop = FALSE],
      data.frame(value = x[idx], value_code = a$code[idx],
                 severity = ifelse(abs(a$code[idx]) == 2L, "extreme", "high"), stringsAsFactors = FALSE),
      a$stats[rep(1L, length(idx)), , drop = FALSE])
  }
  flags <- !is.na(codes) & codes != 0L
  assessed <- !is.na(codes)
  p <- m
  p$n_scores_assessed <- rowSums(assessed)
  p$n_scores_unassessed <- ncol(codes) - p$n_scores_assessed
  p$n_elevated <- rowSums(flags)
  p$pct_elevated <- ifelse(p$n_scores_assessed > 0, 100 * p$n_elevated / p$n_scores_assessed, NA_real_)
  p$n_extreme <- rowSums(!is.na(codes) & abs(codes) == 2L)
  p$n_high <- rowSums(!is.na(codes) & codes == 1L)
  totals <- dict$level == "scale"
  p$n_total_scale_elevated <- rowSums(flags[, totals, drop = FALSE])
  p$n_subscale_elevated <- rowSums(flags[, !totals, drop = FALSE])
  p$n_questionnaires_elevated <- vapply(seq_len(nrow(p)), function(i) length(unique(dict$scale[flags[i, ]])), integer(1))
  p$elevated_scales <- vapply(seq_len(nrow(p)), function(i) {
    if (!any(flags[i, ])) return("")
    paste(paste0(dict$label[flags[i, ]], " [", ifelse(codes[i, flags[i, ]] == 2L, "extreme", "high"), "]"), collapse = "; ")
  }, character(1))
  p <- p[order(-p$n_elevated, -p$n_extreme, -p$pct_elevated, p$id), , drop = FALSE]
  first <- c("id", "sample", "project", "n_elevated", "pct_elevated", "n_high", "n_extreme",
             "group", "diagnosis_group", "survey_day", "age_years", "gender")
  p <- p[, c(first, setdiff(names(p), first)), drop = FALSE]
  list(persons = p, details = bind_tables(detail_parts), thresholds = bind_tables(threshold_parts),
       codes = codes, values = values, metadata = m, dictionary = dict)
}

write_workbook <- function(sheets, path) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    tab <- sheets[[nm]]
    # Excel permits 1,048,576 rows including the header.
    if (nrow(tab) > 1048575L || ncol(tab) > 16384L) stop("Excel dimensions exceeded in ", nm)
    openxlsx::addWorksheet(wb, nm)
    if (nrow(tab)) {
      openxlsx::writeDataTable(wb, nm, tab, tableStyle = "TableStyleMedium2", keepNA = FALSE)
    } else openxlsx::writeData(wb, nm, tab)
    openxlsx::freezePane(wb, nm, firstRow = TRUE, firstCol = TRUE)
    openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(tab)), widths = 18)
    narrow <- which(names(tab) %in% c("id", "project", "survey_day", "age_years") |
                      grepl("^(n_|pct_)", names(tab)))
    if (length(narrow)) openxlsx::setColWidths(wb, nm, cols = narrow, widths = 14)
    wide <- which(names(tab) %in% c("elevated_scales", "description", "label", "survey_day_source", "source_file"))
    if (length(wide)) openxlsx::setColWidths(wb, nm, cols = wide, widths = 65)
    pct <- grep("^pct_", names(tab))
    if (length(pct) && nrow(tab)) openxlsx::addStyle(wb, nm, openxlsx::createStyle(numFmt = "0.0"),
       rows = 2:(nrow(tab) + 1L), cols = pct, gridExpand = TRUE, stack = TRUE)
    if (nm %in% c("persons_elevated", "persons_all") && nrow(tab)) {
      for (cc in intersect(c("n_elevated", "n_high", "n_extreme", "pct_elevated"), names(tab))) {
        openxlsx::conditionalFormatting(wb, nm, cols = match(cc, names(tab)), rows = 2:(nrow(tab) + 1L),
                                        type = "colourScale", style = c("#FFFFFF", "#F4B183", "#C00000"))
      }
    }
    if (startsWith(nm, "matrix_") && nrow(tab)) {
      score_cols <- grep("^score_", names(tab))
      for (code in 0:2) {
        colour <- c("#F4F4F4", "#F4A582", "#B2182B")[code + 1L]
        cell <- paste0(openxlsx::int2col(min(score_cols)), "2")
        # ISNUMBER keeps blank / unassessed cells distinct from numeric zero.
        rule <- paste0("AND(ISNUMBER(", cell, "),", cell, "=", code, ")")
        openxlsx::conditionalFormatting(wb, nm, cols = score_cols, rows = 2:(nrow(tab) + 1L),
          type = "expression", rule = rule, style = openxlsx::createStyle(bgFill = colour))
      }
    }
  }
  # These sheets contain no embedded drawings or comments. Remove openxlsx's
  # placeholder drawing relationships so other Excel readers do not try to load
  # nonexistent drawing parts. Keep printer settings and table relationships.
  for (i in seq_along(wb$worksheets_rels)) {
    rel <- wb$worksheets_rels[[i]]
    wb$worksheets_rels[[i]] <- rel[!grepl("relationships/(drawing|vmlDrawing)\"", rel)]
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}
plot_heatmap <- function(a, path, cfg) {
  p <- head(a$persons[a$persons$n_elevated > 0L, , drop = FALSE], cfg$heatmap_top_n)
  if (!nrow(p)) return(invisible(NULL))
  idx <- match(p$person_key, a$metadata$person_key)
  codes <- a$codes[idx, , drop = FALSE]
  use <- colSums(!is.na(codes) & codes != 0L) > 0L
  codes <- codes[, use, drop = FALSE]
  labels <- a$dictionary$label[use]
  # Reversed y-axis: the most frequently flagged person appears at the top.
  nr <- nrow(codes); nc <- ncol(codes)
  grDevices::png(path, width = max(1600, 35 * nc), height = max(1000, 25 * nr + 480), res = 140)
  on.exit(grDevices::dev.off())
  graphics::par(mar = c(17, 12, 5, 2))
  graphics::plot(NA, xlim = c(.5, nc + .5), ylim = c(.5, nr + .5), axes = FALSE,
                 xlab = "", ylab = "", xaxs = "i", yaxs = "i",
                 main = paste0(a$metadata$sample[1L], ": high/extreme value pattern (top ", nr, ")"))
  colours <- c("#F4F4F4", "#F4A582", "#B2182B")
  for (i in seq_len(nr)) for (j in seq_len(nc)) {
    c <- codes[i, j]
    graphics::rect(j - .5, nr - i + .5, j + .5, nr - i + 1.5,
                   col = if (is.na(c)) "#7F7F7F" else colours[c + 1L], border = "white")
  }
  graphics::axis(1, at = seq_len(nc), labels = labels, las = 2, cex.axis = .65)
  graphics::axis(2, at = seq_len(nr), labels = rev(paste0(p$project, " / ", p$id, " (", p$n_elevated, ")")),
                 las = 2, cex.axis = .7)
  graphics::legend("top", inset = c(0, -.12), xpd = NA, horiz = TRUE, bty = "n", cex = .75,
    legend = c("below high criterion", "high", "extreme", "not assessed"),
    fill = c(colours, "#7F7F7F"))
  invisible(path)
}

raincloud_highlights <- function(a, cfg) {
  top <- head(a$persons[a$persons$n_elevated > 0L, , drop = FALSE], cfg$highlight_top_n)
  # Fixed palette/order within this sample across every scale and overview page.
  palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
               "#56B4E9", "#332288", "#882255", "#44AA99", "#AA4499")
  if (nrow(top) > length(palette)) palette <- grDevices::hcl.colors(nrow(top), "Dark 3")
  top$colour <- palette[seq_len(nrow(top))]
  top$legend_label <- if (nrow(top)) paste0("ID ", top$id, " / ", top$project, " (n=", top$n_elevated, ")") else character()
  top
}

draw_raincloud_page <- function(a, columns, top, page, n_pages, cfg) {
  ncols <- min(4L, cfg$raincloud_panels_per_page)
  nrows <- ceiling(cfg$raincloud_panels_per_page / ncols)
  slots <- nrows * ncols
  grid <- matrix(seq_len(slots), nrow = nrows, byrow = TRUE)
  graphics::layout(rbind(grid, rep(slots + 1L, ncols)), heights = c(rep(1, nrows), .65))
  graphics::par(oma = c(0, 0, 3.6, 0), mar = c(1.7, 4.2, 5.1, 1.2))
  # Deterministic horizontal jitter; no downsampling and no changes to the RNG.
  xpos <- .72 + ((seq_len(nrow(a$metadata)) * .618033988749895) %% 1) * .70
  top_rows <- match(top$person_key, a$metadata$person_key)
  for (slot in seq_len(slots)) {
    if (slot > length(columns)) { graphics::plot.new(); next }
    j <- columns[slot]
    y <- a$values[, j]
    good <- is.finite(y)
    label <- paste(strwrap(a$dictionary$label[j], width = 35L), collapse = "\n")
    limits <- if (any(good)) range(y[good]) else c(0, 1)
    padding <- if (diff(limits) > 0) .06 * diff(limits) else max(.5, .05 * abs(limits[1L]))
    limits <- limits + c(-padding, padding)
    graphics::plot(NA, xlim = c(0, 1.5), ylim = limits, xaxt = "n", xlab = "", ylab = "Score",
                   main = "", bty = "l")
    graphics::title(main = label, cex.main = .88, line = 2.1)
    graphics::mtext(paste0("n = ", sum(good), "  |  high: ", a$thresholds$n_high[j],
                          "  |  extreme: ", a$thresholds$n_extreme[j]),
                    side = 3, line = .3, cex = .7)
    if (!any(good)) {
      graphics::text(1, mean(limits), "No valid scores", col = "#666666")
      next
    }
    t <- a$thresholds[j, , drop = FALSE]
    # Cloud: density clipped to the observed range; no mass beyond real scores.
    if (length(unique(y[good])) > 1L) {
      den <- stats::density(y[good], from = min(y[good]), to = max(y[good]), n = 512L)
      cloud_x <- .45 - .39 * den$y / max(den$y)
      graphics::polygon(c(.45, cloud_x, .45), c(den$x[1L], den$x, tail(den$x, 1L)),
                         col = "#C2DCEA", border = "#699DB8", lwd = .7)
    } else graphics::segments(.06, y[good][1L], .45, y[good][1L], col = "#699DB8", lwd = 2)
    # Box: Q1/median/Q3; whiskers show min/max without IQR-based point flags.
    graphics::segments(.58, min(y[good]), .58, max(y[good]), col = "#4B6573")
    graphics::segments(.54, c(min(y[good]), max(y[good])), .62, c(min(y[good]), max(y[good])), col = "#4B6573")
    graphics::rect(.52, t$q1, .64, t$q3, col = "#E0EBF0", border = "#4B6573")
    graphics::segments(.52, t$median, .64, t$median, lwd = 1.5, col = "#274756")
    if (isTRUE(t$assessed)) {
      graphics::abline(h = t$high_cutoff, lty = 2, lwd = .8, col = "#555555")
      graphics::abline(h = t$extreme_cutoff, lty = 3, lwd = .8, col = "#8B1A4A")
    }
    if (t$reference_status != "assessed") {
      note <- if (t$reference_status == "cutoff_at_floor_strict_comparison") "Floor cutoff: only strictly higher scores classified" else
        gsub("_", " ", t$reference_status)
      graphics::mtext(note, side = 1, line = .2, cex = .62, col = "#666666")
    }
    graphics::points(xpos[good], y[good], pch = 16, cex = .48,
                     col = grDevices::adjustcolor("#606060", alpha.f = .27))
    for (i in seq_len(nrow(top))) {
      row <- top_rows[i]
      if (!is.na(row) && good[row]) graphics::points(xpos[row], y[row], pch = 21,
        cex = 1.25, bg = top$colour[i], col = "white", lwd = .65)
    }
  }
  graphics::par(cex = 1, mar = c(.1, .5, .1, .5))
  graphics::plot.new()
  if (nrow(top)) {
    graphics::legend("center", legend = top$legend_label, pch = 21, pt.bg = top$colour,
                     col = "#FFFFFF", text.col = "#222222", pt.cex = 1.4, cex = .85,
                     ncol = min(5L, nrow(top)), bty = "n", xpd = NA)
  } else graphics::text(.5, .5, "No high/extreme values in the assessed scales.")
  graphics::mtext(paste0(a$metadata$sample[1L], " | raincloud score distributions | page ", page, "/", n_pages),
                  outer = TRUE, side = 3, line = 2, cex = 1.15, font = 2)
  fence_note <- paste0("Dashed: P", 100 * cfg$high_percentile, "; dotted: P", 100 * cfg$extreme_percentile, ".")
  graphics::mtext(paste0("All available participants. Colours: top ", nrow(top),
    " by number of high/extreme values. ", fence_note, " Cloud: density; box: quartiles; rain: individual scores."),
    outer = TRUE, side = 3, line = .6, cex = .8)
}

plot_raincloud_overviews <- function(a, output, cfg) {
  top <- raincloud_highlights(a, cfg)
  pages <- split(seq_len(nrow(a$dictionary)), ceiling(seq_len(nrow(a$dictionary)) / cfg$raincloud_panels_per_page))
  stem <- paste0(a$metadata$sample[1L], "_raincloud_overview")
  pdf_path <- file.path(output, paste0(stem, ".pdf"))
  grDevices::pdf(pdf_path, width = 16, height = 10.8, onefile = TRUE, useDingbats = FALSE)
  tryCatch({
    for (i in seq_along(pages)) draw_raincloud_page(a, pages[[i]], top, i, length(pages), cfg)
  }, finally = grDevices::dev.off())
  for (i in seq_along(pages)) {
    png_path <- file.path(output, sprintf("%s_%02d.png", stem, i))
    grDevices::png(png_path, width = 2400, height = 1620, res = 150)
    tryCatch(draw_raincloud_page(a, pages[[i]], top, i, length(pages), cfg), finally = grDevices::dev.off())
  }
  invisible(list(pdf = pdf_path, highlights = top))
}

run_scale_extreme_value_overview <- function(cfg = CFG) {
  if (!is.finite(cfg$high_percentile) || !is.finite(cfg$extreme_percentile) ||
      cfg$high_percentile <= 0 || cfg$extreme_percentile >= 1 ||
      cfg$high_percentile >= cfg$extreme_percentile) stop("Percentiles must satisfy 0 < high < extreme < 1.")
  if (cfg$min_reference_n < 4L) stop("min_reference_n must be at least 4.")
  if (cfg$highlight_top_n < 1L || cfg$highlight_top_n != as.integer(cfg$highlight_top_n)) stop("Invalid highlight_top_n.")
  if (cfg$raincloud_panels_per_page < 1L || cfg$raincloud_panels_per_page > 12L ||
      cfg$raincloud_panels_per_page != as.integer(cfg$raincloud_panels_per_page)) stop("raincloud_panels_per_page must be 1..12.")
  ensure_packages(cfg)
  root <- resolve_root(cfg)
  samples <- cfg$samples
  auto <- is.null(samples)
  if (auto) {
    discovered <- basename(list.dirs(file.path(root, "02_cleaned"), recursive = FALSE))
    samples <- c(intersect(c("adults", "adolescents"), discovered),
                 setdiff(discovered, c("adults", "adolescents")))
  }
  inputs <- lapply(samples, function(s) resolve_master(root, s, cfg))
  names(inputs) <- samples
  absent <- vapply(inputs, is.null, logical(1))
  if (!auto && any(absent)) stop("No matching scored master for: ", paste(samples[absent], collapse = ", "))
  inputs <- inputs[!absent]
  if (!length(inputs)) stop("No scored masters found. Run Step 05 first.")
  results <- source_rows <- qc_rows <- list()
  for (sample in names(inputs)) {
    source <- inputs[[sample]]
    input_date <- substr(basename(source), 1L, 10L)
    message("Screening ", sample, ": ", source)
    d <- read_master(source)
    if (!nrow(d)) stop("Empty scored master: ", source)
    key_path <- file.path(root, "02_cleaned", "keys", paste0(input_date, "_", sample, "_keys.rds"))
    keys <- if (file.exists(key_path)) readRDS(key_path) else NULL
    if (is.null(keys)) warning("Matching keys not found for ", sample, "; using score-column labels.", call. = FALSE)
    dict <- score_dictionary(d, keys)
    meta <- make_metadata(d, sample, input_date, cfg)
    d <- d[meta$keep_rows, , drop = FALSE]
    m <- recover_survey_dates(meta$metadata, root, sample, input_date, cfg)
    for (field in c("group", "diagnosis_group", "survey_day", "age_years", "gender")) {
      missing <- is.na(m[[field]])
      qc_rows[[length(qc_rows) + 1L]] <- data.frame(
        sample = sample, field = field, source_column = unname(meta$columns[field]),
        n_missing = sum(missing), n_total = nrow(m),
        note = if (field == "diagnosis_group") "Missing diagnosis subtype can be expected for No_Diagnosis." else
          if (field == "survey_day") "See survey_day_source; source recovery uses project + ID and matching input date." else "",
        stringsAsFactors = FALSE)
      if (any(missing) && field != "diagnosis_group") warning(sample, ": ", sum(missing), " missing ", field, call. = FALSE)
    }
    results[[sample]] <- analyse_sample(d, m, dict, cfg)
    source_rows[[sample]] <- data.frame(sample = sample, input_date = input_date, source_file = source,
      key_file = if (is.null(keys)) NA_character_ else key_path,
      score_suffix = if (nzchar(cfg$score_suffix)) cfg$score_suffix else "full",
      redundant_columns_omitted = paste(attr(dict, "excluded_redundant_columns"), collapse = "; "), stringsAsFactors = FALSE)
  }
  persons <- bind_tables(lapply(results, `[[`, "persons"))
  persons <- persons[order(-persons$n_elevated, -persons$n_extreme, -persons$pct_elevated, persons$sample, persons$id), , drop = FALSE]
  details <- bind_tables(lapply(results, `[[`, "details"))
  if (nrow(details)) details <- details[order(match(paste(details$sample, details$person_key),
    paste(persons$sample, persons$person_key)), details$scale, details$subscale), , drop = FALSE]
  thresholds <- bind_tables(lapply(results, `[[`, "thresholds"))
  criteria <- data.frame(topic = c("Scope", "High", "Extreme", "Quantiles and ties", "Small samples", "Constant scales",
    "Missing data", "Person ranking", "Dependence", "Duplicate records", "Dates", "Gender", "Raincloud plots", "Interpretation", "References"),
    description = c(
      "Every existing score_* column, separately per sample across projects and groups. Only the upper tail is classified. No automatic exclusions or changes to source data. These are empirical sample percentiles, not clinical norms.",
      paste0("High = score >= P", 100 * cfg$high_percentile, " and < P", 100 * cfg$extreme_percentile,
        "; code 1. See the floor exception below. n_elevated includes both high and extreme values."),
      paste0("Extreme = score >= P", 100 * cfg$extreme_percentile, "; code 2; overrides high so each score is counted once."),
      paste0("R quantile(type=", cfg$quantile_type, "). Equal scores at a cutoff are treated equally; actual percentages can exceed 5%/1%. If a cutoff equals the sample minimum, use strict > for that cutoff so a common floor value is not labelled elevated. high_comparison/extreme_comparison document >= versus >. Coincident cutoffs and the number of observations at each cutoff are reported."),
      paste0("Fewer than ", cfg$min_reference_n, " valid scores: not assessed. N < ", cfg$small_reference_n,
        " marked small_reference because upper percentiles have limited precision. Reference N is score-specific."),
      "Constant scales are not assessed; all raw points are still plotted. NA matrix cells mean not assessed, not low/ordinary. A zero IQR by itself no longer determines classification.",
      paste0("Missing/nonfinite values and codes ", paste(cfg$missing_numeric_codes, collapse = ", "), " are not assessed. Only available, assessable scores enter the person's percentage denominator. Code 0 = assessed below the elevated-value criterion."),
      "n_elevated = n_high + n_extreme (non-overlapping categories); pct_elevated = n_elevated / n_scores_assessed * 100. Sort by n_elevated, then n_extreme, percentage and ID. Also separates totals/subscales and counts distinct questionnaires.",
      "Total and subscale scores overlap and correlate; counts are descriptive, not independent tests or a probability. Equivalent CAPE Schlier2015 sum/mean outputs count once (sum preferred).",
      "Person key is sample + project + ID. Keep the first occurrence in scored-master row order; skip further records before calculating reference distributions and person summaries.",
      "Erhebungstag is questionnaire start day, then available date alternatives. Missing dates may be recovered from the matching Step 02 export using project + ID; multiple possible days remain missing.",
      "Gender labels/codes and group/diagnosis labels are copied as stored. No recoding or inferred diagnosis.",
      paste0("Every scale/subscale panel contains all available scores, including unassessed distributions; no downsampling. Half-density clipped to observed min/max, quartile box with min/max whiskers, jittered individual points. The density is smoothed and is not a measure of data validity. Top ", cfg$highlight_top_n,
        " people per sample retain their colour/ID across every panel and page. Their available values are highlighted even when below P95 on a particular scale. Dashed line = high percentile; dotted line = extreme percentile. PNG overview pages plus a multipage PDF per sample."),
      "High/extreme describe relative numerical score positions in the current sample. They imply neither invalid data nor clinical abnormality; higher values may mean different things for different constructs. Results depend on the sample's composition and available scales.",
      "R stats::quantile: https://stat.ethz.ch/R-manual/R-patched/library/stats/html/quantile.html ; Allen et al., Raincloud plots, doi:10.12688/wellcomeopenres.15191.2"),
    stringsAsFactors = FALSE)
  sheets <- list(persons_elevated = persons[persons$n_elevated > 0L, , drop = FALSE], persons_all = persons,
                 elevated_values_long = details, scale_overview = thresholds, criteria = criteria,
                 metadata_check = bind_tables(qc_rows), sources = bind_tables(source_rows))
  sheets$raincloud_top10 <- bind_tables(lapply(results, raincloud_highlights, cfg = cfg))
  for (sample in names(results)) {
    a <- results[[sample]]
    idx <- match(a$persons$person_key, a$metadata$person_key)
    matrix_tab <- cbind(a$persons, as.data.frame(a$codes[idx, , drop = FALSE], check.names = FALSE))
    # Sheet names are limited to 31 chars; index avoids truncation collisions.
    sheet_name <- paste0("matrix_", match(sample, names(results)), "_", substr(sample, 1L, 19L))
    sheets[[sheet_name]] <- matrix_tab
  }
  output <- cfg$output_dir
  if (is.null(output)) output <- file.path(root, "01_project_data", "all_projects_backbone", "derivatives",
                                          "questionnaire_extreme_values")
  run_stamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S", tz = "Europe/Berlin")
  output <- file.path(output, paste0(run_stamp, "_", if (nzchar(cfg$score_suffix)) cfg$score_suffix else "full"))
  if (dir.exists(output)) stop("Output directory already exists; rerun with a new timestamp: ", output)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  workbook <- file.path(output, paste0(run_stamp, "_scale_extreme_value_overview.xlsx"))
  write_workbook(sheets, workbook)
  for (sample in names(results)) {
    plot_heatmap(results[[sample]], file.path(output, paste0(sample, "_extreme_value_heatmap.png")), cfg)
    plot_raincloud_overviews(results[[sample]], output, cfg)
  }
  message("Saved: ", workbook)
  message(nrow(persons), " participants screened; ", sum(persons$n_elevated > 0L), " with at least one high/extreme score.")
  invisible(list(workbook = workbook, output_dir = output, persons = persons,
                 details = details, thresholds = thresholds, samples = results))
}

# Backwards-compatible function/option names for earlier versions of this file.
run_scale_outlier_overview <- run_scale_extreme_value_overview
if (isTRUE(getOption("backbone.extreme_values.autorun", getOption("backbone.outliers.autorun", TRUE)))) {
  EXTREME_VALUE_RESULTS <- run_scale_extreme_value_overview(CFG)
}
