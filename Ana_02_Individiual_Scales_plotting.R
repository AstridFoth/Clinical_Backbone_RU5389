# --- Ana_02_Individiual_Scales_plotting.R ---------------------------------------
# Updated: 2026-10-09
# Read the current dated Step 03/05 Excel masters and their date-matched keys.
# Items: one figure per subscale, with one subplot per item.
# Scores: faceted overviews only, sorted into questionnaire folders.
# When both _sum and _mean exist for one score, show its sum (e.g. Schlier).
# Existing score_* columns are plotted as supplied; this script does no scoring.

# ===== Configuration ==========================================================
CFG <- list(
  root = Sys.getenv(
    "BACKBONE_ROOT",
    unset = "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389"
  ),
  samples = c("adults", "adolescents"),
  plot_items = TRUE,
  plot_scores = TRUE,
  by_group = TRUE,
  by_project = TRUE,
  show_points = TRUE,
  point_size = 1.4,
  point_alpha = 0.75,
  # "" = original scores; "lt030" / "lt030_combined" = Step 05 reduced scores.
  score_variant = "",
  item_ncol = 4L,
  score_ncol = 3L,
  dpi = 180,
  base_size = 14,
  # These are the group conventions already used for the Backbone analyses.
  default_hc_projects = c(3L, 4L, 5L, 6L),
  force_hc_projects = 3L,
  non_questionnaire_scales = c(
    "FHSfamilytree", "health", "demographics", "times", "date", "id", "project"
  )
)

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) install.packages(missing, quiet = TRUE)
  unavailable <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(unavailable)) {
    stop("Required packages unavailable: ", paste(unavailable, collapse = ", "))
  }
}

safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", as.character(x))
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nzchar(x), x, "unnamed")
}

# Same score-name convention as Step 05; keep its double-underscore separator.
safe_score_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

# Match both original headers (ASRS[001]) and cleaned headers (asrs001/asrs_001).
normalize_item <- function(x) {
  gsub("[^a-z0-9]", "", tolower(trimws(as.character(x))))
}

as_num <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) x <- gsub(",", ".", trimws(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

pretty_label <- function(x) gsub("_", " ", as.character(x), fixed = TRUE)

latest_dated_file <- function(directory, pattern) {
  paths <- list.files(directory, pattern = pattern, full.names = TRUE,
                      recursive = FALSE)
  if (!length(paths)) return(NA_character_)
  dates <- suppressWarnings(as.Date(substr(basename(paths), 1L, 10L)))
  valid <- !is.na(dates)
  paths <- paths[valid]
  dates <- dates[valid]
  if (!length(paths)) return(NA_character_)
  paths[order(dates, file.info(paths)$mtime, decreasing = TRUE)][1L]
}

resolve_sample_inputs <- function(root, sample, plot_scores, score_variant = "") {
  sample_dir <- file.path(root, "02_cleaned", sample)
  prefix <- paste0("^[0-9]{4}-[0-9]{2}-[0-9]{2}_", sample)
  clean <- latest_dated_file(sample_dir, paste0(prefix, "_clean_master\\.xlsx$"))
  suffix <- if (nzchar(score_variant)) paste0("_", score_variant) else ""
  scored <- latest_dated_file(
    sample_dir, paste0(prefix, "_clean_master_scored", suffix, "\\.xlsx$")
  )
  if (is.na(clean) && is.na(scored)) {
    stop("No dated Excel master found for '", sample, "' in ", sample_dir,
         ". Expected YYYY-MM-DD_", sample, "_clean_master.xlsx",
         " or the matching _clean_master_scored", suffix, ".xlsx file.")
  }
  # Anchor to the latest Step 03 date, so a stale Step 05 file is never reused.
  input_date <- substr(basename(if (!is.na(clean)) clean else scored), 1L, 10L)
  scored_path <- file.path(
    sample_dir, paste0(input_date, "_", sample, "_clean_master_scored", suffix, ".xlsx")
  )
  if (plot_scores && !file.exists(scored_path)) {
    stop("Matching Step 05 scored master missing: ", scored_path,
         ". Run Step 05 for this date, or set CFG$plot_scores = FALSE.")
  }
  master_path <- if (plot_scores || is.na(clean)) scored_path else clean
  key_base <- file.path(root, "02_cleaned", "keys",
                        paste0(input_date, "_", sample, "_keys"))
  key_paths <- paste0(key_base, c(".rds", ".json"))
  key_paths <- key_paths[file.exists(key_paths)]
  if (!length(key_paths)) {
    stop("Date-matched keys missing: ", key_base, ".rds (or .json). Run Step 03.")
  }
  list(master_path = master_path, keys_path = key_paths[1L], input_date = input_date)
}

read_master <- function(path) {
  # Preserve the exact score_* headers, including their '__' separator.
  d <- readxl::read_excel(path, .name_repair = "minimal")
  if (anyNA(names(d)) || any(!nzchar(names(d))) || anyDuplicated(names(d))) {
    stop("Master column names must be nonblank and unique: ", path)
  }
  d
}

key_table <- function(x) {
  if (is.null(x) || !length(x)) {
    return(tibble::tibble(scale = character(), subscale = character(), items = list()))
  }
  if (is.data.frame(x)) {
    if (!all(c("scale", "items") %in% names(x))) {
      stop("Keys need columns 'scale' and 'items'.")
    }
    rows <- lapply(seq_len(nrow(x)), function(i) {
      list(scale = x$scale[i],
           subscale = if ("subscale" %in% names(x)) x$subscale[i] else NA_character_,
           items = x$items[[i]])
    })
  } else {
    rows <- x  # JSON arrays of row objects (simplifyVector = FALSE).
  }
  field <- function(row, name) {
    value <- unlist(row[[name]], use.names = FALSE)
    if (!length(value)) NA_character_ else as.character(value[1L])
  }
  tibble::tibble(
    scale = vapply(rows, field, character(1), name = "scale"),
    subscale = vapply(rows, field, character(1), name = "subscale"),
    items = lapply(rows, function(row) {
      items <- as.character(unlist(row$items, use.names = FALSE))
      unique(items[!is.na(items) & nzchar(trimws(items))])
    })
  )
}

read_keys <- function(path) {
  keys <- if (tolower(tools::file_ext(path)) == "rds") {
    readRDS(path)
  } else {
    jsonlite::fromJSON(path, simplifyVector = FALSE)
  }
  keys$items_by_scale <- key_table(keys$items_by_scale)
  keys$items_by_subscale <- key_table(keys$items_by_subscale)
  if (!nrow(keys$items_by_scale) && !nrow(keys$items_by_subscale)) {
    stop("No item definitions found in keys: ", path)
  }
  keys
}

build_item_sets <- function(keys, excluded_scales) {
  scales <- keys$items_by_scale
  subs <- keys$items_by_subscale
  questionnaires <- unique(c(scales$scale, subs$scale))
  questionnaires <- questionnaires[!is.na(questionnaires) & nzchar(questionnaires)]
  questionnaires <- questionnaires[
    !toupper(questionnaires) %in% toupper(excluded_scales)
  ]
  questionnaires <- questionnaires[order(toupper(questionnaires))]
  sets <- list()
  add_set <- function(questionnaire, subscale, items) {
    items <- unique(as.character(items))
    items <- items[!is.na(items) & nzchar(items)]
    items <- items[!duplicated(normalize_item(items))]
    if (length(items)) {
      sets[[length(sets) + 1L]] <<- list(
        questionnaire = questionnaire, subscale = subscale, items = items
      )
    }
  }
  for (q in questionnaires) {
    q_subs <- subs[!is.na(subs$scale) & subs$scale == q, , drop = FALSE]
    named <- !is.na(q_subs$subscale) & nzchar(trimws(q_subs$subscale))
    q_items <- unique(c(
      unlist(scales$items[!is.na(scales$scale) & scales$scale == q], use.names = FALSE),
      unlist(q_subs$items, use.names = FALSE)
    ))
    if (any(named)) {
      named_subs <- unique(q_subs$subscale[named])
      for (sub in named_subs) {
        hit <- named & !is.na(q_subs$subscale) & q_subs$subscale == sub
        add_set(q, sub, unlist(q_subs$items[hit], use.names = FALSE))
      }
      assigned <- unlist(q_subs$items[named], use.names = FALSE)
      remaining <- q_items[!normalize_item(q_items) %in% normalize_item(assigned)]
      add_set(q, "Items without subscale", remaining)
    } else {
      # AQ, ASRS, MAP-SR, TICS etc. have no named lower-level subscale.
      add_set(q, "Total scale", q_items)
    }
  }
  sets
}

match_item_panels <- function(items, columns) {
  tokens <- normalize_item(columns)
  ids <- normalize_item(items)
  ambiguous <- ids[vapply(ids, function(id) sum(tokens == id) > 1L, logical(1))]
  if (length(ambiguous)) {
    stop("Ambiguous item headers after normalization: ",
         paste(unique(ambiguous), collapse = ", "))
  }
  matched <- columns[match(ids, tokens)]
  tibble::tibble(id = ids, column = matched,
                 label = ifelse(is.na(matched), paste0(items, " (missing column)"), matched))
}

normalize_metadata <- function(d, cfg) {
  header <- normalize_item(names(d))
  project_hit <- match(c("project", "p"), header)
  project_hit <- project_hit[!is.na(project_hit)]
  project <- if (length(project_hit)) as.character(d[[project_hit[1L]]]) else
    rep(NA_character_, nrow(d))
  project <- trimws(project)
  # Actual Backbone labels: 3, p3, project_3, Project 3, etc.
  number <- suppressWarnings(as.integer(sub(
    "^(project|p)[ _-]*", "", tolower(project)
  )))
  project[!is.na(number)] <- paste0("P", number[!is.na(number)])
  project[is.na(project) | !nzchar(project)] <- "unknown"
  project_levels <- unique(project)
  project_levels <- project_levels[order(
    suppressWarnings(as.integer(sub("^P", "", project_levels))),
    project_levels, na.last = TRUE
  )]
  group_hit <- match("group", header)
  group <- if (!is.na(group_hit)) tolower(trimws(as.character(d[[group_hit]]))) else
    rep(NA_character_, nrow(d))
  group <- gsub("[ -]+", "_", group)
  normalized <- rep("unknown", nrow(d))
  normalized[!is.na(group) & group %in%
               c("hc", "control", "healthy", "healthy_control", "without_diagnosis")] <- "HC"
  normalized[!is.na(group) & group %in%
               c("patient", "with_diagnosis")] <- "with_diagnosis"
  normalized[normalized == "unknown" & number %in% cfg$default_hc_projects] <- "HC"
  normalized[number %in% cfg$force_hc_projects] <- "HC"
  d$.plot_group <- factor(normalized, levels = c("HC", "with_diagnosis", "unknown"))
  d$.plot_project <- factor(project, levels = project_levels)
  d
}

score_panels <- function(columns, keys) {
  columns <- columns[grepl("^score_", columns, ignore.case = TRUE)]
  # Avoid duplicate panels for two representations of the same score.
  mean_column <- grepl("_mean$", columns, ignore.case = TRUE)
  sum_partner <- sub("_mean$", "_sum", tolower(columns))
  columns <- columns[!mean_column | !sum_partner %in% tolower(columns)]
  questionnaires <- unique(c(keys$items_by_scale$scale, keys$items_by_subscale$scale))
  questionnaires <- questionnaires[!is.na(questionnaires) & nzchar(questionnaires)]
  q_ids <- safe_score_name(questionnaires)
  panels <- lapply(columns, function(column) {
    body <- sub("^score_", "", tolower(column))
    q_id <- strsplit(body, "__", fixed = TRUE)[[1L]][1L]
    q_hit <- match(q_id, q_ids)
    questionnaire <- if (!is.na(q_hit)) questionnaires[q_hit] else toupper(q_id)
    sub_id <- if (grepl("__", body, fixed = TRUE)) {
      substring(body, nchar(q_id) + 3L)
    } else ""
    subs <- keys$items_by_subscale
    subs <- subs[!is.na(subs$scale) & subs$scale == questionnaire &
                   !is.na(subs$subscale) & nzchar(subs$subscale), , drop = FALSE]
    sub_names <- unique(subs$subscale)
    candidates <- safe_score_name(sub_names)
    sub_hit <- match(sub_id, candidates)
    measure <- ""
    # Fixed CAPE factors now have both a sum and a mean column in Step 05.
    if (is.na(sub_hit) && grepl("_(sum|mean)$", sub_id)) {
      stripped <- sub("_(sum|mean)$", "", sub_id)
      sub_hit <- match(stripped, candidates)
      if (!is.na(sub_hit)) measure <- sub("^.*_", "", sub_id)
    }
    label <- if (!nzchar(sub_id)) "Total score" else if (!is.na(sub_hit)) {
      pretty_label(sub_names[sub_hit])
    } else pretty_label(sub_id)
    if (nzchar(measure)) label <- paste0(label, " (", measure, ")")
    tibble::tibble(id = column, column = column, label = label,
                   questionnaire = questionnaire,
                   order = if (!nzchar(sub_id)) 0L else if (!is.na(sub_hit))
                     sub_hit else length(sub_names) + 1L,
                   measure = measure)
  })
  if (!length(panels)) return(tibble::tibble())
  panels <- dplyr::bind_rows(panels)
  panels[order(toupper(panels$questionnaire), panels$order,
               match(panels$measure, c("", "sum", "mean")), panels$column), ]
}

make_long_data <- function(d, panels) {
  pieces <- lapply(seq_len(nrow(panels)), function(i) {
    column <- panels$column[i]
    value <- if (!is.na(column) && column %in% names(d)) {
      as_num(d[[column]])
    } else rep(NA_real_, nrow(d))
    keep <- is.finite(value)
    tibble::tibble(
      row_id = which(keep), variable = panels$id[i], value = value[keep],
      plot_group = d$.plot_group[keep], project = d$.plot_project[keep]
    )
  })
  dat <- dplyr::bind_rows(pieces)
  dat$variable <- factor(dat$variable, levels = panels$id)
  dat
}

make_violin_plot <- function(dat, panels, title, sample, cfg,
                             by_project = FALSE, free_y = FALSE, ncol = 4L) {
  if (!nrow(dat)) return(NULL)
  dat$x <- if (by_project) dat$project else if (cfg$by_group) {
    dat$plot_group
  } else factor("All")
  if (!cfg$by_group) dat$plot_group <- factor("All")
  dat$plot_group <- droplevels(dat$plot_group)
  dat$x <- droplevels(dat$x)
  # Constant / single-response groups still have points and a box; a density
  # estimate is not meaningful for them.
  violin_dat <- dat |>
    dplyr::group_by(variable, x, plot_group) |>
    dplyr::filter(dplyr::n() >= 2L, dplyr::n_distinct(value) > 1L) |>
    dplyr::ungroup()
  counts <- tabulate(as.integer(dat$variable), nbins = nrow(panels))
  strip_width <- if (nrow(panels) == 1L) 55L else 27L
  strip_labels <- setNames(paste0(
    stringr::str_wrap(panels$label, width = strip_width), "\nN = ", counts
  ), panels$id)
  palette <- c(HC = "#48ADA5", with_diagnosis = "#E7826A",
               unknown = "#A8ACB3", All = "#69A6CC")
  dodge <- ggplot2::position_dodge(width = 0.82)
  p <- ggplot2::ggplot(dat, ggplot2::aes(
    x = x, y = value, fill = plot_group,
    group = interaction(x, plot_group)
  )) +
    ggplot2::geom_violin(
      data = violin_dat, trim = TRUE, scale = "width", width = 0.9,
      alpha = 0.72, colour = "#30343B", linewidth = 0.55, position = dodge
    ) +
    ggplot2::geom_boxplot(
      width = 0.16, outlier.shape = NA, colour = "#30343B", linewidth = 0.55,
      alpha = 0.88, position = dodge
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(variable), ncol = min(as.integer(ncol), nrow(panels)),
      scales = if (free_y) "free_y" else "fixed", drop = FALSE,
      labeller = ggplot2::as_labeller(strip_labels)
    ) +
    ggplot2::scale_fill_manual(values = palette, drop = TRUE) +
    ggplot2::labs(
      title = stringr::str_wrap(title, if (nrow(panels) == 1L) 55L else 85L),
      subtitle = paste0(sample, " | N = ", length(unique(dat$row_id)),
                        " participants | ", nrow(panels), " panels",
                        if (by_project) " | by project" else ""),
      x = if (by_project) "Project" else if (cfg$by_group) "Group" else NULL,
      y = if (free_y) "Score / value" else "Item response", fill = "Group",
      caption = paste0(
        "Panels show valid responses (N). Box: median and interquartile range.",
        if (by_project) " Panel N includes all projects." else "",
        if (any(counts == 0L)) " Empty panels have no available numeric responses." else ""
      )
    ) +
    ggplot2::theme_bw(base_size = cfg$base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "#30343B", linewidth = 0.65),
      strip.background = ggplot2::element_rect(fill = "#F0F3F5", colour = "#30343B"),
      strip.text = ggplot2::element_text(face = "bold", size = cfg$base_size),
      plot.title = ggplot2::element_text(face = "bold", size = cfg$base_size + 5),
      plot.subtitle = ggplot2::element_text(size = cfg$base_size),
      plot.caption = ggplot2::element_text(size = cfg$base_size - 3, hjust = 0),
      panel.spacing = grid::unit(1.0, "lines"),
      plot.margin = ggplot2::margin(12, 15, 12, 12),
      legend.position = if (by_project && cfg$by_group) "bottom" else "none"
    )
  if (cfg$show_points) {
    # Overall panels already place each group on its own x position. Avoid
    # jitter-dodge there: it previously squeezed the points into the boxplot.
    point_position <- if (by_project && cfg$by_group) {
      ggplot2::position_jitterdodge(
        jitter.width = 0.65, jitter.height = 0, dodge.width = 0.82, seed = 5389
      )
    } else {
      ggplot2::position_jitter(width = 0.20, height = 0, seed = 5389)
    }
    p <- p + ggplot2::geom_point(
      position = point_position,
      shape = 21, colour = "#30343B", stroke = 0.3,
      size = cfg$point_size, alpha = cfg$point_alpha, show.legend = FALSE
    )
  }
  if (by_project) p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 11))
  p
}

save_panel_figures <- function(d, panels, title, sample, out_dir, base, cfg,
                               free_y = FALSE, ncol = 4L) {
  dat <- make_long_data(d, panels)
  if (!nrow(dat)) return(0L)
  fs::dir_create(out_dir, recurse = TRUE)
  ncols <- min(as.integer(ncol), nrow(panels))
  nrows <- ceiling(nrow(panels) / ncols)
  height <- if (nrow(panels) == 1L) 5.2 else 2.9 * nrows + 2.1
  count <- 0L
  variants <- if (cfg$by_project && any(as.character(dat$project) != "unknown")) {
    c(FALSE, TRUE)
  } else FALSE
  for (by_project in variants) {
    p <- make_violin_plot(dat, panels, title, sample, cfg, by_project, free_y, ncol)
    panel_width <- if (by_project) max(3.8, length(unique(dat$project)) * 0.58) else 3.5
    width <- if (nrow(panels) == 1L) max(7.5, panel_width) else panel_width * ncols
    path <- file.path(out_dir, paste0(
      "violin__", if (by_project) "by_project" else "overall", "__", base, ".png"
    ))
    ggplot2::ggsave(path, p, width = width, height = height,
                    dpi = cfg$dpi, bg = "white", limitsize = FALSE)
    count <- count + 1L
  }
  count
}

# ===== Run ====================================================================
main <- function(cfg = CFG) {
  if (!dir.exists(cfg$root)) stop("Backbone root does not exist: ", cfg$root)
  if (!cfg$plot_items && !cfg$plot_scores) stop("Enable plot_items or plot_scores.")
  if (!length(cfg$samples)) stop("CFG$samples must contain at least one sample.")
  if (cfg$item_ncol < 1L || cfg$score_ncol < 1L) stop("Panel column counts must be positive.")
  ensure_packages(c("readxl", "jsonlite", "tibble", "dplyr", "ggplot2", "fs", "stringr"))

  # Resolve every requested sample before writing any plots.
  inputs <- setNames(lapply(cfg$samples, function(sample) {
    resolve_sample_inputs(cfg$root, sample, cfg$plot_scores, cfg$score_variant)
  }), cfg$samples)
  summary <- list()
  for (sample in cfg$samples) {
    input <- inputs[[sample]]
    keys <- read_keys(input$keys_path)
    d <- normalize_metadata(read_master(input$master_path), cfg)
    scores <- score_panels(names(d), keys)
    if (cfg$plot_scores && !nrow(scores)) {
      stop("No score_* columns found in the scored master: ", input$master_path)
    }
    out_base <- file.path(cfg$root, "out", "internal_data_analysis",
                         "violin_plots_only", paste0(input$input_date, "_", sample))
    item_dir <- file.path(out_base, "plots", "per_item_violins")
    score_dir <- file.path(out_base, "plots", "per_scale_overviews")
    logs_dir <- file.path(cfg$root, "logs")
    fs::dir_create(logs_dir, recurse = TRUE)
    log_path <- file.path(logs_dir, paste0(input$input_date, "_", sample, "_violin_only_log.txt"))
    log_write <- function(txt) {
      cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", txt, "\n"),
          file = log_path, append = TRUE)
    }
    message("\n== Violin plots: ", sample, " ==")
    message("Input: ", input$master_path)
    message("Keys:  ", input$keys_path)
    log_write(paste0("Input: ", input$master_path))
    log_write(paste0("Keys: ", input$keys_path))
    log_write(paste0("Participants: ", nrow(d), "; score variant: ",
                     if (nzchar(cfg$score_variant)) cfg$score_variant else "unfiltered"))
    item_figures <- 0L
    score_figures <- 0L
    failures <- character()
    save_checked <- function(panels, title, directory, base, free_y, ncol) {
      tryCatch({
        count <- save_panel_figures(d, panels, title, sample, directory, base, cfg,
                                     free_y = free_y, ncol = ncol)
        if (!count) log_write(paste0("Skipped (no numeric responses): ", title))
        count
      }, error = function(e) {
        failure <- paste0(title, ": ", conditionMessage(e))
        failures <<- c(failures, failure)
        log_write(paste0("FAILED: ", failure))
        message("Plot failed: ", failure)
        0L
      })
    }
    if (cfg$plot_items) {
      item_sets <- build_item_sets(keys, cfg$non_questionnaire_scales)
      log_write(paste0("Item panels grouped into ", length(item_sets), " subscale/scale figures."))
      for (set in item_sets) {
        panels <- match_item_panels(set$items, names(d))
        missing <- set$items[is.na(panels$column)]
        title <- paste0(set$questionnaire, " - ", pretty_label(set$subscale), " | Items")
        if (length(missing)) log_write(paste0(
          "Missing item columns [", set$questionnaire, " / ", set$subscale, "]: ",
          paste(missing, collapse = ", ")
        ))
        directory <- file.path(item_dir, safe_filename(set$questionnaire))
        base <- paste0(safe_filename(set$questionnaire), "__", safe_filename(set$subscale), "__items")
        item_figures <- item_figures + save_checked(
          panels, title, directory, base,
          free_y = toupper(set$questionnaire) == "SUQ", ncol = cfg$item_ncol
        )
      }
    }
    if (cfg$plot_scores) {
      questionnaires <- unique(scores$questionnaire)
      for (q in questionnaires) {
        q_panels <- scores[scores$questionnaire == q, , drop = FALSE]
        directory <- file.path(score_dir, safe_filename(q))
        # All scale/subscale scores of a questionnaire on one overview figure.
        overview_count <- save_checked(
          q_panels, paste0(q, " | Scale and subscale scores"), directory,
          paste0(safe_filename(q), "__overview"), free_y = TRUE, ncol = cfg$score_ncol
        )
        score_figures <- score_figures + overview_count
      }
    }
    summary[[sample]] <- list(
      item_figures = item_figures, score_figures = score_figures,
      failures = failures, out_base = out_base, log_path = log_path
    )
    message("Item figures (one per subscale, plus by-project versions): ", item_figures)
    message("Score figures (questionnaire overviews only): ", score_figures)
    message("Output: ", out_base)
    message("Log:    ", log_path)
    log_write(paste0("Done. Item figures: ", item_figures, "; score figures: ",
                     score_figures, "; failed figures: ", length(failures), "."))
    if (length(failures)) {
      warning(length(failures), " plot(s) failed for '", sample,
              "'; see ", log_path, call. = FALSE)
    }
  }
  invisible(summary)
}

main()
