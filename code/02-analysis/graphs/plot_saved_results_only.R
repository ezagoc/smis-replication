# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())

resolve_script_dir <- function() {
  rstudio_path <- tryCatch(
    {
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        rstudioapi::getActiveDocumentContext()$path
      } else {
        ""
      }
    },
    error = function(...) ""
  )

  if (nzchar(rstudio_path)) {
    return(dirname(normalizePath(rstudio_path, winslash = "/", mustWork = TRUE)))
  }

  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    if (file.exists(script_path)) {
      return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

setwd(resolve_script_dir())
source("../analysis/supplemental_outcome_helpers.R")

# 1.0 Import packages
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}

ipak(c("tidyverse", "readxl"))

# 2.0 Paths
repo_root <- local({
  candidate_paths <- c("../../../", "../../", "../", "")

  for (candidate in candidate_paths) {
    if (dir.exists(file.path(candidate, "results"))) {
      return(candidate)
    }
  }

  stop("Could not locate the results root from this script directory.", call. = FALSE)
})

results_root <- file.path(repo_root, "results")
replots_root <- file.path(results_root, "replots")
combined_root <- file.path(replots_root, "combined")

dir.create(replots_root, showWarnings = FALSE, recursive = TRUE)
dir.create(combined_root, showWarnings = FALSE, recursive = TRUE)

# 3.0 Labels and helpers
label_map <- c(
  AC = "Follows Africa Check",
  SMIs = "Number of SMIs followed",
  total_reactions = unname(standard_outcome_label_map["total_reactions"]),
  total_comments = unname(standard_outcome_label_map["total_comments"]),
  total_shares = unname(standard_outcome_label_map["total_shares"]),
  ver = unname(standard_outcome_label_map["ver"]),
  verifiability = unname(standard_outcome_label_map["verifiability"]),
  non_ver = unname(standard_outcome_label_map["non_ver"]),
  true = unname(standard_outcome_label_map["true"]),
  fake = unname(standard_outcome_label_map["fake"]),
  n_posts = unname(standard_outcome_label_map["n_posts"]),
  eng = unname(standard_outcome_label_map["eng"]),
  n_posts_covid = unname(standard_outcome_label_map["n_posts_covid"]),
  pos_b_covid = unname(standard_outcome_label_map["pos_b_covid"]),
  neutral_b_covid = unname(standard_outcome_label_map["neutral_b_covid"]),
  neg_b_covid = unname(standard_outcome_label_map["neg_b_covid"]),
  n_posts_vax = unname(standard_outcome_label_map["n_posts_vax"]),
  pos_b_vax = unname(standard_outcome_label_map["pos_b_vax"]),
  neutral_b_vax = unname(standard_outcome_label_map["neutral_b_vax"]),
  neg_b_vax = unname(standard_outcome_label_map["neg_b_vax"]),
  log_total_reactions = unname(standard_outcome_label_map["total_reactions"]),
  log_total_comments = unname(standard_outcome_label_map["total_comments"]),
  log_total_shares = unname(standard_outcome_label_map["total_shares"]),
  log_verifiability = unname(standard_outcome_label_map["verifiability"]),
  log_non_ver = unname(standard_outcome_label_map["non_ver"]),
  log_true = unname(standard_outcome_label_map["true"]),
  log_fake = unname(standard_outcome_label_map["fake"]),
  log_n_posts = unname(standard_outcome_label_map["n_posts"]),
  log_eng = unname(standard_outcome_label_map["eng"]),
  log_n_posts_covid = unname(standard_outcome_label_map["n_posts_covid"]),
  log_pos_b_covid = unname(standard_outcome_label_map["pos_b_covid"]),
  log_neutral_b_covid = unname(standard_outcome_label_map["neutral_b_covid"]),
  log_neg_b_covid = unname(standard_outcome_label_map["neg_b_covid"]),
  log_n_posts_vax = unname(standard_outcome_label_map["n_posts_vax"]),
  log_pos_b_vax = unname(standard_outcome_label_map["pos_b_vax"]),
  log_neutral_b_vax = unname(standard_outcome_label_map["neutral_b_vax"]),
  log_neg_b_vax = unname(standard_outcome_label_map["neg_b_vax"])
)

horizontal_order <- c("SMIs", "AC", standard_outcome_roots, paste0("log_", standard_outcome_roots))
stage_label_map <- c(AC = "Follows Africa Check", SMIs = "Number of SMIs followed", standard_outcome_label_map)
stage_order <- c("SMIs", "AC", standard_outcome_roots)
stage_map <- standard_stage_labels
standard_stage_panel_defs <- tribble(
  ~var,                ~panel_key, ~panel_label,
  "total_reactions",   "panel_a",  "Panel A. Overall engagement",
  "total_comments",    "panel_a",  "Panel A. Overall engagement",
  "total_shares",      "panel_a",  "Panel A. Overall engagement",
  "n_posts",           "panel_b",  "Panel B. Posting and veracity behavior",
  "eng",               "panel_b",  "Panel B. Posting and veracity behavior",
  "verifiability",     "panel_b",  "Panel B. Posting and veracity behavior",
  "non_ver",           "panel_b",  "Panel B. Posting and veracity behavior",
  "true",              "panel_b",  "Panel B. Posting and veracity behavior",
  "fake",              "panel_b",  "Panel B. Posting and veracity behavior",
  "n_posts_covid",     "panel_c",  "Panel C. COVID-19 content",
  "pos_b_covid",       "panel_c",  "Panel C. COVID-19 content",
  "neutral_b_covid",   "panel_c",  "Panel C. COVID-19 content",
  "neg_b_covid",       "panel_c",  "Panel C. COVID-19 content",
  "n_posts_vax",       "panel_c",  "Panel C. COVID-19 content",
  "pos_b_vax",         "panel_c",  "Panel C. COVID-19 content",
  "neutral_b_vax",     "panel_c",  "Panel C. COVID-19 content",
  "neg_b_vax",         "panel_c",  "Panel C. COVID-19 content"
)
followers_stage_panel_defs <- tribble(
  ~var,    ~panel_key, ~panel_label,
  "SMIs",  "panel_a",  "Panel A. Follower outcomes",
  "AC",    "panel_a",  "Panel A. Follower outcomes"
)

normalize_outcome_var <- function(var_name) {
  ifelse(var_name == "ver", "verifiability", var_name)
}

axis_bounds <- function(lower, upper, pad_fraction = 0.08, min_pad = 0.02) {
  lo <- min(lower, na.rm = TRUE)
  hi <- max(upper, na.rm = TRUE)
  span <- hi - lo

  if (!is.finite(span) || span <= 0) {
    span <- 0
  }

  pad <- max(span * pad_fraction, min_pad)
  c(lo - pad, hi + pad)
}

batch_from_name <- function(filename) {
  if (grepl("_b1_", filename, fixed = TRUE)) {
    return("Batch 1 only")
  }

  if (grepl("_b2_", filename, fixed = TRUE)) {
    return("Batch 2 only")
  }

  if (grepl("_both_", filename, fixed = TRUE)) {
    return("Both batches")
  }

  NA_character_
}

family_profile <- function(family_key) {
  list(
    uses_ads = grepl("^ads_", family_key) || grepl("^followers_ads$", family_key),
    uses_followers_outcomes = grepl("^followers_", family_key),
    is_stage = grepl("stages|linear_90p_p5|95p_weighted", family_key),
    is_aggregate = grepl("aggregate", family_key),
    is_intensive = grepl("intensive", family_key),
    is_strong = grepl("strong", family_key)
  )
}

family_outcome_phrase <- function(profile) {
  if (profile$uses_followers_outcomes) {
    return("follower-account outcomes")
  }

  "online follower behaviors"
}

family_title <- function(family_key, batch_label = NULL) {
  profile <- family_profile(family_key)
  outcome_phrase <- family_outcome_phrase(profile)

  base_title <- if (profile$uses_ads) {
    paste0("Average effect of assignment to receive treatment via paid-for ads on ", outcome_phrase)
  } else if (profile$is_intensive) {
    if (profile$is_strong) {
      paste0("Sample-weighted average marginal effect of an additional strongly-followed SMI being assigned to treatment on ", outcome_phrase)
    } else {
      paste0("Sample-weighted average marginal effect of an additional initially-followed SMI being assigned to treatment on ", outcome_phrase)
    }
  } else if (profile$is_strong) {
    paste0("Average effect of one strongly-followed SMI being assigned to treatment on ", outcome_phrase)
  } else {
    paste0("Average effect of one initially-followed SMI being assigned to treatment on ", outcome_phrase)
  }

  if (!is.null(batch_label) && !is.na(batch_label) && nzchar(batch_label)) {
    return(paste0(base_title, " (", batch_label, ")"))
  }

  base_title
}

family_note <- function(family_key, batch_label = NULL) {
  profile <- family_profile(family_key)
  sample_note <- if (profile$uses_ads) {
    "The treatment is assignment to receive paid-for ads."
  } else if (profile$is_intensive && profile$is_strong) {
    "The sample is restricted to followers who strongly followed at least one study SMI, and the coefficient is the sample-weighted marginal effect of one additional treated SMI."
  } else if (profile$is_intensive) {
    "The coefficient is the sample-weighted marginal effect of one additional treated initially-followed SMI."
  } else if (profile$is_strong) {
    "The sample is restricted to followers who strongly followed at least one study SMI."
  } else {
    "The sample consists of followers who initially followed exactly one study SMI."
  }

  spec_note <- if (profile$is_stage) {
    "This figure reports stage-by-stage effects for weeks 1-4, 5-8, and 9-12."
  } else if (profile$is_aggregate) {
    "This figure reports aggregated treatment-period effects over weeks 1-12."
  } else {
    "This figure reports the existing baseline specification."
  }

  se_note <- if (profile$uses_ads) {
    "Whiskers show 95% confidence intervals based on heteroskedasticity-robust standard errors."
  } else {
    "Whiskers show 95% confidence intervals based on permutation standard deviations."
  }

  batch_note <- if (!is.null(batch_label) && !is.na(batch_label) && nzchar(batch_label)) {
    paste0("The figure uses the ", tolower(batch_label), " sample.")
  } else {
    "Separate panels show Batch 1 only, Batch 2 only, and both batches."
  }

  paste("Notes:", spec_note, sample_note, batch_note, se_note)
}

read_excel_short_path <- function(path) {
  short_copy <- tempfile(pattern = "xlsx_", tmpdir = tempdir(), fileext = ".xlsx")
  on.exit(unlink(short_copy), add = TRUE)
  file.copy(path, short_copy, overwrite = TRUE)
  readxl::read_excel(short_copy)
}

resolve_label <- function(var_name, default_label = NULL) {
  if (!is.na(var_name) && var_name %in% names(label_map)) {
    return(unname(label_map[var_name]))
  }

  if (!is.null(default_label) && !is.na(default_label) && nzchar(default_label)) {
    return(default_label)
  }

  gsub("_", " ", var_name)
}

resolve_stage_label <- function(var_name, default_label = NULL) {
  if (!is.null(default_label) && !is.na(default_label) && nzchar(default_label)) {
    return(default_label)
  }

  if (!is.na(var_name) && var_name %in% names(stage_label_map)) {
    return(unname(stage_label_map[var_name]))
  }

  resolve_label(var_name, default_label)
}

ordered_horizontal_labels <- function(vars, defaults = NULL) {
  ordered <- horizontal_order[horizontal_order %in% vars]
  ordered_labels <- vapply(
    ordered,
    function(x) {
      idx <- match(x, vars)
      default_label <- if (!is.null(defaults) && !is.na(idx)) defaults[[idx]] else NULL
      resolve_label(x, default_label)
    },
    character(1)
  )
  extra_vars <- setdiff(vars, ordered)
  extra_labels <- vapply(
    extra_vars,
    function(x) {
      idx <- match(x, vars)
      default_label <- if (!is.null(defaults) && !is.na(idx)) defaults[[idx]] else NULL
      resolve_label(x, default_label)
    },
    character(1)
  )
  c(ordered_labels, sort(extra_labels))
}

ordered_stage_labels <- function(vars, defaults = NULL) {
  ordered <- stage_order[stage_order %in% vars]
  ordered_labels <- vapply(
    ordered,
    function(x) {
      idx <- match(x, vars)
      default_label <- if (!is.null(defaults) && !is.na(idx)) defaults[[idx]] else NULL
      resolve_stage_label(x, default_label)
    },
    character(1)
  )
  extra_vars <- setdiff(vars, ordered)
  extra_labels <- vapply(
    extra_vars,
    function(x) {
      idx <- match(x, vars)
      default_label <- if (!is.null(defaults) && !is.na(idx)) defaults[[idx]] else NULL
      resolve_stage_label(x, default_label)
    },
    character(1)
  )
  c(ordered_labels, sort(extra_labels))
}

build_horizontal_plot_data <- function(original_path, permutation_path) {
  original <- read_excel_short_path(original_path)
  permutation <- read_excel_short_path(permutation_path)

  final <- original |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "coef") |>
    left_join(
      permutation |>
        summarise(across(everything(), \(x) sd(x, na.rm = TRUE))) |>
        pivot_longer(cols = everything(), names_to = "var", values_to = "sd"),
      by = "var"
    ) |>
    mutate(
      var = normalize_outcome_var(var),
      Variable = vapply(var, resolve_label, character(1)),
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  final$Variable <- factor(
    final$Variable,
    levels = rev(ordered_horizontal_labels(final$var, final$Variable))
  )
  final
}

build_estimate_only_horizontal_data <- function(estimate_path) {
  final <- read_excel_short_path(estimate_path)

  if (!all(c("coef", "sd") %in% names(final))) {
    return(tibble())
  }

  if (!"var" %in% names(final)) {
    final$var <- final$Variable
  }

  final$var <- normalize_outcome_var(final$var)

  if (!"Variable" %in% names(final)) {
    final$Variable <- vapply(final$var, resolve_label, character(1))
  } else {
    final$Variable <- vapply(
      seq_len(nrow(final)),
      function(i) resolve_label(final$var[[i]], final$Variable[[i]]),
      character(1)
    )
  }

  final <- final |>
    mutate(
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  final$Variable <- factor(
    final$Variable,
    levels = rev(ordered_horizontal_labels(final$var, final$Variable))
  )
  final
}

build_combined_horizontal_data <- function(original_dir, permutations_dir) {
  original_files <- list.files(original_dir, pattern = "\\.xlsx$", full.names = TRUE)
  original_files <- preferred_xlsx_files(original_files)

  map_dfr(original_files, function(original_path) {
    filename <- basename(original_path)
    permutation_path <- file.path(permutations_dir, filename)

    if (!file.exists(permutation_path)) {
      return(tibble())
    }

    build_horizontal_plot_data(original_path, permutation_path) |>
      mutate(Batch = batch_from_name(filename))
  })
}

shape_palette <- c(15, 16, 17, 18, 3, 7, 8, 0, 1, 2, 4, 5, 6, 9, 10, 11, 12, 13, 14, 19, 20, 21, 22, 23, 24, 25)

build_stage_plot_data <- function(estimate_path) {
  final <- read_excel_short_path(estimate_path)

  if (!all(c("coef", "sd") %in% names(final))) {
    return(tibble())
  }

  if ("stage" %in% names(final)) {
    final <- final |>
      mutate(Stage = recode(stage, !!!stage_map))
  }

  if (!"Stage" %in% names(final)) {
    return(tibble())
  }

  if (!"var" %in% names(final)) {
    final$var <- final$Variable
  }

  final$var <- normalize_outcome_var(final$var)

  if (!"Variable" %in% names(final)) {
    final$Variable <- vapply(final$var, resolve_stage_label, character(1))
  } else {
    final$Variable <- vapply(
      seq_len(nrow(final)),
      function(i) resolve_stage_label(final$var[[i]], final$Variable[[i]]),
      character(1)
    )
  }

  final <- final |>
    mutate(
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  final$Variable <- factor(final$Variable, levels = ordered_stage_labels(final$var, final$Variable))
  final$Stage <- factor(final$Stage, levels = unname(stage_map))
  final
}

stage_panel_definitions <- function(vars) {
  normalized_vars <- unique(normalize_outcome_var(as.character(vars)))

  if (length(normalized_vars) > 0 && all(normalized_vars %in% followers_stage_panel_defs$var)) {
    return(followers_stage_panel_defs)
  }

  standard_stage_panel_defs
}

split_stage_panel_data <- function(final) {
  defs <- stage_panel_definitions(final$var)
  panel_list <- final |>
    mutate(var = normalize_outcome_var(as.character(var))) |>
    left_join(defs, by = "var") |>
    filter(!is.na(panel_key)) |>
    group_split(panel_key, .keep = TRUE)

  set_names(panel_list, map_chr(panel_list, \(d) d$panel_key[[1]]))
}

panel_output_path <- function(path, panel_key) {
  stem <- sub("\\.pdf$", "", path)
  paste0(stem, "_", panel_key, ".pdf")
}

preferred_xlsx_files <- function(paths) {
  if (length(paths) == 0) {
    return(paths)
  }

  tibble(path = paths, file = basename(paths)) |>
    mutate(
      base_key = sub("_(\\d+)perm(?=(_estimates)?\\.xlsx$)", "", file, perl = TRUE),
      perm_count = suppressWarnings(
        as.integer(sub("^.*_(\\d+)perm(?:_estimates)?\\.xlsx$", "\\1", file, perl = TRUE))
      ),
      perm_count = ifelse(is.na(perm_count), -1L, perm_count)
    ) |>
    arrange(base_key, desc(perm_count), file) |>
    distinct(base_key, .keep_all = TRUE) |>
    pull(path)
}

estimate_stage_panel_height <- function(n_outcomes, has_batch_facets = FALSE) {
  if (has_batch_facets) {
    return(max(4.8, 1.45 * n_outcomes + 1.2))
  }

  n_cols <- if (n_outcomes <= 2) 1 else 2
  n_rows <- ceiling(n_outcomes / n_cols)
  max(4.0, 2.0 * n_rows + 0.8)
}

plot_stage_panel_facets <- function(final, output_path, ylab_text, has_batch_facets = FALSE) {
  if (nrow(final) == 0) {
    return(invisible(NULL))
  }

  final <- final |>
    mutate(
      Variable = factor(as.character(Variable), levels = ordered_stage_labels(var, as.character(Variable)))
    ) |>
    arrange(Variable, Stage)

  y_bounds <- axis_bounds(final$lower, final$upper)
  n_outcomes <- nlevels(droplevels(final$Variable))
  plot_height <- estimate_stage_panel_height(n_outcomes, has_batch_facets = has_batch_facets)

  results_plot <- ggplot(final, aes(x = Stage, y = coef, group = 1)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    geom_point(size = 2.2) +
    geom_linerange(aes(ymin = lower, ymax = upper), linewidth = 0.9) +
    coord_cartesian(ylim = y_bounds) +
    labs(
      y = ylab_text,
      x = "Treatment stage"
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 35, hjust = 1, size = 8.5),
      axis.text.y = element_text(size = 8.5),
      axis.title = element_text(size = 9.5),
      strip.text = element_text(face = "bold", size = 9),
      panel.spacing = unit(0.7, "lines")
    )

  if (has_batch_facets && "Batch" %in% names(final)) {
    results_plot <- results_plot +
      facet_grid(rows = vars(Variable), cols = vars(Batch))
  } else {
    results_plot <- results_plot +
      facet_wrap(vars(Variable), ncol = if (n_outcomes <= 2) 1 else 2)
  }

  ggsave(
    plot = results_plot,
    filename = output_path,
    device = cairo_pdf,
    width = if (has_batch_facets) 10.5 else 8.75,
    height = plot_height,
    units = "in"
  )
}

build_combined_stage_data <- function(estimates_dir) {
  estimate_files <- list.files(estimates_dir, pattern = "\\.xlsx$", full.names = TRUE)
  estimate_files <- preferred_xlsx_files(estimate_files)

  map_dfr(estimate_files, function(estimate_path) {
    filename <- basename(estimate_path)
    final <- build_stage_plot_data(estimate_path)

    if (nrow(final) == 0) {
      return(tibble())
    }

    final |>
      mutate(Batch = batch_from_name(filename))
  })
}

plot_horizontal_family <- function(original_dir, permutations_dir, output_dir, xlab_text = "Average treatment effect, with 95% confidence interval") {
  if (!dir.exists(original_dir) || !dir.exists(permutations_dir)) {
    return(invisible(NULL))
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  family_key <- basename(output_dir)

  original_files <- list.files(original_dir, pattern = "\\.xlsx$", full.names = TRUE)
  original_files <- preferred_xlsx_files(original_files)

  for (original_path in original_files) {
    filename <- basename(original_path)
    permutation_path <- file.path(permutations_dir, filename)

    if (!file.exists(permutation_path)) {
      next
    }

    final <- build_horizontal_plot_data(original_path, permutation_path)

    if (nrow(final) == 0) {
      next
    }

    x_bounds <- axis_bounds(final$lower, final$upper)
    plot_height <- max(7.25, 0.42 * nlevels(droplevels(final$Variable)) + 1.5)
    batch_label <- batch_from_name(filename)

    results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
      geom_point() +
      geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
      coord_cartesian(xlim = x_bounds) +
      labs(
        x = xlab_text,
        y = NULL
      ) +
      theme_bw() +
      theme(
        panel.grid.major = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
        panel.grid.minor = element_blank()
      )

    ggsave(
      plot = results_plot,
      filename = file.path(output_dir, sub("\\.xlsx$", ".pdf", filename)),
      device = cairo_pdf,
      width = 9.5,
      height = plot_height,
      units = "in"
    )
  }
}

plot_estimate_only_horizontal_family <- function(estimates_dir, output_dir, xlab_text) {
  if (!dir.exists(estimates_dir)) {
    return(invisible(NULL))
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  family_key <- basename(output_dir)

  estimate_files <- list.files(estimates_dir, pattern = "\\.xlsx$", full.names = TRUE)
  estimate_files <- preferred_xlsx_files(estimate_files)

  for (estimate_path in estimate_files) {
    filename <- basename(estimate_path)
    final <- build_estimate_only_horizontal_data(estimate_path)

    if (nrow(final) == 0) {
      next
    }

    x_bounds <- axis_bounds(final$lower, final$upper)
    plot_height <- max(7.25, 0.42 * nlevels(droplevels(final$Variable)) + 1.5)
    batch_label <- batch_from_name(filename)

    results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
      geom_point() +
      geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
      coord_cartesian(xlim = x_bounds) +
      labs(
        x = xlab_text,
        y = NULL
      ) +
      theme_bw() +
      theme(
        panel.grid.major = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
        panel.grid.minor = element_blank()
      )

    ggsave(
      plot = results_plot,
      filename = file.path(output_dir, sub("_estimates\\.xlsx$", ".pdf", filename)),
      device = cairo_pdf,
      width = 9.5,
      height = plot_height,
      units = "in"
    )
  }
}

plot_combined_balance_family <- function(original_dir, permutations_dir, output_path, family_title_text = NULL) {
  if (!dir.exists(original_dir) || !dir.exists(permutations_dir)) {
    return(invisible(NULL))
  }

  final <- build_combined_horizontal_data(original_dir, permutations_dir)

  if (nrow(final) == 0) {
    return(invisible(NULL))
  }

  family_key <- basename(output_path)
  family_key <- sub("_batches\\.pdf$", "", family_key)
  family_key <- sub("\\.pdf$", "", family_key)
  final$Batch <- factor(final$Batch, levels = c("Batch 1 only", "Batch 2 only", "Both batches"))
  x_bounds <- axis_bounds(final$lower, final$upper)
  plot_height <- max(6.59, 0.42 * nlevels(droplevels(final$Variable)) + 1.5)

  results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
    geom_point() +
    geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    coord_cartesian(xlim = x_bounds) +
    facet_wrap(~Batch, nrow = 1) +
    labs(
      x = "Average treatment effect, with 95% confidence interval",
      y = NULL
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
      panel.grid.minor = element_blank()
    )

  ggsave(
    plot = results_plot,
    filename = output_path,
    device = cairo_pdf,
    width = 12,
    height = plot_height,
    units = "in"
  )
}

plot_stage_estimates <- function(estimates_dir, output_dir, ylab_text = "Average treatment effect, with 95% confidence interval") {
  if (!dir.exists(estimates_dir)) {
    return(invisible(NULL))
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  family_key <- basename(output_dir)

  estimate_files <- list.files(estimates_dir, pattern = "\\.xlsx$", full.names = TRUE)
  estimate_files <- preferred_xlsx_files(estimate_files)

  for (estimate_path in estimate_files) {
    filename <- basename(estimate_path)
    final <- build_stage_plot_data(estimate_path)

    if (nrow(final) == 0) {
      next
    }

    panel_dfs <- split_stage_panel_data(final)

    walk(
      names(panel_dfs),
      function(panel_key) {
        plot_stage_panel_facets(
          final = droplevels(panel_dfs[[panel_key]]),
          output_path = file.path(
            output_dir,
            sub("_estimates\\.xlsx$", paste0("_", panel_key, ".pdf"), filename)
          ),
          ylab_text = ylab_text
        )
      }
    )
  }
}

plot_combined_stage_family <- function(estimates_dir, output_path, family_title_text = NULL, ylab_text = "Average treatment effect, with 95% confidence interval") {
  if (!dir.exists(estimates_dir)) {
    return(invisible(NULL))
  }

  final <- build_combined_stage_data(estimates_dir)

  if (nrow(final) == 0) {
    return(invisible(NULL))
  }

  family_key <- basename(output_path)
  family_key <- sub("_batches\\.pdf$", "", family_key)
  family_key <- sub("\\.pdf$", "", family_key)
  final$Batch <- factor(final$Batch, levels = c("Batch 1 only", "Batch 2 only", "Both batches"))
  panel_dfs <- split_stage_panel_data(final)

  walk(
    names(panel_dfs),
    function(panel_key) {
      plot_stage_panel_facets(
        final = droplevels(panel_dfs[[panel_key]]),
        output_path = panel_output_path(output_path, panel_key),
        ylab_text = ylab_text,
        has_batch_facets = TRUE
      )
    }
  )
}

# 4.0 Rebuild paper-facing plots from saved results only
plot_horizontal_family(
  original_dir = file.path(results_root, "original"),
  permutations_dir = file.path(results_root, "permutations"),
  output_dir = file.path(replots_root, "extensive")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "strong", "original"),
  permutations_dir = file.path(results_root, "strong", "permutations"),
  output_dir = file.path(replots_root, "extensive_strong")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "intensive_baseline_weighted", "original"),
  permutations_dir = file.path(results_root, "intensive_baseline_weighted", "permutations"),
  output_dir = file.path(replots_root, "intensive_baseline_weighted")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "intensive_baseline_weighted_strong", "original"),
  permutations_dir = file.path(results_root, "intensive_baseline_weighted_strong", "permutations"),
  output_dir = file.path(replots_root, "intensive_baseline_weighted_strong")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "extensive_aggregate_batches", "original"),
  permutations_dir = file.path(results_root, "extensive_aggregate_batches", "permutations"),
  output_dir = file.path(replots_root, "extensive_aggregate_batches")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "extensive_aggregate_batches_strong", "original"),
  permutations_dir = file.path(results_root, "extensive_aggregate_batches_strong", "permutations"),
  output_dir = file.path(replots_root, "extensive_aggregate_batches_strong")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "intensive_aggregate_batches", "original"),
  permutations_dir = file.path(results_root, "intensive_aggregate_batches", "permutations"),
  output_dir = file.path(replots_root, "intensive_aggregate_batches")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "intensive_aggregate_batches_strong", "original"),
  permutations_dir = file.path(results_root, "intensive_aggregate_batches_strong", "permutations"),
  output_dir = file.path(replots_root, "intensive_aggregate_batches_strong")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "followers_extensive", "original"),
  permutations_dir = file.path(results_root, "followers_extensive", "permutations"),
  output_dir = file.path(replots_root, "followers_extensive")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "followers_extensive_strong", "original"),
  permutations_dir = file.path(results_root, "followers_extensive_strong", "permutations"),
  output_dir = file.path(replots_root, "followers_extensive_strong")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "followers_intensive", "original"),
  permutations_dir = file.path(results_root, "followers_intensive", "permutations"),
  output_dir = file.path(replots_root, "followers_intensive")
)

plot_horizontal_family(
  original_dir = file.path(results_root, "followers_aggregate", "original"),
  permutations_dir = file.path(results_root, "followers_aggregate", "permutations"),
  output_dir = file.path(replots_root, "followers_aggregate")
)

plot_estimate_only_horizontal_family(
  estimates_dir = file.path(results_root, "ads_intensive_baseline", "estimates"),
  output_dir = file.path(replots_root, "ads_intensive_baseline"),
  xlab_text = "Ads treatment estimate with 95% confidence interval"
)

plot_estimate_only_horizontal_family(
  estimates_dir = file.path(results_root, "ads_intensive_aggregate", "estimates"),
  output_dir = file.path(replots_root, "ads_intensive_aggregate"),
  xlab_text = "Ads treatment estimate with 95% confidence interval"
)

plot_estimate_only_horizontal_family(
  estimates_dir = file.path(results_root, "followers_ads", "estimates"),
  output_dir = file.path(replots_root, "followers_ads"),
  xlab_text = "Ads treatment estimate with 95% confidence interval"
)

plot_combined_balance_family(
  original_dir = file.path(results_root, "original"),
  permutations_dir = file.path(results_root, "permutations"),
  output_path = file.path(combined_root, "extensive_balance_batches.pdf"),
  family_title = "Extensive Balance"
)

plot_combined_balance_family(
  original_dir = file.path(results_root, "strong", "original"),
  permutations_dir = file.path(results_root, "strong", "permutations"),
  output_path = file.path(combined_root, "extensive_balance_strong_batches.pdf"),
  family_title = "Extensive Balance Strong"
)

plot_combined_balance_family(
  original_dir = file.path(results_root, "intensive_baseline_weighted", "original"),
  permutations_dir = file.path(results_root, "intensive_baseline_weighted", "permutations"),
  output_path = file.path(combined_root, "intensive_balance_batches.pdf"),
  family_title = "Intensive Balance"
)

plot_combined_balance_family(
  original_dir = file.path(results_root, "intensive_baseline_weighted_strong", "original"),
  permutations_dir = file.path(results_root, "intensive_baseline_weighted_strong", "permutations"),
  output_path = file.path(combined_root, "intensive_balance_strong_batches.pdf"),
  family_title = "Intensive Balance Strong"
)

plot_combined_stage_family(
  estimates_dir = file.path(results_root, "extensive_linear_90p_p5", "estimates"),
  output_path = file.path(combined_root, "extensive_stages_batches.pdf"),
  family_title = "Extensive Stages"
)

plot_combined_stage_family(
  estimates_dir = file.path(results_root, "extensive_linear_90p_p5_strong", "estimates"),
  output_path = file.path(combined_root, "extensive_stages_strong_batches.pdf"),
  family_title = "Extensive Stages Strong"
)

plot_combined_stage_family(
  estimates_dir = file.path(results_root, "intensive_linear_95p_weighted", "estimates"),
  output_path = file.path(combined_root, "intensive_stages_batches.pdf"),
  family_title = "Intensive Stages"
)

plot_combined_stage_family(
  estimates_dir = file.path(results_root, "intensive_linear_95p_weighted_strong", "estimates"),
  output_path = file.path(combined_root, "intensive_stages_strong_batches.pdf"),
  family_title = "Intensive Stages Strong"
)

plot_stage_estimates(
  estimates_dir = file.path(results_root, "extensive_linear_90p_p5", "estimates"),
  output_dir = file.path(replots_root, "extensive_linear_90p_p5")
)

plot_stage_estimates(
  estimates_dir = file.path(results_root, "extensive_linear_90p_p5_strong", "estimates"),
  output_dir = file.path(replots_root, "extensive_linear_90p_p5_strong")
)

plot_stage_estimates(
  estimates_dir = file.path(results_root, "intensive_linear_95p_weighted", "estimates"),
  output_dir = file.path(replots_root, "intensive_linear_95p_weighted")
)

plot_stage_estimates(
  estimates_dir = file.path(results_root, "intensive_linear_95p_weighted_strong", "estimates"),
  output_dir = file.path(replots_root, "intensive_linear_95p_weighted_strong")
)

plot_stage_estimates(
  estimates_dir = file.path(results_root, "ads_intensive_stages", "estimates"),
  output_dir = file.path(replots_root, "ads_intensive_stages"),
  ylab_text = "Ads treatment estimate with 95% confidence interval"
)

plot_stage_estimates(
  estimates_dir = file.path(results_root, "followers_stages", "estimates"),
  output_dir = file.path(replots_root, "followers_stages")
)
