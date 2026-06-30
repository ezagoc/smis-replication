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

library(tidyverse)
library(readxl)
library(patchwork)
library(scales)

source("../analysis/supplemental_outcome_helpers.R")

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
stats_path <- file.path(results_root, "aggregate_reference_stats", "aggregate_control_stats.xlsx")
output_root <- file.path(results_root, "replots", "mock_style_aggregates")

figure_specs <- list(
  extensive_aggregate_batches = list(
    sample_key = "extensive",
    input_type = "estimates",
    input_dir = file.path(results_root, "extensive_aggregate_batches", "estimates")
  ),
  extensive_aggregate_batches_strong = list(
    sample_key = "extensive_strong",
    input_type = "estimates",
    input_dir = file.path(results_root, "extensive_aggregate_batches_strong", "estimates")
  ),
  intensive_aggregate_batches = list(
    sample_key = "intensive",
    input_type = "estimates",
    input_dir = file.path(results_root, "intensive_aggregate_batches", "estimates")
  ),
  intensive_aggregate_batches_strong = list(
    sample_key = "intensive_strong",
    input_type = "estimates",
    input_dir = file.path(results_root, "intensive_aggregate_batches_strong", "estimates")
  ),
  ads_intensive_aggregate = list(
    sample_key = "ads_intensive_aggregate",
    input_type = "estimates",
    input_dir = file.path(results_root, "ads_intensive_aggregate", "estimates")
  ),
  extensive_baseline = list(
    sample_key = "extensive_baseline",
    input_type = "original_permutations",
    original_dir = file.path(results_root, "original"),
    permutations_dir = file.path(results_root, "permutations")
  ),
  extensive_baseline_strong = list(
    sample_key = "extensive_baseline_strong",
    input_type = "original_permutations",
    original_dir = file.path(results_root, "strong", "original"),
    permutations_dir = file.path(results_root, "strong", "permutations")
  ),
  intensive_baseline = list(
    sample_key = "intensive_baseline",
    input_type = "original_permutations",
    original_dir = file.path(results_root, "intensive_baseline_weighted", "original"),
    permutations_dir = file.path(results_root, "intensive_baseline_weighted", "permutations")
  ),
  intensive_baseline_strong = list(
    sample_key = "intensive_baseline_strong",
    input_type = "original_permutations",
    original_dir = file.path(results_root, "intensive_baseline_weighted_strong", "original"),
    permutations_dir = file.path(results_root, "intensive_baseline_weighted_strong", "permutations")
  ),
  ads_intensive_baseline = list(
    sample_key = "ads_intensive_baseline",
    input_type = "estimates",
    input_dir = file.path(results_root, "ads_intensive_baseline", "estimates")
  ),
  followers_extensive = list(
    sample_key = "followers_extensive",
    input_type = "followers_permutations",
    input_dir = file.path(results_root, "followers_extensive")
  ),
  followers_extensive_strong = list(
    sample_key = "followers_extensive_strong",
    input_type = "followers_permutations",
    input_dir = file.path(results_root, "followers_extensive_strong")
  ),
  followers_intensive = list(
    sample_key = "followers_intensive",
    input_type = "followers_permutations",
    input_dir = file.path(results_root, "followers_intensive")
  ),
  followers_aggregate = list(
    sample_key = "followers_aggregate",
    input_type = "followers_permutations",
    input_dir = file.path(results_root, "followers_aggregate")
  ),
  followers_ads = list(
    sample_key = "followers_ads",
    input_type = "estimates",
    input_dir = file.path(results_root, "followers_ads", "estimates")
  )
)

family_colors <- c(
  "Overall engagement" = "#0B8F8A",
  "Posting and veracity behavior" = "#006DFF",
  "COVID-19 content" = "#F15A24",
  "Follower outcomes" = "#0B8F8A"
)

standard_panel_defs <- tribble(
  ~outcome_root,       ~panel,      ~panel_rank, ~family,                          ~outcome_rank,
  "total_reactions",   "Panel A.",  1L,          "Overall engagement",             1L,
  "total_comments",    "Panel A.",  1L,          "Overall engagement",             2L,
  "total_shares",      "Panel A.",  1L,          "Overall engagement",             3L,
  "n_posts",           "Panel B.",  2L,          "Posting and veracity behavior",  1L,
  "eng",               "Panel B.",  2L,          "Posting and veracity behavior",  2L,
  "verifiability",     "Panel B.",  2L,          "Posting and veracity behavior",  3L,
  "non_ver",           "Panel B.",  2L,          "Posting and veracity behavior",  4L,
  "true",              "Panel B.",  2L,          "Posting and veracity behavior",  5L,
  "fake",              "Panel B.",  2L,          "Posting and veracity behavior",  6L,
  "n_posts_covid",     "Panel C.",  3L,          "COVID-19 content",               1L,
  "pos_b_covid",       "Panel C.",  3L,          "COVID-19 content",               2L,
  "neutral_b_covid",   "Panel C.",  3L,          "COVID-19 content",               3L,
  "neg_b_covid",       "Panel C.",  3L,          "COVID-19 content",               4L,
  "n_posts_vax",       "Panel C.",  3L,          "COVID-19 content",               5L,
  "pos_b_vax",         "Panel C.",  3L,          "COVID-19 content",               6L,
  "neutral_b_vax",     "Panel C.",  3L,          "COVID-19 content",               7L,
  "neg_b_vax",         "Panel C.",  3L,          "COVID-19 content",               8L
)

followers_panel_defs <- tribble(
  ~outcome_root, ~panel,      ~panel_rank, ~family,             ~outcome_rank,
  "SMIs",        "Panel A.",  1L,          "Follower outcomes", 1L,
  "AC",          "Panel A.",  1L,          "Follower outcomes", 2L
)

aggregate_followers_panel_defs <- tribble(
  ~outcome_root, ~panel,      ~panel_rank, ~family,             ~outcome_rank,
  "SMIs",        "Panel D.",  4L,          "Follower outcomes", 1L,
  "AC",          "Panel D.",  4L,          "Follower outcomes", 2L
)

batch_from_name <- function(filename) {
  if (grepl("_b1_", filename, fixed = TRUE)) {
    return("b1")
  }

  if (grepl("_b2_", filename, fixed = TRUE)) {
    return("b2")
  }

  if (grepl("_both_", filename, fixed = TRUE)) {
    return("both")
  }

  NA_character_
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

batch_title <- function(batch_code) {
  c(b1 = "Batch 1 only", b2 = "Batch 2 only", both = "Both batches")[[batch_code]]
}

is_aggregate_family <- function(sample_key) {
  sample_key %in% c(
    "extensive",
    "extensive_strong",
    "intensive",
    "intensive_strong",
    "ads_intensive_aggregate",
    "followers_aggregate"
  )
}

is_baseline_family <- function(sample_key) {
  sample_key %in% c(
    "extensive_baseline",
    "extensive_baseline_strong",
    "intensive_baseline",
    "intensive_baseline_strong",
    "ads_intensive_baseline"
  )
}

batch_subtitle <- function(batch_code) {
  c(
    b1 = "batch 1",
    b2 = "batch 2",
    both = "both batches"
  )[[batch_code]]
}

sample_subtitle <- function(sample_key, batch_code) {
  batch_text <- batch_subtitle(batch_code)

  if (is_aggregate_family(sample_key)) {
    return(paste("Aggregated effects across weeks 1-12,", batch_text))
  }

  paste("Endline follower outcomes,", batch_text)
}

sample_title <- function(sample_key, include_followers_panel = FALSE) {
  if (sample_key == "extensive_strong") {
    return("Average effect of one strongly-followed SMI being assigned to treatment on online follower behaviors")
  }

  if (sample_key == "extensive") {
    return("Average effect of one initially-followed SMI being assigned to treatment on online follower behaviors")
  }

  if (sample_key == "intensive_strong") {
    return("Sample-weighted average marginal effect of an additional strongly-followed SMI being assigned to treatment on online follower behaviors")
  }

  if (sample_key == "intensive") {
    return("Sample-weighted average marginal effect of an additional initially-followed SMI being assigned to treatment on online follower behaviors")
  }

  if (sample_key == "followers_aggregate") {
    return("Average effect of one initially-followed SMI being assigned to treatment on follower outcomes")
  }

  if (sample_key == "followers_extensive_strong") {
    return("Average effect of one strongly-followed SMI being assigned to treatment on follower outcomes")
  }

  if (sample_key == "followers_extensive") {
    return("Average effect of one initially-followed SMI being assigned to treatment on follower outcomes")
  }

  if (sample_key == "followers_intensive") {
    return("Sample-weighted average marginal effect of an additional initially-followed SMI being assigned to treatment on follower outcomes")
  }

  if (sample_key == "ads_intensive_aggregate") {
    return("Average effect of assignment to receive treatment via paid-for ads on online follower behaviors")
  }

  if (sample_key == "followers_ads") {
    return("Average effect of assignment to receive treatment via paid-for ads on follower outcomes")
  }

  "Average treatment effect"
}

figure_note <- function(sample_key, batch_code) {
  sample_note <- if (sample_key %in% c("followers_ads", "ads_intensive_aggregate", "ads_intensive_baseline")) {
    "The treatment is assignment to receive paid-for ads."
  } else if (sample_key == "intensive_baseline_strong") {
    "The sample is restricted to followers who strongly followed at least one study SMI, and the coefficient is the sample-weighted marginal effect of one additional treated SMI."
  } else if (sample_key == "intensive_baseline") {
    "The coefficient is the sample-weighted marginal effect of one additional treated initially-followed SMI."
  } else if (sample_key == "extensive_baseline_strong") {
    "The sample is restricted to followers who strongly followed at least one study SMI."
  } else if (sample_key == "extensive_baseline") {
    "The sample consists of followers who initially followed exactly one study SMI."
  } else if (sample_key == "followers_intensive") {
    "The coefficient is the sample-weighted marginal effect of one additional treated initially-followed SMI."
  } else if (sample_key == "followers_extensive_strong") {
    "The sample is restricted to followers who strongly followed at least one study SMI and initially followed exactly one study SMI."
  } else if (sample_key == "followers_extensive") {
    "The sample consists of followers who initially followed exactly one study SMI."
  } else if (sample_key == "intensive_strong") {
    "The sample is restricted to followers who strongly followed at least one study SMI, and the coefficient is the sample-weighted marginal effect of one additional treated SMI."
  } else if (sample_key == "intensive") {
    "The coefficient is the sample-weighted marginal effect of one additional treated initially-followed SMI."
  } else if (sample_key == "extensive_strong") {
    "The sample is restricted to followers who strongly followed at least one study SMI."
  } else if (sample_key == "followers_aggregate") {
    "The sample consists of followers who initially followed exactly one study SMI."
  } else {
    "The sample consists of followers who initially followed exactly one study SMI."
  }

  batch_note <- paste0("The figure uses the ", tolower(batch_title(batch_code)), " sample.")

  se_note <- if (sample_key %in% c("followers_ads", "ads_intensive_aggregate", "ads_intensive_baseline")) {
    "Whiskers show 95% confidence intervals based on heteroskedasticity-robust standard errors."
  } else {
    "Whiskers show 95% confidence intervals based on permutation standard deviations."
  }

  spec_note <- if (is_aggregate_family(sample_key)) {
    "This figure reports aggregated treatment-period effects over weeks 1-12."
  } else if (is_baseline_family(sample_key)) {
    "This figure reports baseline balance estimates."
  } else {
    "This figure reports the endline follower-outcome specification."
  }

  paste(
    "Notes:",
    spec_note,
    sample_note,
    batch_note,
    se_note
  )
}

standard_panel_axis_defaults <- list(
  "Panel A." = list(
    limits = c(-0.30, 0.30),
    breaks = c(-0.30, -0.15, 0.00, 0.15, 0.30)
  ),
  "Panel B." = list(
    limits = c(-0.30, 0.30),
    breaks = c(-0.30, -0.15, 0.00, 0.15, 0.30)
  ),
  "Panel C." = list(
    limits = c(-0.02, 0.02),
    breaks = c(-0.02, -0.01, 0.00, 0.01, 0.02)
  )
)

format_range_value <- function(x) {
  if (is.na(x)) {
    return(NA_character_)
  }

  format(round(x), scientific = FALSE, trim = TRUE)
}

format_range_label <- function(outcome_min, outcome_max, fallback = NA_character_) {
  if (!is.na(outcome_min) && !is.na(outcome_max)) {
    return(paste0("(", format_range_value(outcome_min), ", ", format_range_value(outcome_max), ")"))
  }

  fallback
}

read_excel_short_path <- function(path) {
  short_copy <- tempfile(pattern = "xlsx_", tmpdir = tempdir(), fileext = ".xlsx")
  on.exit(unlink(short_copy), add = TRUE)
  file.copy(path, short_copy, overwrite = TRUE)
  readxl::read_excel(short_copy)
}

control_stats <- read_excel_short_path(stats_path)

dir.create(output_root, showWarnings = FALSE, recursive = TRUE)

normalize_outcome_root <- function(outcome_root) {
  ifelse(outcome_root == "ver", "verifiability", outcome_root)
}

outcome_label_from_root <- function(outcome_root, sample_key) {
  if (sample_key %in% c(
    "followers_extensive",
    "followers_extensive_strong",
    "followers_intensive",
    "followers_aggregate",
    "followers_ads"
  )) {
    return(unname(followers_label_map[[outcome_root]]))
  }

  if (outcome_root %in% names(standard_outcome_label_map)) {
    return(unname(standard_outcome_label_map[[outcome_root]]))
  }

  outcome_root
}

read_standard_estimates <- function(estimates_dir, sample_key) {
  estimate_files <- list.files(estimates_dir, pattern = "_estimates\\.xlsx$", full.names = TRUE)
  estimate_files <- preferred_xlsx_files(estimate_files)

  map(
    estimate_files,
    function(estimate_path) {
      estimate_df <- read_excel_short_path(estimate_path)

      if (!"var" %in% names(estimate_df)) {
        return(NULL)
      }

      estimate_df$Variable <- map_chr(
        normalize_outcome_root(sub("^log_", "", estimate_df$var)),
        outcome_label_from_root,
        sample_key = sample_key
      )

      tibble(
        source_path = estimate_path,
        outcome_root = normalize_outcome_root(sub("^log_", "", estimate_df$var)),
        outcome = estimate_df$Variable,
        estimate = estimate_df$coef,
        sd = estimate_df$sd
      )
    }
  ) |>
    compact()
}

read_original_permutation_estimates <- function(original_dir, permutations_dir, sample_key) {
  original_files <- list.files(original_dir, pattern = "\\.xlsx$", full.names = TRUE)
  original_files <- preferred_xlsx_files(original_files)

  map(
    original_files,
    function(original_path) {
      original_df <- read_excel_short_path(original_path)
      permutations_path <- file.path(permutations_dir, basename(original_path))

      if (nrow(original_df) == 0 || !file.exists(permutations_path)) {
        return(NULL)
      }

      permutation_df <- read_excel_short_path(permutations_path)
      available_outcomes <- intersect(
        c(standard_outcome_roots, "ver"),
        names(original_df)
      )

      if (length(available_outcomes) == 0) {
        return(NULL)
      }

      normalized_roots <- normalize_outcome_root(available_outcomes)

      tibble(
        source_path = original_path,
        outcome_root = normalized_roots,
        outcome = map_chr(normalized_roots, outcome_label_from_root, sample_key = sample_key),
        estimate = map_dbl(available_outcomes, \(root) as.numeric(original_df[[root]][[1]])),
        sd = map_dbl(available_outcomes, \(root) stats::sd(permutation_df[[root]], na.rm = TRUE))
      )
    }
  ) |>
    compact()
}

read_followers_permutation_estimates <- function(results_dir) {
  original_dir <- file.path(results_dir, "original")
  permutations_dir <- file.path(results_dir, "permutations")

  original_files <- list.files(original_dir, pattern = "\\.xlsx$", full.names = TRUE)
  original_files <- preferred_xlsx_files(original_files)

  map(
    original_files,
    function(original_path) {
      original_df <- read_excel_short_path(original_path)
      permutations_path <- file.path(permutations_dir, basename(original_path))

      if (nrow(original_df) == 0 || !file.exists(permutations_path)) {
        return(NULL)
      }

      permutation_df <- read_excel_short_path(permutations_path)
      outcome_roots <- intersect(c("AC", "SMIs"), names(original_df))

      if (length(outcome_roots) == 0) {
        return(NULL)
      }

      tibble(
        source_path = original_path,
        outcome_root = outcome_roots,
        outcome = unname(followers_label_map[outcome_roots]),
        estimate = as.numeric(original_df[1, outcome_roots]),
        sd = map_dbl(outcome_roots, \(root) stats::sd(permutation_df[[root]], na.rm = TRUE))
      )
    }
  ) |>
    compact()
}

followers_panel_source <- function(sample_key, batch_code) {
  if (batch_code != "b1") {
    return(NULL)
  }

  switch(
    sample_key,
    extensive = "followers_extensive",
    extensive_strong = "followers_extensive_strong",
    intensive = "followers_intensive",
    ads_intensive_aggregate = "followers_ads",
    ads_intensive_baseline = "followers_ads",
    NULL
  )
}

read_followers_panel_for_sample <- function(sample_key, batch_code) {
  panel_source <- followers_panel_source(sample_key, batch_code)

  if (is.null(panel_source)) {
    return(NULL)
  }

  panel_results_dir <- file.path(results_root, panel_source)

  read_followers_permutation_estimates(panel_results_dir) |>
    keep(\(x) identical(batch_from_name(basename(x$source_path[[1]])), batch_code)) |>
    pluck(1, .default = NULL)
}

read_saved_plot_inputs <- function(results_subdir) {
  spec <- figure_specs[[results_subdir]]
  sample_key <- spec$sample_key

  if (identical(spec$input_type, "followers_permutations")) {
    return(read_followers_permutation_estimates(spec$input_dir))
  }

  if (identical(spec$input_type, "original_permutations")) {
    return(read_original_permutation_estimates(
      original_dir = spec$original_dir,
      permutations_dir = spec$permutations_dir,
      sample_key = sample_key
    ))
  }

  if (!dir.exists(spec$input_dir)) {
    return(list())
  }

  read_standard_estimates(spec$input_dir, sample_key)
}

mock_output_name <- function(source_path) {
  output_name <- basename(source_path)
  output_name <- sub("_estimates\\.xlsx$", "_mock_style.pdf", output_name)
  output_name <- sub("\\.xlsx$", "_mock_style.pdf", output_name)
  output_name
}

panel_definitions_for_sample <- function(sample_key, include_followers_panel = FALSE) {
  if (sample_key %in% c(
    "followers_extensive",
    "followers_extensive_strong",
    "followers_intensive",
    "followers_aggregate",
    "followers_ads"
  )) {
    return(followers_panel_defs)
  }

  if (include_followers_panel) {
    return(bind_rows(standard_panel_defs, aggregate_followers_panel_defs))
  }

  standard_panel_defs
}

prepare_plot_data <- function(estimate_info, sample_key, batch_code) {
  followers_panel_info <- read_followers_panel_for_sample(sample_key, batch_code)
  include_followers_panel <- !is.null(followers_panel_info)
  defs <- panel_definitions_for_sample(sample_key, include_followers_panel = include_followers_panel)

  estimate_df <- estimate_info |>
    mutate(control_sample = sample_key)

  if (!is.null(followers_panel_info)) {
    estimate_df <- bind_rows(
      estimate_df,
      followers_panel_info |>
        mutate(control_sample = followers_panel_source(sample_key, batch_code))
    )
  }

  estimate_df |>
    mutate(
      outcome = if_else(
        !is.na(outcome) & nzchar(outcome),
        outcome,
        map_chr(outcome_root, outcome_label_from_root, sample_key = sample_key)
      ),
      ci_low = estimate - 1.96 * sd,
      ci_high = estimate + 1.96 * sd
    ) |>
    left_join(defs, by = "outcome_root") |>
    left_join(
      control_stats |>
        filter(batch == batch_code) |>
        select(sample, outcome_root, control_mean, control_sd, outcome_min, outcome_max, outcome_range),
      by = c("control_sample" = "sample", "outcome_root" = "outcome_root")
    ) |>
    mutate(
      outcome_range_display = pmap_chr(
        list(outcome_min, outcome_max, outcome_range),
        format_range_label
      )
    ) |>
    filter(!is.na(panel_rank)) |>
    arrange(panel_rank, outcome_rank)
}

panel_axis_spec <- function(panel_df, sample_key) {
  panel_name <- panel_df$panel[[1]]
  default_spec <- standard_panel_axis_defaults[[panel_name]]
  max_abs <- max(abs(c(panel_df$ci_low, panel_df$ci_high, 0)), na.rm = TRUE)

  if (!is.finite(max_abs) || max_abs <= 0) {
    max_abs <- 0.04
  }

  if (
    sample_key %in% c("extensive", "extensive_strong", "intensive", "intensive_strong") &&
      !is.null(default_spec) &&
      max_abs <= max(abs(default_spec$limits))
  ) {
    return(default_spec)
  }

  pad <- max(max_abs * 0.12, 0.01)
  limits <- c(-(max_abs + pad), max_abs + pad)
  breaks <- pretty(limits, n = 5)
  breaks <- breaks[breaks >= limits[1] & breaks <= limits[2]]

  list(
    limits = limits,
    breaks = breaks
  )
}

make_panel_plot <- function(panel_df, sample_key, show_x_title = FALSE) {
  panel_df <- panel_df |>
    mutate(
      row_id = row_number(),
      y = rev(row_id)
    )

  axis_spec <- panel_axis_spec(panel_df, sample_key)
  x_limits <- axis_spec$limits
  x_breaks <- axis_spec$breaks
  family_label <- panel_df$family[[1]]
  family_color <- unname(family_colors[[family_label]])
  panel_name <- panel_df$panel[[1]]
  n <- nrow(panel_df)
  x_span <- diff(x_limits)
  label_x <- x_limits[1] - 0.52 * x_span
  title_x <- label_x
  family_x <- label_x + 0.24 * x_span
  stats_x1 <- x_limits[2] + 0.20 * x_span
  stats_x2 <- x_limits[2] + 0.46 * x_span
  stats_x3 <- x_limits[2] + 0.76 * x_span
  plot_x_min <- label_x - 0.02 * x_span
  plot_x_max <- x_limits[2] + 0.92 * x_span
  y_header <- n + 1.35
  y_rule <- n + 0.85

  ggplot(panel_df) +
    annotate(
      "segment",
      x = plot_x_min,
      xend = plot_x_max,
      y = y_rule,
      yend = y_rule,
      linewidth = 0.35,
      color = "grey45"
    ) +
    geom_vline(
      xintercept = 0,
      linewidth = 0.45,
      color = "black"
    ) +
    geom_errorbarh(
      aes(y = y, xmin = ci_low, xmax = ci_high),
      height = 0,
      linewidth = 0.45,
      color = "black"
    ) +
    geom_point(
      aes(x = estimate, y = y),
      size = 2.5,
      color = family_color
    ) +
    geom_text(
      aes(y = y, label = outcome),
      x = label_x,
      hjust = 0,
      size = 4.2,
      color = "black"
    ) +
    annotate(
      "text",
      x = title_x,
      y = y_header,
      label = panel_name,
      hjust = 0,
      fontface = "bold",
      size = 5.1
    ) +
    annotate(
      "text",
      x = family_x,
      y = y_header,
      label = family_label,
      hjust = 0,
      fontface = "bold",
      size = 5.1,
      color = family_color
    ) +
    annotate(
      "text",
      x = stats_x1,
      y = y_header,
      label = "Control mean",
      hjust = 0.5,
      fontface = "bold",
      size = 4.1
    ) +
    annotate(
      "text",
      x = stats_x2,
      y = y_header,
      label = "Control SD",
      hjust = 0.5,
      fontface = "bold",
      size = 4.1
    ) +
    annotate(
      "text",
      x = stats_x3,
      y = y_header,
      label = "Outcome range",
      hjust = 0.5,
      fontface = "bold",
      size = 4.1
    ) +
    geom_text(
      aes(
        x = stats_x1,
        y = y,
        label = sprintf("%.2f", control_mean)
      ),
      hjust = 0.5,
      size = 4.0,
      na.rm = TRUE
    ) +
    geom_text(
      aes(
        x = stats_x2,
        y = y,
        label = sprintf("%.2f", control_sd)
      ),
      hjust = 0.5,
      size = 4.0,
      na.rm = TRUE
    ) +
    geom_text(
      aes(
        x = stats_x3,
        y = y,
        label = outcome_range_display
      ),
      hjust = 0.5,
      size = 4.0,
      na.rm = TRUE
    ) +
    scale_x_continuous(
      limits = c(plot_x_min, plot_x_max),
      breaks = x_breaks,
      labels = scales::number_format(accuracy = 0.01)
    ) +
    scale_y_continuous(
      limits = c(0.5, n + 1.7),
      breaks = NULL
    ) +
    coord_cartesian(clip = "off") +
    labs(
      x = if (show_x_title) {
        "Average treatment effect, with 95% confidence interval"
      } else {
        NULL
      },
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 12, color = "black", margin = margin(t = 6)),
      axis.ticks.x = element_line(color = "black", linewidth = 0.35),
      axis.line.x = element_line(color = "black", linewidth = 0.45),
      plot.margin = margin(t = 4, r = 18, b = 4, l = 10)
    )
}

build_mock_plot <- function(plot_df, sample_key, batch_code) {
  panel_dfs <- plot_df |>
    group_split(panel_rank, .keep = TRUE)

  panel_dfs <- panel_dfs[order(map_int(panel_dfs, \(d) d$panel_rank[[1]]))]
  panel_heights <- map_dbl(panel_dfs, \(d) max(1.05, 0.18 * nrow(d) + 0.55))

  panel_plots <- map(
    seq_along(panel_dfs),
    function(i) {
      make_panel_plot(
        panel_dfs[[i]],
        sample_key = sample_key,
        show_x_title = i == length(panel_dfs)
      )
    }
  )

  wrap_plots(panel_plots, ncol = 1, heights = panel_heights) +
    plot_annotation(
      theme = theme(
        plot.margin = margin(t = 8, r = 8, b = 8, l = 8)
      )
    )
}

for (results_subdir in names(figure_specs)) {
  sample_key <- figure_specs[[results_subdir]]$sample_key
  output_dir <- file.path(output_root, results_subdir)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  estimate_files <- read_saved_plot_inputs(results_subdir)

  for (estimate_info in estimate_files) {
    estimate_path <- estimate_info$source_path[[1]]
    batch_code <- batch_from_name(basename(estimate_path))

    if (is.na(batch_code)) {
      next
    }

    plot_df <- prepare_plot_data(estimate_info, sample_key, batch_code)

    if (nrow(plot_df) == 0) {
      next
    }

    results_plot <- build_mock_plot(plot_df, sample_key, batch_code)
    plot_height <- max(4.5, 0.45 * nrow(plot_df) + 2.8)

    ggsave(
      filename = file.path(output_dir, mock_output_name(estimate_path)),
      plot = results_plot,
      device = cairo_pdf,
      width = 14,
      height = plot_height,
      units = "in"
    )
  }
}
