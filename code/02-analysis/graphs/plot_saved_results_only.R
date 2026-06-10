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
  SMIs = "Number of SMIs Followed",
  total_shares = "log Total Shares",
  total_comments = "log Total Comments",
  total_reactions = "log Total Reactions",
  ver = "log Verifiable RTs + Posts",
  non_ver = "log Non-Verifiable RTs + Posts",
  true = "log True RTs + Posts",
  fake = "log Fake RTs + Posts",
  n_posts = "log Number of RTs + Posts",
  eng = "log Number of RTs + Posts (English)",
  n_posts_covid = "log COVID RTs + Posts",
  pos_b_covid = "log Positive COVID RTs + Posts",
  neutral_b_covid = "log Neutral COVID RTs + Posts",
  neg_b_covid = "log Negative COVID RTs + Posts",
  n_posts_vax = "log Vaccine RTs + Posts",
  pos_b_vax = "log Positive Vaccine RTs + Posts",
  neutral_b_vax = "log Neutral Vaccine RTs + Posts",
  neg_b_vax = "log Negative Vaccine RTs + Posts",
  log_total_shares = "log Total Shares",
  log_total_comments = "log Total Comments",
  log_total_reactions = "log Total Reactions",
  log_verifiability = "log Verifiable Posts + Shares",
  log_non_ver = "log Non Verifiable Posts + Shares",
  log_true = "log True Posts + Shares",
  log_fake = "log Fake Posts + Shares",
  log_n_posts = "log Number of Posts + Shares",
  log_eng = "log Number of Posts + Shares (English)",
  log_pos_b_covid = "log Positive COVID Posts + Shares",
  log_neutral_b_covid = "log Neutral COVID Posts + Shares",
  log_neg_b_covid = "log Negative COVID Posts + Shares",
  log_n_posts_covid = "log COVID Posts + Shares",
  log_pos_b_vax = "log Positive Vaccine Posts + Shares",
  log_neutral_b_vax = "log Neutral Vaccine Posts + Shares",
  log_neg_b_vax = "log Negative Vaccine Posts + Shares",
  log_n_posts_vax = "log Vaccine Posts + Shares"
)

horizontal_order <- c(
  "SMIs",
  "AC",
  "total_shares",
  "total_comments",
  "total_reactions",
  "n_posts",
  "eng",
  "ver",
  "non_ver",
  "true",
  "fake",
  "n_posts_covid",
  "pos_b_covid",
  "neutral_b_covid",
  "neg_b_covid",
  "n_posts_vax",
  "pos_b_vax",
  "neutral_b_vax",
  "neg_b_vax",
  "log_total_shares",
  "log_total_comments",
  "log_total_reactions",
  "log_n_posts",
  "log_eng",
  "log_verifiability",
  "log_non_ver",
  "log_true",
  "log_fake",
  "log_n_posts_covid",
  "log_pos_b_covid",
  "log_neutral_b_covid",
  "log_neg_b_covid",
  "log_n_posts_vax",
  "log_pos_b_vax",
  "log_neutral_b_vax",
  "log_neg_b_vax"
)

stage_label_map <- c(
  AC = "Follows Africa Check",
  SMIs = "Number of SMIs Followed",
  fake = "log Fake Posts + Shares",
  true = "log True Posts + Shares",
  ver = "log Verifiable Posts + Shares",
  non_ver = "log Non Verifiable Posts + Shares",
  n_posts = "log Number of Posts + Shares",
  eng = "log Number of Posts + Shares (English)"
)

stage_order <- c("SMIs", "AC", "n_posts", "eng", "ver", "non_ver", "true", "fake")

stage_map <- c(
  stage1_2 = "Weeks 1-4",
  stage3_4 = "Weeks 5-8",
  stage5_6 = "Weeks 9-12"
)

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
    return("Batch 1")
  }

  if (grepl("_b2_", filename, fixed = TRUE)) {
    return("Batch 2")
  }

  if (grepl("_both_", filename, fixed = TRUE)) {
    return("Both Batches")
  }

  NA_character_
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
      Variable = vapply(var, resolve_label, character(1)),
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  final$Variable <- factor(
    final$Variable,
    levels = ordered_horizontal_labels(final$var, final$Variable)
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
    levels = ordered_horizontal_labels(final$var, final$Variable)
  )
  final
}

build_combined_horizontal_data <- function(original_dir, permutations_dir) {
  original_files <- list.files(original_dir, pattern = "\\.xlsx$", full.names = TRUE)

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

shape_palette <- c(15, 16, 17, 18, 3, 7, 8, 0, 1, 2, 4, 5, 6, 9, 10, 11, 12, 13, 14)

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

build_combined_stage_data <- function(estimates_dir) {
  estimate_files <- list.files(estimates_dir, pattern = "\\.xlsx$", full.names = TRUE)

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

plot_horizontal_family <- function(original_dir, permutations_dir, output_dir, xlab_text = "Total Treated Estimate with 95% Confidence Interval") {
  if (!dir.exists(original_dir) || !dir.exists(permutations_dir)) {
    return(invisible(NULL))
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  original_files <- list.files(original_dir, pattern = "\\.xlsx$", full.names = TRUE)

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

    results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
      geom_point() +
      geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
      coord_cartesian(xlim = x_bounds) +
      theme_bw() +
      xlab(xlab_text) +
      ylab("Variable") +
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

  estimate_files <- list.files(estimates_dir, pattern = "\\.xlsx$", full.names = TRUE)

  for (estimate_path in estimate_files) {
    filename <- basename(estimate_path)
    final <- build_estimate_only_horizontal_data(estimate_path)

    if (nrow(final) == 0) {
      next
    }

    x_bounds <- axis_bounds(final$lower, final$upper)
    plot_height <- max(7.25, 0.42 * nlevels(droplevels(final$Variable)) + 1.5)

    results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
      geom_point() +
      geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
      coord_cartesian(xlim = x_bounds) +
      theme_bw() +
      xlab(xlab_text) +
      ylab("Variable") +
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

plot_combined_balance_family <- function(original_dir, permutations_dir, output_path, family_title) {
  if (!dir.exists(original_dir) || !dir.exists(permutations_dir)) {
    return(invisible(NULL))
  }

  final <- build_combined_horizontal_data(original_dir, permutations_dir)

  if (nrow(final) == 0) {
    return(invisible(NULL))
  }

  final$Batch <- factor(final$Batch, levels = c("Batch 1", "Batch 2", "Both Batches"))
  x_bounds <- axis_bounds(final$lower, final$upper)
  plot_height <- max(6.59, 0.42 * nlevels(droplevels(final$Variable)) + 1.5)

  results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
    geom_point() +
    geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    coord_cartesian(xlim = x_bounds) +
    facet_wrap(~Batch, nrow = 1) +
    labs(
      title = family_title,
      x = "Total Treated Estimate with 95% Confidence Interval",
      y = "Variable"
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold")
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

plot_stage_estimates <- function(estimates_dir, output_dir, ylab_text = "Total Treated Estimate with 95% Confidence Interval") {
  if (!dir.exists(estimates_dir)) {
    return(invisible(NULL))
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  estimate_files <- list.files(estimates_dir, pattern = "\\.xlsx$", full.names = TRUE)

  for (estimate_path in estimate_files) {
    filename <- basename(estimate_path)
    final <- build_stage_plot_data(estimate_path)

    if (nrow(final) == 0) {
      next
    }

    n_vars <- nlevels(droplevels(final$Variable))

    if (n_vars > length(shape_palette)) {
      stop(
        paste0(
          "plot_stage_estimates only has ", length(shape_palette),
          " point shapes configured, but received ", n_vars, " outcomes in ", filename, "."
        ),
        call. = FALSE
      )
    }

    y_bounds <- axis_bounds(final$lower, final$upper)
    plot_height <- max(7.25, 0.38 * n_vars + 2.25)

    results_plot <- ggplot(final, aes(x = Stage, y = coef)) +
      geom_point(
        aes(shape = Variable, color = Variable),
        size = 3,
        position = position_dodge(width = 0.5)
      ) +
      geom_linerange(
        aes(ymin = lower, ymax = upper, color = Variable),
        position = position_dodge(width = 0.5),
        linewidth = 1
      ) +
      scale_shape_manual(values = shape_palette[seq_len(n_vars)], name = "Outcome") +
      scale_color_manual(values = rep("black", n_vars), name = "Outcome") +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
      coord_cartesian(ylim = y_bounds) +
      theme_bw() +
      ylab(ylab_text) +
      xlab("Stage") +
      theme(
        panel.grid.major = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
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

plot_combined_stage_family <- function(estimates_dir, output_path, family_title, ylab_text = "Total Treated Estimate with 95% Confidence Interval") {
  if (!dir.exists(estimates_dir)) {
    return(invisible(NULL))
  }

  final <- build_combined_stage_data(estimates_dir)

  if (nrow(final) == 0) {
    return(invisible(NULL))
  }

  final$Batch <- factor(final$Batch, levels = c("Batch 1", "Batch 2", "Both Batches"))
  n_vars <- nlevels(droplevels(final$Variable))

  if (n_vars > length(shape_palette)) {
    stop(
      paste0(
        "plot_combined_stage_family only has ", length(shape_palette),
        " point shapes configured, but received ", n_vars, " outcomes in ", family_title, "."
      ),
      call. = FALSE
    )
  }

  y_bounds <- axis_bounds(final$lower, final$upper)
  plot_height <- max(6.59, 0.38 * n_vars + 2.25)

  results_plot <- ggplot(final, aes(x = Stage, y = coef)) +
    geom_point(
      aes(shape = Variable, color = Variable),
      size = 3,
      position = position_dodge(width = 0.5)
    ) +
    geom_linerange(
      aes(ymin = lower, ymax = upper, color = Variable),
      position = position_dodge(width = 0.5),
      linewidth = 1
    ) +
    scale_shape_manual(values = shape_palette[seq_len(n_vars)], name = "Outcome") +
    scale_color_manual(values = rep("black", n_vars), name = "Outcome") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    coord_cartesian(ylim = y_bounds) +
    facet_wrap(~Batch, nrow = 1) +
    labs(
      title = family_title,
      y = ylab_text,
      x = "Stage"
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
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
