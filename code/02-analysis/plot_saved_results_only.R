# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
results_root <- "../../results"
replots_root <- file.path(results_root, "replots")

dir.create(replots_root, showWarnings = FALSE, recursive = TRUE)

# 3.0 Labels and helpers
label_map <- c(
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
  fake = "log Fake Posts + Shares",
  true = "log True Posts + Shares",
  ver = "log Verifiable Posts + Shares",
  non_ver = "log Non Verifiable Posts + Shares",
  n_posts = "log Number of Posts + Shares",
  eng = "log Number of Posts + Shares (English)"
)

stage_order <- c("n_posts", "eng", "ver", "non_ver", "true", "fake")

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

ordered_horizontal_labels <- function(vars) {
  ordered <- horizontal_order[horizontal_order %in% vars]
  ordered_labels <- unname(label_map[ordered])
  extra_vars <- setdiff(vars, ordered)
  extra_labels <- vapply(extra_vars, resolve_label, character(1))
  c(ordered_labels, sort(extra_labels))
}

ordered_stage_labels <- function(vars) {
  ordered <- stage_order[stage_order %in% vars]
  ordered_labels <- unname(stage_label_map[ordered])
  extra_vars <- setdiff(vars, ordered)
  extra_labels <- vapply(
    extra_vars,
    function(x) if (x %in% names(stage_label_map)) unname(stage_label_map[x]) else gsub("_", " ", x),
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

  final$Variable <- factor(final$Variable, levels = ordered_horizontal_labels(final$var))
  final
}

plot_horizontal_family <- function(original_dir, permutations_dir, output_dir) {
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
    x_bounds <- axis_bounds(final$lower, final$upper)

    results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
      geom_point() +
      geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
      coord_cartesian(xlim = x_bounds) +
      theme_bw() +
      xlab("Total Treated Estimate with 95% Confidence Interval") +
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
      height = 7.25,
      units = "in"
    )
  }
}

build_stage_plot_data <- function(estimate_path) {
  final <- read_excel_short_path(estimate_path)

  if (!all(c("coef", "sd", "Stage", "Variable") %in% names(final))) {
    return(tibble())
  }

  if ("stage" %in% names(final)) {
    final <- final |>
      mutate(Stage = recode(stage, !!!stage_map))
  }

  if ("var" %in% names(final)) {
    final$Variable <- vapply(
      seq_len(nrow(final)),
      function(i) {
        if (!is.na(final$var[[i]]) && final$var[[i]] %in% names(stage_label_map)) {
          unname(stage_label_map[final$var[[i]]])
        } else {
          final$Variable[[i]]
        }
      },
      character(1)
    )
  }

  final <- final |>
    mutate(
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  if ("var" %in% names(final)) {
    final$Variable <- factor(final$Variable, levels = ordered_stage_labels(final$var))
  } else {
    final$Variable <- factor(final$Variable, levels = unique(final$Variable))
  }

  final$Stage <- factor(final$Stage, levels = unname(stage_map))
  final
}

plot_stage_estimates <- function(estimates_dir, output_dir) {
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

    y_bounds <- axis_bounds(final$lower, final$upper)
    shape_values <- c(15, 16, 17, 18, 3, 7, 8, 0)
    n_vars <- nlevels(final$Variable)

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
      scale_shape_manual(values = shape_values[seq_len(n_vars)], name = "Outcome") +
      scale_color_manual(values = rep("black", n_vars), name = "Outcome") +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
      coord_cartesian(ylim = y_bounds) +
      theme_bw() +
      ylab("Total Treated Estimate with 95% Confidence Interval") +
      xlab("Period") +
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
      height = 7.25,
      units = "in"
    )
  }
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
