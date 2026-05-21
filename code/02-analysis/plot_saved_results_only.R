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
combined_root <- file.path(replots_root, "combined")

dir.create(replots_root, showWarnings = FALSE, recursive = TRUE)
dir.create(combined_root, showWarnings = FALSE, recursive = TRUE)

# 3.0 Helpers
label_map <- c(
  ver = "Verifiable RTs + Posts",
  non_ver = "Non-Verifiable RTs + Posts",
  true = "True RTs + Posts",
  fake = "Fake RTs + Posts",
  n_posts = "Number of RTs + Posts",
  eng = "Number of RTs + Posts (English)"
)

stage_map <- c(
  stage1_2 = "Weeks 1-4",
  stage3_4 = "Weeks 5-8",
  stage5_6 = "Weeks 9-12"
)

addon_from_name <- function(filename) {
  if (startsWith(filename, "log_")) {
    return("log ")
  }

  if (startsWith(filename, "arc_")) {
    return("arcsinh ")
  }

  ""
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

read_excel_short_path <- function(path) {
  short_copy <- tempfile(pattern = "xlsx_", tmpdir = tempdir(), fileext = ".xlsx")
  on.exit(unlink(short_copy), add = TRUE)
  file.copy(path, short_copy, overwrite = TRUE)
  readxl::read_excel(short_copy)
}

ordered_labels <- function(vars, addon) {
  base_order <- c("fake", "true", "ver", "non_ver", "eng", "n_posts")
  base_order <- base_order[base_order %in% vars]
  paste0(addon, unname(label_map[base_order]))
}

ordered_stage_labels <- function(vars, addon) {
  stage_label_map <- c(
    fake = "Fake Posts + Shares",
    true = "True Posts + Shares",
    ver = "Verifiable Posts + Shares",
    non_ver = "Non Verifiable Posts + Shares",
    n_posts = "Number of Posts + Shares",
    eng = "Number of Posts + Shares (English)"
  )

  base_order <- c("n_posts", "non_ver", "ver", "true", "fake", "eng")
  base_order <- base_order[base_order %in% vars]
  paste0(addon, unname(stage_label_map[base_order]))
}

build_horizontal_plot_data <- function(original_path, permutation_path) {
  original <- read_excel_short_path(original_path)
  permutation <- read_excel_short_path(permutation_path)
  addon <- addon_from_name(basename(original_path))

  final <- original |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "coef") |>
    left_join(
      permutation |>
        summarise(across(everything(), sd, na.rm = TRUE)) |>
        pivot_longer(cols = everything(), names_to = "var", values_to = "sd"),
      by = "var"
    ) |>
    mutate(
      Variable = paste0(addon, unname(label_map[var]))
    )

  final$Variable <- factor(
    final$Variable,
    levels = ordered_labels(final$var, addon)
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
      mutate(
        Batch = batch_from_name(filename),
        lower = coef - 1.96 * sd,
        upper = coef + 1.96 * sd
      )
  })
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

    final <- build_horizontal_plot_data(original_path, permutation_path) |>
      mutate(
        lower = coef - 1.96 * sd,
        upper = coef + 1.96 * sd
      )
    x_bounds <- axis_bounds(final$lower, final$upper)

    results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
      geom_point() +
      geom_linerange(aes(xmin = lower, xmax = upper), size = 1) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.5) +
      coord_cartesian(xlim = x_bounds) +
      theme_bw() +
      xlab("Total Treated Estimate with 95% Confidence Interval") +
      ylab("Variable") +
      theme(
        panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
        panel.grid.minor = element_blank()
      )

    ggsave(
      plot = results_plot,
      filename = file.path(output_dir, sub("\\.xlsx$", ".pdf", filename)),
      device = cairo_pdf,
      width = 8.22,
      height = 6.59,
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

  results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
    geom_point() +
    geom_linerange(aes(xmin = lower, xmax = upper), size = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    coord_cartesian(xlim = x_bounds) +
    facet_wrap(~Batch, nrow = 1) +
    labs(
      title = family_title,
      x = "Total Treated Estimate with 95% Confidence Interval",
      y = "Variable"
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  ggsave(
    plot = results_plot,
    filename = output_path,
    device = cairo_pdf,
    width = 12,
    height = 6.59,
    units = "in"
  )
}

build_combined_stage_data <- function(estimates_dir) {
  estimate_files <- list.files(estimates_dir, pattern = "\\.xlsx$", full.names = TRUE)

  map_dfr(estimate_files, function(estimate_path) {
    filename <- basename(estimate_path)
    addon <- addon_from_name(filename)
    final <- read_excel_short_path(estimate_path)

    if (!all(c("coef", "sd", "Stage", "Variable") %in% names(final))) {
      return(tibble())
    }

    if ("stage" %in% names(final)) {
      final <- final |>
        mutate(Stage = recode(stage, !!!stage_map))
    }

    if ("var" %in% names(final)) {
      final$Variable <- factor(final$Variable, levels = ordered_stage_labels(final$var, addon))
    }

    final |>
      mutate(
        Batch = batch_from_name(filename),
        lower = coef - 1.96 * sd,
        upper = coef + 1.96 * sd
      )
  })
}

plot_combined_stage_family <- function(estimates_dir, output_path, family_title) {
  if (!dir.exists(estimates_dir)) {
    return(invisible(NULL))
  }

  final <- build_combined_stage_data(estimates_dir)

  if (nrow(final) == 0) {
    return(invisible(NULL))
  }

  final$Batch <- factor(final$Batch, levels = c("Batch 1", "Batch 2", "Both Batches"))
  final$Stage <- factor(final$Stage, levels = unname(stage_map))
  y_bounds <- axis_bounds(final$lower, final$upper)

  results_plot <- ggplot(final, aes(x = Stage, y = coef)) +
    geom_point(
      aes(shape = Variable, color = Variable),
      size = 3,
      position = position_dodge(width = 0.5)
    ) +
    geom_linerange(
      aes(
        ymin = lower,
        ymax = upper,
        color = Variable
      ),
      position = position_dodge(width = 0.5),
      size = 1
    ) +
    scale_shape_manual(values = c(15, 16, 17, 4, 7, 18)[seq_along(levels(factor(final$Variable)))], name = "Outcome") +
    scale_color_manual(values = rep("black", length(levels(factor(final$Variable)))), name = "Outcome") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    coord_cartesian(ylim = y_bounds) +
    facet_wrap(~Batch, nrow = 1) +
    labs(
      title = family_title,
      y = "Total Treated Estimate with 95% Confidence Interval",
      x = "Stage"
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  ggsave(
    plot = results_plot,
    filename = output_path,
    device = cairo_pdf,
    width = 12,
    height = 6.59,
    units = "in"
  )
}

plot_stage_estimates <- function(estimates_dir, output_dir) {
  if (!dir.exists(estimates_dir)) {
    return(invisible(NULL))
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  estimate_files <- list.files(estimates_dir, pattern = "\\.xlsx$", full.names = TRUE)

  for (estimate_path in estimate_files) {
    filename <- basename(estimate_path)
    addon <- addon_from_name(filename)

    final <- read_excel_short_path(estimate_path)

    if (!all(c("coef", "sd", "Stage", "Variable") %in% names(final))) {
      next
    }

    if ("stage" %in% names(final)) {
      final <- final |>
        mutate(Stage = recode(stage, !!!stage_map))
    }

    final <- final |>
      mutate(
        lower = coef - 1.96 * sd,
        upper = coef + 1.96 * sd
      )
    y_bounds <- axis_bounds(final$lower, final$upper)

    if ("var" %in% names(final)) {
      var_levels <- ordered_stage_labels(final$var, addon)
    } else {
      var_levels <- unique(final$Variable)
    }

    final$Variable <- factor(final$Variable, levels = var_levels)
    final$Stage <- factor(final$Stage, levels = unname(stage_map))

    results_plot <- ggplot(final, aes(x = Stage, y = coef)) +
      geom_point(
        aes(shape = Variable, color = Variable),
        size = 3,
        position = position_dodge(width = 0.5)
      ) +
      geom_linerange(
        aes(
          ymin = lower,
          ymax = upper,
          color = Variable
        ),
        position = position_dodge(width = 0.5),
        size = 1
      ) +
      scale_shape_manual(values = c(15, 16, 17, 4, 7, 18)[seq_along(levels(final$Variable))], name = "Outcome") +
      scale_color_manual(values = rep("black", length(levels(final$Variable))), name = "Outcome") +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
      coord_cartesian(ylim = y_bounds) +
      theme_bw() +
      ylab("Total Treated Estimate with 95% Confidence Interval") +
      xlab("Stage") +
      theme(
        panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggsave(
      plot = results_plot,
      filename = file.path(output_dir, sub("_estimates\\.xlsx$", ".pdf", filename)),
      device = cairo_pdf,
      width = 8.22,
      height = 6.59,
      units = "in"
    )
  }
}

# 4.0 Rebuild plots from saved results only
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

