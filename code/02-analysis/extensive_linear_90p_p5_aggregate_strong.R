# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)

src_path <- "../../src/utils/"
source_files <- c(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)

map(paste0(src_path, source_files), source)
ipak(packages)

# 2.0 Define constants
country <- "joint"
data_type <- "Verifiability"
list_stages <- c("stage1_2", "stage3_4", "stage5_6")
list_types <- c("log_")
file_stub <- "extensive_verifiability_fes_normal_p90_p5_aggregate_strong"
n_posts_thr <- 0
n_permutations <- 500

results_root <- file.path("../../results", "extensive_linear_90p_p5_aggregate_strong")
original_dir <- file.path(results_root, "original")
permutations_dir <- file.path(results_root, "permutations")
estimates_dir <- file.path(results_root, "estimates")
plots_dir <- file.path(results_root, "plots")

outcome_roots <- c("verifiability", "non_ver", "true", "fake", "n_posts")
base_outcomes <- paste0(outcome_roots, "_base")
output_names <- c("ver", "non_ver", "true", "fake", "n_posts")
period_label <- "Weeks 1-12"
batch_specs <- list(
  b1 = "b1",
  b2 = "b2",
  both = NULL
)

dir.create(results_root, showWarnings = FALSE, recursive = TRUE)
dir.create(original_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(permutations_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(estimates_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

prepare_batch_data <- function(data, batch_filter = NULL) {
  if (is.null(batch_filter)) {
    return(data)
  }

  data |>
    filter(batch_id == batch_filter)
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

extract_total_treated <- function(data, outcome_vars, context_label = "", allow_missing = FALSE) {
  estimates <- map_dbl(outcome_vars, function(outcome_var) {
    fmla <- as.formula(
      paste0(
        outcome_var, " ~ total_treated + ", outcome_var, "_base | ",
        "pais + batch_id"
      )
    )

    fit <- tryCatch(
      feols(fmla, data = data),
      error = function(e) e
    )

    if (inherits(fit, "error")) {
      detail <- paste0(
        "feols failed for ", outcome_var,
        if (nzchar(context_label)) paste0(" (", context_label, ")") else "",
        ": ", conditionMessage(fit)
      )

      if (allow_missing) {
        warning(detail, call. = FALSE)
        return(NA_real_)
      }

      stop(detail, call. = FALSE)
    }

    fit_coefs <- coef(fit)
    has_total_treated <- "total_treated" %in% names(fit_coefs)
    estimate <- if (has_total_treated) unname(fit_coefs[["total_treated"]]) else NA_real_

    if (!has_total_treated || length(estimate) == 0 || is.na(estimate)) {
      detail <- paste0(
        "Missing total_treated estimate for ", outcome_var,
        if (nzchar(context_label)) paste0(" (", context_label, ")") else ""
      )

      if (allow_missing) {
        warning(detail, call. = FALSE)
        return(NA_real_)
      }

      stop(detail, call. = FALSE)
    }

    estimate
  })

  stats::setNames(as.list(estimates), output_names) |>
    as_tibble()
}

summarise_aggregate_results <- function(original_coefs, permutation_coefs, type) {
  addon <- if (type == "log_") {
    "log "
  } else if (type == "arc_") {
    "arcsinh "
  } else {
    ""
  }

  original_coefs |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "coef") |>
    left_join(
      permutation_coefs |>
        summarise(across(everything(), sd, na.rm = TRUE)) |>
        pivot_longer(cols = everything(), names_to = "var", values_to = "sd"),
      by = "var"
    ) |>
    mutate(
      Variable = case_when(
        var == "ver" ~ paste0(addon, "Verifiable Posts + Shares"),
        var == "non_ver" ~ paste0(addon, "Non Verifiable Posts + Shares"),
        var == "true" ~ paste0(addon, "True Posts + Shares"),
        var == "fake" ~ paste0(addon, "Fake Posts + Shares"),
        var == "n_posts" ~ paste0(addon, "Number of Posts + Shares")
      ),
      Stage = period_label
    )
}

write_outputs <- function(type, batch_name, original_coefs, permutation_coefs) {
  file_code <- paste0(type, file_stub, "_", batch_name, "_500perm")

  write_xlsx(
    original_coefs,
    file.path(original_dir, paste0(file_code, ".xlsx"))
  )

  write_xlsx(
    permutation_coefs,
    file.path(permutations_dir, paste0(file_code, ".xlsx"))
  )
}

ensure_extensive_totals <- function(df) {
  df <- df |>
    mutate(
      verifiability = if ("verifiability" %in% names(.)) verifiability else verifiability_rt + verifiability_no_rt,
      non_ver = if ("non_ver" %in% names(.)) non_ver else non_ver_rt + non_ver_no_rt,
      true = if ("true" %in% names(.)) true else true_rt + true_no_rt,
      fake = if ("fake" %in% names(.)) fake else fake_rt + fake_no_rt,
      n_posts = if ("n_posts" %in% names(.)) n_posts else n_posts_rt + n_posts_no_rt,
      verifiability_base = if ("verifiability_base" %in% names(.)) verifiability_base else verifiability_rt_base + verifiability_no_rt_base,
      non_ver_base = if ("non_ver_base" %in% names(.)) non_ver_base else non_ver_rt_base + non_ver_no_rt_base,
      true_base = if ("true_base" %in% names(.)) true_base else true_rt_base + true_no_rt_base,
      fake_base = if ("fake_base" %in% names(.)) fake_base else fake_rt_base + fake_no_rt_base,
      n_posts_base = if ("n_posts_base" %in% names(.)) n_posts_base else n_posts_rt_base + n_posts_no_rt_base
    )

  missing_cols <- setdiff(c(outcome_roots, base_outcomes), names(df))

  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing extensive aggregate columns after reconstruction:",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  df
}

read_stage_verifiability <- function(stage) {
  df_ke <- read_parquet(
    file.path("../../data/04-analysis/KE", stage, "verifiability_b1b2.parquet")
  ) |>
    mutate(pais = "KE")

  df_sa <- read_parquet(
    file.path("../../data/04-analysis/SA", stage, "verifiability_b1b2.parquet")
  ) |>
    mutate(pais = "SA")

  bind_rows(df_sa, df_ke) |>
    mutate(
      total_treated = t_strong + t_weak + t_neither,
      total_influencers = c_t_strong_total + c_t_weak_total + c_t_neither_total
    ) |>
    ensure_extensive_totals()
}

build_stage_slice <- function(stage) {
  read_stage_verifiability(stage) |>
    select(follower_id, pais, batch_id, all_of(outcome_roots)) |>
    rename_with(
      ~paste0(.x, "__", stage),
      all_of(outcome_roots)
    )
}

build_aggregated_verifiability_data <- function() {
  id_cols <- c("follower_id", "pais", "batch_id")
  static_cols <- c("total_treated", "total_influencers", "c_t_strong_total")

  base_df <- read_stage_verifiability(list_stages[[1]]) |>
    select(all_of(id_cols), all_of(static_cols), all_of(base_outcomes))

  stage_slices <- map(list_stages, build_stage_slice)

  aggregated_df <- reduce(
    stage_slices,
    left_join,
    .init = base_df,
    by = id_cols
  ) |>
    rowwise() |>
    mutate(
      across(
        all_of(outcome_roots),
        \(x) sum(c_across(all_of(paste0(cur_column(), "__", list_stages))), na.rm = TRUE)
      )
    ) |>
    ungroup() |>
    select(all_of(id_cols), all_of(static_cols), all_of(base_outcomes), all_of(outcome_roots))

  aggregated_df <- aggregated_df |>
    mutate(
      across(
        all_of(c(base_outcomes, outcome_roots)),
        \(x) Winsorize(x, probs = c(0, .94), na.rm = TRUE)
      )
    )

  log_df <- aggregated_df |>
    select(all_of(id_cols), all_of(c(base_outcomes, outcome_roots))) |>
    mutate(
      across(
        all_of(c(base_outcomes, outcome_roots)),
        \(x) log(x + 1)
      )
    ) |>
    rename_with(
      ~paste0("log_", .x),
      all_of(c(base_outcomes, outcome_roots))
    )

  aggregated_df |>
    left_join(log_df, by = id_cols)
}

# 3.0 Load shared filters
belp90 <- read_parquet("../../data/04-analysis/joint/below_p90_p95_divider.parquet") |>
  select(-n_posts_base)

# 4.0 Build the aggregated full-period data once
aggregated_df <- build_aggregated_verifiability_data() |>
  left_join(belp90, by = c("follower_id", "batch_id", "pais")) |>
  filter(total_influencers == 1) |>
  filter(c_t_strong_total > 0) |>
  filter(n_posts_base > n_posts_thr) |>
  filter(below_p90 == 1)

# 5.0 Run the specification for each batch sample
for (type in list_types) {
  outcome_vars <- paste0(type, outcome_roots)

  for (batch_name in names(batch_specs)) {
    message("Running batch sample: ", batch_name)

    batch_filter <- batch_specs[[batch_name]]

    base_df <- aggregated_df |>
      prepare_batch_data(batch_filter)

    original_coefs <- extract_total_treated(
      data = base_df,
      outcome_vars = outcome_vars,
      context_label = paste(batch_name, "aggregated strong original")
    )

    permutation_coefs <- map_dfr(seq_len(n_permutations), function(i) {
      message("Permutation ", i, " / ", n_permutations, " for ", batch_name, " - aggregated strong")

      perm_treated_col <- paste0("n_influencers_followed_treatment_p", i)
      perm_total_col <- paste0("n_influencers_followed_p_", i)

      permuted_counts <- read_parquet(
        paste0(
          "../../data/04-analysis/joint/small_ties_b1b2/small_tie",
          i,
          ".parquet"
        )
      ) |>
        select(follower_id, pais, batch_id, all_of(c(perm_treated_col, perm_total_col)))

      permuted_df <- base_df |>
        left_join(permuted_counts, by = c("follower_id", "batch_id", "pais"))

      permuted_df <- poolTreatmentBalance2(permuted_df, perm_treated_col, perm_total_col)
      permuted_df[[perm_treated_col]] <- NULL
      permuted_df[[perm_total_col]] <- NULL

      extract_total_treated(
        data = permuted_df,
        outcome_vars = outcome_vars,
        context_label = paste(batch_name, "aggregated strong permutation", i),
        allow_missing = TRUE
      )
    })

    write_outputs(
      type = type,
      batch_name = batch_name,
      original_coefs = original_coefs,
      permutation_coefs = permutation_coefs
    )

    final <- summarise_aggregate_results(
      original_coefs = original_coefs,
      permutation_coefs = permutation_coefs,
      type = type
    )

    final$Variable <- factor(
      final$Variable,
      levels = c(
        "log Number of Posts + Shares",
        "log Non Verifiable Posts + Shares",
        "log Verifiable Posts + Shares",
        "log True Posts + Shares",
        "log Fake Posts + Shares"
      )
    )

    final$Stage <- factor(final$Stage, levels = period_label)

    write_xlsx(
      final,
      file.path(
        estimates_dir,
        paste0(type, file_stub, "_", batch_name, "_500perm_estimates.xlsx")
      )
    )

    y_bounds <- axis_bounds(
      final$coef - 1.96 * final$sd,
      final$coef + 1.96 * final$sd
    )

    results_plot <- ggplot(data = final, aes(x = Stage, y = coef)) +
      geom_point(
        aes(shape = Variable, color = Variable),
        size = 3,
        position = position_dodge(width = 0.5)
      ) +
      geom_linerange(
        aes(
          ymin = coef - 1.96 * sd,
          ymax = coef + 1.96 * sd,
          color = Variable
        ),
        position = position_dodge(width = 0.5),
        size = 1
      ) +
      scale_shape_manual(values = c(15, 16, 17, 4, 7), name = "Outcome") +
      scale_color_manual(values = rep("black", 5), name = "Outcome") +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
      coord_cartesian(ylim = y_bounds) +
      theme_bw() +
      ylab("Total Treated Estimate with 95% Confidence Interval") +
      xlab("Period") +
      theme(
        panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggsave(
      plot = results_plot,
      filename = file.path(
        plots_dir,
        paste0(type, file_stub, "_", batch_name, "_500perm.pdf")
      ),
      device = cairo_pdf,
      width = 8.22,
      height = 6.59,
      units = "in"
    )
  }
}
