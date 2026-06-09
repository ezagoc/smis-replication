# 0.0 Set up the environment and locate this script
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(purrr)
library(fastDummies)

src_path <- "../../../src/utils/"
source_files <- c(
  "funcs_analysis.R",
  "constants_final.R",
  "funcs.R"
)

map(paste0(src_path, source_files), source)
ipak(packages)

aggregate_env <- new.env(parent = globalenv())
sys.source("../pre-process/aggregate_batches_intensive.R", envir = aggregate_env)
aggregate_data <- aggregate_env$aggregate_data

results_root <- file.path("../../../results", "intensive_aggregate_batches")
original_dir <- file.path(results_root, "original")
permutations_dir <- file.path(results_root, "permutations")
estimates_dir <- file.path(results_root, "estimates")
plots_dir <- file.path(results_root, "plots")

file_stub <- "log_intensive_aggregate_batches"
n_permutations <- 1000
permutation_suffix <- paste0("_", n_permutations, "perm")
influencer_thr <- 9
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

add_weights <- function(data) {
  data |>
    mutate(weights = ifelse(total_influencers > 0, 1 / total_influencers, NA_real_)) |>
    filter(!is.na(weights))
}

build_interactions <- function(data) {
  interactions <- generate_interactions(
    data |>
      select(follower_id, pais, batch_id, total_treated, total_influencers)
  )

  int_cols <- grep("^tao_", names(interactions), value = TRUE)

  list(
    data = data |>
      left_join(interactions, by = c("follower_id", "pais", "batch_id")),
    int_cols = int_cols
  )
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

extract_weighted_treated <- function(data, outcome_vars, int_cols, context_label = "", allow_missing = FALSE) {
  estimates <- map_dbl(outcome_vars, function(outcome_var) {
    rhs_terms <- c("total_treated", paste0(outcome_var, "_base"), int_cols)
    rhs_terms <- rhs_terms[nzchar(rhs_terms)]

    fmla <- as.formula(
      paste0(
        outcome_var, " ~ ",
        paste(rhs_terms, collapse = " + "),
        " | total_influencers + pais + batch_id"
      )
    )

    fit <- tryCatch(
      feols(fmla, data = data, weights = ~weights),
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

  stats::setNames(as.list(estimates), outcome_vars) |>
    as_tibble()
}

label_map <- c(
  log_total_shares = "log Total Shares",
  log_total_reactions = "log Total Reactions",
  log_total_comments = "log Total Comments",
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

ordered_vars <- c(
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

summarise_aggregate_results <- function(original_coefs, permutation_coefs, outcome_vars) {
  original_coefs |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "coef") |>
    left_join(
      permutation_coefs |>
        summarise(across(everything(), \(x) sd(x, na.rm = TRUE))) |>
        pivot_longer(cols = everything(), names_to = "var", values_to = "sd"),
      by = "var"
    ) |>
    filter(var %in% outcome_vars) |>
    mutate(
      Variable = ifelse(
        var %in% names(label_map),
        unname(label_map[var]),
        paste0("log ", gsub("_", " ", sub("^log_", "", var)))
      ),
      Stage = period_label
    )
}

write_outputs <- function(batch_name, original_coefs, permutation_coefs, final) {
  file_code <- paste0(file_stub, "_", batch_name, permutation_suffix)

  write_xlsx(original_coefs, file.path(original_dir, paste0(file_code, ".xlsx")))
  write_xlsx(permutation_coefs, file.path(permutations_dir, paste0(file_code, ".xlsx")))
  write_xlsx(final, file.path(estimates_dir, paste0(file_code, "_estimates.xlsx")))
}

all_base_vars <- grep("_base$", names(aggregate_data), value = TRUE)
all_endline_vars <- setdiff(names(aggregate_data), c("follower_id", "pais", "batch_id", all_base_vars))
paired_roots <- all_endline_vars[paste0(all_endline_vars, "_base") %in% all_base_vars]

analysis_df <- aggregate_data |>
  filter(total_influencers < influencer_thr) |>
  mutate(
    across(all_of(paired_roots), \(x) log(x + 1), .names = "log_{.col}"),
    across(all_of(paste0(paired_roots, "_base")), \(x) log(x + 1), .names = "log_{.col}")
  )

outcome_vars <- paste0("log_", paired_roots)
outcome_vars <- outcome_vars[outcome_vars %in% names(analysis_df)]
outcome_vars <- outcome_vars[paste0(outcome_vars, "_base") %in% names(analysis_df)]

for (batch_name in names(batch_specs)) {
  message("Running aggregated intensive sample: ", batch_name)

  batch_filter <- batch_specs[[batch_name]]
  base_df <- analysis_df |>
    prepare_batch_data(batch_filter) |>
    add_weights()

  keep_cols <- c("follower_id", "pais", "batch_id", "total_treated", "total_influencers", "weights", outcome_vars, paste0(outcome_vars, "_base"))
  base_df <- base_df |>
    select(all_of(keep_cols))

  interaction_fit <- build_interactions(base_df)

  original_coefs <- extract_weighted_treated(
    data = interaction_fit$data,
    outcome_vars = outcome_vars,
    int_cols = interaction_fit$int_cols,
    context_label = paste(batch_name, "aggregate intensive original")
  )

  permutation_coefs <- map_dfr(seq_len(n_permutations), function(i) {
    message("Permutation ", i, " / ", n_permutations, " for ", batch_name, " - aggregate intensive")

    perm_treated_col <- paste0("n_influencers_followed_treatment_p", i)
    perm_total_col <- paste0("n_influencers_followed_p_", i)

    permuted_counts <- read_parquet(
      paste0(
        "../../../data/04-analysis/joint/small_ties_b1b2/small_tie",
        i,
        ".parquet"
      )
    ) |>
      select(follower_id, pais, batch_id, all_of(c(perm_treated_col, perm_total_col)))

    permuted_df <- base_df |>
      left_join(permuted_counts, by = c("follower_id", "batch_id", "pais"))

    permuted_df$total_treated <- permuted_df[[perm_treated_col]]
    permuted_df$total_influencers <- permuted_df[[perm_total_col]]
    permuted_df[[perm_treated_col]] <- NULL
    permuted_df[[perm_total_col]] <- NULL
    permuted_df <- add_weights(permuted_df)

    permuted_interactions <- build_interactions(permuted_df)

    extract_weighted_treated(
      data = permuted_interactions$data,
      outcome_vars = outcome_vars,
      int_cols = permuted_interactions$int_cols,
      context_label = paste(batch_name, "aggregate intensive permutation", i),
      allow_missing = TRUE
    )
  })

  final <- summarise_aggregate_results(
    original_coefs = original_coefs,
    permutation_coefs = permutation_coefs,
    outcome_vars = outcome_vars
  )

  ordered_labels <- unname(label_map[ordered_vars[ordered_vars %in% outcome_vars]])
  extra_labels <- setdiff(unique(final$Variable), ordered_labels)

  final$Variable <- factor(
    final$Variable,
    levels = c(ordered_labels, sort(extra_labels))
  )

  write_outputs(
    batch_name = batch_name,
    original_coefs = original_coefs,
    permutation_coefs = permutation_coefs,
    final = final
  )

  x_bounds <- axis_bounds(final$coef - 1.96 * final$sd, final$coef + 1.96 * final$sd)

  results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
    geom_point() +
    geom_linerange(aes(xmin = coef - 1.96 * sd, xmax = coef + 1.96 * sd), size = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    coord_cartesian(xlim = x_bounds) +
    theme_bw() +
    xlab("Total Treated Estimate with 95% Confidence Interval") +
    ylab("Outcome") +
    theme(
      panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
      panel.grid.minor = element_blank()
    )

  ggsave(
    plot = results_plot,
    filename = file.path(plots_dir, paste0(file_stub, "_", batch_name, permutation_suffix, ".pdf")),
    device = cairo_pdf,
    width = 9.5,
    height = 8.5,
    units = "in"
  )
}

