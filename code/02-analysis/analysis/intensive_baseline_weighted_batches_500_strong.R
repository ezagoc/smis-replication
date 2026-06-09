# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
library(fastDummies)

src_path <- "../../../src/utils/"
source_files <- c(
  "funcs_analysis.R",
  "constants_final.R",
  "import_data.R"
)

map(paste0(src_path, source_files), source)
ipak(packages)

# 2.0 Define constants
country <- "joint"
data_type <- "Baseline"
stage <- "stage1_2"
file_stub <- "intensive_baseline_weighted_strong"
influencer_thr <- 9
n_posts_thr <- 0
n_permutations <- 1000
permutation_suffix <- paste0("_", n_permutations, "perm")

results_root <- file.path("../../../results", "intensive_baseline_weighted_strong")
original_dir <- file.path(results_root, "original")
permutations_dir <- file.path(results_root, "permutations")
plots_dir <- file.path(results_root, "plots")

outcome_map <- c(
  ver = "log_verifiability_base_m",
  non_ver = "log_non_ver_base_m",
  true = "log_true_base_m",
  fake = "log_fake_base_m",
  n_posts = "log_n_posts_base_m",
  eng = "log_eng_base_m",
  n_posts_covid = "log_n_posts_covid_base_m",
  pos_b_covid = "log_pos_b_covid_base_m",
  neutral_b_covid = "log_neutral_b_covid_base_m",
  neg_b_covid = "log_neg_b_covid_base_m",
  n_posts_vax = "log_n_posts_vax_base_m",
  pos_b_vax = "log_pos_b_vax_base_m",
  neutral_b_vax = "log_neutral_b_vax_base_m",
  neg_b_vax = "log_neg_b_vax_base_m"
)

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
  neg_b_vax = "log Negative Vaccine RTs + Posts"
)

ordered_vars <- c(
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
  "neg_b_vax"
)

scale_base_vars <- c(
  aux_t_base2,
  "pos_b_covid_base",
  "neutral_b_covid_base",
  "neg_b_covid_base",
  "n_posts_covid_base",
  "pos_b_vax_base",
  "neutral_b_vax_base",
  "neg_b_vax_base",
  "n_posts_vax_base"
)

batch_specs <- list(
  b1 = "b1",
  b2 = "b2",
  both = NULL
)

dir.create(results_root, showWarnings = FALSE, recursive = TRUE)
dir.create(original_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(permutations_dir, showWarnings = FALSE, recursive = TRUE)
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
  interaction_seed <- data |>
    select(follower_id, pais, batch_id, total_treated, total_influencers)

  interactions <- generate_interactions(interaction_seed)
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

extract_weighted_treated <- function(data, outcomes, int_cols, context_label = "", allow_missing = FALSE) {
  rhs_controls <- c("total_treated", int_cols)
  rhs_controls <- rhs_controls[nzchar(rhs_controls)]

  estimates <- map_dbl(unname(outcomes), function(outcome_var) {
    fmla <- as.formula(
      paste0(
        outcome_var,
        " ~ ",
        paste(rhs_controls, collapse = " + "),
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
    estimate <- if ("total_treated" %in% names(fit_coefs)) {
      unname(fit_coefs[["total_treated"]])
    } else {
      NA_real_
    }

    if (!is.finite(estimate)) {
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

  stats::setNames(as.list(estimates), names(outcomes)) |>
    as_tibble()
}

write_outputs <- function(batch_name, original_coefs, permutation_coefs) {
  file_code <- paste0("log_", file_stub, "_", batch_name, permutation_suffix)
  original_path <- file.path(original_dir, paste0(file_code, ".xlsx"))
  permutations_path <- file.path(permutations_dir, paste0(file_code, ".xlsx"))
  plot_path <- file.path(plots_dir, paste0(file_code, ".pdf"))

  write_xlsx(original_coefs, original_path)
  write_xlsx(permutation_coefs, permutations_path)

  final <- original_coefs |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "coef") |>
    left_join(
      permutation_coefs |>
        summarise(across(everything(), \(x) sd(x, na.rm = TRUE))) |>
        pivot_longer(cols = everything(), names_to = "var", values_to = "sd"),
      by = "var"
    ) |>
    mutate(
      Variable = ifelse(
        var %in% names(label_map),
        unname(label_map[var]),
        paste0("log ", gsub("_", " ", var))
      ),
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  ordered_labels <- unname(label_map[ordered_vars[ordered_vars %in% final$var]])
  extra_labels <- setdiff(unique(final$Variable), ordered_labels)

  final$Variable <- factor(
    final$Variable,
    levels = c(ordered_labels, sort(extra_labels))
  )

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
    filename = plot_path,
    device = cairo_pdf,
    width = 9.5,
    height = 7.25,
    units = "in"
  )
}

# 3.0 Load and prepare analysis data
belp90 <- read_parquet("../../../data/04-analysis/joint/below_p90_p95_divider.parquet") |>
  select(-n_posts_base)

ver_df <- get_analysis_ver_final_winsor(
  stage = stage,
  batches = "b1b2",
  initial_path = "../../"
) |>
  left_join(belp90, by = c("follower_id", "batch_id", "pais"))

eng_df <- get_analysis_english_winsor(
  stage = stage,
  batches = "b1b2",
  initial_path = "../../"
) |>
  select(follower_id, pais, batch_id, eng_base)

sent_df <- get_analysis_sent_bert_final2(
  stage = stage,
  batches = "b1b2",
  initial_path = "../../"
) |>
  select(
    follower_id, pais, batch_id,
    pos_b_covid_base,
    neutral_b_covid_base,
    neg_b_covid_base,
    n_posts_covid_base,
    pos_b_vax_base,
    neutral_b_vax_base,
    neg_b_vax_base,
    n_posts_vax_base
  )

base_df <- ver_df |>
  left_join(eng_df, by = c("follower_id", "pais", "batch_id")) |>
  left_join(sent_df, by = c("follower_id", "pais", "batch_id")) |>
  filter(n_posts_base > n_posts_thr) |>
  filter(below_p90 == 1) |>
  filter(total_influencers < influencer_thr) |>
  filter(c_t_strong_total > 0) |>
  mutate(
    across(any_of(scale_base_vars), \(x) x / divider),
    log_verifiability_base_m = log(verifiability_base + 1),
    log_non_ver_base_m = log(non_ver_base + 1),
    log_true_base_m = log(true_base + 1),
    log_fake_base_m = log(fake_base + 1),
    log_n_posts_base_m = log(n_posts_base + 1),
    log_eng_base_m = log(eng_base + 1),
    log_pos_b_covid_base_m = log(pos_b_covid_base + 1),
    log_neutral_b_covid_base_m = log(neutral_b_covid_base + 1),
    log_neg_b_covid_base_m = log(neg_b_covid_base + 1),
    log_n_posts_covid_base_m = log(n_posts_covid_base + 1),
    log_pos_b_vax_base_m = log(pos_b_vax_base + 1),
    log_neutral_b_vax_base_m = log(neutral_b_vax_base + 1),
    log_neg_b_vax_base_m = log(neg_b_vax_base + 1),
    log_n_posts_vax_base_m = log(n_posts_vax_base + 1)
  )

# 4.0 Run the specification for each batch sample
for (batch_name in names(batch_specs)) {
  message("Running batch sample: ", batch_name)

  batch_filter <- batch_specs[[batch_name]]
  batch_df <- base_df |>
    prepare_batch_data(batch_filter) |>
    add_weights()

  interaction_fit <- build_interactions(batch_df)
  analysis_df <- interaction_fit$data |>
    select(
      follower_id,
      pais,
      batch_id,
      total_treated,
      total_influencers,
      weights,
      all_of(interaction_fit$int_cols),
      all_of(unname(outcome_map))
    )

  original_coefs <- extract_weighted_treated(
    data = analysis_df,
    outcomes = outcome_map,
    int_cols = interaction_fit$int_cols
  )

  permutation_coefs <- map_dfr(seq_len(n_permutations), function(i) {
    message("Permutation ", i, " / ", n_permutations, " for ", batch_name)

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

    permuted_df <- batch_df |>
      left_join(permuted_counts, by = c("follower_id", "pais", "batch_id"))

    permuted_df$total_treated <- permuted_df[[perm_treated_col]]
    permuted_df$total_influencers <- permuted_df[[perm_total_col]]
    permuted_df[[perm_treated_col]] <- NULL
    permuted_df[[perm_total_col]] <- NULL
    permuted_df <- add_weights(permuted_df)

    permuted_interactions <- build_interactions(permuted_df)
    permuted_analysis_df <- permuted_interactions$data |>
      select(
        follower_id,
        pais,
        batch_id,
        total_treated,
        total_influencers,
        weights,
        all_of(permuted_interactions$int_cols),
        all_of(unname(outcome_map))
      )

    extract_weighted_treated(
      data = permuted_analysis_df,
      outcomes = outcome_map,
      int_cols = permuted_interactions$int_cols,
      context_label = paste(batch_name, "permutation", i),
      allow_missing = TRUE
    )
  })

  write_outputs(
    batch_name = batch_name,
    original_coefs = original_coefs,
    permutation_coefs = permutation_coefs
  )
}

