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

if (!exists("sample_kind") || !sample_kind %in% c("intensive", "extensive")) {
  stop("sample_kind must be set to 'intensive' or 'extensive' before sourcing this runner.", call. = FALSE)
}

if (!exists("strong_only") || !is.logical(strong_only) || length(strong_only) != 1) {
  stop("strong_only must be set to TRUE or FALSE before sourcing this runner.", call. = FALSE)
}

if (!exists("results_subdir") || !nzchar(results_subdir)) {
  stop("results_subdir must be set before sourcing this runner.", call. = FALSE)
}

if (!exists("file_stub") || !nzchar(file_stub)) {
  stop("file_stub must be set before sourcing this runner.", call. = FALSE)
}

country <- "joint"
data_type <- "Followers"
stage <- "stage1_2"
influencer_thr <- 9
n_posts_thr <- 0
n_permutations <- 500

results_root <- file.path("../../../results", results_subdir)
original_dir <- file.path(results_root, "original")
permutations_dir <- file.path(results_root, "permutations")
estimates_dir <- file.path(results_root, "estimates")

batch_specs <- list(
  b1 = "b1",
  b2 = "b2",
  both = NULL
)

outcome_vars <- c(
  "ihs_ads", "ihs_smi",
  "log1p_ads", "log1p_smi",
  "dummy_ads", "dummy_smi",
  "cr_ads", "cr_smi"
)

transform_map <- c(
  ihs_ads = "IHS (arcsinh)",
  ihs_smi = "IHS (arcsinh)",
  log1p_ads = "log(y+1)",
  log1p_smi = "log(y+1)",
  dummy_ads = "Dummy (y>0)",
  dummy_smi = "Dummy (y>0)",
  cr_ads = "Chen-Roth",
  cr_smi = "Chen-Roth"
)

outcome_map <- c(
  ihs_ads = "Ads Retweets",
  ihs_smi = "SMI Retweets",
  log1p_ads = "Ads Retweets",
  log1p_smi = "SMI Retweets",
  dummy_ads = "Ads Retweets",
  dummy_smi = "SMI Retweets",
  cr_ads = "Ads Retweets",
  cr_smi = "SMI Retweets"
)

dir.create(results_root, showWarnings = FALSE, recursive = TRUE)
dir.create(original_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(permutations_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(estimates_dir, showWarnings = FALSE, recursive = TRUE)

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

safe_min_pos <- function(x) {
  min_pos <- suppressWarnings(min(x[x > 0], na.rm = TRUE))

  if (!is.finite(min_pos)) {
    return(1)
  }

  min_pos
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

rhs_terms <- function(int_cols) {
  paste(c("total_treated", int_cols), collapse = " + ")
}

fe_terms <- function() {
  if (sample_kind == "intensive") {
    return("total_influencers + pais + batch_id")
  }

  "pais + batch_id"
}

extract_weighted_treated <- function(data, outcome_vars, int_cols, context_label = "") {
  rhs <- rhs_terms(int_cols)
  fe_rhs <- fe_terms()

  estimates <- map_dbl(outcome_vars, function(outcome_var) {
    fmla <- as.formula(
      paste0(
        outcome_var, " ~ ", rhs, " | ", fe_rhs
      )
    )

    fit <- feols(fmla, data = data, weights = ~weights)
    estimate <- unname(coef(fit)[["total_treated"]])

    if (length(estimate) == 0 || is.na(estimate)) {
      stop(
        paste0(
          "Missing total_treated estimate for ", outcome_var,
          if (nzchar(context_label)) paste0(" (", context_label, ")") else ""
        ),
        call. = FALSE
      )
    }

    estimate
  })

  stats::setNames(as.list(estimates), outcome_vars) |>
    as_tibble()
}

build_estimate_summary <- function(original_coefs, permutation_coefs, control_means) {
  original_coefs |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "coef") |>
    left_join(
      permutation_coefs |>
        summarise(across(everything(), sd, na.rm = TRUE)) |>
        pivot_longer(cols = everything(), names_to = "var", values_to = "sd"),
      by = "var"
    ) |>
    mutate(
      transform = unname(transform_map[var]),
      outcome = unname(outcome_map[var]),
      control_mean = vapply(
        var,
        function(x) {
          value <- unname(control_means[x])

          if (length(value) == 0 || is.null(value) || is.na(value) || is.nan(value)) {
            return(NA_real_)
          }

          as.numeric(value)
        },
        numeric(1)
      )
    ) |>
    select(transform, outcome, var, coef, sd, control_mean)
}

load_base_data <- function(batch_filter = NULL) {
  belp90 <- read_parquet("../../../data/04-analysis/joint/below_p90_p95_divider.parquet") |>
    select(follower_id:percentile) |>
    select(-n_posts_base) |>
    distinct(follower_id, batch_id, pais, .keep_all = TRUE)

  df_s <- read_parquet("../../../data/others/RTs_counts_smi.parquet") |>
    select(follower_id = id, RTs_smi_treatment) |>
    distinct(follower_id, .keep_all = TRUE)

  df_a <- read_parquet("../../../data/others/RTs_counts_ads.parquet") |>
    select(follower_id = id, RTs_ads_treatment) |>
    distinct(follower_id, .keep_all = TRUE)

  base_df <- get_analysis_ver_final_winsor(
    stage = stage,
    batches = "b1b2",
    initial_path = "../../"
  ) |>
    filter(n_posts_base > n_posts_thr) |>
    left_join(df_s, by = "follower_id", relationship = "many-to-one") |>
    left_join(df_a, by = "follower_id", relationship = "many-to-one") |>
    mutate(
      ads = ifelse(is.na(RTs_ads_treatment), 0, RTs_ads_treatment),
      smi = ifelse(is.na(RTs_smi_treatment), 0, RTs_smi_treatment),
      dummy_ads = ifelse(ads > 0, 1, 0),
      dummy_smi = ifelse(smi > 0, 1, 0),
      total_treated = t_strong + t_weak + t_neither,
      total_influencers = c_t_strong_total + c_t_weak_total + c_t_neither_total
    ) |>
    left_join(
      belp90,
      by = c("follower_id", "batch_id", "pais"),
      relationship = "many-to-one"
    ) |>
    filter(below_p90 == 1)

  if (sample_kind == "intensive") {
    base_df <- base_df |>
      filter(total_influencers < influencer_thr)
  } else {
    base_df <- base_df |>
      filter(total_influencers == 1)
  }

  if (strong_only) {
    base_df <- base_df |>
      filter(c_t_strong_total > 0)
  }

  base_df <- prepare_batch_data(base_df, batch_filter)

  min_pos_ads <- safe_min_pos(base_df$ads)
  min_pos_smi <- safe_min_pos(base_df$smi)

  base_df |>
    mutate(
      ihs_ads = asinh(ads),
      ihs_smi = asinh(smi),
      log1p_ads = log(ads + 1),
      log1p_smi = log(smi + 1),
      cr_ads = ifelse(ads == 0, -min_pos_ads, log(ads)),
      cr_smi = ifelse(smi == 0, -min_pos_smi, log(smi))
    )
}

for (batch_name in names(batch_specs)) {
  message("Running batch sample: ", batch_name)

  batch_filter <- batch_specs[[batch_name]]

  batch_df <- load_base_data(batch_filter) |>
    add_weights()

  pure_control_means <- batch_df |>
    filter(ads_treatment == 0, total_treated == 0) |>
    summarise(across(all_of(outcome_vars), \(x) mean(x, na.rm = TRUE))) |>
    unlist(use.names = TRUE)

  interaction_fit <- build_interactions(batch_df)
  analysis_df <- interaction_fit$data
  int_cols <- interaction_fit$int_cols

  original_coefs <- extract_weighted_treated(
    data = analysis_df,
    outcome_vars = outcome_vars,
    int_cols = int_cols,
    context_label = paste(batch_name, "original")
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

    permuted_df <- poolTreatmentBalance2(permuted_df, perm_treated_col, perm_total_col)
    permuted_df[[perm_treated_col]] <- NULL
    permuted_df[[perm_total_col]] <- NULL
    permuted_df <- add_weights(permuted_df)

    permuted_interactions <- build_interactions(permuted_df)

    extract_weighted_treated(
      data = permuted_interactions$data,
      outcome_vars = outcome_vars,
      int_cols = permuted_interactions$int_cols,
      context_label = paste(batch_name, "permutation", i)
    )
  })

  write_xlsx(
    original_coefs,
    file.path(
      original_dir,
      paste0(file_stub, "_", batch_name, "_500perm.xlsx")
    )
  )

  write_xlsx(
    permutation_coefs,
    file.path(
      permutations_dir,
      paste0(file_stub, "_", batch_name, "_500perm.xlsx")
    )
  )

  estimate_summary <- build_estimate_summary(
    original_coefs = original_coefs,
    permutation_coefs = permutation_coefs,
    control_means = pure_control_means
  )

  write_xlsx(
    estimate_summary,
    file.path(
      estimates_dir,
      paste0(file_stub, "_", batch_name, "_500perm_estimates.xlsx")
    )
  )
}

message("Done: ", file_stub)





