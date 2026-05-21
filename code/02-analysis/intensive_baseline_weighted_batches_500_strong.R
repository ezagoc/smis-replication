# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
library(fastDummies)

src_path <- "../../src/utils/"
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
list_types <- c("log_")
file_stub <- "intensive_baseline_weighted_strong"
influencer_thr <- 9
n_posts_thr <- 0
n_permutations <- 500

results_root <- file.path("../../results", "intensive_baseline_weighted_strong")
original_dir <- file.path(results_root, "original")
permutations_dir <- file.path(results_root, "permutations")
plots_dir <- file.path(results_root, "plots")

outcome_vars_base <- c(
  "verifiability_base",
  "non_ver_base",
  "true_base",
  "fake_base",
  "n_posts_base",
  "eng_base"
)
output_names <- c("ver", "non_ver", "true", "fake", "n_posts", "eng")
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

extract_weighted_treated <- function(data, outcome_vars, int_cols, context_label = "") {
  int_part <- paste(int_cols, collapse = " + ")
  rhs_controls <- paste(c("total_treated", int_part), collapse = " + ")

  estimates <- map_dbl(outcome_vars, function(outcome_var) {
    fmla <- as.formula(
      paste0(
        outcome_var,
        " ~ ",
        rhs_controls,
        " | total_influencers + pais + batch_id"
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

  stats::setNames(as.list(estimates), output_names) |>
    as_tibble()
}

write_outputs <- function(type, batch_name, original_coefs, permutation_coefs) {
  file_code <- paste0(type, file_stub, "_", batch_name, "_500perm")
  original_path <- file.path(original_dir, paste0(file_code, ".xlsx"))
  permutations_path <- file.path(permutations_dir, paste0(file_code, ".xlsx"))
  plot_path <- file.path(plots_dir, paste0(file_code, ".pdf"))

  write_xlsx(original_coefs, original_path)
  write_xlsx(permutation_coefs, permutations_path)

  addon <- if (type == "log_") {
    "log "
  } else if (type == "arc_") {
    "arcsinh "
  } else {
    ""
  }

  final <- original_coefs |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "coef") |>
    left_join(
      permutation_coefs |>
        summarise(across(everything(), sd, na.rm = TRUE)) |>
        pivot_longer(cols = everything(), names_to = "var", values_to = "sd"),
      by = "var"
    ) |>
    mutate(
      Variable = case_when(
        var == "ver" ~ paste0(addon, "Verifiable RTs + Posts"),
        var == "non_ver" ~ paste0(addon, "Non-Verifiable RTs + Posts"),
        var == "true" ~ paste0(addon, "True RTs + Posts"),
        var == "fake" ~ paste0(addon, "Fake RTs + Posts"),
        var == "n_posts" ~ paste0(addon, "Number of RTs + Posts"),
        var == "eng" ~ paste0(addon, "Number of RTs + Posts (English)"),
        TRUE ~ var
      ),
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  final$Variable <- factor(
    final$Variable,
    levels = c(
      paste0(addon, "Fake RTs + Posts"),
      paste0(addon, "True RTs + Posts"),
      paste0(addon, "Verifiable RTs + Posts"),
      paste0(addon, "Non-Verifiable RTs + Posts"),
      paste0(addon, "Number of RTs + Posts (English)"),
      paste0(addon, "Number of RTs + Posts")
    )
  )

  x_bounds <- axis_bounds(final$lower, final$upper)

  results_plot <- ggplot(data = final, aes(y = Variable, x = coef)) +
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
    filename = plot_path,
    device = cairo_pdf,
    width = 8.22,
    height = 6.59,
    units = "in"
  )
}

belp90 <- read_parquet("../../data/analysis/joint/below_p90_p95_divider.parquet") |>
  select(-n_posts_base)

ver_df <- get_analysis_ver_final_winsor(
  stage = stage,
  batches = "b1b2",
  initial_path = "../../../../"
) |>
  left_join(belp90, by = c("follower_id", "batch_id", "pais"))

eng_df <- get_analysis_english_winsor(
  stage = stage,
  batches = "b1b2",
  initial_path = "../../../../"
) |>
  select(
    follower_id, pais, batch_id,
    eng_base, log_eng_base
  )

base_df <- ver_df |>
  left_join(eng_df, by = c("follower_id", "pais", "batch_id")) |>
  filter(n_posts_base > n_posts_thr) |>
  filter(below_p90 == 1) |>
  filter(total_influencers < influencer_thr) |>
  filter(c_t_strong_total > 0) |>
  mutate(
    across(any_of(aux_t_base2), ~.x / divider),
    log_eng_base_m = log(eng_base + 1),
    log_n_posts_base_m = log(n_posts_base + 1),
    log_true_base_m = log(true_base + 1),
    log_fake_base_m = log(fake_base + 1),
    log_verifiability_base_m = log(verifiability_base + 1),
    log_non_ver_base_m = log(non_ver_base + 1)
  )

# 4.0 Run the specification for each batch sample
for (type in list_types) {
  outcome_vars <- paste0(type, outcome_vars_base, "_m")

  for (batch_name in names(batch_specs)) {
    message("Running batch sample: ", batch_name)

    batch_filter <- batch_specs[[batch_name]]
    batch_df <- base_df |>
      prepare_batch_data(batch_filter) |>
      add_weights()

    interaction_fit <- build_interactions(batch_df)
    analysis_df <- interaction_fit$data

    original_coefs <- extract_weighted_treated(
      data = analysis_df,
      outcome_vars = outcome_vars,
      int_cols = interaction_fit$int_cols,
      context_label = paste(batch_name, "original")
    )

    permutation_coefs <- map_dfr(seq_len(n_permutations), function(i) {
      message("Permutation ", i, " / ", n_permutations, " for ", batch_name)

      perm_treated_col <- paste0("n_influencers_followed_treatment_p", i)
      perm_total_col <- paste0("n_influencers_followed_p_", i)

      permuted_counts <- read_parquet(
        paste0(
          "../../data/analysis/joint/small_ties_b1b2/small_tie",
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

      extract_weighted_treated(
        data = permuted_interactions$data,
        outcome_vars = outcome_vars,
        int_cols = permuted_interactions$int_cols,
        context_label = paste(batch_name, "permutation", i)
      )
    })

    write_outputs(
      type = type,
      batch_name = batch_name,
      original_coefs = original_coefs,
      permutation_coefs = permutation_coefs
    )
  }
}


