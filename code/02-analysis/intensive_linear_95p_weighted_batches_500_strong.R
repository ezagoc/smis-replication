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
data_type <- "Verifiability"
list_stages <- c("stage1_2", "stage3_4", "stage5_6")
list_types <- c("log_")
file_stub <- "intensive_linear_95p_weighted_strong"
influencer_thr <- 9
n_posts_thr <- 0
n_permutations <- 1000
permutation_suffix <- paste0("_", n_permutations, "perm")

results_root <- file.path("../../results", "intensive_linear_95p_weighted_strong")
original_dir <- file.path(results_root, "original")
permutations_dir <- file.path(results_root, "permutations")
estimates_dir <- file.path(results_root, "estimates")
plots_dir <- file.path(results_root, "plots")

outcome_roots <- c("verifiability", "non_ver", "true", "fake", "n_posts")
output_names <- c("ver", "non_ver", "true", "fake", "n_posts")
stage_map <- c(
  stage1_2 = "Weeks 1-4",
  stage3_4 = "Weeks 5-8",
  stage5_6 = "Weeks 9-12"
)
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

add_weights <- function(data) {
  data |>
    mutate(weights = ifelse(total_influencers > 0, 1 / total_influencers, NA_real_)) |>
    filter(!is.na(weights))
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
  interaction_part <- paste(int_cols, collapse = " + ")

  estimates <- map_dbl(outcome_vars, function(outcome_var) {
    fmla <- as.formula(
      paste0(
        outcome_var, " ~ total_treated + ",
        outcome_var, "_base + ",
        interaction_part,
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

summarise_stage_results <- function(original_coefs, permutation_coefs, stage, type) {
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
      stage = stage,
      Variable = case_when(
        var == "ver" ~ paste0(addon, "Verifiable Posts + Shares"),
        var == "non_ver" ~ paste0(addon, "Non Verifiable Posts + Shares"),
        var == "true" ~ paste0(addon, "True Posts + Shares"),
        var == "fake" ~ paste0(addon, "Fake Posts + Shares"),
        var == "n_posts" ~ paste0(addon, "Number of Posts + Shares")
      ),
      Stage = recode(stage, !!!stage_map)
    )
}

write_outputs <- function(type, batch_name, stage, original_coefs, permutation_coefs) {
  file_code <- paste0(type, file_stub, "_", batch_name, "_", stage, permutation_suffix)

  write_xlsx(
    original_coefs,
    file.path(original_dir, paste0(file_code, ".xlsx"))
  )

  write_xlsx(
    permutation_coefs,
    file.path(permutations_dir, paste0(file_code, ".xlsx"))
  )
}

# 3.0 Load shared filters
belp90 <- read_parquet("../../data/04-analysis/joint/below_p90_p95_divider.parquet") |>
  select(-n_posts_base)

# 4.0 Run the specification for each batch sample and stage
for (type in list_types) {
  outcome_vars <- paste0(type, outcome_roots)

  for (batch_name in names(batch_specs)) {
    message("Running batch sample: ", batch_name)

    batch_filter <- batch_specs[[batch_name]]
    stage_summaries <- list()

    for (stage in list_stages) {
      message("Running stage: ", stage, " for ", batch_name)

      base_df <- get_analysis_ver_final_winsor(
        stage = stage,
        batches = "b1b2",
        initial_path = "../../"
      ) |>
        left_join(belp90, by = c("follower_id", "batch_id", "pais")) |>
        filter(below_p90 == 1) |>
        filter(n_posts_base > n_posts_thr) |>
        filter(total_influencers < influencer_thr) |>
        filter(c_t_strong_total > 0) |>
        prepare_batch_data(batch_filter) |>
        mutate(
          across(any_of(aux_t_base2), ~.x / divider),
          log_n_posts_base = log(n_posts_base + 1),
          log_true_base = log(true_base + 1),
          log_fake_base = log(fake_base + 1),
          log_verifiability_base = log(verifiability_base + 1),
          log_non_ver_base = log(non_ver_base + 1)
        ) |>
        add_weights()

      interaction_fit <- build_interactions(base_df)
      analysis_df <- interaction_fit$data
      int_cols <- interaction_fit$int_cols

      original_coefs <- extract_weighted_treated(
        data = analysis_df,
        outcome_vars = outcome_vars,
        int_cols = int_cols,
        context_label = paste(batch_name, stage, "original")
      )

      permutation_coefs <- map_dfr(seq_len(n_permutations), function(i) {
        message("Permutation ", i, " / ", n_permutations, " for ", batch_name, " - ", stage)

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
          context_label = paste(batch_name, stage, "permutation", i)
        )
      })

      write_outputs(
        type = type,
        batch_name = batch_name,
        stage = stage,
        original_coefs = original_coefs,
        permutation_coefs = permutation_coefs
      )

      stage_summaries[[stage]] <- summarise_stage_results(
        original_coefs = original_coefs,
        permutation_coefs = permutation_coefs,
        stage = stage,
        type = type
      )
    }

    final <- bind_rows(stage_summaries)

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

    final$Stage <- factor(
      final$Stage,
      levels = unname(stage_map)
    )

    write_xlsx(
      final,
      file.path(
        estimates_dir,
        paste0(type, file_stub, "_", batch_name, permutation_suffix, "_estimates.xlsx")
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
      xlab("Stage") +
      theme(
        panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggsave(
      plot = results_plot,
      filename = file.path(
        plots_dir,
        paste0(type, file_stub, "_", batch_name, permutation_suffix, ".pdf")
      ),
      device = cairo_pdf,
      width = 8.22,
      height = 6.59,
      units = "in"
    )
  }
}


