# 0.0 Set up the environment and locate this script
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
source("supplemental_outcome_helpers.R")

initial_path <- repo_initial_path()
src_path <- paste0(initial_path, "src/utils/")
source_files <- c(
  "funcs_analysis.R",
  "constants_final.R",
  "import_data.R"
)

map(paste0(src_path, source_files), source)
ipak(packages)

# 2.0 Constants
file_stub <- "ads_intensive_aggregate"
results_root <- results_path(file_stub)
estimates_dir <- file.path(results_root, "estimates")
plots_dir <- file.path(results_root, "plots")
batch_specs <- list(
  b1 = "b1",
  b2 = "b2",
  both = NULL
)

n_posts_thr <- 0
influencer_thr <- 9
stage_names <- c("stage1_2", "stage3_4", "stage5_6")
id_cols <- c("follower_id", "pais", "batch_id")

dir.create(results_root, showWarnings = FALSE, recursive = TRUE)
dir.create(estimates_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

ensure_columns <- function(data, required_cols, object_label) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      paste(
        object_label,
        "is missing required columns:",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data
}

build_ads_aggregate_data <- function() {
  ver_endline_vars <- c(
    "total_shares",
    "total_reactions",
    "total_comments",
    "verifiability",
    "non_ver",
    "true",
    "fake",
    "n_posts"
  )

  english_endline_vars <- c("eng")
  sentiment_endline_vars <- c(
    "pos_b_covid", "neutral_b_covid", "neg_b_covid", "n_posts_covid",
    "pos_b_vax", "neutral_b_vax", "neg_b_vax", "n_posts_vax"
  )

  stage_frames <- map(stage_names, function(stage) {
    ver_df <- get_analysis_ver_final_winsor(
      stage = stage,
      batches = "b1b2",
      initial_path = initial_path
    ) |>
      left_join(load_belp90(initial_path), by = c("follower_id", "batch_id", "pais")) |>
      mutate(strat_block1 = paste0(strat_block1, batch_id, pais)) |>
      filter(below_p90 == 1) |>
      filter(n_posts_base > n_posts_thr) |>
      filter(total_influencers < influencer_thr)

    eng_df <- get_analysis_english_winsor(
      stage = stage,
      batches = "b1b2",
      initial_path = initial_path
    )

    sent_df <- get_analysis_sent_bert_final2(
      stage = stage,
      batches = "b1b2",
      initial_path = initial_path
    )

    ver_df <- ensure_columns(ver_df, c(id_cols, ver_endline_vars), paste("Verifiability data for", stage))
    eng_df <- ensure_columns(eng_df, c(id_cols, english_endline_vars), paste("English data for", stage))
    sent_df <- ensure_columns(sent_df, c(id_cols, sentiment_endline_vars), paste("Sentiment data for", stage))

    ver_df |>
      select(all_of(id_cols), ads_treatment, strat_block1, total_influencers, below_p90, n_posts_base, all_of(ver_endline_vars)) |>
      left_join(
        eng_df |>
          select(all_of(id_cols), all_of(english_endline_vars)),
        by = id_cols
      ) |>
      left_join(
        sent_df |>
          select(all_of(id_cols), all_of(sentiment_endline_vars)),
        by = id_cols
      )
  })

  stage1_ver <- get_analysis_ver_final_winsor(
    stage = "stage1_2",
    batches = "b1b2",
    initial_path = initial_path
  ) |>
    left_join(load_belp90(initial_path), by = c("follower_id", "batch_id", "pais")) |>
    mutate(strat_block1 = paste0(strat_block1, batch_id, pais)) |>
    filter(below_p90 == 1) |>
    filter(n_posts_base > n_posts_thr) |>
    filter(total_influencers < influencer_thr)

  stage1_eng <- get_analysis_english_winsor(
    stage = "stage1_2",
    batches = "b1b2",
    initial_path = initial_path
  )

  stage1_sent <- get_analysis_sent_bert_final2(
    stage = "stage1_2",
    batches = "b1b2",
    initial_path = initial_path
  )

  baseline_vars <- c(
    "total_shares_base",
    "total_reactions_base",
    "total_comments_base",
    "verifiability_base",
    "non_ver_base",
    "true_base",
    "fake_base",
    "n_posts_base",
    "eng_base",
    "pos_b_covid_base", "neutral_b_covid_base", "neg_b_covid_base", "n_posts_covid_base",
    "pos_b_vax_base", "neutral_b_vax_base", "neg_b_vax_base", "n_posts_vax_base"
  )

  controls <- stage1_ver |>
    select(all_of(id_cols), ads_treatment, strat_block1, total_influencers, below_p90, any_of(baseline_vars)) |>
    left_join(
      stage1_eng |>
        select(all_of(id_cols), any_of("eng_base")),
      by = id_cols
    ) |>
    left_join(
      stage1_sent |>
        select(all_of(id_cols), any_of(baseline_vars)),
      by = id_cols
    ) |>
    distinct()

  aggregated_outcomes <- bind_rows(stage_frames) |>
    group_by(across(all_of(id_cols))) |>
    summarise(
      across(
        -all_of(c("ads_treatment", "strat_block1", "total_influencers", "below_p90", "n_posts_base")),
        \(x) sum(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  aggregate_df <- controls |>
    left_join(aggregated_outcomes, by = id_cols)

  all_base_vars <- grep("_base$", names(aggregate_df), value = TRUE)
  all_endline_vars <- setdiff(
    names(aggregate_df),
    c(id_cols, "ads_treatment", "strat_block1", "total_influencers", "below_p90", all_base_vars)
  )

  paired_roots <- all_endline_vars[paste0(all_endline_vars, "_base") %in% all_base_vars]

  aggregate_df |>
    mutate(
      across(
        all_of(paired_roots),
        \(x) log(x + 1),
        .names = "log_{.col}"
      ),
      across(
        all_of(paste0(paired_roots, "_base")),
        \(x) log(x + 1),
        .names = "log_{.col}"
      )
    )
}

extract_ads_aggregate <- function(data, outcome_vars) {
  map_dfr(outcome_vars, function(outcome_var) {
    var_name <- sub("^log_", "", outcome_var)
    fmla <- as.formula(
      paste0(outcome_var, " ~ ads_treatment + ", outcome_var, "_base | strat_block1")
    )
    fit <- feols(fmla, data = data, vcov = "HC1")

    tibble(
      var = var_name,
      coef = unname(coef(fit)[["ads_treatment"]]),
      sd = unname(se(fit)[["ads_treatment"]])
    )
  })
}

aggregate_df <- build_ads_aggregate_data()
outcome_vars <- grep("^log_", names(aggregate_df), value = TRUE)
outcome_vars <- outcome_vars[paste0(outcome_vars, "_base") %in% names(aggregate_df)]
outcome_vars <- outcome_vars[order(match(sub("^log_", "", outcome_vars), names(standard_outcome_label_map)))]
outcome_vars <- outcome_vars[!is.na(match(sub("^log_", "", outcome_vars), names(standard_outcome_label_map)))]

estimate_plot_height <- function(n_outcomes) {
  max(7.25, 0.42 * n_outcomes + 1.5)
}

for (batch_name in names(batch_specs)) {
  message("Running ads aggregate specification for sample: ", batch_name)

  batch_filter <- batch_specs[[batch_name]]
  final <- aggregate_df |>
    prepare_batch_data(batch_filter) |>
    extract_ads_aggregate(outcome_vars = outcome_vars) |>
    mutate(Variable = unname(standard_outcome_label_map[var])) |>
    filter(!is.na(Variable))

  final$Variable <- factor(final$Variable, levels = standard_outcome_order)

  write_xlsx(
    final,
    file.path(estimates_dir, paste0(file_stub, "_", batch_name, "_estimates.xlsx"))
  )

  final <- final |>
    mutate(
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  write_horizontal_plot(
    final = final,
    plot_path = file.path(plots_dir, paste0(file_stub, "_", batch_name, ".pdf")),
    xlab_text = "Ads treatment estimate with 95% confidence interval",
    width = 9.5,
    height = estimate_plot_height(nrow(final))
  )
}
