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
list_stages <- c("stage1_2", "stage3_4", "stage5_6")
file_stub <- "ads_intensive_stages"
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
id_cols <- c("follower_id", "pais", "batch_id")

dir.create(results_root, showWarnings = FALSE, recursive = TRUE)
dir.create(estimates_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

collect_stage_ads_data <- function(stage, batch_filter = NULL) {
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

  ver_df <- ensure_columns(
    ver_df,
    c(id_cols, "ads_treatment", "strat_block1", ver_endline_vars, intersect(baseline_vars, names(ver_df))),
    paste("Verifiability data for", stage)
  )
  eng_df <- ensure_columns(eng_df, c(id_cols, english_endline_vars, intersect("eng_base", names(eng_df))), paste("English data for", stage))
  sent_df <- ensure_columns(sent_df, c(id_cols, sentiment_endline_vars), paste("Sentiment data for", stage))

  stage_df <- ver_df |>
    select(all_of(id_cols), ads_treatment, strat_block1, all_of(ver_endline_vars), any_of(baseline_vars)) |>
    left_join(
      eng_df |>
        select(all_of(id_cols), all_of(english_endline_vars), any_of("eng_base")),
      by = id_cols
    ) |>
    left_join(
      sent_df |>
        select(all_of(id_cols), all_of(sentiment_endline_vars), any_of(baseline_vars)),
      by = id_cols
    ) |>
    prepare_batch_data(batch_filter)

  all_base_vars <- grep("_base$", names(stage_df), value = TRUE)
  all_endline_vars <- setdiff(names(stage_df), c(id_cols, "ads_treatment", "strat_block1", all_base_vars))
  paired_roots <- all_endline_vars[paste0(all_endline_vars, "_base") %in% all_base_vars]

  stage_df <- stage_df |>
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

  list(
    data = stage_df,
    outcome_vars = paste0("log_", paired_roots)
  )
}

extract_ads_stage <- function(data, outcome_vars) {
  estimates <- map_dfr(outcome_vars, function(outcome_var) {
    fmla <- as.formula(
      paste0(outcome_var, " ~ ads_treatment + ", outcome_var, "_base | strat_block1")
    )
    fit <- feols(fmla, data = data, vcov = "HC1")

    tibble(
      var = sub("^log_", "", outcome_var),
      coef = unname(coef(fit)[["ads_treatment"]]),
      sd = unname(se(fit)[["ads_treatment"]])
    )
  })

  estimates
}

estimate_plot_height <- function(n_outcomes) {
  max(7.5, 0.38 * n_outcomes + 2.25)
}

for (batch_name in names(batch_specs)) {
  message("Running ads stage specification for sample: ", batch_name)

  batch_filter <- batch_specs[[batch_name]]
  stage_results <- list()

  for (stage in list_stages) {
    message("Running stage: ", stage, " for ", batch_name)

    stage_bundle <- collect_stage_ads_data(stage, batch_filter = batch_filter)

    final_stage <- extract_ads_stage(stage_bundle$data, stage_bundle$outcome_vars) |>
      mutate(
        stage = stage,
        Stage = recode(stage, !!!standard_stage_labels),
        Variable = unname(standard_outcome_label_map[var])
      ) |>
      filter(!is.na(Variable))

    stage_results[[stage]] <- final_stage
  }

  final <- bind_rows(stage_results)
  final$Variable <- factor(final$Variable, levels = standard_outcome_order)
  final$Stage <- factor(final$Stage, levels = unname(standard_stage_labels))

  write_xlsx(
    final,
    file.path(estimates_dir, paste0(file_stub, "_", batch_name, "_estimates.xlsx"))
  )

  final <- final |>
    mutate(
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  write_stage_plot(
    final = final,
    plot_path = file.path(plots_dir, paste0(file_stub, "_", batch_name, ".pdf")),
    ylab_text = "Ads treatment estimate with 95% confidence interval",
    width = 10,
    height = estimate_plot_height(nlevels(droplevels(final$Variable)))
  )
}
