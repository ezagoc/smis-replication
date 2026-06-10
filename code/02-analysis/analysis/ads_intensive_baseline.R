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
file_stub <- "ads_intensive_baseline"
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

build_ads_baseline_data <- function() {
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
    stage = "stage1_2",
    batches = "b1b2",
    initial_path = initial_path
  ) |>
    left_join(load_belp90(initial_path), by = c("follower_id", "batch_id", "pais")) |>
    mutate(strat_block1 = paste0(strat_block1, batch_id, pais)) |>
    filter(below_p90 == 1) |>
    filter(n_posts_base > n_posts_thr) |>
    filter(total_influencers < influencer_thr)

  eng_df <- get_analysis_english_winsor(
    stage = "stage1_2",
    batches = "b1b2",
    initial_path = initial_path
  )

  sent_df <- get_analysis_sent_bert_final2(
    stage = "stage1_2",
    batches = "b1b2",
    initial_path = initial_path
  )

  analysis_df <- ver_df |>
    select(
      all_of(id_cols),
      ads_treatment,
      strat_block1,
      total_influencers,
      below_p90,
      any_of(baseline_vars)
    ) |>
    left_join(
      eng_df |>
        select(all_of(id_cols), any_of("eng_base")),
      by = id_cols
    ) |>
    left_join(
      sent_df |>
        select(all_of(id_cols), any_of(baseline_vars)),
      by = id_cols
    ) |>
    distinct()

  paired_bases <- intersect(baseline_vars, names(analysis_df))

  analysis_df |>
    mutate(
      across(
        all_of(paired_bases),
        \(x) log(x + 1),
        .names = "log_{.col}"
      )
    )
}

extract_ads_baseline <- function(data, outcome_vars) {
  map_dfr(outcome_vars, function(outcome_var) {
    fmla <- as.formula(
      paste0(outcome_var, " ~ ads_treatment | strat_block1")
    )
    fit <- feols(fmla, data = data, vcov = "HC1")

    tibble(
      var = sub("^log_", "", sub("_base$", "", outcome_var)),
      coef = unname(coef(fit)[["ads_treatment"]]),
      sd = unname(se(fit)[["ads_treatment"]])
    )
  })
}

estimate_plot_height <- function(n_outcomes) {
  max(7.25, 0.42 * n_outcomes + 1.5)
}

baseline_df <- build_ads_baseline_data()
outcome_vars <- grep("^log_.*_base$", names(baseline_df), value = TRUE)
outcome_vars <- outcome_vars[
  order(match(sub("^log_", "", sub("_base$", "", outcome_vars)), names(standard_outcome_label_map)))
]
outcome_vars <- outcome_vars[
  !is.na(match(sub("^log_", "", sub("_base$", "", outcome_vars)), names(standard_outcome_label_map)))
]

for (batch_name in names(batch_specs)) {
  message("Running ads baseline specification for sample: ", batch_name)

  batch_filter <- batch_specs[[batch_name]]
  final <- baseline_df |>
    prepare_batch_data(batch_filter) |>
    extract_ads_baseline(outcome_vars = outcome_vars) |>
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
