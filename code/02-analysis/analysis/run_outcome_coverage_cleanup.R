rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

run_isolated_source <- function(path) {
  message("")
  message("========================================")
  message("Sourcing: ", path)
  message("========================================")

  source(
    path,
    local = new.env(parent = globalenv()),
    echo = FALSE,
    chdir = FALSE
  )
}

cleanup_scripts <- c(
  #"extensive_linear_90p_p5.R",
  #"extensive_linear_90p_p5_strong.R",
  #"intensive_linear_95p_weighted_batches_500.R",
  #"intensive_linear_95p_weighted_batches_500_strong.R",
  #"run_all_aggregate_batches.R",
  #"run_ads_followers_supplement.R",
  #"first_stage_extensive_all_transforms_tables.R",
  #"first_stage_extensive_all_transforms_strong_tables.R",
  #"first_stage_intensive_all_transforms_tables.R",
  #"first_stage_intensive_all_transforms_strong_tables.R",
  #"export_aggregate_control_stats.R",
  #"../graphs/plot_saved_results_only.R",
  "../graphs/plot_aggregate_results_mock_style.R"
)

invisible(lapply(cleanup_scripts, run_isolated_source))
