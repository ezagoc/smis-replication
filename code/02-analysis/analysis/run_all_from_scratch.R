# 0.0 Set up the environment and locate this script
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

analysis_scripts <- c(
  "run_baseline_only.R",
  "run_all_aggregate_batches.R",
  "extensive_linear_90p_p5.R",
  "extensive_linear_90p_p5_strong.R",
  "intensive_linear_95p_weighted_batches_500.R",
  "intensive_linear_95p_weighted_batches_500_strong.R",
  "run_ads_followers_supplement.R",
  "../graphs/run_plot_saved_results_only.R"
)

invisible(lapply(analysis_scripts, run_isolated_source))
