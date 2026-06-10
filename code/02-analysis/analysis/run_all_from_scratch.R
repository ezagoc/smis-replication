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
  "extensive_baseline_batches_1000.R",
  "extensive_baseline_batches_1000_strong.R",
  "extensive_linear_90p_p5.R",
  "extensive_linear_90p_p5_strong.R",
  "intensive_baseline_weighted_batches_1000.R",
  "intensive_baseline_weighted_batches_1000_strong.R",
  "intensive_linear_95p_weighted_batches_500.R",
  "intensive_linear_95p_weighted_batches_500_strong.R",
  "ads_intensive_aggregate_batches.R",
  "ads_intensive_stages_batches.R",
  "plot_saved_results_only.R"
)

invisible(lapply(analysis_scripts, run_isolated_source))
