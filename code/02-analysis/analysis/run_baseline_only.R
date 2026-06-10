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

baseline_scripts <- c(
  "extensive_baseline_batches_1000.R",
  "extensive_baseline_batches_1000_strong.R",
  "intensive_baseline_weighted_batches_1000.R",
  "intensive_baseline_weighted_batches_1000_strong.R",
  "ads_intensive_baseline.R"
)

invisible(lapply(baseline_scripts, run_isolated_source))
