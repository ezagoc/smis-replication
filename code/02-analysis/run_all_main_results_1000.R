rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

message("Running baseline extensive results")
source("extensive_fes_batches_500.R")

message("Running baseline extensive results for strong followers only")
source("extensive_fes_batches_500_strong.R")

message("Running stage-by-stage extensive results")
source("extensive_linear_90p_p5.R")

message("Running stage-by-stage extensive results for strong followers only")
source("extensive_linear_90p_p5_strong.R")

message("Running baseline intensive results")
source("intensive_baseline_weighted_batches_500.R")

message("Running baseline intensive results for strong followers only")
source("intensive_baseline_weighted_batches_500_strong.R")

message("Running stage-by-stage intensive results")
source("intensive_linear_95p_weighted_batches_500.R")

message("Running stage-by-stage intensive results for strong followers only")
source("intensive_linear_95p_weighted_batches_500_strong.R")

message("Running all aggregated extensive and intensive results")
source("run_all_aggregate_batches.R")

message("Rebuilding paper-facing plots from saved results")
source("plot_saved_results_only.R")
