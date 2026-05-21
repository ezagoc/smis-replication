# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Variant configuration
sample_kind <- "extensive"
strong_only <- FALSE
results_subdir <- "first_stage_extensive_all_transforms"
file_stub <- "first_stage_extensive_all_transforms"

# 2.0 Run shared first-stage pipeline
source("first_stage_all_transforms_runner.R")
