# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Variant configuration
sample_kind <- "intensive"
strong_only <- TRUE
results_subdir <- "first_stage_intensive_all_transforms_strong"
file_stub <- "first_stage_intensive_all_transforms_strong"
table_stub <- "first_stage_intensive_all_transforms_strong"
table_title <- "First Stage Intensive Strong - All Transforms"

# 2.0 Build tables from saved estimates
source("first_stage_all_transforms_tables_runner.R")

