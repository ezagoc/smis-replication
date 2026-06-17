rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

sample_kind <- "intensive"
strong_only <- TRUE
results_subdir <- "first_stage_intensive_all_transforms_strong"
file_stub <- "first_stage_intensive_all_transforms_strong"
table_stub <- "first_stage_intensive_all_transforms_strong"
table_title <- "Sample-weighted average marginal effect of an additional strongly-followed SMI being assigned to treatment on number of ad retweets and SMI retweets"

source("first_stage_all_transforms_tables_runner.R", local = environment())
