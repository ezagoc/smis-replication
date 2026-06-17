rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

sample_kind <- "extensive"
strong_only <- FALSE
results_subdir <- "first_stage_extensive_all_transforms"
file_stub <- "first_stage_extensive_all_transforms"
table_stub <- "first_stage_extensive_all_transforms"
table_title <- "Average effect of one initially-followed SMI being assigned to treatment on number of ad retweets and SMI retweets"

source("first_stage_all_transforms_tables_runner.R", local = environment())
