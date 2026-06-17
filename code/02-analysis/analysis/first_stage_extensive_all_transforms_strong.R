rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

sample_kind <- "extensive"
strong_only <- TRUE
results_subdir <- "first_stage_extensive_all_transforms_strong"
file_stub <- "first_stage_extensive_all_transforms_strong"

source("first_stage_all_transforms_runner.R", local = environment())
