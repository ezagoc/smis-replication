rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

sample_kind <- "extensive"
strong_only <- FALSE
results_subdir <- "first_stage_extensive_all_transforms"
file_stub <- "first_stage_extensive_all_transforms"

source("first_stage_all_transforms_runner.R", local = environment())
