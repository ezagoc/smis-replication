rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

message("Running extensive aggregated results")
source("extensive_aggregate_batches_500.R")

message("Running extensive aggregated results for strong followers only")
source("extensive_aggregate_batches_500_strong.R")

message("Running intensive aggregated results")
source("intensive_aggregate_batches_1000.R")

message("Running intensive aggregated results for strong followers only")
source("intensive_aggregate_batches_1000_strong.R")

