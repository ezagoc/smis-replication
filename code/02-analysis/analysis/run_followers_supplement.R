rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

message("Running followers extensive analyses")
source("followers_extensive_batches_1000.R")

message("Running followers extensive strong analyses")
source("followers_extensive_batches_1000_strong.R")

message("Running followers intensive analyses")
source("followers_intensive_batches_1000.R")

message("Running followers stage analyses")
source("followers_stage_batches_1000.R")

message("Running followers aggregate analyses")
source("followers_aggregate_batches_1000.R")

message("Running followers ads analyses")
source("followers_ads_batches.R")
