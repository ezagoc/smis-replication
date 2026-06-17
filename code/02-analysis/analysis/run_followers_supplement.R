rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This supplement is limited to follower outcomes observed in batch 1 only.
# It includes the aggregated SMI-treatment specification and the aggregated
# ads-treatment specification, but does not run stage-by-stage or batch 2/both
# batch variants.

message("Running aggregated follower outcomes for SMI treatment (Batch 1 only)")
source("followers_aggregate_batches_1000.R")

message("Running aggregated follower outcomes for ads treatment (Batch 1 only)")
source("followers_ads_batches.R")
