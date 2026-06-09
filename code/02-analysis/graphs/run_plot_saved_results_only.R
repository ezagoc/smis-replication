rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

message("Rebuilding paper-facing plots from saved results")
source("plot_saved_results_only.R")

