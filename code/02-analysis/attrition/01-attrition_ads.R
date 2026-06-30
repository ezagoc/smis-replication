rm(list = ls())

resolve_script_dir <- function() {
  rstudio_path <- tryCatch(
    {
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        rstudioapi::getActiveDocumentContext()$path
      } else {
        ""
      }
    },
    error = function(...) ""
  )

  if (nzchar(rstudio_path)) {
    return(dirname(normalizePath(rstudio_path, winslash = "/", mustWork = TRUE)))
  }

  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    if (file.exists(script_path)) {
      return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

setwd(resolve_script_dir())

source("attrition_helpers.R")

initial_path <- load_attrition_context()

results_root <- results_path("attrition")
tables_dir <- file.path(results_root, "tables")
tables_data_dir <- file.path(results_root, "tables_data")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_data_dir, recursive = TRUE, showWarnings = FALSE)

analysis_df <- prepare_ads_attrition_sample(
  apply_current_restrictions = TRUE,
  initial_path = initial_path
)

results_df <- run_ads_balance_analysis(
  data = analysis_df,
  outcome_vars = names(attrition_outcome_labels)
)

write_xlsx(
  results_df,
  file.path(tables_data_dir, "attrition_ads.xlsx")
)

write_attrition_table(
  results_df = results_df,
  outcome_labels = attrition_outcome_labels,
  output_path = file.path(tables_dir, "attrition_ads.tex"),
  caption = "Attrition balance: ads specification",
  label = "tab:attrition_ads",
  treatment_label = "Ads treatment",
  notes = paste(
    "Columns report the effect of ads treatment on endline attrition outcomes for the current ads-analysis sample.",
    "The sample applies the same baseline-posting, percentile, and influencer-count restrictions used in the main ads regressions.",
    "Batch 1, Batch 2, and both batches pooled are shown separately.",
    "Standard errors are heteroskedasticity-robust."
  ),
  spec_rows = list(
    list(label = "Current ads sample restrictions", value = "Yes"),
    list(label = "Stratification block FE", value = "Yes"),
    list(label = "Permutation-based SE", value = "No")
  )
)
