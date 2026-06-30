rm(list = ls())

resolve_script_dir <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(env) {
      ofile <- env$ofile
      if (is.null(ofile)) "" else ofile
    },
    character(1)
  )

  frame_files <- frame_files[nzchar(frame_files)]

  if (length(frame_files) > 0) {
    return(dirname(normalizePath(tail(frame_files, 1), winslash = "/", mustWork = TRUE)))
  }

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

display_df <- results_df |>
  filter(batch %in% attrition_table_batch_order) |>
  filter(!(batch == "b1" & outcome_var == "dummy_attrition"))

write_xlsx(
  display_df,
  file.path(tables_data_dir, "attrition_ads.xlsx")
)

write_attrition_table(
  results_df = display_df,
  outcome_labels = attrition_outcome_labels,
  output_path = file.path(tables_dir, "attrition_ads.tex"),
  caption = "Attrition balance: ads specification",
  label = "tab:attrition_ads",
  treatment_label = "Ads treatment",
  notes = paste(
    "Columns report the effect of ads treatment on endline attrition outcomes for the current ads-analysis sample.",
    "The sample applies the same baseline-posting, percentile, and influencer-count restrictions used in the main ads regressions.",
    "Batch 1 and Batch 2 are shown separately; the not-yet-scraped outcome is only relevant for Batch 2 because Batch 1 scraping is complete.",
    "Standard errors are heteroskedasticity-robust."
  ),
  spec_rows = list(
    list(label = "Current ads sample restrictions", value = "Yes"),
    list(label = "Stratification block FE", value = "Yes"),
    list(label = "Permutation-based SE", value = "No")
  ),
  batch_order = attrition_table_batch_order,
  outcome_order_by_batch = attrition_table_outcomes_by_batch
)
