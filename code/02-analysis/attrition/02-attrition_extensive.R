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

analysis_df <- prepare_extensive_attrition_sample(
  apply_current_restrictions = TRUE,
  initial_path = initial_path
)

results_df <- run_extensive_balance_analysis(
  data = analysis_df,
  outcome_vars = names(attrition_outcome_labels),
  initial_path = initial_path,
  n_permutations = 1000
)

write_xlsx(
  results_df,
  file.path(tables_data_dir, "attrition_extensive.xlsx")
)

write_attrition_table(
  results_df = results_df,
  outcome_labels = attrition_outcome_labels,
  output_path = file.path(tables_dir, "attrition_extensive.tex"),
  caption = "Attrition balance: extensive specification",
  label = "tab:attrition_extensive",
  treatment_label = "Treated SMI",
  notes = paste(
    "Columns report the effect of being assigned to one treated initially-followed SMI on endline attrition outcomes for the current extensive-analysis sample.",
    "The sample applies the same one-influencer, baseline-posting, and percentile restrictions used in the main extensive regressions.",
    "Batch 1, Batch 2, and both batches pooled are shown separately.",
    "Standard errors are permutation-based standard deviations computed from 1,000 reassigned treatment draws."
  ),
  spec_rows = list(
    list(label = "Current extensive sample restrictions", value = "Yes"),
    list(label = "Country and batch FE", value = "Yes"),
    list(label = "Permutation-based SE", value = "Yes")
  )
)
