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

results_root <- results_path("attrition")
tables_dir <- file.path(results_root, "tables")
tables_data_dir <- file.path(results_root, "tables_data")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

read_saved_attrition_results <- function(file_stub) {
  readxl::read_excel(file.path(tables_data_dir, paste0(file_stub, ".xlsx"))) |>
    mutate(
      batch = as.character(batch),
      outcome_var = as.character(outcome_var)
    ) |>
    filter(batch %in% attrition_table_batch_order) |>
    filter(!(batch == "b1" & outcome_var == "dummy_attrition"))
}

refresh_table_from_saved_results <- function(
  file_stub,
  caption,
  label,
  treatment_label,
  notes,
  spec_rows
) {
  results_df <- read_saved_attrition_results(file_stub)

  write_attrition_table(
    results_df = results_df,
    outcome_labels = attrition_outcome_labels,
    output_path = file.path(tables_dir, paste0(file_stub, ".tex")),
    caption = caption,
    label = label,
    treatment_label = treatment_label,
    notes = notes,
    spec_rows = spec_rows,
    batch_order = attrition_table_batch_order,
    outcome_order_by_batch = attrition_table_outcomes_by_batch
  )
}

refresh_table_from_saved_results(
  file_stub = "attrition_ads",
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
  )
)

refresh_table_from_saved_results(
  file_stub = "attrition_extensive",
  caption = "Attrition balance: extensive specification",
  label = "tab:attrition_extensive",
  treatment_label = "Treated SMI",
  notes = paste(
    "Columns report the effect of being assigned to one treated initially-followed SMI on endline attrition outcomes for the current extensive-analysis sample.",
    "The sample applies the same one-influencer, baseline-posting, and percentile restrictions used in the main extensive regressions.",
    "Batch 1 and Batch 2 are shown separately; the not-yet-scraped outcome is only relevant for Batch 2 because Batch 1 scraping is complete.",
    "Standard errors are permutation-based standard deviations computed from 1,000 reassigned treatment draws."
  ),
  spec_rows = list(
    list(label = "Current extensive sample restrictions", value = "Yes"),
    list(label = "Country and batch FE", value = "Yes"),
    list(label = "Permutation-based SE", value = "Yes")
  )
)

refresh_table_from_saved_results(
  file_stub = "attrition_intensive",
  caption = "Attrition balance: intensive specification",
  label = "tab:attrition_intensive",
  treatment_label = "Additional treated SMI",
  notes = paste(
    "Columns report the sample-weighted marginal effect of one additional treated initially-followed SMI on endline attrition outcomes for the current intensive-analysis sample.",
    "The sample applies the same baseline-posting, percentile, and influencer-count restrictions used in the main intensive regressions.",
    "Batch 1 and Batch 2 are shown separately; the not-yet-scraped outcome is only relevant for Batch 2 because Batch 1 scraping is complete.",
    "Standard errors are permutation-based standard deviations computed from 1,000 reassigned treatment draws."
  ),
  spec_rows = list(
    list(label = "Current intensive sample restrictions", value = "Yes"),
    list(label = "Total-followed, country, and batch FE", value = "Yes"),
    list(label = "Interaction controls", value = "Yes"),
    list(label = "Permutation-based SE", value = "Yes")
  )
)
