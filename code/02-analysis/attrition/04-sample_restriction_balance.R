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

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

write_restriction_table <- function(results_df, file_stub, caption, label, treatment_label, notes, spec_rows) {
  write_attrition_results(results_df, file_stub, results_root = results_root)

  write_attrition_table(
    results_df = results_df,
    outcome_labels = sample_restriction_labels,
    output_path = file.path(tables_dir, paste0(file_stub, ".tex")),
    caption = caption,
    label = label,
    treatment_label = treatment_label,
    notes = notes,
    spec_rows = spec_rows
  )
}

extensive_results <- run_extensive_balance_analysis(
  data = prepare_extensive_attrition_sample(
    apply_current_restrictions = FALSE,
    initial_path = initial_path
  ),
  outcome_vars = names(sample_restriction_labels),
  initial_path = initial_path,
  n_permutations = 1000
)

write_restriction_table(
  results_df = extensive_results,
  file_stub = "sample_restrictions_extensive",
  caption = "Balance of current sample restrictions: extensive specification",
  label = "tab:sample_restrictions_extensive",
  treatment_label = "Treated SMI",
  notes = paste(
    "Columns report the effect of treatment assignment on indicators for satisfying the current extensive-analysis sample restrictions.",
    "The first outcome is the indicator for remaining inside the current percentile-screen sample, and the second outcome indicates whether a user made more than zero baseline posts.",
    "The one-influencer extensive sample definition is retained, but these two restriction outcomes are not imposed before estimation.",
    "Standard errors are permutation-based standard deviations computed from 1,000 reassigned treatment draws."
  ),
  spec_rows = list(
    list(label = "One-influencer sample", value = "Yes"),
    list(label = "Country and batch FE", value = "Yes"),
    list(label = "Permutation-based SE", value = "Yes")
  )
)

intensive_results <- run_intensive_balance_analysis(
  data = prepare_intensive_attrition_sample(
    apply_current_restrictions = FALSE,
    initial_path = initial_path
  ),
  outcome_vars = names(sample_restriction_labels),
  initial_path = initial_path,
  n_permutations = 1000
)

write_restriction_table(
  results_df = intensive_results,
  file_stub = "sample_restrictions_intensive",
  caption = "Balance of current sample restrictions: intensive specification",
  label = "tab:sample_restrictions_intensive",
  treatment_label = "Additional treated SMI",
  notes = paste(
    "Columns report the sample-weighted marginal effect of one additional treated initially-followed SMI on indicators for satisfying the current intensive-analysis sample restrictions.",
    "The first outcome is the indicator for remaining inside the current percentile-screen sample, and the second outcome indicates whether a user made more than zero baseline posts.",
    "The intensive sample keeps the same influencer-count restriction as the main regressions, but these two restriction outcomes are not imposed before estimation.",
    "Standard errors are permutation-based standard deviations computed from 1,000 reassigned treatment draws."
  ),
  spec_rows = list(
    list(label = "Influencer-count restriction (< 9)", value = "Yes"),
    list(label = "Total-followed, country, and batch FE", value = "Yes"),
    list(label = "Interaction controls", value = "Yes"),
    list(label = "Permutation-based SE", value = "Yes")
  )
)

ads_results <- run_ads_balance_analysis(
  data = prepare_ads_attrition_sample(
    apply_current_restrictions = FALSE,
    initial_path = initial_path
  ),
  outcome_vars = names(sample_restriction_labels)
)

write_restriction_table(
  results_df = ads_results,
  file_stub = "sample_restrictions_ads",
  caption = "Balance of current sample restrictions: ads specification",
  label = "tab:sample_restrictions_ads",
  treatment_label = "Ads treatment",
  notes = paste(
    "Columns report the effect of ads treatment on indicators for satisfying the current ads-analysis sample restrictions.",
    "The first outcome is the indicator for remaining inside the current percentile-screen sample, and the second outcome indicates whether a user made more than zero baseline posts.",
    "The ads sample keeps the same influencer-count restriction as the main regressions, but these two restriction outcomes are not imposed before estimation.",
    "Standard errors are heteroskedasticity-robust."
  ),
  spec_rows = list(
    list(label = "Influencer-count restriction (< 9)", value = "Yes"),
    list(label = "Stratification block FE", value = "Yes"),
    list(label = "Permutation-based SE", value = "No")
  )
)
