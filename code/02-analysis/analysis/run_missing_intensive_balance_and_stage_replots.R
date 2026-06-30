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

run_isolated_source <- function(path) {
  message("")
  message("========================================")
  message("Sourcing: ", path)
  message("========================================")

  source(
    path,
    local = new.env(parent = globalenv()),
    echo = FALSE,
    chdir = FALSE
  )
}

scripts_to_run <- c(
  "intensive_baseline_weighted_batches_1000.R",
  "intensive_baseline_weighted_batches_1000_strong.R",
  "../graphs/plot_saved_results_only.R"
)

invisible(lapply(scripts_to_run, run_isolated_source))
