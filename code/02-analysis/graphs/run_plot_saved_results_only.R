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

message("Rebuilding paper-facing plots from saved results")
source("plot_saved_results_only.R")
