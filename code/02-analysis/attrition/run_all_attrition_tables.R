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

script_dir <- resolve_script_dir()
setwd(script_dir)

run_isolated_source <- function(path) {
  full_path <- normalizePath(file.path(script_dir, path), winslash = "/", mustWork = TRUE)

  message("")
  message("========================================")
  message("Sourcing: ", basename(full_path))
  message("========================================")

  source(
    full_path,
    local = new.env(parent = globalenv()),
    echo = FALSE,
    chdir = TRUE
  )
}

scripts_to_run <- c(
  #"00-preprocess_attrition_data.R",
  "01-attrition_ads.R",
  "02-attrition_extensive.R",
  "03-attrition_intensive.R",
  "04-sample_restriction_balance.R"
)

invisible(lapply(scripts_to_run, run_isolated_source))
