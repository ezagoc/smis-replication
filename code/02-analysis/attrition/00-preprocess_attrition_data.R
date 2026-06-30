###################################################
## Data Preparation
## Author: Eduardo Zago-Cuevas
## Output: attrition.parquet in data/others
###################################################

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

repo_root <- normalizePath(file.path(getwd(), "..", "..", ".."), winslash = "/", mustWork = TRUE)
others_dir <- file.path(repo_root, "data", "others")
output_path <- file.path(others_dir, "attrition.parquet")

if (file.exists(output_path)) {
  message("attrition.parquet already exists in data/others; skipping rebuild.")
} else {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "readxl is required to rebuild attrition.parquet from the raw Excel files.",
      call. = FALSE
    )
  }

  ke <- as.data.frame(readxl::read_excel(file.path(others_dir, "rescrapped_cases2_KE.xlsx")))
  ke <- ke[c("follower_handle", "overall_success", "overall_blocked")]
  ke$pais <- "KE"
  ke$dummy_attrition <- ifelse(ke$overall_success == 0, 1, 0)
  ke$dummy_both <- ifelse(ke$dummy_attrition == 1 | ke$overall_blocked == 1, 1, 0)

  sa <- as.data.frame(readxl::read_excel(file.path(others_dir, "rescrapped_cases2_SA.xlsx")))
  sa <- sa[c("follower_handle", "overall_success", "overall_blocked")]
  sa$pais <- "SA"
  sa$dummy_attrition <- ifelse(sa$overall_success == 0, 1, 0)
  sa$dummy_both <- ifelse(sa$dummy_attrition == 1 | sa$overall_blocked == 1, 1, 0)

  final <- rbind(ke, sa)
  names(final)[names(final) == "follower_handle"] <- "username"

  arrow::write_parquet(final, output_path)
}
