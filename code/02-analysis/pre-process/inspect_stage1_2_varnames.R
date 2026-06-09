rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(purrr)

src_path <- "../../../src/utils/"
source_files <- c(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)

map(paste0(src_path, source_files), source)
ipak(packages)

df <- get_analysis_ver_final_winsor(
  stage = "stage1_2",
  batches = "b1b2",
  initial_path = "../../"
)

cat("=== imported stage1_2 columns ===\n")
print(names(df))

pattern <- "ver|true|fake|non_ver|eng|n_posts|share"

cat("\n=== imported stage1_2 extensive-related columns ===\n")
print(sort(grep(pattern, names(df), value = TRUE)))

