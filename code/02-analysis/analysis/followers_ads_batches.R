# 0.0 Set up the environment and locate this script
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
source("supplemental_outcome_helpers.R")

initial_path <- repo_initial_path()
src_path <- paste0(initial_path, "src/utils/")
source_files <- c(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)

map(paste0(src_path, source_files), source)
ipak(packages)

# 2.0 Constants
file_stub <- "followers_ads"
results_root <- results_path(file_stub)
estimates_dir <- file.path(results_root, "estimates")
plots_dir <- file.path(results_root, "plots")
n_posts_thr <- 0
influencer_thr <- 9
batch_specs <- list(
  b1 = "b1"
)

dir.create(results_root, showWarnings = FALSE, recursive = TRUE)
dir.create(estimates_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

extract_ads_followers <- function(data) {
  formulas <- list(
    AC = AC ~ ads_treatment + AC_base | strat_block1,
    SMIs = SMIs ~ ads_treatment | strat_block1 + total_influencers
  )

  map_dfr(names(formulas), function(var_name) {
    fit <- feols(formulas[[var_name]], data = data, vcov = "HC1")

    tibble(
      var = var_name,
      coef = unname(coef(fit)[["ads_treatment"]]),
      sd = unname(se(fit)[["ads_treatment"]])
    )
  })
}

analysis_df <- load_followers_base_data(initial_path) |>
  filter(below_p90 == 1) |>
  filter(n_posts_base > n_posts_thr) |>
  filter(total_influencers < influencer_thr)

for (batch_name in names(batch_specs)) {
  message("Running followers ads sample: ", batch_name)

  batch_filter <- batch_specs[[batch_name]]
  final <- analysis_df |>
    prepare_batch_data(batch_filter) |>
    extract_ads_followers() |>
    mutate(
      Variable = unname(followers_label_map[var]),
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  final$Variable <- factor(final$Variable, levels = followers_order)

  write_xlsx(
    final |> select(-lower, -upper),
    file.path(estimates_dir, paste0(file_stub, "_", batch_name, "_estimates.xlsx"))
  )

  write_horizontal_plot(
    final = final,
    plot_path = file.path(plots_dir, paste0(file_stub, "_", batch_name, ".pdf")),
    xlab_text = "Ads treatment estimate with 95% confidence interval"
  )
}
