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
file_stub <- "followers_extensive_strong"
results_root <- results_path(file_stub)
original_dir <- file.path(results_root, "original")
permutations_dir <- file.path(results_root, "permutations")
plots_dir <- file.path(results_root, "plots")
n_permutations <- 1000
batch_specs <- list(
  b1 = "b1"
)

dir.create(results_root, showWarnings = FALSE, recursive = TRUE)
dir.create(original_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(permutations_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

extract_extensive_followers <- function(data, context_label = "", allow_missing = FALSE) {
  formulas <- list(
    AC = AC ~ total_treated + AC_base | strat_block1,
    SMIs = SMIs ~ total_treated | strat_block1
  )

  estimates <- map_dbl(names(formulas), function(var_name) {
    fit <- tryCatch(
      feols(formulas[[var_name]], data = data),
      error = function(e) e
    )

    if (inherits(fit, "error")) {
      if (allow_missing) {
        warning(
          paste0(
            "feols failed for ", var_name,
            if (nzchar(context_label)) paste0(" (", context_label, ")") else "",
            ": ", conditionMessage(fit)
          ),
          call. = FALSE
        )
        return(NA_real_)
      }

      stop(conditionMessage(fit), call. = FALSE)
    }

    unname(coef(fit)[["total_treated"]])
  })

  stats::setNames(as.list(estimates), names(formulas)) |>
    as_tibble()
}

write_outputs <- function(batch_name, original_coefs, permutation_coefs) {
  file_code <- paste0(file_stub, "_", batch_name, "_1000perm")

  write_xlsx(original_coefs, file.path(original_dir, paste0(file_code, ".xlsx")))
  write_xlsx(permutation_coefs, file.path(permutations_dir, paste0(file_code, ".xlsx")))

  final <- original_coefs |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "coef") |>
    left_join(
      permutation_coefs |>
        summarise(across(everything(), \(x) sd(x, na.rm = TRUE))) |>
        pivot_longer(cols = everything(), names_to = "var", values_to = "sd"),
      by = "var"
    ) |>
    mutate(
      Variable = unname(followers_label_map[var]),
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  final$Variable <- factor(final$Variable, levels = followers_order)

  write_horizontal_plot(
    final = final,
    plot_path = file.path(plots_dir, paste0(file_code, ".pdf")),
    xlab_text = "Total treated estimate with 95% confidence interval"
  )
}

analysis_df <- load_followers_base_data(initial_path) |>
  filter(n_posts_base > 0) |>
  filter(total_influencers == 1) |>
  filter(c_t_strong_total > 0)

for (batch_name in names(batch_specs)) {
  message("Running extensive strong followers sample: ", batch_name)

  batch_filter <- batch_specs[[batch_name]]
  batch_df <- prepare_batch_data(analysis_df, batch_filter)

  original_coefs <- extract_extensive_followers(batch_df)

  permutation_coefs <- map_dfr(seq_len(n_permutations), function(i) {
    message("Permutation ", i, " / ", n_permutations, " for ", batch_name)

    perm_treated_col <- paste0("n_influencers_followed_treatment_p", i)
    permuted_treatment <- read_permutation_counts(
      i = i,
      cols = perm_treated_col,
      initial_path = initial_path
    )

    permuted_df <- batch_df |>
      left_join(permuted_treatment, by = c("follower_id", "pais", "batch_id"))

    permuted_df$total_treated <- permuted_df[[perm_treated_col]]
    permuted_df[[perm_treated_col]] <- NULL

    extract_extensive_followers(
      permuted_df,
      context_label = paste(batch_name, "permutation", i),
      allow_missing = TRUE
    )
  })

  write_outputs(batch_name, original_coefs, permutation_coefs)
}
