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
  "funcs_analysis.R",
  "constants_final.R",
  "import_data.R"
)

map(paste0(src_path, source_files), source)
ipak(c(packages, "fastDummies"))

# 2.0 Constants
file_stub <- "followers_intensive"
results_root <- results_path(file_stub)
original_dir <- file.path(results_root, "original")
permutations_dir <- file.path(results_root, "permutations")
plots_dir <- file.path(results_root, "plots")
n_permutations <- 1000
n_posts_thr <- 0
influencer_thr <- 9
batch_specs <- list(
  b1 = "b1"
)

dir.create(results_root, showWarnings = FALSE, recursive = TRUE)
dir.create(original_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(permutations_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

extract_intensive_followers <- function(data, int_cols, context_label = "", allow_missing = FALSE) {
  ac_rhs <- compose_rhs("total_treated", "AC_base", int_fes, int_cols)
  smi_rhs <- compose_rhs("total_treated", int_fes, int_cols)

  formulas <- list(
    AC = as.formula(paste0("AC ~ ", ac_rhs, " | total_influencers + pais + batch_id")),
    SMIs = as.formula(paste0("SMIs ~ ", smi_rhs, " | total_influencers + pais + batch_id"))
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
  filter(below_p90 == 1) |>
  filter(n_posts_base > n_posts_thr) |>
  filter(total_influencers < influencer_thr) |>
  join_intensive_fes(initial_path = initial_path)

for (batch_name in names(batch_specs)) {
  message("Running intensive followers sample: ", batch_name)

  batch_filter <- batch_specs[[batch_name]]
  batch_df <- prepare_batch_data(analysis_df, batch_filter)
  interaction_fit <- build_intensive_interactions(batch_df)

  original_coefs <- extract_intensive_followers(
    data = interaction_fit$data,
    int_cols = interaction_fit$int_cols
  )

  permutation_coefs <- map_dfr(seq_len(n_permutations), function(i) {
    message("Permutation ", i, " / ", n_permutations, " for ", batch_name)

    perm_treated_col <- paste0("n_influencers_followed_treatment_p", i)
    perm_total_col <- paste0("n_influencers_followed_p_", i)

    permuted_counts <- read_permutation_counts(
      i = i,
      cols = c(perm_treated_col, perm_total_col),
      initial_path = initial_path
    )

    permuted_df <- batch_df |>
      left_join(permuted_counts, by = c("follower_id", "pais", "batch_id"))

    permuted_df$total_treated <- permuted_df[[perm_treated_col]]
    permuted_df$total_influencers <- permuted_df[[perm_total_col]]
    permuted_df[[perm_treated_col]] <- NULL
    permuted_df[[perm_total_col]] <- NULL

    permuted_interactions <- build_intensive_interactions(permuted_df)

    extract_intensive_followers(
      data = permuted_interactions$data,
      int_cols = permuted_interactions$int_cols,
      context_label = paste(batch_name, "permutation", i),
      allow_missing = TRUE
    )
  })

  write_outputs(batch_name, original_coefs, permutation_coefs)
}
