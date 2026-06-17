rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
ipak(packages)

list_stages <- c("stage1_2", "stage3_4", "stage5_6")
type <- "log_"
file_stub <- "extensive_verifiability_fes_normal_p90_p5_strong"
n_posts_thr <- 0
n_permutations <- 1000
permutation_suffix <- paste0("_", n_permutations, "perm")

results_root <- results_path("extensive_linear_90p_p5_strong")
original_dir <- file.path(results_root, "original")
permutations_dir <- file.path(results_root, "permutations")
estimates_dir <- file.path(results_root, "estimates")
plots_dir <- file.path(results_root, "plots")

batch_specs <- list(
  b1 = "b1",
  b2 = "b2",
  both = NULL
)

id_cols <- c("follower_id", "pais", "batch_id")
ver_endline_vars <- c(
  "total_shares",
  "total_comments",
  "total_reactions",
  "verifiability",
  "non_ver",
  "true",
  "fake",
  "n_posts"
)
english_endline_vars <- c("eng")
sentiment_endline_vars <- c(
  "n_posts_covid",
  "pos_b_covid",
  "neutral_b_covid",
  "neg_b_covid",
  "n_posts_vax",
  "pos_b_vax",
  "neutral_b_vax",
  "neg_b_vax"
)

dir.create(results_root, showWarnings = FALSE, recursive = TRUE)
dir.create(original_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(permutations_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(estimates_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

collect_stage_data <- function(stage, batch_filter = NULL) {
  ver_df <- get_analysis_ver_final_winsor(
    stage = stage,
    batches = "b1b2",
    initial_path = initial_path
  ) |>
    left_join(load_belp90(initial_path), by = c("follower_id", "batch_id", "pais")) |>
    filter(total_influencers == 1) |>
    filter(c_t_strong_total > 0) |>
    filter(below_p90 == 1) |>
    filter(n_posts_base > n_posts_thr)

  eng_df <- get_analysis_english_winsor(
    stage = stage,
    batches = "b1b2",
    initial_path = initial_path
  )

  sent_df <- get_analysis_sent_bert_final2(
    stage = stage,
    batches = "b1b2",
    initial_path = initial_path
  )

  stage_df <- ver_df |>
    select(
      all_of(id_cols),
      total_treated,
      total_influencers,
      below_p90,
      n_posts_base,
      c_t_strong_total,
      all_of(ver_endline_vars),
      any_of(paste0(ver_endline_vars, "_base"))
    ) |>
    left_join(
      eng_df |>
        select(all_of(id_cols), all_of(english_endline_vars), any_of("eng_base")),
      by = id_cols
    ) |>
    left_join(
      sent_df |>
        select(all_of(id_cols), all_of(sentiment_endline_vars), any_of(paste0(sentiment_endline_vars, "_base"))),
      by = id_cols
    ) |>
    prepare_batch_data(batch_filter)

  all_base_vars <- grep("_base$", names(stage_df), value = TRUE)
  all_endline_vars <- setdiff(
    names(stage_df),
    c(id_cols, "total_treated", "total_influencers", "below_p90", "c_t_strong_total", all_base_vars)
  )
  paired_roots <- all_endline_vars[paste0(all_endline_vars, "_base") %in% all_base_vars]

  stage_df <- stage_df |>
    mutate(
      across(all_of(paired_roots), \(x) log(x + 1), .names = "log_{.col}"),
      across(all_of(paste0(paired_roots, "_base")), \(x) log(x + 1), .names = "log_{.col}")
    )

  list(
    data = stage_df,
    outcome_vars = order_standard_outcome_vars(paste0("log_", paired_roots))
  )
}

extract_total_treated <- function(data, outcome_vars, context_label = "", allow_missing = FALSE) {
  estimates <- map_dbl(outcome_vars, function(outcome_var) {
    fmla <- as.formula(
      paste0(
        outcome_var, " ~ total_treated + ", outcome_var, "_base | ",
        "pais + batch_id"
      )
    )

    fit <- tryCatch(
      feols(fmla, data = data),
      error = function(e) e
    )

    if (inherits(fit, "error")) {
      detail <- paste0(
        "feols failed for ", outcome_var,
        if (nzchar(context_label)) paste0(" (", context_label, ")") else "",
        ": ", conditionMessage(fit)
      )

      if (allow_missing) {
        warning(detail, call. = FALSE)
        return(NA_real_)
      }

      stop(detail, call. = FALSE)
    }

    fit_coefs <- coef(fit)
    has_total_treated <- "total_treated" %in% names(fit_coefs)
    estimate <- if (has_total_treated) unname(fit_coefs[["total_treated"]]) else NA_real_

    if (!has_total_treated || length(estimate) == 0 || is.na(estimate)) {
      detail <- paste0(
        "Missing total_treated estimate for ", outcome_var,
        if (nzchar(context_label)) paste0(" (", context_label, ")") else ""
      )

      if (allow_missing) {
        warning(detail, call. = FALSE)
        return(NA_real_)
      }

      stop(detail, call. = FALSE)
    }

    estimate
  })

  stats::setNames(as.list(estimates), sub("^log_", "", outcome_vars)) |>
    as_tibble()
}

summarise_stage_results <- function(original_coefs, permutation_coefs, stage) {
  original_coefs |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "coef") |>
    left_join(
      permutation_coefs |>
        summarise(across(everything(), \(x) sd(x, na.rm = TRUE))) |>
        pivot_longer(cols = everything(), names_to = "var", values_to = "sd"),
      by = "var"
    ) |>
    mutate(
      stage = stage,
      Variable = unname(standard_outcome_label_map[var]),
      Stage = recode(stage, !!!standard_stage_labels)
    ) |>
    filter(!is.na(Variable))
}

write_outputs <- function(batch_name, stage, original_coefs, permutation_coefs) {
  file_code <- paste0(type, file_stub, "_", batch_name, "_", stage, permutation_suffix)

  write_xlsx(original_coefs, file.path(original_dir, paste0(file_code, ".xlsx")))
  write_xlsx(permutation_coefs, file.path(permutations_dir, paste0(file_code, ".xlsx")))
}

estimate_plot_height <- function(n_outcomes) {
  max(7.5, 0.38 * n_outcomes + 2.25)
}

for (batch_name in names(batch_specs)) {
  message("Running batch sample: ", batch_name)

  batch_filter <- batch_specs[[batch_name]]
  stage_summaries <- list()

  for (stage in list_stages) {
    message("Running stage: ", stage, " for ", batch_name)

    stage_bundle <- collect_stage_data(stage, batch_filter = batch_filter)
    original_coefs <- extract_total_treated(
      data = stage_bundle$data,
      outcome_vars = stage_bundle$outcome_vars,
      context_label = paste(batch_name, stage, "original")
    )

    permutation_coefs <- map_dfr(seq_len(n_permutations), function(i) {
      message("Permutation ", i, " / ", n_permutations, " for ", batch_name, " - ", stage)

      perm_treated_col <- paste0("n_influencers_followed_treatment_p", i)
      perm_total_col <- paste0("n_influencers_followed_p_", i)

      permuted_counts <- read_permutation_counts(
        i = i,
        cols = c(perm_treated_col, perm_total_col),
        initial_path = initial_path
      )

      permuted_df <- stage_bundle$data |>
        left_join(permuted_counts, by = c("follower_id", "pais", "batch_id"))

      permuted_df <- poolTreatmentBalance2(permuted_df, perm_treated_col, perm_total_col)
      permuted_df[[perm_treated_col]] <- NULL
      permuted_df[[perm_total_col]] <- NULL

      extract_total_treated(
        data = permuted_df,
        outcome_vars = stage_bundle$outcome_vars,
        context_label = paste(batch_name, stage, "permutation", i),
        allow_missing = TRUE
      )
    })

    write_outputs(batch_name, stage, original_coefs, permutation_coefs)
    stage_summaries[[stage]] <- summarise_stage_results(original_coefs, permutation_coefs, stage)
  }

  final <- bind_rows(stage_summaries) |>
    mutate(
      lower = coef - 1.96 * sd,
      upper = coef + 1.96 * sd
    )

  final$Variable <- factor(final$Variable, levels = standard_outcome_order)
  final$Stage <- factor(final$Stage, levels = unname(standard_stage_labels))

  write_xlsx(
    final |> select(-lower, -upper),
    file.path(estimates_dir, paste0(type, file_stub, "_", batch_name, permutation_suffix, "_estimates.xlsx"))
  )

  write_stage_plot(
    final = final,
    plot_path = file.path(plots_dir, paste0(type, file_stub, "_", batch_name, permutation_suffix, ".pdf")),
    ylab_text = "Average treatment effect, with 95% confidence interval",
    width = 10,
    height = estimate_plot_height(nlevels(droplevels(final$Variable)))
  )
}
