source("../analysis/supplemental_outcome_helpers.R")

load_attrition_context <- function() {
  initial_path <- repo_initial_path()
  src_path <- paste0(initial_path, "src/utils/")
  source_files <- c(
    "funcs_analysis.R",
    "constants_final.R",
    "import_data.R"
  )

  lapply(paste0(src_path, source_files), source)
  ipak(packages)

  initial_path
}

batch_specs <- list(
  b1 = "b1",
  b2 = "b2",
  both = NULL
)

batch_labels <- c(
  b1 = "Batch 1",
  b2 = "Batch 2",
  both = "Both batches"
)

attrition_outcome_labels <- c(
  dummy_attrition = "Not Yet Scraped",
  overall_blocked = "Blocked/Unavailable",
  dummy_both = "Any Attrition"
)

sample_restriction_labels <- c(
  below_p90 = "Within current percentile sample",
  positive_baseline_posts = "Users with >0 baseline posts"
)

attrition_table_batch_order <- c("b1", "b2")

attrition_table_outcomes_by_batch <- list(
  b1 = c("overall_blocked", "dummy_both"),
  b2 = c("dummy_attrition", "overall_blocked", "dummy_both")
)

load_stage1_analysis_frame <- function(initial_path = repo_initial_path()) {
  get_analysis_ver_final_winsor(
    stage = "stage1_2",
    batches = "b1b2",
    initial_path = initial_path
  ) |>
    left_join(load_belp90(initial_path), by = c("follower_id", "batch_id", "pais")) |>
    mutate(
      strat_block1 = paste0(strat_block1, batch_id, pais)
    )
}

load_attrition_outcomes <- function(initial_path = repo_initial_path()) {
  attrition_path_candidates <- c(
    paste0(initial_path, "data/others/attrition.parquet"),
    paste0(initial_path, "data/04-analysis/attrition.parquet")
  )

  existing_paths <- attrition_path_candidates[file.exists(attrition_path_candidates)]

  if (length(existing_paths) == 0) {
    stop(
      "Could not find attrition.parquet in either data/others/ or data/04-analysis/.",
      call. = FALSE
    )
  }

  attrition_path <- existing_paths[[1]]

  read_parquet(attrition_path) |>
    select(username, pais, overall_blocked, dummy_attrition, dummy_both) |>
    distinct(username, pais, .keep_all = TRUE)
}

attach_attrition_outcomes <- function(data, initial_path = repo_initial_path()) {
  attrition <- load_attrition_outcomes(initial_path)

  data |>
    left_join(attrition, by = c("username", "pais")) |>
    mutate(
      across(
        any_of(names(attrition_outcome_labels)),
        \(x) ifelse(is.na(x), 0, as.numeric(x))
      )
    )
}

add_sample_restriction_outcomes <- function(data) {
  data |>
    mutate(
      below_p90 = ifelse(is.na(below_p90), NA_real_, as.numeric(below_p90)),
      positive_baseline_posts = as.numeric(n_posts_base > 0)
    )
}

prepare_extensive_attrition_sample <- function(apply_current_restrictions = TRUE, initial_path = repo_initial_path()) {
  data <- load_stage1_analysis_frame(initial_path) |>
    attach_attrition_outcomes(initial_path) |>
    add_sample_restriction_outcomes() |>
    filter(total_influencers == 1)

  if (apply_current_restrictions) {
    data <- data |>
      filter(below_p90 == 1) |>
      filter(n_posts_base > 0)
  }

  data
}

prepare_intensive_attrition_sample <- function(apply_current_restrictions = TRUE, initial_path = repo_initial_path()) {
  data <- load_stage1_analysis_frame(initial_path) |>
    attach_attrition_outcomes(initial_path) |>
    add_sample_restriction_outcomes() |>
    filter(total_influencers < 9)

  if (apply_current_restrictions) {
    data <- data |>
      filter(below_p90 == 1) |>
      filter(n_posts_base > 0)
  }

  data
}

prepare_ads_attrition_sample <- function(apply_current_restrictions = TRUE, initial_path = repo_initial_path()) {
  data <- load_stage1_analysis_frame(initial_path) |>
    attach_attrition_outcomes(initial_path) |>
    add_sample_restriction_outcomes() |>
    filter(total_influencers < 9)

  if (apply_current_restrictions) {
    data <- data |>
      filter(below_p90 == 1) |>
      filter(n_posts_base > 0)
  }

  data
}

pure_control_subset <- function(data) {
  if (all(c("ads_treatment", "total_treated") %in% names(data))) {
    return(data |> filter(ads_treatment == 0, total_treated == 0))
  }

  if ("ads_treatment" %in% names(data)) {
    return(data |> filter(ads_treatment == 0))
  }

  if ("total_treated" %in% names(data)) {
    return(data |> filter(total_treated == 0))
  }

  data
}

control_means_for_outcomes <- function(data, outcome_vars) {
  control_df <- pure_control_subset(data)

  stats::setNames(
    vapply(
      outcome_vars,
      function(outcome_var) mean(control_df[[outcome_var]], na.rm = TRUE),
      numeric(1)
    ),
    outcome_vars
  )
}

stars_from_pvalue <- function(pvalue) {
  if (is.na(pvalue)) {
    return("")
  }

  if (pvalue < 0.01) {
    return("***")
  }

  if (pvalue < 0.05) {
    return("**")
  }

  if (pvalue < 0.1) {
    return("*")
  }

  ""
}

format_estimate_cell <- function(estimate, pvalue, digits = 3) {
  if (is.na(estimate)) {
    return("")
  }

  paste0(formatC(estimate, format = "f", digits = digits), stars_from_pvalue(pvalue))
}

format_se_cell <- function(se, digits = 3) {
  if (is.na(se)) {
    return("")
  }

  paste0("(", formatC(se, format = "f", digits = digits), ")")
}

format_mean_cell <- function(value, digits = 3) {
  if (is.na(value)) {
    return("")
  }

  formatC(value, format = "f", digits = digits)
}

batch_result_frame <- function(batch_name, outcome_vars, control_means) {
  tibble(
    batch = batch_name,
    outcome_var = outcome_vars,
    control_mean = unname(control_means[outcome_vars])
  )
}

extract_extensive_point_estimates <- function(data, outcome_vars, context_label = "", allow_missing = FALSE) {
  estimates <- map_dbl(outcome_vars, function(outcome_var) {
    fmla <- as.formula(paste0(outcome_var, " ~ total_treated | pais + batch_id"))

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
    estimate <- if ("total_treated" %in% names(fit_coefs)) {
      unname(fit_coefs[["total_treated"]])
    } else {
      NA_real_
    }

    if (length(estimate) == 0 || is.na(estimate)) {
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

  stats::setNames(as.list(estimates), outcome_vars) |>
    as_tibble()
}

extract_intensive_point_estimates <- function(data, outcome_vars, int_cols, context_label = "", allow_missing = FALSE) {
  int_part <- paste(int_cols, collapse = " + ")
  rhs_controls <- paste(c("total_treated", int_part), collapse = " + ")

  estimates <- map_dbl(outcome_vars, function(outcome_var) {
    fmla <- as.formula(
      paste0(
        outcome_var,
        " ~ ",
        rhs_controls,
        " | total_influencers + pais + batch_id"
      )
    )

    fit <- tryCatch(
      feols(fmla, data = data, weights = ~weights),
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
    estimate <- if ("total_treated" %in% names(fit_coefs)) {
      unname(fit_coefs[["total_treated"]])
    } else {
      NA_real_
    }

    if (length(estimate) == 0 || is.na(estimate)) {
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

  stats::setNames(as.list(estimates), outcome_vars) |>
    as_tibble()
}

build_intensive_design <- function(data) {
  weighted_df <- data |>
    mutate(weights = ifelse(total_influencers > 0, 1 / total_influencers, NA_real_)) |>
    filter(!is.na(weights))

  interaction_fit <- build_intensive_interactions(weighted_df)

  list(
    data = interaction_fit$data,
    int_cols = interaction_fit$int_cols
  )
}

run_ads_balance_analysis <- function(data, outcome_vars) {
  map_dfr(names(batch_specs), function(batch_name) {
    batch_df <- prepare_batch_data(data, batch_specs[[batch_name]])
    control_means <- control_means_for_outcomes(batch_df, outcome_vars)

    map_dfr(outcome_vars, function(outcome_var) {
      fmla <- as.formula(paste0(outcome_var, " ~ ads_treatment | strat_block1"))
      fit <- feols(fmla, data = batch_df, vcov = "HC1")

      tibble(
        batch = batch_name,
        outcome_var = outcome_var,
        coef = unname(coef(fit)[["ads_treatment"]]),
        se = unname(se(fit)[["ads_treatment"]]),
        pvalue = unname(coeftable(fit)["ads_treatment", "Pr(>|t|)"]),
        control_mean = unname(control_means[[outcome_var]]),
        n = nobs(fit)
      )
    })
  })
}

run_extensive_balance_analysis <- function(data, outcome_vars, initial_path = repo_initial_path(), n_permutations = 1000) {
  map_dfr(names(batch_specs), function(batch_name) {
    batch_df <- prepare_batch_data(data, batch_specs[[batch_name]])
    control_means <- control_means_for_outcomes(batch_df, outcome_vars)

    point_estimates <- extract_extensive_point_estimates(
      data = batch_df,
      outcome_vars = outcome_vars,
      context_label = paste(batch_name, "original")
    )

    permutation_estimates <- map_dfr(seq_len(n_permutations), function(i) {
      perm_treated_col <- paste0("n_influencers_followed_treatment_p", i)

      permuted_counts <- read_permutation_counts(
        i = i,
        cols = perm_treated_col,
        initial_path = initial_path
      )

      permuted_df <- batch_df |>
        left_join(permuted_counts, by = c("follower_id", "pais", "batch_id"))

      permuted_df$total_treated <- permuted_df[[perm_treated_col]]
      permuted_df[[perm_treated_col]] <- NULL

      extract_extensive_point_estimates(
        data = permuted_df,
        outcome_vars = outcome_vars,
        context_label = paste(batch_name, "permutation", i),
        allow_missing = TRUE
      )
    })

    se_vec <- permutation_estimates |>
      summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))

    batch_result_frame(batch_name, outcome_vars, control_means) |>
      mutate(
        coef = map_dbl(outcome_var, \(x) as.numeric(point_estimates[[x]][[1]])),
        se = map_dbl(outcome_var, \(x) as.numeric(se_vec[[x]][[1]])),
        pvalue = 2 * pnorm(abs(coef / se), lower.tail = FALSE),
        n = nrow(batch_df)
      )
  })
}

run_intensive_balance_analysis <- function(data, outcome_vars, initial_path = repo_initial_path(), n_permutations = 1000) {
  map_dfr(names(batch_specs), function(batch_name) {
    batch_df <- prepare_batch_data(data, batch_specs[[batch_name]])
    control_means <- control_means_for_outcomes(batch_df, outcome_vars)

    original_design <- build_intensive_design(batch_df)

    point_estimates <- extract_intensive_point_estimates(
      data = original_design$data,
      outcome_vars = outcome_vars,
      int_cols = original_design$int_cols,
      context_label = paste(batch_name, "original")
    )

    permutation_estimates <- map_dfr(seq_len(n_permutations), function(i) {
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

      permuted_design <- build_intensive_design(permuted_df)

      extract_intensive_point_estimates(
        data = permuted_design$data,
        outcome_vars = outcome_vars,
        int_cols = permuted_design$int_cols,
        context_label = paste(batch_name, "permutation", i),
        allow_missing = TRUE
      )
    })

    se_vec <- permutation_estimates |>
      summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))

    batch_result_frame(batch_name, outcome_vars, control_means) |>
      mutate(
        coef = map_dbl(outcome_var, \(x) as.numeric(point_estimates[[x]][[1]])),
        se = map_dbl(outcome_var, \(x) as.numeric(se_vec[[x]][[1]])),
        pvalue = 2 * pnorm(abs(coef / se), lower.tail = FALSE),
        n = nrow(original_design$data)
      )
  })
}

latex_escape <- function(x) {
  x |>
    gsub("\\\\", "\\\\textbackslash{}", x = _, fixed = FALSE) |>
    gsub("%", "\\\\%", x = _, fixed = TRUE) |>
    gsub("&", "\\\\&", x = _, fixed = TRUE) |>
    gsub("_", "\\\\_", x = _, fixed = TRUE)
}

build_attrition_table_tex <- function(
  results_df,
  outcome_labels,
  caption,
  label,
  treatment_label,
  notes,
  spec_rows = list(),
  digits = 3,
  batch_order = names(batch_specs),
  batch_labels_override = batch_labels,
  outcome_order_by_batch = NULL
) {
  if (is.null(outcome_order_by_batch)) {
    outcome_order_by_batch <- set_names(
      replicate(length(batch_order), names(outcome_labels), simplify = FALSE),
      batch_order
    )
  }

  outcome_counts <- map_int(batch_order, \(batch_name) length(outcome_order_by_batch[[batch_name]]))
  total_columns <- sum(outcome_counts)
  tabular_spec <- paste0("l", paste(rep("c", total_columns), collapse = ""))

  cmidrules <- vapply(
    seq_along(batch_order),
    function(i) {
      start_col <- 2 + sum(outcome_counts[seq_len(i - 1)])
      end_col <- start_col + outcome_counts[[i]] - 1
      paste0("\\cmidrule(lr){", start_col, "-", end_col, "}")
    },
    character(1)
  )

  header_batches <- paste(
    c(
      "",
      vapply(
        batch_order,
        function(batch_name) {
          paste0(
            "\\multicolumn{",
            length(outcome_order_by_batch[[batch_name]]),
            "}{c}{",
            batch_labels_override[[batch_name]],
            "}"
          )
        },
        character(1)
      )
    ),
    collapse = " & "
  )

  header_outcomes <- paste(
    c(
      "",
      unlist(
        map(
          batch_order,
          \(batch_name) unname(outcome_labels[outcome_order_by_batch[[batch_name]]])
        ),
        use.names = FALSE
      )
    ),
    collapse = " & "
  )

  lookup_cells <- function(value_col, formatter) {
    values <- c()

    for (batch_name in batch_order) {
      for (current_outcome_var in outcome_order_by_batch[[batch_name]]) {
        match_row <- results_df |>
          filter(batch == batch_name, outcome_var == current_outcome_var)

        cell_value <- if (nrow(match_row) == 0) {
          NA_real_
        } else {
          match_row[[value_col]][[1]]
        }

        if (identical(value_col, "coef")) {
          pvalue <- if (nrow(match_row) == 0) NA_real_ else match_row$pvalue[[1]]
          values <- c(values, formatter(cell_value, pvalue))
        } else {
          values <- c(values, formatter(cell_value))
        }
      }
    }

    values
  }

  estimate_row <- paste(c(treatment_label, lookup_cells("coef", format_estimate_cell)), collapse = " & ")
  se_row <- paste(c("", lookup_cells("se", format_se_cell)), collapse = " & ")
  mean_row <- paste(c("Pure control mean", lookup_cells("control_mean", format_mean_cell)), collapse = " & ")
  n_row <- paste(
    c(
      "Observations",
      unlist(
        map(
          batch_order,
          function(batch_name) {
            batch_n <- results_df |>
              filter(batch == batch_name) |>
              summarise(n = max(n, na.rm = TRUE)) |>
              pull(n)

            rep(as.character(batch_n[[1]]), times = length(outcome_order_by_batch[[batch_name]]))
          }
        ),
        use.names = FALSE
      )
    ),
    collapse = " & "
  )

  spec_lines <- vapply(
    spec_rows,
    function(row) {
      paste(c(row$label, rep(row$value, times = total_columns)), collapse = " & ")
    },
    character(1)
  )

  lines <- c(
    "\\begin{table}[!htbp]",
    "    \\centering",
    paste0("    \\caption{", caption, "}"),
    paste0("    \\label{", label, "}"),
    "    \\scriptsize",
    "    \\begin{adjustbox}{width=\\textwidth}",
    paste0("    \\begin{tabular}{", tabular_spec, "}"),
    "    \\toprule",
    paste0("    ", header_batches, " \\\\"),
    paste0("    ", paste(cmidrules, collapse = " ")),
    paste0("    ", header_outcomes, " \\\\"),
    "    \\midrule",
    paste0("    ", estimate_row, " \\\\"),
    paste0("    ", se_row, " \\\\"),
    paste0("    ", mean_row, " \\\\"),
    paste0("    ", n_row, " \\\\")
  )

  if (length(spec_lines) > 0) {
    lines <- c(lines, paste0("    ", spec_lines, " \\\\"))
  }

  lines <- c(
    lines,
    "    \\bottomrule",
    "    \\end{tabular}",
    "    \\end{adjustbox}",
    paste0(
      "    \\floatfoot{\\textit{Notes:} ",
      notes,
      "}"
    ),
    "\\end{table}"
  )

  paste(lines, collapse = "\n")
}

write_attrition_table <- function(
  results_df,
  outcome_labels,
  output_path,
  caption,
  label,
  treatment_label,
  notes,
  spec_rows = list(),
  batch_order = names(batch_specs),
  batch_labels_override = batch_labels,
  outcome_order_by_batch = NULL
) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  tex <- build_attrition_table_tex(
    results_df = results_df,
    outcome_labels = outcome_labels,
    caption = caption,
    label = label,
    treatment_label = treatment_label,
    notes = notes,
    spec_rows = spec_rows,
    batch_order = batch_order,
    batch_labels_override = batch_labels_override,
    outcome_order_by_batch = outcome_order_by_batch
  )

  writeLines(tex, output_path)
}

write_attrition_results <- function(results_df, stem, results_root = results_path("attrition")) {
  tables_data_dir <- file.path(results_root, "tables_data")
  dir.create(tables_data_dir, recursive = TRUE, showWarnings = FALSE)
  write_xlsx(results_df, file.path(tables_data_dir, paste0(stem, ".xlsx")))
}
