# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)

src_path <- "../../../src/utils/"
source_files <- c(
  "funcs_analysis.R",
  "constants_final.R",
  "import_data.R"
)

map(paste0(src_path, source_files), source)
ipak(packages)

# 2.0 Define constants
country <- "joint"
data_type <- "Baseline"
stage <- "stage1_2"
list_types <- c("log_")
file_stub <- "extensive_verifiability_noblockfes_p90_p0_strong"
n_posts_thr <- 0
n_permutations <- 1000
results_root <- file.path("../../../results", "strong")
original_dir <- file.path(results_root, "original")
permutations_dir <- file.path(results_root, "permutations")
plots_dir <- file.path(results_root, "plots")

outcome_roots <- c(
  "total_shares", "total_comments", "total_reactions",
  "verifiability", "non_ver", "true", "fake", "n_posts", "eng",
  "n_posts_covid", "pos_b_covid", "neutral_b_covid", "neg_b_covid",
  "n_posts_vax", "pos_b_vax", "neutral_b_vax", "neg_b_vax"
)
output_names <- c(
  "total_shares", "total_comments", "total_reactions",
  "ver", "non_ver", "true", "fake", "n_posts", "eng",
  "n_posts_covid", "pos_b_covid", "neutral_b_covid", "neg_b_covid",
  "n_posts_vax", "pos_b_vax", "neutral_b_vax", "neg_b_vax"
)
batch_specs <- list(
  b1 = "b1",
  b2 = "b2",
  both = NULL
)

axis_bounds <- function(lower, upper, pad_fraction = 0.08, min_pad = 0.02) {
  lo <- min(lower, na.rm = TRUE)
  hi <- max(upper, na.rm = TRUE)
  span <- hi - lo

  if (!is.finite(span) || span <= 0) {
    span <- 0
  }

  pad <- max(span * pad_fraction, min_pad)
  c(lo - pad, hi + pad)
}

diagnosed_contexts <- new.env(parent = emptyenv())

print_collinearity_diagnostics <- function(data, fit = NULL, outcome_var, context_label = "", error_message = NULL) {
  context_key <- if (nzchar(context_label)) context_label else outcome_var

  if (exists(context_key, envir = diagnosed_contexts, inherits = FALSE)) {
    return(invisible(NULL))
  }

  assign(context_key, TRUE, envir = diagnosed_contexts)

  message("")
  message(
    "---- Collinearity diagnostics: ",
    outcome_var,
    if (nzchar(context_label)) paste0(" (", context_label, ")") else ""
  )

  if (!is.null(error_message) && nzchar(error_message)) {
    message("fixest error: ", error_message)
  }

  if (!is.null(fit) && !is.null(fit$collin.var) && length(fit$collin.var) > 0) {
    message("Dropped by fixest: ", paste(fit$collin.var, collapse = ", "))
  }

  message("Rows in estimation sample: ", nrow(data))

  if ("batch_id" %in% names(data)) {
    message("batch_id values: ", paste(sort(unique(data$batch_id)), collapse = ", "))
  }

  if ("pais" %in% names(data)) {
    message("pais values: ", paste(sort(unique(data$pais)), collapse = ", "))
  }

  tt_values <- sort(unique(data$total_treated[!is.na(data$total_treated)]))
  message(
    "total_treated unique values: ",
    if (length(tt_values) > 0) paste(tt_values, collapse = ", ") else "<none>"
  )

  print(table(data$total_treated, useNA = "ifany"))

  if ("pais" %in% names(data)) {
    print(with(data, table(pais, total_treated, useNA = "ifany")))
  }

  if ("dummy_second" %in% names(data)) {
    print(with(data, table(dummy_second, total_treated, useNA = "ifany")))
  }

  if ("dummy_third" %in% names(data)) {
    print(with(data, table(dummy_third, total_treated, useNA = "ifany")))
  }

  if (all(c("pais", "total_treated") %in% names(data))) {
    aux_fit <- lm(total_treated ~ factor(pais), data = data)
    aux_sum <- summary(aux_fit)

    message(
      sprintf(
        "Auxiliary fit: total_treated ~ factor(pais), R2 = %.6f, residual SD = %.6f",
        aux_sum$r.squared,
        aux_sum$sigma
      )
    )

    print(coef(aux_fit))
  }

  message("---- End diagnostics")
  message("")

  invisible(NULL)
}

extract_total_treated <- function(data, outcome_vars, context_label = "", allow_missing = FALSE) {
  estimates <- map_dbl(outcome_vars, function(outcome_var) {
    fmla <- as.formula(
      paste0(
        outcome_var, "_base ~ total_treated | ",
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

      print_collinearity_diagnostics(
        data = data,
        outcome_var = outcome_var,
        context_label = context_label,
        error_message = conditionMessage(fit)
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

      print_collinearity_diagnostics(
        data = data,
        fit = fit,
        outcome_var = outcome_var,
        context_label = context_label
      )

      if (allow_missing) {
        warning(detail, call. = FALSE)
        return(NA_real_)
      }

      stop(detail, call. = FALSE)
    }

    estimate
  })
  
  stats::setNames(as.list(estimates), output_names) |>
    as_tibble()
}

prepare_batch_data <- function(data, batch_filter = NULL) {
  if (is.null(batch_filter)) {
    return(data)
  }
  
  data |>
    filter(batch_id == batch_filter)
}

dir.create(results_root, showWarnings = FALSE, recursive = TRUE)
dir.create(original_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(permutations_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

write_outputs <- function(type, batch_name, original_coefs, permutation_coefs) {
  file_code <- paste0(file_stub, "_", batch_name, "_1000perm")
  original_path <- file.path(original_dir, paste0(type, file_code, ".xlsx"))
  permutations_path <- file.path(permutations_dir, paste0(type, file_code, ".xlsx"))
  plot_path <- file.path(plots_dir, paste0(type, file_code, ".pdf"))
  
  write_xlsx(
    original_coefs,
    original_path
  )
  
  write_xlsx(
    permutation_coefs,
    permutations_path
  )
  
  addon <- if (type == "log_") {
    "log "
  } else if (type == "arc_") {
    "arcsinh "
  } else {
    ""
  }
  
  final <- original_coefs |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "coef") |>
    left_join(
      permutation_coefs |>
        summarise(across(everything(), \(x) sd(x, na.rm = TRUE))) |>
        pivot_longer(cols = everything(), names_to = "var", values_to = "sd"),
      by = "var"
    ) |>
    mutate(
      Variable = case_when(
        var == "total_shares" ~ paste0(addon, "Total Shares"),
        var == "total_comments" ~ paste0(addon, "Total Comments"),
        var == "total_reactions" ~ paste0(addon, "Total Reactions"),
        var == "ver" ~ paste0(addon, "Verifiable RTs + Posts"),
        var == "non_ver" ~ paste0(addon, "Non-Verifiable RTs + Posts"),
        var == "true" ~ paste0(addon, "True RTs + Posts"),
        var == "fake" ~ paste0(addon, "Fake RTs + Posts"),
        var == "n_posts" ~ paste0(addon, "Number of RTs + Posts"),
        var == "eng" ~ paste0(addon, "Number of RTs + Posts (English)"),
        var == "n_posts_covid" ~ paste0(addon, "COVID RTs + Posts"),
        var == "pos_b_covid" ~ paste0(addon, "Positive COVID RTs + Posts"),
        var == "neutral_b_covid" ~ paste0(addon, "Neutral COVID RTs + Posts"),
        var == "neg_b_covid" ~ paste0(addon, "Negative COVID RTs + Posts"),
        var == "n_posts_vax" ~ paste0(addon, "Vaccine RTs + Posts"),
        var == "pos_b_vax" ~ paste0(addon, "Positive Vaccine RTs + Posts"),
        var == "neutral_b_vax" ~ paste0(addon, "Neutral Vaccine RTs + Posts"),
        var == "neg_b_vax" ~ paste0(addon, "Negative Vaccine RTs + Posts"),
        TRUE ~ var
      )
    )
  
  final$Variable <- factor(
    final$Variable,
    levels = c(
      paste0(addon, "Total Shares"),
      paste0(addon, "Total Comments"),
      paste0(addon, "Total Reactions"),
      paste0(addon, "Fake RTs + Posts"),
      paste0(addon, "True RTs + Posts"),
      paste0(addon, "Verifiable RTs + Posts"),
      paste0(addon, "Non-Verifiable RTs + Posts"),
      paste0(addon, "Number of RTs + Posts (English)"),
      paste0(addon, "Number of RTs + Posts"),
      paste0(addon, "COVID RTs + Posts"),
      paste0(addon, "Positive COVID RTs + Posts"),
      paste0(addon, "Neutral COVID RTs + Posts"),
      paste0(addon, "Negative COVID RTs + Posts"),
      paste0(addon, "Vaccine RTs + Posts"),
      paste0(addon, "Positive Vaccine RTs + Posts"),
      paste0(addon, "Neutral Vaccine RTs + Posts"),
      paste0(addon, "Negative Vaccine RTs + Posts")
    )
  )

  x_bounds <- axis_bounds(
    final$coef - 1.96 * final$sd,
    final$coef + 1.96 * final$sd
  )
  
  results_plot <- ggplot(data = final, aes(y = Variable, x = coef)) +
    geom_point() +
    geom_linerange(aes(xmin = coef - 1.96 * sd, xmax = coef + 1.96 * sd), linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    coord_cartesian(xlim = x_bounds) +
    theme_bw() +
    xlab("Total Treated Estimate with 95% Confidence Interval") +
    ylab("Variable") +
    theme(
      panel.grid.major = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
      panel.grid.minor = element_blank()
    )

  ggsave(
    plot = results_plot,
    filename = plot_path,
    device = cairo_pdf,
    width = 8.22,
    height = 6.59,
    units = "in"
  )
}

# 3.0 Load and prepare analysis data
belp90 <- read_parquet("../../../data/04-analysis/joint/below_p90_p95_divider.parquet") |>
  select(-n_posts_base)

baseline_keys <- bind_rows(
  read_parquet("../../../data/04-analysis/KE/baseline/baseline_divided_b1b2.parquet") |>
    mutate(pais = "KE"),
  read_parquet("../../../data/04-analysis/SA/baseline/baseline_divided_b1b2.parquet") |>
    mutate(pais = "SA")
) |>
  select(follower_id, username, pais, batch_id) |>
  distinct()

attrition <- read_parquet("../../../data/04-analysis/attrition.parquet") |>
  mutate(batch_id = "b2")

attrition_flags <- baseline_keys |>
  left_join(attrition, by = c("username", "pais", "batch_id")) |>
  transmute(
    follower_id,
    pais,
    batch_id,
    dummy_second = ifelse(!is.na(dummy_attrition) & pais == "SA" & dummy_attrition == 1, 1, 0),
    dummy_third = ifelse(!is.na(dummy_attrition) & dummy_attrition == 1, 1, 0)
  ) |>
  distinct()

df <- get_analysis_ver_final_winsor(
  stage = stage,
  batches = "b1b2",
  initial_path = "../../../"
) |>
  left_join(
    get_analysis_english_winsor(
      stage = stage,
      batches = "b1b2",
      initial_path = "../../../"
    ) |>
      select(follower_id, pais, batch_id, eng_base),
    by = c("follower_id", "pais", "batch_id")
  ) |>
  left_join(
    get_analysis_sent_bert_final2(
      stage = stage,
      batches = "b1b2",
      initial_path = "../../../"
    ) |>
      select(
        follower_id, pais, batch_id,
        any_of(c(
          "n_posts_covid_base", "pos_b_covid_base", "neutral_b_covid_base", "neg_b_covid_base",
          "n_posts_vax_base", "pos_b_vax_base", "neutral_b_vax_base", "neg_b_vax_base"
        ))
      ),
    by = c("follower_id", "pais", "batch_id")
  ) |>
  left_join(belp90, by = c("follower_id", "batch_id", "pais")) |>
  left_join(attrition_flags, by = c("follower_id", "pais", "batch_id")) |>
  mutate(
    dummy_second = coalesce(dummy_second, 0),
    dummy_third = coalesce(dummy_third, 0),
    across(
      any_of(c(
        "total_shares_base", "total_comments_base", "total_reactions_base",
        "verifiability_base", "non_ver_base", "true_base", "fake_base", "n_posts_base", "eng_base",
        "n_posts_covid_base", "pos_b_covid_base", "neutral_b_covid_base", "neg_b_covid_base",
        "n_posts_vax_base", "pos_b_vax_base", "neutral_b_vax_base", "neg_b_vax_base"
      )),
      \(x) log(x + 1),
      .names = "log_{.col}"
    )
  ) |>
  filter(below_p90 == 1) |>
  filter(total_influencers == 1) |>
  filter(n_posts_base > n_posts_thr)

df <- df |> filter(c_t_strong_total > 0)

# 4.0 Run the specification for each batch sample
for (type in list_types) {
  outcome_vars <- paste0(type, outcome_roots)
  
  base_columns <- c(
    "follower_id",
    "pais",
    "batch_id",
    "total_treated",
    "dummy_second",
    "dummy_third",
    paste0(outcome_vars, "_base")
  )
  
  analysis_df <- df |>
    select(all_of(base_columns))
  
  for (batch_name in names(batch_specs)) {
    message("Running sample: ", batch_name)
    
    batch_filter <- batch_specs[[batch_name]]
    batch_df <- prepare_batch_data(analysis_df, batch_filter)
    
    original_coefs <- extract_total_treated(
      data = batch_df,
      outcome_vars = outcome_vars
    )
    
    permutation_coefs <- map_dfr(seq_len(n_permutations), function(i) {
      message("Permutation ", i, " / ", n_permutations, " for ", batch_name)
      
      perm_treated_col <- paste0("n_influencers_followed_treatment_p", i)
      
      permuted_treatment <- read_parquet(
        paste0(
          "../../../data/04-analysis/joint/small_ties_b1b2/small_tie",
          i,
          ".parquet"
        )
      ) |>
        select(follower_id, pais, batch_id, all_of(perm_treated_col))
      
      permuted_batch_df <- batch_df |>
        left_join(permuted_treatment, by = c("follower_id", "pais", "batch_id"))
      
      permuted_batch_df$total_treated <- permuted_batch_df[[perm_treated_col]]
      permuted_batch_df[[perm_treated_col]] <- NULL
      
      extract_total_treated(
        data = permuted_batch_df,
        outcome_vars = outcome_vars,
        context_label = paste(batch_name, "permutation", i),
        allow_missing = TRUE
      )
    })
    
    write_outputs(
      type = type,
      batch_name = batch_name,
      original_coefs = original_coefs,
      permutation_coefs = permutation_coefs
    )
  }
}
