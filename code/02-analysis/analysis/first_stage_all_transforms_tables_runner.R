library(readxl)
library(dplyr)
library(purrr)

src_path <- "../../../src/utils/"
source_files <- c(
  "funcs_analysis.R",
  "constants_final.R",
  "import_data.R"
)

map(paste0(src_path, source_files), source)
ipak(packages)

if (!exists("results_subdir") || !nzchar(results_subdir)) {
  stop("results_subdir must be set before sourcing this runner.", call. = FALSE)
}

if (!exists("sample_kind") || !sample_kind %in% c("intensive", "extensive")) {
  stop("sample_kind must be set to 'intensive' or 'extensive' before sourcing this runner.", call. = FALSE)
}

if (!exists("strong_only") || !is.logical(strong_only) || length(strong_only) != 1) {
  stop("strong_only must be set to TRUE or FALSE before sourcing this runner.", call. = FALSE)
}

if (!exists("file_stub") || !nzchar(file_stub)) {
  stop("file_stub must be set before sourcing this runner.", call. = FALSE)
}

if (!exists("table_stub") || !nzchar(table_stub)) {
  stop("table_stub must be set before sourcing this runner.", call. = FALSE)
}

if (!exists("table_title") || !nzchar(table_title)) {
  stop("table_title must be set before sourcing this runner.", call. = FALSE)
}

results_root <- file.path("../../../results", results_subdir)
estimates_dir <- file.path(results_root, "estimates")
tables_dir <- file.path(results_root, "tables")

stage <- "stage1_2"
influencer_thr <- 9
n_posts_thr <- 0
initial_path <- "../../../"

dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

batch_map <- c(
  b1 = "Batch 1",
  b2 = "Batch 2",
  both = "Both Batches"
)

transform_order <- c("IHS (arcsinh)", "log(y+1)", "Dummy (y>0)", "Chen-Roth")
outcome_order <- c("Ads Retweets", "SMI Retweets")

read_excel_short_path <- function(path) {
  short_copy <- tempfile(pattern = "xlsx_", tmpdir = tempdir(), fileext = ".xlsx")
  on.exit(unlink(short_copy), add = TRUE)
  file.copy(path, short_copy, overwrite = TRUE)
  readxl::read_excel(short_copy)
}

safe_min_pos <- function(x) {
  min_pos <- suppressWarnings(min(x[x > 0], na.rm = TRUE))

  if (!is.finite(min_pos)) {
    return(1)
  }

  min_pos
}

batch_from_name <- function(filename) {
  if (grepl("_b1_", filename, fixed = TRUE)) {
    return("b1")
  }

  if (grepl("_b2_", filename, fixed = TRUE)) {
    return("b2")
  }

  if (grepl("_both_", filename, fixed = TRUE)) {
    return("both")
  }

  NA_character_
}

prepare_batch_data <- function(data, batch_filter = NULL) {
  if (is.null(batch_filter)) {
    return(data)
  }

  data |>
    filter(batch_id == batch_filter)
}

build_control_mean_table <- function(batch_code) {
  batch_filter <- if (batch_code == "both") NULL else batch_code

  belp90 <- read_parquet("../../../data/04-analysis/joint/below_p90_p95_divider.parquet") |>
    select(follower_id:percentile) |>
    select(-n_posts_base) |>
    distinct(follower_id, batch_id, pais, .keep_all = TRUE)

  df_s <- read_parquet("../../../data/others/RTs_counts_smi.parquet") |>
    select(follower_id = id, RTs_smi_treatment) |>
    distinct(follower_id, .keep_all = TRUE)

  df_a <- read_parquet("../../../data/others/RTs_counts_ads.parquet") |>
    select(follower_id = id, RTs_ads_treatment) |>
    distinct(follower_id, .keep_all = TRUE)

  base_df <- get_analysis_ver_final_winsor(
    stage = stage,
    batches = "b1b2",
    initial_path = initial_path
  ) |>
    filter(n_posts_base > n_posts_thr) |>
    left_join(df_s, by = "follower_id", relationship = "many-to-one") |>
    left_join(df_a, by = "follower_id", relationship = "many-to-one") |>
    mutate(
      ads = ifelse(is.na(RTs_ads_treatment), 0, RTs_ads_treatment),
      smi = ifelse(is.na(RTs_smi_treatment), 0, RTs_smi_treatment),
      dummy_ads = ifelse(ads > 0, 1, 0),
      dummy_smi = ifelse(smi > 0, 1, 0),
      total_treated = t_strong + t_weak + t_neither,
      total_influencers = c_t_strong_total + c_t_weak_total + c_t_neither_total
    ) |>
    left_join(
      belp90,
      by = c("follower_id", "batch_id", "pais"),
      relationship = "many-to-one"
    ) |>
    filter(below_p90 == 1)

  if (sample_kind == "intensive") {
    base_df <- base_df |>
      filter(total_influencers < influencer_thr)
  } else {
    base_df <- base_df |>
      filter(total_influencers == 1)
  }

  if (strong_only) {
    base_df <- base_df |>
      filter(c_t_strong_total > 0)
  }

  base_df <- prepare_batch_data(base_df, batch_filter)

  min_pos_ads <- safe_min_pos(base_df$ads)
  min_pos_smi <- safe_min_pos(base_df$smi)

  means_df <- base_df |>
    mutate(
      ihs_ads = asinh(ads),
      ihs_smi = asinh(smi),
      log1p_ads = log(ads + 1),
      log1p_smi = log(smi + 1),
      cr_ads = ifelse(ads == 0, -min_pos_ads, log(ads)),
      cr_smi = ifelse(smi == 0, -min_pos_smi, log(smi))
    ) |>
    filter(ads_treatment == 0, total_treated == 0) |>
    summarise(
      across(
        c(ihs_ads, ihs_smi, log1p_ads, log1p_smi, dummy_ads, dummy_smi, cr_ads, cr_smi),
        \(x) mean(x, na.rm = TRUE)
      )
    ) |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "control_mean") |>
    mutate(
      transform = case_when(
        var %in% c("ihs_ads", "ihs_smi") ~ "IHS (arcsinh)",
        var %in% c("log1p_ads", "log1p_smi") ~ "log(y+1)",
        var %in% c("dummy_ads", "dummy_smi") ~ "Dummy (y>0)",
        var %in% c("cr_ads", "cr_smi") ~ "Chen-Roth"
      ),
      outcome = case_when(
        grepl("_ads$", var) ~ "Ads Retweets",
        grepl("_smi$", var) ~ "SMI Retweets"
      )
    ) |>
    select(transform, outcome, control_mean)

  means_df
}

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x) || is.nan(x)) {
    return("")
  }

  formatC(x, digits = digits, format = "f")
}

fmt_coef <- function(coef, sd, digits = 3) {
  if (length(coef) == 0 || is.na(coef) || is.nan(coef)) {
    return("")
  }

  if (length(sd) == 0 || is.na(sd) || is.nan(sd) || sd <= 0) {
    return(fmt_num(coef, digits))
  }

  p_value <- 2 * (1 - pnorm(abs(coef / sd)))
  stars <- if (p_value < 0.01) {
    "***"
  } else if (p_value < 0.05) {
    "**"
  } else if (p_value < 0.1) {
    "*"
  } else {
    ""
  }

  paste0(fmt_num(coef, digits), stars)
}

build_batch_table <- function(df, batch_label) {
  ordered <- expand.grid(
    transform = transform_order,
    outcome = outcome_order,
    stringsAsFactors = FALSE
  ) |>
    as_tibble() |>
    left_join(df, by = c("transform", "outcome")) |>
    mutate(
      transform = factor(transform, levels = transform_order),
      outcome = factor(outcome, levels = outcome_order)
    ) |>
    arrange(transform, outcome)

  coef_cells <- apply(
    matrix(
      mapply(fmt_coef, ordered$coef, ordered$sd, SIMPLIFY = TRUE),
      nrow = 2,
      byrow = FALSE
    ),
    2,
    paste,
    collapse = " & "
  )

  se_cells <- apply(
    matrix(
      vapply(ordered$sd, function(x) {
        if (length(x) == 0 || is.na(x) || is.nan(x)) {
          return("")
        }

        paste0("(", fmt_num(x, 3), ")")
      }, character(1)),
      nrow = 2,
      byrow = FALSE
    ),
    2,
    paste,
    collapse = " & "
  )

  mean_cells <- apply(
    matrix(
      vapply(ordered$control_mean, fmt_num, character(1), digits = 4),
      nrow = 2,
      byrow = FALSE
    ),
    2,
    paste,
    collapse = " & "
  )

  paste0(
    "\\begin{table}[!htbp]\n",
    "\\centering\n",
    "\\caption{", table_title, " (", batch_label, ")}\n",
    "\\begin{tabular}{lcccccccc}\n",
    "\\hline\n",
    " & \\multicolumn{2}{c}{IHS (arcsinh)} & \\multicolumn{2}{c}{log(y+1)} & \\multicolumn{2}{c}{Dummy (y>0)} & \\multicolumn{2}{c}{Chen-Roth} \\\\\n",
    " & Ads & SMI & Ads & SMI & Ads & SMI & Ads & SMI \\\\\n",
    "\\hline\n",
    "Treatment effect & ", paste(coef_cells, collapse = " & "), " \\\\\n",
    " & ", paste(se_cells, collapse = " & "), " \\\\\n",
    "Control-group mean & ", paste(mean_cells, collapse = " & "), " \\\\\n",
    "\\hline\n",
    "\\multicolumn{9}{l}{\\parbox[t]{15.5cm}{\\textit{Notes:} Standard errors are permutation-based. Significance stars: * p<0.10, ** p<0.05, *** p<0.01. Columns report paid-for-ad retweets and SMI retweets under the existing first-stage specification.}} \\\\\n",
    "\\end{tabular}\n",
    "\\end{table}\n"
  )
}

estimate_files <- list.files(estimates_dir, pattern = "_estimates\\.xlsx$", full.names = TRUE)

for (estimate_path in estimate_files) {
  filename <- basename(estimate_path)
  batch_code <- batch_from_name(filename)

  if (is.na(batch_code)) {
    next
  }

  final <- read_excel_short_path(estimate_path) |>
    mutate(
      transform = factor(transform, levels = transform_order),
      outcome = factor(outcome, levels = outcome_order)
    ) |>
    select(-any_of("control_mean")) |>
    left_join(build_control_mean_table(batch_code), by = c("transform", "outcome")) |>
    arrange(transform, outcome)

  table_tex <- build_batch_table(final, batch_map[[batch_code]])

  out_path <- file.path(
    tables_dir,
    paste0(table_stub, "_", batch_code, ".tex")
  )

  cat(table_tex, file = out_path)
  message("Saved: ", out_path)
}

message("Done: ", table_stub)
