# 0.0 Set up the environment and locate this script
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import shared functions and packages
library(purrr)

src_path <- "../../../src/utils/"
source_files <- c(
  "funcs_analysis.R",
  "constants_final.R",
  "import_data.R"
)

map(paste0(src_path, source_files), source)
ipak(packages)

# 2.0 Define paths, samples, and the variables we want to aggregate
initial_path <- "../../../"
analysis_root <- file.path(initial_path, "data", "04-analysis")

list_stages <- c("stage1_2", "stage3_4", "stage5_6")
n_posts_thr <- 0
influencer_thr <- 9

id_cols <- c("follower_id", "pais", "batch_id")

extensive_endline_vars <- c(
  "total_shares",
  "total_reactions",
  "total_comments",
  "verifiability",
  "non_ver",
  "true",
  "fake",
  "n_posts"
)

english_endline_vars <- c("eng")

sentiment_endline_vars <- c(
  "pos_b_covid", "neutral_b_covid", "neg_b_covid", "n_posts_covid",
  "pos_b_vax", "neutral_b_vax", "neg_b_vax", "n_posts_vax"
)

baseline_ver_vars <- c(
  "total_shares_base",
  "total_comments_base",
  "total_likes_base",
  "total_reactions_base",
  "verifiability_base",
  "non_ver_base",
  "true_base",
  "fake_base",
  "n_posts_base",
  "verifiability_rt_base",
  "non_ver_rt_base",
  "true_rt_base",
  "fake_rt_base",
  "n_posts_rt_base",
  "verifiability_no_rt_base",
  "non_ver_no_rt_base",
  "true_no_rt_base",
  "fake_no_rt_base",
  "n_posts_no_rt_base"
)

baseline_sentiment_vars <- c(
  "pos_b_covid_base", "neutral_b_covid_base", "neg_b_covid_base", "n_posts_covid_base",
  "pos_b_vax_base", "neutral_b_vax_base", "neg_b_vax_base", "n_posts_vax_base"
)

baseline_english_vars <- c("eng_base")

treatment_info_vars <- c(
  "username",
  "t_strong", "t_weak", "t_neither",
  "c_t_strong_total", "c_t_weak_total", "c_t_neither_total",
  "total_treated", "total_influencers",
  "below_p90"
)

optional_info_vars <- c("strat_block1", "strat_block2")

# 3.0 Load shared filters once
belp90 <- read_parquet(file.path(analysis_root, "joint", "below_p90_p95_divider.parquet")) |>
  select(-n_posts_base)

# 4.0 Helpers
ensure_columns <- function(data, required_cols, object_label) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      paste(
        object_label,
        "is missing required columns:",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data
}

load_stage_verifiability <- function(stage) {
  get_analysis_ver_final_winsor(
    stage = stage,
    batches = "b1b2",
    initial_path = initial_path
  ) |>
    left_join(belp90, by = c("follower_id", "batch_id", "pais")) |>
    filter(below_p90 == 1) |>
    filter(n_posts_base > n_posts_thr) |>
    filter(total_influencers < influencer_thr)
}

load_stage_english <- function(stage) {
  get_analysis_english_winsor(
    stage = stage,
    batches = "b1b2",
    initial_path = initial_path
  )
}

load_stage_sentiment <- function(stage) {
  get_analysis_sent_bert_final2(
    stage = stage,
    batches = "b1b2",
    initial_path = initial_path
  )
}

build_treatment_info <- function(stage1_df, stage1_english, stage1_sentiment) {
  info_vars <- c(
    treatment_info_vars[treatment_info_vars %in% names(stage1_df)],
    optional_info_vars[optional_info_vars %in% names(stage1_df)],
    baseline_ver_vars[baseline_ver_vars %in% names(stage1_df)]
  )

  stage1_english <- ensure_columns(
    stage1_english,
    c(id_cols, baseline_english_vars),
    "Stage 1-2 english data"
  )

  stage1_sentiment <- ensure_columns(
    stage1_sentiment,
    c(id_cols, baseline_sentiment_vars),
    "Stage 1-2 sentiment data"
  )

  stage1_df <- ensure_columns(
    stage1_df,
    c(id_cols, info_vars),
    "Stage 1-2 verifiability data"
  )

  stage1_df |>
    select(all_of(id_cols), all_of(info_vars)) |>
    left_join(
      stage1_english |>
        select(all_of(id_cols), any_of(baseline_english_vars)),
      by = id_cols
    ) |>
    left_join(
      stage1_sentiment |>
        select(all_of(id_cols), any_of(baseline_sentiment_vars)),
      by = id_cols
    ) |>
    distinct()
}

build_stage_endline <- function(stage) {
  ver_df <- load_stage_verifiability(stage)
  english_df <- load_stage_english(stage)
  sentiment_df <- load_stage_sentiment(stage)

  ver_df <- ensure_columns(
    ver_df,
    c(id_cols, extensive_endline_vars),
    paste("Verifiability data for", stage)
  )

  english_df <- ensure_columns(
    english_df,
    c(id_cols, english_endline_vars),
    paste("English data for", stage)
  )

  sentiment_df <- ensure_columns(
    sentiment_df,
    c(id_cols, sentiment_endline_vars),
    paste("Sentiment data for", stage)
  )

  ver_df |>
    select(all_of(id_cols), all_of(extensive_endline_vars)) |>
    left_join(
      english_df |>
        select(all_of(id_cols), all_of(english_endline_vars)),
      by = id_cols
    ) |>
    left_join(
      sentiment_df |>
        select(all_of(id_cols), all_of(sentiment_endline_vars)),
      by = id_cols
    ) |>
    mutate(stage = stage)
}

# 5.0 Build the baseline/treatment-information block from stage 1-2
stage1_ver_df <- load_stage_verifiability("stage1_2")
stage1_english_df <- load_stage_english("stage1_2")
stage1_sentiment_df <- load_stage_sentiment("stage1_2")

treatment_info <- build_treatment_info(
  stage1_df = stage1_ver_df,
  stage1_english = stage1_english_df,
  stage1_sentiment = stage1_sentiment_df
)

# 6.0 Build each stage's endline outcomes with a common column structure
stage_endline_data <- setNames(
  map(list_stages, build_stage_endline),
  list_stages
)

aggregated_long <- bind_rows(stage_endline_data)

# 7.0 Aggregate outcomes across the full treatment period
final <- aggregated_long |>
  select(-stage) |>
  group_by(across(all_of(id_cols))) |>
  summarise(
    across(
      everything(),
      \(x) sum(x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# 8.0 Merge baseline and treatment information back to the aggregated outcomes
aggregate_data <- treatment_info |>
  left_join(final, by = id_cols)
