# Shared helpers for supplementary ads and followers analyses.

repo_initial_path <- local({
  cached_path <- NULL

  function() {
    if (!is.null(cached_path)) {
      return(cached_path)
    }

    candidate_paths <- c("../../", "../../../../", "../../../", "../", "")

    for (candidate in candidate_paths) {
      has_utils <- dir.exists(file.path(candidate, "src", "utils"))
      has_analysis_data <- dir.exists(file.path(candidate, "data", "04-analysis"))

      if (has_utils && has_analysis_data) {
        cached_path <<- candidate
        return(cached_path)
      }
    }

    stop("Could not locate the repository root from this script directory.", call. = FALSE)
  }
})

results_path <- function(...) {
  file.path(repo_initial_path(), "results", ...)
}

read_permutation_counts <- function(i, cols, initial_path = repo_initial_path()) {
  permutation_dirs <- c("small_ties_b1b2", "small_ties_b1b2p")
  selected_dir <- NULL

  for (dir_name in permutation_dirs) {
    dir_path <- file.path(initial_path, "data", "04-analysis", "joint", dir_name)
    if (dir.exists(dir_path)) {
      selected_dir <- dir_path
      break
    }
  }

  if (is.null(selected_dir)) {
    stop("Could not find a permutation directory under data/04-analysis/joint.", call. = FALSE)
  }

  read_parquet(file.path(selected_dir, paste0("small_tie", i, ".parquet"))) |>
    select(follower_id, pais, batch_id, all_of(cols))
}

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

join_keys_present <- function(x, y, preferred = c("follower_id", "pais", "batch_id")) {
  join_cols <- intersect(preferred, names(x))
  join_cols <- intersect(join_cols, names(y))

  if (!"follower_id" %in% join_cols || !"pais" %in% join_cols) {
    stop(
      "Expected follower_id and pais to be present in both objects before joining.",
      call. = FALSE
    )
  }

  join_cols
}

prepare_batch_data <- function(data, batch_filter = NULL) {
  if (is.null(batch_filter)) {
    return(data)
  }

  data |>
    filter(batch_id == batch_filter)
}

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

compose_rhs <- function(...) {
  terms <- c(...)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  paste(terms, collapse = " + ")
}

write_horizontal_plot <- function(
  final,
  plot_path,
  xlab_text = "Estimate with 95% Confidence Interval",
  width = 8.5,
  height = 6.75
) {
  x_bounds <- axis_bounds(final$lower, final$upper)

  results_plot <- ggplot(final, aes(y = Variable, x = coef)) +
    geom_point() +
    geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    coord_cartesian(xlim = x_bounds) +
    theme_bw() +
    xlab(xlab_text) +
    ylab("Variable") +
    theme(
      panel.grid.major = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
      panel.grid.minor = element_blank()
    )

  ggsave(
    plot = results_plot,
    filename = plot_path,
    device = cairo_pdf,
    width = width,
    height = height,
    units = "in"
  )
}

write_stage_plot <- function(
  final,
  plot_path,
  ylab_text = "Estimate with 95% Confidence Interval",
  width = 8.5,
  height = 6.75
) {
  y_bounds <- axis_bounds(final$lower, final$upper)
  n_vars <- nlevels(final$Variable)
  shape_palette <- c(15, 16, 17, 18, 3, 7, 8, 0, 1, 2, 4, 5, 6, 9, 10, 11, 12, 13, 14)

  if (n_vars > length(shape_palette)) {
    stop(
      paste0(
        "write_stage_plot only has ", length(shape_palette),
        " distinct shapes configured, but received ", n_vars, " outcomes."
      ),
      call. = FALSE
    )
  }

  results_plot <- ggplot(final, aes(x = Stage, y = coef)) +
    geom_point(
      aes(shape = Variable, color = Variable),
      size = 3,
      position = position_dodge(width = 0.5)
    ) +
    geom_linerange(
      aes(ymin = lower, ymax = upper, color = Variable),
      position = position_dodge(width = 0.5),
      linewidth = 1
    ) +
    scale_shape_manual(values = shape_palette[seq_len(n_vars)], name = "Outcome") +
    scale_color_manual(values = rep("black", n_vars), name = "Outcome") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    coord_cartesian(ylim = y_bounds) +
    theme_bw() +
    ylab(ylab_text) +
    xlab("Stage") +
    theme(
      panel.grid.major = element_line(color = "gray", linetype = "dashed", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(
    plot = results_plot,
    filename = plot_path,
    device = cairo_pdf,
    width = width,
    height = height,
    units = "in"
  )
}

load_belp90 <- function(initial_path = repo_initial_path()) {
  read_parquet(paste0(initial_path, "data/04-analysis/joint/below_p90_p95_divider.parquet")) |>
    select(-n_posts_base)
}

load_followers_outcomes <- function(initial_path = repo_initial_path()) {
  ac_ke <- read_parquet(paste0(initial_path, "data/04-analysis/KE/AC/AC_final.parquet")) |>
    mutate(pais = "KE")
  ac_sa <- read_parquet(paste0(initial_path, "data/04-analysis/SA/AC/AC_final.parquet")) |>
    mutate(pais = "SA")

  followers <- bind_rows(ac_sa, ac_ke) |>
    select(
      any_of(c(
        "follower_id", "username", "pais", "batch_id", "ads_treatment",
        "t_strong", "t_weak", "t_neither",
        "c_t_strong_total", "c_t_weak_total", "c_t_neither_total",
        "AC", "AC_base"
      ))
    )

  smi_ke <- read_parquet(paste0(initial_path, "data/04-analysis/KE/SMIs/SMIs_final.parquet")) |>
    mutate(pais = "KE")
  smi_sa <- read_parquet(paste0(initial_path, "data/04-analysis/SA/SMIs/SMIs_final.parquet")) |>
    mutate(pais = "SA")

  smis <- bind_rows(smi_sa, smi_ke) |>
    select(any_of(c("follower_id", "pais", "batch_id", "SMIs")))

  by_cols <- join_keys_present(followers, smis)

  followers |>
    left_join(smis, by = by_cols)
}

load_stage_metadata <- function(stage, initial_path = repo_initial_path()) {
  belp90 <- load_belp90(initial_path)

  get_analysis_ver_final_winsor(
    stage = stage,
    batches = "b1b2",
    initial_path = initial_path
  ) |>
    filter(batch_id == "b1") |>
    left_join(belp90, by = c("follower_id", "batch_id", "pais")) |>
    mutate(
      strat_block1 = paste0(strat_block1, batch_id, pais),
      strat_block2 = paste0(strat_block2, batch_id, pais)
    ) |>
    select(
      follower_id,
      pais,
      batch_id,
      n_posts_base,
      below_p90,
      total_treated,
      total_influencers,
      strat_block1,
      strat_block2
    )
}

load_followers_base_data <- function(initial_path = repo_initial_path()) {
  followers <- load_followers_outcomes(initial_path)
  stage_meta <- load_stage_metadata("stage1_2", initial_path)
  join_cols <- join_keys_present(followers, stage_meta)

  followers |>
    left_join(stage_meta, by = join_cols) |>
    mutate(
      total_treated = coalesce(total_treated, t_strong + t_weak + t_neither),
      total_influencers = coalesce(
        total_influencers,
        c_t_strong_total + c_t_weak_total + c_t_neither_total
      )
    )
}

load_followers_stage_data <- function(stage, initial_path = repo_initial_path()) {
  followers <- load_followers_outcomes(initial_path) |>
    select(-any_of(c("total_treated", "total_influencers", "strat_block1", "strat_block2", "below_p90", "n_posts_base")))
  stage_meta <- load_stage_metadata(stage, initial_path)
  join_cols <- join_keys_present(followers, stage_meta)

  followers |>
    inner_join(stage_meta, by = join_cols)
}

load_followers_aggregate_data <- function(initial_path = repo_initial_path()) {
  stage_names <- c("stage1_2", "stage3_4", "stage5_6")
  stage_meta <- map(stage_names, load_stage_metadata, initial_path = initial_path)

  # Followers outcomes are only observed at endline. For the aggregate sample, we
  # keep users observed in all stages and carry the stage1_2 treatment metadata,
  # which is the same assignment object used in the stage-specific follower files.
  common_ids <- reduce(
    map(stage_meta, ~select(.x, follower_id, pais, batch_id)),
    inner_join,
    by = c("follower_id", "pais", "batch_id")
  )

  followers <- load_followers_outcomes(initial_path) |>
    inner_join(common_ids, by = join_keys_present(load_followers_outcomes(initial_path), common_ids))

  stage_meta[[1]] |>
    semi_join(common_ids, by = c("follower_id", "pais", "batch_id")) |>
    left_join(followers, by = join_keys_present(stage_meta[[1]], followers))
}

join_intensive_fes <- function(data, initial_path = repo_initial_path()) {
  fes <- read_parquet(paste0(initial_path, "data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet"))
  join_cols <- intersect(c("follower_id", "pais", "batch_id"), names(fes))

  data |>
    left_join(fes, by = join_cols)
}

build_intensive_interactions <- function(data) {
  interactions <- generate_interactions(
    data |>
      select(follower_id, pais, batch_id, total_treated, total_influencers)
  )

  int_cols <- grep("^tao_", names(interactions), value = TRUE)

  list(
    data = data |>
      left_join(interactions, by = c("follower_id", "pais", "batch_id")),
    int_cols = int_cols
  )
}

followers_label_map <- c(
  AC = "Follows Africa Check",
  SMIs = "Number of SMIs Followed"
)

followers_order <- c("Number of SMIs Followed", "Follows Africa Check")

standard_stage_labels <- c(
  stage1_2 = "Weeks 1-4",
  stage3_4 = "Weeks 5-8",
  stage5_6 = "Weeks 9-12"
)

standard_outcome_label_map <- c(
  total_shares = "Total Shares",
  total_comments = "Total Comments",
  total_reactions = "Total Reactions",
  ver = "Verifiable Posts + Shares",
  verifiability = "Verifiable Posts + Shares",
  non_ver = "Non Verifiable Posts + Shares",
  true = "True Posts + Shares",
  fake = "Fake Posts + Shares",
  n_posts = "Number of Posts + Shares",
  eng = "Number of Posts + Shares (English)",
  n_posts_covid = "COVID Posts + Shares",
  pos_b_covid = "Positive COVID Posts + Shares",
  neutral_b_covid = "Neutral COVID Posts + Shares",
  neg_b_covid = "Negative COVID Posts + Shares",
  n_posts_vax = "Vaccine Posts + Shares",
  pos_b_vax = "Positive Vaccine Posts + Shares",
  neutral_b_vax = "Neutral Vaccine Posts + Shares",
  neg_b_vax = "Negative Vaccine Posts + Shares"
)

standard_outcome_order <- c(
  "Total Shares",
  "Total Comments",
  "Total Reactions",
  "Number of Posts + Shares",
  "Number of Posts + Shares (English)",
  "Non Verifiable Posts + Shares",
  "Verifiable Posts + Shares",
  "True Posts + Shares",
  "Fake Posts + Shares",
  "COVID Posts + Shares",
  "Positive COVID Posts + Shares",
  "Neutral COVID Posts + Shares",
  "Negative COVID Posts + Shares",
  "Vaccine Posts + Shares",
  "Positive Vaccine Posts + Shares",
  "Neutral Vaccine Posts + Shares",
  "Negative Vaccine Posts + Shares"
)
