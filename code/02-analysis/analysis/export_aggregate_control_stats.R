rm(list = ls())

resolve_script_dir <- function() {
  rstudio_path <- tryCatch(
    {
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        rstudioapi::getActiveDocumentContext()$path
      } else {
        ""
      }
    },
    error = function(...) ""
  )

  if (nzchar(rstudio_path)) {
    return(dirname(normalizePath(rstudio_path, winslash = "/", mustWork = TRUE)))
  }

  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg[[1]])

    if (file.exists(script_path)) {
      return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

setwd(resolve_script_dir())

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

cache_once <- function(loader) {
  loaded <- FALSE
  cached <- NULL

  function() {
    if (!loaded) {
      cached <<- loader()
      loaded <<- TRUE
    }

    cached
  }
}

format_range_value <- function(x) {
  if (is.na(x)) {
    return(NA_character_)
  }

  format(round(x), scientific = FALSE, trim = TRUE)
}

load_aggregate_data <- function(script_path, object_name = "aggregate_data") {
  normalized_script <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
  cache_key <- paste(normalized_script, object_name, sep = "::")

  if (exists(cache_key, envir = .aggregate_control_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .aggregate_control_cache, inherits = FALSE))
  }

  aggregate_env <- new.env(parent = globalenv())
  sys.source(normalized_script, envir = aggregate_env, chdir = TRUE)
  loaded_object <- aggregate_env[[object_name]]
  assign(cache_key, loaded_object, envir = .aggregate_control_cache)
  loaded_object
}

.aggregate_control_cache <- new.env(parent = emptyenv())

load_extensive_aggregate <- cache_once(function() {
  load_aggregate_data("../pre-process/aggregate_batches.R")
})

load_intensive_aggregate <- cache_once(function() {
  load_aggregate_data("../pre-process/aggregate_batches_intensive.R")
})

load_followers_aggregate_cached <- cache_once(function() {
  load_followers_aggregate_data(initial_path)
})

load_followers_base_cached <- cache_once(function() {
  load_followers_base_data(initial_path)
})

batch_specs <- list(
  b1 = "b1",
  b2 = "b2",
  both = NULL
)

sample_variants <- list(
  extensive = list(
    loader = function() load_extensive_aggregate(),
    batches = batch_specs,
    outcome_roots = standard_outcome_roots,
    label_map = standard_outcome_control_label_map,
    control_filter = function(df) df$total_treated == 0
  ),
  extensive_strong = list(
    loader = function() load_extensive_aggregate() |> filter(c_t_strong_total > 0),
    batches = batch_specs,
    outcome_roots = standard_outcome_roots,
    label_map = standard_outcome_control_label_map,
    control_filter = function(df) df$total_treated == 0
  ),
  intensive = list(
    loader = function() load_intensive_aggregate(),
    batches = batch_specs,
    outcome_roots = standard_outcome_roots,
    label_map = standard_outcome_control_label_map,
    control_filter = function(df) df$total_treated == 0
  ),
  intensive_strong = list(
    loader = function() load_intensive_aggregate() |> filter(c_t_strong_total > 0),
    batches = batch_specs,
    outcome_roots = standard_outcome_roots,
    label_map = standard_outcome_control_label_map,
    control_filter = function(df) df$total_treated == 0
  ),
  followers_extensive = list(
    loader = function() {
      load_followers_base_cached() |>
        filter(n_posts_base > 0) |>
        filter(total_influencers == 1)
    },
    batches = list(b1 = "b1"),
    outcome_roots = c("AC", "SMIs"),
    label_map = followers_label_map,
    control_filter = function(df) df$total_treated == 0
  ),
  followers_extensive_strong = list(
    loader = function() {
      load_followers_base_cached() |>
        filter(n_posts_base > 0) |>
        filter(total_influencers == 1) |>
        filter(c_t_strong_total > 0)
    },
    batches = list(b1 = "b1"),
    outcome_roots = c("AC", "SMIs"),
    label_map = followers_label_map,
    control_filter = function(df) df$total_treated == 0
  ),
  followers_intensive = list(
    loader = function() {
      load_followers_base_cached() |>
        filter(below_p90 == 1) |>
        filter(n_posts_base > 0) |>
        filter(total_influencers < 9)
    },
    batches = list(b1 = "b1"),
    outcome_roots = c("AC", "SMIs"),
    label_map = followers_label_map,
    control_filter = function(df) df$total_treated == 0
  ),
  followers_aggregate = list(
    loader = function() {
      load_followers_aggregate_cached() |>
        filter(below_p90 == 1) |>
        filter(n_posts_base > 0) |>
        filter(total_influencers < 9)
    },
    batches = list(b1 = "b1"),
    outcome_roots = c("AC", "SMIs"),
    label_map = followers_label_map,
    control_filter = function(df) df$total_treated == 0
  ),
  followers_ads = list(
    loader = function() {
      load_followers_base_cached() |>
        filter(below_p90 == 1) |>
        filter(n_posts_base > 0) |>
        filter(total_influencers < 9)
    },
    batches = list(b1 = "b1"),
    outcome_roots = c("AC", "SMIs"),
    label_map = followers_label_map,
    control_filter = function(df) df$ads_treatment == 0
  )
)

summarise_controls <- function(data, sample_name, batch_name, batch_filter, outcome_roots, label_map, control_filter) {
  analysis_df <- prepare_batch_data(data, batch_filter)
  analysis_df <- analysis_df[control_filter(analysis_df), , drop = FALSE]

  if (nrow(analysis_df) == 0) {
    return(tibble())
  }

  available_roots <- outcome_roots[outcome_roots %in% names(analysis_df)]

  map_dfr(available_roots, function(outcome_root) {
    values <- analysis_df[[outcome_root]]
    non_missing <- values[!is.na(values)]
    control_mean <- mean(values, na.rm = TRUE)
    control_sd <- stats::sd(values, na.rm = TRUE)

    if (length(non_missing) == 0) {
      range_label <- NA_character_
      outcome_min <- NA_real_
      outcome_max <- NA_real_
    } else {
      outcome_min <- min(non_missing)
      outcome_max <- max(non_missing)
      range_label <- paste0(
        "(",
        format_range_value(outcome_min),
        ", ",
        format_range_value(outcome_max),
        ")"
      )
    }

    message(
      sprintf(
        "[control stats] sample=%s batch=%s outcome=%s mean=%.6f sd=%.6f n=%d",
        sample_name,
        batch_name,
        unname(label_map[outcome_root]),
        control_mean,
        control_sd,
        length(non_missing)
      )
    )

    tibble(
      sample = sample_name,
      batch = batch_name,
      outcome_root = outcome_root,
      outcome_label = unname(label_map[outcome_root]),
      control_mean = control_mean,
      control_sd = control_sd,
      outcome_min = outcome_min,
      outcome_max = outcome_max,
      outcome_range = range_label
    )
  })
}

control_stats <- imap_dfr(sample_variants, function(spec, sample_name) {
  sample_start <- Sys.time()
  data <- spec$loader()

  sample_stats <- imap_dfr(spec$batches, function(batch_filter, batch_name) {
    summarise_controls(
      data = data,
      sample_name = sample_name,
      batch_name = batch_name,
      batch_filter = batch_filter,
      outcome_roots = spec$outcome_roots,
      label_map = spec$label_map,
      control_filter = spec$control_filter
    )
  })

  elapsed <- round(as.numeric(difftime(Sys.time(), sample_start, units = "secs")), 2)
  message(sprintf("[control stats] finished sample=%s in %s seconds", sample_name, elapsed))
  sample_stats
})

output_dir <- results_path("aggregate_reference_stats")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_xlsx(control_stats, file.path(output_dir, "aggregate_control_stats.xlsx"))
