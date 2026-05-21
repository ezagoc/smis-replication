ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

robust_se <- function(x) {
  coeftest(x, vcov = vcovHC(x, type = "HC1"))[,2]
}

matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j], na.rm = T))/sd(x[sgroup,j], na.rm = T)
  }
  return(x)
}


icwIndex <- function(
  xmat,
  wgts=rep(1, nrow(xmat)),
  revcols = NULL,
  ind_na = 1, 
  sgroup = rep(TRUE, nrow(xmat))
  ){
  X0 <- xmat
  X <- matStand(X0, sgroup)
  X[is.na(xmat)] <- 0
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  Sx <- cov.wt(X, wt=wgts)[[1]]
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  if (all(colSums(is.na(xmat)) > 0 )) { # if all columns have NA values set NA values to index in those obs
    index[is.na(xmat[,ind_na])] <- NA # Implement ind_na as an auxiliary index for a column that has 463 responses (ONLY PSM)
  }
  return(list(weights = weights, index = index))
}

# Inverse Covariance Weigthing Function
# df: dataframe with selected columns
# reverse: column indexes to change sign 
# Output: list with weights and the index variable

# ------------------------------------------------------------------------------

icwFunction <- function(df, reverse = NULL) {
  
  vars <- Rfast::colVars(as.matrix(df), na.rm = T)
  
  if (length(reverse) > 0 ) {
    df[, reverse] <- -1*df[, reverse]
  }
  
  sum_inverse <- (sum(1/vars))^-1 # sum of the inverse of variances to the inverse
  
  weights <- (1/vars)*sum_inverse # inverse vector of variances times sum of inverses to the inverse
  
  # Temporarily convert NA values
  x0 <- df
  x0[is.na(df)] <- 0
  
  index <- t(weights%*%t(x0)) # Calculate the index.  t(W*t(DF))
  
  if (all(colSums(is.na(df)) > 0 )) { # if all columns have NA values set NA values to index in those obs
    index[is.na(df[,1])] <- NA
  }
  
  return(list(weights, index)) # return weights and index
}


byFollower <- function(x) {
  x <- x %>%
    dplyr::select(
      handle,
      assignment_num,
      date_period,
      n_influencers_followed,
      n_influencers_followed_control, 
      n_influencers_followed_treatment,
      n_influencers_followed_control_no_strong,
      n_influencers_followed_treatment_no_strong,
      n_influencers_followed_control_strong, 
      n_influencers_followed_treatment_strong,
      n_influencers_followed_control_no_weak,
      n_influencers_followed_treatment_no_weak,
      n_influencers_followed_treatment_weak,   
      n_influencers_followed_control_weak,
      hapaxlegomenon_R,
      hapaxlegomenon_hapax,
      hapax_dislegomenon_h,
      hapax_dislegomenon_S, # Readability
      word_density,
      readability_index,
      simpsons_index,
      brunets_measure_w,
      szigriszt_pazos, # Readability
      type_token_ratio, 
      shannon_entropy, 
      yules_characteristic_K, # Readability
      prop_persons, 
      prop_locations, 
      prop_organizations, 
      functional_words_count, # Lexical index factual
      word_count, 
      sentence_count, 
      find_url, 
      has_url, 
      count_persons, 
      count_locations, 
      count_organizations, 
      syllable_count,  # Lexical index length
      flesch_reading, 
      fernandez_huerta, 
      reading_time, 
      crawford, 
      gutierrez_polini, # Richness index
      total_reactions, 
      total_comments, 
      total_shares, # Interaction index
      sentiment_score, 
      has_image, 
      has_text, # quality
      preds_model_beto_sm_num, 
      preds_model_numeric_sm_num, 
      preds_model_wobeto_sm_num, 
      feat_25common_unigrams, 
      feat_50common_unigrams, # share misinformation
      fact_check
      ) %>% group_by(
        handle,
        assignment_num,
        date_period,
        n_influencers_followed,
        n_influencers_followed_control, 
        n_influencers_followed_treatment,
        n_influencers_followed_control_no_strong,
        n_influencers_followed_treatment_no_strong,
        n_influencers_followed_control_strong, 
        n_influencers_followed_treatment_strong,
        n_influencers_followed_control_no_weak,
        n_influencers_followed_treatment_no_weak,
        n_influencers_followed_treatment_weak,   
        n_influencers_followed_control_weak ) %>% 
    summarise_all(funs(mean, sum), na.rm = T)
  
  
  # change NA or NaNs to actual values (if applicable) ---------------------------
  x$reading_time_mean[is.nan(x$reading_time_mean)] <- 0
  x$word_density_mean[is.nan(x$word_density_mean)] <- 1
  
  # change NaN to NA values
  x <- x %>% mutate_all(~ifelse(is.nan(.), NA, .))
  
  # Replace NA values with 0 at baseline
  x$prop_organizations_mean[which(is.na(x$prop_organizations_mean))] <- 0
  x$prop_persons_mean[which(is.na(x$prop_persons_mean))] <- 0
  x$prop_locations_mean[which(is.na(x$prop_locations_mean))] <- 0
  x$functional_words_count_mean[which(is.na(x$functional_words_count_mean))] <- 0
  x$count_organizations_sum[which(is.na(x$count_organizations_sum))] <- 0
  x$word_count_mean[which(is.na(x$word_count_mean))] <- 0
  x$count_persons_sum[which(is.na(x$count_persons_sum))] <- 0
  x$count_locations_sum[which(is.na(x$count_locations_sum))] <- 0
  x$sentence_count_mean[which(is.na(x$sentence_count_mean))] <- 0
  x$count_organizations_sum[which(is.na(x$count_organizations_sum))] <- 0
  x$syllable_count_mean[which(is.na(x$syllable_count_mean))] <- 0
  
  return(x)
  
}

appendIndexes <- function(final_data) {
  # Richness index ------------------------------------------------------------
  rich_base <- final_data[
    c(
      "hapaxlegomenon_R_mean",
      "hapaxlegomenon_hapax_mean",
      "hapax_dislegomenon_h_mean",
      "hapax_dislegomenon_S_mean",
      "type_token_ratio_mean",
      "shannon_entropy_mean",
      "yules_characteristic_K_mean"
      )
    ]
  final_data$index_rich_base <- scale(icwIndex(rich_base, revcols = c(2,3,4,6,7))[[2]])
  rich_end <- final_data[
    c(
      "hapaxlegomenon_R_mean_end", 
      "hapaxlegomenon_hapax_mean_end", 
      "hapax_dislegomenon_h_mean_end", 
      "hapax_dislegomenon_S_mean_end",
      "type_token_ratio_mean_end",
      "shannon_entropy_mean_end",
      "yules_characteristic_K_mean_end"
      )
    ]
  final_data$index_rich_end <- scale(icwIndex(rich_end, revcols = c(2,3,4,6,7))[[2]])
  # Lexical index factual index --------------------------------------------------
  lex_factual_base <- final_data[
    c(
      "prop_persons_mean",
      "prop_locations_mean",
      "prop_organizations_mean",
      "functional_words_count_mean"
      )
    ]
  final_data$index_lex_factual_base <- scale(icwIndex(lex_factual_base, revcols = c(4))[[2]])
  lex_factual_end <- final_data[
    c(
      "prop_persons_mean_end",
      "prop_locations_mean_end",
      "prop_organizations_mean_end",
      "functional_words_count_mean_end"
      )
    ]
  final_data$index_lex_factual_end <- scale(icwIndex(lex_factual_end, revcols = c(4))[[2]])
  # Lexical index length index ---------------------------------------------------
  lex_length_base <- final_data[
    c(
      "word_count_mean",
      "sentence_count_mean",
      "has_url_sum",
      "count_persons_sum",
      "count_locations_sum",
      "count_organizations_sum"
      )
    ]
  final_data$index_lex_length_base <- scale(icwIndex(lex_length_base)[[2]])
  lex_length_end <- final_data[
    c(
      "word_count_mean_end",
      "sentence_count_mean_end",
      "has_url_sum_end",
      "count_persons_sum_end",
      "count_locations_sum_end",
      "count_organizations_sum_end"
      )]
  final_data$index_lex_length_end <- scale(icwIndex(lex_length_end)[[2]])
  # Readability index ------------------------------------------------------------
  read_base <- final_data[
    c(
      "flesch_reading_mean",
      "fernandez_huerta_mean",
      "reading_time_mean",
      "crawford_mean",
      "gutierrez_polini_mean"
      )
    ]
  final_data$index_read_base <- scale(icwIndex(read_base, revcols = c(1,2,3,4))[[2]])
  read_end <- final_data[
    c(
      "flesch_reading_mean_end",
      "fernandez_huerta_mean_end",
      "reading_time_mean_end",
      "crawford_mean_end",
      "gutierrez_polini_mean_end"
      )
    ]
  final_data$index_read_end <- scale(icwIndex(read_end, revcols = c(1,2,3,4))[[2]])
  # Interactions index -----------------------------------------------------------
  int_base <- final_data[
    c(
      "total_reactions_sum",
      "total_comments_sum",
      "total_shares_sum"
      )
    ]
  final_data$index_int_base <- scale(icwIndex(int_base)[[2]])
  int_end <- final_data[
    c(
      "total_reactions_sum_end",
      "total_comments_sum_end",
      "total_shares_sum_end"
      )
    ]
  final_data$index_int_end <- scale(icwIndex(int_end)[[2]])
  # Quality index ----------------------------------------------------------------
  quality_base <- final_data[
    c(
      "has_url_sum",
      "sentiment_score_mean",
      "has_text_sum",
      "has_image_sum"
      )
    ]
  final_data$index_quality_base <- scale(icwIndex(quality_base, revcols = c(4))[[2]])
  quality_end <- final_data[
    c(
      "has_url_sum_end",
      "sentiment_score_mean_end",
      "has_text_sum_end",
      "has_image_sum_end"
      )
    ]
  final_data$index_quality_end <- scale(icwIndex(quality_end, revcols = c(4))[[2]])
  # Share misinformation index ---------------------------------------------------
  share_mis_base <- final_data[
    c(
      "preds_model_beto_sm_num_mean",
      "preds_model_numeric_sm_num_mean",
      "preds_model_wobeto_sm_num_mean"
      )
    ]
  final_data$index_share_mis_base <- scale(icwIndex(share_mis_base)[[2]])
  share_mis_end <- final_data[
    c(
      "preds_model_beto_sm_num_mean_end",
      "preds_model_numeric_sm_num_mean_end",
      "preds_model_wobeto_sm_num_mean_end"
      )
    ]
  final_data$index_share_mis_end <- scale(icwIndex(share_mis_end)[[2]])
  return(final_data)
}


poolTreatmentBalance <- function(data, c_s, t_s, c_w, t_w, c_nw, t_nw) {
  
  data["c_t_strong_total"] <- data[c_s] + data[t_s]
  data["c_t_weak_total"] <- data[c_w] + data[t_w]
  data["c_t_neither_total"] <- data[c_nw] + data[t_nw]
  data["t_strong"] <- data[t_s]
  data["t_weak"] <- data[t_w]
  data["t_neither"] <- data[t_nw]
  data["strong_weak"] <- data['t_strong'] + data['t_weak']
  data["all_t"] <- data['t_strong'] + data['t_weak'] + data['t_neither']
  
  return(data)
}

poolTreatmentBalance1 <- function(data, c_s, t_s, c_w, t_w, all_t, all){
  
  data["c_t_strong_total"] <- data[c_s] + data[t_s]
  data["c_t_weak_total"] <- data[c_w] + data[t_w]
  data["c_t_neither_total"] <- data[all] - data["c_t_strong_total"] - 
    data["c_t_weak_total"]
  data["t_strong"] <- data[t_s]
  data["t_weak"] <- data[t_w]
  data["t_neither"] <- data[all_t] - data["t_strong"] - data["t_weak"]
  data["strong_weak"] <- data['t_strong'] + data['t_weak']
  
  return(data)
}

poolTreatmentBalance2 <- function(data, all_t, all) {
  
  data["total_treated"] <- data[all_t] 
  data["total_influencers"] <- data[all]
  
  return(data)
}


loadBalanceData <- function(path){

  stage0 <- read_parquet(
    paste0(path, 'stage0.parquet'),
    as_tibble = TRUE
  )
  stage2 <- read_parquet(
    paste0(path, 'stage2.parquet'),
    as_tibble = TRUE
  )
  
  df_list <- list(stage0, stage2)
  df_list <- lapply(df_list, byFollower)
  stage0 <- as.data.frame(df_list[[1]])

  stage2 <- as.data.frame(df_list[[2]])

  stage0 <- stage0[stage0$handle %in% stage2$handle, ]
  stage2 <- stage2[stage2$handle %in% stage0$handle, ]
  names(stage2) <- paste0(names(stage2), "_end")

  
  # Append dataframes 
  data <- left_join(stage0, stage2, by = c("handle" = "handle_end"))
  
  return(data)
}

byFollowerCollapse <- function(x, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11){
  x <- x %>%
    dplyr::select(
      handle,
      assignment_num,
      date_period,
      .data[[c1]],
      .data[[c2]],
      .data[[c3]], 
      .data[[c4]],
      .data[[c5]], 
      .data[[c6]], 
      .data[[c7]], 
      .data[[c8]],
      .data[[c9]], 
      .data[[c10]],
      .data[[c11]],
      hapaxlegomenon_R,
      hapaxlegomenon_hapax,
      hapax_dislegomenon_h,
      hapax_dislegomenon_S, # Readability
      word_density,
      readability_index,
      simpsons_index,
      brunets_measure_w,
      szigriszt_pazos, # Readability
      type_token_ratio, 
      shannon_entropy, 
      yules_characteristic_K, # Readability
      prop_persons, 
      prop_locations, 
      prop_organizations, 
      functional_words_count, # Lexical index factual
      word_count, 
      sentence_count, 
      find_url, 
      has_url, 
      count_persons, 
      count_locations, 
      count_organizations, 
      syllable_count,  # Lexical index length
      flesch_reading, 
      fernandez_huerta, 
      reading_time, 
      crawford, 
      gutierrez_polini, # Richness index
      total_reactions, 
      total_comments, 
      total_shares, # Interaction index
      sentiment_score, 
      has_image, 
      has_text, # quality
      preds_model_beto_sm_num, 
      preds_model_numeric_sm_num, 
      preds_model_wobeto_sm_num, 
      feat_25common_unigrams, 
      feat_50common_unigrams, # share misinformation
      fact_check
    ) %>% 
    group_by(
      handle,
      assignment_num,
      date_period,
      .data[[c1]],
      .data[[c2]],
      .data[[c3]], 
      .data[[c4]],
      .data[[c5]], 
      .data[[c6]], 
      .data[[c7]], 
      .data[[c8]],
      .data[[c9]], 
      .data[[c10]],
      .data[[c11]]
    ) %>% 
    summarise_all(funs(mean, sum), na.rm = T)
  
  
  # change NA or NaNs to actual values (if applicable) ---------------------------
  x$reading_time_mean[is.nan(x$reading_time_mean)] <- 0
  x$word_density_mean[is.nan(x$word_density_mean)] <- 1
  
  # change NaN to NA values
  x <- x %>% mutate_all(~ifelse(is.nan(.), NA, .))
  
  # Replace NA values with 0 at baseline
  x$prop_organizations_mean[which(is.na(x$prop_organizations_mean))] <- 0
  x$prop_persons_mean[which(is.na(x$prop_persons_mean))] <- 0
  x$prop_locations_mean[which(is.na(x$prop_locations_mean))] <- 0
  x$functional_words_count_mean[which(is.na(x$functional_words_count_mean))] <- 0
  x$count_organizations_sum[which(is.na(x$count_organizations_sum))] <- 0
  x$word_count_mean[which(is.na(x$word_count_mean))] <- 0
  x$count_persons_sum[which(is.na(x$count_persons_sum))] <- 0
  x$count_locations_sum[which(is.na(x$count_locations_sum))] <- 0
  x$sentence_count_mean[which(is.na(x$sentence_count_mean))] <- 0
  x$count_organizations_sum[which(is.na(x$count_organizations_sum))] <- 0
  x$syllable_count_mean[which(is.na(x$syllable_count_mean))] <- 0
  
  return(x)
}
