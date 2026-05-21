library(tidyverse)
library(DescTools)

### Linear 

proc_coefs_linear <- function(stage, file, type){
  coef <- readxl::read_excel(paste0(ini, stage, '/original/', data_type, '/',
                                    file, '.xlsx')) |>
    pivot_longer(cols = starts_with(type), names_to = 'var', values_to = 'coef') |> 
    mutate(stage = stage)
  return(coef)
}

proc_ses_linear <- function(stage, file, type){
  
  perm <- readxl::read_excel(paste0(ini, stage, '/permutations/', data_type, '/',
                                    file, '.xlsx')) |> group_by(treatment) |>
    summarise(across(starts_with(type), ~sd(.x))) |> ungroup() |>
    pivot_longer(cols = starts_with(type), names_to = 'var', values_to = 'sd') |> 
    mutate(stage = stage)
  
  return(perm)
}

## Non linear

proc_coefs <- function(stage, file){
  coef <- readxl::read_excel(paste0(ini, stage, '/original/', data_type, '/',
                                    file, '.xlsx')) |>
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'coef') |> 
    mutate(stage = stage)
  return(coef)
}

proc_ses <- function(stage, file){
  
  perm <- readxl::read_excel(paste0(ini, stage, '/permutations/', data_type, '/',
                                    file, '.xlsx')) |> 
    summarise(across(everything(), ~sd(.x))) |> 
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'sd') |> 
    mutate(stage = stage)
  
  return(perm)
}

proc_coefs_base <- function(file){
  coef <- readxl::read_excel(paste0(ini, '/Baseline/original/', file, '.xlsx')) |>
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'coef') |> 
    mutate(stage = stage)
  return(coef)
}

proc_ses_base <- function(file){
  
  perm <- readxl::read_excel(paste0(ini, '/Baseline/permutations/',
                                    file, '.xlsx')) |> 
    summarise(across(everything(), ~sd(.x))) |> 
    pivot_longer(cols = everything(), names_to = 'var', values_to = 'sd') |> 
    mutate(stage = stage)
  
  return(perm)
}

# Read main data files: 

get_analysis_interactions_AC <- function(stage, initial_path = '../../../../', 
                                        batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/verifiability_', batches, '_smi_ac.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/verifiability_', batches, '_smi_ac.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    f <- get_analysis_ver_final_winsor(stage = 'stage1_2', batches = 'b1b2',
                                       initial_path = initial_path) |> 
      select(follower_id, pais, batch_id, n_posts_base)
    
    df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    #df <- df |> mutate(across(c(total_likes_base:t_fake_reactions), 
    #~Winsorize(.x, probs = c(0, .94), na.rm = T)))
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           interaction_smi_base:t_fake_ac) |> 
      mutate(across(c(interaction_smi_base:t_fake_ac), 
                    ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              interaction_smi_base:t_fake_ac) |> 
      mutate(across(c(interaction_smi_base:t_fake_ac), 
                    ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}



get_analysis_followers <- function(initial_path = '../../../../', 
                                        batches = 'b1b2'){
    df_ke <- read_parquet(paste0(initial_path, 
                                 'data/04-analysis/KE/AC/AC_final.parquet')) |> 
      mutate(pais = 'KE') |> select(username:t_neither, pais, AC, AC_base)
    
    df_sa <- read_parquet(paste0(initial_path, 
                                 'data/04-analysis/SA/AC/AC_final.parquet')) |> 
      mutate(pais = 'SA') |> select(username:t_neither, pais, AC, AC_base)
    
    df <- rbind(df_sa, df_ke)
    
    df_ke1 <- read_parquet(paste0(initial_path, 
                                  'data/04-analysis/KE/SMIs/SMIs_final.parquet')) |> 
      mutate(pais = 'KE') |> select(follower_id, pais, SMIs)
    
    df_sa1 <- read_parquet(paste0(initial_path, 
                                  'data/04-analysis/SA/SMIs/SMIs_final.parquet')) |> 
      mutate(pais = 'SA') |> select(follower_id, pais, SMIs)
    
    df1 <- rbind(df_sa1, df_ke1)
    
    df <- df |> left_join(df1, by = c('follower_id', 'pais'))
    
    f <- get_analysis_ver_final_winsor(stage = 'stage1_2', batches = 'b1b2',
                                       initial_path = initial_path) |> 
      filter(batch_id == 'b1') |>
      select(follower_id, pais, n_posts_base, strat_block1, strat_block2)
    
    df <- df |> left_join(f, by = c('follower_id', 'pais'))
    
    df <- df |> filter(is.na(n_posts_base) == F)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
  
  return(df)}
  

get_analysis_int_sent_final <- function(stage, initial_path = '../../../../', 
                                              batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/sentiment_bert_', batches, '_interactions.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/sentiment_bert_', batches, '_interactions.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    f <- get_analysis_ver_final_winsor(stage = 'stage1_2', batches = 'b1b2',
                                       initial_path = initial_path) |> 
      select(follower_id, pais, batch_id, n_posts_base)
    
    df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    #df <- df |> mutate(across(c(total_likes_base:t_fake_reactions), 
    #~Winsorize(.x, probs = c(0, .94), na.rm = T)))
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           starts_with('m_'), starts_with('total_')) |> 
      mutate(across(c(starts_with('m_'), starts_with('total_')), 
                    ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              starts_with('m_'), starts_with('total_')) |> 
      mutate(across(c(starts_with('m_'), starts_with('total_')), 
                    ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_sent_bert_final2 <- function(stage, initial_path = '../../../../', 
                                                batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/sentiment_bert_', batches, '.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/sentiment_bert_', batches, '.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    df <- df |> mutate(pos_b_covid = pos_b_rt_covid + pos_b_no_rt_covid, 
                       neutral_b_covid = neutral_b_rt_covid + neutral_b_no_rt_covid,
                       neg_b_covid = neg_b_rt_covid + neg_b_no_rt_covid, 
                       n_posts_covid = n_posts_rt_covid + n_posts_no_rt_covid,
                       pos_b_covid_base = pos_b_rt_covid_base + pos_b_no_rt_covid_base, 
                       neutral_b_covid_base = neutral_b_rt_covid_base + neutral_b_no_rt_covid_base,
                       neg_b_covid_base = neg_b_rt_covid_base + neg_b_no_rt_covid_base, 
                       n_posts_covid_base = n_posts_rt_covid_base + n_posts_no_rt_covid_base,
                       pos_b_vax = pos_b_rt_vax + pos_b_no_rt_vax, 
                       neutral_b_vax = neutral_b_rt_vax + neutral_b_no_rt_vax,
                       neg_b_vax = neg_b_rt_vax + neg_b_no_rt_vax, 
                       n_posts_vax = n_posts_rt_vax + n_posts_no_rt_vax,
                       pos_b_vax_base = pos_b_rt_vax_base + pos_b_no_rt_vax_base, 
                       neutral_b_vax_base = neutral_b_rt_vax_base + neutral_b_no_rt_vax_base,
                       neg_b_vax_base = neg_b_rt_vax_base + neg_b_no_rt_vax_base, 
                       n_posts_vax_base = n_posts_rt_vax_base + n_posts_no_rt_vax_base)
    
    # df <- df |> mutate(pos_b = pos_b_covid + pos_b_vax, 
    #                    neutral_b = neutral_b_covid + neutral_b_vax,
    #                    neg_b = neg_b_covid + neg_b_vax,
    #                    n_posts_b = n_posts_covid + n_posts_vax,
    #                    pos_b_base = pos_b_covid_base + pos_b_vax_base, 
    #                    neutral_b_base = neutral_b_covid_base + neutral_b_vax_base,
    #                    neg_b_base = neg_b_covid_base + neg_b_vax_base, 
    #                    n_posts_b_base = n_posts_covid_base + n_posts_vax_base, 
    #                    pos_b_rt = pos_b_rt_covid + pos_b_rt_vax, 
    #                    neutral_b_rt = neutral_b_rt_covid + neutral_b_rt_vax,
    #                    neg_b_rt = neg_b_rt_covid + neg_b_rt_vax,
    #                    n_posts_rt = n_posts_rt_covid + n_posts_rt_vax,
    #                    pos_b_rt_base = pos_b_rt_covid_base + pos_b_rt_vax_base, 
    #                    neutral_b_rt_base = neutral_b_rt_covid_base + neutral_b_rt_vax_base,
    #                    neg_b_rt_base = neg_b_rt_covid_base + neg_b_rt_vax_base, 
    #                    n_posts_rt_base = n_posts_rt_covid_base + n_posts_rt_vax_base, 
    #                    pos_b_no_rt = pos_b_no_rt_covid + pos_b_no_rt_vax, 
    #                    neutral_b_no_rt = neutral_b_no_rt_covid + neutral_b_no_rt_vax,
    #                    neg_b_no_rt = neg_b_no_rt_covid + neg_b_no_rt_vax,
    #                    n_posts_no_rt = n_posts_no_rt_covid + n_posts_no_rt_vax,
    #                    pos_b_no_rt_base = pos_b_no_rt_covid_base + pos_b_no_rt_vax_base, 
    #                    neutral_b_no_rt_base = neutral_b_no_rt_covid_base + neutral_b_no_rt_vax_base,
    #                    neg_b_no_rt_base = neg_b_no_rt_covid_base + neg_b_no_rt_vax_base, 
    #                    n_posts_no_rt_base = n_posts_no_rt_covid_base + n_posts_no_rt_vax_base)
    
    #df <- df |> mutate(across(c(pos_b_rt_base:n_posts_no_rt_vax), 
    #~Winsorize(.x, probs = c(0, .94), na.rm = T)))
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           pos_b_rt_base:n_posts_vax_base) |> 
      mutate(across(c(pos_b_rt_base:n_posts_vax_base), ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              pos_b_rt_base:n_posts_vax_base) |> 
      mutate(across(c(pos_b_rt_base:n_posts_vax_base), ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                       initial_path = initial_path) |> 
      select(follower_id, pais, batch_id, n_posts_base)
    
    df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_sent_final_winsor <- function(stage, initial_path = '../../../../', 
                                           batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/sentiment_', batches, '.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/sentiment_', batches, '.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    df <- df |> mutate(pos_v_covid = pos_v_rt_covid + pos_v_no_rt_covid, 
                       neutral_v_covid = neutral_v_rt_covid + neutral_v_no_rt_covid,
                       neg_v_covid = neg_v_rt_covid + neg_v_no_rt_covid, 
                       n_posts_covid = n_posts_rt_covid + n_posts_no_rt_covid,
                       pos_v_covid_base = pos_v_rt_covid_base + pos_v_no_rt_covid_base, 
                       neutral_v_covid_base = neutral_v_rt_covid_base + neutral_v_no_rt_covid_base,
                       neg_v_covid_base = neg_v_rt_covid_base + neg_v_no_rt_covid_base, 
                       n_posts_covid_base = n_posts_rt_covid_base + n_posts_no_rt_covid_base,
                       pos_v_vax = pos_v_rt_vax + pos_v_no_rt_vax, 
                       neutral_v_vax = neutral_v_rt_vax + neutral_v_no_rt_vax,
                       neg_v_vax = neg_v_rt_vax + neg_v_no_rt_vax, 
                       n_posts_vax = n_posts_rt_vax + n_posts_no_rt_vax,
                       pos_v_vax_base = pos_v_rt_vax_base + pos_v_no_rt_vax_base, 
                       neutral_v_vax_base = neutral_v_rt_vax_base + neutral_v_no_rt_vax_base,
                       neg_v_vax_base = neg_v_rt_vax_base + neg_v_no_rt_vax_base, 
                       n_posts_vax_base = n_posts_rt_vax_base + n_posts_no_rt_vax_base)
    
    # df <- df |> mutate(pos_v = pos_v_covid + pos_v_vax, 
    #                    neutral_v = neutral_v_covid + neutral_v_vax,
    #                    neg_v = neg_v_covid + neg_v_vax,
    #                    n_posts_v = n_posts_covid + n_posts_vax,
    #                    pos_v_base = pos_v_covid_base + pos_v_vax_base, 
    #                    neutral_v_base = neutral_v_covid_base + neutral_v_vax_base,
    #                    neg_v_base = neg_v_covid_base + neg_v_vax_base, 
    #                    n_posts_v_base = n_posts_covid_base + n_posts_vax_base, 
    #                    pos_v_rt = pos_v_rt_covid + pos_v_rt_vax, 
    #                    neutral_v_rt = neutral_v_rt_covid + neutral_v_rt_vax,
    #                    neg_v_rt = neg_v_rt_covid + neg_v_rt_vax,
    #                    n_posts_rt = n_posts_rt_covid + n_posts_rt_vax,
    #                    pos_v_rt_base = pos_v_rt_covid_base + pos_v_rt_vax_base, 
    #                    neutral_v_rt_base = neutral_v_rt_covid_base + neutral_v_rt_vax_base,
    #                    neg_v_rt_base = neg_v_rt_covid_base + neg_v_rt_vax_base, 
    #                    n_posts_rt_base = n_posts_rt_covid_base + n_posts_rt_vax_base, 
    #                    pos_v_no_rt = pos_v_no_rt_covid + pos_v_no_rt_vax, 
    #                    neutral_v_no_rt = neutral_v_no_rt_covid + neutral_v_no_rt_vax,
    #                    neg_v_no_rt = neg_v_no_rt_covid + neg_v_no_rt_vax,
    #                    n_posts_no_rt = n_posts_no_rt_covid + n_posts_no_rt_vax,
    #                    pos_v_no_rt_base = pos_v_no_rt_covid_base + pos_v_no_rt_vax_base, 
    #                    neutral_v_no_rt_base = neutral_v_no_rt_covid_base + neutral_v_no_rt_vax_base,
    #                    neg_v_no_rt_base = neg_v_no_rt_covid_base + neg_v_no_rt_vax_base, 
    #                    n_posts_no_rt_base = n_posts_no_rt_covid_base + n_posts_no_rt_vax_base)
    
    #df <- df |> mutate(across(c(pos_v_rt_base:n_posts_no_rt_vax), 
    #~Winsorize(.x, probs = c(0, .94), na.rm = T)))
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           pos_v_rt_base:n_posts_v_base) |> 
      mutate(across(c(pos_v_rt_base:n_posts_v_base), ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              pos_v_rt_base:n_posts_v_base) |> 
      mutate(across(c(pos_v_rt_base:n_posts_v_base), ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                       initial_path = initial_path) |> 
      select(follower_id, pais, batch_id, n_posts_base)
    
    df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_english_winsor <- function(stage, initial_path = '../../../../', 
                                          batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/english_', batches, '.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/english_', batches, '.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                       initial_path = initial_path) |> 
      select(follower_id, pais, batch_id, n_posts_base, n_posts_rt_base, 
             n_posts_no_rt_base, n_posts, n_posts_rt, n_posts_no_rt)
    
    df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    df <- df |> mutate(across(c(eng_base:n_eng_no_rt), 
                              ~Winsorize(.x, probs = c(0, .94), na.rm = T)))
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           eng_base:n_eng_no_rt) |> 
      mutate(across(c(eng_base:n_eng_no_rt), ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              eng_base:n_eng_no_rt) |> 
      mutate(across(c(eng_base:n_eng_no_rt), ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_int_ver_final_winsor <- function(stage, initial_path = '../../../../', 
                                          batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/verifiability_', batches, '_interactions.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/verifiability_', batches, '_interactions.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    f <- get_analysis_ver_final_winsor(stage = 'stage1_2', batches = 'b1b2',
                                       initial_path = '../../../../') |> 
      select(follower_id, pais, batch_id, n_posts_base)
    
    df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    #df <- df |> mutate(across(c(total_likes_base:t_fake_reactions), 
                              #~Winsorize(.x, probs = c(0, .94), na.rm = T)))
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           total_likes_base:t_fake_reactions) |> 
      mutate(across(c(total_likes_base:t_fake_reactions), 
                    ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              total_likes_base:t_fake_reactions) |> 
      mutate(across(c(total_likes_base:t_fake_reactions), 
                    ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_urls <- function(stage, initial_path = '../../../../', 
                                                batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/urls_', batches, '.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/urls_', batches, '.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    #df <- df |> mutate(across(c(fact_check_base:total_info), 
    #~Winsorize(.x, probs = c(0, .94), na.rm = T)))
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           fact_check_base:total_info) |> 
      mutate(across(c(fact_check_base:total_info), ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              fact_check_base:total_info) |> 
      mutate(across(c(fact_check_base:total_info), ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                       initial_path = '../../../../') |> 
      select(follower_id, pais, batch_id, n_posts_base)
    
    df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_sent_bert_final_winsor <- function(stage, initial_path = '../../../../', 
                                           batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/sentiment_bert_', batches, '.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/sentiment_bert_', batches, '.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    df <- df |> mutate(pos_b_covid = pos_b_rt_covid + pos_b_no_rt_covid, 
                       neutral_b_covid = neutral_b_rt_covid + neutral_b_no_rt_covid,
                       neg_b_covid = neg_b_rt_covid + neg_b_no_rt_covid, 
                       n_posts_covid = n_posts_rt_covid + n_posts_no_rt_covid,
                       pos_b_covid_base = pos_b_rt_covid_base + pos_b_no_rt_covid_base, 
                       neutral_b_covid_base = neutral_b_rt_covid_base + neutral_b_no_rt_covid_base,
                       neg_b_covid_base = neg_b_rt_covid_base + neg_b_no_rt_covid_base, 
                       n_posts_covid_base = n_posts_rt_covid_base + n_posts_no_rt_covid_base,
                       pos_b_vax = pos_b_rt_vax + pos_b_no_rt_vax, 
                       neutral_b_vax = neutral_b_rt_vax + neutral_b_no_rt_vax,
                       neg_b_vax = neg_b_rt_vax + neg_b_no_rt_vax, 
                       n_posts_vax = n_posts_rt_vax + n_posts_no_rt_vax,
                       pos_b_vax_base = pos_b_rt_vax_base + pos_b_no_rt_vax_base, 
                       neutral_b_vax_base = neutral_b_rt_vax_base + neutral_b_no_rt_vax_base,
                       neg_b_vax_base = neg_b_rt_vax_base + neg_b_no_rt_vax_base, 
                       n_posts_vax_base = n_posts_rt_vax_base + n_posts_no_rt_vax_base)
    
    #df <- df |> mutate(across(c(pos_b_rt_base:n_posts_no_rt_vax), 
                              #~Winsorize(.x, probs = c(0, .94), na.rm = T)))
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           pos_b_rt_base:n_posts_vax_base) |> 
      mutate(across(c(pos_b_rt_base:n_posts_vax_base), ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              pos_b_rt_base:n_posts_vax_base) |> 
      mutate(across(c(pos_b_rt_base:n_posts_vax_base), ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                      initial_path = '../../../../') |> 
      select(follower_id, pais, batch_id, n_posts_base)
    
    df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_sent_final_winsor <- function(stage, initial_path = '../../../../', 
                                          batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/sentiment_', batches, '.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/sentiment_', batches, '.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    df <- df |> mutate(pos_v_covid = pos_v_rt_covid + pos_v_no_rt_covid, 
                       neutral_v_covid = neutral_v_rt_covid + neutral_v_no_rt_covid,
                       neg_v_covid = neg_v_rt_covid + neg_v_no_rt_covid, 
                       n_posts_covid = n_posts_rt_covid + n_posts_no_rt_covid,
                       pos_v_covid_base = pos_v_rt_covid_base + pos_v_no_rt_covid_base, 
                       neutral_v_covid_base = neutral_v_rt_covid_base + neutral_v_no_rt_covid_base,
                       neg_v_covid_base = neg_v_rt_covid_base + neg_v_no_rt_covid_base, 
                       n_posts_covid_base = n_posts_rt_covid_base + n_posts_no_rt_covid_base,
                       pos_v_vax = pos_v_rt_vax + pos_v_no_rt_vax, 
                       neutral_v_vax = neutral_v_rt_vax + neutral_v_no_rt_vax,
                       neg_v_vax = neg_v_rt_vax + neg_v_no_rt_vax, 
                       n_posts_vax = n_posts_rt_vax + n_posts_no_rt_vax,
                       pos_v_vax_base = pos_v_rt_vax_base + pos_v_no_rt_vax_base, 
                       neutral_v_vax_base = neutral_v_rt_vax_base + neutral_v_no_rt_vax_base,
                       neg_v_vax_base = neg_v_rt_vax_base + neg_v_no_rt_vax_base, 
                       n_posts_vax_base = n_posts_rt_vax_base + n_posts_no_rt_vax_base)
    
    #df <- df |> mutate(across(c(pos_v_rt_base:n_posts_no_rt_vax), 
                              #~Winsorize(.x, probs = c(0, .94), na.rm = T)))
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           pos_v_rt_base:n_posts_vax_base) |> 
      mutate(across(c(pos_v_rt_base:n_posts_vax_base), ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              pos_v_rt_base:n_posts_vax_base) |> 
      mutate(across(c(pos_v_rt_base:n_posts_vax_base), ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                       initial_path = '../../../../') |> 
      select(follower_id, pais, batch_id, n_posts_base)
    
    df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_ver_final_winsor <- function(stage, initial_path = '../../../../', 
                                   batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/verifiability_', batches, '.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/verifiability_', batches, '.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    
    df <- df |> mutate(across(c(total_shares_base:n_posts_no_rt), 
                              ~Winsorize(.x, probs = c(0, .94), na.rm = T)))
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           total_shares_base:n_posts_no_rt) |> 
      mutate(across(c(total_shares_base:n_posts_no_rt), ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              total_shares_base:n_posts_no_rt) |> 
      mutate(across(c(total_shares_base:n_posts_rt), ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_ver_final <- function(stage, initial_path = '../../../../', 
                                   batches = 'b1b2'){
  if (stage %in% c('stage1_2', 'stage3_4', 'stage5_6')){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/verifiability_', batches, '.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/verifiability_', batches, '.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    
    excluded <- c('follower_id', 'pais', 'batch_id')
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           total_shares_base:n_posts_no_rt) |> 
      mutate(across(c(total_shares_base:n_posts_no_rt), ~log(.x + 1))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("log_", .))
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              total_shares_base:n_posts_no_rt) |> 
      mutate(across(c(total_shares_base:n_posts_rt), ~asinh(.x))) |>
      rename_at(vars(-all_of(excluded)), ~paste0("arc_", .))
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_data_ver <- function(stage, initial_path = '../../../../'){
  if (stage == 'stage1_2'){
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/final_data_b1b2p.parquet')) |> 
      mutate(pais = 'KE')
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/final_data_b1b2p.parquet')) |> 
      mutate(pais = 'SA')
    
    df <- rbind(df_sa, df_ke)
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           total_shares_base:fake_no_rt) |> 
      mutate(across(c(total_shares_base:fake_no_rt), ~log(.x + 1)))
    
    colnames(df_log)[4:43] <- paste0('log_', colnames(df_log)[4:43])
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              total_shares_base:fake_no_rt) |> 
      mutate(across(c(total_shares_base:fake_no_rt), ~asinh(.x)))
    
    colnames(df_arcsin)[4:43] <- paste0('arc_', colnames(df_arcsin)[4:43])
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
  } else if(stage == 'stage3_4' | stage == 'stage5_6'){
    
    df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                                 '/final_data_b1b2p.parquet')) |> mutate(pais = 'KE')
    
    
    df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                                 '/final_data_b1b2p.parquet')) |> mutate(pais = 'SA')
    
    
    df <- rbind(df_sa, df_ke)
    
    df_log <- df |> select(follower_id, pais, batch_id, 
                           total_shares_rt_base:fake_no_rt) |> 
      mutate(across(c(total_shares_rt_base:fake_no_rt), ~log(.x + 1)))
    
    colnames(df_log)[4:length(df_log)] <- paste0('log_', colnames(df_log)[4:length(df_log)])
    
    df_arcsin <- df |> select(follower_id, pais, batch_id, 
                              total_shares_rt_base:fake_no_rt) |> 
      mutate(across(c(total_shares_rt_base:fake_no_rt), ~asinh(.x)))
    
    colnames(df_arcsin)[4:length(df_arcsin)] <- paste0('arc_', colnames(df_arcsin)[4:length(df_arcsin)])
    
    df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
      left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
    
    df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                       total_influencers = c_t_strong_total + c_t_weak_total + 
                         c_t_neither_total)
    
  } else {print('Not a correct stage, choose from stage1_2, stage3_4 and stage5_6')}
  
  return(df)
  
}

get_analysis_data_sent_bert <- function(stage, initial_path = '../../../../'){
  df_ke <- read_parquet(paste0(initial_path, 'data/04-analysis/KE/', stage, 
                               '/final_data_b1b2p_sent_bert.parquet'))
  
  df_sa <- read_parquet(paste0(initial_path, 'data/04-analysis/SA/', stage, 
                               '/final_data_b1b2p_sent_bert.parquet'))
  
  df <- rbind(df_ke, df_sa)
  
  df <- df |> mutate(total_treated = t_strong + t_weak + t_neither,
                     total_influencers = c_t_strong_total + c_t_weak_total + 
                       c_t_neither_total)
  
  df_log <- df |> select(follower_id, pais, batch_id, 
                         pos_b_rt_base:n_posts_no_rt_vax) |> 
    mutate(across(c(pos_b_rt_base:n_posts_no_rt_vax), ~log(.x + 1)))
  
  colnames(df_log)[4:length(df_log)] <- paste0('log_', 
                                               colnames(df_log)[4:length(df_log)])
  
  df_arcsin <- df |> select(follower_id, pais, batch_id, 
                            pos_b_rt_base:n_posts_no_rt_vax) |> 
    mutate(across(c(pos_b_rt_base:n_posts_no_rt_vax), ~asinh(.x)))
  
  colnames(df_arcsin)[4:length(df_arcsin)] <- paste0('arc_', 
                                                     colnames(df_arcsin)[4:length(df_arcsin)])
  
  df <- df |> left_join(df_log, by = c('follower_id', 'pais', 'batch_id')) |>
    left_join(df_arcsin, by = c('follower_id', 'pais', 'batch_id'))
}