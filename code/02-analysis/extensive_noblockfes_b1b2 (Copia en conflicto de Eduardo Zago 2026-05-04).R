# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
library(fastDummies)
src_path <- c("../../src/utils/")             
source_files <- list(
  "funcs_analysis.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

#############################################
# 2.0 Define constants Here change everything

country <- 'joint'
data_type <- 'Baseline'
stage <- 'stage1_2'
list_types <- list('log_')
file_code <- 'extensive_verifiability_fes_p90_p0_b2_b1'
ini <- '../../data/analysis/joint/'
#influencer_thr <- 8
n_posts_thr <- 0

# Above p90, p95 dummies
belp90 <- read_parquet('../../data/analysis/joint/below_p90_p95_divider.parquet') |>
  select(-n_posts_base)

blocks_ke <- read_parquet(paste0('../../data/analysis/KE/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

blocks_sa <- read_parquet(paste0('../../data/analysis/SA/extensive_fixed_effects.parquet')) |>
  select(follower_id, username_influencer = username, pais:block2_fe)

blocks <- rbind(blocks_ke, blocks_sa)

# Analysis datasets

df <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                    initial_path = '../../') |> 
  left_join(belp90, by = c('follower_id', 'batch_id', 'pais')) 

f <- get_analysis_english_winsor(stage = stage, batches = 'b1b2',
                                 initial_path = '../../') |> 
  select(follower_id, pais, batch_id, eng, eng_base, eng_rt, eng_rt_base, 
         eng_no_rt, eng_no_rt_base, log_eng, log_eng_base, log_eng_rt, 
         log_eng_rt_base, log_eng_no_rt, log_eng_no_rt_base)

# Merges and filters

df <- df |> left_join(f, by = c('follower_id', 'pais', 'batch_id'))

df <- df |> filter(below_p90 == 1) |> 
  filter(total_influencers == 1) |> filter(n_posts_base > n_posts_thr) 

f <- df |> select(follower_id, pais, batch_id, total_treated, 
                  total_influencers) 


# Normalize by monthly tweets

at <- read_parquet('../../data/analysis/attrition.parquet') |> mutate(batch_id = 'b2')

baseline_div <- read_parquet('../../data/analysis/KE/baseline/baseline_divided_b1b2.parquet') |>
  mutate(pais = 'KE')

baseline_div2 <- read_parquet('../../data/analysis/SA/baseline/baseline_divided_b1b2.parquet') |>
  mutate(pais = 'SA')

baseline_div = rbind(baseline_div, baseline_div2) |> 
  select(follower_id, username, pais, batch_id, 
         total_shares_base_first:n_posts_no_rt_base_third) 

baseline_div1 <- read_parquet('../../data/analysis/KE/baseline/baseline_english_divided_b1b2.parquet') |>
  mutate(pais = 'KE')

baseline_div12 <- read_parquet('../../data/analysis/SA/baseline/baseline_english_divided_b1b2.parquet') |>
  mutate(pais = 'SA')

baseline_div1 = rbind(baseline_div1, baseline_div12)

baseline_div = baseline_div |> left_join(at, by = c('username', 'pais', 'batch_id'))

baseline_div <- baseline_div |> 
  mutate(dummy_second = ifelse(is.na(dummy_attrition) == F & 
                                 pais == 'SA' & dummy_attrition == 1, 1, 0), 
         dummy_third = ifelse(is.na(dummy_attrition) == F & 
                                dummy_attrition == 1, 1, 0))

baseline_div <- baseline_div |>
  left_join(baseline_div1 |> select(follower_id, pais, batch_id, 
                                    eng_base_first:n_eng_no_rt_base_third), 
            by = c('follower_id', 'pais', 'batch_id'))
baseline_div <- baseline_div |>
  mutate(across(contains("_base"), ~ log(as.numeric(.x) + 1))) %>%
  rename_with(~ paste0('log_', .x), contains("_base"))

# Clean environment

df <- df |> left_join(baseline_div, by = c('follower_id', 'pais', 'batch_id')) 

df <- df |> select(follower_id, pais, batch_id, total_influencers, total_treated, 
                   dummy_second, dummy_third,
                   starts_with(paste0('log_', aux_t))) |> 
  left_join(blocks, by = c('follower_id', 'pais', 'batch_id')) |> filter(batch_id == 'b2')

#########################
#### Run Looop ##########
#########################


  aux <- paste0('log_', aux_t)
  
  # 4.0 Run original estimates
  #aux_data <- df[aux]
  lm_list_ols <- list()
  count <- 1
  for (x in aux) {
    fmla1 <- as.formula(paste0(x, "_base ~ total_treated + ",
                               " + dummy_second + dummy_third ", 
                               "| pais + batch_id"))
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, feols(fmla1, data = df))
    coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
      select(Estimate)
    names(coefs) <- paste0(x)
    coefs <- cbind('treatment' = rownames(coefs), coefs) |> 
      filter(treatment == 'total_treated')
    rownames(coefs) <- 1:nrow(coefs)
    lm_list_ols[[count]] <- coefs
    count <- count + 1
  }
  coefs_all <- lm_list_ols %>% 
    reduce(left_join, by = "treatment")
  
  # Build matrix
  ver <- coefs_all %>% 
    select(ends_with(aux[1]))
  ver <- ver[1,]
  
  non_ver <- coefs_all %>% 
    select(ends_with(aux[2]))
  non_ver <- non_ver[1,]
  
  true <- coefs_all %>% 
    select(ends_with(aux[3]))
  true <- true[1,]
  
  fake <- coefs_all %>% 
    select(ends_with(aux[4]))
  fake <- fake[1,]
  
  n_posts <- coefs_all %>% 
    select(ends_with(aux[5]))
  n_posts <- n_posts[1,]
  
  # Eng: 
  
  eng <- coefs_all %>% 
    select(ends_with(aux[6]))
  eng <- eng[1,]
  
  coefs_perm <- data.frame(ver, non_ver, true, fake, n_posts, 
                           eng)
  
  write_xlsx(
    coefs_perm, paste0("../../results/Baseline/original/", type, file_code, ".xlsx"))
  ### 5.0 Run 1000 Permutations: 
  
  i <- 1
  coefs_fin <- tibble()
  for (m in 1:1000){
    print(i)
    followers <- read_parquet(paste0("../../data/analysis/joint/",
                                     'small_ties_b1b2', "/small_tie", 
                                     i,".parquet"))
    
    data <- df
    data <- data |> select(-c(starts_with('tao_')))
    
    c1 = paste0("n_influencers_followed_control_no_weak_tie_p", i)
    c2 = paste0("n_influencers_followed_treatment_no_weak_tie_p", i)
    c3 = paste0("n_influencers_followed_treatment_weak_tie_p", i)
    c4 = paste0("n_influencers_followed_control_weak_tie_p", i)
    c5 = paste0("n_influencers_followed_control_strong_tie_p", i)
    c6 = paste0("n_influencers_followed_treatment_strong_tie_p", i)
    c7 = paste0("n_influencers_followed_control_no_strong_tie_p", i)
    c8 = paste0("n_influencers_followed_treatment_no_strong_tie_p", i)
    c9 = paste0("n_influencers_followed_control_p", i)
    c10 = paste0("n_influencers_followed_treatment_p", i)
    c11 = paste0("n_influencers_followed_p_", i)
    
    followers_iter <- followers %>% select(follower_id, c1, c2, c3, c4, 
                                           c5, c6, c7, c8, c9, c10, c11, pais, 
                                           batch_id) 
    
    data <- left_join(
      data, 
      followers_iter,
      by = c('follower_id', 'batch_id', 'pais')
    )
    # Pool treatment variables
    data <- poolTreatmentBalance2(data, c10, c11)
    
    interactions_perms <- generate_interactions(data)
    
    int_cols2 <- paste(grep("^tao_", colnames(interactions_perms), value = TRUE),
                       collapse = ' + ')
    
    data <- data |> left_join(interactions_perms, 
                              by = c('follower_id', 'pais', 'batch_id'))
    
    # Balance tables 
    aux_data <- data[aux]
    coefs_list <- list()
    lm_list_ols <- list()
    count <- 1
    for (au in aux) {
      fmla1 <- as.formula(paste0(au, "_base ~ total_treated + ",
                                 "+ dummy_second + dummy_third ", 
                                 "| pais + batch_id"))
      nam1 <- paste("lm_", count, "_ols", sep = "")
      assign(nam1, feols(fmla1, data = data))
      coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
        select(Estimate)
      names(coefs) <- paste0(au)
      coefs <- cbind('treatment' = rownames(coefs), coefs) |> 
        filter(treatment == 'total_treated')
      rownames(coefs) <- 1:nrow(coefs)
      lm_list_ols[[count]] <- coefs
      count <- count + 1
    }
    coefs_list <- append(coefs_list, lm_list_ols)
    coefs_all <- coefs_list %>% reduce(left_join, by = "treatment")
    
    # Build matrix
    ver <- coefs_all %>% 
      select(ends_with(aux[1]))
    ver <- ver[1,]
    
    non_ver <- coefs_all %>% 
      select(ends_with(aux[2]))
    non_ver <- non_ver[1,]
    
    true <- coefs_all %>% 
      select(ends_with(aux[3]))
    true <- true[1,]
    
    fake <- coefs_all %>% 
      select(ends_with(aux[4]))
    fake <- fake[1,]
    
    n_posts <- coefs_all %>% 
      select(ends_with(aux[5]))
    n_posts <- n_posts[1,]
    
    # Eng: 
    
    eng <- coefs_all %>% 
      select(ends_with(aux[6]))
    eng <- eng[1,]
    
    coefs_perm <- data.frame(ver, non_ver, true, fake, n_posts, 
                             eng)
    
    coefs_fin <- rbind(coefs_fin, coefs_perm)
    
    
    i <- i + 1}
  print(type)
  write_xlsx(coefs_fin, paste0("../../results/Baseline/permutations/", type, file_code, ".xlsx"))

for (type in list_types){
  
  
  file_coefs <- paste0(type, file_code)
  coefs <- proc_coefs_base(file_coefs)
  ses <- proc_ses_base(file_coefs)
  
  if (type == 'log_'){
    addon <- 'log '
  } else if(type == 'arc_'){
    addon <- 'arcsinh '
  }else {
    addon <- ''
  }
  
  final <- coefs |> left_join(ses, by = c('stage', 'var'))
  
  final <- final |> 
    mutate(Variable = case_when(var == 'ver' ~ paste0(addon, 'Verifiable RTs + Posts'),
                                var == 'non_ver' ~ paste0(addon, 'Non-Verifiable RTs + Posts'),
                                var == 'true' ~ paste0(addon, 'True RTs + Posts'),
                                var == 'fake' ~ paste0(addon, 'Fake RTs + Posts'),
                                var == 'n_posts' ~ paste0(addon, 
                                                          'Number of RTs + Posts'),
                                var == 'eng' ~ paste0(addon, 
                                                      'Number of RTs + Posts (English)')
    ))
  
  
  final$Variable <- factor(final$Variable, levels = c(
    paste0(addon, 'Fake RTs + Posts'), 
    paste0(addon, 'True RTs + Posts'), 
    paste0(addon, 'Verifiable RTs + Posts'), 
    paste0(addon, 'Non-Verifiable RTs + Posts'),
    paste0(addon, 'Number of RTs + Posts (English)'),
    paste0(addon, 'Number of RTs + Posts')))
  
  results_plot <- ggplot(data = final, aes(y = Variable, x = coef)) + 
    geom_point() +
    geom_linerange(aes(xmin = coef - 1.96 * sd, xmax = coef + 1.96 * sd), size = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = .5) +  # Set custom fill colors for points # Set custom line colors for error bars
    theme_bw() +  
    xlab("Total Treated Estimate with 95% Confidence Interval") + 
    ylab("Variable") +  # Change title color
    #ggtitle("Dynamic Effects of the Intervention: Verifiability Analysis") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
          panel.grid.minor = element_blank())
  
  if (addon == 'log '){
    results_plot <- results_plot + xlim(-.7, .2)
  }else{
    results_plot <- results_plot
  }
  ggsave(results_plot, 
         filename = paste0('../../results/plots/',
                           data_type, '/', file_coefs,'.pdf'), 
         device = cairo_pdf, width = 8.22, height = 6.59, units = 'in')
}

results_plot






