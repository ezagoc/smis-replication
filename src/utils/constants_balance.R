packages <- c(
  "foreign",
  "dplyr",
  "magrittr",
  "tidyverse",
  "lmtest",
  "sandwich",
  "arrow",
  "estimatr",
  "writexl",
  "car",
  "stargazer",
  "matrixStats",
  "starpolishr",
  "readxl",
  "purrr"
)

aux <- c('total_shares_sum', 'total_reactions_sum', 'total_comments_sum',
         'index_int_base', 'verifiability_base', 'true_base', 'n_posts_base')

aux_w <- c('w_shares_sum', 'w_reactions_sum', 'w_comments_sum', 'w_index_int_base')

aux_log <- c('log_w_shares_sum', 'log_w_reactions_sum', 'log_w_comments_sum', 
             'log_w_index_int_base')

c_s = "n_influencers_followed_control_strong"
t_s = "n_influencers_followed_treatment_strong"
c_w = "n_influencers_followed_control_weak"
t_w = "n_influencers_followed_treatment_weak"
c_nw = "n_influencers_followed_control_no_weak"
t_nw = "n_influencers_followed_treatment_no_weak"

treatments_names <- c(
  "n_influencers_followed",
  "n_influencers_followed_control",          
  "n_influencers_followed_treatment",
  "n_influencers_followed_control_no_strong", 
  "n_influencers_followed_treatment_no_strong",
  "n_influencers_followed_control_strong",    
  "n_influencers_followed_treatment_strong",
  "n_influencers_followed_control_no_weak", 
  "n_influencers_followed_treatment_no_weak",
  "n_influencers_followed_treatment_weak",    
  "n_influencers_followed_control_weak"
)

fixed_effects <- "~ t_strong + t_weak + t_neither + ads_treatment | strat_block1 + c_t_strong_total + c_t_weak_total + c_t_neither_total"

dep_var <-  c("\\shortstack{Total \\\\ Shares}", 
              "\\shortstack{Total \\\\ Reactions}",
              "\\shortstack{Total \\\\ Comments}",
              "\\shortstack{Interactions \\\\ index}",
              "\\shortstack{Verifiability}",
              "\\shortstack{True}",
              "\\shortstack{Number of \\\\ Posts}")

dep_var_w <-  c("\\shortstack{Total \\\\ Shares}", 
                  "\\shortstack{Total \\\\ Reactions}",
                  "\\shortstack{Total \\\\ Comments}",
                  "\\shortstack{Interactions \\\\ index}")

dep_var_log <-  c("\\shortstack{Log Total \\\\ Shares}", 
                  "\\shortstack{Log Total \\\\ Reactions}",
                  "\\shortstack{Log Total \\\\ Comments}",
                  "\\shortstack{Log Interactions \\\\ index}")

covariates <- c(
  "Treated strong ties", 
  "Treated weak ties",
  "Treated absent ties",
  "Ads Treatment"
)

title <- "Balance on main indexes"

omit_var <- c(
  "Constant",
  "c_t_strong_total",
  "c_t_weak_total",
  "c_t_neither_total"
)

show_var_joint <- c(
  "t_strong",
  "t_weak",
  "t_neither",
  "ads_treatment"
)
