packages <- c(
  "foreign",
  "tidyverse",
  "arrow",
  "estimatr",
  "writexl",
  "stargazer",
  "matrixStats",
  "starpolishr",
  "readxl",
  "fixest",
  "lfe"
)

aux3 <- c('total_shares_sum', 'total_reactions_sum', 'total_comments_sum',
          'index_int_base', 'verifiability_base', 'n_posts_base')

aux_w <- c('w_shares', 'w_reactions', 'w_comments',
           'w_index_int_base', 'verifiability_base', 'n_posts_base')

aux_w_l <- c('log_w_shares', 'log_w_reactions', 'log_w_comments',
             'log_w_index_int_base', 'verifiability_base', 'n_posts_base')

aux_endline <- c('w_shares', 'w_reactions', 'w_comments',
                 'w_index_int_end', 'verifiability_end', 'true_end', 
                 'n_posts_end')

aux_endline9 <- c('w_9_shares', 'w_9_reactions', 'w_9_comments',
                 'w_9_index_int_end', 'verifiability_end', 'true_end', 
                 'n_posts_end')

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
  "n_influencers_followed_control_weak")

fixed_feols <- "~ t_strong + t_weak + t_neither + ads_treatment | strat_block1 + c_t_strong_total + c_t_weak_total + c_t_neither_total"

fixed_feols2 <- "~ t_strong + t_weak + t_neither | c_t_strong_total + c_t_weak_total + c_t_neither_total"

dep_var2 <- c("\\shortstack{Total \\\\ Shares}", 
              "\\shortstack{Total \\\\ Reactions}",
              "\\shortstack{Total \\\\ Comments}",
              "\\shortstack{Interactions \\\\ index}",
              "\\shortstack{Verifiability}",
              "\\shortstack{Number of \\\\ Posts}")

dep_var_w <- c("\\shortstack{Total \\\\ Shares}", 
              "\\shortstack{Total \\\\ Reactions}",
              "\\shortstack{Total \\\\ Comments}",
              "\\shortstack{Interactions \\\\ index}",
              "\\shortstack{Verifiability}",
              "\\shortstack{Number of \\\\ Posts}")

dep_var_end <- c("\\shortstack{Total \\\\ Shares}", 
               "\\shortstack{Total \\\\ Reactions}",
               "\\shortstack{Total \\\\ Comments}",
               "\\shortstack{Interactions \\\\ index}",
               "\\shortstack{Verifiability}",
               "\\shortstack{True}",
               "\\shortstack{Number of \\\\ Posts}")

dep_var_log <- c("\\shortstack{Log \\\\ Shares}", 
               "\\shortstack{Log \\\\ Reactions}",
               "\\shortstack{Log \\\\ Comments}",
               "\\shortstack{Log Interactions \\\\ index}",
               "\\shortstack{Verifiability}",
               "\\shortstack{Number of \\\\ Posts}")

covariates <- c(
  "Treated strong ties", 
  "Treated weak ties",
  "Treated absent ties"
)

covariates_joint <- c(
  "Treated strong ties", 
  "Treated weak ties",
  "Treated absent ties",
  "Ads Treatment"
)

title <- "Balance on Main Outcomes, Joint Specification ()"

omit_var <- c(
  "Constant",
  "c_t_strong_total",
  "c_t_weak_total",
  "c_t_neither_total"
)

omit_var_joint <- c(
  "Constant",
  "c_t_strong_total",
  "c_t_weak_total",
  "c_t_neither_total",
  "strat_block1"
)

needed_var_joint <- c(
  "ads_treatment", "t_strong", "t_weak", "t_neither",
  "follower_id",
  "c_t_strong_total",
  "c_t_weak_total",
  "c_t_neither_total",
  "strat_block1"
)

show_var <- c(
  "t_strong",
  "t_weak",
  "t_neither"
)

show_var_joint <- c(
  "t_strong",
  "t_weak",
  "t_neither",
  "ads_treatment"
)