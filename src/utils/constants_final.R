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
  "lfe", 
  'purrr'
)

packages_analysis <- c(
  "tidyverse",
  "arrow",
  "writexl",
  "readxl",
  "fixest",
  "lfe", 
  'purrr'
)

# For regressions:

c_s = "n_influencers_followed_control_strong"
t_s = "n_influencers_followed_treatment_strong"
c_w = "n_influencers_followed_control_weak"
t_w = "n_influencers_followed_treatment_weak"
c_nw = "n_influencers_followed_control_no_weak"
t_nw = "n_influencers_followed_treatment_no_weak"

int_fes = "fe_6SAb1 + fe_19SAb1 + fe_7SAb1 + fe_10SAb1 + fe_3SAb1 + fe_2SAb1 + 
fe_16SAb1 + fe_1SAb1 + fe_17SAb1 + fe_4SAb1 + fe_5SAb1 + fe_11SAb1 + fe_13SAb1 + 
fe_15SAb1 + fe_14SAb1 + fe_18SAb1 + fe_12SAb1 + fe_9SAb1 + fe_8SAb1 + fe_6SAb2 + 
fe_2SAb2 + fe_5SAb2 + fe_11SAb2 + fe_4SAb2 + fe_9SAb2 + fe_13SAb2 + fe_12SAb2 + 
fe_10SAb2 + fe_3SAb2 + fe_1SAb2 + fe_8SAb2 + fe_7SAb2 + fe_10KEb1 + fe_19KEb1 + 
fe_14KEb1 + fe_15KEb1 + fe_13KEb1 + fe_11KEb1 + fe_1KEb1 + fe_2KEb1 + fe_12KEb1 + 
fe_5KEb1 + fe_6KEb1 + fe_7KEb1 + fe_3KEb1 + fe_18KEb1 + fe_8KEb1 + fe_9KEb1 + 
fe_17KEb1 + fe_4KEb1 + fe_16KEb1 + fe_13KEb2 + fe_7KEb2 + fe_6KEb2 + fe_4KEb2 + 
fe_14KEb2 + fe_2KEb2 + fe_11KEb2 + fe_12KEb2 + fe_8KEb2 + fe_15KEb2 + fe_9KEb2 + 
fe_3KEb2 + fe_1KEb2 + fe_10KEb2 + fe_5KEb2"

# RTweeted AC and SMIs

aux_rt <- c('interaction_smi', 'interaction_ac')

# English proportions

aux_eng <- c('eng_prop', 'n_eng_prop')

# Interactions sentiment

aux_sent_reac <- c('total_reactions_covid', 'm_pos_reactions_covid', 
                  'm_neu_reactions_covid',  'm_neg_reactions_covid')

aux_sent_reac_vax <- c('total_reactions_vax', 'm_pos_reactions_vax', 
                   'm_neu_reactions_vax',  'm_neg_reactions_vax')

aux_sent_likes <- c('total_likes_covid', 'm_pos_likes_covid', 
                   'm_neu_likes_covid',  'm_neg_likes_covid')

aux_sent_likes_vax <- c('total_likes_vax', 'm_pos_likes_vax', 
                    'm_neu_likes_vax',  'm_neg_likes_vax')

aux_sent_comm <- c('total_comments_covid', 'm_pos_comments_covid', 
                  'm_neu_comments_covid',  'm_neg_comments_covid')

aux_sent_comm_vax <- c('total_comments_vax', 'm_pos_comments_vax', 
                   'm_neu_comments_vax',  'm_neg_comments_vax')

aux_sent_share <- c('total_shares_covid', 'm_pos_shares_covid', 
                   'm_neu_shares_covid',  'm_neg_shares_covid')

aux_senm_share_vax <-  c('total_shares_vax', 'm_pos_shares_vax', 
                         'm_neu_shares_vax',  'm_neg_shares_vax')

# Interactions Verifiability

aux_int_reac <- c('t_verifiability_reactions', 't_true_reactions', 
                  't_fake_reactions',  't_non_ver_reactions', 't_eng_reactions')

aux_int_likes <- c('t_verifiability_likes', 't_true_likes', 
                   't_fake_likes',  't_non_ver_likes', 't_eng_likes')

aux_int_comm <- c('t_verifiability_comments', 't_true_comments', 
                  't_fake_comments',  't_non_ver_comments', 't_eng_comments')

aux_int_share <- c('t_verifiability_shares', 't_true_shares', 
                   't_fake_shares',  't_non_ver_shares', 't_eng_shares')

aux_int_reac_base <- c('t_verifiability_shares_base', 't_true_shares_base', 
                       't_fake_shares_base',  't_non_ver_shares_base', 
                       't_eng_shares_base', 't_verifiability_comments_base', 
                       't_true_comments_base', 't_fake_comments_base',  
                       't_non_ver_comments_base', 't_eng_comments_base', 
                       't_verifiability_likes_base', 't_true_likes_base', 
                       't_fake_likes_base',  't_non_ver_likes_base', 
                       't_eng_likes_base', 't_verifiability_reactions_base', 
                       't_true_reactions_base', 't_fake_reactions_base',  
                       't_non_ver_reactions_base', 't_eng_reactions_base')

# Verifiability

aux_v <- c('verifiability_rt', 'true_rt', 'fake_rt', 'eng_rt', 
         'verifiability_no_rt', 'true_no_rt',
         'fake_no_rt',  'eng_no_rt', 'non_ver_rt', 'non_ver_no_rt')

aux_t <- c('verifiability', 'non_ver', 'true', 'fake', 'eng')

aux_t_posts <- c('verifiability_no_rt', 'non_ver_no_rt', 'true_no_rt', 
           'fake_no_rt', 'eng_no_rt')

aux_t_base <- c('verifiability_base', 'non_ver_base', 'true_base', 'fake_base', 
                'n_posts_base', 'verifiability_rt_base', 
                'non_ver_rt_base', 'true_rt_base', 'fake_rt_base', 
                'n_posts_rt_base', 'verifiability_no_rt_base', 
                'non_ver_no_rt_base','true_no_rt_base', 'fake_no_rt_base', 
                'n_posts_no_rt_base', 'eng_base', 'eng_rt_base', 'eng_no_rt_base'
)

aux_t_base2 <- c('verifiability_base', 'non_ver_base', 'true_base', 'fake_base', 
                'n_posts_base', 'eng_base')

aux_t_base3 <- c('verifiability_no_rt_base', 'non_ver_no_rt_base', 'true_no_rt_base',
                 'fake_no_rt_base', 
                 'n_posts_no_rt_base', 'eng_no_rt_base')

aux_int_reac_base <- c('t_verifiability_shares_base', 't_true_shares_base', 
                       't_fake_shares_base',  't_non_ver_shares_base', 
                       'total_shares_base', 't_verifiability_comments_base', 
                       't_true_comments_base', 't_fake_comments_base',  
                       't_non_ver_comments_base', 'total_comments_base', 
                       't_verifiability_likes_base', 't_true_likes_base', 
                       't_fake_likes_base',  't_non_ver_likes_base', 
                       'total_likes_base', 't_verifiability_reactions_base', 
                       't_true_reactions_base', 't_fake_reactions_base',  
                       't_non_ver_reactions_base', 'total_reactions_base')

# Sentiment

aux_s_v <- c('pos_v_rt_covid', 'pos_v_no_rt_covid', 'neutral_v_rt_covid', 
         'neutral_v_no_rt_covid', 'neg_v_rt_covid', 'neg_v_no_rt_covid',
         'n_posts_rt_covid', 'n_posts_no_rt_covid',
         'pos_v_rt_vax', 'pos_v_no_rt_vax', 'neutral_v_rt_vax', 
         'neutral_v_no_rt_vax', 'neg_v_rt_vax', 'neg_v_no_rt_vax', 
         'n_posts_rt_vax', 'n_posts_no_rt_vax')

aux_s_v_tot <- c('pos_v_covid', 'neutral_v_covid', 'neg_v_covid',
                 'n_posts_covid')

aux_s_v_tot2 <- c('pos_v_vax', 'neutral_v_vax',  'neg_v_vax', 'n_posts_vax')

aux_s_b <- c('pos_b_rt_covid', 'pos_b_no_rt_covid', 'neutral_b_rt_covid', 
             'neutral_b_no_rt_covid', 'neg_b_rt_covid', 'neg_b_no_rt_covid',
             'n_posts_rt_covid', 'n_posts_no_rt_covid', 
             'pos_b_rt_vax', 'pos_b_no_rt_vax', 'neutral_b_rt_vax', 
             'neutral_b_no_rt_vax', 'neg_b_rt_vax', 'neg_b_no_rt_vax',
             'n_posts_rt_vax', 'n_posts_no_rt_vax')

aux_s_b_tot <- c('pos_b_vax', 'neutral_b_vax',  'neg_b_vax',
                 'n_posts_vax')

aux_s_b_tot2 <- c('pos_b_covid', 'neutral_b_covid', 'neg_b_covid',
                  'n_posts_covid')

# URLS

aux_urls <- c('total_urls', 'total_info', 'total_news', 'fact_check', 
              'rel_news', 'non_rel_news', 'other')

aux_urls_base <- c('total_urls_base', 'total_info_base', 'total_news_base', 
                   'fact_check_base', 'rel_news_base', 'non_rel_news_base', 
                   'other_base')



aux_s_b_base <- c('pos_b_covid_base', 'neutral_b_covid_base', 
                  'neg_b_covid_base', 'n_posts_covid_base', 
                  'pos_b_vax_base', 'neutral_b_vax_base', 
                  'neg_b_vax_base', 'n_posts_vax_base',
                  'pos_b_rt_covid_base', 'neutral_b_rt_covid_base', 
                  'neg_b_rt_covid_base', 'n_posts_rt_covid_base', 
                  'pos_b_rt_vax_base', 'neutral_b_rt_vax_base', 
                  'neg_b_rt_vax_base', 'n_posts_rt_vax_base',
                  'pos_b_no_rt_covid_base', 'neutral_b_no_rt_covid_base', 
                  'neg_b_no_rt_covid_base', 'n_posts_no_rt_covid_base', 
                  'pos_b_no_rt_vax_base', 'neutral_b_no_rt_vax_base', 
                  'neg_b_no_rt_vax_base', 'n_posts_no_rt_vax_base')

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


# For tables: 

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