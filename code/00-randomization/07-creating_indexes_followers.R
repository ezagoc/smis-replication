#############################################################################
# RCT: Social Media Influencers Based in Africa
# Activity: Creating Indexes
#           Preparing for randomization
# Date: Nov 2022
# Edited for replication: May 2026
#############################################################################

######################## Followers ########################

# General Configurations ----------------------------------------------------

rm(list=ls()) # Clean out elements in R environment
setwd('../../data/randomization/')
source('../../code/00-randomization/funcs.R')

# Install and load packages

packages <- ipak(c("dplyr","tidyverse"))

# Covariates 

cov0 <- c(
  "username",
  "follower_id",
  "n_strong",   
  "n_weak",                   
  "n_absent",                    
  "n_strong_treated",
  "n_weak_treated",
  "n_absent_treated",
  "days_old_account"
)

cov1 <- c(
  "followers_count",               
  "following_count",              
  "listed_count",
  "tweet_count"
)

# Batch 1: KE

batch <- ''
country <- 'KE'

library(arrow)
twitter = read_parquet(paste0(country,
                              '/02-variables/variables_followers', batch,'.parquet'), 
                       skip_nul = T) %>% 
  select(c(cov0, cov1))

# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))

# Save data



save(twitter, file = paste0(country, 
                            '/03-assignment/input/twitter_followers_rep', batch,
                            '.Rda'))

################################################################################
#################################### SA ################################

batch <- ''
country <- 'SA'

###
library(arrow)
twitter = read_parquet(paste0(country,
                              '/02-variables/variables_followers', batch,'.parquet'), 
                       skip_nul = T) %>% 
  select(c(cov0, cov1))

# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))

# Save data

save(twitter, file = paste0(country, 
                            '/03-assignment/input/twitter_followers',batch,
                            '.Rda'))

## BATCH 1 SA, FIXING FAULTY PROFILES:

twitter = read_parquet(
  'SA/02-variables/variables_followers_batch2.parquet'
) %>% select(c(cov0, cov1))

tw <- twitter |> filter(is.na(listed_count) == T)

list_faulty <- tw$follower_id

get_profs <- function(ids){
  df <- get_user_profile(ids, bearer)
}
library(academictwitteR)
bearer <- 'AAAAAAAAAAAAAAAAAAAAAAB8lgEAAAAAtHuFxjMbRwl7WNHEOpMvzf7%2BGrc%3DATF52dZ90jRf9u9qxVvuiC7WLYCte5c9U4HrWfsuz9RK59Girq'
final <- list_faulty %>% map_dfr(function(x){get_profs(x)})

final <- final |> unnest(public_metrics)

final <- final |> rename(follower_id = id) |> select(follower_id, 
                                                     followers_count:listed_count)

tw <- tw |> select(-c(followers_count:tweet_count)) |> left_join(final)

twitter <- twitter |> filter(is.na(listed_count) == F)

twitter <- rbind(twitter, tw)

write_parquet(twitter, 'SA/02-variables/variables_followers.parquet')
# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))

# Save data

save(twitter, file = 'SA/03-assignment/input/twitter_followers.Rda')

