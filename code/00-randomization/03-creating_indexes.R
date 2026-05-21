#############################################################################
# RCT: Social Media Influencers Based in Africa
# Activity: Creating Indexes
#           Preparing for randomization
# Date: Nov 2022
# Edited for replication: May 2026
#############################################################################

######################## Influencers ########################

# General Configurations ----------------------------------------------------

rm(list=ls()) # Clean out elements in R environment
setwd('../../data/randomization/')
source('../../code/00-randomization/funcs.R')

# Install and load packages

packages <- ipak(c("dplyr","tidyverse"))

# Covariates 

cov0 <- c(
  "username",
  "name",
  "author_id",
  "followers_count", 
  "listed_count",   
  "n_tweets.na",                   
  "n_tweets",                    
  "n_strong",
  "n_weak",
  "n_absent",
  "days_old_account"
  )

cov1 = c(
  "like_count_sum",               
  "quote_count_sum",              
  "reply_count_sum"  
)

# Batch 1

### KE

twitter = arrow::read_parquet(
  'KE/02-variables/variables.parquet'
)

twitter_ties = arrow::read_parquet(
  'KE/02-variables/variables_ties.parquet'
)

twitter = twitter |> left_join(twitter_ties |> rename(author_id = influencer_id),
                               by = 'author_id') |> select(c(cov0, cov1))

# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))

load('KE/03-assignment/input/twitter.Rda')

# Save data

save(twitter, file = 'KE/03-assignment/input/twitter.Rda')


### SA

twitter = arrow::read_parquet(
  'SA/02-variables/variables.parquet'
) |> mutate(author_id = as.character(author_id))

twitter_ties = arrow::read_parquet(
  'SA/02-variables/variables_ties.parquet'
)

twitter = twitter |> left_join(twitter_ties |> rename(author_id = influencer_id),
                               by = 'author_id') |> select(c(cov0, cov1))

# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))

load('SA/03-assignment/input/twitter.Rda')

# Save data

save(twitter, file = 'SA/03-assignment/input/twitter.Rda')


################################### Batch 2 #####################################

### KE

twitter = arrow::read_parquet(
  'KE/02-variables/variables_batch2.parquet'
  )

twitter_ties = arrow::read_parquet(
  'KE/02-variables/variables_ties_batch2.parquet'
)

twitter = twitter |> left_join(twitter_ties |> rename(author_id = influencer_id),
                               by = 'author_id') |> select(c(cov0, cov1))

# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))


# Save data

save(twitter, file = 'KE/03-assignment/input/twitter_batch2.Rda')


### SA

twitter = arrow::read_parquet(
  'SA/02-variables/variables_batch2.parquet'
) |> mutate(author_id = as.character(author_id))

twitter_ties = arrow::read_parquet(
  'SA/02-variables/variables_ties_batch2.parquet'
)

twitter = twitter |> left_join(twitter_ties |> rename(author_id = influencer_id),
                               by = 'author_id') |> select(c(cov0, cov1))

# Create index for twitter
index <- twitter %>% select(cov1) %>% as.matrix() %>% icwIndex() 
twitter$index_influence <- index$index  
twitter <- twitter %>% select(c(cov0, 'index_influence'))

load('SA/03-assignment/input/twitter_batch2.Rda')

# Save data

save(twitter, file = 'SA/03-assignment/input/twitter_batch2.Rda')
