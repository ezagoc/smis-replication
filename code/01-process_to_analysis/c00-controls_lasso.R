###################################################
## Data Analysis: Linear intensive (margin) interaction filtering to less than 10 total influencers followed
## Author: Eduardo Zago-Cuevas
## Run before: same folder, a number before
## Output: Judicial panel dataset 2009-2012
##
###################################################

# 0.0 Set up the environment, clean it and set working directory to the code path

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

src_path <- c("../../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

library(tidyverse)
library(arrow)


stage <- 'stage3_4'
f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                   initial_path = '../../../') |>
  select(follower_id)

load('../../../data/02-randomize/KE/03-assignment/input/twitter_followers.Rda')

df <- read_parquet('../../../data/02-randomize/KE/02-variables/variables_followers.parquet') 

# |> 
#   select(follower_id, username, followers_count, 
#          following_count, listed_count, days_old_account, tweet_count) |> 
#   filter(follower_id %in% f$follower_id) |> mutate(pais = 'KE', batch_id = 'b1')

df2 <- read_parquet('../../../data/02-randomize/KE/02-variables/variables_followers_batch2.parquet') |> 
  select(follower_id, username, followers_count, n_weak, n_strong, n_absent, 
         following_count, listed_count, days_old_account, tweet_count) |> 
  filter(follower_id %in% f$follower_id) |> mutate(pais = 'KE', batch_id = 'b2')

df3 <- read_parquet('../../../data/02-randomize/SA/02-variables/variables_followers.parquet') |> 
  select(follower_id, username, followers_count, n_weak, n_strong, n_absent, 
         following_count, listed_count, days_old_account, tweet_count) |> 
  filter(follower_id %in% f$follower_id) |> mutate(pais = 'SA', batch_id = 'b1')

df4 <- read_parquet('../../../data/02-randomize/SA/02-variables/variables_followers_batch2.parquet') |> 
  select(follower_id, username, followers_count, n_weak, n_strong, n_absent, 
         following_count, listed_count, days_old_account, tweet_count) |> 
  filter(follower_id %in% f$follower_id) |> mutate(pais = 'SA', batch_id = 'b2')

df <- rbind(df, df2, df3, df4)

df <- df |> mutate(log_followers_count = log(followers_count+1), 
                   log_following_count = log(following_count+1),
                   log_listed_count = log(listed_count+1),
                   log_tweet_count = log(tweet_count+1),
                   log_days_old_account = log(days_old_account+1), 
                   log_n_weak = log(n_weak+1), 
                   log_n_strong = log(n_strong+1), 
                   log_n_absent = log(n_absent+1))

write_parquet(df, '../../../data/04-analysis/joint/lasso_controls.parquet')
