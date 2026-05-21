### Filtering Lists: Strong and Weak to at least 0ne Influencer

######################## Followers ########################

#

# General Configurations ----------------------------------------------------
rm(list = ls()) # Clean out elements in R environment
setwd("../../data/randomization/")
source("../../code/00-randomization/funcs.R")

packages <- c(
  "foreign",
  "dplyr",
  "tidyverse",
  "blockTools",
  "writexl",
  "lmtest",
  "sandwich",
  "arrow"
)

ipak(packages)

cov0 <- c(
  "n_strong",   
  "n_weak",                   
  "n_absent",                    
  "n_strong_treated",
  "n_weak_treated",
  "n_absent_treated",
  "days_old_account",
  "index_influence"
)

# Defining the id
id <- "follower_id"

# KE ------------------------

# Kenya covariates:
load("KE/03-assignment/input/twitter_followers.Rda")

# Kenya rand:

randomization <- read_parquet('KE/04-stratification/integrate/followers_randomized.parquet')

baseline_treatment <- twitter |> left_join(randomization |> 
                                             select(-c(username, name)), by = id)

# Filtering:

baseline_strong <- baseline_treatment |> filter(n_strong > 0 | n_weak > 0) |> 
  select(-c(n_strong:index_influence))

strong <- baseline_treatment |> filter(n_strong > 0)

write_parquet(baseline_strong, 
              'KE/04-stratification/integrate/followers_randomized_strong_weak.parquet')

baseline_absent <- baseline_treatment |> filter(n_strong == 0 | n_weak == 0) |> 
  select(-c(n_strong:index_influence))

perc_10 <- round(.1*nrow(baseline_absent), 0)

set.seed(123)
baseline_absent <- baseline_absent |> sample_n(size = perc_10) |> distinct(follower_id)

weak_strong_absent <- rbind(baseline_strong, baseline_absent)

write_parquet(weak_strong_absent, 
              'KE/04-stratification/integrate/followers_randomized_strong_weak_abs.parquet')



# Seem balanced across observations, we will see across covariates
baseline_n <- weak_strong_absent |> group_by(treatment) |> summarise(n_obs = n())


# SA -----------------------

load("SA/03-assignment/input/twitter_followers.Rda")

# SA rand:

randomization <- read_parquet('SA/04-stratification/integrate/followers_randomized.parquet')

baseline_treatment <- twitter |> left_join(randomization |> 
                                             select(-c(username, name)), by = id)

# Filtering:

baseline_strong <- baseline_treatment |> filter(n_strong > 0 | n_weak > 0) |> 
  select(-c(n_strong:index_influence))

strong <- baseline_treatment |> filter(n_weak > 0)

write_parquet(baseline_strong, 
              'SA/04-stratification/integrate/followers_randomized_strong_weak.parquet')

baseline_absent <- baseline_treatment |> filter(n_strong == 0 & n_weak == 0) |> 
  select(-c(n_strong:index_influence))

perc_10 <- round(.1*nrow(baseline_absent), 0)

set.seed(123)
baseline_absent <- baseline_absent |> sample_n(size = perc_10)

weak_strong_absent <- rbind(baseline_strong, baseline_absent)

write_parquet(weak_strong_absent, 
              'SA/04-stratification/integrate/followers_randomized_strong_weak_abs.parquet')

# Seem balanced across observations, we will see across covariates
baseline_n <- baseline_strong |> group_by(treatment) |> summarise(n_obs = n())

## Generating the absent data sets for scrapping:

# KE 

df <- read_parquet('KE/04-stratification/integrate/followers_randomized_strong_weak_abs.parquet')

df <- df |> left_join(twitter |> select(follower_id:n_weak), by = "follower_id") 

df <- df |> filter(n_strong == 0 & n_weak == 0) |> select(username:id)

write_parquet(df, 
              'KE/04-stratification/integrate/followers_randomized_abs.parquet')

# SA

df <- read_parquet('SA/04-stratification/integrate/followers_randomized_strong_weak_abs.parquet')

df <- df |> left_join(twitter |> select(follower_id:n_weak), by = "follower_id") 

df <- df |> filter(n_strong == 0 & n_weak == 0) |> select(username:id)

write_parquet(df, 
              'SA/04-stratification/integrate/followers_randomized_abs.parquet')
