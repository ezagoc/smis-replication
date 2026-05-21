# General Configurations ----------------------------------------------------
rm(list = ls()) # Clean out elements in R environment
setwd("../../data/randomization/")
source("../../code/00-randomization/funcs.R")
library(tidyverse)

country <- 'KE'

load(paste0(country ,"/03-assignment/input/twitter_followers.Rda"))
df <- arrow::read_parquet(paste0(country,
                          '/04-stratification/integrate/followers_randomized_strong_weak_abs.parquet'))

df_treat <- df |> filter(treatment == 1) |> select(follower_id, treatment)

twitter <- twitter |> left_join(df_treat)

twitter_ov <- twitter |> filter(is.na(treatment) == T)

strong <- twitter_ov |> filter(n_strong > 0 | n_weak > 0)

absent <- twitter_ov |> filter(n_strong == 0 & n_weak == 0)
perc_10 <- round(.01*nrow(absent), 0)

set.seed(123)
absent <- absent |> sample_n(size = perc_10)

weak_strong_absent <- rbind(strong, absent) |> select(-treatment)

# Prueba: 

prueba <- weak_strong_absent |> select(follower_id) |> distinct()

# Export:

arrow::write_parquet(weak_strong_absent, 
                     paste0(country, 
                            "/03-assignment/input/twitter_followers_filtered.parquet"))

################# 2ND BATCH #############

rm(list = ls()) # Clean out elements in R environment
setwd("../../data/randomization/")
source("../../code/00-randomization/funcs.R")
library(tidyverse)

country <- 'KE'

load(paste0(country ,"/03-assignment/input/twitter_followers_batch2.Rda"))
df <- arrow::read_parquet(paste0(country,
                                 '/04-stratification/integrate/followers_randomized_strong_weak_abs.parquet'))

df1 <- arrow::read_parquet(paste0(country, 
                                  '/04-stratification/integrate/followers_randomized.parquet'))

df_treat <- df |> filter(treatment == 1) |> select(follower_id, treatment)

df_treat1 <- df1 |> filter(treatment == 1) |> select(follower_id, treatment)

twitter <- twitter |> left_join(df_treat) |> 
  left_join(df_treat1 |> rename(treatment1 = treatment))

twitter_ov <- twitter |> filter(is.na(treatment) == T & is.na(treatment1) == T)

strong <- twitter_ov |> filter(n_strong > 0 | n_weak > 0)

absent <- twitter_ov |> filter(n_strong == 0 & n_weak == 0)
perc_10 <- round(.01*nrow(absent), 0)

set.seed(123)
absent <- absent |> sample_n(size = perc_10)

weak_strong_absent <- rbind(strong, absent) |> select(-treatment)

# Prueba: 

prueba <- weak_strong_absent |> select(follower_id) |> distinct()

# Export:

arrow::write_parquet(weak_strong_absent, 
                     paste0(country, 
                            "/03-assignment/input/twitter_followers_filtered_batch2.parquet"))



# Prueba :
country <- 'SA'
df <- arrow::read_parquet(paste0(country, 
                          "/04-stratification/integrate/followers_randomized_batch2.parquet"))

df2 <- arrow::read_parquet(paste0(country, 
                                  "/03-assignment/input/twitter_followers_filtered_batch2.parquet"))


df <- df |> left_join(df2 |> select(follower_id:n_absent))

df_strong1 <- df |> filter(n_strong > 0 | n_weak > 0)

df_strong2 <-  df_strong1 |> filter(treatment == 1)
