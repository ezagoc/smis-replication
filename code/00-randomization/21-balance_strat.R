# Balance

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
  "arrow",
  "kableExtra"
)

ipak(packages)

# Defining the co-variates
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

# Kenya covariates:
#load("KE/03-assignment/input/twitter_followers.Rda")
twitter <- arrow::read_parquet("KE/03-assignment/input/twitter_followers_filtered_batch2.parquet")
# Kenya rand:

randomization <- read_parquet('KE/04-stratification/integrate/followers_randomized_batch2.parquet')

# Balance:

fitUp <- lm(as.numeric(randomization$treatment)~as.matrix(twitter[,cov0]))
fitNull <- lm(as.numeric(randomization$treatment)~1)
testpUp <- waldtest(fitUp, fitNull, vcov= vcovHC, test = c("F", "Chisq"))[[4]][2]

baseline_treatment <- merge(twitter, randomization, by=id)

check1 <- t.test(n_strong ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(n_weak ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(n_absent ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(n_strong_treated ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(n_weak_treated ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(n_absent_treated ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(index_influence ~ treatment, data=baseline_treatment)$p.value
check8 <- t.test(days_old_account ~ treatment, data=baseline_treatment)$p.value

p_val <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check8, 
                                         check9)) |> 
  mutate(p_value = round(p_value, 4))

balance_table <- baseline_treatment |> 
  select(n_strong:index_influence, treatment) |> group_by(treatment) |>
  summarise(across(c(n_strong:index_influence), mean)) |> 
  mutate(across(c(n_strong:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(n_strong:index_influence))

treat <- balance_table |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table <- balance_table |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat) |> left_join(p_val) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table, caption = "Balance Table Followers - Kenya",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))

### LOGS

baseline_treatment <- merge(twitter, randomization, by=id)

check1 <- t.test(log(n_strong+1) ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(log(n_weak+1) ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(log(n_absent+1) ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(log(n_strong_treated+1) ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(log(n_weak_treated+1) ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(log(n_absent_treated+1) ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(log(index_influence+1) ~ treatment, data=baseline_treatment)$p.value
check8 <- t.test(log(days_old_account+1) ~ treatment, data=baseline_treatment)$p.value

p_val2 <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check8, 
                                         check9)) |> 
  mutate(p_value = round(p_value, 4))

balance_table2 <- baseline_treatment |> 
  select(n_strong:index_influence, treatment) |> 
  mutate(across(c(n_strong:index_influence), ~log(.x + 1))) |>
  group_by(treatment) |>
  summarise(across(c(n_strong:index_influence), mean)) |> 
  mutate(across(c(n_strong:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(n_strong:index_influence))

treat2 <- balance_table2 |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table2 <- balance_table2 |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat2) |> left_join(p_val2) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table2, caption = "Balance Table Followers - Kenya (Logs)",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))

# Table:


# SA covariates:
#load("SA/03-assignment/input/twitter_followers.Rda")

# SA rand:
twitter <- arrow::read_parquet("SA/03-assignment/input/twitter_followers_filtered_batch2.parquet")
randomization <- read_parquet('SA/04-stratification/integrate/followers_randomized_batch2.parquet')

# Balance:

fitUp <- lm(as.numeric(randomization$treatment)~as.matrix(twitter[,cov0]))
fitNull <- lm(as.numeric(randomization$treatment)~1)
testpUp <- waldtest(fitUp, fitNull, vcov= vcovHC, test = c("F", "Chisq"))[[4]][2]

baseline_treatment <- merge(twitter, randomization, by=id)

check1 <- t.test(n_strong ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(n_weak ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(n_absent ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(n_strong_treated ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(n_weak_treated ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(n_absent_treated ~ treatment, data=baseline_treatment)$p.value
check8 <- t.test(index_influence ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(days_old_account ~ treatment, data=baseline_treatment)$p.value

p_val <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check8, 
                                         check9)) |> 
  mutate(p_value = round(p_value, 4))

balance_table <- baseline_treatment |> 
  select(n_strong:index_influence, treatment) |> group_by(treatment) |>
  summarise(across(c(n_strong:index_influence), mean)) |> 
  mutate(across(c(n_strong:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(n_strong:index_influence))

treat <- balance_table |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table <- balance_table |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat) |> left_join(p_val) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table, caption = "Balance Table Followers - South Africa",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))

### LOGS

baseline_treatment <- merge(twitter, randomization, by=id)

check1 <- t.test(log(n_strong+1) ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(log(n_weak+1) ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(log(n_absent+1) ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(log(n_strong_treated+1) ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(log(n_weak_treated+1) ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(log(n_absent_treated+1) ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(log(index_influence+1) ~ treatment, data=baseline_treatment)$p.value
check8 <- t.test(log(days_old_account+1) ~ treatment, data=baseline_treatment)$p.value

p_val2 <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                          check5, check6, check8, 
                                          check9)) |> 
  mutate(p_value = round(p_value, 4))

balance_table2 <- baseline_treatment |> 
  select(n_strong:index_influence, treatment) |> 
  mutate(across(c(n_strong:index_influence), ~log(.x + 1))) |>
  group_by(treatment) |>
  summarise(across(c(n_strong:index_influence), mean)) |> 
  mutate(across(c(n_strong:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(n_strong:index_influence))

treat2 <- balance_table2 |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table2 <- balance_table2 |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat2) |> left_join(p_val2) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table2, caption = "Balance Table Followers - Kenya (Logs)",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))
