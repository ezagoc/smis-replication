#############################################################################
# RCT: Social Media Influencers Based in Africa
# Activity: Randomnization Within Blocks (2,4)
# Date: November 2022
#############################################################################

######################## Influencers ########################

# General Configurations ----------------------------------------------------
rm(list = ls()) # Clean out elements in R environment
setwd("../../data/randomization/")
source("../../code/00-randomization/funcs.R")

# Install and load packages

#url <- "https://cran.r-project.org/src/contrib/Archive/blockTools/blockTools_0.6-3.tar.gz"
#pkgFile <- "blockTools_0.6-3.tar.gz"
#download.file(url = url, destfile = pkgFile)
#install.packages(pkgs=pkgFile, type="source", repos=NULL)
#unlink(pkgFile)

packages <- c(
  "foreign",
  "dplyr",
  "tidyverse",
  "blockTools",
  "writexl",
  "lmtest",
  "sandwich"
)

ipak(packages)
library(arrow)

randomization <- readxl::read_excel("KE/03-assignment/output/RandomizedTwitterSampleKE_batch2.xlsx")
# Check balance

fitUp <- lm(as.numeric(randomization$treatment)~as.matrix(twitter[,cov0]))
fitNull <- lm(as.numeric(randomization$treatment)~1)
testpUp <- waldtest(fitUp, fitNull, vcov= vcovHC, test = c("F", "Chisq"))[[4]][2]

baseline_treatment <- merge(twitter, randomization, by=id)

check1 <- t.test(followers_count ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(listed_count ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(n_tweets ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(n_tweets.na ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(n_strong ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(n_weak ~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(n_absent ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(index_influence ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(days_old_account ~ treatment, data=baseline_treatment)$p.value

p_val <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check7, 
                                         check9, check10))

balance_table <- baseline_treatment |> 
  select(followers_count:index_influence, treatment) |> group_by(treatment) |>
  summarise(across(c(followers_count:index_influence), mean)) |> 
  mutate(across(c(followers_count:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(followers_count:index_influence))

treat <- balance_table |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table <- balance_table |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat) |> left_join(p_val) |> 
  rename(Covariates = name)


library(kableExtra)

kbl(balance_table, caption = "Balance Table Influencers - Kenya",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))

# Logs:

check1 <- t.test(log(followers_count + 1)~ treatment, 
                 data=baseline_treatment)$p.value
check2 <- t.test(log(listed_count+1) ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(log(n_tweets+1) ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(log(n_tweets.na + 1)~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(log(n_strong+1) ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(log(n_weak + 1)~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(log(n_absent+1) ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(log(index_influence+1) ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(log(days_old_account+1) ~ treatment, data=baseline_treatment)$p.value

p_val <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check7, 
                                         check9, check10))

balance_table2 <- baseline_treatment |> 
  select(followers_count:index_influence, treatment) |> 
  mutate(across(c(followers_count:index_influence), ~log(.x + 1))) |>
  group_by(treatment) |>
  summarise(across(c(followers_count:index_influence), mean)) |> 
  mutate(across(c(followers_count:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(followers_count:index_influence))

treat2 <- balance_table2 |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table2 <- balance_table2 |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat2) |> left_join(p_val) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table2, caption = "Balance Table Influencers - Kenya (Logs)",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))


randomization <- readxl::read_excel("SA/03-assignment/output/RandomizedTwitterSampleSA.xlsx")

# Check balance

fitUp <- lm(as.numeric(randomization$treatment)~as.matrix(twitter[,cov0]))
fitNull <- lm(as.numeric(randomization$treatment)~1)
testpUp <- waldtest(fitUp, fitNull, vcov= vcovHC, test = c("F", "Chisq"))[[4]][2]

baseline_treatment <- merge(twitter, randomization, by=id)

check1 <- t.test(followers_count ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(listed_count ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(n_tweets ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(n_tweets.na ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(n_strong ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(n_weak ~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(n_absent ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(index_influence ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(days_old_account ~ treatment, data=baseline_treatment)$p.value

p_val <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check7, 
                                         check9, check10))

balance_table <- baseline_treatment |> 
  select(followers_count:index_influence, treatment) |> group_by(treatment) |>
  summarise(across(c(followers_count:index_influence), mean)) |> 
  mutate(across(c(followers_count:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(followers_count:index_influence))

treat <- balance_table |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table <- balance_table |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat) |> left_join(p_val) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table, caption = "Balance Table Influencers - South Africa",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))

##### 

check1 <- t.test(log(followers_count + 1)~ treatment, 
                 data=baseline_treatment)$p.value
check2 <- t.test(log(listed_count+1) ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(log(n_tweets+1) ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(log(n_tweets.na + 1)~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(log(n_strong+1) ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(log(n_weak + 1)~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(log(n_absent+1) ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(log(index_influence+1) ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(log(days_old_account+1) ~ treatment, data=baseline_treatment)$p.value

p_val <- tibble(name = cov0, p_value = c(check1, check2, check3, check4, 
                                         check5, check6, check7, 
                                         check9, check10))

balance_table2 <- baseline_treatment |> 
  select(followers_count:index_influence, treatment) |> 
  mutate(across(c(followers_count:index_influence), ~log(.x + 1))) |>
  group_by(treatment) |>
  summarise(across(c(followers_count:index_influence), mean)) |> 
  mutate(across(c(followers_count:index_influence), ~round(.x, 4))) |> 
  pivot_longer(cols = c(followers_count:index_influence))

treat2 <- balance_table2 |> filter(treatment == 1) |> rename(Treated = value) |> 
  select(-treatment)

balance_table2 <- balance_table2 |> filter(treatment == 0) |> rename(Control = value) |> 
  select(-treatment) |> left_join(treat2) |> left_join(p_val) |> 
  rename(Covariates = name)

library(kableExtra)

kbl(balance_table2, caption = "Balance Table Influencers - Kenya (Logs)",  booktabs = T, 
    format = 'latex') %>%
  kable_styling(latex_options = c("hold_position"))


### KE B1

randomization <- readxl::read_excel("KE/03-assignment/output/RandomizedTwitterSampleKE.xlsx")

# Check balance

fitUp <- lm(as.numeric(randomization$treatment)~as.matrix(twitter[,cov0]))
fitNull <- lm(as.numeric(randomization$treatment)~1)
testpUp <- waldtest(fitUp, fitNull, vcov= vcovHC, test = c("F", "Chisq"))[[4]][2]

baseline_treatment <- merge(combined, randomization, by=id)

check1 <- t.test(followers_count ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(listed_count ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(n_tweets ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(n_tweets.na ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(n_strong ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(n_weak ~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(n_absent ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(index_influence ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(days_old_account ~ treatment, data=baseline_treatment)$p.value

### SA B1

randomization <- readxl::read_excel("SA/03-assignment/output/RandomizedTwitterSampleSA.xlsx")

# Check balance

fitUp <- lm(as.numeric(randomization$treatment)~as.matrix(twitter[,cov0]))
fitNull <- lm(as.numeric(randomization$treatment)~1)
testpUp <- waldtest(fitUp, fitNull, vcov= vcovHC, test = c("F", "Chisq"))[[4]][2]

baseline_treatment <- merge(combined, randomization, by=id)

check1 <- t.test(followers_count ~ treatment, data=baseline_treatment)$p.value
check2 <- t.test(listed_count ~ treatment, data=baseline_treatment)$p.value
check3 <- t.test(n_tweets ~ treatment, data=baseline_treatment)$p.value
check4 <- t.test(n_tweets.na ~ treatment, data=baseline_treatment)$p.value
check5 <- t.test(n_strong ~ treatment, data=baseline_treatment)$p.value
check6 <- t.test(n_weak ~ treatment, data=baseline_treatment)$p.value
check7 <- t.test(n_absent ~ treatment, data=baseline_treatment)$p.value
check9 <- t.test(index_influence ~ treatment, data=baseline_treatment)$p.value
check10 <- t.test(days_old_account ~ treatment, data=baseline_treatment)$p.value