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

###KE

# Read in the data to randomize ----------------

# BATCH 1 KE

load("KE/03-assignment/input/twitter.Rda")

# List covariates for determining blocks -------

cov0 <- c(
  "followers_count",
  "listed_count",
  "n_tweets.na",
  "n_tweets",
  "n_strong",
  "n_weak",
  "n_absent",
  "days_old_account",
  "index_influence"
)

##################### 1. TWITTER

id <- "author_id"

# Determine level-1 and level-2 blocks
# of sizes 4 and 2 within each group

block0 <- block(
  twitter,
  n.tr = 4,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optGreedy",
  distance = "mahalanobis",
  valid.range = c(0, 500),
  verbose = F
  )

blockid1 <- createBlockIDs(
  block0,
  twitter,
  id.var = id
  )

twitter <- cbind(twitter, blockid1)

block0 <- block(
  twitter,
  n.tr = 2,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optimal",
  distance = "mahalanobis",
  groups = "blockid1",
  valid.range = c(0, 500),
  verbose = F
  )

blockid2 <- createBlockIDs(block0, twitter, id.var = id)

combined <- cbind(twitter, blockid2)

# Randomize treatment assigment

cluster_assign0 <- assignment(block0, seed = 12345)

treatments0 <- treatments1 <- c()
for (i in unique(twitter$blockid1)) {
  end <- eval(parse(text = paste("cluster_assign0$assg$`", i, "`", sep = "")))
  treatments0 <- rbind(treatments0, end)
}

treatments <- rbind(treatments0, treatments1)

assignments <- rbind(
  cbind(as.vector(treatments[, 1]), 0),
  cbind(as.vector(treatments[, 2]), 1)
  )

assignments <- na.omit(assignments)

colnames(assignments) <- c(id, "treatment")

# Join everything

randomization <- as.data.frame(merge(combined, assignments, id = id))

cols_twitter <- c(
  "username",
  "name",
  id,
  "blockid1",
  "blockid2",
  "treatment"
  )

randomization <- randomization[, cols_twitter]

write_xlsx(
  randomization, 
  "KE/03-assignment/output/RandomizedTwitterSampleKE.xlsx"
  )

#################################################################################
### SA: 
#################################################################################

load("SA/03-assignment/input/twitter.Rda")

# List covariates for determining blocks -------

cov0 <- c(
  "followers_count",
  "listed_count",
  "n_tweets.na",
  "n_tweets",
  "n_strong",
  "n_weak",
  "n_absent",
  "days_old_account",
  "index_influence"
)

##################### 1. TWITTER

id <- "author_id"

# Determine level-1 and level-2 blocks
# of sizes 4 and 2 within each group

block0 <- block(
  twitter,
  n.tr = 4,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optGreedy",
  distance = "mahalanobis",
  valid.range = c(0, 500),
  verbose = F
)

blockid1 <- createBlockIDs(
  block0,
  twitter,
  id.var = id
)

twitter <- cbind(twitter, blockid1)

block0 <- block(
  twitter,
  n.tr = 2,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optimal",
  distance = "mahalanobis",
  groups = "blockid1",
  valid.range = c(0, 500),
  verbose = F
)

blockid2 <- createBlockIDs(block0, twitter, id.var = id)

combined <- cbind(twitter, blockid2)

# Randomize treatment assigment

cluster_assign0 <- assignment(block0, seed = 12345)

treatments0 <- treatments1 <- c()
for (i in unique(twitter$blockid1)) {
  end <- eval(parse(text = paste("cluster_assign0$assg$`", i, "`", sep = "")))
  treatments0 <- rbind(treatments0, end)
}

treatments <- rbind(treatments0, treatments1)

assignments <- rbind(
  cbind(as.vector(treatments[, 1]), 0),
  cbind(as.vector(treatments[, 2]), 1)
)

assignments <- na.omit(assignments)

colnames(assignments) <- c(id, "treatment")

# Join everything

randomization <- as.data.frame(merge(combined, assignments, id = id))

cols_twitter <- c(
  "username",
  "name",
  id,
  "blockid1",
  "blockid2",
  "treatment"
)

randomization <- randomization[, cols_twitter]

write_xlsx(
  randomization, 
  "SA/03-assignment/output/RandomizedTwitterSampleSA.xlsx"
)

#################################################################################
############################# Batch 2 ##########################################
################################################################################


######################## KE ###################################################
load("KE/03-assignment/input/twitter_batch2.Rda")

# List covariates for determining blocks -------

cov0 <- c(
  "followers_count",
  "listed_count",
  "n_tweets.na",
  "n_tweets",
  "n_strong",
  "n_weak",
  "n_absent",
  "days_old_account",
  "index_influence"
)

##################### 1. TWITTER

id <- "author_id"

# Determine level-1 and level-2 blocks
# of sizes 4 and 2 within each group

block0 <- block(
  twitter,
  n.tr = 4,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optGreedy",
  distance = "mahalanobis",
  valid.range = c(0, 500),
  verbose = F
)

blockid1 <- createBlockIDs(
  block0,
  twitter,
  id.var = id
)

twitter <- cbind(twitter, blockid1)

block0 <- block(
  twitter,
  n.tr = 2,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optimal",
  distance = "mahalanobis",
  groups = "blockid1",
  valid.range = c(0, 500),
  verbose = F
)

blockid2 <- createBlockIDs(block0, twitter, id.var = id)

combined <- cbind(twitter, blockid2)

# Second KE
cluster_assign0 <- assignment(block0, seed = 1253)

treatments0 <- treatments1 <- c()
for (i in unique(twitter$blockid1)) {
  end <- eval(parse(text = paste("cluster_assign0$assg$`", i, "`", sep = "")))
  treatments0 <- rbind(treatments0, end)
}

treatments <- rbind(treatments0, treatments1)

assignments <- rbind(
  cbind(as.vector(treatments[, 1]), 0),
  cbind(as.vector(treatments[, 2]), 1)
)

assignments <- na.omit(assignments)

colnames(assignments) <- c(id, "treatment")

# Join everything

randomization <- as.data.frame(merge(combined, assignments, id = id))

cols_twitter <- c(
  "username",
  "name",
  id,
  "blockid1",
  "blockid2",
  "treatment"
)

randomization <- randomization[, cols_twitter]

write_xlsx(
  randomization, 
  "KE/03-assignment/output/RandomizedTwitterSampleKE_batch2.xlsx"
)

#################################################################################
################################ SA #############################################
#################################################################################

# Read in the data to randomize ----------------

load("SA/03-assignment/input/twitter_batch2.Rda")

# List covariates for determining blocks -------

cov0 <- c(
  "followers_count",
  "listed_count",
  "n_tweets.na",
  "n_tweets",
  "n_strong",
  "n_weak",
  "n_absent",
  "days_old_account",
  "index_influence"
)

##################### 1. TWITTER

id <- "author_id"

# Determine level-1 and level-2 blocks
# of sizes 4 and 2 within each group

block0 <- block(
  twitter,
  n.tr = 4,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optGreedy",
  distance = "mahalanobis",
  valid.range = c(0, 500),
  verbose = F
)

blockid1 <- createBlockIDs(
  block0,
  twitter,
  id.var = id
)

twitter <- cbind(twitter, blockid1)

block0 <- block(
  twitter,
  n.tr = 2,
  id.vars = c(id),
  block.vars = cov0,
  algorithm = "optimal",
  distance = "mahalanobis",
  groups = "blockid1",
  valid.range = c(0, 500),
  verbose = F
)

blockid2 <- createBlockIDs(block0, twitter, id.var = id)

combined <- cbind(twitter, blockid2)

# Randomize treatment assigment

# Seed 2nd batch

cluster_assign0 <- assignment(block0, seed = 1239)
treatments0 <- treatments1 <- c()
for (i in unique(twitter$blockid1)) {
  end <- eval(parse(text = paste("cluster_assign0$assg$`", i, "`", sep = "")))
  treatments0 <- rbind(treatments0, end)
}

treatments <- rbind(treatments0, treatments1)

assignments <- rbind(
  cbind(as.vector(treatments[, 1]), 0),
  cbind(as.vector(treatments[, 2]), 1)
)

assignments <- na.omit(assignments)

colnames(assignments) <- c(id, "treatment")

# Join everything

randomization <- as.data.frame(merge(combined, assignments, id = id))

cols_twitter <- c(
  "username",
  "name",
  id,
  "blockid1",
  "blockid2",
  "treatment"
)

randomization <- randomization[, cols_twitter]

write_xlsx(
  randomization, 
  "SA/03-assignment/output/RandomizedTwitterSampleSA_batch2.xlsx"
)


