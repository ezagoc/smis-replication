
#############################################################################
# RCT: Social Media Influencers Based in Africa
# Activity: Permutation of Assignments in Randomization Within Blocks (2,4)
# Date: April 17th 2023
#############################################################################

######################## Influencers ########################

# General Configurations ----------------------------------------------------
rm(list = ls()) # Clean out elements in R environment
setwd("../../data/")
source("../src/utils/funcs.R")

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


id <- "author_id"

# Read in the data to randomize ----------------

for (country in c("KE", "SA")) {
    #setwd(paste0("../../../data/02-randomize/", country))
    load(paste0('02-randomize/',country,"/03-assignment/input/twitter.Rda"))

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

    df <- list()

    for (seed in seq(1, 1000)) {
        cluster_assign0 <- assignment(block0, seed = seed)

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

        randomization <- as.data.frame(merge(combined, assignments, id = id))
        cols_twitter <- c(
            id,
            "blockid1",
            "blockid2",
            "treatment"
        )

        randomization <- randomization[, cols_twitter]
        colnames(randomization) <- c(
            "author_id",
            paste0("blockid1_p", seed),
            paste0("blockid2_p", seed),
            paste0("treatment_p", seed)
        )

        df[[seed]] <- randomization
    }

    df <- df %>% reduce(left_join, by = id)
    name <- paste0(
        "04-analysis/",country,"/assignments_permutations.xlsx"
    )
    write_xlsx(
        df,
        path = name
    )
}


#BATCH2

for (country in c("KE", "SA")) {
  #setwd(paste0("../../../data/02-randomize/", country))
  load(paste0('02-randomize/',country,"/03-assignment/input/twitter_batch2.Rda"))
  
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
  
  df <- list()
  
  for (seed in seq(1, 1000)) {
    cluster_assign0 <- assignment(block0, seed = seed)
    
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
    
    randomization <- as.data.frame(merge(combined, assignments, id = id))
    cols_twitter <- c(
      id,
      "blockid1",
      "blockid2",
      "treatment"
    )
    
    randomization <- randomization[, cols_twitter]
    colnames(randomization) <- c(
      "author_id",
      paste0("blockid1_p", seed),
      paste0("blockid2_p", seed),
      paste0("treatment_p", seed)
    )
    
    df[[seed]] <- randomization
  }
  
  df <- df %>% reduce(left_join, by = id)
  name <- paste0(
    "04-analysis/",country,"/assignments_permutations_batch2.xlsx"
  )
  write_xlsx(
    df,
    path = name
  )
}

