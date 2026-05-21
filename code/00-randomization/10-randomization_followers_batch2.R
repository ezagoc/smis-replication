######################## Followers ########################

### Code to Stratify by quartiles, terciles, or deciles a big data frame into smaller ones
### and then run the block randomization (if possible) in the smaller data sets.

### 2ND BATCH ###

# General Configurations ----------------------------------------------------
rm(list = ls()) # Clean out elements in R environment
setwd("../../data/randomization/")
source("../../code/00-randomization/funcs.R")

# Install and load packages
#install.packages('RTools')
#url <- "https://cran.r-project.org/src/contrib/Archive/blockTools/blockTools_0.6-3.tar.gz"
#pkgFile <- "blockTools_0.6-3.tar.gz"
#download.file(url = url, destfile = pkgFile)
#install.packages(pkgs=pkgFile, type="source", repos=NULL)
#unlink(pkgFile)
#install.packages('nbpMatching')

packages <- c(
  "foreign",
  "dplyr",
  "tidyverse",
  "blockTools"
)
library(nbpMatching)
library(arrow)

ipak(packages)

### General functions: ---------------------

stratify_df <- function(id_dist, df){
  df_filtered <- df |> filter(id == id_dist)
  df_filtered
}

block_rand <- function(df){
  Sys.sleep(5)
  id_rand_print <- df$id[1]
  print(id_rand_print)
  
  rand <- tryCatch({
    block0 <- block(
      df,
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
      df,
      id.var = id
    )
    
    df <- cbind(df, blockid1)
    
    block0 <- block(
      df,
      n.tr = 2,
      id.vars = c(id),
      block.vars = cov0,
      algorithm = "optimal",
      distance = "mahalanobis",
      groups = "blockid1",
      valid.range = c(0, 500),
      verbose = F
    )
    
    blockid2 <- createBlockIDs(block0, df, id.var = id)
    
    combined <- cbind(df, blockid2)
    
    # Randomize treatment assigment
    
    cluster_assign0 <- assignment(block0, seed = 1234)
    
    treatments0 <- treatments1 <- c()
    for (i in unique(df$blockid1)) {
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
      id,
      "blockid1",
      "blockid2",
      "treatment",
      "id"
    )
    
    randomization1 <- randomization[, cols_twitter]
    randomization1
    print('Block Rand.')
    id_rand <- randomization1$id[1]
    write_parquet(randomization1, 
                  paste0(country,'/04-stratification/collect_batch2/', 
                         id_rand, '.parquet'))
    
  },
  error=function(cond) {
    df <- df |> mutate(rand = runif(n(), 0,1), 
                       treatment = case_when(rank(rand) < .5*n() ~ 1,
                                             TRUE ~ 0)) |> mutate(blockid1 = NA,
                                                                  blockid2 = NA)
    
    cols_twitter <- c(
      "username",
      id,
      "blockid1",
      "blockid2",
      "treatment",
      "id"
    )
    
    df <- df[, cols_twitter]
    id_rand <- df$id[1]
    write_parquet(df, paste0(country,'/04-stratification/collect_batch2/', 
                             id_rand,'.parquet'))
    print(cond)
    return(NA)
  })
  
  print('Rand Success')
}


############### Preparing the block code: ------------------

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






# KE ----------------

# Read in the data to randomize 

twitter <- arrow::read_parquet("KE/03-assignment/input/twitter_followers_filtered_batch2.parquet")

country = 'KE'
#load("KE/03-assignment/input/twitter_followers.Rda")

# Creating the tercile ids
# Merging with the original data set

twitter_median <- twitter |> 
  left_join(twitter |> select(-c(username)) |> 
              mutate(across(c(n_strong:index_influence), ~ntile(.x, 2))) |>
              mutate(id = paste0(n_strong, n_weak, n_absent,
                                 n_strong_treated, n_weak_treated, 
                                 n_absent, days_old_account, index_influence)) |>
              rename_with(~paste0('median_', .x))  |> 
              rename(follower_id = median_follower_id, id = median_id),
            by = "follower_id")

median_ids <- twitter_median |> select(id) |> distinct() |> as_vector()

#tercile_id_df <- twitter_tercile |> select(id) |> group_by(id) |> 
#summarise(n = n()) |> filter(n > 7)

#write_parquet(tercile_id_df, 'KE/04-stratification/id/ids_strat.parquet')

# Running the loops to obtain the different df for each combination:

list_median <-  median_ids |> map(~stratify_df(.x, twitter_median))

## DFs with more than 8 observations (the R session aborted for the ones with less):
list_median_1 <- list_median %>% keep(~ nrow(.x) < 20000 & nrow(.x) > 7)
lapply(list_median_1, block_rand)

lapply(list_median %>% keep(~nrow(.x) == 7), block_rand)
lapply(list_median %>% keep(~nrow(.x) == 6), block_rand)
lapply(list_median %>% keep(~nrow(.x) == 5), block_rand)
lapply(list_median %>% keep(~nrow(.x) == 4), block_rand)

# We pool together again the ones that are left:

df2 <- do.call(rbind.data.frame, list_median %>% keep(~ nrow(.x) < 4))
# Now we pool by median:

df2 <- df2 |> mutate(id = '21212211a')

df2 <- block_rand(df2)

############# SA ----------------

# Read in the data to randomize 

#load("SA/03-assignment/input/twitter_followers.Rda")
twitter <- arrow::read_parquet("SA/03-assignment/input/twitter_followers_filtered_batch2.parquet")
country = 'SA'
# Creating the median, quartile and decile id s

twitter <- block_rand(twitter)
# Merging with the original data set

twitter_median <- twitter |> 
  left_join(twitter |> select(-c(username)) |> 
              mutate(across(c(n_strong:index_influence), ~ntile(.x, 2))) |>
              mutate(id = paste0(n_strong, n_weak, n_absent,
                                 n_strong_treated, n_weak_treated, 
                                 n_absent, days_old_account, index_influence)) |>
              rename_with(~paste0('median_', .x))  |> 
              rename(follower_id = median_follower_id, id = median_id),
            by = "follower_id")

median_ids <- twitter_median |> select(id) |> distinct() |> as_vector()

#tercile_id_df <- twitter_tercile |> select(id) |> group_by(id) |> 
#summarise(n = n()) |> filter(n > 7)

#write_parquet(tercile_id_df, 'KE/04-stratification/id/ids_strat.parquet')

# Running the loops to obtain the different df for each combination:

list_median <-  median_ids |> map(~stratify_df(.x, twitter_median))

## DFs with more than 8 observations (the R session aborted for the ones with less):
list_median_1 <- list_median %>% keep(~ nrow(.x) < 20000 & nrow(.x) > 7)
lapply(list_median_1, block_rand)

lapply(list_median %>% keep(~nrow(.x) == 6), block_rand)
lapply(list_median %>% keep(~nrow(.x) == 5), block_rand)
lapply(list_median %>% keep(~nrow(.x) == 4), block_rand)

df2 <- do.call(rbind.data.frame, list_median %>% keep(~ nrow(.x) < 4))
# Now we pool by median:

df2 <- df2 |> mutate(id = '222222222a')

df2 <- block_rand(df2)
