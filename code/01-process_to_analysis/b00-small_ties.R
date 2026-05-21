# COUNTRY TIE GENERATIONS

rm(list = ls())
library(lfe)
library(fixest)
library(purrr)
src_path <- c("../../src/utils/")             
source_files <- list(
  "funcs.R",
  "constants_balance.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

# Define round and set of dependent variables:
country <- 'joint'

# Functions:
small_ties <- function(i){
  df <- followers |> 
    select(follower_id:paste0("n_influencers_followed_p_", i), pais)
  
  write_parquet(df, paste0("../../data/04-analysis/",country, "/",
                           stage, "/small_tie", 
                           i,".parquet"))
}

small_ties2 <- function(i){
  df <- followers |> 
    select(follower_id,
           paste0("n_influencers_followed_p_", i-1):paste0("n_influencers_followed_p_", i),
           pais) |>
    select(-c(paste0("n_influencers_followed_p_", i-1)))
  
  write_parquet(df, paste0("../../data/04-analysis/",country, "/",
                           stage, "/small_tie", 
                           i,".parquet"))
}

small_ties3 <- function(i){
  df <- followers |> 
    select(follower_id:paste0("n_influencers_followed_p_", i), batch_id, pais)
  
  write_parquet(df, paste0("../../data/04-analysis/",country, "/",
                           stage, "/small_tie", 
                           i,".parquet"))
}

small_ties4 <- function(i){
  df <- followers |> 
    select(follower_id, batch_id, pais, 
           paste0("n_influencers_followed_p_", i-1):paste0("n_influencers_followed_p_", i)) |>
    select(-c(paste0("n_influencers_followed_p_", i-1)))
  
  write_parquet(df, paste0("../../data/04-analysis/",country, "/",
                           stage, "/small_tie", 
                           i,".parquet"))
}

stage <- 'small_ties'

###########
## BATCH1

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties",0,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties",0,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA')) 

c(1) |> map(~small_ties(.x))

c(2:250) |> map(~small_ties2(.x))

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties",1,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties",1,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA')) 

251 |> map(~small_ties(.x))

c(252:500) |> map(~small_ties2(.x))

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties",2,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties",2,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA'))

501 |> map(~small_ties(.x))

c(502:750) |> map(~small_ties2(.x))

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties",3,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties",3,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA'))

751 |> map(~small_ties(.x))

c(752:1000) |> map(~small_ties2(.x))


######
stage <- 'small_ties_b1b2'
############

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties_batch1_batch2_",0,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties_batch1_batch2_",0,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA'))

c(1) |> map(~small_ties3(.x))

c(2:250) |> map(~small_ties4(.x))

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties_batch1_batch2_",1,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties_batch1_batch2_",1,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA'))
251 |> map(~small_ties3(.x))

c(252:500) |> map(~small_ties4(.x))

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties_batch1_batch2_",2,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties_batch1_batch2_",2,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA'))

501 |> map(~small_ties3(.x))

c(502:750) |> map(~small_ties4(.x))

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties_batch1_batch2_",3,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties_batch1_batch2_",3,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA'))
751 |> map(~small_ties3(.x))

c(752:1000) |> map(~small_ties4(.x))


stage <- 'small_ties_b1b2p'
############

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties_batch1_batch2_p_",0,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties_batch1_batch2_p_",0,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA')) 

c(1) |> map(~small_ties3(.x))

c(2:250) |> map(~small_ties4(.x))

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties_batch1_batch2_p_",1,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties_batch1_batch2_p_",1,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA'))

251 |> map(~small_ties3(.x))

c(252:500) |> map(~small_ties4(.x))

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties_batch1_batch2_p_",2,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties_batch1_batch2_p_",2,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA')) 
501 |> map(~small_ties3(.x))

c(502:750) |> map(~small_ties4(.x))

followers <- rbind(read_parquet(paste0("../../data/04-analysis/KE/ties_batch1_batch2_p_",3,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'KE'),
                   read_parquet(paste0("../../data/04-analysis/SA/ties_batch1_batch2_p_",3,
                                       ".parquet"), as_tibble = TRUE) |> 
                     mutate(pais = 'SA'))

751 |> map(~small_ties3(.x))

c(752:1000) |> map(~small_ties4(.x))