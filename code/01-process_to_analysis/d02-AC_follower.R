rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyverse, purrr)
src_path <- c("../../../src/utils/")             
source_files <- list(
  "import_data.R"
)
map(paste0(src_path, source_files), source)
`%!in%` = Negate(`%in%`)

# ORIGINAL ---------------------------------------------
stage <- 'AC'
for (country in c('KE', 'SA')){
  df <- get_analysis_ver_final_winsor(stage = 'stage1_2', batches = 'b1b2', 
                                      initial_path = '../../../') |> 
    filter(batch_id == 'b1') |> filter(pais == country) |>
    select(username:t_neither, pais)
  
  followers_base1 <- read_parquet('../../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-02-03/collect/625489039.parquet') 
  followers_base2 <- read_parquet('../../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-02-03/collect/1468955884092936200.parquet') 
  
  followers_base <- rbind(followers_base1 |> select(id, username),
                          followers_base2 |> select(id, username)) |> 
    mutate(AC_base = 1) |> rename(follower_id = id)
  
  followers_base <- followers_base[!duplicated(followers_base$follower_id), ]
  
  followers1 <- read_parquet('../../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-02-03/collect/625489039.parquet') 
  followers2 <- read_parquet('../../../../social-media-influencers-africa/data/07-followers/AfricaCheck/2023-06-13/collect/1468955884092936200.parquet') 
  
  followers <- rbind(followers1 |> select(id, username),
                     followers2 |> select(id, username)) |> 
    mutate(AC = 1) |> rename(follower_id = id)
  
  followers <- followers[!duplicated(followers$follower_id), ]
  
  df <- df |> left_join(followers, by = c('follower_id', 'username')) |> 
    left_join(followers_base, by = c('follower_id', 'username'))
  
  df <- df |> mutate(AC = ifelse(is.na(AC) == T, 0, AC),
                     AC_base = ifelse(is.na(AC_base) == T, 0, AC_base))
  
  write_parquet(df, paste0('../../../data/04-analysis/',country,'/', stage,
                           '/AC_final.parquet'))
}

# Read df (Aggregated Data Set)
