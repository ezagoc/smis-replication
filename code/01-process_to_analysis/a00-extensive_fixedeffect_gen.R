rm(list = ls())
library(tidyverse)
library(arrow)

# Import data sets:

#country <- 'KE'
country <- 'SA'
# b1

foll1 <- read_parquet(paste0('../../../data/01-characterize/followers/', country, 
                             '/00-raw/integrate/followers.parquet.gzip')) |> 
  select(author_id = author_id_following, follower_id = id) |> 
  group_by(follower_id) |> mutate(n_count = n()) |> ungroup() |> filter(n_count == 1) |>
  select(-n_count)

inf1 <- readxl::read_excel(paste0('../../../data/02-randomize/', country, 
                                  '/03-assignment/output/RandomizedTwitterSample',
                                  country, '.xlsx'))

rand1 <- read_parquet(paste0('../../../data/02-randomize/', country, 
                             '/04-stratification/integrate/followers_randomized.parquet')) |>
  select(follower_id) |> mutate(dummy_final = 1)

foll1 <- foll1 |> left_join(rand1) |> filter(dummy_final == 1) |> 
  select(-dummy_final) |> left_join(inf1) |> 
  mutate(pais = country, batch_id = 'b1')

############### b2

foll2 <- read_parquet(paste0('../../../data/01-characterize/followers/', country, 
                             '/00-raw/integrate/followers_batch2.parquet.gzip')) |> 
  select(author_id = author_id_following, follower_id = id) |> 
  group_by(follower_id) |> mutate(n_count = n()) |> ungroup() |> filter(n_count == 1) |>
  select(-n_count)

inf2 <- readxl::read_excel(paste0('../../../data/02-randomize/', country, 
                                  '/03-assignment/output/RandomizedTwitterSample',
                                  country, '_batch2.xlsx'))

rand2 <- read_parquet(paste0('../../../data/02-randomize/', country, 
                             '/04-stratification/integrate/followers_randomized_batch2.parquet')) |>
  select(follower_id) |> mutate(dummy_final = 1)

foll2 <- foll2 |> left_join(rand2) |> filter(dummy_final == 1) |> 
  select(-dummy_final) |> left_join(inf2) |> 
  mutate(pais = country, batch_id = 'b2')

################## p

follp <- read_parquet(paste0('../../../../social-media-influencers-africa/data/01-characterize/followers/',
                             country, '/00-raw/integrate/followers.gzip')) |> 
  select(author_id = author_id_following, follower_id = id) |> 
  group_by(follower_id) |> mutate(n_count = n()) |> ungroup() |> filter(n_count == 1) |>
  select(-n_count)

infp <- readxl::read_excel(paste0('../../../../social-media-influencers-africa/data/02-randomize/',
                                  country, '/03-assignment/output/RandomizedTwitterSample',
                                  country, '.xlsx'))

randp <- read_parquet(paste0('../../../../social-media-influencers-africa/data/02-randomize/', country, 
                             '/04-stratification/integrate/followers_randomized.parquet')) |>
  select(follower_id) |> mutate(dummy_final = 1)

follp <- follp |> left_join(randp) |> filter(dummy_final == 1) |> 
  select(-dummy_final) |> left_join(infp) |> 
  mutate(pais = country, batch_id = 'p')

## Concat all:

foll_final <- rbind(foll1, foll2, follp)

# Generate the block identifier 

foll_final <- foll_final |> mutate(block1_fe = paste0(pais,'-',batch_id, '-', 
                                                      blockid1),
                                   block2_fe = paste0(pais,'-',batch_id, '-', 
                                                      blockid2))
write_parquet(foll_final, 
              paste0('../../../data/04-analysis/', country, 
                     '/extensive_fixed_effects.parquet'))
