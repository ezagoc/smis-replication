rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(arrow)

# 1.0 Import functions and packages
library(purrr)
src_path <- c("../../../src/utils/")             
source_files <- list(
  "import_data.R"
)
map(paste0(src_path, source_files), source)

# Import datasets with blocks and bind on top:

#
country <- 'SA'
inf1 <- readxl::read_excel(paste0('../../../data/02-randomize/', country, 
                                  '/03-assignment/output/RandomizedTwitterSample',
                                  country, '.xlsx')) |> 
  mutate(pais = country, batch_id = 'b1', 
         block1_fe = paste0(blockid1, country, batch_id))

inf2 <- readxl::read_excel(paste0('../../../data/02-randomize/', country, 
                                  '/03-assignment/output/RandomizedTwitterSample',
                                  country, '_batch2.xlsx')) |> 
  mutate(pais = country, batch_id = 'b2', 
         block1_fe = paste0(blockid1, country, batch_id))

country <- 'KE'

inf3 <- readxl::read_excel(paste0('../../../data/02-randomize/', country, 
                                  '/03-assignment/output/RandomizedTwitterSample',
                                  country, '.xlsx')) |> 
  mutate(pais = country, batch_id = 'b1', 
         block1_fe = paste0(blockid1, country, batch_id))

inf4 <- readxl::read_excel(paste0('../../../data/02-randomize/', country, 
                                  '/03-assignment/output/RandomizedTwitterSample',
                                  country, '_batch2.xlsx')) |> 
  mutate(pais = country, batch_id = 'b2', 
         block1_fe = paste0(blockid1, country, batch_id))

inf <- rbind(inf1, inf2, inf3, inf4)

rm(inf1, inf2, inf3, inf4)

# Final list of analysis:

df <- get_analysis_english_winsor(stage = 'stage1_2', batches = 'b1b2',
                                  initial_path = '../../../') |> 
  select(follower_id, batch_id, pais)

country <- 'SA'

foll1 <- read_parquet(paste0('../../../data/01-characterize/followers/', country, 
                             '/00-raw/integrate/followers.parquet.gzip')) |> 
  select(author_id = author_id_following, follower_id = id) |> 
  mutate(pais = country, batch_id = 'b1')

foll2 <- read_parquet(paste0('../../../data/01-characterize/followers/', country, 
                             '/00-raw/integrate/followers_batch2.parquet.gzip')) |> 
  select(author_id = author_id_following, follower_id = id) |> 
  mutate(pais = country, batch_id = 'b2')

country <- 'KE'

foll3 <- read_parquet(paste0('../../../data/01-characterize/followers/', country, 
                             '/00-raw/integrate/followers.parquet.gzip')) |> 
  select(author_id = author_id_following, follower_id = id)  |> 
  mutate(pais = country, batch_id = 'b1')

foll4 <- read_parquet(paste0('../../../data/01-characterize/followers/', country, 
                             '/00-raw/integrate/followers_batch2.parquet.gzip')) |> 
  select(author_id = author_id_following, follower_id = id)  |> 
  mutate(pais = country, batch_id = 'b2')

foll <- rbind(foll1, foll2, foll3, foll4)

rm(foll1, foll2, foll3, foll4)

foll <- foll |> left_join(df, by = c('follower_id', 'pais', 'batch_id')) |> 
  filter(is.na(d) == F)

# Merge

foll <- foll |> left_join(inf |> select(author_id, block1_fe)) 

#write_parquet(foll |> select(-c(d, block1_fe)), '../../../data/04-analysis/joint/BlocksIntensive/followers_filtered.parquet')

foll1 <- foll |> mutate(id = paste0(follower_id, '-', batch_id, '-', pais)) |>
  select(id, block1_fe)

# Convert to wide format
wide_f <- foll1 %>% mutate(value = 1) |>
  pivot_wider(
    names_from = block1_fe,
    values_from = value,
    values_fill = list(value = 0),
    values_fn = list(value = max)
  )

wide_f[is.na(wide_f)] <- 0

df1 <- df |> mutate(id = paste0(follower_id, '-', batch_id, '-', pais)) |>
  left_join(wide_f, by = 'id') |> select(-id)

df1 <- df1 |>
  rename_at(vars(-all_of(c('follower_id', 'pais', 'batch_id'))), 
            ~paste0("fe_", .))

write_parquet(df1,
              '../../../data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet')


# Get the formula:

cls <- colnames(df1 |> select(-c(follower_id, batch_id, pais)))

cls <- paste0(cls, collapse = ' + ')

cls # saved in constants final