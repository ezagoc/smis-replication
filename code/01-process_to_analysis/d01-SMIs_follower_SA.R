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

country <- 'SA'
stage <- 'SMIs'

## Paths

path_ids <-  paste0('../../../data/02-randomize/', country, 
                    '/03-assignment/output/RandomizedTwitterSample',country, '.xlsx')
path_out <- paste0('../../../data/07-followers/',country,
                   '/batch1/2023-06-13/collect/')

# Read df (Aggregated Data Set):

df <- get_analysis_ver_final_winsor(stage = 'stage1_2', batches = 'b1b2', 
                                    initial_path = '../../../') |> 
  filter(batch_id == 'b1') |> filter(pais == country) |>
  select(follower_id, pais)

# Read Followers at baseline:
followers_base <- read_parquet(paste0('../../../data/07-followers/', country,
                                      '/batch1/2023-02-20/integrate/followers.parquet.gzip'))


# Read the file of ids:

data <- readxl::read_xlsx(path_ids)

ids <- data$author_id

onlyfiles <- list.files(path_out)
onlyfiles <- gsub('.parquet', '', onlyfiles)
ids_final <- setdiff(unique(ids), onlyfiles)
ids_final <- sort(ids_final)

# Filter the followers at baseline that follow the influencers that got accounts suspended:

followers_cancelled <- followers_base |> 
  select(author_id_following, follower_id = id) |> 
  filter(author_id_following %in% ids_final) |> select(follower_id) |>
  mutate(SMI_na = 1)

followers_cancelled <- followers_cancelled[!duplicated(followers_cancelled$follower_id), ] 

read_followers <- function(id){
  df <- read_parquet(paste0(path_out, id, '.parquet'))
  df
}

followers <- onlyfiles |> map_dfr(~read_followers(.x))

followers_agg <- followers |> group_by(id) |> summarise(SMIs = n()) |>
  ungroup()

df <- df |> left_join(followers_agg |> rename(follower_id = id)) |> 
  left_join(followers_cancelled)

df <- df |> mutate(SMIs = ifelse(is.na(SMIs) == T, 0, SMIs),
                   SMI_na = ifelse(is.na(SMI_na) == T, 0, SMI_na))

df <- df |> mutate(SMIs = ifelse(SMI_na == 1, NA, SMIs))

write_parquet(df, paste0('../../../data/04-analysis/',country,'/', stage,
                         '/SMIs_final.parquet'))
