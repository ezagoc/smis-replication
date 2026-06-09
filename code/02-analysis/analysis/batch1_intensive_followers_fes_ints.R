# 0.0 Set up the environment, clean it and set working directory to the code path
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.0 Import functions and packages
library(purrr)
library(fastDummies)
src_path <- c("../../../../src/utils/")             
source_files <- list(
  "funcs_analysis.R",
  "constants_final.R",
  "import_data.R"
)
map(paste0(src_path, source_files), source)
ipak(packages)
`%!in%` = Negate(`%in%`)

# 2.0 Define constants
country <- 'joint'
data_type <- 'Followers'
file_code <- 'intensive_followers'
ini <- '../../../../data/04-analysis/joint/'
influencer_thr <- 9
n_posts_thr <- 0

fes <- read_parquet('../../../../data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet') |>
  filter(batch_id == 'b1')

belp90 <- read_parquet('../../../../data/04-analysis/joint/below_p90_p95_divider.parquet') |>
  select(follower_id:percentile) |> select(-n_posts_base) 

stage <- 'stage3_4'
f <- get_analysis_ver_final_winsor(stage = stage, batches = 'b1b2',
                                   initial_path = '../../../../') |> 
  left_join(belp90, by = c('follower_id', 'batch_id', 'pais')) 

### Filters

f <- f |> filter(below_p90 == 1) |> filter(n_posts_base>n_posts_thr) |> 
  filter(total_influencers < influencer_thr) |> 
  filter(batch_id == 'b1') |>
  select(follower_id, pais, total_treated, batch_id, 
         total_influencers)

# Generate interactions

interactions <- generate_interactions(f)

interactions <- interactions |> left_join(fes, 
                                          by = c('follower_id', 'pais'))

rm(fes)

int_cols <- paste(grep("^tao_", colnames(interactions), value = TRUE),
                  collapse = ' + ')

# 3.0 Import data and manipulate
df <- get_analysis_followers(batches = 'b1b2', initial_path = '../../../../')
df <- df |> filter(n_posts_base>0)

df <- df |> left_join(belp90 |> filter(batch_id == 'b1') |>
                        select(-batch_id), by = c('follower_id', 'pais')) |> 
  filter(below_p90 == 1) |> filter(total_influencers < 9)

df <- df |> left_join(interactions, by = c('follower_id', 'pais'))

means <- c(mean(df$AC), mean(df$SMIs, na.rm=T))

aux <- c('AC', 'SMIs')
# 4.0 Run original estimates
lm_list_ols <- list()
count <- 1
for (x in aux) {
  if (x == 'AC'){
    fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                               x, "_base + ", int_fes, ' + ', int_cols,  "| total_influencers"))}
  else {
    fmla1 <- as.formula(paste0(x, "~ total_treated + ", int_fes, ' + ', int_cols,  "| total_influencers"))
  }
  
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, feols(fmla1, data = df))
  coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
    select(Estimate)
  names(coefs) <- paste0(x)
  coefs <- cbind('treatment' = rownames(coefs), coefs) |>
    filter(treatment == 'total_treated')
  rownames(coefs) <- 1:nrow(coefs)
  lm_list_ols[[count]] <- coefs
  count <- count + 1
}
coefs_all <- lm_list_ols %>% 
  reduce(left_join, by = "treatment")

# Build matrix
ac <- coefs_all %>% 
  select(ends_with(aux[1]))
ac <- ac[1,]

smi <- coefs_all %>% 
  select(ends_with(aux[2]))
smi <- smi[1,]

coefs_perm <- data.frame(ac, smi)

write_xlsx(
  coefs_perm, paste0("../../../../data/04-analysis/",country,
                     "/",data_type,"/original/", file_code,
                     ".xlsx"))
### 5.0 Run 1000 Permutations: 

df_na <- df |> filter(is.na(SMIs) == T)

i <- 1
coefs_fin <- tibble()
for (m in 1:1000){
  print(i) 
  followers <- read_parquet(paste0("../../../../data/04-analysis/joint/",
                                   'small_ties_b1b2p', "/small_tie", 
                                   i,".parquet"))
  
  data <- df
  
  data <- data |> select(-c(starts_with('tao_')))
  
  c1 = paste0("n_influencers_followed_control_no_weak_tie_p", i)
  c2 = paste0("n_influencers_followed_treatment_no_weak_tie_p", i)
  c3 = paste0("n_influencers_followed_treatment_weak_tie_p", i)
  c4 = paste0("n_influencers_followed_control_weak_tie_p", i)
  c5 = paste0("n_influencers_followed_control_strong_tie_p", i)
  c6 = paste0("n_influencers_followed_treatment_strong_tie_p", i)
  c7 = paste0("n_influencers_followed_control_no_strong_tie_p", i)
  c8 = paste0("n_influencers_followed_treatment_no_strong_tie_p", i)
  c9 = paste0("n_influencers_followed_control_p", i)
  c10 = paste0("n_influencers_followed_treatment_p", i)
  c11 = paste0("n_influencers_followed_p_", i)
  
  followers_iter <- followers %>% select(follower_id, c1, c2, c3, c4, 
                                         c5, c6, c7, c8, c9, c10, c11, pais, 
                                         batch_id)
  followers_iter <- followers_iter |> filter(batch_id == 'b1')
  
  data <- left_join(
    data, 
    followers_iter,
    by = c('follower_id', 'pais')
  )
  # Pool treatment variables
  data <- poolTreatmentBalance2(data, c10, c11)
  
  interactions_perms <- generate_interactions(data)
  
  int_cols2 <- paste(grep("^tao_", colnames(interactions_perms), value = TRUE),
                     collapse = ' + ')
  
  data <- data |> left_join(interactions_perms, 
                            by = c('follower_id', 'pais', 'batch_id'))
  
  # Balance tables 
  aux_data <- data[aux]
  coefs_list <- list()
  lm_list_ols <- list()
  count <- 1
  for (au in aux) {
    if (x == 'AC'){
      fmla1 <- as.formula(paste0(au, "~ total_treated + ",
                                 au, "_base + ", int_fes, ' + ', int_cols2,  "| total_influencers"))}
    else {
      fmla1 <- as.formula(paste0(au, "~ total_treated + ", int_fes, ' + ', int_cols2,  "| total_influencers"))
    }
    nam1 <- paste("lm_", count, "_ols", sep = "")
    assign(nam1, feols(fmla1, data = data))
    coefs <- data.frame(coeftable(get(nam1, envir = globalenv()))) |> 
      select(Estimate)
    names(coefs) <- paste0(au)
    coefs <- cbind('treatment' = rownames(coefs), coefs) |> 
      filter(treatment != paste0(au, '_base'))
    rownames(coefs) <- 1:nrow(coefs)
    lm_list_ols[[count]] <- coefs
    count <- count + 1
  }
  coefs_list <- append(coefs_list, lm_list_ols)
  coefs_all <- coefs_list %>% reduce(left_join, by = "treatment")
  
  # Build matrix
  ac <- coefs_all %>% 
    select(ends_with(aux[1]))
  ac <- ac[1,]
  
  smi <- coefs_all %>% 
    select(ends_with(aux[2]))
  smi <- smi[1,]
  
  coefs_perm <- data.frame(ac, smi)
  
  coefs_fin <- rbind(coefs_fin, coefs_perm)
  
  
  i <- i + 1}
write_xlsx(coefs_fin, paste0("../../../../data/04-analysis/joint/", data_type,
                             "/permutations/", file_code, ".xlsx"))


coefs <- readxl::read_excel(paste0("../../../../data/04-analysis/",country,
                                   "/",data_type,"/original/", file_code,
                                   ".xlsx")) |>
  pivot_longer(cols = everything(), names_to = 'var', values_to = 'coef')

perm <- readxl::read_excel(paste0("../../../../data/04-analysis/joint/", data_type,
                                  "/permutations/", file_code, ".xlsx")) |> 
  summarise(across(everything(), ~sd(.x))) |> 
  pivot_longer(cols = everything(), names_to = 'var', values_to = 'sd')

final <- coefs |> left_join(perm, by = c( 'var'))

ses <- final$sd

ses <- paste(as.character(ses), collapse =" & ")

count <- 1
for (x in aux) {
  if (x == 'AC'){
    fmla1 <- as.formula(paste0(x, "~ total_treated + ",
                               x, "_base + ", int_fes, "| total_influencers"))}
  else {
    fmla1 <- as.formula(paste0(x, "~ total_treated + ", int_fes, "| total_influencers"))
  }
  nam1 <- paste("lm_", count, "_ols", sep = "")
  assign(nam1, felm(fmla1, data = df))
  lm_list_ols[[count]] <- get(nam1, envir = globalenv())
  count <- count + 1
}

table <- stargazer(
  lm_list_ols, # robust standard errors
  label = paste0("tab:followers_ac"),
  header = FALSE,
  font.size = "scriptsize",
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  table.placement = "!htpb",
  column.labels = c('Africa Check', 'SMIs'),
  covariate.labels = c("Total Treated"),
  keep = c('total_treated'),
  omit.stat=c("f", "ser","adj.rsq"),
  column.sep.width = "0pt",
  add.lines = list(c("Baseline control", 'Yes', 'No'),
                   c("Block1 FEs", rep("Yes", 2)), 
                   c('Outcome mean', means)),
  title = 'Intensive Margin Analysis',
  type = "latex") 

note.latex <- paste0("\\multicolumn{3}{l} {\\parbox[t]{9cm}{ \\textit{Notes:} Real SDs are ",ses, "
The unit of observation is an influencer's follower. Total URLs is the sum of all posts that contained at least one URL that was not a Twitter link (pictures, RTs) or other Social Media link.
Total info is the sum of all posts that contain links to information, this could be blogs, newsites and organizations. Totla news is the sum of all posts that contained links to newsites.
We report estimates from OLS regression. Specifications further include block1 randomization of the influencers fixed effects. Robust standard errors are in parentheses. 
 * denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
table[grepl("Note", table)] <- note.latex
print(table)
cat(table, file = paste0("../../../../results/01-regression_graphs/",
                         data_type,
                         "/intensive_followers_interactions.tex"))


