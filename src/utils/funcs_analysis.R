ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

poolTreatmentBalance <- function(data, c_s, t_s, c_w, t_w, c_nw, t_nw) {
  
  data["c_t_strong_total"] <- data[c_s] + data[t_s]
  data["c_t_weak_total"] <- data[c_w] + data[t_w]
  data["c_t_neither_total"] <- data[c_nw] + data[t_nw]
  data["t_strong"] <- data[t_s]
  data["t_weak"] <- data[t_w]
  data["t_neither"] <- data[t_nw]
  data["strong_weak"] <- data['t_strong'] + data['t_weak']
  data["all_t"] <- data['t_strong'] + data['t_weak'] + data['t_neither']
  
  return(data)
}

poolTreatmentBalance1 <- function(data, c_s, t_s, c_w, t_w, all_t, all){
  
  data["c_t_strong_total"] <- data[c_s] + data[t_s]
  data["c_t_weak_total"] <- data[c_w] + data[t_w]
  data["c_t_neither_total"] <- data[all] - data["c_t_strong_total"] - 
    data["c_t_weak_total"]
  data["t_strong"] <- data[t_s]
  data["t_weak"] <- data[t_w]
  data["t_neither"] <- data[all_t] - data["t_strong"] - data["t_weak"]
  data["strong_weak"] <- data['t_strong'] + data['t_weak']
  
  return(data)
}

poolTreatmentBalance2 <- function(data, all_t, all) {
  
  data["total_treated"] <- data[all_t] 
  data["total_influencers"] <- data[all]
  
  return(data)
}

matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j], na.rm = T))/sd(x[sgroup,j], na.rm = T)
  }
  return(x)
}


icwIndex <- function(
    xmat,
    wgts=rep(1, nrow(xmat)),
    revcols = NULL,
    ind_na = 1, 
    sgroup = rep(TRUE, nrow(xmat))
){
  X0 <- xmat
  X <- matStand(X0, sgroup)
  X[is.na(xmat)] <- 0
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  Sx <- cov.wt(X, wt=wgts)[[1]]
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  if (all(colSums(is.na(xmat)) > 0 )) { # if all columns have NA values set NA values to index in those obs
    index[is.na(xmat[,ind_na])] <- NA # Implement ind_na as an auxiliary index for a column that has 463 responses (ONLY PSM)
  }
  return(list(weights = weights, index = index))
}

# Inverse Covariance Weigthing Function
# df: dataframe with selected columns
# reverse: column indexes to change sign 
# Output: list with weights and the index variable

# ------------------------------------------------------------------------------

icwFunction <- function(df, reverse = NULL) {
  
  vars <- Rfast::colVars(as.matrix(df), na.rm = T)
  
  if (length(reverse) > 0 ) {
    df[, reverse] <- -1*df[, reverse]
  }
  
  sum_inverse <- (sum(1/vars))^-1 # sum of the inverse of variances to the inverse
  
  weights <- (1/vars)*sum_inverse # inverse vector of variances times sum of inverses to the inverse
  
  # Temporarily convert NA values
  x0 <- df
  x0[is.na(df)] <- 0
  
  index <- t(weights%*%t(x0)) # Calculate the index.  t(W*t(DF))
  
  if (all(colSums(is.na(df)) > 0 )) { # if all columns have NA values set NA values to index in those obs
    index[is.na(df[,1])] <- NA
  }
  
  return(list(weights, index)) # return weights and index
}

#### Interactions and FEs

generate_interactions <- function(ints){
  
  ints <- ints |> select(follower_id, pais, batch_id, total_treated, 
                         total_influencers)
  
  ints <- fastDummies::dummy_cols(
    ints,
    select_columns = "total_treated",
    remove_first_dummy = FALSE
  ) # Generate total treated dummies
  
  ints <- fastDummies::dummy_cols(
    ints,
    select_columns = "total_influencers",
    remove_first_dummy = FALSE
  ) # Generate total influencers dummies
  
  ints <- ints |>  
    mutate(across(starts_with('total_influencers_'), 
                  ~.x - mean(.x))) # total inf dummy - proportion
  
  interactions_terms <- ints |> select(follower_id, pais, batch_id)
  
  treated_columns <- grep("^total_treated_", colnames(ints), value = TRUE)
  
  total_columns <- grep("^total_influencers_", colnames(ints), value = TRUE)
  
  treated_columns <- treated_columns[treated_columns != "total_treated_0"]
  
  total_columns <- total_columns[total_columns != "total_influencers_1"]
  
  count_i <- 1
  for (i in treated_columns) {
    count_j <- 2
    for (j in total_columns) {
      interaction_name <- paste0('tao_', count_i, '_', count_j)
      interactions_terms[[interaction_name]] <- ints[[i]] * ints[[j]]
      
      count_j <- count_j + 1
    }
    count_i <- count_i + 1
  } # Generate interactions
  
  return(interactions_terms)
  
}
