# Function for calculating regression statistics
calculate_stats <- function(Y, X) {
  Y <- matrix(Y, ncol = 1)
  X <- cbind(1, X)
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
  Y_hat <- X %*% beta_hat
  residuals <- Y - Y_hat
  TSS <- sum((Y - mean(Y))^2)
  RSS <- sum(residuals^2)
  RMSS <- TSS - RSS
  R_squared <- RMSS / TSS
  return(list(TSS = TSS, RMSS = RMSS, RSS = RSS, R_squared = R_squared))
}

# Function for generating combinations of variables
generate_combinations <- function(vars) {
  n <- length(vars)
  combos <- list()
  
  for (i in 1:n) {
    combos <- c(combos, combn(vars, i, simplify = FALSE))
  }
  
  return(combos)
}

# Function for model selection based on R-squared
model_selection <- function(Y, X) {
  var_names <- colnames(X)
  all_models <- list()
  
  combinations <- generate_combinations(var_names)
  
  for (combo in combinations) {
    X_subset <- as.matrix(X[, combo])
    stats <- calculate_stats(Y, X_subset)
    all_models[[length(all_models) + 1]] <- c(length(combo), paste(combo, collapse = " "), stats)
  }
  
  models_df <- do.call(rbind, lapply(all_models, function(model) {
    c(Model = as.character(model[2]), 
      Number_of_Variables = model[1], 
      TSS = model$TSS, 
      RMSS = model$RMSS, 
      RSS = model$RSS, 
      R_squared = model$R_squared)
  }))
  
  models_df <- as.data.frame(models_df, stringsAsFactors = FALSE)
  models_df$Number_of_Variables <- as.numeric(models_df$Number_of_Variables)
  models_df$TSS <- as.numeric(models_df$TSS)
  models_df$RMSS <- as.numeric(models_df$RMSS)
  models_df$RSS <- as.numeric(models_df$RSS)
  models_df$R_squared <- as.numeric(models_df$R_squared)
  
  models_df <- models_df[order(models_df$Number_of_Variables, -models_df$R_squared),]
  return(models_df)
}

# Example usage
data <- read.table("MultRegData.txt", header = TRUE)
Y <- data$Y
X <- as.matrix(data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7")])

model_results <- model_selection(Y, X)
print(model_results)
