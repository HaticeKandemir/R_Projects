# Function: Linear Regression Analysis
linear_regression <- function(Y, X) {
  # Convert Y to a column matrix
  Y <- matrix(Y, ncol = 1)
  
  # Add intercept column to X matrix
  X <- cbind(1, X)
  
  # Calculate regression coefficients (beta)
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  # Calculate estimated Y values (Y^)
  Y_hat <- X %*% beta_hat
  
  # Calculate residuals (e^)
  residuals <- Y - Y_hat
  
  # Total sum of squares (TSS)
  TSS <- sum((Y - mean(Y))^2)
  
  # Residual sum of squares (RSS)
  RSS <- sum(residuals^2)
  
  # Regression model sum of squares (RMSS)
  RMSS <- TSS - RSS
  
  # R-squared (R_squared)
  R_squared <- RMSS / TSS
  
  return(list(beta_hat = beta_hat, 
              Y_hat = Y_hat, 
              residuals = residuals, 
              TSS = TSS, 
              RMSS = RMSS, 
              RSS = RSS, 
              R_squared = R_squared))
}

# Example Usage
data <- read.table("MultRegData.txt", header = TRUE)
Y <- data$Y
X <- as.matrix(data[, c("X1", "X2", "X3", "X4", "X5", "X6", "X7")])

# Perform the regression analysis
result <- linear_regression(Y, X)

# Print the results in a readable format
cat("Linear Regression Results:\n")
cat("-------------------------\n")
cat("Beta Coefficients:\n")
for (i in 1:length(result$beta_hat)) {
  cat(sprintf("Beta %d: %f\n", i - 1, result$beta_hat[i]))
}


cat("Estimated Y (Y^):\n")
print(head(result$Y_hat, 10)) # Print first 10 for brevity


cat("Residuals (e^):\n")
print(head(result$residuals, 10)) # Print first 10 for brevity


cat(sprintf("Total Sum of Squares (TSS): %f\n", result$TSS))
cat(sprintf("Regression Model Sum of Squares (RMSS): %f\n", result$RMSS))
cat(sprintf("Residual Sum of Squares (RSS): %f\n", result$RSS))
cat(sprintf("R-squared: %f\n", result$R_squared))

