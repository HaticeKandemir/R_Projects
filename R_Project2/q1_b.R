data <- read.table("DatasetNA.txt", header = TRUE)

cross_products <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Length of vectors x and y must be equal.")
  }
  n <- length(x)
  cross_prod <- sum(x * y)
  return(cross_prod)
}

# Function to calculate covariance
covariance <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Length of vectors x and y must be equal.")
  }
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)
  covar <- sum((x - mean_x) * (y - mean_y), na.rm = TRUE) / (n - 1)
  return(covar)
}

# Function to calculate correlations
correlation <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Length of vectors x and y must be equal.")
  }
  covar_xy <- covariance(x, y)
  sd_x <- sd(x, na.rm = TRUE)
  sd_y <- sd(y, na.rm = TRUE)
  corr <- covar_xy / (sd_x * sd_y)
  return(corr)
}

continuous_vars <- data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")]

# cross-products
cross_product_matrix <- matrix(0, ncol = ncol(continuous_vars), nrow = ncol(continuous_vars))
for (i in 1:(ncol(continuous_vars) - 1)) {
  for (j in (i + 1):ncol(continuous_vars)) {
    cross_product_matrix[i, j] <- cross_products(continuous_vars[, i], continuous_vars[, j])
    cross_product_matrix[j, i] <- cross_product_matrix[i, j]
  }
}

# covariance matrix
covariance_matrix <- cov(continuous_vars, use = "pairwise.complete.obs")

# correlation matrix
correlation_matrix <- cor(continuous_vars, use = "pairwise.complete.obs")

# Results
print("Cross-products:")
print(cross_product_matrix)
print("Covariance Matrix:")
print(covariance_matrix)
print("Correlation Matrix:")
print(correlation_matrix)



