data <- read.table("DatasetNA.txt", header = TRUE)

# Define a function to compute cross-products, covariance, and correlations
compute_stats <- function(data, factor1, factor2 = NULL) {
  # Filter out NA values
  data <- na.omit(data)
  
  if (is.null(factor2)) {
    # If only one factor is provided
    result <- by(data, data[[factor1]], function(subdata) {
      # Convert all columns to numeric
      subdata <- apply(subdata, 2, as.numeric)
      
      # Calculate cross-products
      cross_products <- crossprod(subdata[, -c(1, which(names(subdata) == factor1))])
      
      # Calculate covariance
      covariance <- cov(subdata[, -c(1, which(names(subdata) == factor1))])
      
      # Calculate correlations
      correlations <- cor(subdata[, -c(1, which(names(subdata) == factor1))])
      
      return(list(cross_products = cross_products, covariance = covariance, correlations = correlations))
    })
  } else {
    # If both factors are provided
    result <- by(data, list(data[[factor1]], data[[factor2]]), function(subdata) {
      # Convert all columns to numeric
      subdata <- apply(subdata, 2, as.numeric)
      
      # Calculate cross-products
      cross_products <- crossprod(subdata[, -c(1, which(names(subdata) %in% c(factor1, factor2)))])
      
      # Calculate covariance
      covariance <- cov(subdata[, -c(1, which(names(subdata) %in% c(factor1, factor2)))])
      
      # Calculate correlations
      correlations <- cor(subdata[, -c(1, which(names(subdata) %in% c(factor1, factor2)))])
      
      return(list(cross_products = cross_products, covariance = covariance, correlations = correlations))
    })
  }
  
  return(result)
}

# Compute statistics for different combinations of factors
stats_group <- compute_stats(data, "Group")
stats_gender <- compute_stats(data, "Gender")
stats_group_gender <- compute_stats(data, "Group", "Gender")

# Output the results
print("Statistics by Group:")
print(stats_group)

print("Statistics by Gender:")
print(stats_gender)

print("Statistics by Group and Gender:")
print(stats_group_gender)




