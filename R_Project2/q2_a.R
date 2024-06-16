data <- read.table("DatasetNA.txt", header = TRUE)

# Fill missing values
for (i in 4:11) {
  data[[i]][is.na(data[[i]])] <- mean(data[[i]], na.rm = TRUE)
}

# Calculate statistics by Group factor
statistics_group <- lapply(colnames(data)[4:11], function(x) {
  aggregate(data[[x]], by = list(Group = data$Group), FUN = function(y) {
    c(
      "Variable" = x,
      "Number of observations" = sum(!is.na(y)),
      "Minimum" = min(y),
      "Maximum" = max(y),
      "Range" = max(y) - min(y),
      "Sum" = sum(y),
      "Mean" = mean(y),
      "Median" = median(y),
      "Sum of squares" = sum(y^2),
      "Variance" = var(y),
      "Standard deviation" = sd(y)
    )
  })
})

# Calculate statistics by Gender factor
statistics_gender <- lapply(colnames(data)[4:11], function(x) {
  aggregate(data[[x]], by = list(Gender = data$Gender), FUN = function(y) {
    c(
      "Variable" = x,
      "Number of observations" = sum(!is.na(y)),
      "Minimum" = min(y),
      "Maximum" = max(y),
      "Range" = max(y) - min(y),
      "Sum" = sum(y),
      "Mean" = mean(y),
      "Median" = median(y),
      "Sum of squares" = sum(y^2),
      "Variance" = var(y),
      "Standard deviation" = sd(y)
    )
  })
})

# Calculate statistics by Group and Gender factors
statistics_group_gender <- lapply(colnames(data)[4:11], function(x) {
  aggregate(data[[x]], by = list(Group = data$Group, Gender = data$Gender), FUN = function(y) {
    c(
      "Variable" = x,
      "Number of observations" = sum(!is.na(y)),
      "Minimum" = min(y),
      "Maximum" = max(y),
      "Range" = max(y) - min(y),
      "Sum" = sum(y),
      "Mean" = mean(y),
      "Median" = median(y),
      "Sum of squares" = sum(y^2),
      "Variance" = var(y),
      "Standard deviation" = sd(y)
    )
  })
})

# Results
print("Statistics by Group:")
print(statistics_group)
print("Statistics by Gender:")
print(statistics_gender)
print("Statistics by Group and Gender:")
print(statistics_group_gender)

