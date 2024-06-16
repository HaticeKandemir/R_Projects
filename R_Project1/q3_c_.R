custom_boxplot <- function(data, var_names, main = "Boxplot", xlab = "Groups", ylab = "Values", color = "navy") {
  # Open a new graphics device
  dev.new()
  plot.new()
  
  # Arrange the plot areas horizontally
  par(mfrow = c(1, length(var_names)))
  
  for (i in seq_along(var_names)) {
    var_name <- var_names[i]
    var <- as.numeric(data[[var_name]])
    var <- na.omit(var)  # Select non-missing values
    
    # Boxplot components
    box_vals <- boxplot.stats(var)$stats
    box_median <- box_vals[3]
    box_lower <- box_vals[2]
    box_upper <- box_vals[4]
    lower_whisker <- box_vals[1]
    upper_whisker <- box_vals[5]
    
    # Draw the box
    segments(0.8, box_lower, 1.2, box_lower, lwd = 2) # lower whisker
    segments(0.8, box_upper, 1.2, box_upper, lwd = 2) # upper whisker
    segments(0.8, box_lower, 0.8, box_upper, lwd = 2) # vertical line
    
    # Draw the median line
    segments(0.6, box_median, 1.4, box_median, lwd = 2)
    
    # Outliers
    outlier_values <- boxplot(var)$out
    if (length(outlier_values) > 0) {
      points(rep(1, length(outlier_values)), outlier_values, pch = 19, col = "red", cex = 1.5)
    }
    
    # Title
    title(main = paste0("Boxplot of ", var_name), xlab = "", ylab = "")
  }
  par(mfrow = c(1, 1))
}

# Example usage
data <- read.table("DatasetNA.txt", header = TRUE, na.strings = "NA")
custom_boxplot(data, c("Var1", "Var3", "Var5"), main = "Custom Boxplot for Var1, Var3, and Var5", ylab = "Values", color = "yellow")
