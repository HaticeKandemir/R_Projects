custom_boxplot <- function(data, var_name, group_var, main = "Boxplot", xlab = "Groups", ylab = "Values", color = c("navy", "pink")) {
  # Get unique groups
  groups <- unique(data[[group_var]])
  
  # Create an empty plot
  plot(NULL, xlim = c(0.5, length(groups) + 0.5), ylim = range(data[[var_name]], na.rm = TRUE), 
       xlab = xlab, ylab = ylab, main = main,xaxt='n')
  
  # Loop through groups
  for (i in 1:length(groups)) {
    # Subset data for each group
    subset_data <- data[data[[group_var]] == groups[i], var_name]
    
    # Calculate boxplot statistics
    box_min <- quantile(subset_data, 0.25, na.rm = TRUE)
    box_med <- median(subset_data, na.rm = TRUE)
    box_max <- quantile(subset_data, 0.75, na.rm = TRUE)
    whisker_min <- min(subset_data[subset_data >= box_min - 1.5 * IQR(subset_data, na.rm = TRUE)], na.rm = TRUE)
    whisker_max <- max(subset_data[subset_data <= box_max + 1.5 * IQR(subset_data, na.rm = TRUE)], na.rm = TRUE)
    outliers <- subset_data[subset_data < box_min - 1.5 * IQR(subset_data, na.rm = TRUE) |
                              subset_data > box_max + 1.5 * IQR(subset_data, na.rm = TRUE)]
    
    # Box
    polygon(c(i - 0.2, i + 0.2, i + 0.2, i - 0.2), c(box_min, box_min, box_max, box_max), border = "black", col = color[i])
    
    # Median line
    segments(i - 0.2, box_med, i + 0.2, box_med)
    
    # Whiskers
    segments(i, whisker_min, i, box_min)
    segments(i, whisker_max, i, box_max)
    
    # Outliers
    if (length(outliers) > 0) {
      points(rep(i, length(outliers)), outliers, pch = 19, col = "red")
    }
  }
  
  # X-axis labels
  axis(1, at = 1:length(groups), labels = groups)
}

# Load data
data <- read.table("DatasetNA.txt", header = TRUE, na.strings = "NA")

# Plot boxplot for Var1 using custom function based on Gender
dev.new()
custom_boxplot(data, "Var1", "Gender", main = "Custom Boxplot for Var1 based on Gender", ylab = "Var1 Values", color = c("navy", "maroon"))
dev.new()
custom_boxplot(data, "Var2", "Gender", main = "Custom Boxplot for Var2 based on Gender", ylab = "Var2 Values", color = c("pink", "purple"))

