#çalıştırmadan önce setwd() ile çalışma dizinini değiştirmek gerekiyor

custom_boxplot <- function(data, var_name, main = "Boxplot", xlab = "Groups", ylab = "Values",color = "navy") {
  # Create an empty plot
  plot(NULL, xlim = c(0.5, 1.5), ylim = range(data[[var_name]], na.rm = TRUE), 
       xlab = xlab, ylab = ylab, main = main,xaxt='n')
  
  # Plot boxplot for the single variable
  box_min <- quantile(data[[var_name]], 0.25, na.rm = TRUE)
  box_med <- median(data[[var_name]], na.rm = TRUE)
  box_max <- quantile(data[[var_name]], 0.75, na.rm = TRUE)
  whisker_min <- min(data[data[[var_name]] >= box_min - 1.5 * IQR(data[[var_name]], na.rm = TRUE), var_name], na.rm = TRUE)
  whisker_max <- max(data[data[[var_name]] <= box_max + 1.5 * IQR(data[[var_name]], na.rm = TRUE), var_name], na.rm = TRUE)
  outliers <- data[data[[var_name]] < box_min - 1.5 * IQR(data[[var_name]], na.rm = TRUE) |
                     data[[var_name]] > box_max + 1.5 * IQR(data[[var_name]], na.rm = TRUE), var_name]
  
  # Box
  polygon(c(1 - 0.2, 1 + 0.2, 1 + 0.2, 1 - 0.2), c(box_min, box_min, box_max, box_max), border = "black", col = color)
  
  # Median line
  segments(1 - 0.2, box_med, 1 + 0.2, box_med)
  
  # Whiskers
  segments(1, whisker_min, 1, box_min)
  segments(1, whisker_max, 1, box_max)
  
  # Outliers
  if (length(outliers) > 0) {
    points(rep(1, length(outliers)), outliers, pch = 19, col = "red")
  }
  
  # X-axis label
  text(1, par("usr")[3] - 0.1 * diff(par("usr")[3:4]), labels = var_name, srt = 0, adj = 0.5, xpd = TRUE)
}

data <- read.table("DatasetNA.txt", header = TRUE, na.strings = "NA")

