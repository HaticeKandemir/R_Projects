#çalıştırmadan önce setwd() ile çalışma dizinini değiştirmek gerekiyor

custom_histogram <- function(data, var, bins = 10, title = "", xlab = "", ylab = "", xlim = NULL, ylim = NULL, color = "blue") {
  data[[var]] <- as.numeric(gsub(",", ".", data[[var]]))
  
  data <- na.omit(data[[var]])

  breaks <- seq(min(data), max(data), length.out = bins + 1)
  counts <- rep(0, bins)
  
  for (i in 1:length(data)) {
    for (j in 1:bins) {
      if (data[i] >= breaks[j] && data[i] < breaks[j + 1]) {
        counts[j] <- counts[j] + 1
        break
      }
    }
  }
  plot(breaks, c(counts, 0), type = "h", lwd = 4, col = color, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = title)
}


data <- read.table("DatasetNA.txt", header = TRUE)