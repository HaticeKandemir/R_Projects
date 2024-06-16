#çalıştırmadan önce setwd() ile çalışma dizinini değiştirmek gerekiyor

custom_barplot <- function(data, x_var, xlab = "", ylab = "", main = "", xlim = NULL, ylim = NULL, border_color = "black", fill_color = "blue") {
  unique_values <- unique(data[[x_var]])
  counts <- vector(length = length(unique_values))
  for (i in seq_along(unique_values)) {
    counts[i] <- sum(data[[x_var]] == unique_values[i])
  }
  
  bar_width <- 0.7
  bar_positions <- 1:length(unique_values)
  
  plot(NULL, type = "n", xlim = c(0, length(unique_values) + 1), ylim = c(0, max(counts)+5), xlab = xlab, ylab = ylab, main = main,xaxt = 'n')
  
  for (i in 1:length(unique_values)) {
    polygon(
      c(i - bar_width / 2, i - bar_width / 2, i + bar_width / 2, i + bar_width / 2),
      c(0, counts[i], counts[i], 0),
      border = border_color,
      col = fill_color
    )
  }
  
  text(bar_positions, counts, labels = counts, pos = 3)
  
  axis(1, at = bar_positions, labels = unique_values)
  
  if (!is.null(xlim)) {
    usr <- par("usr")
    x_range <- xlim[2] - xlim[1]
    usr[1] <- xlim[1] - 0.1 * x_range
    usr[2] <- xlim[2] + 0.1 * x_range
    par(usr = usr)
  }
  
  if (!is.null(ylim)) {
    usr <- par("usr")
    y_range <- ylim[2] - ylim[1]
    usr[3] <- ylim[1] - 0.1 * y_range
    usr[4] <- ylim[2] + 0.1 * y_range
    par(usr = usr)
  }
  
}

data <- read.table("DatasetNA.txt", header = TRUE)
