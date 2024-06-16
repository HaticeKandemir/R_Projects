data <- read.table("DatasetNA.txt", header = TRUE, dec=",")

calculate_q1_a<- function(dataset, col_name){
  data <- na.omit(dataset)
  data[[col_name]] <- as.numeric(data[[col_name]])
  
  n <- nrow(data)
  min <- data[[col_name]][1]
  max <- data[[col_name]][1]
  sum <- 0
  for (i in 1:n) {
    if (!is.na(data[[col_name]][i])) {
      sum <- sum + data[[col_name]][i]
      if (data[[col_name]][i] < min) {
        min <- data[[col_name]][i]
      }
      if (data[[col_name]][i] > max) {
        max <- data[[col_name]][i]
      }
    }
  }
  range <- max - min
  mean <- sum / n
  sorted_data <- sort(data[[col_name]], na.last = NA)
  middle <- floor(n/2)
  if (n%%2 == 0) {
    median <- (sorted_data[middle] + sorted_data[middle+1])/2
  } else {
    median <- sorted_data[middle+1]
  }
  sum_of_sequares <- 0
  for (i in 1:n) {
    if (!is.na(data[[col_name]][i])) {
      sum_of_sequares <- sum_of_sequares + (data[[col_name]][i] - mean)^2
    }
  }
  variance <- sum_of_sequares / (n - 1)
  standard_deviation <- sqrt(variance)
  
  cat("--------", col_name, "--------\n")
  cat("Number of observations:", n, "\n")
  cat("Minimum:", min, "\n")
  cat("Maximum:", max, "\n")
  cat("Range:", range, "\n")
  cat("Sum:", sum, "\n")
  cat("Mean:", mean, "\n")
  cat("Median:", median, "\n")
  cat("Sum of squares:", sum_of_sequares, "\n")
  cat("Variance:", variance, "\n")
  cat("Standard deviation:", standard_deviation, "\n")
}

calculate_q1_a(data,"Var1")
calculate_q1_a(data,"Var2")
calculate_q1_a(data,"Var3")
calculate_q1_a(data,"Var4")
calculate_q1_a(data,"Var5")
calculate_q1_a(data,"Var6")
calculate_q1_a(data,"Var7")
calculate_q1_a(data,"Var8")

