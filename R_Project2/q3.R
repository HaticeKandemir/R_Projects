# Define a function to draw a scatterplot between two variables
draw_scatterplot <- function(data, x_var, y_var) {
  plot(data[[x_var]], data[[y_var]], 
       xlab = x_var, ylab = y_var,
       main = paste("Scatterplot of", x_var, "vs", y_var))
}

# Define a function to draw a scatterplot matrix between variables in a dataframe
draw_scatterplot_matrix <- function(data) {
  # Select numeric variables
  numeric_vars <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_vars]
  
  # Draw a scatterplot matrix between all numeric variables
  pairs(numeric_data, main = "Scatterplot Matrix")
  
  # Print the names of variables in the scatterplot matrix
  cat("\nVariables in the Scatterplot Matrix:\n")
  print(colnames(numeric_data))
}

# Define a function to remove missing values from a dataframe
clean_data <- function(data) {
  clean_data <- na.omit(data)
  return(clean_data)
}

# Load the dataframe (adapt this step to your own dataframe)
data <- read.table("DatasetNA.txt", header = TRUE)

# Remove missing values
cleaned_data <- clean_data(data)

# Draw a scatterplot between two variables
draw_scatterplot(cleaned_data, "Var1", "Var4")

# Draw a scatterplot matrix between all variables
draw_scatterplot_matrix(cleaned_data)
