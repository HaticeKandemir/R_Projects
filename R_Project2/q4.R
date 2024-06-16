# Define a scaling function
scale_variable <- function(data, var_names, method = "standard") {
  # Copy the original data frame to avoid modifying the original
  scaled_data <- data
  
  # Check if var_names is a character vector, if not convert it to one
  if (!is.character(var_names)) {
    var_names <- as.character(var_names)
  }
  
  # Scale each variable based on the specified method
  for (var in var_names) {
    if (method == "standard") {
      # Compute mean and standard deviation, excluding NA values
      mean_val <- mean(data[[var]], na.rm = TRUE)
      sd_val <- sd(data[[var]], na.rm = TRUE)
      # Scale the variable
      scaled_data[[var]] <- (data[[var]] - mean_val) / sd_val
    } else if (method == "minmax") {
      # Compute min and max values, excluding NA values
      min_val <- min(data[[var]], na.rm = TRUE)
      max_val <- max(data[[var]], na.rm = TRUE)
      # Scale the variable
      scaled_data[[var]] <- (data[[var]] - min_val) / (max_val - min_val)
    } else if (method == "robust") {
      # Compute median and IQR values, excluding NA values
      median_val <- median(data[[var]], na.rm = TRUE)
      iqr_val <- IQR(data[[var]], na.rm = TRUE)
      # Scale the variable
      scaled_data[[var]] <- (data[[var]] - median_val) / iqr_val
    } else if (method == "maxabs") {
      # Compute max absolute value, excluding NA values
      max_abs_val <- max(abs(data[[var]]), na.rm = TRUE)
      # Scale the variable
      scaled_data[[var]] <- data[[var]] / max_abs_val
    } else if (method == "unitlength") {
      # Compute the length (norm) of the variable, excluding NA values
      length_val <- sqrt(sum(data[[var]]^2, na.rm = TRUE))
      # Scale the variable
      scaled_data[[var]] <- data[[var]] / length_val
    } else {
      stop("Invalid method. Please choose 'standard', 'minmax', 'robust', 'maxabs', or 'unitlength'.")
    }
  }
  
  return(scaled_data)
}

# Load the dataset
data <- read.table("DatasetNA.txt", header = TRUE)

# Scale all variables starting with "Var" using different scaling methods
scaled_data_standard <- scale_variable(data, names(data)[grep("^Var", names(data))], method = "standard")
print("Standard Scaled Data:")
print(scaled_data_standard)

scaled_data_minmax <- scale_variable(data, names(data)[grep("^Var", names(data))], method = "minmax")
print("Min-Max Scaled Data:")
print(scaled_data_minmax)

scaled_data_robust <- scale_variable(data, names(data)[grep("^Var", names(data))], method = "robust")
print("Robust Scaled Data:")
print(scaled_data_robust)

scaled_data_maxabs <- scale_variable(data, names(data)[grep("^Var", names(data))], method = "maxabs")
print("Max-Abs Scaled Data:")
print(scaled_data_maxabs)

scaled_data_unitlength <- scale_variable(data, names(data)[grep("^Var", names(data))], method = "unitlength")
print("Unit Length Scaled Data:")
print(scaled_data_unitlength)
