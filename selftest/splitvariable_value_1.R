## split variable and value

## load figure
## value list
## optimal first split of growing parametric identification of a regression tree

# Load necessary packages
library(MASS)  # If needed for any reason

# Load the data
load("D:/ULB/9-ULBstudy/bloc1_sem2/info_f422_statistical foundation for machine learning/exam/selftest/UV.NL.Rdata")

# Assuming df is the data frame loaded from the RData file
df <- df  # Replace 'df' with the actual name of the dataframe if different

# Ensure no NA values
if (any(is.na(df))) {
  cat("NA values found in the dataset. Please handle them before proceeding.\n")
}

# Define the potential splits
splits <- seq(-1, 1, by = 0.1)

# Initialize variables to store the best split results
best_var <- NULL
best_split <- NULL
lowest_mse <- Inf  # Start with an infinitely large MSE

# Function to calculate MSE
calculate_mse <- function(left, right) {
  left_mse <- ifelse(length(left) > 0, sum((left - mean(left))^2), 0)
  right_mse <- ifelse(length(right) > 0, sum((right - mean(right))^2), 0)
  total_length <- length(left) + length(right)
  return(ifelse(total_length > 0, (left_mse + right_mse) / total_length, Inf))
}

# Evaluate each split for each variable
for (var in names(df)[-which(names(df) == "Y")]) {  # Corrected to "Y"
  for (split in splits) {
    left_index <- df[[var]] <= split
    right_index <- df[[var]] > split
    left_y <- df$Y[left_index]  # Corrected to "Y"
    right_y <- df$Y[right_index]  # Corrected to "Y"
    mse <- calculate_mse(left_y, right_y)
    
    # Diagnostic output
    cat(sprintf("Var: %s, Split: %f, MSE: %f, Left Count: %d, Right Count: %d\n", var, split, mse, length(left_y), length(right_y)))
    
    if (mse < lowest_mse) {
      lowest_mse <- mse
      best_var <- var
      best_split <- split
    }
  }
}

# Output the best split
cat("Best variable to split:", best_var, "\n")
cat("Value of the split:", best_split, "\n")
cat("Lowest MSE achieved:", lowest_mse, "\n")

