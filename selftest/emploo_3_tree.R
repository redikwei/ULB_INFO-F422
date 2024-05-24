## find empirical risk and generalisation leave-one-out (tree models/with value table)

## x1, x2, y
## value table
## 13 observations
## s=1/5/10 hidden nodes (s smallest allowed node size)
## 两个都小是好的结果，如果一大一小，优先选loo小的，loo(generalize to new data),emp(fit training)

# Load necessary library
library(tree)

# Data input
data <- data.frame(
  x1 = c(1.3, -1.2, 0.5, 1.0, 0.3, 0, 0.4, -0.9, -0.2, -0.1, 0.1, 0.2, 0.9),
  x2 = c(0.6, 0.1, 0.5, -0.3, 0.01, -0.05, 0.15, -0.3, 0.5, 0.3, 0.7, -0.5, 0.3),
  y = c(-1.2, 0.3, 0.6, -1, 0.1, 0.1, 0.3, -0.7, 0.6, 0.3, 0.1, -0.2, 0.1)
)

# Set seed for reproducibility
set.seed(0)

# Create tree models with varying node sizes
model1 <- tree(y ~ ., data, minsize = 1)
model2 <- tree(y ~ ., data, minsize = 5)
model3 <- tree(y ~ ., data, minsize = 10)

# Function to calculate MSE
calculate_mse <- function(model, data) {
  predictions <- predict(model, newdata = data)
  mean((data$y - predictions)^2)
}

# Empirical risk for each model
mse_emp_model1 <- calculate_mse(model1, data)
mse_emp_model2 <- calculate_mse(model2, data)
mse_emp_model3 <- calculate_mse(model3, data)

# Function to calculate leave-one-out MSE
calculate_loo_mse <- function(model_func, data) {
  loo_mse <- numeric(nrow(data))
  for (i in 1:nrow(data)) {
    training_data <- data[-i, ]
    testing_data <- data[i, , drop = FALSE]
    model <- model_func(training_data)
    prediction <- predict(model, newdata = testing_data)
    loo_mse[i] <- (testing_data$y - prediction)^2
  }
  mean(loo_mse)
}

# Leave-one-out generalization error for each model
loo_mse_model1 <- calculate_loo_mse(function(data) tree(y ~ ., data, minsize = 1), data)
loo_mse_model2 <- calculate_loo_mse(function(data) tree(y ~ ., data, minsize = 5), data)
loo_mse_model3 <- calculate_loo_mse(function(data) tree(y ~ ., data, minsize = 10), data)

# Output the results
cat("Model 1: MISE_emp =", mse_emp_model1, "MISE_loo =", loo_mse_model1, "\n")
cat("Model 2: MISE_emp =", mse_emp_model2, "MISE_loo =", loo_mse_model2, "\n")
cat("Model 3: MISE_emp =", mse_emp_model3, "MISE_loo =", loo_mse_model3, "\n")
