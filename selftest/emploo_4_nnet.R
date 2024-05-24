## find empirical risk and generalisation leave-one-out (nnet models/with value table)

## x1, x2, y
## value table
## 13 observations
## s=1/3/5 hidden nodes (s smallest allowed node size)

library(nnet)
# Create the data frame from the provided data
data <- data.frame(
  x1 = c(1.3, -1.2, 0.5, 1.0, 0.3, 0, 0.4, -0.9, -0.2, -0.1, 0.1, 0.2, 0.9),
  x2 = c(0.6, 0.1, 0.5, -0.3, 0.01, -0.05, 0.15, -0.3, 0.5, 0.3, 0.7, -0.5, 0.3),
  y = c(-1.2, 0.3, 0.6, -1, 0.1, 0.1, 0.3, -0.7, 0.6, 0.3, 0.1, -0.2, 0.1)
)
# Set seed for reproducibility
set.seed(0)

# Function to fit model and calculate MSE
fit_and_evaluate <- function(hidden_nodes) {
  model <- nnet(y ~ x1 + x2, data = data, size = hidden_nodes, linout = TRUE, trace = FALSE, maxit = 100)
  
  # Calculate empirical risk
  predictions <- predict(model, data)
  mse_emp <- mean((data$y - predictions)^2)
  # Calculate leave-one-out generalization error
  loo_errors <- sapply(1:nrow(data), function(i) {
    loo_data <- data[-i, ]
    loo_model <- nnet(y ~ x1 + x2, loo_data, size = hidden_nodes, linout = TRUE, trace = FALSE, maxit = 100)
    loo_prediction <- predict(loo_model, data[i, , drop = FALSE])
    (data$y[i] - loo_prediction)^2
  })
  mse_loo <- mean(loo_errors)
  
  return(list(MISE_emp = mse_emp, MISE_loo = mse_loo))
}

# Model evaluations
results_model1 <- fit_and_evaluate(1)
results_model2 <- fit_and_evaluate(3)
results_model3 <- fit_and_evaluate(5)

# Output the results
cat("Model 1 with 1 hidden node: MISE_emp =", results_model1$MISE_emp, "MISE_loo =", results_model1$MISE_loo, "\n")
cat("Model 2 with 3 hidden nodes: MISE_emp =", results_model2$MISE_emp, "MISE_loo =", results_model2$MISE_loo, "\n")
cat("Model 3 with 5 hidden nodes: MISE_emp =", results_model3$MISE_emp, "MISE_loo =", results_model3$MISE_loo, "\n")
