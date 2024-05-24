## find var/bias^2

## y=sinx+w
## w normal random variable zero mean and 0.5 variance
## x uniform variable [-1,1]
## 20 observations
## estimate regression function in x=0
## Monte Carlo 10000
## 1: β0+β1x
## 2: β0+β1x+β2x^2

# Load necessary library
library(MASS)  # For ginv (generalized inverse function)

# Define number of trials for Monte Carlo simulation
n_trials <- 10000
N <- 20  # Number of observations per trial

# Initialize vectors to store prediction results
predictions_M1 <- numeric(n_trials)
predictions_M2 <- numeric(n_trials)

# True function
true_y <- function(x) sin(x)

# Monte Carlo Simulation
set.seed(123)  # For reproducibility
for (i in 1:n_trials) {
  # Generate data
  x <- runif(N, -1, 1)  # Uniformly distributed x values
  w <- rnorm(N, mean = 0, sd = sqrt(0.5))  # Normal noise
  y <- true_y(x) + w  # Response variable
  
  # Fit Model 1: y = beta0 + beta1*x
  X1 <- cbind(1, x)
  beta1 <- ginv(t(X1) %*% X1) %*% t(X1) %*% y
  predictions_M1[i] <- sum(beta1 * c(1, 0))  # Predict at x = 0
  
  # Fit Model 2: y = beta0 + beta1*x + beta2*x^2
  X2 <- cbind(1, x, x^2)
  beta2 <- ginv(t(X2) %*% X2) %*% t(X2) %*% y
  predictions_M2[i] <- sum(beta2 * c(1, 0, 0))  # Predict at x = 0
}

# Calculate variance and bias for each model
var_M1 <- var(predictions_M1)
bias_squared_M1 <- (mean(predictions_M1) - true_y(0))^2

var_M2 <- var(predictions_M2)
bias_squared_M2 <- (mean(predictions_M2) - true_y(0))^2

# Print the results
cat("Model M1:\n")
cat(sprintf("Variance: %f\n", var_M1))
cat(sprintf("Bias^2: %f\n", bias_squared_M1))

cat("\nModel M2:\n")
cat(sprintf("Variance: %f\n", var_M2))
cat(sprintf("Bias^2: %f\n", bias_squared_M2))