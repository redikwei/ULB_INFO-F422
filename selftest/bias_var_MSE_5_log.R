## find bias/variance/MSE of different estimators (log)

## x, y normal distribution N(μ=1, σ^2=1)
## N=100 samples zi
## Monte Carlo = 10000
## 1: log(σ)
## 2: log(max{zi}-min{zi})
## 3: log(∑|zi-μ|/N)
## 4: ∑log|zi-μ|/N

set.seed(123) # for reproducibility

# Number of Monte Carlo simulations
num_simulations <- 10000
# Sample size
N <- 100

# Storage for the estimates
estimates <- matrix(0, nrow = num_simulations, ncol = 4)

# True parameters
mu <- 1
sigma <- 1

for (i in 1:num_simulations) {
  # Generate random sample
  z <- rnorm(N, mean = mu, sd = sigma)
  
  # Compute sample mean and sample variance
  mu_hat <- mean(z)
  sigma2_hat <- var(z)
  
  # Estimators
  estimates[i, 1] <- log(sqrt(sigma2_hat))
  estimates[i, 2] <- log(max(z) - min(z))
  estimates[i, 3] <- log(mean(abs(z - mu_hat)))
  estimates[i, 4] <- mean(log(abs(z)))
}

# True value of log(sigma)
true_value <- log(sigma)

# Compute bias, variance and MSE for each estimator
results <- data.frame(
  Estimator = 1:4,
  Bias = apply(estimates, 2, function(est) mean(est) - true_value),
  Variance = apply(estimates, 2, var),
  MSE = apply(estimates, 2, function(est) mean((est - true_value)^2))
)

# Add absolute value of bias to the results
results$Abs_Bias <- abs(results$Bias)

# Display results
print(results)

# Which estimator has the highest variance?
highest_variance <- which.max(results$Variance)
cat("Estimator with the highest variance:", highest_variance, "\n")

# Which estimator has the highest MSE?
highest_mse <- which.max(results$MSE)
cat("Estimator with the highest MSE:", highest_mse, "\n")

# Which estimator has the lowest MSE?
lowest_mse <- which.min(results$MSE)
cat("Estimator with the lowest MSE:", lowest_mse, "\n")
