## find bias/variance/MSE of different estimators (Kurtosis)

## normal distribution N(μ=1,σ^2=1^2) Kurtosis of normal is 3
## N=100 samples zi
## Monte Carlo = 10000
## 1: μ/σ
## 2: ∑zi
## 3: ∑(zi-μ)^3/N^3
## 4: ∑(zi-μ)^4/N^4

set.seed(123)  # Ensuring reproducibility
N <- 100       # Sample size
trials <- 10000
mu <- 1
sigma <- 1

# True kurtosis for a normal distribution
true_kurtosis <- 3

# Storage for estimates
estimates <- matrix(NA, nrow = trials, ncol = 4)

for (i in 1:trials) {
  x <- rnorm(N, mean = mu, sd = sigma)
  mu_hat <- mean(x)
  sigma_hat <- sd(x)
  
  # Estimator 1: Mean (not correct for kurtosis)
  estimates[i, 1] <- mu_hat
  
  # Estimator 2: Incorrect use of mean for kurtosis
  estimates[i, 2] <- sum(x / (N-1))
  
  # Estimator 3: Skewness proxy
  estimates[i, 3] <- sum((x - mu_hat)^3) / (N * sigma_hat^3)
  
  # Estimator 4: Excess kurtosis (needs adjustment to match standard kurtosis)
  estimates[i, 4] <- sum((x - mu_hat)^4) / (N * sigma_hat^4) - 3
}

# Calculate bias, variance, and MSE for each estimator
bias <- colMeans(estimates) - true_kurtosis
variance <- apply(estimates, 2, var)
mse <- colMeans((estimates - true_kurtosis)^2)

results <- data.frame(
  Estimator = 1:4,
  Bias = abs(bias),
  Variance = variance,
  MSE = mse
)

print(results)
