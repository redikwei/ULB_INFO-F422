## find bias/variance/MSE of different estimators

## normal distribution N(μ=0,σ^2=2^2)
## N=100 samples zi
## Monte Carlo = 10000
## 1: σ
## 2: zi ∑最大-∑最小
## 3: ∑|zi-μ|/N
## 4: median zi

set.seed(123)  # Ensuring reproducibility
N <- 100       # Sample size
trials <- 10000
sigma <- 2     # True sigma

# Storage for estimates
estimates <- matrix(NA, nrow = trials, ncol = 4)

for (i in 1:trials) {
  x <- rnorm(N, mean = 0, sd = sigma)
  
  # Estimator 1: Sample standard deviation
  estimates[i, 1] <- sd(x)
  
  # Estimator 2: Range
  estimates[i, 2] <- max(x) - min(x)
  
  # Estimator 3: Variance using N, then take square root
  estimates[i, 3] <- sqrt(sum((x - mean(x))^2) / N)
  
  # Estimator 4: Median (not a correct estimator of sigma)
  estimates[i, 4] <- median(x)  # This is conceptually incorrect but included for completion
}

# Calculate bias, variance, and MSE for each estimator
bias <- colMeans(estimates) - sigma
variance <- apply(estimates, 2, var)
mse <- colMeans((estimates - sigma)^2)

results <- data.frame(
  Estimator = 1:4,
  Bias = abs(bias),
  Variance = variance,
  MSE = mse
)

print(results)
