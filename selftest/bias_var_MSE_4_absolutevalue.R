## find bias/variance/MSE of different estimators (absolute value)

## plot sampling distributions
## x, y uniform distribution U(0,1)
## N=50 samples (xi, yi)
## estimate θ=E[|x-y|]=1/3
## Monte Carlo = 10000
## 1: ∑|xi-yi|/N
## 2: ∑(|xi|-|yi|)/N
## 3: |∑xi/2N|+|∑yi/2N|
## 4: |∑(xi-yi)/N|

set.seed(123)  # for reproducibility
N <- 50        # sample size
trials <- 10000
true_theta <- 1/3
# Store results
results <- matrix(NA, nrow = trials, ncol = 4)
for (i in 1:trials) {
  x <- runif(N)
  y <- runif(N)
  diff <- abs(x - y)
  
  # Estimator 1: Simple mean of differences
  results[i, 1] <- mean(diff)
  
  # Estimator 2: Mean of differences adjusted by sample mean of diff (incorrect application)
  results[i, 2] <- mean(diff - mean(diff))
  
  # Estimator 3: Mean of x plus mean of y
  results[i, 3] <- mean(x) + mean(y)
  
  # Estimator 4: Mean of differences with N-1 normalization
  results[i, 4] <- sum(diff) / (N - 1)
}

# Calculate bias, variance, and MSE
bias <- colMeans(results) - true_theta
variance <- apply(results, 2, var)
mse <- rowMeans((results - true_theta)^2)
# Create summary data frame
summary_df <- data.frame(
  Estimator = 1:4,
  Bias = abs(bias),
  Variance = variance,
  MSE = mse
)

print(summary_df)

# Plot distributions
par(mfrow=c(2,2))
for (j in 1:4) {
  hist(results[, j], main = paste("Estimator", j), xlab = "Value", breaks = 30, col = "skyblue")
}
