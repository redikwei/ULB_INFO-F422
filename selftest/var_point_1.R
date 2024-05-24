## find variance at certain point (with value table)

## x1, x2, x3, y
## y=1+0.1x1-0.1x2+x3+w
## w normal random variable with zero mean and uniti variance
## y=β0+β1x1+β2x2+β3x3         all β estimated by least squares
## variance prediction y in point (1,1,1)
## Monte Carlo 10000

# Required Libraries
library(MASS)  # For ginv() if needed

# Given data
x1 <- c(-1, -0.5, 0.5, 1, -1, -0.5, 1, 0.5, -1, -0.5, 0.5, 1)
x2 <- c(-1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 1, 1)
x3 <- c(-1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1)

# Design matrix
X <- cbind(1, x1, x2, x3)  # Include intercept
x0 <- c(1, 1, 1, 1)  # New data point (1,1,1)

# Variance of error
sigma2 <- 1

# Calculate (X'X)^-1
XtX_inv <- solve(t(X) %*% X)

# Variance of prediction at (1, 1, 1)
var_y_hat <- sigma2 * (1 + t(x0) %*% XtX_inv %*% x0)

# Output the calculated variance
print(var_y_hat)

#### Monte Carlo Simulation to Verify the Variance
set.seed(123)
n <- 10000  # Number of simulations
predictions <- numeric(n)

for (i in 1:n) {
  w <- rnorm(12, mean = 0, sd = 1)  # Generate new w for each simulation
  y <- 1 + 0.1 * x1 - 0.1 * x2 + x3 + w  # Using the specified model
  fit <- lm(y ~ x1 + x2 + x3)  # Fit model
  predictions[i] <- predict(fit, newdata = data.frame(x1 = 1, x2 = 1, x3 = 1))
}

# Empirical variance of the predictions
emp_var_y_hat <- var(predictions)
print(emp_var_y_hat)
