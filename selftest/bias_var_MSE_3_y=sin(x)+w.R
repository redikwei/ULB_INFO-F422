## find bias/variance/MSE of different estimators (sin)

## w normal distribution N(μ=0,σ^2=0.25^2)
## y=sinx+w
## N=50 samples (xi, yi) xi equally spaced over [-1, 1]
## estimate θ=E[y|x=-0.75]=sin(-0.75)
## Monte Carlo = 10000
## 1: β0
## 2: β0+β1x
## 3: β0+β1x+β2x^2
## 4: β0+β1x+β2x^2+β3x^3

set.seed(123)  # for reproducibility
n <- 50
simulations <- 10000
true_theta <- sin(-0.75)
x <- seq(-1, 1, length.out = n)

# Storage for estimates
estimates <- matrix(NA, nrow = simulations, ncol = 4)

for (i in 1:simulations) {
  y <- sin(x) + rnorm(n, mean = 0, sd = 0.25)
  
  # Fit the models
  fit1 <- lm(y ~ 1)
  fit2 <- lm(y ~ x)
  fit3 <- lm(y ~ x + I(x^2))
  fit4 <- lm(y ~ x + I(x^2) + I(x^3))
  
  # Predict at x = -0.75
  estimates[i, 1] <- predict(fit1, newdata = data.frame(x = -0.75))
  estimates[i, 2] <- predict(fit2, newdata = data.frame(x = -0.75))
  estimates[i, 3] <- predict(fit3, newdata = data.frame(x = -0.75))
  estimates[i, 4] <- predict(fit4, newdata = data.frame(x = -0.75))
}

# Calculate bias, variance, and MSE
bias <- colMeans(estimates) - true_theta
variance <- apply(estimates, 2, var)
mse <- colMeans((estimates - true_theta)^2)

# Output results
data.frame(Model = 1:4, Bias = abs(bias), Variance = variance, MSE = mse)
