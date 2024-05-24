## find empirical risk (with value table)

## x1, x2, y
## N=10 observations
## 1: x1+x2
## 2: -x1
## 3: β0+β1x1+β2x2

# Data
x1 <- c(1.3, -1.2, 0.5, 1.0, 0.3, 0, 0.4, -0.9, -0.2, -0.1)
x2 <- c(0.6, 0.1, 0.5, -0.3, 0.01, -0.05, 0.15, -0.3, 0.5, 0.3)
y <- c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3, -0.7, 0.6, 0.3)

# Fit Model 1
model1 <- lm(y ~ x1 + x2 - 1)  # -1 to fit without an intercept as specified
mse1 <- mean(model1$residuals^2)

# Fit Model 2
model2 <- lm(y ~ x1 + 0)  # + 0 to fit without an intercept and negate x1
model2$coefficients['x1'] <- -model2$coefficients['x1']  # negating the coefficient
mse2 <- mean(model2$residuals^2)

# Fit Model 3
model3 <- lm(y ~ x1 + x2)  # includes an intercept
mse3 <- mean(model3$residuals^2)

# Print MSEs
print(paste("MSE for Model 1:", mse1))
print(paste("MSE for Model 2:", mse2))
print(paste("MSE for Model 3:", mse3))
