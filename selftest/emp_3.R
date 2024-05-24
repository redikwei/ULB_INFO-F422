## find empirical risk (with value table)

## x, y
## N=8 observations
## 1: x
## 2: -x1
## 3: β0+β1x

# Data
x <- c(1.3, -1.2, 0.5, 1.0, 0.3, 0, 0.4, -0.9)
y <- c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3, -0.7)

# Define data frame
data <- data.frame(x, y)

# Fit Model 1
model1 <- lm(y ~ x + 0, data = data)  # +0 to fit without an intercept
mse1 <- mean(model1$residuals^2)

# Fit Model 2
model2 <- lm(y ~ I(-x) + 0, data = data)  # Negate x and fit without an intercept
mse2 <- mean(model2$residuals^2)

# Fit Model 3
model3 <- lm(y ~ x, data = data)  # Standard linear model with an intercept
mse3 <- mean(model3$residuals^2)

# Output results
print(paste("MSE_emp for Model 1:", mse1))
print(paste("MSE_emp for Model 2:", mse2))
print(paste("MSE_emp for Model 3:", mse3))
