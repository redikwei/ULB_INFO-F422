## find empirical risk and generalisation leave-one-out (with value table)

## x1, x2, y
## N=10 observations
## 1: β0
## 2: β0+β1x1
## 3: β0+β1x1+β2x2

# Load necessary library
library(boot)

# Data
x1 <- c(1.3, -1.2, 0.5, 1.0, 0.3, 0, 0.4, -0.9, -0.2, -0.1)
x2 <- c(0.6, 0.1, 0.5, -0.3, 0.01, -0.05, 0.15, -0.3, 0.5, 0.3)
y <- c(-1.2, 0.3, 0.6, -1, 0, 0.1, 0.3, -0.7, 0.6, 0.3)

# Define data frame
data <- data.frame(x1, x2, y)

# Fit Model 1
model1 <- lm(y ~ 1, data = data)
mse_emp1 <- mean(model1$residuals^2)
mse_loo1 <- sum((residuals(model1) / (1 - lm.influence(model1)$hat))^2) / length(y)

# Fit Model 2
model2 <- lm(y ~ x1, data = data)
mse_emp2 <- mean(model2$residuals^2)
mse_loo2 <- sum((residuals(model2) / (1 - lm.influence(model2)$hat))^2) / length(y)

# Fit Model 3
model3 <- lm(y ~ x1 + x2, data = data)
mse_emp3 <- mean(model3$residuals^2)
mse_loo3 <- sum((residuals(model3) / (1 - lm.influence(model3)$hat))^2) / length(y)

# Output results
print(paste("MSE_emp for Model 1:", mse_emp1))
print(paste("MSE_loo for Model 1:", mse_loo1))
print(paste("MSE_emp for Model 2:", mse_emp2))
print(paste("MSE_loo for Model 2:", mse_loo2))
print(paste("MSE_emp for Model 3:", mse_emp3))
print(paste("MSE_loo for Model 3:", mse_loo3))

# Determine which model has the lowest empirical risk and LOO error
models <- c("Model 1", "Model 2", "Model 3")
min_emp <- models[which.min(c(mse_emp1, mse_emp2, mse_emp3))]
min_loo <- models[which.min(c(mse_loo1, mse_loo2, mse_loo3))]

print(paste("Model with the lowest empirical risk:", min_emp))
print(paste("Model with the lowest LOO error:", min_loo))
