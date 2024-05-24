## training and testing MISE

## load figure
## 2 inputs 1 output
## RBF with M=4 σ^2=1 ρm(x)=exp(-|x-μm|^2/σ^2)
## 1: μ1=[1,1]T
## 2: μ2=[1,-1]T
## 3: μ3=[-1,1]T
## 4: μ4=[-1,-1]T

# Load necessary library
library(MASS)  # For ginv (generalized inverse function)

# Load the data
load("D:/ULB/9-ULBstudy/bloc1_sem2/info_f422_statistical foundation for machine learning/exam/selftest/UV.NL.Rdata")

# Define the RBF function
rbf <- function(x, centers, sigma_squared) {
  # Ensuring x is a matrix and centers is transposed correctly for subtraction
  x <- matrix(x, nrow = 1)  # Make sure x is a row matrix
  distances <- apply(centers, 1, function(center) sum((x - center)^2))
  exp(-distances / (2 * sigma_squared))
}

# Centers and sigma squared
centers <- matrix(c(1, 1, 1, -1, -1, 1, -1, -1), ncol = 2, byrow = TRUE)
sigma_squared <- 1

# Training and testing sets
X_train <- as.matrix(df2tr[, 1:2])  # Assuming the first two columns are features
y_train <- df2tr[, 3]  # Assuming the third column is the output
X_test <- as.matrix(df2ts[, 1:2])
y_test <- df2ts[, 3]

# Calculate RBFs for training and testing data
phi_train <- t(apply(X_train, 1, function(x) rbf(x, centers, sigma_squared)))
phi_test <- t(apply(X_test, 1, function(x) rbf(x, centers, sigma_squared)))

# Fit the model using least squares
beta <- ginv(t(phi_train) %*% phi_train) %*% t(phi_train) %*% y_train

# Predictions
predictions_train <- phi_train %*% beta
predictions_test <- phi_test %*% beta

# Calculate MISE
mise_train <- mean((y_train - predictions_train)^2)
mise_test <- mean((y_test - predictions_test)^2)

# Output results
cat("Training MISE:", mise_train, "\n")
cat("Test MISE:", mise_test, "\n")
