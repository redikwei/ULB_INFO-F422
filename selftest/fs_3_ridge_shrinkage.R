## FS ridge-regression(loo assessment strategy) to optimal shrinkage parameter

## load figure
## 50 input
## 计算结果为1.52选的是1, 计算结果为0.16选0

# Load libraries
library(glmnet)
library(caret)

# Load your data
load("D:/ULB/9-ULBstudy/bloc1_sem2/info_f422_statistical foundation for machine learning/exam/selftest/FS2.Rdata")  # Update path to where your .RData file is stored

# Assuming 'X' and 'Y' are loaded correctly and are the names of your matrices
# Set up your data (make sure 'X' is a matrix and 'Y' is a vector or matrix as appropriate)

# Define the sequence of lambda values (optional, glmnet will also generate its sequence)
lambda_sequence <- 10^seq(10, -2, length = 100)

# Run leave-one-out cross-validation for ridge regression
cv_fit <- cv.glmnet(X, Y, alpha=0, lambda=lambda_sequence, type.measure="mse", nfolds=nrow(X))

# Extract the optimal lambda
optimal_lambda <- cv_fit$lambda.min

# Print optimal lambda
print(optimal_lambda)
