## find empirical risk/generalisation leave-one-out/FPE

## load figure
## 不让用lm指令
## y, x
## 1: β0
## 2: β0+β1x1+β2x2
## 3: β0+β1x1
## 4: β0+β1x1^2+β2x2^2
## 5: β0+β1x1+β2x2+β3x1^2+β4x2^2+β5x1x2+β6x1^3+β7x2^3
## 6: β0+β1x1^2
## 7: β0+β1x2^2
## 8: β0+β1x1^3
## 9: β0+β1x1^2+β2x2^2+β3x1x2

# Load the necessary library for matrix calculations
library(MASS)  # for ginv() function, which provides a generalized inverse

# Sample data input
load("D:\\ULB\\9-ULBstudy\\bloc1_sem2\\info_f422_statistical foundation for machine learning\\exam\\selftest\\EXAM_2021_1s.Rdata")
data <- Q4.G2.D

# Check the structure of data
str(data)

# Convert to a dataframe if it is not already
if (!is.data.frame(data)) {
  data <- as.data.frame(data)
  # Assign names if necessary
  names(data) <- c("x1", "x2", "y")  # Adjust these names according to actual column indices or names
}

# Continue with your computations as planned

# Function to compute coefficients
compute_coefficients <- function(X, y) {
  # Compute beta using the formula beta = (X'X)^(-1)X'y
  beta <- ginv(t(X) %*% X) %*% t(X) %*% y
  return(beta)
}

# Function to compute predictions
predict_values <- function(X, beta) {
  predictions <- X %*% beta
  return(predictions)
}

# Function to compute empirical risk
get_empirical_risk <- function(predictions, actual) {
  mse <- mean((predictions - actual) ^ 2)
  return(mse)
}

# Function to compute FPE
get_fpe <- function(predictions, actual, p) {
  n <- length(actual)
  mse <- mean((predictions - actual)^2)
  fpe <- mse * ((n + p) / (n - p))
  return(fpe)
}

# Function to compute LOOCV MSE
compute_loocv_mse <- function(X, y) {
  mse_values <- numeric(nrow(X))
  
  for (i in 1:nrow(X)) {
    # Leave out the ith observation
    X_train <- X[-i, , drop = FALSE]
    y_train <- y[-i]
    X_test <- X[i, , drop = FALSE]
    
    # Compute coefficients and predictions for the left-out observation
    beta <- compute_coefficients(X_train, y_train)
    predictions <- predict_values(X_test, beta)
    
    # Calculate the squared error for the left-out observation
    mse_values[i] <- (predictions - y[i])^2
  }
  
  # Return the mean of the squared errors
  return(mean(mse_values))
}

# Prepare matrix X for each model and calculate risks
# Model 1: Constant model
X1 <- matrix(1, nrow(data), 1)
beta_1 <- compute_coefficients(X1, data$y)
predictions_1 <- predict_values(X1, beta_1)
risk_1 <- get_empirical_risk(predictions_1, data$y)
fpe_1 <- get_fpe(predictions_1, data$y, ncol(X1))
loocv_mse_1 <- compute_loocv_mse(X1, data$y)
beta_0_model_1 <- beta_1[1]

# Model 2: Linear terms only
X2 <- cbind(1, data$x1, data$x2)
beta_2 <- compute_coefficients(X2, data$y)
predictions_2 <- predict_values(X2, beta_2)
risk_2 <- get_empirical_risk(predictions_2, data$y)
fpe_2 <- get_fpe(predictions_2, data$y, ncol(X2))
loocv_mse_2 <- compute_loocv_mse(X2, data$y)

# Model 3: x1 linear
X3 <- cbind(1, data$x1)
beta_3 <- compute_coefficients(X3, data$y)
predictions_3 <- predict_values(X3, beta_3)
risk_3 <- get_empirical_risk(predictions_3, data$y)
fpe_3 <- get_fpe(predictions_3, data$y, ncol(X3))
loocv_mse_3 <- compute_loocv_mse(X3, data$y)

# Model 4: x1 and x2 squared
X4 <- cbind(1, data$x1^2, data$x2^2)
beta_4 <- compute_coefficients(X4, data$y)
predictions_4 <- predict_values(X4, beta_4)
risk_4 <- get_empirical_risk(predictions_4, data$y)
fpe_4 <- get_fpe(predictions_4, data$y, ncol(X4))
loocv_mse_4 <- compute_loocv_mse(X4, data$y)

# Model 5: All up to x2 squared and interactions
X5 <- cbind(1, data$x1, data$x2, data$x1^2, data$x2^2, data$x1 * data$x2)
beta_5 <- compute_coefficients(X5, data$y)
predictions_5 <- predict_values(X5, beta_5)
risk_5 <- get_empirical_risk(predictions_5, data$y)
fpe_5 <- get_fpe(predictions_5, data$y, ncol(X5))
loocv_mse_5 <- compute_loocv_mse(X5, data$y)

# Model 6: x1 squared
X6 <- cbind(1, data$x1^2)
beta_6 <- compute_coefficients(X6, data$y)
predictions_6 <- predict_values(X6, beta_6)
risk_6 <- get_empirical_risk(predictions_6, data$y)
fpe_6 <- get_fpe(predictions_6, data$y, ncol(X6))
loocv_mse_6 <- compute_loocv_mse(X6, data$y)

# Model 7: x2 squared
X7 <- cbind(1, data$x2^2)
beta_7 <- compute_coefficients(X7, data$y)
predictions_7 <- predict_values(X7, beta_7)
risk_7 <- get_empirical_risk(predictions_7, data$y)
fpe_7 <- get_fpe(predictions_7, data$y, ncol(X7))
loocv_mse_7 <- compute_loocv_mse(X7, data$y)

# Model 8: x1 and x2 cubed
X8 <- cbind(1, data$x1^3, data$x2^3)
beta_8 <- compute_coefficients(X8, data$y)
predictions_8 <- predict_values(X8, beta_8)
risk_8 <- get_empirical_risk(predictions_8, data$y)
fpe_8 <- get_fpe(predictions_8, data$y, ncol(X8))
loocv_mse_8 <- compute_loocv_mse(X8, data$y)

# Model 9: x1 and x2 up to cubic including interaction
X9 <- cbind(1, data$x1, data$x2, data$x1^2, data$x2^2, data$x1 * data$x2, data$x1^3, data$x2^3)
beta_9 <- compute_coefficients(X9, data$y)
predictions_9 <- predict_values(X9, beta_9)
risk_9 <- get_empirical_risk(predictions_9, data$y)
fpe_9 <- get_fpe(predictions_9, data$y, ncol(X9))
loocv_mse_9 <- compute_loocv_mse(X9, data$y)
beta_0_model_9 <- beta_9[1]

# Print the empirical risks, FPE, and LOOCV MSE for all models
cat("Empirical Risks / FPE / LOOCV MSE for each model:\n")
cat(sprintf("Model 1: Risk = %f, FPE = %f, LOOCV MSE = %f\n", risk_1, fpe_1, loocv_mse_1))
cat(sprintf("Model 2: Risk = %f, FPE = %f, LOOCV MSE = %f\n", risk_2, fpe_2, loocv_mse_2))
cat(sprintf("Model 3: Risk = %f, FPE = %f, LOOCV MSE = %f\n", risk_3, fpe_3, loocv_mse_3))
cat(sprintf("Model 4: Risk = %f, FPE = %f, LOOCV MSE = %f\n", risk_4, fpe_4, loocv_mse_4))
cat(sprintf("Model 5: Risk = %f, FPE = %f, LOOCV MSE = %f\n", risk_5, fpe_5, loocv_mse_5))
cat(sprintf("Model 6: Risk = %f, FPE = %f, LOOCV MSE = %f\n", risk_6, fpe_6, loocv_mse_6))
cat(sprintf("Model 7: Risk = %f, FPE = %f, LOOCV MSE = %f\n", risk_7, fpe_7, loocv_mse_7))
cat(sprintf("Model 8: Risk = %f, FPE = %f, LOOCV MSE = %f\n", risk_8, fpe_8, loocv_mse_8))
cat(sprintf("Model 9: Risk = %f, FPE = %f, LOOCV MSE = %f\n", risk_9, fpe_9, loocv_mse_9))
cat("The estimate of beta_0 for Model 1 is:", beta_0_model_1, "\n")
cat("The estimate of beta_0 for Model 9 is:", beta_0_model_9, "\n")
