## find KNN K=7 nearest neighbour emp/loo/q=[0,0]

## load figure
## 7邻域是由欧拉距离给出的
## 输入为x目标函数为y
## 最后邻域的选取是求的loo的最小值

# Load required libraries
library(class)  # For KNN
library(FNN)    # For KNN regression

# Load the dataset from an .RData file
load("D:/ULB/9-ULBstudy/bloc1_sem2/info_f422_statistical foundation for machine learning/exam/selftest/EXAM_2021_2s.Rdata")  # Adjust the path as needed
data <- Q4.G1.D
# Convert matrix to dataframe
data <- as.data.frame(data)

# Now you can access columns using $ operator
print("Structure of 'data' after conversion:")
print(str(data))

# Proceed with your existing functions


# Check what data was loaded
print("Objects loaded:")
print(ls())

# Check the structure of the dataframe 'data'
print("Structure of 'data':")
print(str(data))

# Assuming data is correctly loaded and structured
# Function to compute squared errors
compute_errors <- function(data, k) {
  # Empirical risk calculation (Training error)
  predictions_train <- knn.reg(train = data[, c("x1", "x2")], y = data$y, k = k)$pred
  train_error <- mean((predictions_train - data$y)^2)
  
  # Generalization error using leave-one-out cross-validation
  loo_error <- 0
  n <- nrow(data)
  for (i in 1:n) {
    loo_train <- data[-i, ]
    loo_test <- data[i, ]
    prediction <- knn.reg(loo_train[, c("x1", "x2")], loo_test[, c("x1", "x2")], y = loo_train$y, k = k)$pred
    loo_error <- loo_error + (prediction - loo_test$y)^2
  }
  loo_error <- loo_error / n
  
  # Returning both errors
  return(c(train_error, loo_error))
}

# Empirical and LOO errors for k from 1 to 7
results <- sapply(1:7, function(k) compute_errors(data, k))
rownames(results) <- c("Empirical Risk", "LOO Error")
colnames(results) <- paste("K =", 1:7)

# Display results
print("Empirical Risks and LOO Errors for each K:")
print(results)

# Predict the output at q = [0, 0]
q <- data.frame(x1 = 0, x2 = 0)
predictions_at_q <- sapply(1:7, function(k) {
  knn.reg(data[, c("x1", "x2")], q, y = data$y, k = k)$pred
})

# Display predictions
print("Predictions at q = [0, 0] for each K:")
print(predictions_at_q)

# Recommendation based on LOO Errors
recommended_k <- which.min(results["LOO Error", ])
cat(sprintf("Recommended number of neighbors (K) based on lowest LOO error: K = %d\n", recommended_k))
