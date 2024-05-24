## binary classification misclassified points with linear decision boundary

## target y {-1, 1}
## linear decision boundary (xT)β+β0=0
## linear perceptron β0=0 β=[-1,1]T
## number of misclassified points --> gradient based learning rate 0.01 number of mis points

# Load necessary packages
library(e1071)

# Load the data
load("D://ULB//9-ULBstudy//bloc1_sem2//info_f422_statistical foundation for machine learning//exam//selftest//EXAM.Rdata")

# Check the structure of the loaded data
str(Q6.G1.D)

# Separate the features and the target variable
features <- Q6.G1.D[, 1:2]
target <- Q6.G1.D[, 3]

# Initial parameters
beta <- c(-1, 1)  # Initial beta
beta0 <- 0  # Initial beta0
learning_rate <- 0.01

# Define a function to calculate the number of misclassified points
calculate_misclassified <- function(features, target, beta, beta0) {
  predictions <- ifelse((features %*% beta + beta0) >= 0, 1, -1)
  sum(predictions != target)
}

# Calculate the number of misclassified points with initial parameters
initial_misclassified <- calculate_misclassified(features, target, beta, beta0)
cat("Number of misclassified points initially:", initial_misclassified, "\n")

# Define a function to perform a single step of gradient descent
gradient_descent_step <- function(features, target, beta, beta0, learning_rate) {
  misclassified_indices <- which((features %*% beta + beta0) * target <= 0)
  
  for (i in misclassified_indices) {
    beta <- beta + learning_rate * target[i] * features[i, ]
    beta0 <- beta0 + learning_rate * target[i]
  }
  
  return(list(beta = beta, beta0 = beta0))
}

# Perform gradient descent and calculate the number of misclassified points after each step
steps <- 4
misclassified_points <- numeric(steps)

for (step in 1:steps) {
  result <- gradient_descent_step(features, target, beta, beta0, learning_rate)
  beta <- result$beta
  beta0 <- result$beta0
  misclassified_points[step] <- calculate_misclassified(features, target, beta, beta0)
  cat("Number of misclassified points after", step, "step(s):", misclassified_points[step], "\n")
}
