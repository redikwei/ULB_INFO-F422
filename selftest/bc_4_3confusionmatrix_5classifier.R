## binary classification confusion matrix / most accurate classifier

## target y {-1, 1}
## 1: 1 if x1>0; -1 else
## 2: 1 if x1x2<0; -1 else
## 3: 1 if x2<0; -1 else
## real/predicted class = 1 / -1
## most accurate classifier MisclassificationRate/BalancedMisclassificationRate/Precision/TruePositiveRate/TrueNegativeRate

# Load necessary packages
library(e1071)

# Load the data
load("D://ULB//9-ULBstudy//bloc1_sem2//info_f422_statistical foundation for machine learning//exam//selftest//EXAM_2021_1s.Rdata")

# Check the structure of the loaded data
str(Q5.G1.D)

# Separate the features and the target variable
features <- Q5.G1.D[, 1:2]
target <- Q5.G1.D[, 3]

# Define the classifiers
classifier1 <- function(x) {
  ifelse(x[, 1] > 0, 1, -1)
}

classifier2 <- function(x) {
  ifelse(x[, 1] * x[, 2] < 0, 1, -1)
}

classifier3 <- function(x) {
  ifelse(x[, 2] < 0, 1, -1)
}

# Predict the classes using the classifiers
pred1 <- classifier1(features)
pred2 <- classifier2(features)
pred3 <- classifier3(features)

# Compute confusion matrices
conf_matrix1 <- table(pred1, target)
conf_matrix2 <- table(pred2, target)
conf_matrix3 <- table(pred3, target)

# Print confusion matrices
cat("Confusion Matrix for Classifier 1:\n")
print(conf_matrix1)
cat("\nConfusion Matrix for Classifier 2:\n")
print(conf_matrix2)
cat("\nConfusion Matrix for Classifier 3:\n")
print(conf_matrix3)

# Calculate performance metrics
misclassification_rate <- function(conf_matrix) {
  (conf_matrix["1", "-1"] + conf_matrix["-1", "1"]) / sum(conf_matrix)
}

balanced_misclassification_rate <- function(conf_matrix) {
  0.5 * (conf_matrix["1", "-1"] / sum(conf_matrix[, "-1"]) + conf_matrix["-1", "1"] / sum(conf_matrix[, "1"]))
}

precision <- function(conf_matrix) {
  conf_matrix["1", "1"] / sum(conf_matrix["1", ])
}

true_positive_rate <- function(conf_matrix) {
  conf_matrix["1", "1"] / sum(conf_matrix[, "1"])
}

true_negative_rate <- function(conf_matrix) {
  conf_matrix["-1", "-1"] / sum(conf_matrix[, "-1"])
}

metrics <- list(
  misclassification_rate = sapply(list(conf_matrix1, conf_matrix2, conf_matrix3), misclassification_rate),
  balanced_misclassification_rate = sapply(list(conf_matrix1, conf_matrix2, conf_matrix3), balanced_misclassification_rate),
  precision = sapply(list(conf_matrix1, conf_matrix2, conf_matrix3), precision),
  true_positive_rate = sapply(list(conf_matrix1, conf_matrix2, conf_matrix3), true_positive_rate),
  true_negative_rate = sapply(list(conf_matrix1, conf_matrix2, conf_matrix3), true_negative_rate)
)

# Print metrics
print(metrics)

# Determine the best classifier based on each metric
best_classifier <- sapply(metrics, function(metric) which.min(metric))
best_classifier["precision"] <- which.max(metrics$precision)
best_classifier["true_positive_rate"] <- which.max(metrics$true_positive_rate)
best_classifier["true_negative_rate"] <- which.max(metrics$true_negative_rate)

# Print the best classifiers
cat("\nBest classifiers based on different metrics:\n")
print(best_classifier)
