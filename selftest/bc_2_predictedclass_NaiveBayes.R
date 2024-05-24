## binary classification predicted class with Naive Bayes and conditional distributions are Normal

## target y {-1, 1}
## [0, 0]T
## [1, 1]T
## [-1, -1]T
## [2, 2]T

# Load necessary packages
library(e1071)

# Load the data
load("D://ULB//9-ULBstudy//bloc1_sem2//info_f422_statistical foundation for machine learning//exam//selftest//EXAM.Rdata")

# Check the structure of the loaded data
str(Q5.G1.D)

# Separate the features and the target variable
features <- Q5.G1.D[, 1:2]
target <- Q5.G1.D[, 3]

# Fit a Naive Bayes model
nb_model <- naiveBayes(features, as.factor(target))

# Define the points to predict
points_to_predict <- data.frame(x1 = c(0, 1, -1, 2), x2 = c(0, 1, -1, 2))

# Predict the class for the given points
predicted_classes <- predict(nb_model, points_to_predict)

# Display the predicted classes
predicted_classes
