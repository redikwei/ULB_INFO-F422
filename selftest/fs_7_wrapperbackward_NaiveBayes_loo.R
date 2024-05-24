## FS wrapper backward selection with Naive Bayes and loo misclassification

## load figure
## 10 binary inputs
## 5 most relevant 
## 第11行是目标行

# Load necessary packages
if(!require(e1071)){
  install.packages("e1071")
  library(e1071)
}

# Load the data
load("D:\\ULB\\9-ULBstudy\\bloc1_sem2\\info_f422_statistical foundation for machine learning\\exam\\selftest\\EXAM.2s.23.Rdata")  # Replace with the correct path

# Check the structure of the loaded data
str(Q3.G1.D)

# Separate the features and the target variable
features <- Q3.G1.D[, -11]
target <- Q3.G1.D[, 11]

# Define a function for leave-one-out cross-validation
loo_cv <- function(data, target, feature_indices) {
  errors <- 0
  for (i in 1:nrow(data)) {
    train_data <- data[-i, feature_indices, drop = FALSE]
    train_target <- target[-i]
    test_data <- data[i, feature_indices, drop = FALSE]
    test_target <- target[i]
    
    model <- naiveBayes(train_data, as.factor(train_target))
    prediction <- predict(model, test_data)
    
    if (prediction != test_target) {
      errors <- errors + 1
    }
  }
  return(errors / nrow(data))
}

# Backward selection process
current_features <- 1:ncol(features)
best_features <- current_features
best_error <- loo_cv(features, target, current_features)

repeat {
  errors <- sapply(current_features, function(feature) {
    subset <- setdiff(current_features, feature)
    loo_cv(features, target, subset)
  })
  
  min_error <- min(errors)
  if (min_error < best_error) {
    best_error <- min_error
    best_feature <- current_features[which.min(errors)]
    best_features <- setdiff(current_features, best_feature)
    current_features <- best_features
  } else {
    break
  }
}

# Select the top 5 features
selected_features <- head(best_features, 5)

# Display the selected features
print(selected_features)