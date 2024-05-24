## FS mRMR filter where mutual info about Pearson correlation

## load figure
## 20 input
## 4 most relevant 

# Load necessary packages
library(mRMRe)

# Load the data
load("D:/ULB/9-ULBstudy/bloc1_sem2/info_f422_statistical foundation for machine learning/exam/selftest/FS.Rdata")

# Check the structure of the loaded data
str(X)
str(Y)

# Convert the data to mRMRe.Data object
data <- mRMR.data(data = data.frame(X, Y))

# Set the target index (assuming Y is the target variable and is the last column)
target_index <- ncol(X) + 1

# Perform mRMR filter to find the 4 most relevant features
feature_selection <- mRMR.classic(data = data, target_indices = target_index, feature_count = 4)

# Get the indices of the 4 selected features
selected_features <- solutions(feature_selection)

# Display the indices of the 4 most relevant features
print(selected_features)

# Convert indices to feature names if needed
feature_names <- colnames(X)[selected_features[[1]]]
print(feature_names)

