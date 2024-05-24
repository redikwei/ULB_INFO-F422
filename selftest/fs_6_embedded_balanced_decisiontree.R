## FS embedded where depth=2 balanced decision tree, split each variable using values in seq(-2,2,by=0.5)

## load figure
## 20 input
## 3 most relevant 

# Load the library
library(rpart)

# Load the data
load("D:/ULB/9-ULBstudy/bloc1_sem2/info_f422_statistical foundation for machine learning/exam/selftest/FS2.Rdata")

# Check the structure of the loaded data
str(X)
str(Y)

# Combine the data into a single data frame
data <- data.frame(X, Y = Y)

# Fit a decision tree model with depth 2
tree_model <- rpart(Y ~ ., data = data, control = rpart.control(maxdepth = 2))

# Print the summary of the tree model
summary(tree_model)

# Extract the variable importance
importance <- tree_model$variable.importance

# Select the top 3 most important features
top_features <- names(sort(importance, decreasing = TRUE))[1:3]

# Display the top 3 features
print(top_features)