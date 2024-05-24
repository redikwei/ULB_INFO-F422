## FS 5/20 correlation ranking algo 

## load figure
## 20 input

# Load the data
load("D:/ULB/9-ULBstudy/bloc1_sem2/info_f422_statistical foundation for machine learning/exam/selftest/FS.Rdata")

# Check the structure of X and Y
print(str(X))
print(str(Y))

# Assuming X is a matrix or dataframe and Y is a vector, calculate correlations
correlations <- cor(X, Y)

# Rank features by absolute correlation value
feature_rank <- sort(abs(correlations), decreasing = TRUE)

# Extract indices of the top 5 features (since no column names)
top_features_indices <- order(abs(correlations), decreasing = TRUE)[1:5]

# Print the indices of the top 5 features
print(top_features_indices)
