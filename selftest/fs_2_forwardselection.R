## FS 5/20 linear least-squares and leave-one-out forward selection algo

## load figure
## compare MISE of 5 and 20
## 5的MISE降低了说明FS是有用的
## 20 input

library(MASS)

# Load the data
load("D:/ULB/9-ULBstudy/bloc1_sem2/info_f422_statistical foundation for machine learning/exam/selftest/FS.Rdata")

# Assuming X and Y are loaded directly
# Split data manually for demonstration purposes (adjust according to your specific case)
set.seed(123)  # for reproducibility
indices <- sample(1:nrow(X), round(0.8 * nrow(X)))
train_X <- X[indices, ]
train_Y <- Y[indices]
test_X <- X[-indices, ]
test_Y <- Y[-indices]

# Convert matrices to data frames and add column names for model fitting
train_X_df <- as.data.frame(train_X)
test_X_df <- as.data.frame(test_X)
names(train_X_df) <- paste0("V", 1:ncol(train_X))
names(test_X_df) <- names(train_X_df)

# Fit a full model with all predictors
fit_full <- lm(train_Y ~ ., data = train_X_df)
predict_full <- predict(fit_full, newdata = test_X_df)
MSE_full <- mean((test_Y - predict_full)^2)

# Forward selection using AIC from a minimal model
fit_reduced <- stepAIC(lm(train_Y ~ 1, data = train_X_df), scope = list(lower = ~1, upper = as.formula(paste("train_Y ~", paste(names(train_X_df), collapse = "+")))), direction = "forward", trace = FALSE)
predict_reduced <- predict(fit_reduced, newdata = test_X_df)
MSE_reduced <- mean((test_Y - predict_reduced)^2)

# Output results
print(c(MSE_full = MSE_full, MSE_reduced = MSE_reduced))
