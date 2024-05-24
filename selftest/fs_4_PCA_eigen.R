## FS PCA to compute most eigen-features then compare its MISE

## load figure
## 50 input
## 4 most relevant eigen-features

# Load necessary library
library(stats)

# Load data from .Rdata file
load("D:\\ULB\\9-ULBstudy\\bloc1_sem2\\info_f422_statistical foundation for machine learning\\exam\\selftest\\FS2.Rdata")

# Perform PCA on training data
pca <- prcomp(X, scale. = TRUE)

# Extract the first 4 principal components
X_pca <- pca$x[, 1:4]

# Fit a linear model using all features
model_all <- lm(Y ~ X)
predictions_all <- predict(model_all, newdata = as.data.frame(Xts))
mise_all <- mean((Yts - predictions_all)^2)

# Fit a linear model using the first 4 principal components
model_pca <- lm(Y ~ X_pca)
# Prepare test data with the same principal components
Xts_pca <- predict(pca, newdata = Xts)[, 1:4]
predictions_pca <- predict(model_pca, newdata = as.data.frame(Xts_pca))
mise_pca <- mean((Yts - predictions_pca)^2)

# Print MISE errors
print(paste("MISE with all features:", mise_all))
print(paste("MISE with 4 PCA features:", mise_pca))
