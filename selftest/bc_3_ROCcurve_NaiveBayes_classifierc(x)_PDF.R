## binary classification ROC curve of Naive Bayes / classfier c(x)

## target y {-1, 1}
## c(x) = 1 if x1>h; =-1 else
## 本题在C盘生成了对应的pdf图像
## 最后选的是蓝色的，也就是AUC更高的NB，原因如下
## AUC is performance metric represents classifier's ability to distinguish between pos and neg classes. And ROC curve for NB demonstrates better sensitivity and specificity.

# load the library
library(e1071)
library(pROC)

# Load the data
load("D://ULB//9-ULBstudy//bloc1_sem2//info_f422_statistical foundation for machine learning//exam//selftest//EXAM_2021_1s.Rdata")

# Check the structure of the loaded data
str(Q6.G1.D)

# Separate the features and the target variable
features <- Q6.G1.D[, 1:2]
target <- Q6.G1.D[, 3]

# Fit a Naive Bayes model
nb_model <- naiveBayes(features, as.factor(target))
nb_predictions <- predict(nb_model, features, type = "raw")[, 2]

# Trace the ROC curve for Naive Bayes
roc_nb <- roc(target, nb_predictions)

# Trace the ROC curve for the threshold-based classifier
threshold_based_classifier <- function(features, threshold) {
  return(ifelse(features[, 1] > threshold, 1, -1))
}

roc_threshold <- function(target, features) {
  thresholds <- sort(unique(features[, 1]))
  tpr <- numeric(length(thresholds))
  fpr <- numeric(length(thresholds))
  
  for (i in 1:length(thresholds)) {
    predictions <- threshold_based_classifier(features, thresholds[i])
    confusion <- table(factor(predictions, levels = c(-1, 1)), factor(target, levels = c(-1, 1)))
    
    tpr[i] <- confusion["1", "1"] / sum(target == 1)
    fpr[i] <- confusion["1", "-1"] / sum(target == -1)
  }
  
  return(data.frame(thresholds = thresholds, tpr = tpr, fpr = fpr))
}

roc_data_threshold <- roc_threshold(target, features)

# Plot the ROC curves
plot(roc_nb, col = "blue", main = "ROC Curves", lwd = 2)
lines(roc_data_threshold$fpr, roc_data_threshold$tpr, col = "red", lwd = 2)
legend("bottomright", legend = c("Naive Bayes", "Threshold-based"),
       col = c("blue", "red"), lwd = 2)

# Print the AUC values
cat("AUC for Naive Bayes:", auc(roc_nb), "\n")
cat("AUC for Threshold-based classifier:", auc(roc_data_threshold$fpr, roc_data_threshold$tpr), "\n")

pdf("ROC_Curves.pdf")
plot(roc_nb, col = "blue", main = "ROC Curves", lwd = 2)
lines(roc_data_threshold$fpr, roc_data_threshold$tpr, col = "red", lwd = 2)
legend("bottomright", legend = c("Naive Bayes", "Threshold-based"),
       col = c("blue", "red"), lwd = 2)
dev.off()