## binary classification ROC abd PrecisionRecall curve / 5 and 10 nearest neighbors

## target y {-1, 1}
## ROC和PR图都是谁高选谁，且本题也在C盘最后生成了PDF图像
## ROC谁比谁高可以用better trade-off between TPR and FPR, PR就直接better trade-off
## estimate conditional posterior probability of target方法：
## 1 --> calculate distance, for each test point, the Euclidean deistance to all training points
## 2 --> find nearest neighbors, identify k-nearest neighbors based on smallest distances
## 3 --> calculate posterior probability as proportion of k-nearest neighborrs that belong to positive class (target=1)

# Load the data
load("D://ULB//9-ULBstudy//bloc1_sem2//info_f422_statistical foundation for machine learning//exam//selftest//EXAM_2122_2s.Rdata")

# Check the structure of the loaded data
str(Q4.G2.D)

# Separate the features and the target variable
features <- Q4.G2.D[, 1:2]
target <- Q4.G2.D[, 3]

# Implement the KNN classifier
knn_classifier <- function(train_features, train_target, test_features, k) {
  predict_knn <- function(test_point) {
    distances <- apply(train_features, 1, function(x) sum((x - test_point)^2))
    neighbors <- order(distances)[1:k]
    neighbor_labels <- train_target[neighbors]
    prediction <- ifelse(mean(neighbor_labels) > 0, 1, -1)
    return(prediction)
  }
  predictions <- apply(test_features, 1, predict_knn)
  return(predictions)
}

# Calculate the ROC and PR curves
calculate_roc_pr <- function(predictions, target) {
  thresholds <- sort(unique(predictions))
  tpr <- fpr <- precision <- recall <- numeric(length(thresholds))
  
  for (i in 1:length(thresholds)) {
    threshold <- thresholds[i]
    predicted_labels <- ifelse(predictions > threshold, 1, -1)
    tp <- sum(predicted_labels == 1 & target == 1)
    fp <- sum(predicted_labels == 1 & target == -1)
    fn <- sum(predicted_labels == -1 & target == 1)
    tn <- sum(predicted_labels == -1 & target == -1)
    
    tpr[i] <- tp / (tp + fn)
    fpr[i] <- fp / (fp + tn)
    precision[i] <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))  # Handle NaN cases
    recall[i] <- tp / (tp + fn)
  }
  
  return(list(tpr = tpr, fpr = fpr, precision = precision, recall = recall))
}

# Get predictions for 5NN and 10NN
predictions_5nn <- knn_classifier(features, target, features, 5)
predictions_10nn <- knn_classifier(features, target, features, 10)

# Calculate ROC and PR curves
roc_pr_5nn <- calculate_roc_pr(predictions_5nn, target)
roc_pr_10nn <- calculate_roc_pr(predictions_10nn, target)

# Plot ROC curves
plot(roc_pr_5nn$fpr, roc_pr_5nn$tpr, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curves")
lines(roc_pr_10nn$fpr, roc_pr_10nn$tpr, col = "red")
legend("bottomright", legend = c("5NN", "10NN"), col = c("blue", "red"), lwd = 2)

# Plot PR curves
plot(roc_pr_5nn$recall, roc_pr_5nn$precision, type = "l", col = "blue", xlab = "Recall", ylab = "Precision", main = "Precision-Recall Curves", xlim = c(0, 1), ylim = c(0, 1))
lines(roc_pr_10nn$recall, roc_pr_10nn$precision, col = "red")
legend("bottomright", legend = c("5NN", "10NN"), col = c("blue", "red"), lwd = 2)

# Print ROC and PR curves
roc_pr_5nn
roc_pr_10nn

# Save ROC curves to a PDF
pdf("ROC_Curves.pdf")
plot(roc_pr_5nn$fpr, roc_pr_5nn$tpr, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curves")
lines(roc_pr_10nn$fpr, roc_pr_10nn$tpr, col = "red")
legend("bottomright", legend = c("5NN", "10NN"), col = c("blue", "red"), lwd = 2)
dev.off()

# Save PR curves to a PDF
pdf("PR_Curves.pdf")
plot(roc_pr_5nn$recall, roc_pr_5nn$precision, type = "l", col = "blue", xlab = "Recall", ylab = "Precision", main = "Precision-Recall Curves", xlim = c(0, 1), ylim = c(0, 1))
lines(roc_pr_10nn$recall, roc_pr_10nn$precision, col = "red")
legend("bottomright", legend = c("5NN", "10NN"), col = c("blue", "red"), lwd = 2)
dev.off()