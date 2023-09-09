# Load required libraries
library(dplyr)
library(caret)

# Create a synthetic dataset
set.seed(123)
data <- data.frame(
  rain_or_not = rbinom(100, 1, 0.5),  # Binary variable (0: No Rain, 1: Rain)
  amount_of_rain = runif(100, 0, 10),   # Random amount of rain (in inches)
  duration_of_rain = runif(100, 0, 24)  # Random duration of rain (in hours)
)

# Split the dataset into training and testing sets
train_idx <- sample(1:nrow(data), 70)
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Train a logistic regression model
model <- glm(rain_or_not ~ amount_of_rain + duration_of_rain, data = train_data, family = binomial)

# Make predictions on the testing set
predictions <- predict(model, newdata = test_data, type = "response")

# Convert predicted probabilities to binary predictions
threshold <- 0.5
binary_predictions <- ifelse(predictions > threshold, 1, 0)

# Create a new data frame without the index
results <- data.frame(
  rain_or_not = test_data$rain_or_not,
  binary_predictions
)



# Evaluate the model using diagnostic tests

# Calculate the confusion matrix
confusion_matrix <- table(Actual = test_data$rain_or_not, Predicted = binary_predictions)

# Print the confusion matrix
print(confusion_matrix)
# Access TP, TN, FP, FN
TP <- confusion_matrix["1", "1"]
TN <- confusion_matrix["0", "0"]
FP <- confusion_matrix["0", "1"]
FN <- confusion_matrix["1", "0"]

# Print TP, TN, FP, FN
cat("True Positives (TP):", TP, "\n")
cat("True Negatives (TN):", TN, "\n")
cat("False Positives (FP):", FP, "\n")
cat("False Negatives (FN):", FN, "\n")


# Calculate accuracy
accuracy <- (TP + TN) / (TP + TN + FP + FN)

# Calculate sensitivity (True Positive Rate)
sensitivity <- TP / (TP + FN)

# Calculate specificity (True Negative Rate)
specificity <- TN / (TN + FP)

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity (True Positive Rate):", sensitivity, "\n")
cat("Specificity (True Negative Rate):", specificity, "\n")


#install.packages("pROC")
library(pROC)
# Calculate ROC curve
roc_curve <- roc(test_data$rain_or_not, predictions)

# Calculate AUC (Area Under the Curve)
auc_value <- auc(roc_curve)

# Print AUC
cat("AUC (Area Under the Curve):", auc_value, "\n")
# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue")

# Add a reference line (random guessing)
#abline(a = 0, b = 1, lty = 2, col = "red")

# Add AUC value to the plot
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lty = 1)

###
# Install and load the neuralnet package
#install.packages("neuralnet")
library(neuralnet)
# Create a data frame with the predictors and target variable
ann_data <- data.frame(
  amount_of_rain = train_data$amount_of_rain,
  duration_of_rain = train_data$duration_of_rain,
  rain_or_not = as.factor(train_data$rain_or_not)
)

# Define the formula for the neural network
formula <- rain_or_not ~ amount_of_rain + duration_of_rain

# Train the neural network
ann_model <- neuralnet(formula, data = ann_data, hidden = 5, linear.output = FALSE)

# Make predictions on the test data


ann_predictions <- predict(ann_model, newdata = test_data)


# Evaluate the model using diagnostic tests


# Convert predicted probabilities to binary predictions
ann_binary_predictions <- ifelse(ann_predictions > 0.5, 1, 0)

# Calculate accuracy, sensitivity, and specificity
ann_confusion_matrix <- table(Actual = test_data$rain_or_not, Predicted = ann_binary_predictions[,1])
TP <- ann_confusion_matrix["1", "1"]
TN <- ann_confusion_matrix["0", "0"]
FP <- ann_confusion_matrix["0", "1"]
FN <- ann_confusion_matrix["1", "0"]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity (True Positive Rate):", sensitivity, "\n")
cat("Specificity (True Negative Rate):", specificity, "\n")

library(pROC)
# Calculate ROC curve
roc_curve <- roc(test_data$rain_or_not,ann_predictions[,1])

# Calculate AUC (Area Under the Curve)
auc_value <- auc(roc_curve)

# Print AUC
cat("AUC (Area Under the Curve):", auc_value, "\n")
# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue")

# Add a reference line (random guessing)
#abline(a = 0, b = 1, lty = 2, col = "red")

# Add AUC value to the plot
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lty = 1)
