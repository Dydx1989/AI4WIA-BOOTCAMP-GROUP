# Generate Synthetic Data

# Split the data into training and testing sets


set.seed(123)
n <- 100
rainfall <- runif(n, min = 0, max = 100)
humidity <- runif(n, min = 0, max = 100)
frost_free_days <- runif(n, min = 0, max = 365)
temperature <- runif(n, min = 0, max = 40)
crop_yield <- 50 + 2 * rainfall - 1.5 * humidity - 0.3 * frost_free_days + 1.5 * temperature + rnorm(n, mean = 0, sd = 5)

# Create a data frame
crop_data <- data.frame(Rainfall = rainfall, Humidity = humidity, FrostFreeDays = frost_free_days, Temperature = temperature, CropYield = crop_yield)
head(crop_data)
library(caTools)
set.seed(123)
split <- sample.split(crop_data$CropYield, SplitRatio = 0.7)
train_data <- crop_data[split, ]
test_data <- crop_data[!split, ]
# Build a linear regression model
crop_model <- lm(CropYield ~ Rainfall + Humidity + FrostFreeDays + Temperature, data = train_data)
# Summary of the model
summary(crop_model)
# Make predictions on the test data
predictions <- predict(crop_model, newdata = test_data)

# Evaluate the model
#install.packages("Metrics")
library(Metrics)
rmse_value <- rmse(test_data$CropYield, predictions)
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")
# Visualize the results
library(ggplot2)
ggplot(test_data, aes(x = CropYield, y = predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Crop Yield", y = "Predicted Crop Yield") +
  ggtitle("Crop Yield Prediction")






# Artificial Neural Network (ANN)


library(neuralnet)

# Generate synthetic data (you can replace this with your own dataset)
set.seed(123)
n <- 100
rainfall <- runif(n, min = 0, max = 100)
humidity <- runif(n, min = 0, max = 100)
frost_free_days <- runif(n, min = 0, max = 365)
temperature <- runif(n, min = 0, max = 40)
crop_yield <- 50 + 2 * rainfall - 1.5 * humidity - 0.3 * frost_free_days + 1.5 * temperature + rnorm(n, mean = 0, sd = 5)

# Create a data frame
crop_data <- data.frame(Rainfall = rainfall, Humidity = humidity, FrostFreeDays = frost_free_days, Temperature = temperature, CropYield = crop_yield)
head(crop_data)
# MAX-MIN NORMALIZATION
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(crop_data, normalize))
head(maxmindf)
# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)
library(caTools)
split <- sample.split(maxmindf$CropYield, SplitRatio = 0.7)
train_data <- maxmindf[split, ]
test_data <- maxmindf[!split, ]

# Define the neural network model
# In this example, we have a single hidden layer with 5 neurons
crop_nn <- neuralnet(CropYield ~ Rainfall + Humidity + FrostFreeDays + Temperature, data = train_data, hidden = 5,linear.output=FALSE)
crop_nn$result.matrix

plot(crop_nn)

# Make predictions on the test data
predictions <- predict(crop_nn, newdata = test_data)

# Evaluate the model (you can use different metrics as needed)
rmse_value <- sqrt(mean((predictions - test_data$CropYield)^2))
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")


library(ggplot2)
ggplot(test_data, aes(x = CropYield, y = predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Crop Yield", y = "Predicted Crop Yield") +
  ggtitle("Crop Yield Prediction")





## Variable Importance


library(neuralnet)

# Generate synthetic data (you can replace this with your own dataset)
set.seed(123)
n <- 100
rainfall <- runif(n, min = 0, max = 100)
humidity <- runif(n, min = 0, max = 100)
frost_free_days <- runif(n, min = 0, max = 365)
temperature <- runif(n, min = 0, max = 40)
crop_yield <- 50 + 2 * rainfall - 1.5 * humidity - 0.3 * frost_free_days + 1.5 * temperature + rnorm(n, mean = 0, sd = 5)

# Create a data frame
crop_data <- data.frame(Rainfall = rainfall, Humidity = humidity, FrostFreeDays = frost_free_days, Temperature = temperature, CropYield = crop_yield)
head(crop_data)
# MAX-MIN NORMALIZATION
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(crop_data, normalize))
head(maxmindf)
# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)
library(caTools)
split <- sample.split(maxmindf$CropYield, SplitRatio = 0.7)
train_data <- maxmindf[split, ]
test_data <- maxmindf[!split, ]

# code from link noted above (slightly updated)
require(clusterGeneration)
require(nnet)
rand.vars<-data.frame( maxmindf[,-5] )
y<-data.frame( maxmindf[,5] )

mod1<-nnet(rand.vars,y,size=8,linout=T)

require(devtools)

#import 'gar.fun' from beckmw's Github - this is Garson's algorithm
source_gist('6206737')

#use the function on the model created above
gar.fun('y',mod1)



## Logistic regression


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






