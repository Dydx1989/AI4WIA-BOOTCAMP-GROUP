
## Over fitting 1

# Load required libraries
library(ggplot2)

# Generate synthetic data
set.seed(123)
x_train <- seq(0, 2 * pi, by = 0.2)
y_true <- sin(x_train)
y_noise <- rnorm(length(x_train), mean = 0, sd = 0.2)
y_train <- y_true + y_noise

x_test <- seq(0, 2 * pi, by = 0.3)  # Test data with different spacing
y_test <- sin(x_test) + rnorm(length(x_test), mean = 0, sd = 0.2)

# Create dataframes for training and test data
train_data <- data.frame(x = x_train, y = y_train, dataset = "Training")
test_data <- data.frame(x = x_test, y = y_test, dataset = "Test")

# Combine the dataframes
data <- rbind(train_data, test_data)

# Function to fit polynomial regression models
fit_polynomial <- function(data, degree) {
  model <- lm(y ~ poly(x, degree), data = data)
  return(model)
}

# Plot the synthetic data with legend
ggplot(data, aes(x, y, color = dataset)) +
  geom_point() +
  geom_line(aes(group = dataset), color = "blue") +
  ggtitle("Synthetic Data with Legend") +
  labs(color = "Dataset")



## Over fitting 2
# Load required libraries
library(ggplot2)

# Generate synthetic data
set.seed(123)
x <- seq(0, 2 * pi, by = 0.2)
y_true <- sin(x)
y_noise <- rnorm(length(x), mean = 0, sd = 0.2)
y <- y_true + y_noise

# Create a dataframe
data <- data.frame(x = x, y = y)

# Function to fit polynomial regression models
fit_polynomial <- function(data, degree) {
  model <- lm(y ~ poly(x, degree), data = data)
  return(model)
}

# Plot the synthetic data
ggplot(data, aes(x, y)) +
  geom_point(aes(color = "Data"), show.legend = TRUE) +
  ggtitle("Synthetic Data") +
  scale_color_manual(values = c("Data" = "black"))

# Fit and plot a high-degree polynomial regression model
degree <- 15
model <- fit_polynomial(data, degree)
data$y_pred <- predict(model, newdata = data)
plot_title <- paste("Polynomial Degree =", degree)

ggplot(data, aes(x, y)) +
  geom_point(aes(color = "Data"), show.legend = TRUE) +
  geom_line(aes(y = y_pred, color = "Fitted"), show.legend = TRUE) +
  ggtitle(plot_title) +
  scale_color_manual(values = c("Data" = "black", "Fitted" = "blue"))



# Load required libraries
library(ggplot2)

# Generate synthetic data
set.seed(123)
x <- seq(0, 2 * pi, by = 0.2)
y_true <- sin(x)
y_noise <- rnorm(length(x), mean = 0, sd = 0.2)
y <- y_true + y_noise

# Create a dataframe
data <- data.frame(x = x, y = y)

# Plot the synthetic data
ggplot(data, aes(x, y)) +
  geom_point() +
  ggtitle("Synthetic Data")

# Fit and plot a linear regression model
linear_model <- lm(y ~ x, data = data)
data$y_pred <- predict(linear_model, newdata = data)

ggplot(data, aes(x, y)) +
  geom_point() +
  geom_line(aes(y = y_pred), color = "blue") +
  ggtitle("Linear Regression (Underfitting)")




