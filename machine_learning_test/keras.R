# =============================================================================
# KERAS IN R: A BEGINNER'S SHOWCASE
# =============================================================================
# This script demonstrates how to use Keras directly in R for a simple
# classification task using the iris dataset.

# =============================================================================
# STEP 1: SETUP AND INSTALLATION
# =============================================================================

# Install necessary packages if not already installed
# Uncomment these lines if you need to install the packages
# install.packages("keras")
# keras::install_keras()  # This installs TensorFlow and Keras

# Load the required libraries
library(keras)   # R interface to Keras
library(dplyr)   # For data manipulation
library(ggplot2) # For visualization

# Set seed for reproducibility
set.seed(123)


# =============================================================================
# STEP 2: EXPLORE AND PREPARE THE DATA
# =============================================================================

# We'll use the iris dataset, which is built into R
# Let's first look at the data
head(iris)

# Some basic information about the dataset
# iris has 150 observations, 4 numeric predictors, and 1 categorical target
str(iris)

# For Keras models, inputs need to be numeric and outputs need to be 
# appropriately encoded
# TERMS TO LOOK UP: "one-hot encoding", "matrix data format for neural networks"

# Split the data into training and testing sets
# This is a manual split (without using rsample from tidymodels)
indices <- sample(1:nrow(iris), size = round(0.8 * nrow(iris)))
iris_train <- iris[indices, ]
iris_test <- iris[-indices, ]

# Verify the split
print(paste("Training set size:", nrow(iris_train)))
print(paste("Testing set size:", nrow(iris_test)))

# Separate features (X) and target (y) for both training and test sets
x_train <- iris_train %>% 
  select(-Species) %>% 
  as.matrix()  # Keras requires matrix input

x_test <- iris_test %>% 
  select(-Species) %>% 
  as.matrix()

# Convert the categorical outcome to numeric format
# In Keras, for multi-class classification, we need one-hot encoded targets
# TERMS TO LOOK UP: "to_categorical in Keras"

# First, convert the species to integers (0, 1, 2)
y_train_int <- as.integer(iris_train$Species) - 1
y_test_int <- as.integer(iris_test$Species) - 1

# Then, convert to one-hot encoding
y_train <- to_categorical(y_train_int, num_classes = 3)
y_test <- to_categorical(y_test_int, num_classes = 3)

# Look at the format of the one-hot encoded data
head(y_train)


# =============================================================================
# STEP 3: PREPROCESS THE DATA
# =============================================================================

# Neural networks work better when input features are normalized
# TERMS TO LOOK UP: "feature scaling", "normalization vs standardization"

# Calculate mean and standard deviation for each feature using training data
feature_means <- apply(x_train, 2, mean)
feature_sds <- apply(x_train, 2, sd)

# Apply standardization to training and test data
# This ensures each feature has mean 0 and standard deviation 1
x_train_scaled <- scale(x_train, center = feature_means, scale = feature_sds)
x_test_scaled <- scale(x_test, center = feature_means, scale = feature_sds)

# Check the standardized data
head(x_train_scaled)
colMeans(x_train_scaled)  # Should be close to 0
apply(x_train_scaled, 2, sd)  # Should be close to 1


# =============================================================================
# STEP 4: DEFINE THE MODEL ARCHITECTURE
# =============================================================================

# In Keras, we define the network architecture layer by layer
# TERMS TO LOOK UP: "sequential model in Keras", "layers in neural networks"

# Create a sequential model (layers stacked linearly)
model <- keras_model_sequential()

# Add layers to the model
model %>%
  # Input layer with shape matching our features
  layer_dense(units = 10, input_shape = ncol(x_train_scaled), 
              activation = "relu") %>%
  # Hidden layer with ReLU activation function
  # ReLU (Rectified Linear Unit) applies f(x) = max(0, x) to each input
  layer_dense(units = 10, activation = "relu") %>%
  # Output layer with softmax activation (for multi-class classification)
  # Softmax turns raw scores into probabilities that sum to 1
  layer_dense(units = 3, activation = "softmax")

# Print the model summary to see its architecture
summary(model)


# =============================================================================
# STEP 5: COMPILE THE MODEL
# =============================================================================

# Before training, we need to configure the learning process by "compiling" the model
# TERMS TO LOOK UP: "loss function", "optimizer", "learning rate", "metrics in Keras"

model %>% compile(
  # Loss function for multi-class classification
  loss = "categorical_crossentropy",
  # Optimizer controls how the model updates based on the data it sees and its loss
  optimizer = optimizer_adam(learning_rate = 0.001),
  # Metrics to be evaluated during training and testing
  metrics = c("accuracy")
)


# =============================================================================
# STEP 6: TRAIN THE MODEL
# =============================================================================

# Now we train the model on our data
# TERMS TO LOOK UP: "epochs", "batch size", "validation split"

# The fit function trains the model
history <- model %>% fit(
  x = x_train_scaled,
  y = y_train,
  # Number of complete passes through the training dataset
  epochs = 100,
  # Number of samples per gradient update
  batch_size = 16,
  # Portion of training data to use for validation during training
  validation_split = 0.2,
  # Reduce verbosity for cleaner output
  verbose = 1
)

# Plot the training history
plot(history)


# =============================================================================
# STEP 7: EVALUATE THE MODEL ON TEST DATA
# =============================================================================

# Evaluate the model on the test data
model_evaluation <- model %>% evaluate(
  x = x_test_scaled,
  y = y_test,
  verbose = 0
)

# Print the evaluation metrics
cat("Test loss:", model_evaluation[1], "\n")
cat("Test accuracy:", model_evaluation[2], "\n")

# Make predictions on the test set
predictions <- model %>% predict(x_test_scaled)

# Convert the probabilities to class labels
predicted_classes <- max.col(predictions) - 1
actual_classes <- max.col(y_test) - 1

# Create a dataframe with predictions and actual values
results_df <- data.frame(
  Actual = factor(actual_classes, levels = 0:2,
                  labels = levels(iris$Species)),
  Predicted = factor(predicted_classes, levels = 0:2,
                     labels = levels(iris$Species))
)

# Confusion matrix
confusion_matrix <- table(results_df$Predicted, results_df$Actual)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))


# =============================================================================
# STEP 8: VISUALIZE THE RESULTS
# =============================================================================

# Plot the confusion matrix as a heatmap
confusion_df <- as.data.frame.table(confusion_matrix)
colnames(confusion_df) <- c("Predicted", "Actual", "Frequency")

ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix",
       x = "Actual Class",
       y = "Predicted Class") +
  theme_minimal()

# Bar plot of actual vs predicted
ggplot(results_df, aes(x = Actual, fill = Predicted)) +
  geom_bar(position = "dodge") +
  labs(title = "Actual vs Predicted Species",
       x = "Actual Species",
       y = "Count",
       fill = "Predicted Species") +
  theme_minimal()


# =============================================================================
# STEP 9: USING THE MODEL FOR PREDICTIONS
# =============================================================================

# Let's create a new sample to predict
new_samples <- data.frame(
  Sepal.Length = c(5.1, 6.3, 7.0),
  Sepal.Width = c(3.5, 2.8, 3.2),
  Petal.Length = c(1.4, 5.1, 6.0),
  Petal.Width = c(0.2, 1.5, 2.0)
)

# Standardize the new samples using the same means and standard deviations
new_samples_scaled <- scale(as.matrix(new_samples), 
                            center = feature_means, 
                            scale = feature_sds)

# Make predictions
new_predictions <- model %>% predict(new_samples_scaled)

# Convert to class labels
new_predicted_classes <- levels(iris$Species)[max.col(new_predictions)]

# Print predictions
prediction_results <- cbind(new_samples, 
                            Predicted_Species = new_predicted_classes,
                            new_predictions)
colnames(prediction_results)[5:7] <- paste0("Prob_", levels(iris$Species))
print(prediction_results)


# =============================================================================
# STEP 10: SAVE AND LOAD THE MODEL
# =============================================================================

# Save the model architecture and weights
model %>% save_model_hdf5("iris_keras_model.h5")
cat("Model saved to disk\n")

# To load the model in a different session
# loaded_model <- load_model_hdf5("iris_keras_model.h5")

# =============================================================================
# STEP 11: IMPROVING THE MODEL (OPTIONAL)
# =============================================================================

# If you want to improve the model, you could:
# 1. Add more layers or neurons
# 2. Adjust the learning rate
# 3. Use regularization to prevent overfitting
# 4. Try different activation functions
# 5. Use early stopping to prevent overfitting

# Example of a more complex model with regularization:
# TERMS TO LOOK UP: "dropout regularization", "L2 regularization", "early stopping"

# model_complex <- keras_model_sequential() %>%
#   layer_dense(units = 32, input_shape = ncol(x_train_scaled), 
#              activation = "relu",
#              kernel_regularizer = regularizer_l2(0.001)) %>%
#   layer_dropout(rate = 0.3) %>%
#   layer_dense(units = 16, activation = "relu") %>%
#   layer_dropout(rate = 0.3) %>%
#   layer_dense(units = 3, activation = "softmax")