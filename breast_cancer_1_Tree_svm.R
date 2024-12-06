# Step 1: Load the Dataset and Inspect It
# Install required libraries if not already installed
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("skimr")) install.packages("skimr")
if (!require("rpart")) install.packages("rpart")
if (!require("rpart.plot")) install.packages("rpart.plot")
if (!require("e1071")) install.packages("e1071")

library(readr)
library(dplyr)
library(caret)
library(skimr)
library(rpart)
library(rpart.plot)
library(e1071)

# Load dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
col_names <- c("ID", "Diagnosis", paste0("Feature_", 1:30))
dataset <- read_csv(url, col_names = col_names, col_types = cols(.default = "c"))

# Convert data types
dataset <- dataset %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(starts_with("Feature_"), as.numeric))

# Drop ID column as it is not useful
dataset <- dataset %>% select(-ID)

# Inspect dataset
skim(dataset)

# Step 2: Split the Dataset
set.seed(123) # For reproducibility

# Stratified sampling
train_index <- createDataPartition(dataset$Diagnosis, p = 0.6, list = FALSE)
train_data <- dataset[train_index, ]
temp_data <- dataset[-train_index, ]

# Further split into validation and test
valid_index <- createDataPartition(temp_data$Diagnosis, p = 0.5, list = FALSE)
valid_data <- temp_data[valid_index, ]
test_data <- temp_data[-valid_index, ]

# Step 3: Decision Tree
# Train a decision tree model
tree_model <- rpart(Diagnosis ~ ., data = train_data, method = "class")

# Visualize the decision tree
rpart.plot(tree_model)

# Predict on the validation set
tree_predictions <- predict(tree_model, valid_data, type = "class")

# Evaluate the decision tree model
conf_matrix_tree <- confusionMatrix(tree_predictions, valid_data$Diagnosis)
print(conf_matrix_tree)
tree_accuracy <- conf_matrix_tree$overall["Accuracy"]
cat("Decision Tree Accuracy:", tree_accuracy, "\n")

# Step 4: Support Vector Machine (SVM)
# Train an SVM model
svm_model <- svm(Diagnosis ~ ., data = train_data, kernel = "radial")

# Predict on the validation set
svm_predictions <- predict(svm_model, valid_data)

# Evaluate the SVM model
conf_matrix_svm <- confusionMatrix(svm_predictions, valid_data$Diagnosis)
print(conf_matrix_svm)
svm_accuracy <- conf_matrix_svm$overall["Accuracy"]
cat("SVM Accuracy:", svm_accuracy, "\n")

# Step 5: Test the Best Model
# Based on validation accuracy, choose the better model for final testing
cat("\nFinal Testing on Test Dataset\n")
if (svm_accuracy > tree_accuracy) {
  final_predictions <- predict(svm_model, test_data)
  final_conf_matrix <- confusionMatrix(final_predictions, test_data$Diagnosis)
  cat("Using SVM for testing.\n")
} else {
  final_predictions <- predict(tree_model, test_data, type = "class")
  final_conf_matrix <- confusionMatrix(final_predictions, test_data$Diagnosis)
  cat("Using Decision Tree for testing.\n")
}

# Print final test metrics
print(final_conf_matrix)
final_accuracy <- final_conf_matrix$overall["Accuracy"]
cat("Final Test Accuracy:", final_accuracy, "\n")

