Customer_csv<-read.csv("C:\\Users\\bida22-096\\Downloads\\Customer.csv")
View(Customer_csv)
str(Customer_csv)
head(Customer_csv)
summary(Customer_csv)
#identifying outliers
install.packages("ggplot2")
library(ggplot2)
ggplot(Customer_csv, aes(y= `Time.on.App`)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Time on App to identify outliers",
       y = "Time on App",
       x = "") +
  theme_minimal()

ggplot(Customer_csv, aes(y= `Time.on.Website`)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Boxplot of Time on Website to identify outliers",
       y = "Time on Website",
       x = "") +
  theme_minimal()

ggplot(Customer_csv, aes(y= `Length.of.Membership`)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Boxplot of Length of Membership to identify outliers",
       y = "Length of Membership",
       x = "") +
  theme_minimal()

ggplot(Customer_csv, aes(y= `Yearly.Amount.Spent`)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Boxplot of Yearly Amount Spent to identify outliers",
       y = "Yearly Amonut Spent",
       x = "") +
  theme_minimal()

#capping out outliers
cap_outliers <- function(x) {
  lower_bound <- quantile(x, 0.01, na.rm = TRUE)  
  upper_bound <- quantile(x, 0.99, na.rm = TRUE)  
  x[x < lower_bound] <- lower_bound  
  x[x > upper_bound] <- upper_bound  
  return(x)
}

# install.packages("dplyr")
library(dplyr)
Customer_csv <- Customer_csv %>% mutate(across(where(is.numeric), cap_outliers))

#confirming outliers have been capped
library(ggplot2)
ggplot(Customer_csv, aes(y= `Time.on.App`)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Time on App to identify outliers",
       y = "Time on App",
       x = "") +
  theme_minimal()

ggplot(Customer_csv, aes(y= `Time.on.Website`)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Boxplot of Time on Website to identify outliers",
       y = "Time on Website",
       x = "") +
  theme_minimal()

ggplot(Customer_csv, aes(y= `Length.of.Membership`)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Boxplot of Length of Membership to identify outliers",
       y = "Length of Membership",
       x = "") +
  theme_minimal()

ggplot(Customer_csv, aes(y= `Yearly.Amount.Spent`)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Boxplot of Yearly Amount Spent to identify outliers",
       y = "Yearly Amonut Spent",
       x = "") +
  theme_minimal()

#checking for missing values
colSums(is.na(Customer_csv))

#checking for duplicates
duplicates <- duplicated(Customer_csv)

# Display rows that are duplicates
Customer_csv[duplicates, ]

#converting times from decimals
convert_to_hours_minutes <- function(time_in_hours) {
  hours <- floor(time_in_hours)  # Extract the hours
  minutes <- round((time_in_hours - hours) * 60)  # Convert fractional hours to minutes
  return(paste(hours, "hours", minutes, "minutes"))
}

# Function to format as currency (e.g., USD)
convert_to_currency <- function(amount) {
  return(paste("$", sprintf("%.2f", amount), sep = ""))}

#making times readable
Customer_csv$Time_on_App_HumanReadable <- sapply(Customer_csv$Time.on.App, convert_to_hours_minutes)
Customer_csv$Time_on_Website_HumanReadable <- sapply(Customer_csv$Time.on.Website, convert_to_hours_minutes)
Customer_csv$Yearly_Amount_Spent_HumanReadable <- sapply(Customer_csv$Yearly.Amount.Spent, convert_to_currency)
head(Customer_csv)

#Scaling Data
Dataset_scaled <- Customer_csv %>%
  mutate(across(c(Time.on.App, Yearly.Amount.Spent, Length.of.Membership), scale))

head(Customer_csv)

#Checking for negative values
negative_values <- apply(Customer_csv, 2, function(x) any(x < 0))
negative_columns <- names(negative_values[negative_values == TRUE])
negative_columns

negative_value_counts <- sapply(Customer_csv, function(x) sum(x < 0, na.rm = TRUE))

# Display the counts of negative values per column
negative_value_counts[negative_value_counts > 0]

# Display rows where Time.on.App is negative
negative_time_on_app <- Customer_csv[Customer_csv$Time.on.App < 0, ]
negative_time_on_app


# EXPLORATORY DATA ANALYSIS

#Histogram for the Distribution of Time on App, Website and Annual Amount Spent
hist(Customer_csv$Time.on.App, main = "Distribution of Time on App", xlab = "Time on App", ylim = c(0, 120))
hist(Customer_csv$Time.on.Website, main = "Distribution of Time on Website", xlab = "Time on Website", ylim = c(0, 120))
hist(Customer_csv$Yearly.Amount.Spent, main = "Distribution of Yearly Amount Spent", xlab = "Yearly Amount Spent", ylim = c(0,140))


#Checking variable correlations
numeric_columns <- Customer_csv[, sapply(Customer_csv, is.numeric)]  # Select only numeric columns
cor_matrix <- cor(numeric_columns, use = "complete.obs")  # Correlation matrix
print(cor_matrix)

#Correlation heat map using corrplot() package
# install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "color", addCoef.col = "grey", number.cex = 0.7)

#K-means clustering: segmenting customer's spending by app usage.

set.seed(123)
 kmeans_result<-kmeans(Customer_csv[, c("Time.on.App", "Yearly.Amount.Spent")], centers = 3)
 Customer_csv$Cluster<-as.factor(kmeans_result$cluster)
 
#Visualizing clusters
ggplot(Customer_csv, aes(x = Time.on.App, y = Yearly.Amount.Spent, color= Cluster)) +
  geom_point() + labs(title="Customer Segmentation by App Usage and Annual Spending")

# A scatter plot for Length of Membership vs Yearly Amount Spent
ggplot(Customer_csv, aes(x = Length.of.Membership, y = Yearly.Amount.Spent)) +
  geom_point(color = "blue") +  # Add points to the plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  #Trend line (linear regression)
  labs(title = "Length of Membership vs Yearly Amount Spent",
       x = "Length of Membership (years)",
       y = "Yearly Amount Spent ($)") +
  theme_minimal()


#RANDOM FOREST MODEL
# install.packages("randomForest")
library(randomForest)  # For Random Forest
# install.packages("caret")
library(caret)         # For model evaluation
library(pROC)


# Check column names
names(Customer_csv)

# Define thresholds for low engagement and long membership
time_on_app_low <- quantile(Customer_csv$Time.on.App, 0.25)  # 25th percentile for app engagement
time_on_website_low <- quantile(Customer_csv$Time.on.Website, 0.25)  # 25th percentile for website engagement
long_membership_threshold <- median(Customer_csv$Length.of.Membership)  # Median for membership duration

# Define a function to evaluate performance based on different thresholds
evaluate_threshold <- function() {
  
  # Create a Churn column based on low engagement and long membership
  Customer_csv$Churn <- ifelse(
    Customer_csv$Time.on.App < time_on_app_low & 
      Customer_csv$Time.on.Website < time_on_website_low & 
      Customer_csv$Length.of.Membership > long_membership_threshold, 
    "Yes", "No"
  )
  Customer_csv$Churn <- as.factor(Customer_csv$Churn)  # Convert to factor
  
  # Splitting data into training and testing sets
  set.seed(123)  # For reproducibility
  train_index <- createDataPartition(Customer_csv$Churn, p = 0.7, list = FALSE)
  train_data <- Customer_csv[train_index, ]
  test_data <- Customer_csv[-train_index, ]
  
  # Building the random forest model
  rf_model <- randomForest(Churn ~ Time.on.App + Time.on.Website + Length.of.Membership + Yearly.Amount.Spent,
                           data = train_data, 
                           ntree = 500,  # Number of trees
                           mtry = 2,     # Number of variables considered at each split
                           importance = TRUE)  # To compute variable importance
  
  # Tuned model with more trees and higher mtry
  rf_model_tune <- randomForest(Churn ~ Time.on.App + Time.on.Website + Length.of.Membership + Yearly.Amount.Spent,
                                data = train_data,
                                ntree = 1000,  # Increase number of trees
                                mtry = 3,      # Higher mtry value
                                importance = TRUE)
  
  # Predict on test data using the tuned model
  rf_predictions <- predict(rf_model_tune, test_data)
  
  # Display the first few predictions
  print("First 10 Predictions:")
  print(head(rf_predictions, 10))
  
  # Compare predictions with actual churn values (from test_data)
  comparison <- data.frame(Actual = test_data$Churn, Predicted = rf_predictions)
  print("Predicted vs Actual Churn:")
  print(head(comparison, 10))
  
  # Confusion matrix to evaluate model performance
  conf_matrix <- confusionMatrix(rf_predictions, test_data$Churn)
  print("Confusion Matrix:")
  print(conf_matrix)
  
  # Predict probabilities for the positive class ("Yes")
  rf_probabilities <- predict(rf_model_tune, test_data, type = "prob")[, 2]
  
  # Print probabilities for the positive class (churn)
  print("Probabilities for positive class:")
  print(head(rf_probabilities, 10))
  
  # Generate the ROC curve
  roc_curve <- roc(test_data$Churn, rf_probabilities)
  plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
  
  # Calculate the AUC value
  auc_value <- auc(roc_curve)
  print(paste("AUC Value:", auc_value))
  
  # Return relevant performance metrics (accuracy and AUC)
  return(c(accuracy = conf_matrix$overall["Accuracy"], AUC = auc_value))
}

# Evaluate using the new churn definition
evaluate_threshold()

# Confusion Matrix for tuned model (Final model)
conf_matrix_final <- confusionMatrix(rf_predictions, test_data$Churn)
print("Final Confusion Matrix:")
print(conf_matrix_final)

# Precision and Recall for tuned model
precision <- conf_matrix_final$byClass["Pos Pred Value"]  # Positive Predictive Value (Precision)
recall <- conf_matrix_final$byClass["Sensitivity"]        # Sensitivity (Recall)
print(paste("Precision:", precision))
print(paste("Recall:", recall))

# Cross-validation 
train_control <- trainControl(method = "cv", number = 5)  # 5-fold CV
cv_model <- train(Churn ~ Time.on.App + Time.on.Website + Length.of.Membership + Yearly.Amount.Spent,
                  data = Customer_csv,
                  method = "rf",
                  trControl = train_control,
                  tuneGrid = expand.grid(mtry = 2))  # Can adjust mtry, ntree etc.
print("Cross-validation Results:")
print(cv_model)

# Visualizing variable importance for the tuned random forest model
importance_plot <- randomForest::importance(rf_model_tune)
varImpPlot(rf_model_tune)  # Plot the importance of each variable

