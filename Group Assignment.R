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

install.packages("dplyr")
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

#making times readable
Customer_csv$Time_on_App_HumanReadable <- sapply(Customer_csv$Time.on.App, convert_to_hours_minutes)
Customer_csv$Time_on_Website_HumanReadable <- sapply(Customer_csv$Time.on.Website, convert_to_hours_minutes)
Customer_csv$Yearly_Amount_Spent_HumanReadable <- sapply(Customer_csv$Yearly.Amount.Spent, convert_to_hours_minutes)

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

