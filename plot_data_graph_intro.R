# Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)




data<- read.csv("C:\\Users\\admin\\Downloads\\data\\admissions.csv")

data <- data %>%
  arrange(subject_id, desc(hospital_expire_flag)) %>%
  distinct(subject_id, .keep_all = TRUE)


data$race <- as.character(data$race)

data$race[grepl("WHITE", data$race,ignore.case = TRUE)] <- "WHITE"
data$race[grepl("BLACK", data$race, ignore.case = TRUE)] <- "BLACK"
data$race[grepl("LATINO", data$race, ignore.case = TRUE)] <- "LATION"
data$race[grepl("ASIAN", data$race, ignore.case = TRUE)] <- "ASIAN"
data$race <- as.factor(data$race)

# data <- data %>% filter(!(race %in% c("UNKNOWN", "OTHER","ANSWER")))
data <- data %>% filter(!grepl("UNKNOWN|OTHER|ANSWER", race, ignore.case = TRUE))
data <- data %>% filter(!grepl("UNABLE|MULTIPLE|PORTUG", race, ignore.case = TRUE))

data$insurance <- as.factor(data$insurance)

data$hospital_expire_flag <- as.factor(data$hospital_expire_flag)

ggplot(data, aes(x = insurance, fill = hospital_expire_flag )) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Status Types by Insurance",
       x = "Insurance Type",
       y = "Count",
       fill = "ALIVE-DEAD")

ggplot(data, aes(x = insurance, fill = race)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Insurance Types by Race",
       x = "Insurance Type",
       y = "Count",
       fill = "Race")






data <- read.csv("C:\\Users\\admin\\Downloads\\data\\omr.csv")

library(ggplot2)

# Convert result_value to numeric
data$result_value <- as.numeric(data$result_value)

# Filter data for Height, Weight, and BMI
height <- subset(data, result_name == "Height (Inches)")
weight <- subset(data, result_name == "Weight (Lbs)")
bmi <- subset(data, result_name == "BMI (kg/m2)")

bmi$result_value <- as.numeric(bmi$result_value)

# Calculate statistics
bmi_mean <- mean(bmi$result_value, na.rm = TRUE)
bmi_median <- median(bmi$result_value, na.rm = TRUE)
bmi_min <- min(bmi$result_value, na.rm = TRUE)
bmi_max <- max(bmi$result_value, na.rm = TRUE)
bmi_sd <- sd(bmi$result_value, na.rm = TRUE)

# Print the statistics
cat("Mean BMI:", bmi_mean, "\n")
cat("Median BMI:", bmi_median, "\n")
cat("Minimum BMI:", bmi_min, "\n")
cat("Maximum BMI:", bmi_max, "\n")
cat("Standard Deviation of BMI:", bmi_sd, "\n")



# BMI Histogram
ggplot(bmi, aes(x = `result_value`)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  scale_x_continuous(limits = c(0, 70)) +
  geom_vline(xintercept = bmi_mean, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of BMI", x = "BMI (kg/m2)", y = "Frequency")



# Calculate statistics for Height
height$result_value <- as.numeric(height$result_value)
height_mean <- mean(height$result_value, na.rm = TRUE)
height_median <- median(height$result_value, na.rm = TRUE)
height_min <- min(height$result_value, na.rm = TRUE)
height_max <- max(height$result_value, na.rm = TRUE)
height_sd <- sd(height$result_value, na.rm = TRUE)

# Print the statistics for Height
cat("Mean Height:", height_mean, "\n")
cat("Median Height:", height_median, "\n")
cat("Minimum Height:", height_min, "\n")
cat("Maximum Height:", height_max, "\n")
cat("Standard Deviation of Height:", height_sd, "\n")

# Height Histogram
ggplot(height, aes(x = `result_value`)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  scale_x_continuous(limits = c(40, 90)) +
  geom_vline(xintercept = height_mean, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Height", x = "Height (Inches)", y = "Frequency")

# Calculate statistics for Weight
weight$result_value <- as.numeric(weight$result_value)
weight_mean <- mean(weight$result_value, na.rm = TRUE)
weight_median <- median(weight$result_value, na.rm = TRUE)
weight_min <- min(weight$result_value, na.rm = TRUE)
weight_max <- max(weight$result_value, na.rm = TRUE)
weight_sd <- sd(weight$result_value, na.rm = TRUE)

# Print the statistics for Weight
cat("Mean Weight:", weight_mean, "\n")
cat("Median Weight:", weight_median, "\n")
cat("Minimum Weight:", weight_min, "\n")
cat("Maximum Weight:", weight_max, "\n")
cat("Standard Deviation of Weight:", weight_sd, "\n")

# Weight Histogram
ggplot(weight, aes(x = `result_value`)) +
  geom_histogram(binwidth = 5, fill = "lightcoral", color = "black") +
  scale_x_continuous(limits = c(40,300)) +
  geom_vline(xintercept = weight_mean, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Weight", x = "Weight (Lbs)", y = "Frequency")




############################ PLOTS WE DIDNT USE ###############################################

admissions <- read.csv("C:\\Users\\admin\\Downloads\\data\\admissions.csv")

summary(admissions)


ggplot(admissions, aes(x = admission_type)) + 
  geom_bar(fill = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Distribution of Admission Types", x = "Admission Type", y = "Count")

# Bar plot for admission locations
ggplot(admissions, aes(x = admission_location)) + 
  geom_bar(fill = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Distribution of Admission Locations", x = "Admission Location", y = "Count")

# Bar plot for insurance types
ggplot(admissions, aes(x = insurance)) + 
  geom_bar(fill = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Distribution of Insurance Types", x = "Insurance Type", y = "Count")

# Bar plot for discharge locations
ggplot(admissions, aes(x = discharge_location)) + 
  geom_bar(fill = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Distribution of Discharge Locations", x = "Discharge Location", y = "Count")


# Bar plot for hospital expire flag
ggplot(admissions, aes(x = factor(hospital_expire_flag))) + 
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(title = "Hospital Expire Flag Distribution", x = "Hospital Expire Flag", y = "Count")


# Bar plot for race
ggplot(admissions, aes(x = race)) + 
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(title = "Distribution of Race", x = "Race", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for insurance types
ggplot(admissions, aes(x = insurance)) + 
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(title = "Distribution of Insurance Types", x = "Insurance Type", y = "Count")
