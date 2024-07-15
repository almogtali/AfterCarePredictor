# Install and load necessary packages
library(pROC)
library(readr)
library(magrittr)
library(tidymodels)
library(dplyr)
library(skimr)
library(tidyverse)
library(rsample)  # for cross-validation
library(parsnip)
library(recipes)
library(workflows)
library(PRROC)



data <- read_csv("data.csv", show_col_types = FALSE)
data <- drop_na(data)

data <- data %>%
  select(-drg_category, -count_diagnoses, -drg_code) %>%
  filter(discharge_location_group!="DIED") %>%
  mutate(across(where(is.character),as.factor),
         isdead = as.factor(isdead))

  # select(-discharge_location, -race, -subject_id, -hadm_id, -admityear, -insurance, -drg_type , -count_diagnoses, -drg_category) %>%
  # filter(admitage >= 60) %>%
  
  
set.seed(42) # in order to split the same way each time
data_split <- initial_split(data, prop = 0.8, strata = isdead) # argument strata - for imbalance data - equals to target variable
train_data <- training(data_split)
test_data <- testing(data_split)

# Fit the logistic regression model
logistic_model <- glm(isdead ~ .,
                      data = train_data, 
                      family = binomial)

# Define a recipe for preprocessing
data_recipe <- recipe(isdead ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -isdead, ) %>%
  step_smote(isdead, over_ratio = 0.5, neighbors = 3)  # can add step_smote(isdead) to account for imbalance data-set
#%>% step_zv() for zero-variance features

logistic_spec <- logistic_reg() %>%
  set_engine("glm")

# Create a cross-validation object
cv <- vfold_cv(train_data, v = 5, repeats = 1, strata = isdead)  # 5-fold cross-validation, 1 repeat # argument strata - for imbalance data - equals to target variable


# Create the workflow
wf <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(logistic_spec)

# Train the model with cross-validation
logistic_res <- wf %>%
  tune_grid(resamples = cv,
            metrics = metric_set(roc_auc, accuracy),
            control = control_grid(save_pred = TRUE))


# Select the best model
best_model <- logistic_res %>%
  select_best(metric = "roc_auc")

# Finalize the workflow
final_wf <- wf %>%
  finalize_workflow(best_model)

# Fit the finalized model on the training data
final_fit <- final_wf %>%
  last_fit(split = initial_split(data, strata = isdead))

# Extract the parsnip model object
logistic_model <- final_fit %>%
  extract_fit_parsnip()

# View the coefficients (weights) of the logistic regression model
model_summary <- tidy(logistic_model)
print(model_summary %>% arrange(p.value, estimate))



# Make predictions on the test data
test_predictions <- final_fit %>%
  collect_predictions()

# Confusion matrix and accuracy
conf_matrix <- conf_mat(test_predictions, truth = isdead, estimate = .pred_class)
conf_matrix

accuracy <- test_predictions %>%
  accuracy(truth = isdead, estimate = .pred_class)
print(accuracy)

precision <- test_predictions %>%
  precision(truth = isdead, estimate = .pred_class)
print(precision)

sensitivity <- test_predictions %>%
  sensitivity(truth = isdead, estimate = .pred_class)
print(sensitivity)

# ROC curve and AUC
roc_curve <- roc(test_predictions$isdead, test_predictions$.pred_1)
plot(roc_curve)
plot(roc_curve, col="blue", main="ROC Curve for Logistic Regression Model")
grid()

auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))


