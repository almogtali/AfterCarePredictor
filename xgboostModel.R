library(dplyr)
library(skimr)
library(readr)
library(magrittr)
library(tidyverse)
library(tidymodels)
library(rsample)  # for cross-validation
library(parsnip)
library(recipes)
library(workflows)
library(dials)  # for tuning parameters
library(xgboost)
library(themis)
library(vip)

# read data
data <- read_csv("dataF.csv", show_col_types = FALSE)
data <- drop_na(data)

# factoring columns
data_model <- data %>%
  select(-discharge_location, -race, -subject_id, -hadm_id, -admityear, -family, -insurance) %>%
  select(-drg_category, -count_diagnoses, -drg_code, -insurance) %>%
  filter(discharge_location_group!="DIED") %>%
  mutate(across(where(is.character),as.factor),
         isdead = as.factor(isdead))

set.seed(42) # in order to split the same way each time
data_split <- initial_split(data_model, prop = 0.8, strata = isdead) # argument strata - for imbalance data - equals to target variable
train_data <- training(data_split)
test_data <- testing(data_split)

# Define a model specification
boost_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune(),
  mode = "classification"
) %>%
  set_engine("xgboost")

# Define a recipe for preprocessing
data_recipe <- recipe(isdead ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -isdead, one_hot = TRUE) %>%
  step_smote(isdead, over_ratio = 0.5, neighbors = 3)  # can add step_smote(isdead) to account for imbalance data-set

# Create a cross-validation object
cv <- vfold_cv(train_data, v = 5, repeats = 1, strata = isdead)  # 5-fold cross-validation, 1 repeat # argument strata - for imbalance data - equals to target variable

# tune grid to optimize each parameter
xgb_grid<- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size=sample_prop(),
  mtry(range = c(3, 7)),# Set the range for mtry
  learn_rate(),
  size = 10
)


head(xgb_grid) # to see the grid values for each model - check how it chose exactly those


# Create a workflow with cross-validation
  wf <- workflow() %>%
    add_model(boost_spec) %>%
    add_recipe(data_recipe)

# training on the cross validation folds
xgb_res <-  tune_grid(wf, 
                      resamples = cv,
                      grid = xgb_grid,
                      control = control_grid(verbose = TRUE, save_pred = TRUE),
                      metrics = metric_set(accuracy, roc_auc, precision, sensitivity, specificity, mn_log_loss))
  


# visualizing the results of the cross-validation
xgb_res %>% 
  collect_metrics() %>%
  filter(.metric=="roc_auc") %>%
  select(mean,mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,names_to = "parameter",
               values_to = "value") %>%
  ggplot(aes(value,mean, color=parameter))+
  geom_point(show.legend = F)+
  facet_wrap(~parameter,scales = "free_x")+
  theme_classic()

  show_best(xgb_res,metric = "roc_auc")
# need to look on the trends in the metrics - pipe it to the noted code to visualize it
# x axis is the value of the parameter in the model
# y axis is the mean of the metric chosen
# if there is a trend then narrow parameter range to the optimal range - in the grid parameters

# Show the best models based on roc_auc
top_models <- show_best(xgb_res, metric = "roc_auc")
top_models

# # Extract the second best model parameters
# # Manually extract the second best model parameters
# second_best_params <- top_models[2, c("mtry", "min_n", "tree_depth", "learn_rate", "loss_reduction", "sample_size")]
# 
# 
# # Finalize the workflow with the second best parameters
# final_wf <- wf %>%
#   finalize_workflow(second_best_params)  
  
  
  
# create final workflow according to the best model found in the cross-validation
final_wf <- wf %>%
  finalize_workflow(select_best(xgb_res, metric="roc_auc"))

# metrics of finalized model
metrics <- final_wf %>% 
  fit_resamples(resamples=cv,
  metrics=metric_set(accuracy, roc_auc, precision, sensitivity, specificity, mn_log_loss)) %>% 
  collect_metrics()

metrics

predicted_data <- last_fit(final_wf, data_split, metric_set(accuracy, roc_auc, precision, sensitivity, specificity, mn_log_loss))

#Variable Importance

predicted_data %>% 
  extract_fit_parsnip() %>% 
  vip(num_features=20,geom="point",aesthetics=list(color=rainbow(20),shape=18,size=4))+theme_classic()+
  ggtitle("XGBoost Variable Importance")+
  theme(plot.title = element_text(hjust = 0.5))


# confusion matrix
collect_predictions(predicted_data) %>% 
  conf_mat(isdead,.pred_class) 

#ROC Plot

collect_predictions(predicted_data) %>% 
  roc_curve(isdead, .pred_1, event_level="second") %>% 
  ggplot(aes(1-specificity,sensitivity))+
  geom_abline(lty=2,color="grey",linewidth=1.5)+
  geom_path(alpha=0.8,size=1)+theme_classic()+labs(color="Year Post-Hospitalization Mortality")+
  ggtitle("ROC Curve for XGBoost Model")+
  coord_equal()+theme(plot.title = element_text(size=19,hjust = 0.5))

# validate test metrics
collect_metrics(predicted_data)
