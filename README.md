# AfterCare Predictor


## Overview
This project aims to develop a predictive model for assessing the likelihood of patient mortality within one year after discharge from an Intensive Care Unit (ICU). By analyzing a comprehensive dataset of patient information collected during ICU admissions, we seek to identify key characteristics and factors that correlate with long-term patient outcomes.
The primary goals of this research are:

To explore the relationship between ICU admission data and post-discharge mortality rates.
To create a model that can predict the risk of mortality within one year of hospital discharge.
To provide medical professionals with a tool for identifying patients who may require enhanced post-discharge supervision and care.

By using machine learning techniques and statistical analysis, we aim to contribute to improved patient care and safety in the critical period following ICU hospitalization.

## Data Processing

This project processes hospital admission, patient, diagnosis, and DRG (Diagnosis Related Group) data to create a comprehensive dataset for analysis. The process involves several key steps:

1. Data Loading: We load multiple datasets including admissions, patients, diagnoses, family history, and DRG codes.

2. Admission Processing:
   - Convert admission times to dates and extract admission years.
   - Calculate patient ages at admission.
   - Determine mortality within one year of admission.

3. Patient Data Merging: Combine admission data with patient information.

4. Data Filtering and Selection:
   - Focus on urgent admissions.
   - Select relevant columns for analysis.

5. Feature Engineering:
   - Create discharge location groups.
   - Derive ethnicity from race information.
   - Add family history indicators.
   - Count diagnoses per admission.
   - Map DRG codes to categories.

6. Data Integration: Merge all processed datasets into a single comprehensive dataset.

## Machine Learning Process - XGBoost

After data processing, we apply machine learning techniques to predict one-year mortality. Here's an overview of the process:

1. Data Preparation: Load the processed data and prepare it for modeling.
2. Feature Selection: Select relevant features and encode categorical variables.
3. Data Splitting: Split data into training and testing sets.
4. Model Specification: Define an XGBoost model with hyperparameter tuning.
5. Preprocessing: Create a recipe for one-hot encoding and SMOTE for class imbalance.
6. Cross-Validation: Use 5-fold cross-validation for model evaluation.
7. Hyperparameter Tuning: Use a Latin Hypercube sampling for hyperparameter optimization.
8. Model Training and Evaluation: Train the model and evaluate performance.
9. Final Model Selection: Select the best model based on ROC AUC.
10. Model Interpretation: Analyze variable importance and model performance.



## Logistic Regression Analysis

After the XGBoost analysis, we also perform a logistic regression analysis for comparison. Here's an overview of the process:

1. Data Preparation: Load the processed data and prepare it for modeling.
2. Feature Selection: Select relevant features and remove certain columns.
3. Data Splitting: Split data into training and testing sets.
4. Model Specification: Define a logistic regression model.
5. Preprocessing: Create a recipe for one-hot encoding and SMOTE for class imbalance.
6. Cross-Validation: Use 5-fold cross-validation for model evaluation.
7. Model Training and Evaluation: Train the model and evaluate performance.
8. Model Interpretation: Analyze variable importance and model performance.