# Load necessary libraries
library(dplyr)
library(skimr)
library(readr)
library(magrittr)
library(tidyverse) 

# Load data
admissions <- read_csv("admissions.csv", show_col_types = FALSE)  # Admissions data
patients <- read_csv("patients.csv", show_col_types = FALSE)      # Patients data

# Process admissions data
admissions <- admissions %>%
  mutate(date_admittime = as.Date(admittime))  # Convert admission time to Date format
admissions$admityear <- year(admissions$date_admittime)  # Extract year from admission date

# Process patient data
years_per_subject <- admissions %>%
  select(subject_id, admityear) %>%
  unique() %>%
  left_join(patients, by = c("subject_id"))  # Merge admissions and patients data

years_per_subject$dif_year <- years_per_subject$admityear - years_per_subject$anchor_year  # Calculate difference in years

years_per_subject$admitage <- years_per_subject$anchor_age + years_per_subject$dif_year  # Calculate age at admission

years_per_subject$dodyear <- year(years_per_subject$dod)  # Extract year of death
years_per_subject$difdeadyear <- years_per_subject$dodyear - years_per_subject$admityear  # Calculate difference between death year and admission year

years_per_subject$isdead <- years_per_subject$difdeadyear <= 1  # Determine if the patient died within a year of admission

# Merge processed data and filter urgent admissions
data <- admissions %>%
  left_join(years_per_subject, by = c("admityear", "subject_id")) %>%
  filter(admission_type == "URGENT") %>%
  select(subject_id, hadm_id, discharge_location, insurance, marital_status, race, admityear, gender, admitage, isdead) %>%
  mutate(isdead = ifelse(is.na(isdead), 0, ifelse(isdead, 1, 0)))  # Handle missing death status

data$discharge_location_group <- NA  # Initialize discharge location group
data$ethnicity <- NA  # Initialize ethnicity

# Transform and categorize data
data <- data %>%
  mutate(insurance = as.factor(insurance),
         marital_status = as.factor(marital_status),
         gender = as.factor(gender),
         isdead = as.factor(isdead),
         discharge_location = as.character(discharge_location),
         race = as.character(race),
         discharge_location_group = case_when(
           grepl("HOME", discharge_location, ignore.case = TRUE) ~ "HOME",
           grepl("DIED", discharge_location, ignore.case = TRUE) ~ "DIED",
           TRUE ~ "OTHER"
         ),
         discharge_location_group = as.factor(discharge_location_group),
         ethnicity = case_when(
           grepl("WHITE", race, ignore.case = TRUE) ~ "WHITE",
           grepl("BLACK", race, ignore.case = TRUE) ~ "BLACK",
           grepl("HISPANIC/LATINO|SOUTH AMERICAN|PORTUGUESE|HISPANIC OR LATINO", race, ignore.case = TRUE) ~ "HISPANIC/LATINO",
           grepl("ASIAN", race, ignore.case = TRUE) ~ "ASIAN",
           grepl("AMERICAN INDIAN/ALASKA NATIVE", race, ignore.case = TRUE) ~ "AMERICAN INDIAN/ALASKA NATIVE",
           grepl("NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", race, ignore.case = TRUE) ~ "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
           grepl("UNKNOWN|OTHER|PATIENT DECLINED TO ANSWER|UNABLE TO OBTAIN|MULTIPLE RACE/ETHNICITY", race, ignore.case = TRUE) ~ "OTHER",
           TRUE ~ "OTHER"
         ),
         ethnicity = as.factor(ethnicity)
  )

# Load and process diagnoses data
diagnoses <- read_csv("diagnoses_icd.csv", show_col_types = FALSE) %>%
  mutate(icd_version = as.character(icd_version))  # Convert ICD version to character

diagnoses$code <- paste(diagnoses$icd_code, diagnoses$icd_version, sep = "-")  # Combine ICD code and version

# Load and process family history data
familyHistory <- read_csv("FamilyHistoryCodes.csv", show_col_types = FALSE)
familyHistory$family <- "1"  # Mark family history codes

# Merge diagnoses and family history data
result <- left_join(diagnoses, familyHistory, by = "code") %>%
  drop_na() %>%
  select(-hadm_id, -seq_num, -icd_code.x, -icd_version.x, -icd_code.y, -icd_version.y, -code) %>%
  distinct()  # Remove duplicates

# Merge processed data with family history data
data <- left_join(data, result, by = "subject_id") %>%
  mutate(family = ifelse(is.na(family), 0, family))  # Handle missing family history

# Summarize diagnoses data
diagnoses <- diagnoses %>%
  group_by(subject_id, hadm_id) %>%
  mutate(count_diagnoses = n())

patients_diagnoses <- diagnoses %>%
  distinct(subject_id, hadm_id, .keep_all = TRUE) %>%
  select(-seq_num, -icd_code, -icd_version, -code)

# Merge processed data with summarized diagnoses data
data <- left_join(data, patients_diagnoses, by = c("subject_id", "hadm_id"))

# Load and process DRG codes data
drgcodes <- read_csv("drgcodes.csv", show_col_types = FALSE) %>%
  drop_na() %>%
  select(-drg_mortality, -description) %>%
  distinct(subject_id, hadm_id, drg_code, .keep_all = TRUE)

# Merge processed data with DRG codes data
data <- left_join(data, drgcodes, by = c("subject_id", "hadm_id")) %>%
  drop_na() %>%
  mutate(drg_code = as.numeric(drg_code))  # Convert DRG code to numeric

# Define the mapping table for APR DRG categories
category_mapping <- data.frame(
  start_code = c(001, 020, 113, 129, 163, 215, 326, 405, 453, 573, 616, 652, 707, 734, 769, 789, 799, 820, 853, 876, 894, 901, 927, 939, 955, 969, 110, 161, 317, 447, 650, 890),
  end_code = c(019, 103, 125, 159, 208, 316, 404, 446, 566, 607, 645, 700, 730, 761, 831, 794, 816, 849, 872, 887, 897, 923, 935, 951, 965, 977, 111, 162, 321, 447, 651, 893),
  category = c("Pre-MDC",
               "Nervous System",
               "Eye",
               "Ear, Nose, Mouth, and Throat",
               "Respiratory System",
               "Circulatory System",
               "Digestive System",
               "Hepatobiliary System and Pancreas",
               "Musculoskeletal System and Connective Tissue",
               "Skin, Subcutaneous Tissue and Breast",
               "Endocrine, Nutritional and Metabolic",
               "Kidney and Urinary Tract",
               "Male Reproductive System",
               "Female Reproductive System",
               "Pregnancy, Childbirth and the Puerperium",
               "Newborns and Other Neonates with Conditions Originating in the Perinatal Period",
               "Blood, Blood Forming Organs, Immunologic Disorders",
               "Myeloproliferative Diseases & Disorders, Poorly Differentiated Neoplasms",
               "Infectious and Parasitic Diseases, Systemic or Unspecified Sites",
               "Mental Diseases and Disorders",
               "Alcohol/Drug Use and Induced Organic Mental Disorders",
               "Injuries, Poisonings, and Toxic Effects of Drugs",
               "Burns",
               "Factors Influencing Health Status & Health Services",
               "Multiple Significant Trauma",
               "Human Immunodeficiency Virus Infections",
               "Ear, Nose, Mouth, and Throat",
               "Circulatory System",
               "Musculoskeletal System and Connective Tissue",
               "Kidney and Urinary Tract",
               "Blood, Blood Forming Organs, Immunologic Disorders",
               "Infectious and Parasitic Diseases, Systemic or Unspecified Sites")
)

# Function to map APR DRG codes to categories
map_to_category <- function(code, mapping) {
  for (i in 1:nrow(mapping)) {
    if (code >= mapping$start_code[i] && code <= mapping$end_code[i]) {
      return(paste(mapping$start_code[i], "-", mapping$end_code[i]))
    }
  }
  return(paste(code))
}

# Apply the mapping function to the data frame
data$drg_category <- sapply(data$drg_code, map_to_category, mapping = category_mapping)

# Function to map APR DRG codes to category names
map_to_category_name <- function(code, mapping) {
  for (i in 1:nrow(mapping)) {
    if (code >= mapping$start_code[i] && code <= mapping$end_code[i]) {
      return(paste(mapping$category[i]))
    }
  }
  return(paste("Uncategorized (", code, ")"))
}

# Apply the mapping function to the data frame
data$drg_category_name <- sapply(data$drg_code, map_to_category_name, mapping = category_mapping)

# Save the final processed data to a CSV file
write.csv(data, "dataF.csv", row.names = FALSE)
