# Load necessary libraries
library(tidyverse)

path <- "C:\\Users\\admin\\Downloads\\omr.csv"
omr <- read.csv(path)


omr$result_name[grepl("BMI (kg/m2)", omr$result_name,ignore.case = TRUE)] <- "BMI"
omr$result_name[grepl("Weight (Lbs)", omr$result_name, ignore.case = TRUE)] <- "Weight"


omr <- omr %>%
  pivot_wider(names_from = result_name, values_from = result_value)

# Write the processed data to a new CSV file
write.csv(omr, "omr_pivot_wider.csv", row.names = FALSE)


path <- "C:\\Users\\admin\\Downloads\\admissions.csv"
admissions <- read_csv(path)

admissions$admittime <- ymd_hms(admissions$admittime)
admissions$dischtime <- ymd_hms(admissions$dischtime)
admissions$edregtime <- ymd_hms(admissions$edregtime)


df <- admissions %>%
  mutate(date_admittime = as.Date(admittime),
         date_dischtime = as.Date(dischtime),
         date_edregtime = as.Date(edregtime)
         )

processed_data$chartdate <- dmy(processed_data$chartdate)

df <- df %>%
  left_join(processed_data, by = c("date_edregtime"="chartdate", "subject_id"))


df$diff_days_disedr <- as.numeric(difftime(df$date_dischtime, df$date_edregtime, units = "days"))



path <- "C:\\Users\\admin\\Downloads\\patients.csv"
patients <- read_csv(path)

df$diff_days_dodedr <- as.numeric(difftime(df$dod, df$date_edregtime, units = "days"))


df <- df %>%
  left_join(patients %>% select(subject_id,gender, anchor_year, dod), by = c("subject_id"="subject_id"))


path <- "C:\\Users\\admin\\Downloads\\drgcodes.csv"
drgcodes <- read_csv(path)

df <- df %>%
  left_join(drgcodes, by = c("hadm_id", "subject_id"))

# write.csv(df, "admission_process.csv", row.names = FALSE)
