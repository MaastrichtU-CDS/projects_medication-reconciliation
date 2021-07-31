
####################################################################################################################

# Packages

####################################################################################################################

library(readxl)
library(ggplot2)
library(caret)

####################################################################################################################

# Data

####################################################################################################################

data <- read_excel("./QZExport99-Pivot.xlsx", skip = 4)
colnames(data)[1] <- "id"
data <- subset(data, id != "AGFspkFhsI") # All variables are missing for the ID equal to "AGFspkFhsI". Data entry error? Excluded.
dup_id <- data[(duplicated(data$id, fromLast = FALSE) == TRUE) | duplicated(data$id, fromLast = TRUE) == TRUE,]$id
data <- subset(data, (id %in% dup_id) == FALSE) # Duplicate ids: same id for multiple patients, same id for same patient on multiple visits, or same id for same patient on one visit that included various out patient clinics? Excluded.

####################################################################################################################

# Responses 

####################################################################################################################

## Part 1.A

### sex
names(data)[names(data) == "A1a"] <- "sex"
data$sex <- factor(data$sex, 
                   levels = c(1, 
                              2), 
                   labels = c("male", 
                              "female"))
table(data$sex, exclude = NULL)
data$sex.male_1 <- ifelse(data$sex == "male", 
                          1, 
                          0)

### age
names(data)[names(data) == "A2a"] <- "age"
summary(data$age)
ggplot(data, aes(x = age)) + 
  geom_histogram() + 
  theme_minimal()

### diseases
names(data)[names(data) == "A10a"] <- "diseases"
data$diseases <- factor(data$diseases, 
                        levels = c(1, 
                                   2, 
                                   3, 
                                   4, 
                                   5, 
                                   6, 
                                   7, 
                                   8), 
                        labels = c("diabetes", 
                                   "rheumatoid arthritis", 
                                   "asthma/COPD", 
                                   "cardiovascular disease", 
                                   "cardiac arrhythmia", 
                                   "heart failure", 
                                   "cancer", 
                                   "none of the above"))
table(data$diseases, exclude = NULL)
data$diseases.diabetes_1 <- ifelse(is.na(data$diseases), 
                                   NA, 
                                   0)
data$diseases.diabetes_1 <- ifelse(data$diseases == "diabetes", 
                                   1, 
                                   data$diseases.diabetes_1)
data$diseases.rheumatoid_arthritis_1 <- ifelse(is.na(data$diseases), 
                                               NA, 
                                               0)
data$diseases.rheumatoid_arthritis_1 <- ifelse(data$diseases == "rheumatoid arthritis", 
                                               1, 
                                               data$diseases.rheumatoid_arthritis_1)
data$diseases.asthma_COPD_1 <- ifelse(is.na(data$diseases), 
                                      NA, 
                                      0)
data$diseases.asthma_COPD_1 <- ifelse(data$diseases == "asthma/COPD", 
                                      1, 
                                      data$diseases.asthma_COPD_1)
data$diseases.cardiovascular_disease_1 <- ifelse(is.na(data$diseases), 
                                                 NA, 
                                                 0)
data$diseases.cardiovascular_disease_1 <- ifelse(data$diseases == "cardiovascular disease", 
                                                 1, 
                                                 data$diseases.cardiovascular_disease_1)
data$diseases.cardiac_arrhythmia_1 <- ifelse(is.na(data$diseases), 
                                             NA, 
                                             0)
data$diseases.cardiac_arrhythmia_1 <- ifelse(data$diseases == "cardiac arrhythmia", 
                                             1, 
                                             data$diseases.cardiac_arrhythmia_1)
data$diseases.heart_failure_1 <- ifelse(is.na(data$diseases), 
                                        NA, 
                                        0)
data$diseases.heart_failure_1 <- ifelse(data$diseases == "heart failure", 
                                        1, 
                                        data$diseases.heart_failure_1)
data$diseases.cancer_1 <- ifelse(is.na(data$diseases), 
                                 NA, 
                                 0)
data$diseases.cancer_1 <- ifelse(data$diseases == "cancer", 
                                 1, 
                                 data$diseases.cancer_1)
data$diseases.none_of_the_above_1 <- ifelse(is.na(data$diseases), 
                                            NA, 
                                            0)
data$diseases.none_of_the_above_1 <- ifelse(data$diseases == "none of the above", 
                                            1, 
                                            data$diseases.none_of_the_above_1)

### number of diseases
names(data)[names(data) == "A9a"] <- "n_diseases"
table(data$n_diseases, exclude = NULL)

### eGFR
names(data)[names(data) == "A12a"] <- "eGFR"
data$eGFR <- factor(data$eGFR, 
                    levels = c(1, 
                               2, 
                               3, 
                               4, 
                               5, 
                               6), 
                    labels = c("> 90 ml/min", 
                               "60-89 ml/min", 
                               "30-59 ml/min", 
                               "15-29 ml/min", 
                               "< 15 ml/min", 
                               "unknown or measured more than 12 months ago"))
table(data$eGFR, exclude = NULL)
data$eGFR.above_90_1 <- ifelse(data$eGFR == "> 90 ml/min", 
                               1, 
                               0)
data$eGFR.60_89_1 <- ifelse(data$eGFR == "60 - 89 ml/min", 
                            1, 
                            0)
data$eGFR.30_59_1 <- ifelse(data$eGFR == "30 - 59 ml/min", 
                            1, 
                            0)
data$eGFR.15_29_1 <- ifelse(data$eGFR == "15 - 29 ml/min", 
                            1, 
                            0)
data$eGFR.below_15_1 <- ifelse(data$eGFR == "< 15 ml/min", 
                               1, 
                               0)
data$eGFR.NA_1 <- ifelse(data$eGFR == "unknown or measured more than 12 months ago", 
                         1, 
                         0)

### number of visits all specialties outpatient clinics in past 12 months in MUMC+ 
names(data)[names(data) == "A14a"] <- "n_visits_out_all_12m_MUMC"
data$n_visits_out_all_12m_MUMC <- factor(data$n_visits_out_all_12m_MUMC, 
                                         levels = c(1, 
                                                    2, 
                                                    3, 
                                                    4), 
                                         labels = c("0", 
                                                    "1", 
                                                    "2-5", 
                                                    "> 5"))
table(data$n_visits_out_all_12m_MUMC, exclude = NULL)
data$n_visits_out_all_12m_MUMC.0_1 <- ifelse(data$n_visits_out_all_12m_MUMC == "0",
                                             1, 
                                             0)
data$n_visits_out_all_12m_MUMC.1_1 <- ifelse(data$n_visits_out_all_12m_MUMC == "1", 
                                             1, 
                                             0)
data$n_visits_out_all_12m_MUMC.2_to_5_1 <- ifelse(data$n_visits_out_all_12m_MUMC == "2-5",
                                                  1, 
                                                  0)
data$n_visits_out_all_12m_MUMC.above_5_1 <- ifelse(data$n_visits_out_all_12m_MUMC == "> 5",
                                                   1, 
                                                   0)

### visited specialty as outpatient and/or inpatient in past 12 months in MUMC+
inout_specialty_12m_MUMC.list <- strsplit(data$A150a, ",")
data$inout_specialty_12m_MUMC.cardiology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.cardiology_1[i] <- ifelse(1 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                          1, 
                                                          0) 
  }
table(data$inout_specialty_12m_MUMC.cardiology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.urology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.urology_1[i] <- ifelse(2 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                       1, 
                                                       0) 
  }
table(data$inout_specialty_12m_MUMC.urology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.psychiatry_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.psychiatry_1[i] <- ifelse(3 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                          1, 
                                                          0) 
  }
table(data$inout_specialty_12m_MUMC.psychiatry_1, exclude = NULL)
data$inout_specialty_12m_MUMC.otorhinolaryngology_1 <- 0 
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.otorhinolaryngology_1[i] <- ifelse(4 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                                   1, 
                                                                   0) 
  }
table(data$inout_specialty_12m_MUMC.otorhinolaryngology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.ophthalmology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.ophthalmology_1[i] <- ifelse(5 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                             1, 
                                                             0) 
  }
table(data$inout_specialty_12m_MUMC.ophtalmology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.internal_medicine_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.internal_medicine_1[i] <- ifelse(6 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                                 1, 
                                                                 0) 
  }
table(data$inout_specialty_12m_MUMC.internal_medicine_1, exclude = NULL)
data$inout_specialty_12m_MUMC.surgery_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.surgery_1[i] <- ifelse(7 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                       1, 
                                                       0) 
  }
table(data$inout_specialty_12m_MUMC.surgery_1, exclude = NULL)
data$inout_specialty_12m_MUMC.orthopedics_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.orthopedics_1[i] <- ifelse(8 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                           1, 
                                                           0) 
  }
table(data$inout_specialty_12m_MUMC.orthopedics_1, exclude = NULL)
data$inout_specialty_12m_MUMC.plastic_surgery_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.plastic_surgery_1[i] <- ifelse(9 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                               1, 
                                                               0) 
  }
table(data$inout_specialty_12m_MUMC.plastic_surgery_1, exclude = NULL)
data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1[i] <- ifelse(10 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                                          1, 
                                                                          0) 
  }
table(data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.neurology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.neurology_1[i] <- ifelse(11 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                         1, 
                                                         0) 
  }
table(data$inout_specialty_12m_MUMC.neurology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.dermatology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.dermatology_1[i] <- ifelse(12 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                           1, 
                                                           0) 
  }
table(data$inout_specialty_12m_MUMC.dermatology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.gastroenterology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.gastroenterology_1[i] <- ifelse(13 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                                1, 
                                                                0) 
  }
table(data$inout_specialty_12m_MUMC.gastroenterology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.pneumology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.pneumology_1[i] <- ifelse(14 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                          1, 
                                                          0) 
  }
table(data$inout_specialty_12m_MUMC.pneumology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.rheumatology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.rheumatology_1[i] <- ifelse(15 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                            1, 
                                                            0) 
  }
table(data$inout_specialty_12m_MUMC.rheumatology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.do_not_select_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.do_not_select_1[i] <- ifelse(16 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                             1, 
                                                             0) 
  }
table(data$inout_specialty_12m_MUMC.do_not_select_1, exclude = NULL)
data$inout_specialty_12m_MUMC.pediatrics_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.pediatrics_1[i] <- ifelse(17 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                          1, 
                                                          0) 
  }
table(data$inout_specialty_12m_MUMC.pediatrics_1, exclude = NULL)
data$inout_specialty_12m_MUMC.anesthesiology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.anesthesiology_1[i] <- ifelse(18 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                              1, 
                                                              0) 
  }
table(data$inout_specialty_12m_MUMC.anesthesiology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.other_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_MUMC.other_1[i] <- ifelse(19 %in% unlist(inout_specialty_12m_MUMC.list[i]), 
                                                     1, 
                                                     0) 
  }
table(data$inout_specialty_12m_MUMC.other_1, exclude = NULL)

### hospitalisation in past 12 months in MUMC+
names(data)[names(data) == "A16a"] <- "in_all_12m_MUMC"
data$in_all_12m_MUMC <- factor(data$in_all_12m_MUMC, 
                               levels = c(1, 
                                          2), 
                               labels = c("yes", 
                                          "no"))
table(data$in_all_12m_MUMC, exclude = NULL)
data$in_all_12m_MUMC.yes_1 <- ifelse(data$in_all_12m_MUMC == "yes", 
                                     1, 
                                     0)

### number of ER visits in past 12 months in MUMC+
names(data)[names(data) == "A18a"] <- "n_ER_12m_MUMC"
table(data$n_ER_12m_MUMC, exclude = NULL)
data$ER_12m_MUMC.yes_1 <- ifelse(data$n_ER_12m_MUMC == 0, 
                                 0, 
                                 1)

## Part 1.B

### number of medications
names(data)[names(data) == "B1a"] <- "n_medications"
table(data$n_medications, exclude = NULL)

### presence of following high-risk medications 
high_risk_medications.list <- strsplit(data$B7a, ",")
data$platelet_aggregation_inhibitors.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$platelet_aggregation_inhibitors.yes_1[i] <- ifelse(1 %in% unlist(high_risk_medications.list[i]), 
                                                          1, 
                                                          0) 
  }
data$anticoagulants.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$anticoagulants.yes_1[i] <- ifelse(2 %in% unlist(high_risk_medications.list[i]), 
                                         1, 
                                         0) 
  }
data$NSAIDs.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$NSAIDs.yes_1[i] <- ifelse(3 %in% unlist(high_risk_medications.list[i]), 
                                 1, 
                                 0) 
  }
data$diuretics.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$diuretics.yes_1[i] <- ifelse(4 %in% unlist(high_risk_medications.list[i]), 
                                    1, 
                                    0) 
  }
data$RAS_inhibitors.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$RAS_inhibitors.yes_1[i] <- ifelse(5 %in% unlist(high_risk_medications.list[i]), 
                                         1, 
                                         0) 
  }
data$systemic_corticosteroids.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$systemic_corticosteroids.yes_1[i] <- ifelse(6 %in% unlist(high_risk_medications.list[i]), 
                                                   1, 
                                                   0) 
  }
data$opioids.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$opioids.yes_1[i] <- ifelse(7 %in% unlist(high_risk_medications.list[i]), 
                                  1, 
                                  0) 
  }
data$glucose_lowering_medications.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$glucose_lowering_medications.yes_1[i] <- ifelse(8 %in% unlist(high_risk_medications.list[i]), 
                                                       1, 
                                                       0) 
  }
data$psychotropics.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$psychotropics.yes_1[i] <- ifelse(9 %in% unlist(high_risk_medications.list[i]), 
                                        1, 
                                        0) 
  }
data$cardiac_medications.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$cardiac_medications.yes_1[i] <- ifelse(10 %in% unlist(high_risk_medications.list[i]), 
                                              1, 
                                              0) 
  }
data$immunosuppressants.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$immunosuppressants.yes_1[i] <- ifelse(11 %in% unlist(high_risk_medications.list[i]), 
                                             1, 
                                             0) 
  }
data$oncolytics.yes_1 <- 0
for (i in 1 : nrow(data)) { 
  data$oncolytics.yes_1[i] <- ifelse(12 %in% unlist(high_risk_medications.list[i]), 
                                     1, 
                                     0) 
  }

### high-risk medications
names(data)[names(data) == "B6a"] <- "high_risk_medications"
data$high_risk_medications <- factor(data$high_risk_medications, 
                                     levels = c(1, 
                                                2), 
                                     labels = c("yes", 
                                                "no"))
table(data$high_risk_medications, exclude = NULL) 
data$high_risk_medications.yes_1 <- ifelse(is.na(data$high_risk_medications), 
                                           NA, 
                                           0)
data$high_risk_medications.yes_1 <- ifelse(data$high_risk_medications == "yes", 
                                           1, 
                                           data$high_risk_medications.yes_1)

## Part 1 C

### current visit specialty 
data$out_specialty_current_MUMC <- data$SPEC
data$out_specialty_current_MUMC <- factor(data$out_specialty_current_MUMC, 
                                          levels = c("CARDIO", 
                                                     "CHI", 
                                                     "CHI HPB",
                                                     "CHI TRAU",
                                                     "CHI VAAT",
                                                     "DERMA", 
                                                     "INTERN-D", 
                                                     "INTERN ALG", 
                                                     "INTERN GER", 
                                                     "NEURO"), 
                                          labels = c("cardiology",
                                                     "surgery",
                                                     "surgery: hepato-pancreatico-biliary", 
                                                     "surgery: trauma",
                                                     "surgery: cardiovascular", 
                                                     "dermatology",
                                                     "internal medicine: D", 
                                                     "internal medicine: general",
                                                     "internal medicine: geriatry",
                                                     "neurology"))
table(data$out_specialty_current_MUMC, exclude = NULL)
data$out_specialty_current_MUMC.cardiology_1 <- ifelse(data$out_specialty_current_MUMC == "cardiology", 
                                                       1, 
                                                       0)
data$out_specialty_current_MUMC.surgery_1 <- ifelse(data$out_specialty_current_MUMC == "surgery", 
                                                    1, 
                                                    0)
data$out_specialty_current_MUMC.surgery_hpb_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: hepato-pancreatico-biliary", 
                                                        1, 
                                                        0)
data$out_specialty_current_MUMC.surgery_trauma_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: trauma", 
                                                           1, 
                                                           0)
data$out_specialty_current_MUMC.surgery_cardiovascular_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: cardiovascular", 
                                                                   1, 
                                                                   0)
data$out_specialty_current_MUMC.surgery_composite_1 <- ifelse((data$out_specialty_current_MUMC.surgery_1 == 1) | 
                                                              (data$out_specialty_current_MUMC.surgery_hpb_1 == 1) | 
                                                              (data$out_specialty_current_MUMC.surgery_trauma_1 == 1) | 
                                                              (data$out_specialty_current_MUMC.surgery_cardiovascular_1 == 1), 
                                                               1, 
                                                               0)
data$out_specialty_current_MUMC.dermatology_1 <- ifelse(data$out_specialty_current_MUMC == "dermatology", 
                                                        1, 
                                                        0)
data$out_specialty_current_MUMC.internal_medicine_D_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: D", 
                                                                1, 
                                                                0)
data$out_specialty_current_MUMC.internal_medicine_general_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: general", 
                                                                      1, 
                                                                      0)
data$out_specialty_current_MUMC.internal_medicine_geriatry_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: geriatry", 
                                                                       1, 
                                                                       0)
data$out_specialty_current_MUMC.internal_medicine_composite_1 <- ifelse((data$out_specialty_current_MUMC.internal_medicine_D_1 == 1) | 
                                                                        (data$out_specialty_current_MUMC.internal_medicine_general_1 == 1) | 
                                                                        (data$out_specialty_current_MUMC.internal_medicine_geriatry_1 == 1),
                                                                         1, 
                                                                         0)
data$out_specialty_current_MUMC.neurology_1 <- ifelse(data$out_specialty_current_MUMC == "neurology", 
                                                      1, 
                                                      0)

### number of visits out patient clinic current specialty in past 12 months in MUMC+ 
names(data)[names(data) == "C3a"] <- "n_visits_out_current_specialty_12m_MUMC"
table(data$n_visits_out_current_specialty_12m_MUMC, exclude = NULL)

## Part 2 A

### housing
names(data)[names(data) == "A3b"] <- "housing"
data$housing <- factor(data$housing, 
                       levels = c(1, 
                                  2, 
                                  3), 
                       labels = c("independent", 
                                  "independent with home care", 
                                  "institution"))
table(data$housing, exclude = NULL)
data$housing.independent_1 <- ifelse(is.na(data$housing), 
                                     NA, 
                                     0)
data$housing.independent_1 <- ifelse(data$housing == "independent", 
                                     1, 
                                     data$housing.independent_1)
data$housing.independent_with_home_care_1 <- ifelse(is.na(data$housing), 
                                                    NA, 
                                                    0)
data$housing.independent_with_home_care_1 <- ifelse(data$housing == "independent with home care", 
                                                    1, 
                                                    data$housing.independent_with_home_care_1)
data$housing.institution_1 <- ifelse(is.na(data$housing), 
                                     NA, 
                                     0)
data$housing.institution_1 <- ifelse(data$housing == "institution", 
                                     1, 
                                     data$housing.institution_1)

### patient-reported medication use 
names(data)[names(data) == "A21b"] <- "patient_reported_medication_use"
data$patient_reported_medication_use <- factor(data$patient_reported_medication_use, 
                                               levels = c(1, 
                                                          2), 
                                               labels = c("yes", 
                                                          "no"))
table(data$patient_reported_medication_use, exclude = NULL)
data$patient_reported_medication_use.yes_1 <- ifelse(is.na(data$patient_reported_medication_use), 
                                                     NA, 
                                                     0)
data$patient_reported_medication_use.yes_1 <- ifelse(data$patient_reported_medication_use == "yes", 
                                                     1, 
                                                     data$patient_reported_medication_use.yes_1)

### pill box (week)
names(data)[names(data) == "A4b"] <- "pill_box.week"
data$pill_box.week <- factor(data$pill_box.week, 
                             levels = c(1, 
                                        2), 
                             labels = c("yes", 
                                        "no"))
table(data$pill_box.week, exclude = NULL)
data$pill_box.week.yes_1 <- ifelse(is.na(data$pill_box.week), 
                                   NA, 
                                   0)
data$pill_box.week.yes_1 <- ifelse(data$pill_box.week == "yes", 
                                   1, 
                                   0)

### person responsible for medications
names(data)[names(data) == "A5b"] <- "person_responsible_for_medications"
data$person_responsible_for_medications <- factor(data$person_responsible_for_medications, 
                                                  levels = c(1, 
                                                             2, 
                                                             3), 
                                                  labels = c("patient", 
                                                             "partner or caregiver", 
                                                             "health professional"))
table(data$person_responsible_for_medications, exclude = NULL)
data$person_responsible_for_medications.patient_1 <- ifelse(is.na(data$person_responsible_for_medications), 
                                                            NA, 
                                                            0)
data$person_responsible_for_medications.patient_1 <- ifelse(data$person_responsible_for_medications == "patient", 
                                                            1, 
                                                            data$person_responsible_for_medications.patient_1)
data$person_responsible_for_medications.partner_or_caregiver_1 <- ifelse(is.na(data$person_responsible_for_medications), 
                                                                         NA, 
                                                                         0)
data$person_responsible_for_medications.partner_or_caregiver_1 <- ifelse(data$person_responsible_for_medications == "partner or caregiver", 
                                                                         1, 
                                                                         data$person_responsible_for_medications.partner_or_caregiver_1)
data$person_responsible_for_medications.health_professional_1 <- ifelse(is.na(data$person_responsible_for_medications), 
                                                                        NA, 
                                                                        0)
data$person_responsible_for_medications.health_professional_1 <- ifelse(data$person_responsible_for_medications == "health professional", 
                                                                        1, 
                                                                        data$person_responsible_for_medications.health_professional_1)

### highest level of education attained
names(data)[names(data) == "A6b"] <- "education"
data$education <- factor(data$education, 
                         levels = c(1, 
                                    2, 
                                    3, 
                                    4), 
                         labels = c("elementary school", 
                                    "LTS, huishoudschool, MAVO, MULO, VMBO", 
                                    "MBO, HAVO, HBS", 
                                    "HTS, HBO, VWO, WO"))
table(data$education, exclude = NULL)
data$education.category_1_1 <- ifelse(is.na(data$education), 
                                      NA, 
                                      0)
data$education.category_1_1 <- ifelse(data$education == "elementary school", 
                                      1, 
                                      0)
data$education.category_2_1 <- ifelse(is.na(data$education), 
                                      NA, 
                                      0)
data$education.category_2_1 <- ifelse(data$education == "LTS, huishoudschool, MAVO, MULO, VMBO", 
                                      1, 
                                      0)
data$education.category_3_1 <- ifelse(is.na(data$education), 
                                      NA, 
                                      0)
data$education.category_3_1 <- ifelse(data$education == "MBO, HAVO, HBS", 
                                      1, 
                                      0)
data$education.category_4_1 <- ifelse(is.na(data$education), 
                                      NA, 
                                      0)
data$education.category_4_1 <- ifelse(data$education == "HTS, HBO, VWO, WO", 
                                      1, 
                                      0)

### allergy medication
names(data)[names(data) == "A13b"] <- "allergy_medication"
data$allergy_medication <- factor(data$allergy_medication, 
                                  levels = c(1, 
                                             2), 
                                  labels = c("yes", 
                                             "no"))
table(data$allergy_medication, exclude = NULL)
data$allergy_medication.yes_1 <- ifelse(is.na(data$allergy_medication), 
                                        NA, 
                                        0)
data$allergy_medication.yes_1 <- ifelse(data$allergy_medication == "yes", 
                                        1, 
                                        0)

### allergy medication symptoms
names(data)[names(data) == "A20b"] <- "allergy_medication_symptoms"
View(subset(data, !is.na(allergy_medication_symptoms))$allergy_medication_symptoms)

### visit other hospital in past 12 months
names(data)[names(data) == "A140b"] <- "visit_12m_other_hospital"
data$visit_12m_other_hospital <- factor(data$visit_12m_other_hospital, 
                                        levels = c(1,
                                                   2), 
                                        labels = c("yes", 
                                                   "no"))
table(data$visit_12m_other_hospital, exclude = NULL)
data$visit_12m_other_hospital.yes_1 <- ifelse(is.na(data$visit_12m_other_hospital), 
                                              NA, 
                                              0)
data$visit_12m_other_hospital.yes_1 <- ifelse(data$visit_12m_other_hospital == "yes", 
                                              1, 
                                              0)

### visited as outpatient and/or inpatient specialty in past 12 months in other hospital
inout_specialty_12m_other_hospital.list <- strsplit(data$A150b, ",")
data$inout_specialty_12m_other_hospital.cardiology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.cardiology_1[i] <- ifelse(1 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                    1, 
                                                                    0) 
  }
table(data$inout_specialty_12m_other_hospital.cardiology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.urology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.urology_1[i] <- ifelse(2 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                 1, 
                                                                 0) 
  }
table(data$inout_specialty_12m_other_hospital.urology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.psychiatry_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.psychiatry_1[i] <- ifelse(3 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                    1, 
                                                                    0) 
  }
table(data$inout_specialty_12m_other_hospital.psychiatry_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.otorhinolaryngology_1 <- 0 
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.otorhinolaryngology_1[i] <- ifelse(4 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                             1, 
                                                                             0) 
  }
table(data$inout_specialty_12m_other_hospital.otorhinolaryngology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.ophthalmology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.ophthalmology_1[i] <- ifelse(5 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                       1, 
                                                                       0) 
  }
table(data$inout_specialty_12m_other_hospital.ophthalmology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.internal_medicine_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.internal_medicine_1[i] <- ifelse(6 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                           1, 
                                                                           0) 
  }
table(data$inout_specialty_12m_other_hospital.internal_medicine_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.surgery_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.surgery_1[i] <- ifelse(7 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                 1, 
                                                                 0) 
  }
table(data$inout_specialty_12m_other_hospital.surgery_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.orthopedics_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.orthopedics_1[i] <- ifelse(8 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                     1, 
                                                                     0) 
  }
table(data$inout_specialty_12m_other_hospital.orthopedics_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.plastic_surgery_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.plastic_surgery_1[i] <- ifelse(9 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                         1, 
                                                                         0) 
  }
table(data$inout_specialty_12m_other_hospital.plastic_surgery_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.obstetrics_and_gynaecology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.obstetrics_and_gynaecology_1[i] <- ifelse(10 %in% unlist(inout_specialty_12m_other_hospital.list[i]),
                                                                                    1, 
                                                                                    0) 
  }
table(data$inout_specialty_12m_other_hospital.obstetrics_and_gynaecology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.neurology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.neurology_1[i] <- ifelse(11 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                   1, 
                                                                   0) 
  }
table(data$inout_specialty_12m_other_hospital.neurology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.dermatology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.dermatology_1[i] <- ifelse(12 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                     1, 
                                                                     0) 
  }
table(data$inout_specialty_12m_other_hospital.dermatology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.gastroenterology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.gastroenterology_1[i] <- ifelse(13 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                          1, 
                                                                          0) 
  }
table(data$inout_specialty_12m_other_hospital.gastroenterology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.pneumology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.pneumology_1[i] <- ifelse(14 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                    1, 
                                                                    0) 
  }
table(data$inout_specialty_12m_other_hospital.pneumology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.rheumatology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.rheumatology_1[i] <- ifelse(15 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                      1, 
                                                                      0) 
  }
table(data$inout_specialty_12m_other_hospital.rheumatology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.do_not_select_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.do_not_select_1[i] <- ifelse(16 %in% unlist(inout_specialty_12m_other_hospital.list[i]),
                                                                       1, 
                                                                       0) 
  }
table(data$inout_specialty_12m_other_hospital.do_not_select_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.pediatrics_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.pediatrics_1[i] <- ifelse(17 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                                    1, 
                                                                    0) 
  }
table(data$inout_specialty_12m_other_hospital.pediatrics_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.anesthesiology_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.anesthesiology_1[i] <- ifelse(18 %in% unlist(inout_specialty_12m_other_hospital.list[i]),
                                                                        1, 
                                                                        0) 
  }
table(data$inout_specialty_12m_other_hospital.anesthesiology_1, exclude = NULL)
data$inout_specialty_12m_other_hospital.other_1 <- 0
for (i in 1 : nrow(data)) { 
  data$inout_specialty_12m_other_hospital.other_1[i] <- ifelse(19 %in% unlist(inout_specialty_12m_other_hospital.list[i]), 
                                                               1, 
                                                               0) 
  }
table(data$inout_specialty_12m_other_hospital.other_1, exclude = NULL)

### hospitalisation in past 12 months in other hospital
names(data)[names(data) == "A16b"] <- "in_all_12m_other_hospital"
data$in_all_12m_other_hospital <- factor(data$in_all_12m_other_hospital, 
                                         levels = c(1, 
                                                    2), 
                                         labels = c("yes", 
                                                    "no"))
table(data$in_all_12m_other_hospital, exclude = NULL)
data$in_all_12m_other_hospital.yes_1 <- ifelse(is.na(data$in_all_12m_other_hospital), 
                                               NA, 
                                               0) 
data$in_all_12m_other_hospital.yes_1 <- ifelse(data$in_all_12m_other_hospital == "yes", 
                                               1, 
                                               data$in_all_12m_other_hospital.yes_1) 

### number of ER visists in past 12 months in other hospital
names(data)[names(data) == "A18b"] <- "n_ER_12m_other_hospital"
table(data$n_ER_12m_other_hospital, exclude = NULL)
data$ER_12m_other_hospital.yes_1 <- ifelse(data$n_ER_12m_other_hospital == 0, 
                                           0, 
                                           1)

### medication without prescription
names(data)[names(data) == "B2b"] <- "medication_without_prescription"
data$medication_without_prescription <- factor(data$medication_without_prescription, 
                                               levels = c(1, 
                                                          2, 
                                                          3, 
                                                          4, 
                                                          5, 
                                                          6, 
                                                          7), 
                                               labels = c("NSAIDs", 
                                                          "proton pump inhibitors", 
                                                          "hypericum", 
                                                          "red yeast rice", 
                                                          "multi vitamins / dietary supplement", 
                                                          "other", 
                                                          "none"))
table(data$medication_without_prescription, exclude = NULL)
data$medication_without_prescription.NSAIDs_1 <- ifelse(is.na(data$medication_without_prescription), 
                                                        NA, 
                                                        0)
data$medication_without_prescription.NSAIDs_1 <- ifelse(data$medication_without_prescription == "NSAIDs", 
                                                        1, 
                                                        data$medication_without_prescription.NSAIDs_1)
data$medication_without_prescription.proton_pump_inhibitors_1 <- ifelse(is.na(data$medication_without_prescription), 
                                                                        NA, 
                                                                        0)
data$medication_without_prescription.proton_pump_inhibitors_1 <- ifelse(data$medication_without_prescription == "proton pump inhibitors", 
                                                                        1, 
                                                                        data$medication_without_prescription.proton_pump_inhibitors_1)
data$medication_without_prescription.hypericum_1 <- ifelse(is.na(data$medication_without_prescription), 
                                                           NA, 
                                                           0)
data$medication_without_prescription.hypericum_1 <- ifelse(data$medication_without_prescription == "hypericum", 
                                                           1, 
                                                           data$medication_without_prescription.hypericum_1)
data$medication_without_prescription.red_yeast_rice_1 <- ifelse(is.na(data$medication_without_prescription), 
                                                                NA, 
                                                                0)
data$medication_without_prescription.red_yeast_rice_1 <- ifelse(data$medication_without_prescription == "red yeast rice", 
                                                                1, 
                                                                data$medication_without_prescription.red_yeast_rice_1)
data$medication_without_prescription.multi_vitamins_dietary_supplement_1 <- ifelse(is.na(data$medication_without_prescription), 
                                                                                   NA, 
                                                                                   0)
data$medication_without_prescription.multi_vitamins_dietary_supplement_1 <- ifelse(data$medication_without_prescription == "multi vitamins / dietary supplement", 
                                                                                   1, 
                                                                                   data$medication_without_prescription.multi_vitamins_dietary_supplement_1)
data$medication_without_prescription.other_1 <- ifelse(is.na(data$medication_without_prescription), 
                                                       NA, 
                                                       0)
data$medication_without_prescription.other_1 <- ifelse(data$medication_without_prescription == "other", 
                                                       1, 
                                                       data$medication_without_prescription.other_1)
data$medication_without_prescription.none_1 <- ifelse(is.na(data$medication_without_prescription), 
                                                      NA, 
                                                      0)
data$medication_without_prescription.none_1 <- ifelse(data$medication_without_prescription == "none", 
                                                      1, 
                                                      data$medication_without_prescription.none_1)

table(data$medication_without_prescription.NSAIDs_1, data$NSAIDs.yes_1, exclude = NULL)
data$NSAIDs_composite.yes_1 <- ifelse((data$medication_without_prescription.NSAIDs_1 == 1) |
                                      (data$NSAIDs.yes_1 == 1),
                                      1, 
                                      0)
data$NSAIDs_composite.yes_1 <- ifelse(is.na(data$NSAIDs_composite.yes_1), 
                                      0, 
                                      data$NSAIDs_composite.yes_1)

## Part 3

### medication literacy
names(data)[names(data) == "A7c"] <- "medication_literacy"
data$medication_literacy <- factor(data$medication_literacy, 
                                   levels = c(1, 
                                              2, 
                                              3), 
                                   labels = c("adequate", 
                                              "suboptimal", 
                                              "insufficient"))
table(data$medication_literacy, exclude = NULL)
data$medication_literacy.adequate_1 <- ifelse(is.na(data$medication_literacy), 
                                              NA, 
                                              0)
data$medication_literacy.adequate_1 <- ifelse(data$medication_literacy == "adequate", 
                                              1, 
                                              0)
data$medication_literacy.suboptimal_1 <- ifelse(is.na(data$medication_literacy), 
                                                NA, 
                                                0)
data$medication_literacy.suboptimal_1 <- ifelse(data$medication_literacy == "suboptimal", 
                                                1, 
                                                0)
data$medication_literacy.insufficient_1 <- ifelse(is.na(data$medication_literacy), 
                                                  NA, 
                                                  0)
data$medication_literacy.insufficient_1 <- ifelse(data$medication_literacy == "insufficient", 
                                                  1, 
                                                  0)

### medication prescribed during consultation: yes/no
names(data)[names(data) == "B10c"] <- "medication_prescribed_during_consult.yes_no"
data$medication_prescribed_during_consult.yes_no <- factor(data$medication_prescribed_during_consult.yes_no, 
                                                           levels = c(1, 
                                                                      2), 
                                                           labels = c("yes", 
                                                                      "no"))
table(data$medication_prescribed_during_consult.yes_no, exclude = NULL)
data$medication_prescribed_during_consult.yes_1 <- ifelse(is.na(data$medication_prescribed_during_consult.yes_no), 
                                                          NA, 
                                                          0)
data$medication_prescribed_during_consult.yes_1 <- ifelse(data$medication_prescribed_during_consult.yes_no == "yes", 
                                                          1, 
                                                          data$medication_prescribed_during_consult.yes_1)

### medication prescribed during consultation: name
names(data)[names(data) == "B11c"] <- "medication_prescribed_during_consult.name"
View(subset(data, !is.na(medication_prescribed_during_consult.name))$medication_prescribed_during_consult.name)

### medication prescribed during consultation: new
names(data)[names(data) == "B12c"] <- "medication_prescribed_during_consult.new"
data$medication_prescribed_during_consult.new <- factor(data$medication_prescribed_during_consult.new, 
                                                        levels = c(1, 
                                                                   2), 
                                                        labels = c("yes", 
                                                                   "no"))
table(data$medication_prescribed_during_consult.new, exclude = NULL)
data$medication_prescribed_during_consult.new_1 <- ifelse(data$medication_prescribed_during_consult.new == "yes", 
                                                          1, 
                                                          0)

### medication stopped during consult: yes/no
names(data)[names(data) == "B20c"] <- "medication_stopped_during_consult.yes_no"
data$medication_stopped_during_consult.yes_no <- factor(data$medication_stopped_during_consult.yes_no, 
                                                        levels = c(1, 
                                                                   2), 
                                                        labels = c("yes", 
                                                                   "no"))
table(data$medication_stopped_during_consult.yes_no, exclude = NULL)
data$medication_stopped_during_consult.yes_1 <- ifelse(is.na(data$medication_stopped_during_consult.yes_no), 
                                                       NA, 
                                                       0)
data$medication_stopped_during_consult.yes_1 <- ifelse(data$medication_stopped_during_consult.yes_no == "yes", 
                                                       1, 
                                                       data$medication_stopped_during_consult.yes_1)

### medication stopped during consult: name
names(data)[names(data) == "B21c"] <- "medication_stopped_during_consult.name"
View(subset(data, !is.na(medication_stopped_during_consult.name))$medication_stopped_during_consult.name)

### non-medication intervention during consult 
names(data)[names(data) == "B13c"] <- "non_medication_intervention_during_consult"
data$non_medication_intervention_during_consult <- factor(data$non_medication_intervention_during_consult, 
                                                          levels = c(1, 
                                                                     2), 
                                                          labels = c("yes", 
                                                                     "no"))
table(data$non_medication_intervention_during_consult, exclude = NULL)
data$non_medication_intervention_during_consult.yes_1 <- ifelse(is.na(data$non_medication_intervention_during_consult), 
                                                                NA, 
                                                                0)
data$non_medication_intervention_during_consult.yes_1 <- ifelse(data$non_medication_intervention_during_consult == "yes", 
                                                                1, 
                                                                data$non_medication_intervention_during_consult.yes_1)

### warnings
names(data)[names(data) == "S1c"] <- "warnings"
data$warnings <- factor(data$warnings, 
                        levels = c(1, 
                                   2), 
                        labels = c("yes", 
                                   "no"))
table(data$warnings, exclude = NULL)
data$warnings.yes_1 <- ifelse(is.na(data$warnings), 
                              NA, 
                              0)
data$warnings.yes_1 <- ifelse(data$warnings == "yes", 
                              1, 
                              data$warnings.yes_1)

### medications interaction
names(data)[names(data) == "S2c"] <- "medications_interaction"
data$medications_interaction <- factor(data$medications_interaction, 
                                       levels = c(1, 
                                                  2), 
                                       labels = c("yes", 
                                                  "no"))
table(data$medications_interaction, exclude = NULL)
data$medications_interaction.yes_1 <- ifelse(is.na(data$medications_interaction), 
                                             NA, 
                                             0)
data$medications_interaction.yes_1 <- ifelse(data$medications_interaction == "yes", 
                                             1, 
                                             data$medications_interaction.yes_1)

### medications interaction G-standaard number
id.interaction_number <- as.data.frame(cbind(data$id, 
                                             data$S31c, 
                                             data$S32c, 
                                             data$S33c)) 
colnames(id.interaction_number) <- c( "id",
                                      "interaction_number.1",
                                      "interaction_number.2",
                                      "interaction_number.3")

### medications interaction WFG
# View(cbind(data$S41c, data$S42c, data$S43c))
# Meaning of the variables unclear. 

### medications revision: pharmacy assistant 
id.medication_revision.pharmacy_assistant <- as.data.frame(cbind(data$id, 
                                                                 data$S51c, 
                                                                 data$S52c, 
                                                                 data$S53c)) 
colnames(id.medication_revision.pharmacy_assistant) <- c("id", 
                                                         "revision.1", 
                                                         "revision.2", 
                                                         "revision.3")
data$medication_revision.pharmacy_assistant.yes_1 <- ifelse((id.medication_revision.pharmacy_assistant$revision.1 == 1) | 
                                                            (id.medication_revision.pharmacy_assistant$revision.2 == 1) | 
                                                            (id.medication_revision.pharmacy_assistant$revision.3 == 1),
                                                             1,
                                                             0)
table(data$medication_revision.pharmacy_assistant.yes_1, exclude = NULL)                                                            

### medications revision: physician
names(data)[names(data) == "S6c"] <- "medications_revision.physician"
data$medications_revision.physician <- factor(data$medications_revision.physician, 
                                              levels = c(1, 
                                                         2, 
                                                         3), 
                                              labels = c("yes", 
                                                         "no: non-medication revision", 
                                                         "no: no revision"))
table(data$medications_revision.physician, exclude = NULL)
data$medications_revision.physician.yes_1 <- ifelse(is.na(data$medications_revision.physician), 
                                                    NA, 
                                                    0)
data$medications_revision.physician.yes_1 <- ifelse(data$medications_revision.physician == "yes", 
                                                    1, 
                                                    data$medications_revision.physician.yes_1)

### prevented consequences
# multiple possible matches: C5a = C5b or C5c

### information gained
names(data)[names(data) == "S8c"] <- "information_gained"
data$information_gained <- factor(data$information_gained, 
                                  levels = c(1, 
                                             2, 
                                             3), 
                                  labels = c("yes, direct", 
                                             "yes, indirect", 
                                             "no"))
table(data$information_gained, exclude = NULL)
data$information_gained.yes_direct_1 <- ifelse(is.na(data$information_gained), 
                                               NA, 
                                               0)
data$information_gained.yes_direct_1 <- ifelse(data$information_gained == "yes, direct", 
                                               1, 
                                               data$information_gained.yes_direct_1)
data$information_gained.yes_indirect_1 <- ifelse(is.na(data$information_gained), 
                                                 NA, 
                                                 0)
data$information_gained.yes_indirect_1 <- ifelse(data$information_gained == "yes, indirect", 
                                                 1, 
                                                 data$information_gained.yes_indirect_1)
data$information_gained.no_1 <- ifelse(is.na(data$information_gained), 
                                       NA, 
                                       0)
data$information_gained.no_1 <- ifelse(data$information_gained == "no", 
                                       1, 
                                       data$information_gained.no_1)

####################################################################################################################

# Definition of an event

####################################################################################################################

table(data$medications_interaction, data$warnings, exclude = NULL) # Peculiar: 1 observation with interaction NA, warning yes.
View(table(data$medications_interaction, data$warnings, data$medications_revision.physician, exclude = NULL))

data$y <- ifelse((data$medications_interaction.yes_1 == 1) | 
                 (data$medications_revision.physician.yes_1 == 1), 
                  1, 
                  0)
data$y <- ifelse(is.na(data$y), 
                 0, 
                 data$y) # Assumption NA equals to no event. 
table(data$y, exclude = NULL)

####################################################################################################################

# Description of the sample indicating events

####################################################################################################################

## Socio-demographics

### age, y
ggplot(data, aes(x = age, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  theme_minimal()

### sex, y
table(data$sex, data$y, exclude = NULL)
prop.table(table(data$sex, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = sex, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  theme_minimal()

### education, y
table(data$education, data$y, exclude = NULL)
prop.table(table(data$education, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = education, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_minimal()

### medication literacy, y
table(data$medication_literacy, data$y, exclude = NULL)
prop.table(table(data$medication_literacy, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = medication_literacy, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "medication literacy") +
  theme_minimal()

### person responsible for medications, y
table(data$person_responsible_for_medications, data$y, exclude = NULL)
prop.table(table(data$person_responsible_for_medications, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = person_responsible_for_medications, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "person responsible for medications") +
  theme_minimal()

#### medication literacy, person responsible for medications, y
table(data$medication_literacy, data$person_responsible_for_medications, exclude = NULL)
prop.table(table(data$medication_literacy, data$person_responsible_for_medications, exclude = NULL), margin = 1)

table(data$medication_literacy, data$person_responsible_for_medications, data$y, exclude = NULL)
prop.table(table(subset(data, y == 0)$medication_literacy, subset(data, y == 0)$person_responsible_for_medications, exclude = NULL), margin = 1)
prop.table(table(subset(data, y == 1)$medication_literacy, subset(data, y == 1)$person_responsible_for_medications, exclude = NULL), margin = 1)

### housing, y
table(data$housing, data$y, exclude = NULL)
prop.table(table(data$housing, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = housing, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  theme_minimal()

## Diseases

### diseases, y
table(data$diseases, data$y, exclude = NULL)
prop.table(table(data$diseases, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = diseases, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_minimal()

### number of diseases, y
ggplot(data, aes(x = as.factor(n_diseases), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "number of diseases") +
  theme_minimal()

### eGFR, y
table(data$eGFR, data$y, exclude = NULL) # cfr. table(data$eGFR, data$medications_interaction, exclude = NULL) 
prop.table(table(data$eGFR, data$y, exclude = NULL), margin = 1) 

ggplot(data, aes(x = eGFR, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_minimal()

## Medication use

### Any medication use

#### patient-reported medication use, y
table(data$patient_reported_medication_use, data$y, exclude = NULL)# Peculiar: medications revision when patient did not report any medication use?
prop.table(table(data$patient_reported_medication_use, data$y, exclude = NULL), margin = 1) 

ggplot(data, aes(x = patient_reported_medication_use, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of medications") +
  theme_minimal()

#### pill box (week), y
table(data$pill_box.week, data$y, exclude = NULL)
prop.table(table(data$pill_box.week, data$y, exclude = NULL), margin = 1) 

ggplot(data, aes(x = pill_box.week, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of pill box (week)") +
  theme_minimal()

### High-risk medications

#### any high-risk medications, y
table(data$high_risk_medications, data$y, exclude = NULL)
prop.table(table(data$high_risk_medications, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = high_risk_medications, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of high-risk medications") +
  theme_minimal()

#### platelet aggregation inhibitors, y
table(data$platelet_aggregation_inhibitors.yes_1, data$y, exclude = NULL)
prop.table(table(data$platelet_aggregation_inhibitors.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(platelet_aggregation_inhibitors.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of platelet aggregation inhibitors", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### anticoagulants, y
table(data$anticoagulants.yes_1, data$y, exclude = NULL)
prop.table(table(data$anticoagulants.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(anticoagulants.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of anticoagulants", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### NSAIDs, y
table(data$NSAIDs_composite.yes_1, data$y, exclude = NULL)
prop.table(table(data$NSAIDs_composite.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(NSAIDs_composite.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of NSAIDs", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### diuretics, y
table(data$diuretics.yes_1, data$y, exclude = NULL)
prop.table(table(data$diuretics.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(diuretics.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of diuretics", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### RAS inhibitors, y
table(data$RAS_inhibitors.yes_1, data$y, exclude = NULL)
prop.table(table(data$diuretics.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(RAS_inhibitors.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of RAS inhibitors", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### systemic corticosteroids, y
table(data$systemic_corticosteroids.yes_1, data$y, exclude = NULL)
prop.table(table(data$systemic_corticosteroids.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(systemic_corticosteroids.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of systemic corticosteroids", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### opioids, y
table(data$opioids.yes_1, data$y, exclude = NULL)
prop.table(table(data$opioids.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(opioids.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of opioids", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### glucose-lowering medications, y
table(data$glucose_lowering_medications.yes_1, data$y, exclude = NULL)
prop.table(table(data$glucose_lowering_medications.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(glucose_lowering_medications.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of glucose-lowering medications", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### psychotropics, y
table(data$psychotropics.yes_1, data$y, exclude = NULL)
prop.table(table(data$psychotropics.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(psychotropics.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of psychotropics", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### cardiac medications, y
table(data$cardiac_medications.yes_1, data$y, exclude = NULL)
prop.table(table(data$cardiac_medications.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(cardiac_medications.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of cardiac medications", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### immunosuppressants, y
table(data$immunosuppressants.yes_1, data$y, exclude = NULL)
prop.table(table(data$immunosuppressants.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(immunosuppressants.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of immunosuppressants", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### oncolytics, y
table(data$oncolytics.yes_1, data$y, exclude = NULL)
prop.table(table(data$oncolytics.yes_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(oncolytics.yes_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of oncolytics", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### combinations of high-risk medications

##### number of high-risk medications, y
id.y.high_risk_medications <- data[, c("id",
                                     "y",
                                     "platelet_aggregation_inhibitors.yes_1", 
                                     "anticoagulants.yes_1",
                                     "NSAIDs_composite.yes_1",
                                     "diuretics.yes_1",
                                     "RAS_inhibitors.yes_1",
                                     "systemic_corticosteroids.yes_1",
                                     "opioids.yes_1",
                                     "glucose_lowering_medications.yes_1",
                                     "psychotropics.yes_1",
                                     "cardiac_medications.yes_1",
                                     "immunosuppressants.yes_1",
                                     "oncolytics.yes_1")]
id.y.high_risk_medications$n_high_risk_medications <- apply(id.y.high_risk_medications[,-c(1,2)], 1, sum) 

ggplot(id.y.high_risk_medications, aes(x = as.factor(n_high_risk_medications), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "number of high-risk medicatons") +
  theme_minimal()

#### specific combinations of high-risk medications, y
high_risk_medications.combination <- as.data.frame(table(id.y.high_risk_medications[,3:14]))
high_risk_medications.combination[,1:12] <- data.matrix(high_risk_medications.combination[,1:12]) - 1
high_risk_medications.combination$n_high_risk_medications <- apply(high_risk_medications.combination[,1:12], 1, sum)
table(high_risk_medications.combination$Freq)
high_risk_medications.combination <- subset(high_risk_medications.combination, Freq != 0)

high_risk_medications.combination.y.sum <- aggregate(id.y.high_risk_medications$y,
                   by = list(id.y.high_risk_medications$platelet_aggregation_inhibitors.yes_1,
                             id.y.high_risk_medications$anticoagulants.yes_1,
                             id.y.high_risk_medications$NSAIDs_composite.yes_1,
                             id.y.high_risk_medications$diuretics.yes_1,
                             id.y.high_risk_medications$RAS_inhibitors.yes_1,
                             id.y.high_risk_medications$systemic_corticosteroids.yes_1,
                             id.y.high_risk_medications$opioids.yes_1,
                             id.y.high_risk_medications$glucose_lowering_medications.yes_1,
                             id.y.high_risk_medications$psychotropics.yes_1,
                             id.y.high_risk_medications$cardiac_medications.yes_1,
                             id.y.high_risk_medications$immunosuppressants.yes_1,
                             id.y.high_risk_medications$oncolytics.yes_1),
                   sum)
colnames(high_risk_medications.combination.y.sum) <- c("platelet_aggregation_inhibitors.yes_1",
                                                       "anticoagulants.yes_1",
                                                       "NSAIDs_composite.yes_1",
                                                       "diuretics.yes_1",
                                                       "RAS_inhibitors.yes_1",
                                                       "systemic_corticosteroids.yes_1",
                                                       "opioids.yes_1",
                                                       "glucose_lowering_medications.yes_1",
                                                       "psychotropics.yes_1",
                                                       "cardiac_medications.yes_1",
                                                       "immunosuppressants.yes_1",
                                                       "oncolytics.yes_1",
                                                       "y.sum")

high_risk_medications.combination <- merge(high_risk_medications.combination, 
                                           high_risk_medications.combination.y.sum,
                                           by = c("platelet_aggregation_inhibitors.yes_1",
                                                  "anticoagulants.yes_1",
                                                  "NSAIDs_composite.yes_1",
                                                  "diuretics.yes_1",
                                                  "RAS_inhibitors.yes_1",
                                                  "systemic_corticosteroids.yes_1",
                                                  "opioids.yes_1",
                                                  "glucose_lowering_medications.yes_1",
                                                  "psychotropics.yes_1",
                                                  "cardiac_medications.yes_1",
                                                  "immunosuppressants.yes_1",
                                                  "oncolytics.yes_1"))

high_risk_medications.combination$y.prop <- high_risk_medications.combination$y.sum / high_risk_medications.combination$Freq
View(high_risk_medications.combination)

### Medication without prescription

#### proton pump inhibitors, y
table(data$medication_without_prescription.proton_pump_inhibitors_1, data$y, exclude = NULL)
prop.table(table(data$medication_without_prescription.proton_pump_inhibitors_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(medication_without_prescription.proton_pump_inhibitors_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of proton pump inhibitors", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### hypericum, y
table(data$medication_without_prescription.hypericum_1, data$y, exclude = NULL)
prop.table(table(data$medication_without_prescription.hypericum_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(medication_without_prescription.hypericum_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of hypericum", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### red yeast rice, y
table(data$medication_without_prescription.red_yeast_rice_1, data$y, exclude = NULL)
prop.table(table(data$medication_without_prescription.red_yeast_rice_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(medication_without_prescription.red_yeast_rice_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of red yeast rice", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### multi vitamins / dietary supplement, y
table(data$medication_without_prescription.multi_vitamins_dietary_supplement_1, data$y, exclude = NULL)
prop.table(table(data$medication_without_prescription.multi_vitamins_dietary_supplement_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(medication_without_prescription.multi_vitamins_dietary_supplement_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of multi-vitamins / dietary supplement", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### other, y
table(data$medication_without_prescription.other_1, data$y, exclude = NULL)
prop.table(table(data$medication_without_prescription.other_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(medication_without_prescription.other_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of medication without prescription, excl. NSAIDs, proton pump inhibitors, hypericum, red yeast rice, multi vitamins / dietary supplement", 
                   labels = c("0" = "no", 
                              "1" = "yes")) +
  theme_minimal()

#### none, y
table(data$medication_without_prescription.none_1, data$y, exclude = NULL)
prop.table(table(data$medication_without_prescription.none_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(medication_without_prescription.none_1), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "use of medication without prescription", 
                   labels = c("0" = "yes", 
                              "1" = "no")) +
  theme_minimal()

### Allergy

#### allergy medication, y
table(data$allergy_medication, data$y, exclude = NULL)
prop.table(table(data$allergy_medication, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = allergy_medication, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "allergy concerning medication") +
  theme_minimal()

## Hospital use

### Current visit specialty, y
table(data$out_specialty_current_MUMC, data$y, exclude = NULL)
prop.table(table(data$out_specialty_current_MUMC, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = out_specialty_current_MUMC, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "Current visit outpatient clinic MUMC+: specialty", 
                   guide = guide_axis(angle = 90)) +
  theme_minimal()

### In/out patient in past 12 months

#### MUMC+

##### cardiology, y
table(data$inout_specialty_12m_MUMC.cardiology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.cardiology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.cardiology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: cardiology") +
  theme_minimal()

##### urology, y
table(data$inout_specialty_12m_MUMC.urology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.urology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.urology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: urology") +
  theme_minimal()

##### psychiatry, y
table(data$inout_specialty_12m_MUMC.psychiatry_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.psychiatry_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.psychiatry_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: psychiatry") +
  theme_minimal()

##### otorhinolaryngology, y
table(data$inout_specialty_12m_MUMC.otorhinolaryngology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.otorhinolaryngology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.otorhinolaryngology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: otorhinolaryngology") +
  theme_minimal()

##### ophthalmology, y
table(data$inout_specialty_12m_MUMC.ophthalmology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.ophthalmology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.ophthalmology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: ophthalmology") +
  theme_minimal()

##### internal medicine, y
table(data$inout_specialty_12m_MUMC.internal_medicine_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.internal_medicine_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.internal_medicine_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: internal medicine") +
  theme_minimal()

##### surgery, y
table(data$inout_specialty_12m_MUMC.surgery_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.surgery_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.surgery_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: surgery") +
  theme_minimal()

##### orthopedics, y
table(data$inout_specialty_12m_MUMC.orthopedics_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.orthopedics_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.orthopedics_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: orthopedics") +
  theme_minimal()

##### plastic surgery, y
table(data$inout_specialty_12m_MUMC.plastic_surgery_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.plastic_surgery_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.plastic_surgery_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: plastic surgery") +
  theme_minimal()

##### obstetrics and gynaecology, y
table(data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: obstetrics and gynaecology") +
  theme_minimal()

##### neurology, y
table(data$inout_specialty_12m_MUMC.neurology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.neurology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.neurology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: neurology") +
  theme_minimal()

##### dermatology, y
table(data$inout_specialty_12m_MUMC.dermatology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.dermatology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.dermatology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: dermatology") +
  theme_minimal()

##### gastroenterology, y
table(data$inout_specialty_12m_MUMC.gastroenterology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.gastroenterology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.gastroenterology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: gastroenterology") +
  theme_minimal()

##### pneumology, y
table(data$inout_specialty_12m_MUMC.pneumology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.pneumology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.pneumology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: pneumology") +
  theme_minimal()

##### rheumatology, y
table(data$inout_specialty_12m_MUMC.rheumatology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.rheumatology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.rheumatology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"),
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: rheumatology") +
  theme_minimal()

##### pediatrics, y
table(data$inout_specialty_12m_MUMC.pediatrics_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.pediatrics_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.pediatrics_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: pediatrics") +
  theme_minimal()

##### anesthesiology, y
table(data$inout_specialty_12m_MUMC.anesthesiology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.anesthesiology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_MUMC.anesthesiology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: MUMC+: anesthesiology") +
  theme_minimal()

#### other hospital

##### cardiology, y
table(data$inout_specialty_12m_other_hospital.cardiology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.cardiology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.cardiology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: cardiology") +
  theme_minimal()

##### urology, y
table(data$inout_specialty_12m_other_hospital.urology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.urology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.urology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: urology") +
  theme_minimal()

##### psychiatry, y
table(data$inout_specialty_12m_other_hospital.psychiatry_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.psychiatry_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.psychiatry_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: psychiatry") +
  theme_minimal()

##### otorhinolaryngology, y
table(data$inout_specialty_12m_other_hospital.otorhinolaryngology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.otorhinolaryngology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.otorhinolaryngology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: otorhinolaryngology") +
  theme_minimal()

##### ophthalmology, y
table(data$inout_specialty_12m_other_hospital.ophthalmology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.ophthalmology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.ophthalmology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: ophthalmology") +
  theme_minimal()

##### internal medicine, y
table(data$inout_specialty_12m_other_hospital.internal_medicine_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.internal_medicine_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.internal_medicine_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: internal medicine") +
  theme_minimal()

##### surgery, y
table(data$inout_specialty_12m_other_hospital.surgery_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.surgery_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.surgery_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: surgery") +
  theme_minimal()

##### orthopedics, y
table(data$inout_specialty_12m_other_hospital.orthopedics_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.orthopedics_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.orthopedics_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: orthopedics") +
  theme_minimal()

##### plastic surgery, y
table(data$inout_specialty_12m_other_hospital.plastic_surgery_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.plastic_surgery_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.plastic_surgery_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: plastic surgery") +
  theme_minimal()

##### obstetrics and gynaecology, y
table(data$inout_specialty_12m_other_hospital.obstetrics_and_gynaecology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.obstetrics_and_gynaecology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.obstetrics_and_gynaecology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: obstetrics and gynaecology") +
  theme_minimal()

##### neurology, y
table(data$inout_specialty_12m_other_hospital.neurology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.neurology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.neurology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: neurology") +
  theme_minimal()

##### dermatology, y
table(data$inout_specialty_12m_other_hospital.dermatology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.dermatology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.dermatology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: dermatology") +
  theme_minimal()

##### gastroenterology, y
table(data$inout_specialty_12m_other_hospital.gastroenterology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.gastroenterology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.gastroenterology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: gastroenterology") +
  theme_minimal()

##### pneumology, y
table(data$inout_specialty_12m_other_hospital.pneumology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.pneumology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.pneumology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: pneumology") +
  theme_minimal()

##### rheumatology, y
table(data$inout_specialty_12m_other_hospital.rheumatology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.rheumatology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.rheumatology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: rheumatology") +
  theme_minimal()

##### pediatrics, y
table(data$inout_specialty_12m_other_hospital.pediatrics_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.pediatrics_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.pediatrics_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: pediatrics") +
  theme_minimal()

##### anesthesiology, y
table(data$inout_specialty_12m_other_hospital.anesthesiology_1, data$y, exclude = NULL)
prop.table(table(data$inout_specialty_12m_other_hospital.anesthesiology_1, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = inout_specialty_12m_other_hospital.anesthesiology_1, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "In/outpatient in past 12 months: other hospital: anesthesiology") +
  theme_minimal()

### hospitalisation in past 12 months, y

#### MUMC+
table(data$in_all_12m_MUMC, data$y, exclude = NULL)
prop.table(table(data$in_all_12m_MUMC, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = in_all_12m_MUMC, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "Inpatient in past 12 months: MUMC+") +
  theme_minimal()

#### other hospital
table(data$in_all_12m_other_hospital, data$y, exclude = NULL)
prop.table(table(data$in_all_12m_other_hospital, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = in_all_12m_other_hospital, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "Inpatient in past 12 months: other hospital") +
  theme_minimal()

### number of ER visits in past 12 months, y

#### MUMC+
ggplot(data, aes(x = n_ER_12m_MUMC, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_continuous(name = "Number of ER visits: MUMC+", 
                     breaks = 0:20, 
                     labels = 0:20) +
  theme_minimal()

#### other hospital
ggplot(data, aes(x = n_ER_12m_other_hospital, fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_continuous(name = "Number of ER visits: other hospital", 
                     breaks = 0:20, 
                     labels = 0:20) +
  theme_minimal()

## Information gained 

### information gained, y
table(data$information_gained, data$y, exclude = NULL)
prop.table(table(data$information_gained, data$y, exclude = NULL), margin = 1)

ggplot(data, aes(x = as.factor(information_gained), fill = as.factor(y))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "deepskyblue4", 
                               "1" = "red3"), 
                    labels = c("0" = "no medications revision", 
                               "1" = "medications revision"), 
                    name = NULL) +
  scale_x_discrete(name = "information gained") +
  theme_minimal()

####################################################################################################################

# Modelling

####################################################################################################################

# PRELIMINARY

## 5 proposed predictors

### Data objects

#### data.m
data.m <- data[, c("y", 
                    "age", 
                    "n_medications", 
                    "high_risk_medications.yes_1",
                    "out_specialty_current_MUMC.cardiology_1",
                    "out_specialty_current_MUMC.surgery_1",
                    "out_specialty_current_MUMC.surgery_hpb_1",
                    "out_specialty_current_MUMC.surgery_trauma_1",
                    "out_specialty_current_MUMC.surgery_cardiovascular_1",
                    "out_specialty_current_MUMC.dermatology_1",
                    "out_specialty_current_MUMC.internal_medicine_D_1",
                    "out_specialty_current_MUMC.internal_medicine_general_1",
                    "out_specialty_current_MUMC.internal_medicine_geriatry_1",
                    "out_specialty_current_MUMC.neurology_1",
                    "housing.independent_1",
                    "housing.independent_with_home_care_1",
                    "housing.institution_1")] 

data.m$y <- factor(as.character(data.m$y),
                   levels = c("0", "1"),
                   labels = c("no_event", "event"))

#### data.m.imp (1 id, single imputation with kNN) # id == "7EGpgbdhnP" : missing values for n_medications, high_risk_medications.yes_1, and housing 
data.m.imp <- data.m[, -1]
preProcValues <- preProcess(data.matrix(data.m.imp), method = "knnImpute", k = 5)
data.m.imp <- predict(preProcValues, data.m.imp)
data.m.imp <- as.data.frame(cbind(data.m$y, data.m.imp))
colnames(data.m.imp)[1] <- "y"

#### data.m.cc # id == "7EGpgbdhnP" : missing values for n_medications, high_risk_medications.yes_1, and housing
data.m.cc <- data[-345,] 

data.m.cc$y <- factor(as.character(data.m.cc$y),
                      levels = c("0", "1"),
                      labels = c("no_event", "event"))

### glmnet
fit.control.m1 <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 10,
                            search = "random",
                            verboseIter = TRUE,
                            savePredictions = TRUE)

#### data.m.imp
fit.m1.imp <- train(y ~ 
                  age +
                  n_medications +
                  high_risk_medications.yes_1 +
                  out_specialty_current_MUMC.surgery_1 +
                  out_specialty_current_MUMC.surgery_hpb_1 +
                  out_specialty_current_MUMC.surgery_trauma_1 +
                  out_specialty_current_MUMC.surgery_cardiovascular_1 +
                  out_specialty_current_MUMC.dermatology_1 +
                  out_specialty_current_MUMC.internal_medicine_D_1 +
                  out_specialty_current_MUMC.internal_medicine_general_1 +
                  out_specialty_current_MUMC.internal_medicine_geriatry_1 +
                  out_specialty_current_MUMC.neurology_1 +
                  housing.independent_with_home_care_1 +
                  housing.institution_1,
                data = data.m.imp,
                method = "glmnet",
                tuneLength = 25,
                trControl = fit.control.m1) # reference category out_specialty_current_MUMC: cardiology; reference category housing: independent

coef(fit.m1.imp$finalModel, fit.m1.imp$finalModel$lambdaOpt)

pred.prob.m1.imp <- predict(fit.m1.imp, data.m.imp, type = "prob") # Probability of an event = 0.04545455

#### data.m.cc
fit.m1.cc <- train(y ~ 
                   age +
                   n_medications +
                   high_risk_medications.yes_1 +
                   out_specialty_current_MUMC.surgery_1 +
                   out_specialty_current_MUMC.surgery_hpb_1 +
                   out_specialty_current_MUMC.surgery_trauma_1 +
                   out_specialty_current_MUMC.surgery_cardiovascular_1 +
                   out_specialty_current_MUMC.dermatology_1 +
                   out_specialty_current_MUMC.internal_medicine_D_1 +
                   out_specialty_current_MUMC.internal_medicine_general_1 +
                   out_specialty_current_MUMC.internal_medicine_geriatry_1 +
                   out_specialty_current_MUMC.neurology_1 +
                   housing.independent_with_home_care_1 +
                   housing.institution_1,
                 data = data.m.cc,
                 method = "glmnet",
                 tuneLength = 25,
                 trControl = fit.control.m1) # reference category out_specialty_current_MUMC: cardiology; reference category housing: independent

coef(fit.m1.cc$finalModel, fit.m1.cc$finalModel$lambdaOpt)

pred.prob.m1.cc <- predict(fit.m1.cc, data.m.cc, type = "prob") # Probability of an event = 0.04549675

