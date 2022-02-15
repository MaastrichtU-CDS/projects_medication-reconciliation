# PACKAGES ----

library(readxl)
library(ggplot2)

# DATA ----

data <- read_excel("C:/Users/P70070766/Documents/Medication reconciliation/Data/Kopie van QZImport107x-Anoniem.xlsx", skip = 4) 
colnames(data)[1] <- "id"
table(is.na(data$id))
data <- data[data$id != "AGFspkFhsI", ] # All missing values. Has been checked, most likely data entry error. 
data <- data[data$id != "7EGpgbdhnP", ] # Many missing values. Has been checked, most likely data entry error. 
dup_id <- data[(duplicated(data$id, fromLast = FALSE) == TRUE) | duplicated(data$id, fromLast = TRUE) == TRUE,]$id
#View(subset(data, (id %in% dup_id)))
data <- data[!c( (data$id == "RWhAI8VyRg") & (data$SPEC == "NEU VASC") ), ] # Duplicated ID has been checked, correct entries of same patient. Removed second entry. (Note: entries are not according to time.)
data <- data[!c( (data$id == "8wTTvGKcfs") & (data$SPEC == "REUM SPA") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "+VwiZPm8lX") & (data$SPEC == "UROL ONCO") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "a+/hkR+SLB") & (data$SPEC == "UROL FU") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "A0pW4TS3pV") & (data$SPEC == "UROL FU") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "2QFpSTWBcc") & (data$SPEC == "DERMA") ), ] # Duplicated ID has been checked, correct entries of same patient. Removed second entry. (Note: entries are not according to time.)
data <- data[!c( (data$id == "HLkzz6SpkZ") & (data$SPEC == "UROL FU") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "Iy68G3IisW") & (data$SPEC == "MDL NGM") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "2oNzMJDTeO") & (data$SPEC == "UROL FU") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "tqsHn6eVH+") & (data$SPEC == "NEURO") ), ] # Duplicated ID has been checked, correct entries of same patient. Removed second entry. (Note: entries are not according to time.)
data <- data[!c( (data$id == "DUzC4IENg4") & (data$SPEC == "UROL FU") ), ] # Duplicated ID has been checked, correct entries of same patient. Removed second entry. (Note: entries are not according to time.)
data <- data[!c( (data$id == "NJIvDpH2nF") & (data$SPEC == "CHI VAAT") ), ] # Duplicated ID has been checked, correct entries of same patient. Removed second entry. (Note: entries are not according to time.)
data <- data[!c( (data$id == "apeIkxgLow") & (data$SPEC == "UROL FU") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "8ColabP9nF") & (data$SPEC == "KNO-ALG") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "azHk1mtt7Q") & (data$SPEC == "UROL ONCO") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "icPvB/wJqm") & (data$SPEC == "INTERN ALG") ), ] # Duplicated ID has been checked, correct entries of same patient. Removed second entry. (Note: entries are not according to time.)
data <- data[!c( (data$id == "icPvB/wJqm") & (data$SPEC == "UROL FU") ), ] # Duplicated ID has been checked, correct entries of same patient. Removed third entry. (Note: entries are not according to time.)
data <- data[!c( (data$id == "DIj75C1KaE") & (data$SPEC == "ORTHO") ), ] # REQUEST TO CHECK.
data <- data[!c( (data$id == "9WNYBZm5+T") & (data$SPEC == "INTERN-D") ), ] # Duplicated ID has been checked, correct entries of same patient. Removed second entry. (Note: entries are not according to time.)

# CODING RESPONSES ----

## PART 1 ---- 

### PART 1.A ----

#### sex
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

#### age
names(data)[names(data) == "A2a"] <- "age"
summary(data$age)
ggplot(data, aes(x = age)) + 
  geom_histogram() + 
  theme_minimal()

data$age.cat_10 <- ifelse(data$age < 10, "0 - 9 years", data$age)
data$age.cat_10 <- ifelse((data$age >= 10) & (data$age < 20), "10 - 19 years", data$age.cat_10)
data$age.cat_10 <- ifelse((data$age >= 20) & (data$age < 30), "20 - 29 years", data$age.cat_10)
data$age.cat_10 <- ifelse((data$age >= 30) & (data$age < 40), "30 - 39 years", data$age.cat_10)
data$age.cat_10 <- ifelse((data$age >= 40) & (data$age < 50), "40 - 49 years", data$age.cat_10)
data$age.cat_10 <- ifelse((data$age >= 50) & (data$age < 60), "50 - 59 years", data$age.cat_10)
data$age.cat_10 <- ifelse((data$age >= 60) & (data$age < 70), "60 - 69 years", data$age.cat_10)
data$age.cat_10 <- ifelse((data$age >= 70) & (data$age < 80), "70 - 79 years", data$age.cat_10)
data$age.cat_10 <- ifelse((data$age >= 80) & (data$age < 90), "80 - 89 years", data$age.cat_10)
data$age.cat_10 <- ifelse((data$age >= 90) & (data$age < 100), "90 - 99 years", data$age.cat_10)
data$age.cat_10 <- ifelse(data$age >= 100, "100+ years", data$age.cat_10)
table(data$age.cat_10, exclude = NULL)
data$age.cat_10 <- ordered(data$age.cat_10, levels = c("0 - 9 years",
                                                 "10 - 19 years",
                                                 "20 - 29 years",
                                                 "30 - 39 years",
                                                 "40 - 49 years",
                                                 "50 - 59 years",
                                                 "60 - 69 years",
                                                 "70 - 79 years",
                                                 "80 - 89 years",
                                                 "90 - 99 years",
                                                 "100+ years"))

data$age.centered <- data$age - mean(data$age)

#### diseases
names(data)[names(data) == "A10a"] <- "diseases"
diseases.list <- strsplit(data$diseases, ",")
table(data$diseases, exclude = NULL) # '8. None of the above' was never selected. 
data$diseases.diabetes_1 <- ifelse(lapply(diseases.list, function(x) { "1" %in% x}),
                                   1, 
                                   0) # Assumption NA equals to negative indication.
data$diseases.rheumatoid_arthritis_1 <- ifelse(lapply(diseases.list, function(x) { "2" %in% x}),
                                               1, 
                                               0) # Assumption NA equals to negative indication. 
data$diseases.asthma_COPD_1 <- ifelse(lapply(diseases.list, function(x) { "3" %in% x}),
                                      1, 
                                      0) # Assumption NA equals to negative indication. 
data$diseases.cardiovascular_disease_1 <- ifelse(lapply(diseases.list, function(x) { "4" %in% x}),
                                                 1, 
                                                 0) # Assumption NA equals to negative indication.
data$diseases.cardiac_arrhythmia_1 <- ifelse(lapply(diseases.list, function(x) { "5" %in% x}),
                                             1, 
                                             0) # Assumption NA equals to negative indication.
data$diseases.heart_failure_1 <- ifelse(lapply(diseases.list, function(x) { "6" %in% x}),
                                        1, 
                                        0) # Assumption NA equals to negative indication.
data$diseases.cancer_1 <- ifelse(lapply(diseases.list, function(x) { "7" %in% x}),
                                 1, 
                                 0) # Assumption NA equals to negative indication.
data$diseases.none_of_the_above_1 <- ifelse(lapply(diseases.list, function(x) { "8" %in% x}),
                                            1, 
                                            0) # Assumption NA equals to negative indication. 

#### number of diseases
names(data)[names(data) == "A9a"] <- "n_diseases"
table(data$n_diseases, exclude = NULL)

#data$n_diseases.centered <- data$n_diseases - mean(data$n_diseases) # REQUEST TO CHECK CAUSE OF NA.

#### eGFR
names(data)[names(data) == "A12a"] <- "eGFR"
data$eGFR <- ifelse(is.na(data$eGFR), 
                    6, 
                    data$eGFR)
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
data$eGFR.60_89_1 <- ifelse(data$eGFR == "60-89 ml/min", 
                            1, 
                            0)
data$eGFR.30_59_1 <- ifelse(data$eGFR == "30-59 ml/min", 
                            1, 
                            0)
data$eGFR.15_29_1 <- ifelse(data$eGFR == "15-29 ml/min", 
                            1, 
                            0)
data$eGFR.below_15_1 <- ifelse(data$eGFR == "< 15 ml/min", 
                               1, 
                               0)
data$eGFR.NA_1 <- ifelse(data$eGFR == "unknown or measured more than 12 months ago", 
                         1, 
                         0)

data$eGFR.below_60_1 <- ifelse( (data$eGFR.30_59_1 == 1)
                                |
                                (data$eGFR.15_29_1 == 1)
                                |
                                (data$eGFR.below_15_1 == 1),
                                1,
                                0) # Assumption NA equals to negative indication. 

#### number of visits all specialties outpatient clinics in past 12 months in MUMC+ 
names(data)[names(data) == "A14a"] <- "n_visits_out_all_12m_MUMC.cat"
data$n_visits_out_all_12m_MUMC.cat <- factor(data$n_visits_out_all_12m_MUMC.cat,
                                             levels = c(1, 
                                                        2, 
                                                        3, 
                                                        4), 
                                             labels = c("0", 
                                                        "1", 
                                                        "2-5", 
                                                        "> 5"))
table(data$n_visits_out_all_12m_MUMC.cat, exclude = NULL)
data$n_visits_out_all_12m_MUMC.0_1 <- ifelse(data$n_visits_out_all_12m_MUMC.cat == "0",
                                             1, 
                                             0)
data$n_visits_out_all_12m_MUMC.1_1 <- ifelse(data$n_visits_out_all_12m_MUMC.cat == "1", 
                                             1, 
                                             0)
data$n_visits_out_all_12m_MUMC.2_to_5_1 <- ifelse(data$n_visits_out_all_12m_MUMC.cat == "2-5",
                                                  1, 
                                                  0)
data$n_visits_out_all_12m_MUMC.above_5_1 <- ifelse(data$n_visits_out_all_12m_MUMC.cat == "> 5",
                                                   1, 
                                                   0)

#### visited specialty as outpatient and/or inpatient in past 12 months in MUMC+
inout_specialty_12m_MUMC.list <- strsplit(data$A150a, ",")
data$inout_specialty_12m_MUMC.cardiology_1 <-ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "1" %in% x }), 
                                                    1, 
                                                    0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.cardiology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.urology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "2" %in% x }),
                                                  1, 
                                                  0) # Assumption NA equals to negative indication. 
table(data$inout_specialty_12m_MUMC.urology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.psychiatry_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "3" %in% x }), 
                                                     1, 
                                                     0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.psychiatry_1, exclude = NULL)
data$inout_specialty_12m_MUMC.otorhinolaryngology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "4" %in% x }),
                                                              1, 
                                                              0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.otorhinolaryngology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.ophthalmology_1 <-ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "5" %in% x }),
                                                       1, 
                                                       0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.ophthalmology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.internal_medicine_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "6" %in% x }),
                                                            1, 
                                                            0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.internal_medicine_1, exclude = NULL)
data$inout_specialty_12m_MUMC.surgery_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "7" %in% x }),
                                                  1, 
                                                  0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.surgery_1, exclude = NULL)
data$inout_specialty_12m_MUMC.orthopedics_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "8" %in% x }),
                                                      1, 
                                                      0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.orthopedics_1, exclude = NULL)
data$inout_specialty_12m_MUMC.plastic_surgery_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "9" %in% x }),
                                                          1, 
                                                          0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.plastic_surgery_1, exclude = NULL)
data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "10" %in% x }),
                                                                     1,
                                                                     0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.neurology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "11" %in% x }),
                                                    1, 
                                                    0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.neurology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.dermatology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "12" %in% x }),
                                                      1, 
                                                      0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.dermatology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.gastroenterology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "13" %in% x }),
                                                           1, 
                                                           0) # Assumption NA equals to negative indication. 
table(data$inout_specialty_12m_MUMC.gastroenterology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.pneumology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "14" %in% x }),
                                                     1, 
                                                     0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.pneumology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.rheumatology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "15" %in% x }),
                                                       1, 
                                                       0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.rheumatology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.do_not_select_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "16" %in% x }), 
                                                        1, 
                                                        0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.do_not_select_1, exclude = NULL)
data$inout_specialty_12m_MUMC.pediatrics_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "17" %in% x }), 
                                                     1, 
                                                     0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.pediatrics_1, exclude = NULL)
data$inout_specialty_12m_MUMC.anesthesiology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "18" %in% x }), 
                                                         1, 
                                                         0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.anesthesiology_1, exclude = NULL)
data$inout_specialty_12m_MUMC.other_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "19" %in% x }),
                                                1, 
                                                0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_MUMC.other_1, exclude = NULL)

#### hospitalisation in past 12 months in MUMC+
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

#### number of ER visits in past 12 months in MUMC+
names(data)[names(data) == "A18a"] <- "n_ER_12m_MUMC"
table(data$n_ER_12m_MUMC, exclude = NULL)

data$n_ER_12m_MUMC.centered <- data$n_ER_12m_MUMC - mean(data$n_ER_12m_MUMC)

data$ER_12m_MUMC.yes_1 <- ifelse(data$n_ER_12m_MUMC == 0, 
                                 0, 
                                 1)

### PART 1.B ----

#### number of medications
names(data)[names(data) == "B1a"] <- "n_prescribed_medications"
table(data$n_prescribed_medications, exclude = NULL)

data$n_prescribed_medications.centered <- data$n_prescribed_medications - mean(data$n_prescribed_medications)

#### presence of following high-risk medications 
high_risk_medications.list <- strsplit(data$B7a, ",")
data$high_risk_medications.platelet_aggregation_inhibitors_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "1" %in% x }),
                                                                       1, 
                                                                       0) # Assumption NA equals to negative indication. 
table(data$high_risk_medications.platelet_aggregation_inhibitors_1, exclude = NULL)
data$high_risk_medications.anticoagulants_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "2" %in% x }),
                                                      1, 
                                                      0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.anticoagulants_1, exclude = NULL)
data$high_risk_medications.NSAIDs_1 <-ifelse(lapply(high_risk_medications.list, function(x) { "3" %in% x }),
                                             1, 
                                             0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.NSAIDs_1, exclude = NULL)
data$high_risk_medications.diuretics_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "4" %in% x }),
                                                 1, 
                                                 0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.diuretics_1, exclude = NULL)
data$high_risk_medications.RAS_inhibitors_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "5" %in% x }),
                                                      1, 
                                                      0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.RAS_inhibitors_1, exclude = NULL)
data$high_risk_medications.systemic_corticosteroids_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "6" %in% x }), 
                                                                1, 
                                                                0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.systemic_corticosteroids_1, exclude = NULL) # Low percentage of patients uses systemic corticosteroids.
data$high_risk_medications.opioids_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "7" %in% x }),
                                               1, 
                                               0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.opioids_1, exclude = NULL)
data$high_risk_medications.glucose_lowering_medications_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "8" %in% x }),
                                                                    1, 
                                                                    0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.glucose_lowering_medications_1, exclude = NULL)
data$high_risk_medications.psychotropics_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "9" %in% x }),
                                                     1, 
                                                     0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.psychotropics_1, exclude = NULL)
data$high_risk_medications.cardiac_medications_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "10" %in% x }),
                                                           1, 
                                                           0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.cardiac_medications_1, exclude = NULL)
data$high_risk_medications.immunosuppressants_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "11" %in% x }),
                                                          1, 
                                                          0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.immunosuppressants_1, exclude = NULL) # Low percentage of patients uses immunosuppressants.
data$high_risk_medications.oncolytics_1 <- ifelse(lapply(high_risk_medications.list, function(x) { "12" %in% x }),
                                                  1, 
                                                  0) # Assumption NA equals to negative indication.
table(data$high_risk_medications.oncolytics_1, exclude = NULL) # Low percentage of patients uses oncolytics.
data$high_risk_medications.cardiovascular_composite_1 <- ifelse( (data$high_risk_medications.platelet_aggregation_inhibitors_1 == 1)
                                                                 |
                                                                 (data$high_risk_medications.anticoagulants_1 == 1)
                                                                 |
                                                                 (data$high_risk_medications.diuretics_1 == 1)
                                                                 |
                                                                 (data$high_risk_medications.RAS_inhibitors_1 == 1)
                                                                 |
                                                                 (data$high_risk_medications.cardiac_medications_1 == 1),
                                                                 1,
                                                                 0 )
table(data$high_risk_medications.cardiovascular_composite_1, exclude = NULL)
                                                              
#### high-risk medications
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

### PART 1 C ----

#### current visit specialty 
data$out_specialty_current_MUMC <- data$SPEC
data$out_specialty_current_MUMC <- factor(data$out_specialty_current_MUMC, 
                                          levels = c("CARDIO", 
                                                     "CHI", 
                                                     "CHI HPB",
                                                     "CHI TRAU",
                                                     "CHI VAAT",
                                                     "DERMA",
                                                     "INTE VASC",
                                                     "INTERN-D", 
                                                     "INTERN ALG", 
                                                     "INTERN GER",
                                                     "KNO-ALG",
                                                     "MDL-IBD",
                                                     "MDL NGM",
                                                     "NEU VASC",
                                                     "NEURO",
                                                     "ORTHO",
                                                     "REUM JICHT",
                                                     "REUM SPA",
                                                     "REUMA",
                                                     "UROL FU",
                                                     "UROL ONCO"), 
                                          labels = c("cardiology",
                                                     "surgery",
                                                     "surgery: hepato-pancreato-biliary", 
                                                     "surgery: trauma",
                                                     "surgery: vascular", 
                                                     "dermatology",
                                                     "internal medicine: vascular",
                                                     "internal medicine: D", 
                                                     "internal medicine: general",
                                                     "internal medicine: geriatrics",
                                                     "otorhinolaryngology",
                                                     "gastroenterology: inflammatory bowel diseases",
                                                     "gastroenterology: neuro",
                                                     "neurology: vascular",
                                                     "neurology",
                                                     "orthopedics",
                                                     "rheumatology: gout",
                                                     "rheumatology: SPA",
                                                     "rheumatology",
                                                     "urology: functional",
                                                     "urology: oncology"))
table(data$out_specialty_current_MUMC, exclude = NULL)
data$out_specialty_current_MUMC.cardiology_1 <- ifelse(data$out_specialty_current_MUMC == "cardiology", 
                                                       1, 
                                                       0)
data$out_specialty_current_MUMC.surgery_1 <- ifelse(data$out_specialty_current_MUMC == "surgery", 
                                                    1, 
                                                    0)
data$out_specialty_current_MUMC.surgery_hpb_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: hepato-pancreato-biliary", 
                                                        1, 
                                                        0)
data$out_specialty_current_MUMC.surgery_trauma_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: trauma", 
                                                           1, 
                                                           0)
data$out_specialty_current_MUMC.surgery_vascular_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: vascular", 
                                                                   1, 
                                                                   0)
data$out_specialty_current_MUMC.surgery_composite_1 <- ifelse((data$out_specialty_current_MUMC.surgery_1 == 1) 
                                                              | 
                                                              (data$out_specialty_current_MUMC.surgery_hpb_1 == 1) 
                                                              | 
                                                              (data$out_specialty_current_MUMC.surgery_trauma_1 == 1) 
                                                              | 
                                                              (data$out_specialty_current_MUMC.surgery_vascular_1 == 1), 
                                                              1, 
                                                              0)
data$out_specialty_current_MUMC.dermatology_1 <- ifelse(data$out_specialty_current_MUMC == "dermatology", 
                                                        1, 
                                                        0)
data$out_specialty_current_MUMC.internal_medicine_vascular_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: vascular", 
                                                                       1, 
                                                                       0)
data$out_specialty_current_MUMC.internal_medicine_D_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: D", 
                                                                1, 
                                                                0)
data$out_specialty_current_MUMC.internal_medicine_general_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: general", 
                                                                      1, 
                                                                      0)
data$out_specialty_current_MUMC.internal_medicine_geriatrics_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: geriatrics", 
                                                                         1, 
                                                                         0)
data$out_specialty_current_MUMC.internal_medicine_composite_1 <- ifelse((data$out_specialty_current_MUMC.internal_medicine_D_1 == 1) 
                                                                        | 
                                                                        (data$out_specialty_current_MUMC.internal_medicine_general_1 == 1) 
                                                                        | 
                                                                        (data$out_specialty_current_MUMC.internal_medicine_geriatrics_1 == 1),
                                                                        1, 
                                                                        0)
data$out_specialty_current_MUMC.otorhinolaryngology_1 <- ifelse(data$out_specialty_current_MUMC == "otorhinolaryngology",
                                                                1,
                                                                0)
data$out_specialty_current_MUMC.gastroenterology_ibd_1 <- ifelse(data$out_specialty_current_MUMC == "gastroenterology: inflammatory bowel diseases",
                                                                 1,
                                                                 0)
data$out_specialty_current_MUMC.gastroenterology_neuro_1 <- ifelse(data$out_specialty_current_MUMC == "gastroenterology: neuro",
                                                                   1,
                                                                   0)
data$out_specialty_current_MUMC.gastroenterology_composite_1 <- ifelse((data$out_specialty_current_MUMC.gastroenterology_ibd_1 == 1)
                                                                       |
                                                                       (data$out_specialty_current_MUMC.gastroenterology_neuro_1 == 1),
                                                                       1,
                                                                       0)
data$out_specialty_current_MUMC.neurology_vascular_1 <- ifelse(data$out_specialty_current_MUMC == "neurology: vascular",
                                                               1,
                                                               0)
data$out_specialty_current_MUMC.neurology_1 <- ifelse(data$out_specialty_current_MUMC == "neurology", 
                                                      1, 
                                                      0)
data$out_specialty_current_MUMC.neurology_composite_1 <- ifelse((data$out_specialty_current_MUMC.neurology_vascular_1 == 1) 
                                                                | 
                                                                (data$out_specialty_current_MUMC.neurology_1 == 1),
                                                                1, 
                                                                0)
data$out_specialty_current_MUMC.orthopedics_1 <- ifelse(data$out_specialty_current_MUMC == "orthopedics",
                                                        1,
                                                        0)
data$out_specialty_current_MUMC.rheumatology_gout_1 <- ifelse(data$out_specialty_current_MUMC == "rheumatology: gout",
                                                              1,
                                                              0)
data$out_specialty_current_MUMC.rheumatology_SPA_1 <- ifelse(data$out_specialty_current_MUMC == "rheumatology: SPA",
                                                             1,
                                                             0)
data$out_specialty_current_MUMC.rheumatology_1 <- ifelse(data$out_specialty_current_MUMC == "rheumatology",
                                                        1,
                                                        0)
data$out_specialty_current_MUMC.rheumatology_composite_1 <- ifelse((data$out_specialty_current_MUMC.rheumatology_gout_1 == 1)
                                                                   |
                                                                   (data$out_specialty_current_MUMC.rheumatology_SPA_1 == 1) 
                                                                   | 
                                                                   (data$out_specialty_current_MUMC.rheumatology_1 == 1),
                                                                   1,
                                                                   0)
data$out_specialty_current_MUMC.urology_functional_1 <- ifelse(data$out_specialty_current_MUMC == "urology: functional",
                                                               1,
                                                               0)
data$out_specialty_current_MUMC.urology_oncology_1 <- ifelse(data$out_specialty_current_MUMC == "urology: oncology",
                                                               1,
                                                               0)
data$out_specialty_current_MUMC.urology_composite_1 <- ifelse((data$out_specialty_current_MUMC.urology_functional_1 == 1) 
                                                               | 
                                                               (data$out_specialty_current_MUMC.urology_oncology_1 == 1),
                                                               1,
                                                               0)
data$out_specialty_current_MUMC.cardiovascular_composite_1 <- ifelse((data$out_specialty_current_MUMC.cardiology_1 == 1)
                                                                     |
                                                                     (data$out_specialty_current_MUMC.neurology_vascular_1 == 1)
                                                                     |
                                                                     (data$out_specialty_current_MUMC.internal_medicine_vascular_1 == 1),
                                                                     1,
                                                                     0)

#### inout_specialty_current_12m_MUMC.multiple_1
data$inout_specialty_current_12m_MUMC.multiple_1 <- ifelse((data$out_specialty_current_MUMC.cardiology_1 == 1) 
                                 &
                                 ((data$inout_specialty_12m_MUMC.urology_1 == 1) 
                                 |
                                 (data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.orthopedics_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.neurology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.dermatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.rheumatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)),
                                 1, 
                                 0)
data$inout_specialty_current_12m_MUMC.multiple_1 <- ifelse((data$out_specialty_current_MUMC.surgery_composite_1 == 1) 
                                 &
                                 ((data$inout_specialty_12m_MUMC.cardiology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.urology_1 == 1) 
                                 |
                                 (data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.orthopedics_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.neurology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.dermatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.rheumatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)),
                                 1, 
                                 data$inout_specialty_current_12m_MUMC.multiple_1)
data$inout_specialty_current_12m_MUMC.multiple_1 <- ifelse((data$out_specialty_current_MUMC.dermatology_1 == 1) 
                                 &
                                 ((data$inout_specialty_12m_MUMC.cardiology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.urology_1 == 1) 
                                 |
                                 (data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.orthopedics_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.neurology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.rheumatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)),
                                 1, 
                                 data$inout_specialty_current_12m_MUMC.multiple_1)
data$inout_specialty_current_12m_MUMC.multiple_1 <- ifelse((data$out_specialty_current_MUMC.internal_medicine_composite_1 == 1) 
                                 &
                                 ((data$inout_specialty_12m_MUMC.cardiology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.urology_1 == 1) 
                                 |
                                 (data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.orthopedics_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.neurology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.dermatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.rheumatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)),
                                 1, 
                                 data$inout_specialty_current_12m_MUMC.multiple_1)
data$inout_specialty_current_12m_MUMC.multiple_1 <- ifelse((data$out_specialty_current_MUMC.otorhinolaryngology_1 == 1) 
                                 &
                                 ((data$inout_specialty_12m_MUMC.cardiology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.urology_1 == 1) 
                                 |
                                 (data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.orthopedics_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.neurology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.dermatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.rheumatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)),
                                 1, 
                                 data$inout_specialty_current_12m_MUMC.multiple_1)
data$inout_specialty_current_12m_MUMC.multiple_1 <- ifelse((data$out_specialty_current_MUMC.gastroenterology_composite_1 == 1) 
                                 &
                                 ((data$inout_specialty_12m_MUMC.cardiology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.urology_1 == 1) 
                                 |
                                 (data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1)
                                 | 
                                 (data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.orthopedics_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.neurology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.dermatology_1 == 1)
                                 |                                 
                                 (data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.rheumatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)),
                                 1, 
                                 data$inout_specialty_current_12m_MUMC.multiple_1)
data$inout_specialty_current_12m_MUMC.multiple_1 <- ifelse((data$out_specialty_current_MUMC.neurology_composite_1 == 1) 
                                 &
                                 ((data$inout_specialty_12m_MUMC.cardiology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.urology_1 == 1) 
                                 |
                                 (data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1)
                                 | 
                                 (data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.orthopedics_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.dermatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.rheumatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)),
                                 1, 
                                 data$inout_specialty_current_12m_MUMC.multiple_1)
data$inout_specialty_current_12m_MUMC.multiple_1 <- ifelse((data$out_specialty_current_MUMC.orthopedics_1 == 1) 
                                 &
                                 ((data$inout_specialty_12m_MUMC.cardiology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.urology_1 == 1) 
                                 |
                                 (data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1)
                                 | 
                                 (data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.neurology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.dermatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.rheumatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)),
                                 1, 
                                 data$inout_specialty_current_12m_MUMC.multiple_1)
data$inout_specialty_current_12m_MUMC.multiple_1 <- ifelse((data$out_specialty_current_MUMC.rheumatology_composite_1 == 1) 
                                 &
                                 ((data$inout_specialty_12m_MUMC.cardiology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.urology_1 == 1) 
                                 |
                                 (data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1)
                                 | 
                                 (data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.orthopedics_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.neurology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.dermatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                 |                                 
                                 (data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)),
                                 1, 
                                 data$inout_specialty_current_12m_MUMC.multiple_1)
data$inout_specialty_current_12m_MUMC.multiple_1 <- ifelse((data$out_specialty_current_MUMC.urology_composite_1 == 1) 
                                 &
                                 ((data$inout_specialty_12m_MUMC.cardiology_1 == 1)
                                 |                                 
                                 (data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1)
                                 | 
                                 (data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.orthopedics_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.neurology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.dermatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.rheumatology_1 == 1)
                                 |
                                 (data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)),
                                 1, 
                                 data$inout_specialty_current_12m_MUMC.multiple_1)
table(data$inout_specialty_current_12m_MUMC.multiple_1, exclude = NULL)

### number of visits out patient clinic current specialty in past 12 or 36 months in MUMC+ 
names(data)[names(data) == "C3a"] <- "n_visits_out_current_specialty_12m_36m_MUMC.cat"
table(data$n_visits_out_current_specialty_12m_36m_MUMC.cat, exclude = NULL)
data$n_visits_out_current_specialty_12m_36m_MUMC.cat <- factor(data$n_visits_out_current_specialty_12m_36m_MUMC.cat,
                                                       levels = c("1",
                                                                  "2",
                                                                  "3"),
                                                       labels = c("1",
                                                                  "2",
                                                                  "3+"))

## PART 2 ----

### PART 2.A ----

#### housing
names(data)[names(data) == "A3b"] <- "housing"
data$housing <- ordered(data$housing, 
                        levels = c(1, 
                                   2, 
                                   3), 
                        labels = c("independent", 
                                   "home care", 
                                   "institution"))
table(data$housing, exclude = NULL)
data$housing.independent_1 <- ifelse(is.na(data$housing), 
                                     NA, 
                                     0)
data$housing.independent_1 <- ifelse(data$housing == "independent", 
                                     1, 
                                     data$housing.independent_1)
data$housing.home_care_1 <- ifelse(is.na(data$housing), 
                                   NA, 
                                   0)
data$housing.home_care_1 <- ifelse(data$housing == "home care", 
                                   1, 
                                   data$housing.home_care_1)
data$housing.institution_1 <- ifelse(is.na(data$housing), 
                                     NA, 
                                     0)
data$housing.institution_1 <- ifelse(data$housing == "institution", 
                                     1, 
                                     data$housing.institution_1)

#### patient-reported medication use 
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

#### pill box (week)
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

#### person responsible for medications
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

#### highest level of education attained
names(data)[names(data) == "A6b"] <- "education"
data$education <- ordered(data$education, 
                          levels = c(1, 
                                     2, 
                                     3, 
                                     4), 
                          labels = c("type 1",
                                     "type 2", 
                                     "type 3",
                                     "type 4"))
table(data$education, exclude = NULL)
data$education.type_1_1 <- ifelse(is.na(data$education), 
                                  NA, 
                                  0)
data$education.type_1_1 <- ifelse(data$education == "type 1", 
                                  1, 
                                  0)
data$education.type_2_1 <- ifelse(is.na(data$education), 
                                  NA, 
                                  0)
data$education.type_2_1 <- ifelse(data$education == "type 2", 
                                  1, 
                                  0)
data$education.type_3_1 <- ifelse(is.na(data$education), 
                                  NA, 
                                  0)
data$education.type_3_1 <- ifelse(data$education == "type 3", 
                                  1, 
                                  0)
data$education.type_4_1 <- ifelse(is.na(data$education), 
                                  NA, 
                                  0)
data$education.type_4_1 <- ifelse(data$education == "type 4", 
                                  1, 
                                  0)

#### allergy medication
names(data)[names(data) == "A13b"] <- "allergy_medication"
data$allergy_medication <- factor(data$allergy_medication, 
                                  levels = c(1, 
                                             2), 
                                  labels = c("yes", 
                                             "no"))
table(data$allergy_medication, exclude = NULL)
data$allergy_medication.yes_1 <- ifelse(data$allergy_medication == "yes", 
                                        1, 
                                        0) # Assumption NA equals to negative indication.

#### allergy medication symptoms
names(data)[names(data) == "A20b"] <- "allergy_medication_symptoms"
allergy_medication_symptoms.not_NA.description <- subset(data, !is.na(allergy_medication_symptoms))$allergy_medication_symptoms

#### visit external hospital in past 12 months
names(data)[names(data) == "A140b"] <- "visit_12m_external_hospital"
data$visit_12m_external_hospital <- factor(data$visit_12m_external_hospital, 
                                        levels = c(1,
                                                   2), 
                                        labels = c("yes", 
                                                   "no"))
table(data$visit_12m_external_hospital, exclude = NULL)
data$visit_12m_external_hospital.yes_1 <- ifelse(is.na(data$visit_12m_external_hospital), 
                                              NA, 
                                              0)
data$visit_12m_external_hospital.yes_1 <- ifelse(data$visit_12m_external_hospital == "yes", 
                                              1, 
                                              0)

#### visited as outpatient and/or inpatient specialty in past 12 months in external hospital
inout_specialty_12m_external_hospital.list <- strsplit(data$A150b, ",")
data$inout_specialty_12m_external_hospital.cardiology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "1" %in% x }),
                                                               1, 
                                                               0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.cardiology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.urology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "2" %in% x }),
                                                            1, 
                                                            0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.urology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.psychiatry_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "3" %in% x }),
                                                               1, 
                                                               0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.psychiatry_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.otorhinolaryngology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "4" %in% x }),
                                                                        1, 
                                                                        0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.otorhinolaryngology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.ophthalmology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "5" %in% x }), 
                                                                         1, 
                                                                         0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.ophthalmology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.internal_medicine_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "6" %in% x }), 
                                                                             1, 
                                                                             0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.internal_medicine_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.surgery_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "7" %in% x }),
                                                            1, 
                                                            0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.surgery_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.orthopedics_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "8" %in% x }), 
                                                                1, 
                                                                0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.orthopedics_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.plastic_surgery_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "9" %in% x }),
                                                                    1, 
                                                                    0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.plastic_surgery_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.obstetrics_and_gynaecology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "10" %in% x }),
                                                                               1, 
                                                                               0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.obstetrics_and_gynaecology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.neurology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "11" %in% x }),
                                                              1, 
                                                              0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.neurology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.dermatology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "12" %in% x }),
                                                                1, 
                                                                0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.dermatology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.gastroenterology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "13" %in% x }),
                                                                     1, 
                                                                     0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.gastroenterology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.pneumology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "14" %in% x }), 
                                                               1, 
                                                               0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.pneumology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.rheumatology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "15" %in% x }),
                                                                 1, 
                                                                 0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.rheumatology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.do_not_select_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "16" %in% x }), 
                                                                  1, 
                                                                  0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.do_not_select_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.pediatrics_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "17" %in% x }), 
                                                               1, 
                                                               0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.pediatrics_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.anesthesiology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "18" %in% x }),
                                                                   1, 
                                                                   0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.anesthesiology_1, exclude = NULL)
data$inout_specialty_12m_external_hospital.other_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "19" %in% x }),
                                                          1, 
                                                          0) # Assumption NA equals to negative indication.
table(data$inout_specialty_12m_external_hospital.other_1, exclude = NULL)

#### hospitalisation in past 12 months in external hospital
names(data)[names(data) == "A16b"] <- "in_all_12m_external_hospital"
data$in_all_12m_external_hospital <- factor(data$in_all_12m_external_hospital, 
                                         levels = c(1, 
                                                    2), 
                                         labels = c("yes", 
                                                    "no"))
table(data$in_all_12m_external_hospital, exclude = NULL)
data$in_all_12m_external_hospital.yes_1 <- ifelse(is.na(data$in_all_12m_external_hospital), 
                                               NA, 
                                               0) 
data$in_all_12m_external_hospital.yes_1 <- ifelse(data$in_all_12m_external_hospital == "yes", 
                                               1, 
                                               data$in_all_12m_external_hospital.yes_1) 

#### number of ER visists in past 12 months in external hospital
names(data)[names(data) == "A18b"] <- "n_ER_12m_external_hospital"
table(data$n_ER_12m_external_hospital, exclude = NULL)

data$n_ER_12m_external_hospital.centered <- data$n_ER_12m_external_hospital - mean(data$n_ER_12m_external_hospital)

data$ER_12m_external_hospital.yes_1 <- ifelse(data$n_ER_12m_external_hospital == 0, 
                                           0, 
                                           1)

### PART 2.B ----

#### medication without prescription
names(data)[names(data) == "B2b"] <- "medication_wo_prescription"
medication_wo_prescription.list <- strsplit(data$medication_wo_prescription, ",")
data$medication_wo_prescription.NSAIDs_1 <- ifelse(is.na(medication_wo_prescription.list), 
                                                        NA, 
                                                        ifelse(lapply(medication_wo_prescription.list, function(x) { "1" %in% x }), 
                                                               1, 
                                                               0))
data$medication_wo_prescription.proton_pump_inhibitors_1 <- ifelse(is.na(medication_wo_prescription.list), 
                                                                        NA, 
                                                                        ifelse(lapply(medication_wo_prescription.list, function(x) { "2" %in% x }), 
                                                                               1, 
                                                                               0))
data$medication_wo_prescription.hypericum_1 <- ifelse(is.na(medication_wo_prescription.list), 
                                                           NA, 
                                                           ifelse(lapply(medication_wo_prescription.list, function(x) { "3" %in% x }), 
                                                                  1, 
                                                                  0))
data$medication_wo_prescription.red_yeast_rice_1 <- ifelse(is.na(medication_wo_prescription.list), 
                                                                NA, 
                                                                ifelse(lapply(medication_wo_prescription.list, function(x) { "4" %in% x }), 
                                                                       1, 
                                                                       0))
data$medication_wo_prescription.multi_vitamins_dietary_supplement_1 <- ifelse(is.na(medication_wo_prescription.list), 
                                                                                   NA, 
                                                                                   ifelse(lapply(medication_wo_prescription.list, function(x) { "5" %in% x }), 
                                                                                          1, 
                                                                                          0))
data$medication_wo_prescription.other_1 <- ifelse(is.na(medication_wo_prescription.list), 
                                                       NA, 
                                                       ifelse(lapply(medication_wo_prescription.list, function(x) { "6" %in% x }), 
                                                              1, 
                                                              0))
data$medication_wo_prescription.none_1 <- ifelse(is.na(medication_wo_prescription.list), 
                                                      NA, 
                                                      ifelse(lapply(medication_wo_prescription.list, function(x) { "7" %in% x }), 
                                                             1, 
                                                             0))

table(data$medication_wo_prescription.NSAIDs_1, data$high_risk_medications.NSAIDs_1, exclude = NULL)
data$NSAIDs_composite_1 <- ifelse((data$medication_wo_prescription.NSAIDs_1 == 1) 
                                  |
                                  (data$high_risk_medications.NSAIDs_1 == 1),
                                  1, 
                                  0)

## PART 3 ----

#### medication literacy
names(data)[names(data) == "A7c"] <- "medication_literacy"
data$medication_literacy <- factor(data$medication_literacy, 
                                   levels = c(1, 
                                              2, 
                                              3), 
                                   labels = c("adequate", 
                                              "suboptimal", 
                                              "insufficient"))
table(data$medication_literacy, exclude = NULL)
data$medication_literacy.adequate_1 <- ifelse( (data$medication_literacy == "adequate") 
                                               |
                                               is.na(data$medication_literacy), 
                                               1, 
                                               0) # Assumption NA equals to adequate.
data$medication_literacy.suboptimal_1 <- ifelse(data$medication_literacy == "suboptimal", 
                                                1, 
                                                0)
data$medication_literacy.insufficient_1 <- ifelse(data$medication_literacy == "insufficient", 
                                                  1, 
                                                  0)

#### medication prescribed during consultation: yes/no
names(data)[names(data) == "B10c"] <- "medication_prescribed_during_consult.yes_no"
table(data$medication_prescribed_during_consult.yes_no, exclude = NULL) # View(t(subset(data, is.na(data$medication_prescribed_during_consult.yes_no)))) # New medication was prescribed (see variable below), missing value equals yes.
data$medication_prescribed_during_consult.yes_no <- ifelse(is.na(data$medication_prescribed_during_consult.yes_no),
                                                           1,
                                                           data$medication_prescribed_during_consult.yes_no)
data$medication_prescribed_during_consult.yes_no <- factor(data$medication_prescribed_during_consult.yes_no, 
                                                           levels = c(1, 
                                                                      2), 
                                                           labels = c("yes", 
                                                                      "no"))
data$medication_prescribed_during_consult.yes_1 <- ifelse(data$medication_prescribed_during_consult.yes_no == "yes", 
                                                          1, 
                                                          0)

#### medication prescribed during consultation: name
names(data)[names(data) == "B11c"] <- "medication_prescribed_during_consult.name"
medication_prescribed_during_consult.not_NA.name <- subset(data, !is.na(medication_prescribed_during_consult.name))$medication_prescribed_during_consult.name

#### medication prescribed during consultation: new
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

#### medication stopped during consult: yes/no
names(data)[names(data) == "B20c"] <- "medication_stopped_during_consult.yes_no"
table(data$medication_stopped_during_consult.yes_no, exclude = NULL)
data$medication_stopped_during_consult.yes_no <- factor(data$medication_stopped_during_consult.yes_no, 
                                                        levels = c(1, 
                                                                   2), 
                                                        labels = c("yes", 
                                                                   "no"))
data$medication_stopped_during_consult.yes_1 <- ifelse(is.na(data$medication_stopped_during_consult.yes_no), 
                                                       NA, 
                                                       0)
data$medication_stopped_during_consult.yes_1 <- ifelse(data$medication_stopped_during_consult.yes_no == "yes", 
                                                       1, 
                                                       data$medication_stopped_during_consult.yes_1)

#### medication stopped during consult: name
names(data)[names(data) == "B21c"] <- "medication_stopped_during_consult.name"
medication_stopped_during_consult.not_NA.name <- subset(data, !is.na(medication_stopped_during_consult.name))$medication_stopped_during_consult.name

#### non-medication intervention during consult 
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

#### warnings
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

#### medications interaction
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

#### medications interaction G-standaard number
id.interaction_number <- as.data.frame(cbind(data$id, 
                                             data$S31c, 
                                             data$S32c, 
                                             data$S33c)) 
colnames(id.interaction_number) <- c( "id",
                                      "interaction_number.1",
                                      "interaction_number.2",
                                      "interaction_number.3")

#### medications interaction WFG
## View(cbind(data$S41c, data$S42c, data$S43c))

#### medications revision: pharmacy assistant 
id.medication_revision.pharmacy_assistant <- as.data.frame(cbind(data$id, 
                                                                 data$S51c, 
                                                                 data$S52c, 
                                                                 data$S53c)) 
colnames(id.medication_revision.pharmacy_assistant) <- c("id", 
                                                         "revision.1", 
                                                         "revision.2", 
                                                         "revision.3")
data$medication_revision.pharmacy_assistant.yes_1 <- ifelse((id.medication_revision.pharmacy_assistant$revision.1 == 1) 
                                                            | 
                                                            (id.medication_revision.pharmacy_assistant$revision.2 == 1) 
                                                            | 
                                                            (id.medication_revision.pharmacy_assistant$revision.3 == 1),
                                                            1,
                                                            0)
table(data$medication_revision.pharmacy_assistant.yes_1, exclude = NULL)                                                            

#### medications revision: physician
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

#### prevented consequences
names(data)[names(data) == "C5c"] <- "prevented_consequences"
data$prevented_consequences <- factor(data$prevented_consequences, 
                                      levels = c(1, 
                                                 2, 
                                                 3,
                                                 4,
                                                 5,
                                                 6,
                                                 7,
                                                 8,
                                                 9),
                                      labels = c("A",
                                                 "B",
                                                 "C",
                                                 "D",
                                                 "E",
                                                 "F",
                                                 "G",
                                                 "H",
                                                 "I"))
table(data$prevented_consequences, exclude = NULL)
data$prevented_consequences.A_1 <- ifelse(is.na(data$prevented_consequences), 
                                          NA, 
                                          0)
data$prevented_consequences.A_1 <- ifelse(data$prevented_consequences == "A", 
                                          1, 
                                          data$prevented_consequences.A_1)
data$prevented_consequences.B_1 <- ifelse(is.na(data$prevented_consequences), 
                                          NA, 
                                          0)
data$prevented_consequences.B_1 <- ifelse(data$prevented_consequences == "B", 
                                          1, 
                                          data$prevented_consequences.B_1)
data$prevented_consequences.C_1 <- ifelse(is.na(data$prevented_consequences), 
                                          NA, 
                                          0)
data$prevented_consequences.C_1 <- ifelse(data$prevented_consequences == "C", 
                                          1, 
                                          data$prevented_consequences.C_1)
data$prevented_consequences.D_1 <- ifelse(is.na(data$prevented_consequences), 
                                          NA, 
                                          0)
data$prevented_consequences.D_1 <- ifelse(data$prevented_consequences == "D", 
                                          1, 
                                          data$prevented_consequences.D_1)
data$prevented_consequences.E_1 <- ifelse(is.na(data$prevented_consequences), 
                                          NA, 
                                          0)
data$prevented_consequences.E_1 <- ifelse(data$prevented_consequences == "E", 
                                          1, 
                                          data$prevented_consequences.E_1)
data$prevented_consequences.F_1 <- ifelse(is.na(data$prevented_consequences), 
                                          NA, 
                                          0)
data$prevented_consequences.F_1 <- ifelse(data$prevented_consequences == "F", 
                                          1, 
                                          data$prevented_consequences.F_1)
data$prevented_consequences.G_1 <- ifelse(is.na(data$prevented_consequences), 
                                          NA, 
                                          0)
data$prevented_consequences.G_1 <- ifelse(data$prevented_consequences == "G", 
                                          1, 
                                          data$prevented_consequences.G_1)
data$prevented_consequences.H_1 <- ifelse(is.na(data$prevented_consequences), 
                                          NA, 
                                          0)
data$prevented_consequences.H_1 <- ifelse(data$prevented_consequences == "H", 
                                          1, 
                                          data$prevented_consequences.H_1)
data$prevented_consequences.I_1 <- ifelse(is.na(data$prevented_consequences), 
                                          NA, 
                                          0)
data$prevented_consequences.I_1 <- ifelse(data$prevented_consequences == "I", 
                                          1, 
                                          data$prevented_consequences.I_1)

#### information gained
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

# DEFINITION OF AN EVENT ----

table(data$medications_interaction, data$warnings, exclude = NULL) 
table(data$medications_interaction, data$warnings, data$medications_revision.physician, exclude = NULL)
table(data$medications_interaction, data$medications_revision.physician, exclude = NULL)

data$y.interaction <- ifelse(data$medications_interaction.yes_1 == 1,
                             1,
                             0) 
data$y.interaction <- ifelse(is.na(data$y.interaction),
                             0,
                             data$y.interaction) # Assumption NA equals to no medications interaction (warnings NA).
table(data$y.interaction, exclude = NULL)

data$y.revision_wo_interaction <- ifelse((data$y.interaction == 0) 
                                         &
                                         (data$medications_revision.physician.yes_1 == 1),
                                         1,
                                         0) # Assumption NA equals to no revision without medications interaction.
table(data$y.revision_wo_interaction, exclude = NULL)

data$y <- ifelse((data$medications_interaction.yes_1 == 1) 
                 | 
                 (data$medications_revision.physician.yes_1 == 1), 
                 1, 
                 0)
data$y <- ifelse(is.na(data$y), 
                 0, 
                 data$y) # Assumption NA equals to no event. 
table(data$y, exclude = NULL)

# FINAL DATA FRAME ----

data <- subset(data, select = -c(SPEC,
                                 A13a,
                                 A20a,
                                 A15a,
                                 A150a,
                                 C1a,
                                 C10a,
                                 C11a,
                                 C12a,
                                 C13a,
                                 C4a,
                                 C5a,
                                 C6a,
                                 B7a,
                                 B8a,
                                 A9b,
                                 A10b,
                                 A14b,
                                 A150b,
                                 A15b,
                                 B9b,
                                 C1b,
                                 C3b,
                                 C10b,
                                 C11b,
                                 C12b,
                                 C5b,
                                 C6b,
                                 C9b,
                                 C200c,
                                 C2c,
                                 C14c,
                                 C15c,
                                 C7c,
                                 ...66,
                                 S31c,
                                 S41c,
                                 S51c,
                                 S32c,
                                 S42c,
                                 S52c,
                                 S33c,
                                 S43c,
                                 S53c,
                                 S34c,
                                 S44c,
                                 S54c,
                                 S35c,
                                 S45c,
                                 S55c,
                                 S36c,
                                 S46c,
                                 S56c,
                                 S9c,
                                 S10c))

rm(list = setdiff(ls(), "data"))
