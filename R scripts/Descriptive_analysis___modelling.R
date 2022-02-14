# PREPARATION ----

## DATA ----

source("C:\\Users\\P70070766\\Documents\\Medication reconciliation\\Scripts\\20220211 - Preprocessing.R")

## PACKAGE & FUNCTION ----

library(rms)
source("C:\\Users\\P70070766\\Documents\\Medication reconciliation\\Scripts\\20220212 - Function - Confusion matrix based statistics with optimism correction.R")

## SEED ----

#set.seed()

# VARIABLES ----

## n_prescribed_medications ----

summary(data$n_prescribed_medications)
sd(data$n_prescribed_medications)
ggplot(data, aes(x = n_prescribed_medications)) +
  geom_bar(colour = "black") +
  scale_x_continuous(name = "number of prescribed medications",
                     breaks = seq(0, 20, 1),
                     labels = seq(0,20, 1)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

## high_risk_medications.yes_1 ----

table(data$high_risk_medications.yes_1, exclude = NULL) 

## inout_specialty_current_12m_MUMC.multiple_1 ----

table(data$inout_specialty_current_12m_MUMC.multiple_1, exclude = NULL) 

## eGFR.below_60_1 ----

table(data$eGFR)
table(data$eGFR.below_60_1, exclude = NULL) 

## out_specialty_current_MUMC ----

table(data$out_specialty_current_MUMC.cardiovascular_composite_1, exclude = NULL)
table(data$out_specialty_current_MUMC.surgery_composite_1, exclude = NULL)

## age ----

summary(data$age)
sd(data$age)
ggplot(data, aes(x = age)) +
  geom_bar(colour = "black") +
  scale_x_binned(breaks = seq(0, 100, 5),
                 labels = seq(0, 100, 5)) +
  theme_minimal()

## medication_literacy.adequate_1 ----

table(data$medication_literacy, exclude = NULL)
prop.table(table(data$medication_literacy, exclude = NULL))

table(data$medication_literacy.adequate_1, exclude = NULL) 

### housing.independent_1 ----

table(data$person_responsible_for_medications, exclude = NULL)
prop.table(table(data$person_responsible_for_medications, exclude = NULL))

table(data$housing, exclude = NULL)
prop.table(table(data$housing, exclude = NULL))

table(data$person_responsible_for_medications, data$housing, exclude = NULL) # Check with Veronique

table(data$housing.independent_1, exclude = NULL) 

# MODELLING ----

## Create an object to capture statistics ----

m_names <- c(paste0("m", 1:33)) 
statistics <- as.data.frame(matrix(ncol = 58, nrow = 33))
colnames(statistics) <- c("n_df",
                          "RsqN.app",
                          "RsqN.optimism_corrected",
                          "RsqN.optimism",
                          "Brier.app",
                          "Brier.optimism_corrected",
                          "C.app",
                          "C.optimism_corrected",
                          "intercept.dev",
                          "intercept.boot",
                          "slope.dev",
                          "slope.boot",
                          "accuracy.app.threshold_0.03",
                          "accuracy.optimism_corrected.threshold_0.03",
                          "true_positive_rate.app.threshold_0.03",
                          "true_positive_rate.optimism_corrected.threshold_0.03",
                          "true_negative_rate.app.threshold_0.03",
                          "true_negative_rate.optimism_corrected.threshold_0.03",
                          "positive_predictive_value.app.threshold_0.03",
                          "positive_predictive_value.optimism_corrected.threshold_0.03",
                          "negative_predictive_value.app.threshold_0.03",
                          "negative_predictive_value.optimism_corrected.threshold_0.03",
                          "accuracy.optimism_corrected.threshold_0.04",
                          "true_positive_rate.app.threshold_0.04",
                          "true_positive_rate.optimism_corrected.threshold_0.04",
                          "true_negative_rate.app.threshold_0.04",
                          "true_negative_rate.optimism_corrected.threshold_0.04",
                          "positive_predictive_value.app.threshold_0.04",
                          "positive_predictive_value.optimism_corrected.threshold_0.04",
                          "negative_predictive_value.app.threshold_0.04",
                          "negative_predictive_value.optimism_corrected.threshold_0.04",
                          "accuracy.optimism_corrected.threshold_0.05",
                          "true_positive_rate.app.threshold_0.05",
                          "true_positive_rate.optimism_corrected.threshold_0.05",
                          "true_negative_rate.app.threshold_0.05",
                          "true_negative_rate.optimism_corrected.threshold_0.05",
                          "positive_predictive_value.app.threshold_0.05",
                          "positive_predictive_value.optimism_corrected.threshold_0.05",
                          "negative_predictive_value.app.threshold_0.05",
                          "negative_predictive_value.optimism_corrected.threshold_0.05",
                          "accuracy.optimism_corrected.threshold_0.06",
                          "true_positive_rate.app.threshold_0.06",
                          "true_positive_rate.optimism_corrected.threshold_0.06",
                          "true_negative_rate.app.threshold_0.06",
                          "true_negative_rate.optimism_corrected.threshold_0.06",
                          "positive_predictive_value.app.threshold_0.06",
                          "positive_predictive_value.optimism_corrected.threshold_0.06",
                          "negative_predictive_value.app.threshold_0.06",
                          "negative_predictive_value.optimism_corrected.threshold_0.06",
                          "accuracy.optimism_corrected.threshold_0.07",
                          "true_positive_rate.app.threshold_0.07",
                          "true_positive_rate.optimism_corrected.threshold_0.07",
                          "true_negative_rate.app.threshold_0.07",
                          "true_negative_rate.optimism_corrected.threshold_0.07",
                          "positive_predictive_value.app.threshold_0.07",
                          "positive_predictive_value.optimism_corrected.threshold_0.07",
                          "negative_predictive_value.app.threshold_0.07",
                          "negative_predictive_value.optimism_corrected.threshold_0.07")
rownames(statistics) <- m_names

## MODELS ----

### m1 ----

#### Fit ----

fit.m1 <- lrm(y ~ 
                n_prescribed_medications +
                high_risk_medications.yes_1,
              data = data,
              x = TRUE,
              y = TRUE)
fit.m1
anova(fit.m1)
plot(anova(fit.m1))

##### Write statistics to object ----

statistics["m1", "n_df"] <- 2
statistics["m1", "RsqN.app"] <- fit.m1[["stats"]][["R2"]]
statistics["m1", "Brier.app"] <- fit.m1[["stats"]][["Brier"]]
statistics["m1", "C.app"] <- fit.m1[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m1 <- validate(fit.m1,
                        method = "boot",
                        B = 1000,
                        pr = TRUE)

boot.fit.m1.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m1", "RsqN.optimism_corrected"] <- boot.fit.m1[2, 5]
statistics["m1", "RsqN.optimism"] <- boot.fit.m1[2, 4]
statistics["m1", "Brier.optimism_corrected"] <- boot.fit.m1[9, 5]
statistics["m1", "C.optimism_corrected"] <-  (boot.fit.m1[1, 5] + 1)/2
statistics["m1", "intercept.dev"] <- boot.fit.m1[3, 1]
statistics["m1", "intercept.boot"] <- boot.fit.m1[3, 5]
statistics["m1", "slope.dev"] <- boot.fit.m1[4, 1]
statistics["m1", "slope.boot"] <- boot.fit.m1[4, 5]
statistics["m1", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m1", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m1", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m1", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m1", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m1", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m1", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m1", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m1", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m1", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m1", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m1", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m1", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m1", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m1", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m1", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m1", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m1", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m1", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m1", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m1", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m1", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m1", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m1", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m1", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m1", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m1", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m1", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m1", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m1", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m1", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m1", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m1", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m1", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m1", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m1", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m1", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m1", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m1", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m1", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m1", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m1", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m1", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m1", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m1", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m1", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m1", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m1", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m1", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m1", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m2 ----

#### Fit ----

fit.m2 <- lrm(y ~ 
                rcs(n_prescribed_medications, 3) +
                high_risk_medications.yes_1,
              data = data,
              x = TRUE,
              y = TRUE)
fit.m2
anova(fit.m2)
plot(anova(fit.m2))

##### Write statistics to object ----

statistics["m2", "n_df"] <- 3
statistics["m2", "RsqN.app"] <- fit.m2[["stats"]][["R2"]]
statistics["m2", "Brier.app"] <- fit.m2[["stats"]][["Brier"]]
statistics["m2", "C.app"] <- fit.m2[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m2 <- validate(fit.m2,
                        method = "boot",
                        B = 1000,
                        pr = TRUE)

boot.fit.m2.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m2", "RsqN.optimism_corrected"] <- boot.fit.m2[2, 5]
statistics["m2", "RsqN.optimism"] <- boot.fit.m2[2, 4]
statistics["m2", "Brier.optimism_corrected"] <- boot.fit.m2[9, 5]
statistics["m2", "C.optimism_corrected"] <-  (boot.fit.m2[1, 5] + 1)/2
statistics["m2", "intercept.dev"] <- boot.fit.m2[3, 1]
statistics["m2", "intercept.boot"] <- boot.fit.m2[3, 5]
statistics["m2", "slope.dev"] <- boot.fit.m2[4, 1]
statistics["m2", "slope.boot"] <- boot.fit.m2[4, 5]
statistics["m2", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m2", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m2", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m2", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m2", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m2", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m2", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m2", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m2", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m2", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m2", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m2", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m2", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m2", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m2", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m2", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m2", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m2", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m2", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m2", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m2", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m2", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m2", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m2", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m2", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m2", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m2", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m2", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m2", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m2", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m2", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m2", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m2", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m2", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m2", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m2", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m2", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m2", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m2", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m2", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m2", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m2", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m2", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m2", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m2", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m2", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m2", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m2", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m2", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m2", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m2.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m3 ----

#### Fit ----

fit.m3 <- lrm(y ~ 
                rcs(n_prescribed_medications, 4) +
                high_risk_medications.yes_1,
              data = data,
              x = TRUE,
              y = TRUE)
fit.m3
anova(fit.m3)
plot(anova(fit.m3))

##### Write statistics to object ----

statistics["m3", "n_df"] <- 4
statistics["m3", "RsqN.app"] <- fit.m3[["stats"]][["R2"]]
statistics["m3", "Brier.app"] <- fit.m3[["stats"]][["Brier"]]
statistics["m3", "C.app"] <- fit.m3[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m3 <- validate(fit.m3,
                        method = "boot",
                        B = 1000,
                        pr = TRUE)

boot.fit.m3.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m3", "RsqN.optimism_corrected"] <- boot.fit.m3[2, 5]
statistics["m3", "RsqN.optimism"] <- boot.fit.m3[2, 4]
statistics["m3", "Brier.optimism_corrected"] <- boot.fit.m3[9, 5]
statistics["m3", "C.optimism_corrected"] <-  (boot.fit.m3[1, 5] + 1)/2
statistics["m3", "intercept.dev"] <- boot.fit.m3[3, 1]
statistics["m3", "intercept.boot"] <- boot.fit.m3[3, 5]
statistics["m3", "slope.dev"] <- boot.fit.m3[4, 1]
statistics["m3", "slope.boot"] <- boot.fit.m3[4, 5]
statistics["m3", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m3", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m3", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m3", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m3", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m3", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m3", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m3", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m3", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m3", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m3", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m3", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m3", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m3", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m3", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m3", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m3", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m3", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m3", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m3", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m3", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m3", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m3", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m3", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m3", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m3", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m3", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m3", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m3", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m3", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m3", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m3", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m3", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m3", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m3", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m3", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m3", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m3", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m3", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m3", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m3", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m3", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m3", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m3", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m3", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m3", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m3", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m3", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m3", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m3", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m3.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m4 ----

#### Fit ----

fit.m4 <- lrm(y ~ 
                n_prescribed_medications +
                high_risk_medications.yes_1 +
                eGFR.below_60_1,
              data = data,
              x = TRUE,
              y = TRUE)
fit.m4
anova(fit.m4)
plot(anova(fit.m4))

##### Write statistics to object ----

statistics["m4", "n_df"] <- 3
statistics["m4", "RsqN.app"] <- fit.m4[["stats"]][["R2"]]
statistics["m4", "Brier.app"] <- fit.m4[["stats"]][["Brier"]]
statistics["m4", "C.app"] <- fit.m4[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m4 <- validate(fit.m4,
                        method = "boot",
                        B = 1000,
                        pr = TRUE)

boot.fit.m4.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m4", "RsqN.optimism_corrected"] <- boot.fit.m4[2, 5]
statistics["m4", "RsqN.optimism"] <- boot.fit.m4[2, 4]
statistics["m4", "Brier.optimism_corrected"] <- boot.fit.m4[9, 5]
statistics["m4", "C.optimism_corrected"] <-  (boot.fit.m4[1, 5] + 1)/2
statistics["m4", "intercept.dev"] <- boot.fit.m4[3, 1]
statistics["m4", "intercept.boot"] <- boot.fit.m4[3, 5]
statistics["m4", "slope.dev"] <- boot.fit.m4[4, 1]
statistics["m4", "slope.boot"] <- boot.fit.m4[4, 5]
statistics["m4", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m4", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m4", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m4", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m4", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m4", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m4", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m4", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m4", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m4", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m4", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m4", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m4", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m4", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m4", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m4", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m4", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m4", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m4", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m4", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m4", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m4", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m4", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m4", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m4", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m4", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m4", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m4", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m4", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m4", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m4", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m4", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m4", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m4", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m4", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m4", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m4", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m4", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m4", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m4", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m4", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m4", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m4", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m4", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m4", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m4", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m4", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m4", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m4", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m4", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m4.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m5 ----

#### Fit ----

fit.m5 <- lrm(y ~ 
                rcs(n_prescribed_medications, 3) +
                high_risk_medications.yes_1 +
                eGFR.below_60_1,
              data = data,
              x = TRUE,
              y = TRUE)
fit.m5
anova(fit.m5)
plot(anova(fit.m5))

##### Write statistics to object ----

statistics["m5", "n_df"] <- 4
statistics["m5", "RsqN.app"] <- fit.m5[["stats"]][["R2"]]
statistics["m5", "Brier.app"] <- fit.m5[["stats"]][["Brier"]]
statistics["m5", "C.app"] <- fit.m5[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m5 <- validate(fit.m5,
                        method = "boot",
                        B = 1000,
                        pr = TRUE)

boot.fit.m5.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m5", "RsqN.optimism_corrected"] <- boot.fit.m5[2, 5]
statistics["m5", "RsqN.optimism"] <- boot.fit.m5[2, 4]
statistics["m5", "Brier.optimism_corrected"] <- boot.fit.m5[9, 5]
statistics["m5", "C.optimism_corrected"] <-  (boot.fit.m5[1, 5] + 1)/2
statistics["m5", "intercept.dev"] <- boot.fit.m5[3, 1]
statistics["m5", "intercept.boot"] <- boot.fit.m5[3, 5]
statistics["m5", "slope.dev"] <- boot.fit.m5[4, 1]
statistics["m5", "slope.boot"] <- boot.fit.m5[4, 5]
statistics["m5", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m5", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m5", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m5", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m5", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m5", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m5", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m5", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m5", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m5", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m5", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m5", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m5", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m5", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m5", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m5", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m5", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m5", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m5", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m5", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m5", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m5", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m5", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m5", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m5", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m5", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m5", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m5", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m5", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m5", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m5", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m5", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m5", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m5", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m5", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m5", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m5", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m5", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m5", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m5", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m5", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m5", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m5", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m5", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m5", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m5", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m5", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m5", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m5", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m5", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m5.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m6 ----

#### Fit ----

fit.m6 <- lrm(y ~ 
                rcs(n_prescribed_medications, 4) +
                high_risk_medications.yes_1 +
                eGFR.below_60_1,
              data = data,
              x = TRUE,
              y = TRUE)
fit.m6
anova(fit.m6)
plot(anova(fit.m6))

##### Write statistics to object ----

statistics["m6", "n_df"] <- 5
statistics["m6", "RsqN.app"] <- fit.m6[["stats"]][["R2"]]
statistics["m6", "Brier.app"] <- fit.m6[["stats"]][["Brier"]]
statistics["m6", "C.app"] <- fit.m6[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m6 <- validate(fit.m6,
                        method = "boot",
                        B = 1000,
                        pr = TRUE)

boot.fit.m6.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

#### Write statistics to object ----

statistics["m6", "RsqN.optimism_corrected"] <- boot.fit.m6[2, 5]
statistics["m6", "RsqN.optimism"] <- boot.fit.m6[2, 4]
statistics["m6", "Brier.optimism_corrected"] <- boot.fit.m6[9, 5]
statistics["m6", "C.optimism_corrected"] <-  (boot.fit.m6[1, 5] + 1)/2
statistics["m6", "intercept.dev"] <- boot.fit.m6[3, 1]
statistics["m6", "intercept.boot"] <- boot.fit.m6[3, 5]
statistics["m6", "slope.dev"] <- boot.fit.m6[4, 1]
statistics["m6", "slope.boot"] <- boot.fit.m6[4, 5]
statistics["m6", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m6", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m6", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m6", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m6", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m6", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m6", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m6", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m6", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m6", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m6", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m6", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m6", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m6", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m6", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m6", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m6", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m6", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m6", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m6", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m6", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m6", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m6", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m6", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m6", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m6", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m6", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m6", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m6", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m6", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m6", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m6", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m6", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m6", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m6", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m6", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m6", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m6", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m6", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m6", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m6", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m6", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m6", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m6", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m6", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m6", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m6", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m6", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m6", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m6", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m6.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m7 ----

#### Fit ----

fit.m7 <- lrm(y ~ 
                n_prescribed_medications +
                high_risk_medications.yes_1 +
                age,
              data = data,
              x = TRUE,
              y = TRUE)
fit.m7
anova(fit.m7)
plot(anova(fit.m7))

##### Write statistics to object ----

statistics["m7", "n_df"] <- 3
statistics["m7", "RsqN.app"] <- fit.m7[["stats"]][["R2"]]
statistics["m7", "Brier.app"] <- fit.m7[["stats"]][["Brier"]]
statistics["m7", "C.app"] <- fit.m7[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m7 <- validate(fit.m7,
                        method = "boot",
                        B = 1000,
                        pr = TRUE)

boot.fit.m7.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                age",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m7", "RsqN.optimism_corrected"] <- boot.fit.m7[2, 5]
statistics["m7", "RsqN.optimism"] <- boot.fit.m7[2, 4]
statistics["m7", "Brier.optimism_corrected"] <- boot.fit.m7[9, 5]
statistics["m7", "C.optimism_corrected"] <-  (boot.fit.m7[1, 5] + 1)/2
statistics["m7", "intercept.dev"] <- boot.fit.m7[3, 1]
statistics["m7", "intercept.boot"] <- boot.fit.m7[3, 5]
statistics["m7", "slope.dev"] <- boot.fit.m7[4, 1]
statistics["m7", "slope.boot"] <- boot.fit.m7[4, 5]
statistics["m7", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m7", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m7", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m7", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m7", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m7", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m7", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m7", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m7", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m7", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m7", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m7", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m7", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m7", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m7", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m7", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m7", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m7", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m7", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m7", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m7", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m7", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m7", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m7", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m7", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m7", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m7", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m7", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m7", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m7", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m7", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m7", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m7", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m7", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m7", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m7", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m7", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m7", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m7", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m7", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m7", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m7", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m7", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m7", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m7", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m7", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m7", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m7", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m7", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m7", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m7.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m8 ----

#### Fit ----

fit.m8 <- lrm(y ~ 
                rcs(n_prescribed_medications, 3) +
                high_risk_medications.yes_1 +
                age,
              data = data,
              x = TRUE,
              y = TRUE)
fit.m8
anova(fit.m8)
plot(anova(fit.m8))

##### Write statistics to object ----

statistics["m8", "n_df"] <- 4
statistics["m8", "RsqN.app"] <- fit.m8[["stats"]][["R2"]]
statistics["m8", "Brier.app"] <- fit.m8[["stats"]][["Brier"]]
statistics["m8", "C.app"] <- fit.m8[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m8 <- validate(fit.m8,
                        method = "boot",
                        B = 1000,
                        pr = TRUE)

boot.fit.m8.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1 +
                                                age",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m8", "RsqN.optimism_corrected"] <- boot.fit.m8[2, 5]
statistics["m8", "RsqN.optimism"] <- boot.fit.m8[2, 4]
statistics["m8", "Brier.optimism_corrected"] <- boot.fit.m8[9, 5]
statistics["m8", "C.optimism_corrected"] <-  (boot.fit.m8[1, 5] + 1)/2
statistics["m8", "intercept.dev"] <- boot.fit.m8[3, 1]
statistics["m8", "intercept.boot"] <- boot.fit.m8[3, 5]
statistics["m8", "slope.dev"] <- boot.fit.m8[4, 1]
statistics["m8", "slope.boot"] <- boot.fit.m8[4, 5]
statistics["m8", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m8", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m8", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m8", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m8", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m8", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m8", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m8", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m8", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m8", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m8", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m8", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m8", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m8", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m8", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m8", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m8", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m8", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m8", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m8", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m8", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m8", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m8", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m8", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m8", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m8", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m8", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m8", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m8", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m8", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m1", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m8", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m8", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m8", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m8", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m8", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m8", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m8", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m8", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m8", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m8", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m8", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m8", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m8", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m8", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m8", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m8", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m8", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m8", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m8", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m8.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m9 ----

#### Fit ----

fit.m9 <- lrm(y ~ 
                rcs(n_prescribed_medications, 4) +
                high_risk_medications.yes_1 +
                age,
              data = data,
              x = TRUE,
              y = TRUE)
fit.m9
anova(fit.m9)
plot(anova(fit.m9))

##### Write statistics to object ----

statistics["m9", "n_df"] <- 5
statistics["m9", "RsqN.app"] <- fit.m9[["stats"]][["R2"]]
statistics["m9", "Brier.app"] <- fit.m9[["stats"]][["Brier"]]
statistics["m9", "C.app"] <- fit.m9[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m9 <- validate(fit.m9,
                        method = "boot",
                        B = 1000,
                        pr = TRUE)

boot.fit.m9.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1 +
                                                age",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m9", "RsqN.optimism_corrected"] <- boot.fit.m9[2, 5]
statistics["m9", "RsqN.optimism"] <- boot.fit.m9[2, 4]
statistics["m9", "Brier.optimism_corrected"] <- boot.fit.m9[9, 5]
statistics["m9", "C.optimism_corrected"] <-  (boot.fit.m9[1, 5] + 1)/2
statistics["m9", "intercept.dev"] <- boot.fit.m9[3, 1]
statistics["m9", "intercept.boot"] <- boot.fit.m9[3, 5]
statistics["m9", "slope.dev"] <- boot.fit.m9[4, 1]
statistics["m9", "slope.boot"] <- boot.fit.m9[4, 5]
statistics["m9", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m9", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m9", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m9", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m9", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m9", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m9", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m9", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m9", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m9", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m9", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m9", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m9", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m9", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m9", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m9", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m9", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m9", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m9", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m9", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m9", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m9", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m9", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m9", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m9", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m9", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m9", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m9", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m9", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m9", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m9", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m9", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m9", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m9", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m9", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m9", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m9", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m9", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m9", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m9", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m9", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m9", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m9", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m9", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m9", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m9", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m9", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m9", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m9", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m9", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m9.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m10 ----

#### Fit ----

fit.m10 <- lrm(y ~ 
                n_prescribed_medications +
                high_risk_medications.yes_1 +
                eGFR.below_60_1 +
                age,
              data = data,
              x = TRUE,
              y = TRUE)
fit.m10
anova(fit.m10)
plot(anova(fit.m10))

##### Write statistics to object ----

statistics["m10", "n_df"] <- 4
statistics["m10", "RsqN.app"] <- fit.m10[["stats"]][["R2"]]
statistics["m10", "Brier.app"] <- fit.m10[["stats"]][["Brier"]]
statistics["m10", "C.app"] <- fit.m10[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m10 <- validate(fit.m10,
                        method = "boot",
                        B = 1000,
                        pr = TRUE)

boot.fit.m10.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m10", "RsqN.optimism_corrected"] <- boot.fit.m10[2, 5]
statistics["m10", "RsqN.optimism"] <- boot.fit.m10[2, 4]
statistics["m10", "Brier.optimism_corrected"] <- boot.fit.m10[9, 5]
statistics["m10", "C.optimism_corrected"] <-  (boot.fit.m10[1, 5] + 1)/2
statistics["m10", "intercept.dev"] <- boot.fit.m10[3, 1]
statistics["m10", "intercept.boot"] <- boot.fit.m10[3, 5]
statistics["m10", "slope.dev"] <- boot.fit.m10[4, 1]
statistics["m10", "slope.boot"] <- boot.fit.m10[4, 5]
statistics["m10", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m10", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m10", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m10", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m10", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m10", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m10", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m10", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m10", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m10", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m10", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m10", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m10", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m10", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m10", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m10", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m10", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m10", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m10", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m10", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m10", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m10", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m10", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m10", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m10", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m10", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m10", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m10", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m10", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m10", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m10", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m10", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m10", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m10", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m10", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m10", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m10", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m10", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m10", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m10", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m10", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m10", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m10", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m10", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m10", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m10", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m10", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m10", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m10", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m10", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m10.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m11 ----

#### Fit ----

fit.m11 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 3) +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m11
anova(fit.m11)
plot(anova(fit.m11))

##### Write statistics to object ----

statistics["m11", "n_df"] <- 5
statistics["m11", "RsqN.app"] <- fit.m11[["stats"]][["R2"]]
statistics["m11", "Brier.app"] <- fit.m11[["stats"]][["Brier"]]
statistics["m11", "C.app"] <- fit.m11[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m11 <- validate(fit.m11,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m11.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m11", "RsqN.optimism_corrected"] <- boot.fit.m11[2, 5]
statistics["m11", "RsqN.optimism"] <- boot.fit.m11[2, 4]
statistics["m11", "Brier.optimism_corrected"] <- boot.fit.m11[9, 5]
statistics["m11", "C.optimism_corrected"] <-  (boot.fit.m11[1, 5] + 1)/2
statistics["m11", "intercept.dev"] <- boot.fit.m11[3, 1]
statistics["m11", "intercept.boot"] <- boot.fit.m11[3, 5]
statistics["m11", "slope.dev"] <- boot.fit.m11[4, 1]
statistics["m11", "slope.boot"] <- boot.fit.m11[4, 5]
statistics["m11", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m11", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m11", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m11", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m11", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m11", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m11", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m11", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m11", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m11", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m11", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m11", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m11", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m11", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m11", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m11", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m11", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m11", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m11", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m11", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m11", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m11", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m11", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m11", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m11", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m11", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m11", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m11", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m11", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m11", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m11", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m11", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m11", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m11", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m11", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m11", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m11", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m11", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m11", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m11", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m11", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m11", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m11", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m11", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m11", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m11", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m11", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m11", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m11", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m11", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m11.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m12 ----

#### Fit ----

fit.m12 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 4) +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m12
anova(fit.m12)
plot(anova(fit.m12))

##### Write statistics to object ----

statistics["m12", "n_df"] <- 6
statistics["m12", "RsqN.app"] <- fit.m12[["stats"]][["R2"]]
statistics["m12", "Brier.app"] <- fit.m12[["stats"]][["Brier"]]
statistics["m12", "C.app"] <- fit.m12[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m12 <- validate(fit.m12,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m12.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m12", "RsqN.optimism_corrected"] <- boot.fit.m12[2, 5]
statistics["m12", "RsqN.optimism"] <- boot.fit.m12[2, 4]
statistics["m12", "Brier.optimism_corrected"] <- boot.fit.m12[9, 5]
statistics["m12", "C.optimism_corrected"] <-  (boot.fit.m12[1, 5] + 1)/2
statistics["m12", "intercept.dev"] <- boot.fit.m12[3, 1]
statistics["m12", "intercept.boot"] <- boot.fit.m12[3, 5]
statistics["m12", "slope.dev"] <- boot.fit.m12[4, 1]
statistics["m12", "slope.boot"] <- boot.fit.m12[4, 5]
statistics["m12", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m12", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m12", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m12", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m12", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m12", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m12", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m12", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m12", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m12", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m12", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m12", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m12", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m12", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m12", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m12", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m12", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m12", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m12", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m12", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m12", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m12", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m12", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m12", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m12", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m12", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m12", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m12", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m12", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m12", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m12", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m12", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m12", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m12", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m12", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m12", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m12", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m12", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m12", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m12", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m12", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m12", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m12", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m12", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m12", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m12", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m12", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m12", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m12", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m12", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m12.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m13 ----

#### Fit ----

fit.m13 <- lrm(y ~ 
                 n_prescribed_medications +
                 high_risk_medications.yes_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m13
anova(fit.m13)
plot(anova(fit.m13))

##### Write statistics to object ----

statistics["m13", "n_df"] <- 5
statistics["m13", "RsqN.app"] <- fit.m13[["stats"]][["R2"]]
statistics["m13", "Brier.app"] <- fit.m13[["stats"]][["Brier"]]
statistics["m13", "C.app"] <- fit.m13[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m13 <- validate(fit.m13,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m13.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m13", "RsqN.optimism_corrected"] <- boot.fit.m13[2, 5]
statistics["m13", "RsqN.optimism"] <- boot.fit.m13[2, 4]
statistics["m13", "Brier.optimism_corrected"] <- boot.fit.m13[9, 5]
statistics["m13", "C.optimism_corrected"] <-  (boot.fit.m13[1, 5] + 1)/2
statistics["m13", "intercept.dev"] <- boot.fit.m13[3, 1]
statistics["m13", "intercept.boot"] <- boot.fit.m13[3, 5]
statistics["m13", "slope.dev"] <- boot.fit.m13[4, 1]
statistics["m13", "slope.boot"] <- boot.fit.m13[4, 5]
statistics["m13", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m13", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m13", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m13", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m13", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m13", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m13", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m13", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m13", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m13", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m13", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m13", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m13", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m13", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m13", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m13", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m13", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m13", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m13", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m13", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m13", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m13", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m13", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m13", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m13", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m13", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m13", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m13", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m13", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m13", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m13", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m13", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m13", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m13", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m13", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m13", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m13", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m13", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m13", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m13", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m13", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m13", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m13", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m13", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m13", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m13", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m13", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m13", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m13", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m13", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m13.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m14 ----

#### Fit ----

fit.m14 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 3) +
                 high_risk_medications.yes_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m14
anova(fit.m14)
plot(anova(fit.m14))

##### Write statistics to object ----

statistics["m14", "n_df"] <- 6
statistics["m14", "RsqN.app"] <- fit.m14[["stats"]][["R2"]]
statistics["m14", "Brier.app"] <- fit.m14[["stats"]][["Brier"]]
statistics["m14", "C.app"] <- fit.m14[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m14 <- validate(fit.m14,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m14.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m14", "RsqN.optimism_corrected"] <- boot.fit.m14[2, 5]
statistics["m14", "RsqN.optimism"] <- boot.fit.m14[2, 4]
statistics["m14", "Brier.optimism_corrected"] <- boot.fit.m14[9, 5]
statistics["m14", "C.optimism_corrected"] <-  (boot.fit.m14[1, 5] + 1)/2
statistics["m14", "intercept.dev"] <- boot.fit.m14[3, 1]
statistics["m14", "intercept.boot"] <- boot.fit.m14[3, 5]
statistics["m14", "slope.dev"] <- boot.fit.m14[4, 1]
statistics["m14", "slope.boot"] <- boot.fit.m14[4, 5]
statistics["m14", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m14", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m14", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m14", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m14", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m14", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m14", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m14", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m14", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m14", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m14", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m14", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m14", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m14", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m14", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m14", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m14", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m14", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m14", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m14", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m14", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m14", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m14", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m14", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m14", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m14", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m14", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m14", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m14", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m14", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m14", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m14", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m14", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m14", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m14", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m14", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m14", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m14", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m14", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m14", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m14", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m14", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m14", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m14", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m14", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m14", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m14", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m14", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m14", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m14", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m14.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m15 ----

#### Fit ----

fit.m15 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 4) +
                 high_risk_medications.yes_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m15
anova(fit.m15)
plot(anova(fit.m15))

##### Write statistics to object ----

statistics["m15", "n_df"] <- 7
statistics["m15", "RsqN.app"] <- fit.m15[["stats"]][["R2"]]
statistics["m15", "Brier.app"] <- fit.m15[["stats"]][["Brier"]]
statistics["m15", "C.app"] <- fit.m15[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m15 <- validate(fit.m15,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m15.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m15", "RsqN.optimism_corrected"] <- boot.fit.m15[2, 5]
statistics["m15", "RsqN.optimism"] <- boot.fit.m15[2, 4]
statistics["m15", "Brier.optimism_corrected"] <- boot.fit.m15[9, 5]
statistics["m15", "C.optimism_corrected"] <-  (boot.fit.m15[1, 5] + 1)/2
statistics["m15", "intercept.dev"] <- boot.fit.m15[3, 1]
statistics["m15", "intercept.boot"] <- boot.fit.m15[3, 5]
statistics["m15", "slope.dev"] <- boot.fit.m15[4, 1]
statistics["m15", "slope.boot"] <- boot.fit.m15[4, 5]
statistics["m15", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m15", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m15", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m15", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m15", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m15", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m15", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m15", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m15", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m15", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m15", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m15", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m15", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m15", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m15", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m15", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m15", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m15", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m15", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m15", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m15", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m15", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m15", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m15", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m15", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m15", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m15", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m15", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m15", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m15", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m15", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m15", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m15", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m15", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m15", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m15", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m15", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m15", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m15", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m15", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m15", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m15", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m15", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m15", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m15", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m15", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m15", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m15", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m15", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m15", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m15.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m16 ----

#### Fit ----

fit.m16 <- lrm(y ~ 
                 n_prescribed_medications +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m16
anova(fit.m16)
plot(anova(fit.m16))

##### Write statistics to object ----

statistics["m16", "n_df"] <- 6
statistics["m16", "RsqN.app"] <- fit.m16[["stats"]][["R2"]]
statistics["m16", "Brier.app"] <- fit.m16[["stats"]][["Brier"]]
statistics["m16", "C.app"] <- fit.m16[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m16 <- validate(fit.m16,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m16.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m16", "RsqN.optimism_corrected"] <- boot.fit.m16[2, 5]
statistics["m16", "RsqN.optimism"] <- boot.fit.m16[2, 4]
statistics["m16", "Brier.optimism_corrected"] <- boot.fit.m16[9, 5]
statistics["m16", "C.optimism_corrected"] <-  (boot.fit.m16[1, 5] + 1)/2
statistics["m16", "intercept.dev"] <- boot.fit.m16[3, 1]
statistics["m16", "intercept.boot"] <- boot.fit.m16[3, 5]
statistics["m16", "slope.dev"] <- boot.fit.m16[4, 1]
statistics["m16", "slope.boot"] <- boot.fit.m16[4, 5]
statistics["m16", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m16", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m16", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m16", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m16", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m16", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m16", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m16", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m16", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m16", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m16", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m16", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m16", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m16", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m16", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m16", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m16", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m16", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m16", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m16", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m16", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m16", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m16", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m16", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m16", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m16", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m16", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m16", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m16", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m16", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m1", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m1.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m16", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m16", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m16", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m16", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m16", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m16", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m16", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m16", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m16", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m16", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m16", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m16", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m16", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m16", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m16", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m16", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m16", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m16", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m16", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m16.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m17 ----

#### Fit ----

fit.m17 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 3) +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m17
anova(fit.m17)
plot(anova(fit.m17))

##### Write statistics to object ----

statistics["m17", "n_df"] <- 7
statistics["m17", "RsqN.app"] <- fit.m17[["stats"]][["R2"]]
statistics["m17", "Brier.app"] <- fit.m17[["stats"]][["Brier"]]
statistics["m17", "C.app"] <- fit.m17[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m17 <- validate(fit.m17,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m17.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m17", "RsqN.optimism_corrected"] <- boot.fit.m17[2, 5]
statistics["m17", "RsqN.optimism"] <- boot.fit.m17[2, 4]
statistics["m17", "Brier.optimism_corrected"] <- boot.fit.m17[9, 5]
statistics["m17", "C.optimism_corrected"] <-  (boot.fit.m17[1, 5] + 1)/2
statistics["m17", "intercept.dev"] <- boot.fit.m17[3, 1]
statistics["m17", "intercept.boot"] <- boot.fit.m17[3, 5]
statistics["m17", "slope.dev"] <- boot.fit.m17[4, 1]
statistics["m17", "slope.boot"] <- boot.fit.m17[4, 5]
statistics["m17", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m17", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m17", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m17", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m17", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m17", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m17", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m17", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m17", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m17", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m17", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m17", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m17", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m17", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m17", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m17", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m17", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m17", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m17", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m17", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m17", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m17", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m17", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m17", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m17", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m17", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m17", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m17", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m17", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m17", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m17", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m17", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m17", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m17", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m17", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m17", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m17", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m17", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m17", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m17", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m17", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m17", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m17", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m17", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m17", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m17", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m17", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m17", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m17", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m17", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m17.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m18 ----

#### Fit ----

fit.m18 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 4) +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m18
anova(fit.m18)
plot(anova(fit.m18))

##### Write statistics to object ----

statistics["m18", "n_df"] <- 8
statistics["m18", "RsqN.app"] <- fit.m18[["stats"]][["R2"]]
statistics["m18", "Brier.app"] <- fit.m18[["stats"]][["Brier"]]
statistics["m18", "C.app"] <- fit.m18[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m18 <- validate(fit.m18,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m18.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m18", "RsqN.optimism_corrected"] <- boot.fit.m18[2, 5]
statistics["m18", "RsqN.optimism"] <- boot.fit.m18[2, 4]
statistics["m18", "Brier.optimism_corrected"] <- boot.fit.m18[9, 5]
statistics["m18", "C.optimism_corrected"] <-  (boot.fit.m18[1, 5] + 1)/2
statistics["m18", "intercept.dev"] <- boot.fit.m18[3, 1]
statistics["m18", "intercept.boot"] <- boot.fit.m18[3, 5]
statistics["m18", "slope.dev"] <- boot.fit.m18[4, 1]
statistics["m18", "slope.boot"] <- boot.fit.m18[4, 5]
statistics["m18", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m18", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m18", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m18", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m18", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m18", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m18", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m18", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m18", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m18", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m18", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m18", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m18", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m18", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m18", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m18", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m18", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m18", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m18", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m18", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m18", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m18", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m18", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m18", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m18", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m18", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m18", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m18", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m18", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m18", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m18", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m18", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m18", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m18", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m18", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m18", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m18", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m18", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m18", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m18", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m18", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m18", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m18", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m18", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m18", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m18", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m18", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m18", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m18", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m18", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m18.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m19 ----

#### Fit ----

fit.m19 <- lrm(y ~ 
                 n_prescribed_medications +
                 high_risk_medications.yes_1 +
                 age +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m19
anova(fit.m19)
plot(anova(fit.m19))

##### Write statistics to object ----

statistics["m19", "n_df"] <- 4
statistics["m19", "RsqN.app"] <- fit.m19[["stats"]][["R2"]]
statistics["m19", "Brier.app"] <- fit.m19[["stats"]][["Brier"]]
statistics["m19", "C.app"] <- fit.m19[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m19 <- validate(fit.m19,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m19.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                age +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m19", "RsqN.optimism_corrected"] <- boot.fit.m19[2, 5]
statistics["m19", "RsqN.optimism"] <- boot.fit.m19[2, 4]
statistics["m19", "Brier.optimism_corrected"] <- boot.fit.m19[9, 5]
statistics["m19", "C.optimism_corrected"] <-  (boot.fit.m19[1, 5] + 1)/2
statistics["m19", "intercept.dev"] <- boot.fit.m19[3, 1]
statistics["m19", "intercept.boot"] <- boot.fit.m19[3, 5]
statistics["m19", "slope.dev"] <- boot.fit.m19[4, 1]
statistics["m19", "slope.boot"] <- boot.fit.m19[4, 5]
statistics["m19", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m19", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m19", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m19", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m19", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m19", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m19", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m19", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m19", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m19", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m19", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m19", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m19", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m19", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m19", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m19", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m19", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m19", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m19", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m19", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m19", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m19", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m19", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m19", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m19", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m19", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m19", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m19", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m19", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m19", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m19", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m19", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m19", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m19", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m19", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m19", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m19", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m19", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m19", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m19", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m19", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m19", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m19", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m19", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m19", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m19", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m19", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m19", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m19", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m19", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m20 ----

#### Fit ----

fit.m20 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 3) +
                 high_risk_medications.yes_1 +
                 age +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m20
anova(fit.m20)
plot(anova(fit.m20))

##### Write statistics to object ----

statistics["m20", "n_df"] <- 5
statistics["m20", "RsqN.app"] <- fit.m20[["stats"]][["R2"]]
statistics["m20", "Brier.app"] <- fit.m20[["stats"]][["Brier"]]
statistics["m20", "C.app"] <- fit.m20[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m20 <- validate(fit.m20,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m20.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1 +
                                                age +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m20", "RsqN.optimism_corrected"] <- boot.fit.m20[2, 5]
statistics["m20", "RsqN.optimism"] <- boot.fit.m20[2, 4]
statistics["m20", "Brier.optimism_corrected"] <- boot.fit.m20[9, 5]
statistics["m20", "C.optimism_corrected"] <-  (boot.fit.m20[1, 5] + 1)/2
statistics["m20", "intercept.dev"] <- boot.fit.m20[3, 1]
statistics["m20", "intercept.boot"] <- boot.fit.m20[3, 5]
statistics["m20", "slope.dev"] <- boot.fit.m20[4, 1]
statistics["m20", "slope.boot"] <- boot.fit.m20[4, 5]
statistics["m20", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m20", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m20", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m20", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m20", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m20", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m20", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m20", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m20", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m20", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m20", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m20", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m20", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m20", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m20", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m20", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m20", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m20", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m20", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m20", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m20", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m20", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m20", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m20", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m20", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m20", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m20", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m20", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m20", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m20", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m20", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m20", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m20", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m20", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m20", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m20", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m20", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m20", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m20", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m20", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m20", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m20", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m20", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m20", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m20", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m20", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m20", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m20", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m20", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m20", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m20.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m21 ----

#### Fit ----

fit.m21 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 4) +
                 high_risk_medications.yes_1 +
                 age +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m21
anova(fit.m21)
plot(anova(fit.m21))

##### Write statistics to object ----

statistics["m21", "n_df"] <- 6
statistics["m21", "RsqN.app"] <- fit.m21[["stats"]][["R2"]]
statistics["m21", "Brier.app"] <- fit.m21[["stats"]][["Brier"]]
statistics["m21", "C.app"] <- fit.m21[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m21 <- validate(fit.m21,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m21.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1 +
                                                age +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m21", "RsqN.optimism_corrected"] <- boot.fit.m21[2, 5]
statistics["m21", "RsqN.optimism"] <- boot.fit.m21[2, 4]
statistics["m21", "Brier.optimism_corrected"] <- boot.fit.m21[9, 5]
statistics["m21", "C.optimism_corrected"] <-  (boot.fit.m21[1, 5] + 1)/2
statistics["m21", "intercept.dev"] <- boot.fit.m21[3, 1]
statistics["m21", "intercept.boot"] <- boot.fit.m21[3, 5]
statistics["m21", "slope.dev"] <- boot.fit.m21[4, 1]
statistics["m21", "slope.boot"] <- boot.fit.m21[4, 5]
statistics["m21", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m21", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m21", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m21", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m21", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m21", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m21", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m21", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m21", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m21", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m21", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m21", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m21", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m21", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m21", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m21", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m21", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m21", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m21", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m21", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m21", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m21", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m21", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m21", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m21", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m21", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m21", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m21", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m21", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m21", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m21", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m21", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m21", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m21", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m21", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m21", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m21", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m21", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m21", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m21", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m21", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m21", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m21", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m21", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m21", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m21", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m21", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m21", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m21", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m21", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m21.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m22 ----

#### Fit ----

fit.m22 <- lrm(y ~ 
                 n_prescribed_medications +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m22
anova(fit.m22)
plot(anova(fit.m22))

##### Write statistics to object ----

statistics["m22", "n_df"] <- 5
statistics["m22", "RsqN.app"] <- fit.m22[["stats"]][["R2"]]
statistics["m22", "Brier.app"] <- fit.m22[["stats"]][["Brier"]]
statistics["m22", "C.app"] <- fit.m22[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m22 <- validate(fit.m22,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m22.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m22", "RsqN.optimism_corrected"] <- boot.fit.m22[2, 5]
statistics["m22", "RsqN.optimism"] <- boot.fit.m22[2, 4]
statistics["m22", "Brier.optimism_corrected"] <- boot.fit.m22[9, 5]
statistics["m22", "C.optimism_corrected"] <-  (boot.fit.m22[1, 5] + 1)/2
statistics["m22", "intercept.dev"] <- boot.fit.m22[3, 1]
statistics["m22", "intercept.boot"] <- boot.fit.m22[3, 5]
statistics["m22", "slope.dev"] <- boot.fit.m22[4, 1]
statistics["m22", "slope.boot"] <- boot.fit.m22[4, 5]
statistics["m22", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m22", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m22", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m22", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m22", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m22", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m22", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m22", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m22", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m22", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m22", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m22", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m22", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m22", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m22", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m22", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m22", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m22", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m22", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m22", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m22", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m22", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m22", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m22", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m22", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m22", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m22", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m22", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m22", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m22", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m22", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m22", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m22", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m22", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m22", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m22", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m22", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m22", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m22", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m22", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m22", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m22", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m22", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m22", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m22", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m22", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m22", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m22", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m22", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m22", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m22.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m23 ----

#### Fit ----

fit.m23 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 3) +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m23
anova(fit.m23)
plot(anova(fit.m23))

##### Write statistics to object ----

statistics["m23", "n_df"] <- 6
statistics["m23", "RsqN.app"] <- fit.m23[["stats"]][["R2"]]
statistics["m23", "Brier.app"] <- fit.m23[["stats"]][["Brier"]]
statistics["m23", "C.app"] <- fit.m23[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m23 <- validate(fit.m23,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m23.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m23", "RsqN.optimism_corrected"] <- boot.fit.m23[2, 5]
statistics["m23", "RsqN.optimism"] <- boot.fit.m23[2, 4]
statistics["m23", "Brier.optimism_corrected"] <- boot.fit.m23[9, 5]
statistics["m23", "C.optimism_corrected"] <-  (boot.fit.m23[1, 5] + 1)/2
statistics["m23", "intercept.dev"] <- boot.fit.m23[3, 1]
statistics["m23", "intercept.boot"] <- boot.fit.m23[3, 5]
statistics["m23", "slope.dev"] <- boot.fit.m23[4, 1]
statistics["m23", "slope.boot"] <- boot.fit.m23[4, 5]
statistics["m23", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m23", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m23", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m23", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m23", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m23", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m23", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m23", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m23", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m23", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m23", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m23", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m23", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m23", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m23", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m23", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m23", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m23", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m23", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m23", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m23", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m23", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m23", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m23", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m23", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m23", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m23", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m23", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m23", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m23", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m23", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m23", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m23", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m23", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m23", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m23", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m23", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m23", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m23", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m23", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m23", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m23", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m23", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m23", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m23", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m23", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m23", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m23", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m23", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m23", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m23.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m24 ----

#### Fit ----

fit.m24 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 4) +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m24
anova(fit.m24)
plot(anova(fit.m24))

##### Write statistics to object ----

statistics["m24", "n_df"] <- 7
statistics["m24", "RsqN.app"] <- fit.m24[["stats"]][["R2"]]
statistics["m24", "Brier.app"] <- fit.m24[["stats"]][["Brier"]]
statistics["m24", "C.app"] <- fit.m24[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m24 <- validate(fit.m24,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m24.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m24", "RsqN.optimism_corrected"] <- boot.fit.m24[2, 5]
statistics["m24", "RsqN.optimism"] <- boot.fit.m24[2, 4]
statistics["m24", "Brier.optimism_corrected"] <- boot.fit.m24[9, 5]
statistics["m24", "C.optimism_corrected"] <-  (boot.fit.m24[1, 5] + 1)/2
statistics["m24", "intercept.dev"] <- boot.fit.m24[3, 1]
statistics["m24", "intercept.boot"] <- boot.fit.m24[3, 5]
statistics["m24", "slope.dev"] <- boot.fit.m24[4, 1]
statistics["m24", "slope.boot"] <- boot.fit.m24[4, 5]
statistics["m24", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m24", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m24", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m24", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m24", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m24", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m24", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m24", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m24", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m24", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m24", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m24", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m24", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m24", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m24", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m24", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m24", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m24", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m24", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m24", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m24", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m24", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m24", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m24", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m24", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m24", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m24", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m24", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m24", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m24", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m24", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m24", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m24", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m24", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m24", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m24", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m24", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m24", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m24", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m24", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m24", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m24", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m24", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m24", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m24", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m24", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m24", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m24", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m24", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m24", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m24.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m25 ----

#### Fit ----

fit.m25 <- lrm(y ~ 
                 n_prescribed_medications +
                 high_risk_medications.yes_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1 +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m25
anova(fit.m25)
plot(anova(fit.m25))

##### Write statistics to object ----

statistics["m25", "n_df"] <- 6
statistics["m25", "RsqN.app"] <- fit.m25[["stats"]][["R2"]]
statistics["m25", "Brier.app"] <- fit.m25[["stats"]][["Brier"]]
statistics["m25", "C.app"] <- fit.m25[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m25 <- validate(fit.m25,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m25.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1 +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m25", "RsqN.optimism_corrected"] <- boot.fit.m25[2, 5]
statistics["m25", "RsqN.optimism"] <- boot.fit.m25[2, 4]
statistics["m25", "Brier.optimism_corrected"] <- boot.fit.m25[9, 5]
statistics["m25", "C.optimism_corrected"] <-  (boot.fit.m25[1, 5] + 1)/2
statistics["m25", "intercept.dev"] <- boot.fit.m25[3, 1]
statistics["m25", "intercept.boot"] <- boot.fit.m25[3, 5]
statistics["m25", "slope.dev"] <- boot.fit.m25[4, 1]
statistics["m25", "slope.boot"] <- boot.fit.m25[4, 5]
statistics["m25", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m25", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m25", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m25", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m25", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m25", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m25", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m25", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m25", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m25", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m25", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m25", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m25", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m25", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m25", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m25", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m25", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m25", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m25", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m25", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m25", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m25", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m25", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m25", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m25", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m25", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m25", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m25", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m25", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m25", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m25", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m25", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m25", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m25", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m25", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m25", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m25", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m25", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m25", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m25", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m25", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m25", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m25", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m25", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m25", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m25", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m25", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m25", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m25", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m25", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m25.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m26 ----

#### Fit ----

fit.m26 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 3) +
                 high_risk_medications.yes_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1 +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m26
anova(fit.m26)
plot(anova(fit.m26))

##### Write statistics to object ----

statistics["m26", "n_df"] <- 7
statistics["m26", "RsqN.app"] <- fit.m26[["stats"]][["R2"]]
statistics["m26", "Brier.app"] <- fit.m26[["stats"]][["Brier"]]
statistics["m26", "C.app"] <- fit.m26[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m26 <- validate(fit.m26,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m26.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1 +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m26", "RsqN.optimism_corrected"] <- boot.fit.m26[2, 5]
statistics["m26", "RsqN.optimism"] <- boot.fit.m26[2, 4]
statistics["m26", "Brier.optimism_corrected"] <- boot.fit.m26[9, 5]
statistics["m26", "C.optimism_corrected"] <-  (boot.fit.m26[1, 5] + 1)/2
statistics["m26", "intercept.dev"] <- boot.fit.m26[3, 1]
statistics["m26", "intercept.boot"] <- boot.fit.m26[3, 5]
statistics["m26", "slope.dev"] <- boot.fit.m26[4, 1]
statistics["m26", "slope.boot"] <- boot.fit.m26[4, 5]
statistics["m26", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m26", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m26", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m26", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m26", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m26", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m26", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m26", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m26", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m26", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m26", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m26", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m26", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m26", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m26", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m26", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m26", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m26", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m26", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m26", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m26", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m26", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m26", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m26", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m26", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m26", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m26", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m26", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m26", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m26", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m26", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m26", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m26", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m26", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m26", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m26", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m26", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m26", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m26", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m26", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m26", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m26", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m26", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m26", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m26", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m26", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m26", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m26", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m26", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m26", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m26.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m27 ----

#### Fit ----

fit.m27 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 4) +
                 high_risk_medications.yes_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1 +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m27
anova(fit.m27)
plot(anova(fit.m27))

##### Write statistics to object ----

statistics["m27", "n_df"] <- 8
statistics["m27", "RsqN.app"] <- fit.m27[["stats"]][["R2"]]
statistics["m27", "Brier.app"] <- fit.m27[["stats"]][["Brier"]]
statistics["m27", "C.app"] <- fit.m27[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m27 <- validate(fit.m27,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m27.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1 +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m27", "RsqN.optimism_corrected"] <- boot.fit.m27[2, 5]
statistics["m27", "RsqN.optimism"] <- boot.fit.m27[2, 4]
statistics["m27", "Brier.optimism_corrected"] <- boot.fit.m27[9, 5]
statistics["m27", "C.optimism_corrected"] <-  (boot.fit.m27[1, 5] + 1)/2
statistics["m27", "intercept.dev"] <- boot.fit.m27[3, 1]
statistics["m27", "intercept.boot"] <- boot.fit.m27[3, 5]
statistics["m27", "slope.dev"] <- boot.fit.m27[4, 1]
statistics["m27", "slope.boot"] <- boot.fit.m27[4, 5]
statistics["m27", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m27", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m27", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m27", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m27", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m27", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m27", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m27", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m27", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m27", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m27", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m27", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m27", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m27", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m27", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m27", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m27", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m27", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m27", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m27", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m27", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m27", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m27", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m27", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m27", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m27", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m27", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m27", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m27", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m27", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m27", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m27", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m27", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m27", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m27", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m27", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m27", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m27", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m27", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m27", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m27", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m27", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m27", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m27", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m27", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m27", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m27", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m27", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m27", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m27", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m27.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m28 ----

#### Fit ----

fit.m28 <- lrm(y ~ 
                 n_prescribed_medications +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1 +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m28
anova(fit.m28)
plot(anova(fit.m28))

##### Write statistics to object ----

statistics["m28", "n_df"] <- 7
statistics["m28", "RsqN.app"] <- fit.m28[["stats"]][["R2"]]
statistics["m28", "Brier.app"] <- fit.m28[["stats"]][["Brier"]]
statistics["m28", "C.app"] <- fit.m28[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m28 <- validate(fit.m28,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m28.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1 +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m28", "RsqN.optimism_corrected"] <- boot.fit.m28[2, 5]
statistics["m28", "RsqN.optimism"] <- boot.fit.m28[2, 4]
statistics["m28", "Brier.optimism_corrected"] <- boot.fit.m28[9, 5]
statistics["m28", "C.optimism_corrected"] <-  (boot.fit.m28[1, 5] + 1)/2
statistics["m28", "intercept.dev"] <- boot.fit.m28[3, 1]
statistics["m28", "intercept.boot"] <- boot.fit.m28[3, 5]
statistics["m28", "slope.dev"] <- boot.fit.m28[4, 1]
statistics["m28", "slope.boot"] <- boot.fit.m28[4, 5]
statistics["m28", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m28", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m28", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m28", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m28", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m28", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m28", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m28", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m28", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m28", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m28", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m28", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m28", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m28", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m28", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m28", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m28", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m28", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m28", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m28", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m28", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m28", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m28", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m28", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m28", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m28", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m28", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m28", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m28", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m28", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m28", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m28", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m28", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m28", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m28", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m28", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m28", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m28", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m28", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m28", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m28", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m28", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m28", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m28", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m28", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m28", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m28", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m28", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m28", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m28", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m28.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m29 ----

#### Fit ----

fit.m29 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 3) +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1 +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m29
anova(fit.m29)
plot(anova(fit.m29))

##### Write statistics to object ----

statistics["m29", "n_df"] <- 8
statistics["m29", "RsqN.app"] <- fit.m29[["stats"]][["R2"]]
statistics["m29", "Brier.app"] <- fit.m29[["stats"]][["Brier"]]
statistics["m29", "C.app"] <- fit.m29[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m29 <- validate(fit.m29,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m29.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1 +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m29", "RsqN.optimism_corrected"] <- boot.fit.m29[2, 5]
statistics["m29", "RsqN.optimism"] <- boot.fit.m29[2, 4]
statistics["m29", "Brier.optimism_corrected"] <- boot.fit.m29[9, 5]
statistics["m29", "C.optimism_corrected"] <-  (boot.fit.m29[1, 5] + 1)/2
statistics["m29", "intercept.dev"] <- boot.fit.m29[3, 1]
statistics["m29", "intercept.boot"] <- boot.fit.m29[3, 5]
statistics["m29", "slope.dev"] <- boot.fit.m29[4, 1]
statistics["m29", "slope.boot"] <- boot.fit.m29[4, 5]
statistics["m29", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m29", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m29", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m29", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m29", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m29", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m29", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m29", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m29", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m29", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m29", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m29", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m29", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m29", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m29", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m29", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m29", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m29", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m29", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m29", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m29", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m29", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m29", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m29", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m29", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m29", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m29", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m29", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m29", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m29", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m29", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m29", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m29", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m29", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m29", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m29", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m29", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m29", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m29", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m29", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m29", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m29", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m29", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m29", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m29", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m29", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m29", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m29", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m29", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m29", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m29.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m30 ----

#### Fit ----

fit.m30 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 4) +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1 +
                 inout_specialty_current_12m_MUMC.multiple_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m30
anova(fit.m30)
plot(anova(fit.m30))

##### Write statistics to object ----

statistics["m30", "n_df"] <- 9
statistics["m30", "RsqN.app"] <- fit.m30[["stats"]][["R2"]]
statistics["m30", "Brier.app"] <- fit.m30[["stats"]][["Brier"]]
statistics["m30", "C.app"] <- fit.m30[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m30 <- validate(fit.m30,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m30.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1 +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m30", "RsqN.optimism_corrected"] <- boot.fit.m30[2, 5]
statistics["m30", "RsqN.optimism"] <- boot.fit.m30[2, 4]
statistics["m30", "Brier.optimism_corrected"] <- boot.fit.m30[9, 5]
statistics["m30", "C.optimism_corrected"] <-  (boot.fit.m30[1, 5] + 1)/2
statistics["m30", "intercept.dev"] <- boot.fit.m30[3, 1]
statistics["m30", "intercept.boot"] <- boot.fit.m30[3, 5]
statistics["m30", "slope.dev"] <- boot.fit.m30[4, 1]
statistics["m30", "slope.boot"] <- boot.fit.m30[4, 5]
statistics["m30", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m30", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m30", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m30", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m30", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m30", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m30", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m30", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m30", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m30", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m30", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m30", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m30", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m30", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m30", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m30", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m30", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m30", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m30", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m30", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m30", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m30", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m30", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m30", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m30", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m30", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m30", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m30", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m30", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m30", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m30", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m30", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m30", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m30", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m30", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m30", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m30", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m30", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m30", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m30", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m30", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m30", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m30", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m30", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m30", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m30", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m30", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m30", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m30", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m30", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m30.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m31 ----

#### Fit ----

fit.m31 <- lrm(y ~ 
                 n_prescribed_medications +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1 +
                 inout_specialty_current_12m_MUMC.multiple_1 +
                 medication_literacy.adequate_1 +
                 housing.independent_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m31
anova(fit.m31)
plot(anova(fit.m31))

##### Write statistics to object ----

statistics["m31", "n_df"] <- 9
statistics["m31", "RsqN.app"] <- fit.m31[["stats"]][["R2"]]
statistics["m31", "Brier.app"] <- fit.m31[["stats"]][["Brier"]]
statistics["m31", "C.app"] <- fit.m31[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m31 <- validate(fit.m31,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m31.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1 +
                                                inout_specialty_current_12m_MUMC.multiple_1 +
                                                medication_literacy.adequate_1 +
                                                housing.independent_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m31", "RsqN.optimism_corrected"] <- boot.fit.m31[2, 5]
statistics["m31", "RsqN.optimism"] <- boot.fit.m31[2, 4]
statistics["m31", "Brier.optimism_corrected"] <- boot.fit.m31[9, 5]
statistics["m31", "C.optimism_corrected"] <-  (boot.fit.m31[1, 5] + 1)/2
statistics["m31", "intercept.dev"] <- boot.fit.m31[3, 1]
statistics["m31", "intercept.boot"] <- boot.fit.m31[3, 5]
statistics["m31", "slope.dev"] <- boot.fit.m31[4, 1]
statistics["m31", "slope.boot"] <- boot.fit.m31[4, 5]
statistics["m31", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m31", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m31", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m31", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m31", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m31", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m31", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m31", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m31", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m31", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m31", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m31", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m31", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m31", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m31", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m31", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m31", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m31", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m31", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m31", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m31", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m31", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m31", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m31", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m31", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m31", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m31", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m31", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m31", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m31", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m31", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m31", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m31", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m31", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m31", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m31", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m31", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m31", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m31", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m31", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m31", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m31", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m31", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m31", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m31", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m31", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m31", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m31", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m31", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m31", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m31.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m32 ----

#### Fit ----

fit.m32 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 3) +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1 +
                 inout_specialty_current_12m_MUMC.multiple_1 +
                 medication_literacy.adequate_1 +
                 housing.independent_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m32
anova(fit.m32)
plot(anova(fit.m32))

##### Write statistics to object ----

statistics["m32", "n_df"] <- 10
statistics["m32", "RsqN.app"] <- fit.m32[["stats"]][["R2"]]
statistics["m32", "Brier.app"] <- fit.m32[["stats"]][["Brier"]]
statistics["m32", "C.app"] <- fit.m32[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m32 <- validate(fit.m32,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m32.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 3) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1 +
                                                inout_specialty_current_12m_MUMC.multiple_1 +
                                                medication_literacy.adequate_1 +
                                                housing.independent_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m32", "RsqN.optimism_corrected"] <- boot.fit.m32[2, 5]
statistics["m32", "RsqN.optimism"] <- boot.fit.m32[2, 4]
statistics["m32", "Brier.optimism_corrected"] <- boot.fit.m32[9, 5]
statistics["m32", "C.optimism_corrected"] <-  (boot.fit.m32[1, 5] + 1)/2
statistics["m32", "intercept.dev"] <- boot.fit.m32[3, 1]
statistics["m32", "intercept.boot"] <- boot.fit.m32[3, 5]
statistics["m32", "slope.dev"] <- boot.fit.m32[4, 1]
statistics["m32", "slope.boot"] <- boot.fit.m32[4, 5]
statistics["m32", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m32", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m32", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m32", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m32", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m32", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m32", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m32", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m32", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m32", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m32", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m32", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m32", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m32", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m32", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m32", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m32", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m32", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m32", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m32", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m32", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m32", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m32", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m32", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m32", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m32", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m32", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m32", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m32", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m32", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m32", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m32", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m32", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m32", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m32", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m32", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m32", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m32", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m32", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m32", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m32", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m32", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m32", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m32", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m32", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m32", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m32", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m32", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m32", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m32", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m32.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

### m33 ----

#### Fit ----

fit.m33 <- lrm(y ~ 
                 rcs(n_prescribed_medications, 4) +
                 high_risk_medications.yes_1 +
                 eGFR.below_60_1 +
                 age +
                 out_specialty_current_MUMC.cardiovascular_composite_1 +
                 out_specialty_current_MUMC.surgery_composite_1 +
                 inout_specialty_current_12m_MUMC.multiple_1 +
                 medication_literacy.adequate_1 +
                 housing.independent_1,
               data = data,
               x = TRUE,
               y = TRUE)
fit.m33
anova(fit.m33)
plot(anova(fit.m33))

##### Write statistics to object ----

statistics["m33", "n_df"] <- 11
statistics["m33", "RsqN.app"] <- fit.m33[["stats"]][["R2"]]
statistics["m33", "Brier.app"] <- fit.m33[["stats"]][["Brier"]]
statistics["m33", "C.app"] <- fit.m33[["stats"]][["C"]]

#### Internal validation using bootstrapping procedure ----

boot.fit.m33 <- validate(fit.m33,
                         method = "boot",
                         B = 1000,
                         pr = TRUE)

boot.fit.m33.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                rcs(n_prescribed_medications, 4) + 
                                                high_risk_medications.yes_1 +
                                                eGFR.below_60_1 +
                                                age +
                                                out_specialty_current_MUMC.cardiovascular_composite_1 +
                                                out_specialty_current_MUMC.surgery_composite_1 +
                                                inout_specialty_current_12m_MUMC.multiple_1 +
                                                medication_literacy.adequate_1 +
                                                housing.independent_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.03, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

##### Write statistics to object ----

statistics["m33", "RsqN.optimism_corrected"] <- boot.fit.m33[2, 5]
statistics["m33", "RsqN.optimism"] <- boot.fit.m33[2, 4]
statistics["m33", "Brier.optimism_corrected"] <- boot.fit.m33[9, 5]
statistics["m33", "C.optimism_corrected"] <-  (boot.fit.m33[1, 5] + 1)/2
statistics["m33", "intercept.dev"] <- boot.fit.m33[3, 1]
statistics["m33", "intercept.boot"] <- boot.fit.m33[3, 5]
statistics["m33", "slope.dev"] <- boot.fit.m33[4, 1]
statistics["m33", "slope.boot"] <- boot.fit.m33[4, 5]
statistics["m33", "accuracy.app.threshold_0.03"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.app"]
statistics["m33", "accuracy.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "accuracy.optimism_corrected"]
statistics["m33", "true_positive_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.app"]
statistics["m33", "true_positive_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_positive_rate.optimism_corrected"]
statistics["m33", "true_negative_rate.app.threshold_0.03"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.app"]
statistics["m33", "true_negative_rate.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "true_negative_rate.optimism_corrected"]
statistics["m33", "positive_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.app"]
statistics["m33", "positive_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "positive_predictive_value.optimism_corrected"]
statistics["m33", "negative_predictive_value.app.threshold_0.03"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.app"]
statistics["m33", "negative_predictive_value.optimism_corrected.threshold_0.03"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.03", "negative_predictive_value.optimism_corrected"]
statistics["m33", "accuracy.app.threshold_0.04"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.app"]
statistics["m33", "accuracy.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "accuracy.optimism_corrected"]
statistics["m33", "true_positive_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.app"]
statistics["m33", "true_positive_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_positive_rate.optimism_corrected"]
statistics["m33", "true_negative_rate.app.threshold_0.04"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.app"]
statistics["m33", "true_negative_rate.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "true_negative_rate.optimism_corrected"]
statistics["m33", "positive_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.app"]
statistics["m33", "positive_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "positive_predictive_value.optimism_corrected"]
statistics["m33", "negative_predictive_value.app.threshold_0.04"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.app"]
statistics["m33", "negative_predictive_value.optimism_corrected.threshold_0.04"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.04", "negative_predictive_value.optimism_corrected"]
statistics["m33", "accuracy.app.threshold_0.05"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m33", "accuracy.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.optimism_corrected"]
statistics["m33", "true_positive_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.app"]
statistics["m33", "true_positive_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_positive_rate.optimism_corrected"]
statistics["m33", "true_negative_rate.app.threshold_0.05"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.app"]
statistics["m33", "true_negative_rate.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "true_negative_rate.optimism_corrected"]
statistics["m33", "positive_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.app"]
statistics["m33", "positive_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "positive_predictive_value.optimism_corrected"]
statistics["m33", "negative_predictive_value.app.threshold_0.05"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.app"]
statistics["m33", "negative_predictive_value.optimism_corrected.threshold_0.05"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "negative_predictive_value.optimism_corrected"]
statistics["m33", "accuracy.app.threshold_0.06"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.05", "accuracy.app"]
statistics["m33", "accuracy.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "accuracy.optimism_corrected"]
statistics["m33", "true_positive_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.app"]
statistics["m33", "true_positive_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_positive_rate.optimism_corrected"]
statistics["m33", "true_negative_rate.app.threshold_0.06"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.app"]
statistics["m33", "true_negative_rate.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "true_negative_rate.optimism_corrected"]
statistics["m33", "positive_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.app"]
statistics["m33", "positive_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "positive_predictive_value.optimism_corrected"]
statistics["m33", "negative_predictive_value.app.threshold_0.06"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.app"]
statistics["m33", "negative_predictive_value.optimism_corrected.threshold_0.06"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.06", "negative_predictive_value.optimism_corrected"]
statistics["m33", "accuracy.app.threshold_0.07"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.app"]
statistics["m33", "accuracy.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "accuracy.optimism_corrected"]
statistics["m33", "true_positive_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.app"]
statistics["m33", "true_positive_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_positive_rate.optimism_corrected"]
statistics["m33", "true_negative_rate.app.threshold_0.07"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.app"]
statistics["m33", "true_negative_rate.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "true_negative_rate.optimism_corrected"]
statistics["m33", "positive_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.app"]
statistics["m33", "positive_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "positive_predictive_value.optimism_corrected"]
statistics["m33", "negative_predictive_value.app.threshold_0.07"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.app"]
statistics["m33", "negative_predictive_value.optimism_corrected.threshold_0.07"] <- as.data.frame(boot.fit.m33.confusion_matrix_statistics$confusion_matrix_statistics)["threshold_0.07", "negative_predictive_value.optimism_corrected"]

## MODEL COMPARISON ----

statistics$index <- 1:33

### RsqN.optimism ----

summary(statistics$RsqN.optimism)

ggplot(statistics, aes(x = index, y = RsqN.optimism, col = as.factor(n_df))) +
  scale_x_continuous(name = "model", 
                     breaks = seq(1, 33, 1)) +
  scale_y_continuous(limits = c(0, 0.05)) +
  scale_colour_discrete(name = "number of df") +
  geom_point() +
  theme_minimal()

### slope.boot ----

summary(statistics$slope.boot)

ggplot(statistics, aes(x = index, y = slope.boot, col = as.factor(n_df))) +
  scale_x_continuous(name = "model", 
                     breaks = seq(1, 33, 1)) +
  scale_y_continuous(limits = c(0.8, 1)) +
  scale_colour_discrete(name = "number of df") +
  geom_hline(yintercept = 0.90) +
  geom_point() +
  theme_minimal()

### RsqN ----

summary(statistics$RsqN.app)
summary(statistics$RsqN.optimism_corrected)

ggplot(statistics, aes(x = index)) +
  geom_point(aes(y = RsqN.app), shape = 1) +
  geom_point(aes(y = RsqN.optimism_corrected), shape = 19) +
  scale_x_continuous(name = "model", 
                     breaks = seq(1, 33, 1)) +
  scale_y_continuous(limits = c(0, 0.15)) +
  theme_minimal()

### Brier ----

summary(statistics$Brier.app)
summary(statistics$Brier.optimism_corrected)

ggplot(statistics, aes(x = index)) +
  geom_point(aes(y = Brier.app), shape = 1) +
  geom_point(aes(y = Brier.optimism_corrected), shape = 19) +
  scale_x_continuous(name = "model", 
                     breaks = seq(1, 33, 1)) +
  scale_y_continuous(limits = c(0, 0.05)) +
  theme_minimal()

### C ----

summary(statistics$C.app)
summary(statistics$C.optimism_corrected)
ggplot(statistics, aes(x = index)) +
  geom_point(aes(y = C.app), shape = 1) +
  geom_point(aes(y = C.optimism_corrected), shape = 19) +
  scale_x_continuous(name = "model", 
                     breaks = seq(1, 33, 1)) +
  scale_y_continuous(limits = c(0.50, 1.00),
                     breaks = seq(0.50, 1.00, 0.05)) +
  theme_minimal()

### Confusion matrix based statistics ----

summary(statistics$accuracy.app.threshold_0.03)
summary(statistics$accuracy.optimism_corrected.threshold_0.03)
summary(statistics$accuracy.app.threshold_0.04)
summary(statistics$accuracy.optimism_corrected.threshold_0.04)
summary(statistics$accuracy.app.threshold_0.05)
summary(statistics$accuracy.optimism_corrected.threshold_0.05)
summary(statistics$accuracy.app.threshold_0.06)
summary(statistics$accuracy.optimism_corrected.threshold_0.06)
summary(statistics$accuracy.app.threshold_0.07)
summary(statistics$accuracy.optimism_corrected.threshold_0.07)

summary(statistics$true_positive_rate.app.threshold_0.03)
summary(statistics$true_positive_rate.optimism_corrected.threshold_0.03)
summary(statistics$true_positive_rate.app.threshold_0.04)
summary(statistics$true_positive_rate.optimism_corrected.threshold_0.04)
summary(statistics$true_positive_rate.app.threshold_0.05)
summary(statistics$true_positive_rate.optimism_corrected.threshold_0.05)
summary(statistics$true_positive_rate.app.threshold_0.06)
summary(statistics$true_positive_rate.optimism_corrected.threshold_0.06)
summary(statistics$true_positive_rate.app.threshold_0.07)
summary(statistics$true_positive_rate.optimism_corrected.threshold_0.07)

summary(statistics$true_negative_rate.app.threshold_0.03)
summary(statistics$true_negative_rate.optimism_corrected.threshold_0.03)
summary(statistics$true_negative_rate.app.threshold_0.04)
summary(statistics$true_negative_rate.optimism_corrected.threshold_0.04)
summary(statistics$true_negative_rate.app.threshold_0.05)
summary(statistics$true_negative_rate.optimism_corrected.threshold_0.05)
summary(statistics$true_negative_rate.app.threshold_0.06)
summary(statistics$true_negative_rate.optimism_corrected.threshold_0.06)
summary(statistics$true_negative_rate.app.threshold_0.07)
summary(statistics$true_negative_rate.optimism_corrected.threshold_0.07)

summary(statistics$positive_predictive_value.app.threshold_0.03)
summary(statistics$positive_predictive_value.optimism_corrected.threshold_0.03)
summary(statistics$positive_predictive_value.app.threshold_0.04)
summary(statistics$positive_predictive_value.optimism_corrected.threshold_0.04)
summary(statistics$positive_predictive_value.app.threshold_0.05)
summary(statistics$positive_predictive_value.optimism_corrected.threshold_0.05)
summary(statistics$positive_predictive_value.app.threshold_0.06)
summary(statistics$positive_predictive_value.optimism_corrected.threshold_0.06)
summary(statistics$positive_predictive_value.app.threshold_0.07)
summary(statistics$positive_predictive_value.optimism_corrected.threshold_0.07)

summary(statistics$true_negative_rate.app.threshold_0.03)
summary(statistics$true_negative_rate.optimism_corrected.threshold_0.03)
summary(statistics$true_negative_rate.app.threshold_0.04)
summary(statistics$true_negative_rate.optimism_corrected.threshold_0.04)
summary(statistics$true_negative_rate.app.threshold_0.05)
summary(statistics$true_negative_rate.optimism_corrected.threshold_0.05)
summary(statistics$true_negative_rate.app.threshold_0.06)
summary(statistics$true_negative_rate.optimism_corrected.threshold_0.06)
summary(statistics$true_negative_rate.app.threshold_0.07)
summary(statistics$true_negative_rate.optimism_corrected.threshold_0.07)

confusion_matrix_based_statistics.threshold_0.03 <- statistics[, c("index",
                                                                   "accuracy.app.threshold_0.03",
                                                                   "accuracy.optimism_corrected.threshold_0.03",
                                                                   "true_positive_rate.app.threshold_0.03",
                                                                   "true_positive_rate.optimism_corrected.threshold_0.03",
                                                                   "true_negative_rate.app.threshold_0.03",
                                                                   "true_negative_rate.optimism_corrected.threshold_0.03",
                                                                   "positive_predictive_value.app.threshold_0.03",
                                                                   "positive_predictive_value.optimism_corrected.threshold_0.03",
                                                                   "negative_predictive_value.app.threshold_0.03",
                                                                   "negative_predictive_value.optimism_corrected.threshold_0.03")]
ggplot(confusion_matrix_based_statistics.threshold_0.03, aes(x = index)) +
  geom_point(aes(y = accuracy.optimism_corrected.threshold_0.03, colour = "1")) +
  geom_point(aes(y = true_positive_rate.optimism_corrected.threshold_0.03, colour = "2")) +
  scale_x_continuous(name = "Model",
                     breaks = seq(1, 33, 1),
                     labels = seq(1, 33, 1)) +
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue", 
                                 "2" = "forestgreen"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected recall")) +
  geom_hline(yintercept = 0.85, colour = "skyblue") +
  geom_hline(yintercept = 0.75, colour = "forestgreen") +
  ggtitle("Threshold = 0.03") +
  theme_minimal()
ggplot(confusion_matrix_based_statistics.threshold_0.03, aes(x = index)) +
  geom_point(aes(y = accuracy.optimism_corrected.threshold_0.03, colour = "1")) +
  geom_point(aes(y = true_positive_rate.optimism_corrected.threshold_0.03, colour = "2")) +
  geom_point(aes(y = true_negative_rate.optimism_corrected.threshold_0.03, colour = "3")) +
  geom_point(aes(y = positive_predictive_value.optimism_corrected.threshold_0.03, colour = "4")) +
  geom_point(aes(y = negative_predictive_value.optimism_corrected.threshold_0.03, colour = "5")) +
  scale_x_continuous(name = "Model",
                     breaks = seq(1, 33, 1),
                     labels = seq(1, 33, 1)) +
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue",
                                 "2" = "forestgreen",
                                 "3" = "palegreen3",
                                 "4" = "red3",
                                 "5" = "pink"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected true positive rate",
                                 "3" = "Optimism-corrected true negative rate",
                                 "4" = "Optimism-corrected positive predictive value",
                                 "5" = "Optimism-corrected negative predictive value")) +
  ggtitle("Threshold = 0.03") +
  theme_minimal()

confusion_matrix_based_statistics.threshold_0.04 <- statistics[, c("index",
                                                                   "accuracy.app.threshold_0.04",
                                                                   "accuracy.optimism_corrected.threshold_0.04",
                                                                   "true_positive_rate.app.threshold_0.04",
                                                                   "true_positive_rate.optimism_corrected.threshold_0.04",
                                                                   "true_negative_rate.app.threshold_0.04",
                                                                   "true_negative_rate.optimism_corrected.threshold_0.04",
                                                                   "positive_predictive_value.app.threshold_0.04",
                                                                   "positive_predictive_value.optimism_corrected.threshold_0.04",
                                                                   "negative_predictive_value.app.threshold_0.04",
                                                                   "negative_predictive_value.optimism_corrected.threshold_0.04")]
ggplot(confusion_matrix_based_statistics.threshold_0.04, aes(x = index)) +
  geom_point(aes(y = accuracy.optimism_corrected.threshold_0.04, colour = "1")) +
  geom_point(aes(y = true_positive_rate.optimism_corrected.threshold_0.04, colour = "2")) +
  scale_x_continuous(name = "Model",
                     breaks = seq(1, 33, 1),
                     labels = seq(1, 33, 1)) +
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue", 
                                 "2" = "forestgreen"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected recall")) +
  geom_hline(yintercept = 0.85, colour = "skyblue") +
  geom_hline(yintercept = 0.75, colour = "forestgreen") +
  ggtitle("Threshold = 0.04") +
  theme_minimal()
ggplot(confusion_matrix_based_statistics.threshold_0.04, aes(x = index)) +
  geom_point(aes(y = accuracy.optimism_corrected.threshold_0.04, colour = "1")) +
  geom_point(aes(y = true_positive_rate.optimism_corrected.threshold_0.04, colour = "2")) +
  geom_point(aes(y = true_negative_rate.optimism_corrected.threshold_0.04, colour = "3")) +
  geom_point(aes(y = positive_predictive_value.optimism_corrected.threshold_0.04, colour = "4")) +
  geom_point(aes(y = negative_predictive_value.optimism_corrected.threshold_0.04, colour = "5")) +
  scale_x_continuous(name = "Model",
                     breaks = seq(1, 33, 1),
                     labels = seq(1, 33, 1)) +
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue",
                                 "2" = "forestgreen",
                                 "3" = "palegreen3",
                                 "4" = "red3",
                                 "5" = "pink"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected true positive rate",
                                 "3" = "Optimism-corrected true negative rate",
                                 "4" = "Optimism-corrected positive predictive value",
                                 "5" = "Optimism-corrected negative predictive value")) +
  ggtitle("Threshold = 0.04") +
  theme_minimal()

confusion_matrix_based_statistics.threshold_0.05 <- statistics[, c("index",
                                                                   "accuracy.app.threshold_0.05",
                                                                   "accuracy.optimism_corrected.threshold_0.05",
                                                                   "true_positive_rate.app.threshold_0.05",
                                                                   "true_positive_rate.optimism_corrected.threshold_0.05",
                                                                   "true_negative_rate.app.threshold_0.05",
                                                                   "true_negative_rate.optimism_corrected.threshold_0.05",
                                                                   "positive_predictive_value.app.threshold_0.05",
                                                                   "positive_predictive_value.optimism_corrected.threshold_0.05",
                                                                   "negative_predictive_value.app.threshold_0.05",
                                                                   "negative_predictive_value.optimism_corrected.threshold_0.05")]
ggplot(confusion_matrix_based_statistics.threshold_0.05, aes(x = index)) +
  geom_point(aes(y = accuracy.optimism_corrected.threshold_0.05, colour = "1")) +
  geom_point(aes(y = true_positive_rate.optimism_corrected.threshold_0.05, colour = "2")) +
  scale_x_continuous(name = "Model",
                     breaks = seq(1, 33, 1),
                     labels = seq(1, 33, 1)) +
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue", 
                                 "2" = "forestgreen"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected recall")) +
  geom_hline(yintercept = 0.85, colour = "skyblue") +
  geom_hline(yintercept = 0.75, colour = "forestgreen") +
  ggtitle("Threshold = 0.05") +
  theme_minimal()
ggplot(confusion_matrix_based_statistics.threshold_0.05, aes(x = index)) +
  geom_point(aes(y = accuracy.optimism_corrected.threshold_0.05, colour = "1")) +
  geom_point(aes(y = true_positive_rate.optimism_corrected.threshold_0.05, colour = "2")) +
  geom_point(aes(y = true_negative_rate.optimism_corrected.threshold_0.05, colour = "3")) +
  geom_point(aes(y = positive_predictive_value.optimism_corrected.threshold_0.05, colour = "4")) +
  geom_point(aes(y = negative_predictive_value.optimism_corrected.threshold_0.05, colour = "5")) +
  scale_x_continuous(name = "Model",
                     breaks = seq(1, 33, 1),
                     labels = seq(1, 33, 1)) +
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue",
                                 "2" = "forestgreen",
                                 "3" = "palegreen3",
                                 "4" = "red3",
                                 "5" = "pink"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected true positive rate",
                                 "3" = "Optimism-corrected true negative rate",
                                 "4" = "Optimism-corrected positive predictive value",
                                 "5" = "Optimism-corrected negative predictive value")) +
  ggtitle("Threshold = 0.05") +
  theme_minimal()

confusion_matrix_based_statistics.threshold_0.06 <- statistics[, c("index",
                                                                   "accuracy.app.threshold_0.06",
                                                                   "accuracy.optimism_corrected.threshold_0.06",
                                                                   "true_positive_rate.app.threshold_0.06",
                                                                   "true_positive_rate.optimism_corrected.threshold_0.06",
                                                                   "true_negative_rate.app.threshold_0.06",
                                                                   "true_negative_rate.optimism_corrected.threshold_0.06",
                                                                   "positive_predictive_value.app.threshold_0.06",
                                                                   "positive_predictive_value.optimism_corrected.threshold_0.06",
                                                                   "negative_predictive_value.app.threshold_0.06",
                                                                   "negative_predictive_value.optimism_corrected.threshold_0.06")]
ggplot(confusion_matrix_based_statistics.threshold_0.06, aes(x = index)) +
  geom_point(aes(y = accuracy.optimism_corrected.threshold_0.06, colour = "1")) +
  geom_point(aes(y = true_positive_rate.optimism_corrected.threshold_0.06, colour = "2")) +
  scale_x_continuous(name = "Model",
                     breaks = seq(1, 33, 1),
                     labels = seq(1, 33, 1)) +
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue", 
                                 "2" = "forestgreen"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected recall")) +
  geom_hline(yintercept = 0.85, colour = "skyblue") +
  geom_hline(yintercept = 0.75, colour = "forestgreen") +
  ggtitle("Threshold = 0.06") +
  theme_minimal()
ggplot(confusion_matrix_based_statistics.threshold_0.06, aes(x = index)) +
  geom_point(aes(y = accuracy.optimism_corrected.threshold_0.06, colour = "1")) +
  geom_point(aes(y = true_positive_rate.optimism_corrected.threshold_0.06, colour = "2")) +
  geom_point(aes(y = true_negative_rate.optimism_corrected.threshold_0.06, colour = "3")) +
  geom_point(aes(y = positive_predictive_value.optimism_corrected.threshold_0.06, colour = "4")) +
  geom_point(aes(y = negative_predictive_value.optimism_corrected.threshold_0.06, colour = "5")) +
  scale_x_continuous(name = "Model",
                     breaks = seq(1, 33, 1),
                     labels = seq(1, 33, 1)) +
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue",
                                 "2" = "forestgreen",
                                 "3" = "palegreen3",
                                 "4" = "red3",
                                 "5" = "pink"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected true positive rate",
                                 "3" = "Optimism-corrected true negative rate",
                                 "4" = "Optimism-corrected positive predictive value",
                                 "5" = "Optimism-corrected negative predictive value")) +
  ggtitle("Threshold = 0.06") +
  theme_minimal()

confusion_matrix_based_statistics.threshold_0.07 <- statistics[, c("index",
                                                                   "accuracy.app.threshold_0.07",
                                                                   "accuracy.optimism_corrected.threshold_0.07",
                                                                   "true_positive_rate.app.threshold_0.07",
                                                                   "true_positive_rate.optimism_corrected.threshold_0.07",
                                                                   "true_negative_rate.app.threshold_0.07",
                                                                   "true_negative_rate.optimism_corrected.threshold_0.07",
                                                                   "positive_predictive_value.app.threshold_0.07",
                                                                   "positive_predictive_value.optimism_corrected.threshold_0.07",
                                                                   "negative_predictive_value.app.threshold_0.07",
                                                                   "negative_predictive_value.optimism_corrected.threshold_0.07")]
ggplot(confusion_matrix_based_statistics.threshold_0.07, aes(x = index)) +
  geom_point(aes(y = accuracy.optimism_corrected.threshold_0.07, colour = "1")) +
  geom_point(aes(y = true_positive_rate.optimism_corrected.threshold_0.07, colour = "2")) +
  scale_x_continuous(name = "Model",
                     breaks = seq(1, 33, 1),
                     labels = seq(1, 33, 1)) +
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue", 
                                 "2" = "forestgreen"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected recall")) +
  geom_hline(yintercept = 0.85, colour = "skyblue") +
  geom_hline(yintercept = 0.75, colour = "forestgreen") +
  ggtitle("Threshold = 0.07") +
  theme_minimal()
ggplot(confusion_matrix_based_statistics.threshold_0.07, aes(x = index)) +
  geom_point(aes(y = accuracy.optimism_corrected.threshold_0.07, colour = "1")) +
  geom_point(aes(y = true_positive_rate.optimism_corrected.threshold_0.07, colour = "2")) +
  geom_point(aes(y = true_negative_rate.optimism_corrected.threshold_0.07, colour = "3")) +
  geom_point(aes(y = positive_predictive_value.optimism_corrected.threshold_0.07, colour = "4")) +
  geom_point(aes(y = negative_predictive_value.optimism_corrected.threshold_0.07, colour = "5")) +
  scale_x_continuous(name = "Model",
                     breaks = seq(1, 33, 1),
                     labels = seq(1, 33, 1)) +
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue",
                                 "2" = "forestgreen",
                                 "3" = "palegreen3",
                                 "4" = "red3",
                                 "5" = "pink"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected true positive rate",
                                 "3" = "Optimism-corrected true negative rate",
                                 "4" = "Optimism-corrected positive predictive value",
                                 "5" = "Optimism-corrected negative predictive value")) +
  ggtitle("Threshold = 0.07") +
  theme_minimal()

#

plot(anova(fit.m19))

pred.prob <- predict(fit.m19, data, type = "fitted.ind")

table(pred.prob > 0.05, exclude = NULL)
prop.table(table(pred.prob > 0.05, exclude = NULL))

ggplot(as.data.frame(pred.prob), aes(x = pred.prob)) +
  geom_histogram() + 
  theme_minimal()

#

ggplot(statistics, aes(x = seq(0.03, 0.07, 0.01))) +
  geom_point(aes(x = 0.03, y = subset(statistics, index == 19)$accuracy.optimism_corrected.threshold_0.03, colour = "1")) +
  geom_point(aes(x = 0.03, y = subset(statistics, index == 19)$true_positive_rate.optimism_corrected.threshold_0.03, colour = "2")) +
  geom_point(aes(x = 0.03, y = subset(statistics, index == 19)$true_negative_rate.optimism_corrected.threshold_0.03, colour = "3")) +
  geom_point(aes(x = 0.03, y = subset(statistics, index == 19)$positive_predictive_value.optimism_corrected.threshold_0.03, colour = "4")) +
  geom_point(aes(x = 0.03, y = subset(statistics, index == 19)$negative_predictive_value.optimism_corrected.threshold_0.03, colour = "5")) +
  geom_point(aes(x = 0.04, y = subset(statistics, index == 19)$accuracy.optimism_corrected.threshold_0.04, colour = "1")) +
  geom_point(aes(x = 0.04, y = subset(statistics, index == 19)$true_positive_rate.optimism_corrected.threshold_0.04, colour = "2")) +
  geom_point(aes(x = 0.04, y = subset(statistics, index == 19)$true_negative_rate.optimism_corrected.threshold_0.04, colour = "3")) +
  geom_point(aes(x = 0.04, y = subset(statistics, index == 19)$positive_predictive_value.optimism_corrected.threshold_0.04, colour = "4")) +
  geom_point(aes(x = 0.04, y = subset(statistics, index == 19)$negative_predictive_value.optimism_corrected.threshold_0.04, colour = "5")) +
  geom_point(aes(x = 0.05, y = subset(statistics, index == 19)$accuracy.optimism_corrected.threshold_0.05, colour = "1")) +
  geom_point(aes(x = 0.05, y = subset(statistics, index == 19)$true_positive_rate.optimism_corrected.threshold_0.05, colour = "2")) +
  geom_point(aes(x = 0.05, y = subset(statistics, index == 19)$true_negative_rate.optimism_corrected.threshold_0.05, colour = "3")) +
  geom_point(aes(x = 0.05, y = subset(statistics, index == 19)$positive_predictive_value.optimism_corrected.threshold_0.05, colour = "4")) +
  geom_point(aes(x = 0.05, y = subset(statistics, index == 19)$negative_predictive_value.optimism_corrected.threshold_0.06, colour = "5")) +
  geom_point(aes(x = 0.06, y = subset(statistics, index == 19)$accuracy.optimism_corrected.threshold_0.06, colour = "1")) +
  geom_point(aes(x = 0.06, y = subset(statistics, index == 19)$true_positive_rate.optimism_corrected.threshold_0.06, colour = "2")) +
  geom_point(aes(x = 0.06, y = subset(statistics, index == 19)$true_negative_rate.optimism_corrected.threshold_0.06, colour = "3")) +
  geom_point(aes(x = 0.06, y = subset(statistics, index == 19)$positive_predictive_value.optimism_corrected.threshold_0.06, colour = "4")) +
  geom_point(aes(x = 0.06, y = subset(statistics, index == 19)$negative_predictive_value.optimism_corrected.threshold_0.06, colour = "5")) +
  geom_point(aes(x = 0.07, y = subset(statistics, index == 19)$accuracy.optimism_corrected.threshold_0.07, colour = "1")) +
  geom_point(aes(x = 0.07, y = subset(statistics, index == 19)$true_positive_rate.optimism_corrected.threshold_0.07, colour = "2")) +
  geom_point(aes(x = 0.07, y = subset(statistics, index == 19)$true_negative_rate.optimism_corrected.threshold_0.07, colour = "3")) +
  geom_point(aes(x = 0.07, y = subset(statistics, index == 19)$positive_predictive_value.optimism_corrected.threshold_0.07, colour = "4")) +
  geom_point(aes(x = 0.07, y = subset(statistics, index == 19)$negative_predictive_value.optimism_corrected.threshold_0.07, colour = "5")) +
  scale_x_continuous(name = "Threshold",
                     breaks = seq(0.03, 0.07, 0.01),
                     labels = seq(0.03, 0.07, 0.01))+
  scale_y_continuous(name = NULL, 
                     limits = c(0, 1),
                     breaks = seq(0.00, 1.00, 0.05),
                     labels = format(round(seq(0.00, 1.00, 0.05), 2))) +
  scale_colour_manual(name = NULL, 
                      values = c("1" = "skyblue",
                                 "2" = "forestgreen",
                                 "3" = "palegreen3",
                                 "4" = "red3",
                                 "5" = "pink"),
                      labels = c("1" = "Optimism-corrected accuracy", 
                                 "2" = "Optimism-corrected true positive rate",
                                 "3" = "Optimism-corrected true negative rate",
                                 "4" = "Optimism-corrected positive predictive value",
                                 "5" = "Optimism-corrected negative predictive value")) +
  theme_minimal()

#

table(data$medication_prescribed_during_consult.yes_1, exclude = NULL)
table(data$medication_prescribed_during_consult.new_1, exclude = NULL)
table(data$medication_prescribed_during_consult.yes_1, data$medication_prescribed_during_consult.new_1, exclude = NULL)
table(data$medication_stopped_during_consult.yes_1, exclude = NULL)
table(data$medication_prescribed_during_consult.yes_1, data$medication_stopped_during_consult.yes_1, exclude = NULL)

data$medication_corrected_during_consult.yes_1 <- ifelse( (data$medication_prescribed_during_consult.yes_1 == 1)
                                                         |
                                                         (data$medication_stopped_during_consult.yes_1 == 1),
                                                         1,
                                                         0 )

table(data$medication_corrected_during_consult.yes_1, exclude = NULL)

table(data$medication_corrected_during_consult.yes_1, data$y, exclude = NULL)

## Assuming NA equals no:
### true positive = 62
### true negative = 1067 + 369 = 1431
### false positive = 427
### false negative = 17 + 4 = 21
### accuracy = (62 + 1431) / (62 + 1431 + 427 + 21) = 0.7692
### true positive rate = 62 / (62 + 21) = 0.7470
### true negative rate = 1431 / (1431 + 427) = 0.7702
### positive predictive value = 62 / (62 + 427) = 0.1268
### negative predictive value = 1431 / (1431 + 21) = 0.9855

#### Partly or completely post medication reconciliation?

#

table(data$age >= 70, exclude = NULL)
table(data$n_prescribed_medications >= 5, exclude = NULL)

data$decision_rule.age_70_or_n_prescribed_medications_5 <- ifelse( (data$age >= 70)
                                                                   |
                                                                   (data$n_prescribed_medications >= 5),
                                                                   1,
                                                                   0)
table(data$decision_rule.age_70_or_n_prescribed_medications_5, exclude = NULL)

table(data$decision_rule.age_70_or_n_prescribed_medications_5, data$y, exclude = NULL)

## true positive = 72
## true negative = 676
## false positive = 1187
## false negative = 11
## accuracy = (72 + 11) / (72 + 11 + 676 + 1187) = 0.04265159
## true positive rate = 72 / (72 + 11) = 0.8675
## true negative rate = 676 / (676 + 1187) = 0.3629
## positive predictive value = 72 / (72 + 1187) = 0.0571
## negative predictive value = 676 / (676 + 11) = 0.9839

statistics["m19", "accuracy.optimism_corrected.threshold_0.05"]
statistics["m19", "true_positive_rate.optimism_corrected.threshold_0.05"]
statistics["m19", "true_negative_rate.optimism_corrected.threshold_0.05"]
statistics["m19", "positive_predictive_value.optimism_corrected.threshold_0.05"]
statistics["m19", "negative_predictive_value.optimism_corrected.threshold_0.05"]

#

boot.fit.m19.confusion_matrix_statistics <- 
  fun.confusion_matrix_statistics(model_formula = "y ~ 
                                                n_prescribed_medications + 
                                                high_risk_medications.yes_1 +
                                                age +
                                                inout_specialty_current_12m_MUMC.multiple_1",
                                  data = data,
                                  n_bootstraps = 1000, 
                                  threshold.lb = 0.01, 
                                  threshold.ub = 0.07,
                                  threshold.increment_unit = 0.01)

View(as.data.frame(boot.fit.m19.confusion_matrix_statistics$confusion_matrix_statistics))
