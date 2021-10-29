# Preprocessed data
source("C:\\Users\\P70070766\\Documents\\Medication reconciliation\\Scripts\\20211027 - Preprocessing.R")
rm(list = setdiff(ls(), "data"))

# Packages
library(caret)

# Data model
data.m <- data[, c("id",
                   "age",
                   "housing.independent_1",
                   "out_specialty_current_MUMC.cardiology_1",
                   "out_specialty_current_MUMC.surgery_composite_1",
                   "out_specialty_current_MUMC.internal_medicine_composite_1",
                   "out_specialty_current_MUMC.neurology_1",
                   "n_medications",
                   "high_risk_medications.yes_1",
                   "y")]
data.m$y <- factor(data.m$y,
                   levels = c("0",
                              "1"),
                    labels = c("no_event",
                               "event"))

## Missing data
### unknown housing -> reference category
### unknown number of medications -> 0 ? 

# Model

fit.m <- glm(y ~ 
               age +
               housing.independent_1 +
               out_specialty_current_MUMC.cardiology_1 +
               out_specialty_current_MUMC.surgery_composite_1 +
               out_specialty_current_MUMC.internal_medicine_composite_1 +
               out_specialty_current_MUMC.neurology_1 +
               n_medications +
               high_risk_medications.yes_1,
             data = data.m,
             family = "binomial") # specialties with few patients (dermatology, rheumatology: SPA, rheumatology, urology: functional, urology: oncology) as reference category
fit.m
confint(fit.m)
summary(fit.m)


# 10x10 cv
k <- 10 # number of folds
r <- 10 # number of repeats

fit.control.m <- trainControl(method = "repeatedcv",
                               number = k,
                               repeats = r)

fit.m <- train(y ~ 
                 age +
                 housing.independent_1 +
                 out_specialty_current_MUMC.cardiology_1 +
                 out_specialty_current_MUMC.surgery_composite_1 +
                 out_specialty_current_MUMC.internal_medicine_composite_1 +
                 out_specialty_current_MUMC.neurology_1 +
                 n_medications +
                 high_risk_medications.yes_1,
               data = data.m,
               method = "glm",
               family = "binomial",
               trControl = fit.control.m) # specialties with few patients (dermatology, rheumatology: SPA, rheumatology, urology: functional, urology: oncology) as reference category
fit.m
summary(fit.m)

pred.prob.m <- predict(fit.m, data.m, type = "prob")

y.pred.prob.event.m <- as.data.frame(cbind(data.m$y, pred.prob.m$event))
colnames(y.pred.prob.event.m) <- c("y", "pred.prob.event")

ggplot(y.pred.prob.event.m, aes(x = pred.prob.event, fill = as.factor(y))) +
  geom_histogram(colour = "black") +
  scale_fill_manual(name = "y", values = c("1" = "deepskyblue4", "2" = "red3"), labels = c("no event", "event")) +
  theme_minimal()

confusionMatrix.train(fit.m)
print(confusionMatrix.train(fit.m)$table)

accuracy <- matrix(nrow = (k*r), ncol = 99)
colnames(accuracy) <- c(paste("threshold", seq(1, 99, 1), "perc", sep = "_"))
sensitivity <- matrix(nrow = (k*r), ncol = 99)
colnames(sensitivity) <- c(paste("threshold", seq(1, 99, 1), "perc", sep = "_"))
specificity <- matrix(nrow = (k*r), ncol = 99)
colnames(specificity) <- c(paste("threshold", seq(1, 99, 1), "perc", sep = "_"))
balanced_accuracy <- matrix(nrow = (k*r), ncol = 99)
colnames(balanced_accuracy) <- c(paste("threshold", seq(1, 99, 1), "perc", sep = "_"))
for (i in 1:(k*r)){ 
  for (j in seq(1, 99, 1)) {
    data.train <- data.m[fit.m$control$index[[i]], ]
    data.test <- data.m[fit.m$control$indexOut[[i]], ]
    
    fit.train <- glm(y ~ 
                       age +
                       housing.independent_1 +
                       out_specialty_current_MUMC.cardiology_1 +
                       out_specialty_current_MUMC.surgery_composite_1 +
                       out_specialty_current_MUMC.internal_medicine_composite_1 +
                       out_specialty_current_MUMC.neurology_1 +
                       n_medications +
                       high_risk_medications.yes_1,
                     family = "binomial",
                     data = data.train)
    
    pred.prob.test <- predict(fit.train, data.test, type = "response")
    pred.class.test <- ifelse(pred.prob.test > (j * 10 ** -2), "event", "no_event")
    
    true_positive <- 
      sum(
        ifelse((data.test$y == "event")
               &
               (pred.class.test == "event"),
               1,
               0)
      )
    true_negative <- 
      sum(
        ifelse((data.test$y == "no_event")
               &
               (pred.class.test == "no_event"),
               1,
               0)
      )
    false_positive <- 
      sum(
        ifelse((data.test$y == "no_event")
               &
               (pred.class.test == "event"),
               1,
               0)
      )
    false_negative <- 
      sum(
        ifelse((data.test$y == "event")
               &
               (pred.class.test == "no_event"),
               1,
               0)
      )
    
    accuracy[i, j] <- (true_positive + true_negative) / (true_positive + true_negative + false_positive + false_negative)
    sensitivity[i, j] <- (true_positive) / (true_positive + false_negative)
    specificity[i, j] <- (true_negative) / (true_negative + false_positive)
    balanced_accuracy[i, j] <- (sensitivity[i, j] + specificity[i, j]) / 2
  }
}

test.sample_size <- as.data.frame(matrix(nrow = (k*r), ncol = 1))
colnames(test.sample_size) <- "test.sample_size"
for (a in 1:(k*r)) {
  test.sample_size[a, 1] <- nrow(data.m[fit.m$control$indexOut[[a]], ])
}

weights <- test.sample_size$test.sample_size / max(test.sample_size$test.sample_size)

View(accuracy)
table(is.na(accuracy))
summary(accuracy[,"threshold_50_perc"]) # Mean equals output confusionMatrix.train(fit.m).

accuracy_weighted_average_across_rep <- as.data.frame(matrix(nrow = 1, ncol = ncol(accuracy))) 
colnames(accuracy_weighted_average_across_rep) <- colnames(accuracy)
for (b in 1:ncol(accuracy)) {
  accuracy_weighted_average_across_rep[1, b] <- weighted.mean(accuracy[, b], w = weights, na.rm = FALSE)
}
accuracy_weighted_average_across_rep <- as.data.frame(t(accuracy_weighted_average_across_rep))
colnames(accuracy_weighted_average_across_rep) <- "accuracy_weighted_average_across_rep"
summary(accuracy_weighted_average_across_rep) 

ggplot(accuracy_weighted_average_across_rep, aes(x = seq(1, 99, 1))) +
  geom_point(aes(y = accuracy_weighted_average_across_rep)) +
  scale_x_continuous(name = "classification threshold (if predicted probability > threshold, prediction occurence of an event)",
                     breaks = seq(0, 100, 10),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  scale_y_continuous(name = "accuracy (weighted average)",
                     lim = c(0, 1),
                     breaks = c(seq(0, 100, 10)* 10 ** -2),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  theme_minimal()

accuracy_unweighted_average_across_rep <- apply(accuracy, 2, mean)
accuracy_unweighted_average_across_rep <- as.data.frame(accuracy_unweighted_average_across_rep)
colnames(accuracy_unweighted_average_across_rep) <- "accuracy_unweighted_average_across_rep"
summary(accuracy_unweighted_average_across_rep)

ggplot(accuracy_unweighted_average_across_rep, aes(x = seq(1, 99, 1))) +
  geom_point(aes(y = accuracy_unweighted_average_across_rep)) +
  scale_x_continuous(name = "classification threshold (if predicted probability > threshold, prediction occurence of an event)",
                     breaks = seq(0, 100, 10),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  scale_y_continuous(name = "accuracy (unweighted average)",
                     lim = c(0, 1),
                     breaks = c(seq(0, 100, 10)* 10 ** -2),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  theme_minimal()

View(sensitivity)
table(is.na(sensitivity)) 

sensitivity_weighted_average_across_rep <- as.data.frame(matrix(nrow = 1, ncol = ncol(sensitivity))) 
colnames(sensitivity_weighted_average_across_rep) <- colnames(sensitivity)
for (b in 1:ncol(sensitivity)) {
  sensitivity_weighted_average_across_rep[1, b] <- weighted.mean(sensitivity[, b], w = weights, na.rm = FALSE)
}
sensitivity_weighted_average_across_rep <- as.data.frame(t(sensitivity_weighted_average_across_rep))
colnames(sensitivity_weighted_average_across_rep) <- "sensitivity_weighted_average_across_rep"
summary(sensitivity_weighted_average_across_rep) 

ggplot(sensitivity_weighted_average_across_rep, aes(x = seq(1, 99, 1))) +
  geom_point(aes(y = sensitivity_weighted_average_across_rep)) +
  scale_x_continuous(name = "classification threshold (if predicted probability > threshold, prediction occurence of an event)",
                     breaks = seq(0, 100, 10),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  scale_y_continuous(name = "sensitivity (weighted average)",
                     lim = c(0, 1),
                     breaks = c(seq(0, 100, 10)* 10 ** -2),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  theme_minimal()

sensitivity_unweighted_average_across_rep <- apply(sensitivity, 2, mean)
sensitivity_unweighted_average_across_rep <- as.data.frame(sensitivity_unweighted_average_across_rep)
colnames(sensitivity_unweighted_average_across_rep) <- "sensitivity_unweighted_average_across_rep"
summary(sensitivity_unweighted_average_across_rep)

ggplot(sensitivity_unweighted_average_across_rep, aes(x = seq(1, 99, 1))) +
  geom_point(aes(y = sensitivity_unweighted_average_across_rep)) +
  scale_x_continuous(name = "classification threshold (if predicted probability > threshold, prediction occurence of an event)",
                     breaks = seq(0, 100, 10),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  scale_y_continuous(name = "sensitivity (unweighted average)",
                     lim = c(0, 1),
                     breaks = c(seq(0, 100, 10)* 10 ** -2),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  theme_minimal()

View(specificity)
table(is.na(specificity))

specificity_weighted_average_across_rep <- as.data.frame(matrix(nrow = 1, ncol = ncol(specificity))) 
colnames(specificity_weighted_average_across_rep) <- colnames(specificity)
for (b in 1:ncol(specificity)) {
  specificity_weighted_average_across_rep[1, b] <- weighted.mean(specificity[, b], w = weights, na.rm = FALSE)
}
specificity_weighted_average_across_rep <- as.data.frame(t(specificity_weighted_average_across_rep))
colnames(specificity_weighted_average_across_rep) <- "specificity_weighted_average_across_rep"
summary(specificity_weighted_average_across_rep)  

ggplot(specificity_weighted_average_across_rep, aes(x = seq(1, 99, 1))) +
  geom_point(aes(y = specificity_weighted_average_across_rep)) +
  scale_x_continuous(name = "classification threshold (if predicted probability > threshold, prediction occurence of an event)",
                     breaks = seq(0, 100, 10),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  scale_y_continuous(name = "specificity (weighted average)",
                     lim = c(0, 1),
                     breaks = c(seq(0, 100, 10)* 10 ** -2),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  theme_minimal()

specificity_unweighted_average_across_rep <- apply(specificity, 2, mean)
specificity_unweighted_average_across_rep <- as.data.frame(specificity_unweighted_average_across_rep)
colnames(specificity_unweighted_average_across_rep) <- "specificity_unweighted_average_across_rep"
summary(specificity_unweighted_average_across_rep)

ggplot(specificity_unweighted_average_across_rep, aes(x = seq(1, 99, 1))) +
  geom_point(aes(y = specificity_unweighted_average_across_rep)) +
  scale_x_continuous(name = "classification threshold (if predicted probability > threshold, prediction occurence of an event)",
                     breaks = seq(0, 100, 10),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  scale_y_continuous(name = "specificity (unweighted average)",
                     lim = c(0, 1),
                     breaks = c(seq(0, 100, 10)* 10 ** -2),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  theme_minimal()

View(balanced_accuracy)
table(is.na(balanced_accuracy)) 

balanced_accuracy_weighted_average_across_rep <- as.data.frame(matrix(nrow = 1, ncol = ncol(balanced_accuracy))) 
colnames(balanced_accuracy_weighted_average_across_rep) <- colnames(balanced_accuracy)
for (b in 1:ncol(balanced_accuracy)) {
  balanced_accuracy_weighted_average_across_rep[1, b] <- weighted.mean(balanced_accuracy[, b], w = weights, na.rm = FALSE)
}
balanced_accuracy_weighted_average_across_rep <- as.data.frame(t(balanced_accuracy_weighted_average_across_rep))
colnames(balanced_accuracy_weighted_average_across_rep) <- "balanced_accuracy_weighted_average_across_rep"
summary(balanced_accuracy_weighted_average_across_rep) # View(balanced_accuracy_weighted_average_across_rep) 

ggplot(balanced_accuracy_weighted_average_across_rep, aes(x = seq(1, 99, 1))) +
  geom_point(aes(y = balanced_accuracy_weighted_average_across_rep)) +
  scale_x_continuous(name = "classification threshold (if predicted probability > threshold, prediction occurence of an event)",
                     breaks = seq(0, 100, 10),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  scale_y_continuous(name = "balanced_accuracy (weighted average)",
                     lim = c(0, 1),
                     breaks = c(seq(0, 100, 10)* 10 ** -2),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  theme_minimal()

balanced_accuracy_unweighted_average_across_rep <- apply(balanced_accuracy, 2, mean)
balanced_accuracy_unweighted_average_across_rep <- as.data.frame(balanced_accuracy_unweighted_average_across_rep)
colnames(balanced_accuracy_unweighted_average_across_rep) <- "balanced_accuracy_unweighted_average_across_rep"
summary(balanced_accuracy_unweighted_average_across_rep)

ggplot(balanced_accuracy_unweighted_average_across_rep, aes(x = seq(1, 99, 1))) +
  geom_point(aes(y = balanced_accuracy_unweighted_average_across_rep)) +
  scale_x_continuous(name = "classification threshold (if predicted probability > threshold, prediction occurence of an event)",
                     breaks = seq(0, 100, 10),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  scale_y_continuous(name = "balanced_accuracy (unweighted average)",
                     lim = c(0, 1),
                     breaks = c(seq(0, 100, 10)* 10 ** -2),
                     labels = sprintf("%0.2f", round((seq(0, 100, 10) * 10 ** -2), 3))) +
  theme_minimal()

