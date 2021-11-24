library(pROC)
# Preprocessed data
source("Preprocessing.R")
rm(list = setdiff(ls(), "data"))

# Packages
library(caret)

# Data model
data.m <- data[, c("id",
                   "age",
                   "housing.home_care_1",
                   "n_diseases",
                   "out_specialty_current_MUMC.cardiology_1",
                   "out_specialty_current_MUMC.surgery_1",
                   "out_specialty_current_MUMC.dermatology_1",
                   "n_medications",
                   "high_risk_medications.yes_1",
                   "n_visits_out_current_specialty_12m_MUMC.cat",
                   "y")]

#Preprocess value

data.m$age_gt_65 <- ifelse(data.m$age > 65, 1, 0)
data.m$n_diseases <- ifelse(is.na(data$n_diseases), 0, data$n_diseases)
data.m$high_risk_medications.anticoagulants_or_psychotropics <- ifelse(data$high_risk_medications.anticoagulants_1 + data$high_risk_medications.psychotropics_1>0,1,0)
data.m$n_visits_out_current_specialty_12m_MUMC_3_or_more <- ifelse(data.m$n_visits_out_current_specialty_12m_MUMC.cat == "3+", 1, 0)
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
               age_gt_65 +
               n_diseases +
               housing.home_care_1 +
               out_specialty_current_MUMC.cardiology_1 +
               out_specialty_current_MUMC.surgery_1 +
               out_specialty_current_MUMC.dermatology_1 +
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

customSummary <- function (data, lev = NULL, model = NULL){
  threshold <- 0.085 #change threshold here
  pred <- factor(ifelse(data[, "event"] > threshold, "1", "0"), levels = c("0","1"), labels = c("no_event", "event"))
  obs <- data[, "obs"]
  sens <- caret::sensitivity(pred, obs, positive = "event")
  acc <- sum(as.character(pred) == as.character(obs)) / nrow(data) 
  prec <- caret::precision(data = pred, reference = obs, relevant = "event")
  out <- c(sens, acc, prec)
  names(out) <- c("Sensitivity", "Accuracy", "Precision")
  out
}


fit.control.m <- trainControl(method = "repeatedcv",
                               number = k,
                               repeats = r,
                              classProbs=TRUE,
                              summaryFunction=customSummary
#                              summaryFunction=twoClassSummary
)

results <- data.frame(matrix(nrow = 0, ncol = 8))
sample_sizes <-  c(500, 750, 1000, 1200, 1400, 1526)
for(sample_size in sample_sizes){
  population_sample <- data.m[1:sample_size,]
  population_sample <- population_sample[sample(sample_size),]
  
  if(sample_size == 500){
    fit.m <- train(y ~ 
                     age_gt_65 +
                     housing.home_care_1 +
                     out_specialty_current_MUMC.cardiology_1,
                   data = population_sample,
                   method = "glm",
                   family = "binomial",
                   trControl = fit.control.m) 
  } else if (sample_size == 750)
  {
    fit.m <- train(y ~ 
                     age_gt_65 +
                     n_medications +
                     housing.home_care_1 +
                     out_specialty_current_MUMC.cardiology_1,
                   data = population_sample,
                   method = "glm",
                   family = "binomial",
                   trControl = fit.control.m) 
    
  } else if (sample_size == 1000)
  {
    fit.m <- train(y ~ 
                     age_gt_65 +
                     n_medications +
                     housing.home_care_1 +
                     out_specialty_current_MUMC.cardiology_1 +
                     high_risk_medications.anticoagulants_or_psychotropics,
                   data = population_sample,
                   method = "glm",
                   family = "binomial",
                   trControl = fit.control.m) 
  }else if (sample_size == 1200)
  {
    fit.m <- train(y ~ 
                     age_gt_65 +
                     n_medications +
                     n_diseases +
                     housing.home_care_1 +
                     out_specialty_current_MUMC.cardiology_1 +
                     high_risk_medications.anticoagulants_or_psychotropics,
                   data = population_sample,
                   method = "glm",
                   family = "binomial",
                   trControl = fit.control.m) 
  }else if (sample_size == 1400)
  {
    fit.m <- train(y ~ 
                     age_gt_65 +
                     n_medications +
                     n_diseases +
                     housing.home_care_1 +
                     out_specialty_current_MUMC.cardiology_1 +
                     out_specialty_current_MUMC.surgery_1 +
                     high_risk_medications.anticoagulants_or_psychotropics,
                   data = population_sample,
                   method = "glm",
                   family = "binomial",
                   trControl = fit.control.m) 
  }else 
  {
    fit.m <- train(y ~ 
                     age_gt_65 +
                     n_medications +
                     n_diseases +
                     housing.home_care_1 +
                     out_specialty_current_MUMC.cardiology_1 +
                     out_specialty_current_MUMC.surgery_1 +
                     out_specialty_current_MUMC.dermatology_1 +
                     n_visits_out_current_specialty_12m_MUMC_3_or_more +
                     high_risk_medications.anticoagulants_or_psychotropics,
                   data = population_sample,
                   method = "glm",
                   family = "binomial",
                   trControl = fit.control.m) 
  }
  results <-  rbind(results,cbind(data.frame("sample_size" = c(sample_size)), fit.m$results))
}

plot(results$sample_size, results$Sensitivity,
     ylim=range(c(0, 1)),
     pch=19, xlab="Sample size", ylab="Mean +/- SD",
     main="Sensitivity for different sample sizes"
)
lines(results$sample_size, results$Sensitivity, pch=16)
# hack: we draw arrows but with very special "arrowheads"
arrows(results$sample_size, results$Sensitivity-results$SensitivitySD, results$sample_size, results$Sensitivity+results$SensitivitySD, length=0.05, angle=90, code=3)

fit.m$results

summary(fit.m)

pred.prob.m <- predict(fit.m, data.m, type = "prob")

caret::sensitivity(pred_event, data.m$y)

pred_event <- factor(ifelse(pred.prob.m$event > 0.15, 1, 0),levels = c("0","1"), labels = c("no_event", "event"))

pred.prob.rounded <- as.integer(pred.prob.m$event * 100) / 100 

y.pred.prob.event.m <- as.data.frame(cbind(data.m$y, pred.prob.m$event, pred.prob.rounded))
colnames(y.pred.prob.event.m) <- c("y", "pred.prob.event", "pred.prob.event.rounded")

hist(pred.prob.m$event, breaks = 40)

# Histogram
ggplot(y.pred.prob.event.m, aes(x = pred.prob.event, fill = as.factor(y))) +
  geom_histogram(colour = "black", bins = 40) +
  scale_fill_manual(name = "y", values = c("1" = "deepskyblue4", "2" = "red3"), labels = c("no event", "event")) +
  theme_minimal()

# 100% stacked bar
ggplot() + 
  geom_bar(data = y.pred.prob.event.m,
         aes(x = factor(pred.prob.event.rounded),fill = factor(y)),
         position = "fill") +
  scale_fill_manual(name = "y", values = c("1" = "deepskyblue4", "2" = "red3"), labels = c("no event", "event"))


# ROC curve
roc_obj <- roc(data.m$y, pred.prob.m$event)

plot(roc_obj)

# Calculate the area under the curve (AUC)
auc(roc_obj)

confusionMatrix.train(fit.m)
print(confusionMatrix.train(fit.m)$table)