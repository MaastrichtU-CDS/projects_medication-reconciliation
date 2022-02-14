
source("C:\\Users\\P70070766\\Documents\\Medication reconciliation\\Scripts\\20220212 - Function - Confusion matrix based statistics with optimism correction.R")
library(rms)
library(caret)

x1 <- rnorm(n = 100, mean = 0, sd = 1)
x2 <- rnorm(n = 100, mean = 0, sd = 1)
error <- rnorm(n = 100, mean = 0, sd = 0.5)
ln_odds <- 2*x1 + 3*x2 + error
y <- ifelse(runif(100) < plogis(ln_odds), 1, 0)
data <- as.data.frame(cbind(y, x1, x2))

fit.rms <- lrm(y ~ x1 + x2,
               data = data)
fit.rms.pred_prob <- predict(fit.rms, data, type = "fitted.ind")
histogram(fit.rms.pred_prob)

statistics <- fun.confusion_matrix_statistics(model_formula =  "y ~ x1 + x2",
                                              data = data,
                                              n_bootstraps = 50,
                                              threshold.lb = 0.06,
                                              threshold.ub = 0.08,
                                              threshold.increment_unit = 0.01)
statistics$confusion_matrix_statistics
                                
fit.glm <- glm(y ~ x1 + x2,
               data = data,
               family = "binomial")

fit.glm.pred_prob <- predict(fit.glm, data, type = "response")
histogram(fit.glm.pred_prob)

y <- as.factor(data$y)

fit.glm.pred_class.threshold_0.06 <- ifelse(fit.glm.pred_prob >= 0.06, 1, 0)
fit.glm.pred_class.threshold_0.06 <- as.factor(fit.glm.pred_class.threshold_0.06)
confusionMatrix(fit.glm.pred_class.threshold_0.06, y, positive = "1")

fit.glm.pred_class.threshold_0.07 <- ifelse(fit.glm.pred_prob >= 0.07, 1, 0)
fit.glm.pred_class.threshold_0.07 <- as.factor(fit.glm.pred_class.threshold_0.07)
confusionMatrix(fit.glm.pred_class.threshold_0.07, y, positive = "1")

fit.glm.pred_class.threshold_0.08 <- ifelse(fit.glm.pred_prob >= 0.08, 1, 0)
fit.glm.pred_class.threshold_0.08 <- as.factor(fit.glm.pred_class.threshold_0.08)
confusionMatrix(fit.glm.pred_class.threshold_0.08, y, positive = "1")
