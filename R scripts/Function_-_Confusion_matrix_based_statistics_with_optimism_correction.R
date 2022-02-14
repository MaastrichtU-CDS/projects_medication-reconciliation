fun.confusion_matrix_statistics <- 
  function(model_formula, 
           data, 
           n_bootstraps, 
           threshold.lb, 
           threshold.ub,
           threshold.increment_unit) { 
    
    model_formula <- as.formula(model_formula)
    
    # 1. APPARENT PERFORMANCE ----
    
    accuracy.app <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)))
    rownames(accuracy.app) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    true_positive_rate.app <-  matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)))
    rownames(true_positive_rate.app) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    true_negative_rate.app <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)))
    rownames(true_negative_rate.app) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    positive_predictive_value.app <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)))
    rownames(positive_predictive_value.app) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    negative_predictive_value.app <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)))
    rownames(negative_predictive_value.app) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    
    for (i in seq(threshold.lb, threshold.ub, threshold.increment_unit)) {
      
      fit <- lrm(model_formula, data)
      
      pred.prob <- predict(fit, data, type = "fitted.ind")
      pred.class <- ifelse(pred.prob >= i, 1, 0)
      
      true_positive <- 
        sum(ifelse((data$y == 1)
                   &
                   (pred.class == 1),
                   1,
                   0))
                 
      true_negative <- 
        sum(ifelse((data$y == 0)
                   &
                   (pred.class == 0),
                   1,
                   0))
      false_positive <- 
        sum(ifelse((data$y == 0)
                   &
                   (pred.class == 1),
                   1,
                   0))
      false_negative <- 
        sum(ifelse((data$y == 1)
                   &
                   (pred.class == 0),
                   1,
                   0))
      
      accuracy.app[paste0("threshold_", i), ] <- 
        (true_positive + true_negative) / 
        (true_positive + true_negative + false_positive + false_negative) 
      true_positive_rate.app[paste0("threshold_", i), ] <- 
        (true_positive) / 
        (true_positive + false_negative)
      true_negative_rate.app[paste0("threshold_", i), ] <- 
        (true_negative) / 
        (true_negative + false_positive)
      positive_predictive_value.app[paste0("threshold_", i), ] <- 
        (true_positive) / 
        (true_positive + false_positive)
      negative_predictive_value.app[paste0("threshold_", i), ] <- 
        (true_negative) / 
        (true_negative + false_negative)
      }
    
    # 2. BOOTSTRAP ----
    
    accuracy.boot.app <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)), 
                                ncol = n_bootstraps)
    rownames(accuracy.boot.app) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    colnames(accuracy.boot.app) <- 1:n_bootstraps
    true_positive_rate.boot.app <-  matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)), 
                                           ncol = n_bootstraps)
    rownames(true_positive_rate.boot.app) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    colnames(true_positive_rate.boot.app) <- 1:n_bootstraps
    true_negative_rate.boot.app <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)), 
                                          ncol = n_bootstraps)
    rownames(true_negative_rate.boot.app) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    colnames(true_negative_rate.boot.app) <- 1:n_bootstraps
    positive_predictive_value.boot.app <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)), 
                                                 ncol = n_bootstraps)
    rownames(positive_predictive_value.boot.app) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    colnames(positive_predictive_value.boot.app) <- 1:n_bootstraps
    negative_predictive_value.boot.app <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)), 
                                                 ncol = n_bootstraps)
    rownames(negative_predictive_value.boot.app) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    colnames(negative_predictive_value.boot.app) <- 1:n_bootstraps
    
    accuracy.boot.test <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)), 
                                 ncol = n_bootstraps)
    rownames(accuracy.boot.test) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    colnames(accuracy.boot.test) <- 1:n_bootstraps
    true_positive_rate.boot.test <-  matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)), 
                                            ncol = n_bootstraps)
    rownames(true_positive_rate.boot.test) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    colnames(true_positive_rate.boot.test) <- 1:n_bootstraps
    true_negative_rate.boot.test <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)),
                                           ncol = n_bootstraps)
    rownames(true_negative_rate.boot.test) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    colnames(true_negative_rate.boot.test) <- 1:n_bootstraps
    positive_predictive_value.boot.test <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)),
                                                  ncol = n_bootstraps)
    rownames(positive_predictive_value.boot.test) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    colnames(positive_predictive_value.boot.test) <- 1:n_bootstraps
    negative_predictive_value.boot.test <- matrix(nrow = length(seq(threshold.lb, threshold.ub, threshold.increment_unit)),
                                                  ncol = n_bootstraps)
    rownames(negative_predictive_value.boot.test) <- paste0("threshold_", seq(threshold.lb, threshold.ub, threshold.increment_unit))
    colnames(negative_predictive_value.boot.test) <- 1:n_bootstraps
    
    for (b in 1:n_bootstraps) {
      
      index.boot <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
      data.boot <- data[index.boot, ]
      
      fit.boot <- lrm(model_formula, data = data.boot)
      
      for (i in seq(threshold.lb, threshold.ub, threshold.increment_unit)) {
        
        ## APPARENT PERFORMANCE
        
        pred.prob <- predict(fit.boot, data.boot, type = "fitted.ind")
        pred.class <- ifelse(pred.prob >= i, 1, 0)
        
        true_positive <- 
          sum(ifelse((data.boot$y == 1)
                     &
                     (pred.class == 1),
                     1,
                     0))
        true_negative <- 
          sum(ifelse((data.boot$y == 0)
                     &
                     (pred.class == 0),
                     1,
                     0))
        false_positive <- 
          sum(ifelse((data.boot$y == 0)
                     &
                     (pred.class == 1),
                     1,
                     0))
        false_negative <- 
          sum(ifelse((data.boot$y == 1)
                     &
                     (pred.class == 0),
                     1,
                     0))
        
        accuracy.boot.app[paste0("threshold_", i), b] <- 
          (true_positive + true_negative) / 
          (true_positive + true_negative + false_positive + false_negative) 
        true_positive_rate.boot.app[paste0("threshold_", i), b] <- 
          (true_positive) / 
          (true_positive + false_negative)
        true_negative_rate.boot.app[paste0("threshold_", i), b] <- 
          (true_negative) / 
          (true_negative + false_positive)
        positive_predictive_value.boot.app[paste0("threshold_", i), b] <- 
          (true_positive) / 
          (true_positive + false_positive)
        negative_predictive_value.boot.app[paste0("threshold_", i), b] <- 
          (true_negative) / 
          (true_negative + false_negative)
        
        ## TEST PERFORMANCE

        pred.prob <- predict(fit.boot, data, type = "fitted.ind")
        pred.class <- ifelse(pred.prob >= i, 1, 0)
        
        true_positive <- 
          sum(ifelse((data$y == 1)
                     &
                     (pred.class == 1),
                     1,
                     0))
        true_negative <- 
          sum(ifelse((data$y == 0)
                     &
                     (pred.class == 0),
                     1,
                     0))
        false_positive <- 
          sum(ifelse((data$y == 0)
                     &
                     (pred.class == 1),
                     1,
                     0))
        false_negative <- 
          sum(ifelse((data$y == 1)
                     &
                     (pred.class == 0),
                     1,
                     0))
        
        accuracy.boot.test[paste0("threshold_", i), b] <- 
          (true_positive + true_negative) / 
          (true_positive + true_negative + false_positive + false_negative) 
        true_positive_rate.boot.test[paste0("threshold_", i), b] <- 
          (true_positive) / 
          (true_positive + false_negative)
        true_negative_rate.boot.test[paste0("threshold_", i), b] <- 
          (true_negative) / 
          (true_negative + false_positive)
        positive_predictive_value.boot.test[paste0("threshold_", i), b] <- 
          (true_positive) / 
          (true_positive + false_positive)
        negative_predictive_value.boot.test[paste0("threshold_", i), b] <- 
          (true_negative) / 
          (true_negative + false_negative)
      }
    }
    
    # 4. OPTIMISM ----
    
    accuracy.optimism <- accuracy.boot.app - accuracy.boot.test
    true_positive_rate.optimism <- true_positive_rate.boot.app - true_positive_rate.boot.test
    true_negative_rate.optimism <- true_negative_rate.boot.app - true_negative_rate.boot.test
    positive_predictive_value.optimism <- positive_predictive_value.boot.app - positive_predictive_value.boot.test
    negative_predictive_value.optimism <- negative_predictive_value.boot.app - negative_predictive_value.boot.test
    
    ## OPTIMISM CONVERGENCE CHECK 
    
    accuracy.optimism.convergence_check <- matrix(nrow = nrow(accuracy.optimism),
                                                  ncol = ncol(accuracy.optimism))
    rownames(accuracy.optimism.convergence_check) <- rownames(accuracy.optimism)
    colnames(accuracy.optimism.convergence_check) <- colnames(accuracy.optimism)
    
    for (i in 1:nrow(accuracy.optimism)) {
      for (j in 1:ncol(accuracy.optimism)){
        accuracy.optimism.convergence_check[i, j] <- sum(accuracy.optimism[i , 1:j]) / j
      }
    }
    
    true_positive_rate.optimism.convergence_check <- matrix(nrow = nrow(true_positive_rate.optimism),
                                                            ncol = ncol(true_positive_rate.optimism))
    rownames(true_positive_rate.optimism.convergence_check) <- rownames(true_positive_rate.optimism)
    colnames(true_positive_rate.optimism.convergence_check) <- colnames(true_positive_rate.optimism)
    
    for (i in 1:nrow(true_positive_rate.optimism)) {
      for (j in 1:ncol(true_positive_rate.optimism)){
        true_positive_rate.optimism.convergence_check[i, j] <- sum(true_positive_rate.optimism[i , 1:j]) / j
      }
    }
    
    true_negative_rate.optimism.convergence_check <- matrix(nrow = nrow(true_negative_rate.optimism),
                                                            ncol = ncol(true_negative_rate.optimism))
    rownames(true_negative_rate.optimism.convergence_check) <- rownames(true_negative_rate.optimism)
    colnames(true_negative_rate.optimism.convergence_check) <- colnames(true_negative_rate.optimism)
    
    for (i in 1:nrow(true_negative_rate.optimism)) {
      for (j in 1:ncol(true_negative_rate.optimism)){
        true_negative_rate.optimism.convergence_check[i, j] <- sum(true_negative_rate.optimism[i , 1:j]) / j
      }
    }
    
    positive_predictive_value.optimism.convergence_check <- matrix(nrow = nrow(positive_predictive_value.optimism),
                                                                   ncol = ncol(positive_predictive_value.optimism))
    rownames(positive_predictive_value.optimism.convergence_check) <- rownames(positive_predictive_value.optimism)
    colnames(positive_predictive_value.optimism.convergence_check) <- colnames(positive_predictive_value.optimism)
    
    for (i in 1:nrow(positive_predictive_value.optimism)) {
      for (j in 1:ncol(positive_predictive_value.optimism)){
        positive_predictive_value.optimism.convergence_check[i, j] <- sum(positive_predictive_value.optimism[i , 1:j]) / j
      }
    }
    
    negative_predictive_value.optimism.convergence_check <- matrix(nrow = nrow(negative_predictive_value.optimism),
                                                                   ncol = ncol(negative_predictive_value.optimism))
    rownames(negative_predictive_value.optimism.convergence_check) <- rownames(negative_predictive_value.optimism)
    colnames(negative_predictive_value.optimism.convergence_check) <- colnames(negative_predictive_value.optimism)
    
    for (i in 1:nrow(negative_predictive_value.optimism)) {
      for (j in 1:ncol(negative_predictive_value.optimism)){
        negative_predictive_value.optimism.convergence_check[i, j] <- sum(negative_predictive_value.optimism[i , 1:j]) / j
      }
    }
    
    # 5. OPTIMISM-CORRECTED ESTIMATE ---- 
    
    accuracy.optimism_corrected <- accuracy.app - apply(accuracy.optimism, 1, mean)
    true_positive_rate.optimism_corrected <- true_positive_rate.app - apply(true_positive_rate.optimism, 1, mean)
    true_negative_rate.optimism_corrected <- true_negative_rate.app - apply(true_negative_rate.optimism, 1, mean)
    positive_predictive_value.optimism_corrected <- positive_predictive_value.app - apply(positive_predictive_value.optimism, 1, mean)
    negative_predictive_value.optimism_corrected <- negative_predictive_value.app - apply(negative_predictive_value.optimism, 1, mean)
    
    confusion_matrix_statistics <- cbind(accuracy.app,
                                         accuracy.optimism_corrected,
                                         true_positive_rate.app,
                                         true_positive_rate.optimism_corrected,
                                         true_negative_rate.app,
                                         true_negative_rate.optimism_corrected,
                                         positive_predictive_value.app,
                                         positive_predictive_value.optimism_corrected,
                                         negative_predictive_value.app,
                                         negative_predictive_value.optimism_corrected)
    colnames(confusion_matrix_statistics) <- c("accuracy.app",
                                               "accuracy.optimism_corrected",
                                               "true_positive_rate.app",
                                               "true_positive_rate.optimism_corrected",
                                               "true_negative_rate.app",
                                               "true_negative_rate.optimism_corrected",
                                               "positive_predictive_value.app",
                                               "positive_predictive_value.optimism_corrected",
                                               "negative_predictive_value.app",
                                               "negative_predictive_value.optimism_corrected")
    
    # RETURN ----
    
    invisible(list(
      "confusion_matrix_statistics" = list(confusion_matrix_statistics),
      "convergence_check" = 
        list("accuracy.optimism.convergence_check" = list(accuracy.optimism.convergence_check),
             "true_positive_rate.optimism.convergence_check" = list(true_positive_rate.optimism.convergence_check),
             "true_negative_rate.optimism.convergence_check" = list(true_negative_rate.optimism.convergence_check),
             "positive_predictive_value.optimism.convergence_check" = list(positive_predictive_value.optimism.convergence_check),
             "negative_predictive_value.optimism.convergence_check" = list(negative_predictive_value.optimism.convergence_check))))
  }
