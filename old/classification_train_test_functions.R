
# loocv_lr ####
loocv_lr <- function(d_model, fmla, yrs, yr_test = NULL) {
  if (is.null(yr_test)) {
    mean_pct <- list()
    for (i in 1:length(yrs)) {
      vector_train <- !(d_model$yr_start %in% yrs[i])
      
      d_model_test <- d_model[!vector_train,]
      d_y_test <- d_model$result[!vector_train]
      
      
      # Not sure why this isn't working.
      # lr_fit_train <- glm(fmla, data = d_model,
      #                     family = binomial,
      #                     subset = vector_train)
      # This is an alternative.
      d_model_train <- d_model[vector_train,]
      lr_fit_train <-
        glm(fmla, data = d_model_train, family = binomial)
      lr_prob_test <-
        predict(lr_fit_train, d_model_test, type = "response")
      
      lr_pred_test <- rep("U", dim(d_model_test)[1])
      # lr_pred_test[lr_prob_test > 0.5] = "O"
      lr_pred_test[lr_prob_test > mean(lr_prob_test)] = "O"
      
      
      mean_pct[i] <- mean(lr_pred_test == d_y_test)
      
      # # This is an alternative.
      # lr_conf_matrix <- table(lr_pred_test, d_y_test)
      # mean_pct[i] <-
      #   (sum(lr_conf_matrix[1, 1] + lr_conf_matrix[2, 2]) /
      #      sum(lr_conf_matrix))
    }
  } else {
    vector_train <- !(d_model$yr_start %in% yr_test)
    d_model_test <- d_model[!vector_train,]
    d_y_test <- d_model$result[!vector_train]
    d_model_train <- d_model[vector_train,]
    lr_fit_train <-
      glm(fmla, data = d_model_train, family = binomial)
    lr_prob_test <-
      predict(lr_fit_train, d_model_test, type = "response")
    lr_pred_test <- rep("U", dim(d_model_test)[1])
    lr_pred_test[lr_prob_test > mean(lr_prob_test)] = "O"
    mean_pct <- mean(lr_pred_test == d_y_test)
  }
  mean_pct
}

loocv_lda <- function(d_model, fmla, yrs) {
  mean_pct <- list()
  for (i in 1:length(yrs)) {
    vector_train <- !(d_model$yr_start %in% yrs[i])
    
    d_model_test <- d_model[!vector_train,]
    d_y_test <- d_model$result[!vector_train]
    
    lda_fit_train <-
      lda(as.formula(fmla), data = d_model, subset = vector_train)
    # This is an alternative.
    # d_model_train <- d_model[vector_train, ]
    # lda_fit_train <- lda(as.formula(fmla), data = d_model_train)
    suppressWarnings(lda_pred_test <-
                       predict(lda_fit_train, d_model_test))
    lda_conf_table <- table(lda_pred_test$class, d_y_test)
    mean_pct[i] <-
      (sum(lda_conf_table[1, 1] + lda_conf_table[2, 2]) / sum(lda_conf_table))
  }
  mean_pct
}


loocv_svm <-
  function(d_model_no_factors,
           fmla,
           yrs,
           kernel_type,
           cost = 1) {
    # fmla <- fmla
    # kernel_type <- "linear"
    # cost <- 1
    # i <- 1
    range_cost <- 10 ^ seq(-3, 2, 1)
    svm_linear_tune <-
      tune(
        svm,
        as.formula(fmla),
        data = d_model_no_fctrs,
        kernel = kernel_type,
        ranges = list(cost = range_cost)
      )
    svm_linear_best <- svm_linear_tune$best.model
    mean_pct <- list()
    for (i in 1:length(yrs)) {
      vector_train <- !(d_model_no_fctrs$yr_start %in% yrs[i])
      d_model_no_factors_test <- d_model_no_fctrs[!vector_train,]
      d_y_test <- d_model_no_fctrs$result[!vector_train]
      
      # This isn't working?
      # svm_fit_train <-
      #   svm(
      #     as.formula(fmla),
      #     data = d_model_no_fctrs,
      #     kernel = kernel_type,
      #     cost = 1
      #   )
      # This is an alternative. It also doesn't work?
      # d_model_train <- d_model_no_fctrs[vector_train, ]
      # svm_fit_train <-
      #   svm(
      #     as.formula(fmla),
      #     data = d_model_train,
      #     kernel = kernel_type,
      #     cost = cost
      #   )
      svm_pred_test <-
        predict(svm_linear_best, d_model_no_fctrs_test)
      svm_conf_table <- table(svm_pred_test, d_y_test)
      mean_pct[i] <- mean(svm_pred_test == d_y_test)
      # This is an alternative.
      # mean_pct[i] <-
      #   (sum(svm_conf_table[1, 1] + svm_conf_table[2, 2]) /
      #      sum(svm_conf_table))
    }
    mean_pct
  }


