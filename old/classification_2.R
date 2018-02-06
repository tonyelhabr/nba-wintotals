#' ---
#' author: "Tony"
#' title: "Classification"
#' output:
#'   html_document:
#'     keep_md: true
#'     toc: true
#'     number_sections: true
#' ---
#+ global_options, include = FALSE
knitr::opts_chunk$set(
  fig.width = 11,
  fig.height = 7,
  fig.path = "figs/",
  warning = FALSE,
  message = FALSE
)
#'
#' # Introduction
#'
#' This is a continuation of my previous experimentation with
#'  classification models.
#'
#'
#' # Importing the Data
#'
#+ setup, include = FALSE
if (substr(getwd(), 1, 1) == "C") {
  setwd("C:/Users/aelhabr/Dropbox/data_science/projects/nba/tr")
} else if (substr(getwd(), 1, 1) == "O") {
  setwd("O:/_other/code/tony/nba/tr")
}
rm(list = ls())
dev.off()
# graphics.off()
# par(mar = c(0, 0, 0, 0))
#'
#' The "tidied" data that I import here comes from some of my
#' previous classification experimentation.
#'
#+ import_data
dir_data <- paste0(getwd(), "/data/")
d_join <-
  readRDS(paste0(dir_data, "d_join_classification.rds"))
#'
#' Now, I'm actually going to replicate the `d_model` object that I used
#' before. I didn't save this variable explicity in order to give
#' myself some flexibility with working with the `d_join` object.
#' 
d_model <- d_join %>%
  dplyr::select(-team,
                -person,
                -pick,
                -confidence,
                -pick_result_bool,
                -w_pick_confidence) %>%
  distinct()
#'
#'
#' I'll be using the same set of years as before.
#'
#+ define_variables
yrs <- 2007:2016
#'
#' ### Logistic Regression Attempt #2
#' 
#' Now I'll pick up where I left off last time with logistic regression
#' models, this time using different formulas.
#'
#' How about using an interaction term between the same two predictors?
#' Unfortuantely, that still doesn't create such a great model. Again,
#' the p-values indicate that neither of the variables is significant.
#'
#+ lm_2_create
fmla_2 <-
  paste(y_name, paste(x_names, collapse = " * "), sep = " ~ ")
# fmla_2 <- "result ~ w_sportsbook * w_pick_confidence"
# fmla_2 <- "result ~ result_lag1 * w_pick_confidence"
# fmla_2 <- "result ~ w_sportsbook_err1 * w_diff2"
lm_2_fit_full <- glm(fmla_2, data = d_model, family = binomial)
summary(lm_2_fit_full)$coef
coef(lm_2_fit_full)
#'
#' As with the previous model, this model's performance
#' when trained and tested on the full data set is unexceptional.
#'
#+ lm_2_eval
lm_2_prob_full <- predict(lm_2_fit_full, type = "response")
lm_2_pred_full <- rep("U", dim(d_model)[1])
lm_2_pred_full[lm_2_prob_full > mean(lm_2_prob_full)] = "O"
table(lm_2_pred_full, d_model$result)
round(mean(lm_2_pred_full == d_model$result), 2)
#'
#' And how does it look if we implement a more proper train/test methodology?
#' Again, it doesn't seem like there is not much of a difference.
#'
#+ lm_2_eval_2
lm_2_mean_pct <- train_test_lm(d_model, fmla_2, yrs)
round(mean(as.numeric(lm_2_mean_pct)), 2)
round(as.numeric(lm_2_mean_pct), 2)
#'
#' Interestingly, the first model looks (marginally) better when evaluating by
#' the mean correct prediction percentage over each year. Nevertheless,
#' the two models show the similar performance trends in the same years.
#' That is, both perform above/below/equal to average on the same years, but
#' even this inference is not very definitive.
#'
#' In all, I conclude that both aren't very good models.
#'
#' ### Logistic Regression Attempt #3
#'
#' Enough of those models! Let's try something simpler. This time, let's see how
#' using only one predictor works. I'm choosing to go with the predictor that
#' portrays the amount of error in the sportsbook's projected win total
#' from the previous year since that variable exhibited the one of the lower
#' p-values in our previous models.
#'
#+ lm_3_create
fmla_3 <- "result ~ w_sportsbook_err1"
lm_3_fit_full <- glm(fmla_3, data = d_model, family = binomial)
coef(lm_3_fit_full)
summary(lm_3_fit_full)$coef
#'
#' Unfortunately, even in this setting, the predictor's p-value doesn't
#' seem to be significant. Anyways, let's see how well this simple model
#' does in prediction.
#'
#+ lm_3_eval
lm_3_prob_full <- predict(lm_3_fit_full, type = "response")
lm_3_pred_full <- rep("U", dim(d_model)[1])
lm_3_pred_full[lm_3_prob_full > mean(lm_3_prob_full)] = "O"
table(lm_3_pred_full, d_model$result)
round(mean(lm_3_pred_full == d_model$result), 2)
#'
#' Wow! That's outstanding performance. But... what if I don't train
#' and predict on the full data set?
#'
#+ lm_3_eval_2
lm_3_mean_pct <- train_test_lm(d_model, fmla_3, yrs)
round(mean(as.numeric(lm_3_mean_pct)), 2)
round(as.numeric(lm_3_mean_pct), 2)
#'
#' Training/testing gives a prediction accuracy around 50% once again.
#' These results (unfortunately) are much more reasonable and
#' are in line with the other models that I've tried. This is why you shouldn't
#' train/test on the entire data set! You can end up over-fitting the model
#' very badly!
#'
#' ### Logistic Regression Attempt #4
#'
#' How about adding in another one of the variable that showed a
#' low p-value in our previous attempts? Perhaps using only one variable is
#' too drastic.
#'
#+ lm_4_create
fmla_4 <- "result ~ w_sportsbook_err1 + prob_implied_diff"
lm_4_fit_full <- glm(fmla_4, data = d_model, family = binomial)
coef(lm_4_fit_full)
summary(lm_4_fit_full)$coef
#'
#+ lm_4_eval
lm_4_prob_full <- predict(lm_4_fit_full, type = "response")
lm_4_pred_full <- rep("U", dim(d_model)[1])
lm_4_pred_full[lm_4_prob_full > mean(lm_4_prob_full)] = "O"
table(lm_4_pred_full, d_model$result)
round(mean(lm_4_pred_full == d_model$result), 2)
#'
#' Again, we see outstanding performance when training/testing on the entire
#' data set. Now let's do some proper training/testing.
#'
#+ lm_4_eval_2
lm_4_mean_pct <- train_test_lm(d_model, fmla_4, yrs)
round(mean(as.numeric(lm_4_mean_pct)), 2)
round(as.numeric(lm_4_mean_pct), 2)
#'
#' And again, we see that the prediction performance of the improperly
#' trained/tested model was deceiving.
