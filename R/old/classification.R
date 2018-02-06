#' ---
#' author: "Tony"
#' title: "Classification"
#' date: '`r Sys.Date()`'
#' ouptut:
#'   html_document:
#'     keep_md: TRUE
#      toc: true
#      number_sections: true
#' ---
#+ global_options, include = FALSE
knitr::opts_chunk$set(
  fig.width = 11,
  fig.height = 7,
  fig.path = "figs/",
  # tidy = TRUE,
  # tidy.opts=list(width.cutoff = 80),
  warning = FALSE,
  message = FALSE
)

#'
#' # Introduction
#'
#' This project describes my experimentation/experience with creating
#' classification models to predict whether or not an NBA team will go over
#' or under the number of wins projected by sportsbooks. The inspiration for
#' this project is multi-faceted:
#'   1. My brother and I have been picking over/unders for NBA teams dating
#'   back to 2012 and have always been fascinated with sports gambling (alhtough
#'   we have never actually participated ourselves). I wanted to evaluate
#'   our picks in some way, perhaps to create a model that could make
#'   accurate win total picks for me. If I can create something good enough,
#'   then maybe one day I will be confident enough to actually place a bet!
#'   2. I wanted a project to help me develop my R skills.
#'   3. I wanted to practice some of the statistical and machine learning
#'   techniques that I have studied and read about all the time.
#'
#' Alhtough my brother and I have only been recording our over/under picks
#' since 2012, I will be looking at NBA seasons from 2007 to 2016 because
#' I have win totals going back to the 2007 season. Notably, my brother and I
#' also submitted "confidence ratings" gauging our level of confidence
#' in our picks starting in 2014. (These ratings go from 1 to 30, where 1 is
#' our most confident pick and 30 is our least.)
#'
#' Although I would like to use and/or evaluate our "personal" data (i.e. picks)
#' to possibly create better models (perhaps by exposing our internal biases/
#' tendencies and identifying where we might "fade" our initial picks), I won't
#' be using this pick data in this part of the project. I'll leave that
#' for another time. Thus, it is not problemmatic that I will be
#' investigating years which we did not record picks
#' (or submit confidence ratings). Nevertheless, some evidence of code that
#' includes some of this "personal" data may be evident in the code that follows
#' because I evaluated it during the development of this project.
#'
#' In this portion of the project, I'll evaulate different
#' models while keeping the model formula constant. Although choice of model
#' formula is a significant decision in and of itself, my purpose here is to
#' compare classification models. I'll evaluate differen formulas
#' in another portion of this project. Nevertheless, I'll use my
#' best judgement and intuition in my selection of the model formula to be used.
#'
#' Finally, as a note to those who are interesting in code style, I attempt to
#' implement the "tidy" workflow
#' promoted by Hadley Wickham and many other prominent R community members.
#' This approach advocates initial steps of
#' importing and tidying data; followed by a cycle
#' of transforming, modeling, and analyzing the data; and, finally,
#' communicating the results.
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

#+ setup_2, include = FALSE
# dev.off()
# graphics.off()
# par(mar = c(0, 0, 0, 0))
#'
#' I start by loading in the data set for stats and team win totals. I
#' have saved this data in local rds files for use across multiple projects.
#'
#' One might notice a couple of things about my naming of variables:
#'   + I generally use the prefix "d_" in order to distinguish data. This
#' is a personal convention that I picked up from traces of things in other
#' languages (variable naming with VBA and table naming with Oracle SQL).
#'   + I tend to use the suffix "_raw" if the variable is going to be
#'   manipulated/transformed in some way.
#'   + Sometimes, when I create a new variable that copies all or most of the
#'   data from an existing variable, I append the new variable's name with
#'   the suffix "_2", "_3", etc. I do this for one of two reason:
#'     1. This might be done for ease in debugging/developing the code because
#'     it helps me confirm that the changes that I intended to make actually
#'     do take effect when re-running a certain chunk of code multiple times.
#'     The variables that get created in this manner might be deemed
#'     "temporary".
#'     I realize that this is not exactly a "best practice", but it works
#'     for me.
#'     2. At other times, such as when I'm looking at different models that are
#'     from the same family of models, then I'll use the suffix to simply
#'     distinguish that the variable is similar to another variable (and has
#'     the same dimenstions, etc.) but there is something intentionally
#'     different about the way it has been defined.
#'
#'
#+ test_vars, include = FALSE
do_extra <- FALSE
do_error <- FALSE
#+ import_data
dir_data <- paste0(getwd(), "/data/")
d_win_totals_raw <-
  readRDS(paste0(dir_data, "d_win_totals.rds"))
d_scraped_stats_classification_raw <-
  readRDS(paste0(dir_data, "d_scraped_stats_classification_raw.rds"))
#'
#' # Tidying and Transforming the Data
#'
#' Now I'll tidy and transform.
#'
#+ tidy_and_transform_data
library(tidyverse)

d_scraped_stats_classification <- d_scraped_stats_classification_raw %>%
  group_by(team) %>%
  mutate(avg_scoring_margin_lag1 = lag(avg_scoring_margin),
         win_pct_close_games_lag1 = lag(win_pct_close_games)) %>%
  ungroup() %>%
  dplyr::select(-avg_scoring_margin, -avg_margin_thru_three_quarters, -win_pct_close_games)

# Selecting only the desired columns.
# dplyr's select() function may conflict with other packages.
# Not only selecting the desired columns, but also adding
# some variables to (possibly) be used in models.
# The inner_join might not work if the team columns is not of the same type
# in each data set. (It must be either a character/factor in both data sets.)
d_win_totals <- d_win_totals_raw %>%
#   dplyr::select(
#     one_of(
#       "pick_id",
#       "yr_start",
#       "team",
#       "w",
#       "w_sportsbook",
#       "odds_over",
#       "odds_under"
#     )
#   ) %>%
  #   group_by(yr_start) %>%
  #   mutate(w_rnk = dense_rank(desc(w)),
  #   w_sportsbook_rnk = dense_rank(desc(w_sportsbook))) %>%
  #   ungroup() %>%
  group_by(team) %>%
  mutate(w_lag1 = lag(w),
         w_diff2 = (lag(w) - lag(w, 2))) %>%
  # ungroup() %>%
  # group_by(yr_start) %>%
  # mutate(w_diff2_rnk = dense_rank(desc(w_diff2))) %>%
  ungroup() %>%
  # mutate(result = ifelse(w > w_sportsbook, "O", "U")) %>%
  mutate(result = ifelse(w > w_sportsbook, 1, 0)) %>%
  ungroup() %>%
  group_by(team) %>%
  mutate(result_lag1 = lag(result),
         w_sportsbook_err1 = (lag(w) - lag(w_sportsbook))) %>%
  ungroup() %>%
  mutate(
    prob_implied_over =
      ifelse(
        odds_over < 0,-odds_over / (-odds_over + 100),
        ifelse(odds_over > 0, 100 / (odds_over + 100), 0)
      ),
    prob_implied_under =
      ifelse(
        odds_under < 0,-odds_under / (-odds_under + 100),
        ifelse(odds_under > 0, 100 / (odds_under + 100), 0)
      ),
    prob_implied_combo = prob_implied_over - prob_implied_under,
    # underdog_over =
    #   ifelse(odds_over > 0, 1,
    #          ifelse(odds_over < 0, 0, 0)),
    # underdog_under =
    #   ifelse(odds_under > 0, 1,
    #          ifelse(odds_under < 0, 0, 0)),
    underdog_combo = ifelse(odds_over > 0, 1, ifelse(odds_under > 0,-1, 0))
  )

# Could convert these to factors, but choosing not.
# d_win_totals$result <- as.factor(d_win_totals$result)
# d_win_totals$result_lag1 <- as.factor(d_win_totals$result_lag1)
# d_win_totals$underdog_combo <- as.factor(d_win_totals$underdog_combo)


# Could replace years with no odds with a constant values such as -110,
# but choosing to exclude them instead.
# d_win_totals$odds_over <-
#   as.numeric(str_replace_all(d_win_totals$odds_over, "na", -110))
# d_win_totals$odds_under <-
#   as.numeric(str_replace_all(d_win_totals$odds_under, "na", -110))
#'
#' A bit more refining to do...
#'
#+ d_join_bookmark, include = FALSE
# d_join ####
cols_keep <- c("yr_start", "team",
               "w_lag1", "w_diff2", "result_lag1", "w_sportsbook_err1",
               "prob_implied_combo", "underdog_combo",
               "avg_scoring_margin_lag1", "win_pct_close_games_lag1",
               "result")

d_join <- d_win_totals %>%
  inner_join(d_scraped_stats_classification, by = c("yr_start", "team")) %>%
  dplyr::select(one_of(cols_keep))

# d_join <- d_join %>%
#   group_by(yr_start) %>%
#   mutate(team_int = dense_rank(team)) %>%
#   ungroup() %>%
#   dplyr::select(-team) %>%
#   rename(team = team_int)
#'
#+ import_tidy_transform_stats, include = FALSE
# d_scraped_stats_raw <-
#   readRDS(paste0(dir_data, "d_scraped_stats_raw.rds"))
# cols_join <- c("team", "yr_start")  # essential variables for joins
# cols_stats <- c("off_efficiency", "def_efficiency")
# d_scraped_stats <- d_scraped_stats_raw %>%
#   dplyr::select(one_of(c(cols_join, cols_stats)))
#'
#' # Modeling the Data
#'
#+ d_model_bookmark, include = FALSE
# d_model ####
#+ d_model
d_model <- d_join %>%
  dplyr::select(-team) %>%
  filter(yr_start > 2008) %>%
  distinct()

# Save a copy of d_model before normalizing it for potential future use.
d_model_non_norm <- d_model

normalize <- function(col) (col - mean(col)) / sd(col)
d_model <- d_model %>%
  mutate_at(vars(-yr_start, -result_lag1, -result), funs(normalize(.)))

d_model_no_fctrs <- d_model

# Make only the response variable a factor to make modeling more uniform.
d_model$result <- as.factor(d_model$result)

# d_model$result_lag1 <- as.factor(d_model$result_lag1)
# d_model$underdog_combo <- as.factor(d_model$underdog_combo)

#'
#+ save_data, include = FALSE
saveRDS(d_join, file = paste0(dir_data, "d_join_classification", ".rds"))
saveRDS(d_model, file = paste0(dir_data, "d_model_classification", ".rds"))
#'
#+ remove_temp_data, include = FALSE
# rm(list = ls()[!(ls() %in% c("dir_data", "d_model"))])
#'
#' For my analysis, I'm going to use the `result` variable as the response
#' variable that I'll be trying to predict, and only a handful of the
#' remaining variables as predictors. But which predictors to choose?
#' Let's look at some correlations between possible predictor variables.
#'
#+ correlations_bookmark, include = FALSE
# correlations ####
#+ correlations
names_x_plausible <-
  c(
    "w_lag1", "w_diff2", "result_lag1", "w_sportsbook_err1",
    "prob_implied_combo", "underdog_combo",
    "avg_scoring_margin_lag1", "win_pct_close_games_lag1"
  )
d_model_x <- d_model %>% dplyr::select(one_of(names_x_plausible))

library(polycor)
# Need as.data.frame() because tibbles are interpreted as lists.
d_cor <- hetcor(as.data.frame(d_model_x))
d_cor$correlations <- round(d_cor$correlations, 2)
d_cor$correlations

#+ correlations_plot
# A visualization of variable relationships.
# if (do_extra) {
#   library(GGally)
#   ggpairs(d_model_x, lower = list(combo = wrap("facethist", binwidth = 30))
# }
#' It turns out that there is some strong collinearity between
#' `result_lag1` and `w_sportsbook_err1`, as well as between
#' `prob_implied_combo` and `underdog_combo`.
#' This actually isn't all that expected since each pair of variables
#' essentially portrays the same information.

#' Alternatively, I could take a more systematic approach.
#' I could run a subsetting algorithm on all possible predictors
#' and see what turns out to be "optimal".
#'
#+ subset_fit_full
library(leaps)
name_y <- "result"
names_exclude <- c("yr_start", "result", "w", "w_sportsbook")
names_x_all <- names(d_model)[!(names(d_model) %in% names_exclude)]

# Cannot have NA's in data.matrix
d_x <- data.matrix(dplyr::select(d_model, one_of(names_x_all)))
d_y <- data.matrix(dplyr::select(d_model, one_of(name_y)))
subset_fit_full <- regsubsets(d_x, d_y, nvmax = length(names_x_all))
#'
#' There are several different criteria that could be used to choose model
#' parameters. I'll look at Adjusted R^2, Cp, and BIC.
#'
#+ subset_summary
subset_summary <- summary(subset_fit_full)
# subset_summary
# which.max(subset_summary$adjr2)
# which.min(subset_summary$cp)
# which.min(subset_summary$bic)
coef(subset_fit_full, which.max(subset_summary$adjr2))
coef(subset_fit_full, which.min(subset_summary$cp))
coef(subset_fit_full, which.min(subset_summary$bic))

if (do_extra) {
  par(mfrow = c(3, 1))
  plot(subset_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
  points(which.max(subset_summary$adjr2), subset_summary$adjr2[which.max(subset_summary$adjr2)],
         col = "red", cex = 2, pch = 20)
  plot(subset_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
  points(which.min(subset_summary$cp), subset_summary$cp[which.min(subset_summary$cp)],
         col = "red", cex = 2, pch = 20)
  plot(subset_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
  points(which.min(subset_summary$bic), subset_summary$bic[which.min(subset_summary$bic)],
         col = "red", cex = 2, pch = 20)
  par(mfrow = c(1, 1))
  plot(subset_fit_full, scale = "adjr2")
  plot(subset_fit_full, scale = "Cp")
  plot(subset_fit_full, scale = "bic")
}
#'
#' It looks like these critiera each select a small number of parameters,
#' including some of the variables that I identified as signficant. Each
#' criterion suggests variables that are not collinear with others.
#' I'll stick with my heurestically chosen formula for now, but when it comes
#' time to expermenting with different formulas, I'll take a second look
#' at the variables identified by this subsetting algorithm.
#'

#' In order to create robust models (which often depend on
#' independence among predictors), it would be a good idea to remove
#' one of the variables in a pair of variables exhibiting collinearity.
#' Another option  might be to modify the redundant variables in some way.
#' I'll opt for the latter option and neglect some variables for future
#' consideration. I'll stick with the `w_sporstbook_err1` and
#'`prob_iimplied_combo` variables since they are more granular.
#' `result_lag1` are `underdog_combo` are both factors, while their
#' counterparts are not.
#'
#' I'm also going to remove the `w_lag1` variable for intuitional reasons.
#' The raw number of wins that a team had in the previous season
#' shouldn't have any effect on  whether or not the team exceeds
#' its win total in the current season.
#'
#+ correlations_2
names_x <- c("w_sportsbook_err1", "prob_implied_combo")
d_model_x_2 <- d_model %>% dplyr::select(one_of(names_x))

d_cor_2 <- hetcor(as.data.frame(d_model_x_2))
d_cor_2$correlations <- round(d_cor_2$correlations, 2)
d_cor_2$correlations

#+ correlations_plot_2
# A visualization of variable relationships.
library(GGally)
ggpairs(d_model_x_2, lower = list(combo = wrap("facethist", binwidth = 30)))
# A visualization of just the correlations.
# ggcorr(d_cor$correlations, palette = "RdBu", label = TRUE)
#+ correlations_3, include = FALSE
# library(corrplot)
# corrplot(
#   d_cor$correlation,
#   type = "upper",
#   order = "hclust",
#   tl.col = "black",
#   tl.srt = 45
# )
#'
#'
#' Alright, that's enough of that. Let's get to some model experimentation.
#' Here are the variables and formula that I'll be using.
#' To predict whether or not a given team will go over or under its
#' win total in a given year, I will be using the
#' difference between the sportbook win total in predicting
#' a given team's number of wins from the previous year
#' and a singular number that combines the implied
#' probability of the moneyline odds for the over and under.
#'
#' model_fmla_paste
name_y <- "result"
names_x <- c("w_sportsbook_err1", "prob_implied_combo")
fmla <- paste(name_y, paste(names_x, collapse = " + "), sep = " ~ ")
#' model_fmla_explicit, include = FALSE
# fmla <- "result ~ w_sportsbook_err1 + prob_implied_combo"
#'
#' ## Logisitic Regression
#'
#' Now I'll start with a fairly well known family of models: logistic
#' regression. I'll fit a model on the entire data set just to check that
#' everything works.
#'
#+ lr_bookmark, include = FALSE
# lr ####
# glm() can work with either categorical or continuous response variable.
# However, family = binomial must be specified for a
# categorical response variable. Nevertheless, categorical variables may
# be coded as numerics and glm() will still function properly
# if family = binomial.
# glmnet(..., family = "binomial") is an alternative.
# Also, type = "response" should be specified in predict().
# If not, then another type (e.g. logit) may be used.
#+ lr_fit_full
lr_fit_full <- glm(fmla, data = d_model, family = binomial)
summary(lr_fit_full)$coef
coef(lr_fit_full)
#'
#' It looks like this model isn't all too bad. It certainly could be worse.
#' However, even after my process of trying to identify the best predictors
#' to use heuristically, the p-values indicate that only one of the predictors
#' is signficant. This actually might not be a flaw in my selection process.
#' It could just be that there isn't a great combination of predictors out of
#' those available. Anyways, I'll leave formula experimentation for another
#' portion of this project.
#'
#'
#+ lr_pred_full
lr_prob_full <- predict(lr_fit_full, type = "response")
# lr_pred_full <- rep("U", dim(d_model)[1])
lr_pred_full <- rep(0, dim(d_model)[1])
#'
#' TODO: Needs more explanation.
#' The naive value for the barrier between choosing 0 and 1 would
#' be 0.5
#' However, in this context, I think choosing 50% for each side is arguably
#' more appropriate. Also, using the mean probability
#' reduces sensitivity to model parameters.
#+ lr_pred_full_2
# lr_pred_full[lr_prob_full > 0.5] = 1
# lr_pred_full[lr_prob_full > mean(lr_prob_full)] = "O"
lr_pred_full[lr_prob_full > mean(lr_prob_full)] = 1
table(lr_pred_full, d_model$result)
round(mean(lr_pred_full == d_model$result), 2)
#'
#' So it isn't the worst model one could possibly come up with. Atleast it
#' better than 50% in predicitng over/unders when trained and tested on the
#' full data set.
#'
#' But how much worse (or better) might the prediction performance get when
#' properly trained and tested on different sets? I'll forego the creation of
#' a validation set for now since I'm not explicity trying to refine
#' some cost parameter or another kind of tuning parameter. Instead,
#' I'm simply going to split the data into train and test sets. This could be
#' done in a number of ways, such as random sampling. However,
#' I'm simply going to use a single year's worth of data
#' as a test set and the rest as the train set. I'll do this for every year.
#' This is essentially "leave-one-out cross validation" (LOOCV).
#'
#' I'll save some of the prediction performance data
#' so that I can compare other models with this one.
#'
#+ lr_accuracy_pct
yrs_cv <- 2009:2016
lr_accuracy_pct <- list()
for (i in 1:length(yrs_cv)) {
  vector_train <- !(d_model$yr_start %in% yrs_cv[i])

  d_model_test <- d_model[!vector_train,]
  d_y_test <- d_model$result[!vector_train]
  d_model_train <- d_model[vector_train,]
  lr_fit_train <-
    glm(fmla, data = d_model_train, family = binomial)
  lr_prob_test <-
    predict(lr_fit_train, d_model_test, type = "response")
  # lr_pred_test <- rep("U", dim(d_model_test)[1])
  lr_pred_test <- rep(0, dim(d_model_test)[1])
  # lr_pred_test[lr_prob_test > mean(lr_prob_test)] = "O"
  lr_pred_test[lr_prob_test > mean(lr_prob_test)] = 1
  lr_accuracy_pct[i] <- mean(lr_pred_test == d_y_test)
  lr_conf_table <- table(lr_pred_test, d_y_test)
  if (i > 1) {
    lr_conf_table_all <-
      data.frame(
        rbind(lr_conf_table_all,
              cbind(
                data.frame(yr_start = rep(yrs_cv[i], length(lr_conf_table))),
                lr_conf_table
              )))
  } else {
    lr_conf_table_all <-
      data.frame(
        cbind(data.frame(yr_start = rep(yrs_cv[i], length(lr_conf_table)
        )),
        lr_conf_table))
  }
}

round(mean(as.numeric(lr_accuracy_pct)), 2)
round(as.numeric(lr_accuracy_pct), 2)
lr_conf_table_all <- lr_conf_table_all %>%
  rename(pred_test = lr_pred_test, count = Freq)
# lr_conf_table_all
#'
#' Actually, I can use the `cv.glm` function in the `boot` package
#' to perform cross validation. It performs k-fold cross validation with
#' a default value of k = # of number of observations. This is also known
#' as leave-one-out-cross-validation (LOOCV).
#'
#+ lr_cv_loocv
library(boot)
set.seed(42)
lr_cv_loocv <- cv.glm(data = d_model, glmfit = lr_fit_full)
summary(lr_cv_loocv)
lr_cv_loocv$delta[1]
#'
#' Now I'll do a k-fold cross-fold validation with k =- 10.
#+ lr_cv_k10
set.seed(42)
lr_cv_k10 <- cv.glm(data = d_model, glmfit = lr_fit_full, K = 10)
summary(lr_cv_k10)
1 - sqrt(lr_cv_k10$delta[1])
#'
#' These estimates of test set error seem too high, so maybe I'll just trust
#' the accuracy rates generated by my manual k-fold cross-validation.
#'
#' And look at that! As I feared, the model did worse (much worse) when
#' properly trained and test. In fact, it performs worse than random guessing!
#' I'll intepret this to indicate that the model isn't very good.
#'
#' Let's try a different kind of model now.
#'
#' ## Linear Discriminant Analysis (LDA)
#'
#' Now I'm going to try a different model framework: Linear Discriminant
#' Analysis (LDA). I'll be using the `lda` function from the `MASS` package.
#'
#' As I mentioned before, I'll be using the same formula with all of the
#' models that I experiment with in this part of the project.
#'
#' Also, as I did with logisitic regression model, I'll start by evaluating
#' a model fit on the entire data set even thought its predictions will be
#' deceivingly more accurate than they should be.
#'
#+ lda_bookmark, include = FALSE
# lda ####
# Response variable may be either categorical or continutuous. Categorical
# variable may be coded as numerics and lda() output will be the same
# as if they were coded as factors.
#+ lda_fit_full
library(MASS)
# Need as.formula() for lda function.
lda_fit_full <- lda(as.formula(fmla), data = d_model)
lda_fit_full
#'
#' Simple enough. Now let's evaluate this model on the entire data set.
#'
#+ lda_pred_full
# Using suppressWarnings() to ignore annoying "NA" warnings.
lda_pred_full <- predict(lda_fit_full, d_model)
lda_conf_table <- table(lda_pred_full$class, d_model$result)
lda_conf_table

# The line below doesn't work due to NA's in the data set.
# mean((lda_pred_full$posterior == d_model$result), na.rm = TRUE)
(sum(lda_conf_table[1, 1] + lda_conf_table[2, 2]) / sum(lda_conf_table))

#'
#' It appears that this model performs better than its logistic
#'  regression counterpart when trained and tested on the entire data set.
#' However, its true robustness will be shown when it is trained and tested in
#' a more proper manner. I'll use the same method as shown before.
#'
#+ lda_accuracy_pct
lda_accuracy_pct <- list()
for (i in 1:length(yrs_cv)) {
  vector_train <- !(d_model$yr_start %in% yrs_cv[i])
  d_model_test <- d_model[!vector_train,]
  d_y_test <- d_model$result[!vector_train]
  lda_fit_train <-
    lda(as.formula(fmla), data = d_model, subset = vector_train)
  lda_pred_test <- predict(lda_fit_train, d_model_test)
  lda_conf_table <- table(lda_pred_test$class, d_y_test)
  lda_accuracy_pct[i] <-
    (sum(lda_conf_table[1, 1] + lda_conf_table[2, 2]) / sum(lda_conf_table))
  if (i > 1) {
    lda_conf_table_all <-
      data.frame(
        rbind(lda_conf_table_all,
              cbind(
                data.frame(yr_start = rep(yrs_cv[i], length(lda_conf_table))),
                lda_conf_table
              )))
  } else {
    lda_conf_table_all <-
      data.frame(
        cbind(data.frame(yr_start = rep(yrs_cv[i], length(lda_conf_table)
        )),
    lda_conf_table))
  }
}
lda_accuracy_pct <- unlist(lda_accuracy_pct)
round(mean(as.numeric(lda_accuracy_pct), na.rm = TRUE), 2)
round(as.numeric(lda_accuracy_pct), 2)
lda_conf_table_all
#'
#' Not too bad ... Maybe this is an acceptable model.
#'
#' Now, I want to take a closer look at the train/test output of this LDA
#' model. I'll use the data from the 2016 season as the tet set
#' and the data from the years prior to it as the train set.
#'
#' Here, I'm basically removing the iteration from the train/test function.
#'
#+ lda_pred_test
# vector_train <- !(d_model$yr_start %in% yrs_cv)
vector_train <- d_model$yr_start %in% yrs_cv[1:(length(yrs_cv) - 1)]
vector_test <- d_model$yr_start %in% yrs_cv[length(yrs_cv)]
# d_model_test <- d_model[!vector_train,]
# d_y_test <- d_model$result[!vector_train]
d_model_test <- d_model[vector_test,]
d_y_test <- d_model$result[vector_test]
lda_fit_train <- lda(as.formula(fmla), data = d_model, subset = vector_train)
lda_pred_test <- predict(lda_fit_train, d_model_test)
lda_conf_table <- table(lda_pred_test$class, d_y_test)
lda_conf_table

lda_pred_test$posterior[1:30, 1]
sum(lda_pred_test$posterior[, 1] > 0.6)
sum(lda_pred_test$posterior[, 1] < 0.4)
#'
#' It seems like this model generates fairly exaggerated probabilities.
#' I would expect that the posterior probabilities would all be near 0.5.
#'
#' But which teams does this model project have the highest probability of
#' outperforming/underperforming their expectations (as determined by the
#' sportsbook win total)?
#'
#+ lda_pred_test_2
# Need to re-introduce team data since it is not in d_model.
# Team column was removed from d_join when creating d_model.
g_teams <- d_win_totals_raw <- readRDS(paste0(dir_data, "g_teams.rds"))

max(lda_pred_test$posterior[, 1])
# This assumes that the teams are listed in alphabetical order in d_model.
# d_model_test$team[which.max(lda_pred_test$posterior[, 1])]
g_teams$team[which.max(lda_pred_test$posterior[, 1])]
min(lda_pred_test$posterior[, 1])
# d_model_test$team[which.min(lda_pred_test$posterior[, 1])]
g_teams$team[which.min(lda_pred_test$posterior[, 1])]
#'
#' UPDATE: Actually, it appears that this LDA model is weighing the w_diff2
#' factor heavily, so the teams with the largest +- w_diff2 have the largest
#' posterior probabilities.
#'
#' So this LDA model picks POR to have the best chance of going over its
#' win total and NYK to have the worst chance of going under its win total in
#' the year 2016. This actually makes sense. Although POR was projected to do
#' relatively well, and did, in fact, end up with a good win-loss
#' record in 2016, one might expect that bookies set the win total lower than
#' it should be for bad and/or "non-public" teams in order to try to
#' even out the money placed on each betting side.
#'
#' In general, public bettors are more likely to make
#' bets purely on perception, so they are more likely to believe a bad team
#' will do very bad and a good team will do very good, and so they are more
#' likely to bet the under on a bad team and the over on a good team. Although
#' this deduction is fairly anecdotal, one can easily find research proving
#' this statement. Thus, in order to even out the betting sides, bookies are
#' more likely to exaggerate the win totals for the teams at the top and bottom
#' of the league.
#'
#' Another common bettor behavior may explain the why LDA model shows that
#' NYK is most likely to go under its projected win total (and, conversely, why
#' POR is most likely to exceed its win total). Teams in big cities
#' like New York are more likely to have "fanatical" fans that set overly-
#' optimistic expectations for their teams. Thus, to take advantage of their
#' potential foolishness and willingness to bet the over, bookies generally tend
#' to set the win totals for these "public" (i.e. big city or very popular)
#' teams
#' higher than they should be. (For example, this is also true of the
#' Dallas Cowboys in the NFL and the New York Yankees and Chicago Cubs in
#' the MLB.)
#'
#'Support Vector Machine (SVM) Attempt # 1
#'
#' Now, I'll look experiment with another family of models that can be
#' used to predict categorical response variables: support vector machines (SVMs).
#'
#+ svm_bookmark, include = FALSE
# svm ####
#+ svm_fit_full
library(e1071)
# Predictors can't be factors, so remove them. Alternatively, could convert
# factors to numerics.
# Response variable must be a factor.

# Need as.formula() here.
svm_fit_full <-
  svm(
    as.formula(fmla),
    data = d_model,
    kernel = "linear",
    cost = 1
  )
svm_fit_full
#+ svm_fit_full_plot, include = FALSE
# plot() won't work since formula isn't explicit in svm() call.
# plot(svm_fit_full, d_model)
#'
#' This package has a `tune` function to perform k-fold cross validation, so
#' I don't necessarily need to create a function to do training/testing.
#' (The default number of folds is 10.)
#'
#' But, to be consistent, I'll try out the implement the train/test evaluation
#' that I've been using and I'll use the cost value identified by
#' the `tune` function.
#'
#+ svm_tune
set.seed(42)
range_cost <- 10 ^ seq(-3, 2, 1)
svm_tune <-
  tune(
    svm,
    as.formula(fmla),
    data = d_model,
    kernel = "linear",
    ranges = list(cost = range_cost)
  )
summary(svm_tune)
svm_best <- svm_tune$best.model
svm_best$cost
#'
#' It turns out that the best cost value from the several that I tried out is 1,
#' so it looks like using the default cost value in my custom train/test
#' function wasn't so bad.
#'
#' Now I'll perform cross validation as I've done before.
#'
#+ svm_accuracy_pct
svm_accuracy_pct <- list()
for (i in 1:length(yrs_cv)) {
  vector_train <- !(d_model$yr_start %in% yrs_cv[i])
  d_model_test <- d_model[!vector_train,]
  d_y_test <- d_model$result[!vector_train]
  svm_pred_test <- predict(svm_best, d_model_test)
  svm_conf_table <- table(svm_pred_test, d_y_test)
  svm_accuracy_pct[i] <- mean(svm_pred_test == d_y_test)
  if (i > 1) {
    svm_conf_table_all <-
      data.frame(rbind(
        svm_conf_table_all,
        cbind(data.frame(yr_start = rep(
          yrs_cv[i], length(svm_conf_table)
        )),
        svm_conf_table)
      ))
  } else {
    svm_conf_table_all <-
      data.frame(cbind(data.frame(yr_start = rep(
        yrs_cv[i], length(svm_conf_table)
      )),
      svm_conf_table))
  }
}

round(mean(as.numeric(svm_accuracy_pct), na.rm = TRUE), 2)
round(as.numeric(svm_accuracy_pct), 2)
svm_conf_table_all <- svm_conf_table_all %>%
  rename(pred_test = svm_pred_test, count = Freq)
# svm_conf_table_all
#'
#' It turns out that this model isn't so bad.
#'
#' ## Trees
#'
#' Now, I'll look at trees. Before using the same 2-predictor formula
#' that I have been using, let's see what kind of model we get when we use all
#' of the predictors. One of the advantages of using a tree is identifying
#' important predictors. (In a way, it can serve a purpose similar to that of
#' subsetting and regularization.)
#'
#+ tree_bookmark, include = FALSE
# tree ####
# Note that testing section is giving errors at predict function.
# However, this error does not seem to be related to the response variable
# being coded as a factor or numeric.
# May need to implement rpart() as an alternative.
#+ tree_0_fit_full
library(tree)
fmla_0 <- paste(name_y, paste(names_x_plausible, collapse = " + "), sep = " ~ ")
tree_0_fit_full <- tree(fmla_0, data = d_model)
summary(tree_0_fit_full)
plot(tree_0_fit_full)
text(tree_0_fit_full, pretty = 0)

#'
#' That's quite a "busy" tree. It's difficult to make much from its output.
#'
#' Now, I'll look at a tree for the `fmla` that I've been using before.
#'
#'#+ tree_fit_full
set.seed(42)
tree_fit_full <- tree(fmla, d_model)
summary(tree_fit_full)
# plot(tree_fit_full)
# text(tree_fit_full, pretty = 0)

#+ tree_rpart, include = FALSE
# library(rpart)
# tree_fit_full <- rpart(fmla, data = d_model, method = "class")
# printcp(tree_fit_full)
# plotcp(tree_fit_full)
# plot(tree_fit_full)
# text(tree_fit_full, pretty = 0)
#'
#' Don't necessarily need this since I'm using estimates of test set error.
#' Nevertheless, this is useful for comparing results of predicting 2016
#' for all models.
#'
#+ tree_pred, include = FALSE
vector_train_seq <- seq_along(1:((length(yrs_cv) - 1) * 30))
# if(do_error) {
#   vector_train_seq <- seq_along(1:((length(yrs_cv) - 1) * 30))
#   d_model_test <- d_model_no_fctrs[-vector_train_seq,]
#   d_y_test <- d_model_no_fctrs$result[-vector_train_seq]
#   d_model_train <- d_model_no_fctrs[vector_train_seq,]
#   tree_train <- tree(fmla, d_model_train)
#   # tree_pred_test <- predict(tree_train, d_model_test, type = "class")
#   tree_pred_test <- predict(tree_train, d_model_test)
#   tree_conf_table <- table(tree_pred_test, d_y_test)
# tree_conf_table
# }
#'
#' Now I'll implement cross validation.
#'
#+ tree_cv
# if(do_error) {
#   tree_cv <- cv.tree(tree_fit_full, FUN  = prune.misclass)
#   tree_cv$dev
#
#   tree_prune <- prune.misclass(tree_fit_full, best = length(tree_cv$dev))
#
#   plot(tree_prune)
#   text(tree_prune, pretty = 0)
#
#   # TODO: Extract actual predictions from this.
#   tree_prune_pred_test <- predict(tree_prune, d_model_test, type = "class")
#   tree_prune_conf_table <- table(tree_prune_pred_test, d_y_test)
#   tree_prune_conf_table
#   tree_prune_accuracy_pct <- mean(tree_pred_test == d_model$result)
#   round(as.numeric(tree_prune_accuracy_pct), 2)
# }
# tree_prune_pred_test <- predict(tree_prune, d_model, type = "class")
# tree_prune_conf_table <- table(tree_prune_pred_test, d_model$result)
# tree_prune_conf_table
# tree_prune_accuracy_pct <- mean(tree_pred_test == d_model$result)
# round(as.numeric(tree_prune_accuracy_pct), 2)
#'
#'
#'
#+ rf_bookmark, include = FALSE
# rf ####
# randomForest() will attempt to use regression if the response variable is not
# coded as a factor.
#+ rf_0_fit_full
library(randomForest)
set.seed(42)
rf_0_fit_full <-
  randomForest(
    as.formula(fmla_0), mtry = length(names_x_plausible), data = d_model, importance = TRUE
  )
rf_0_fit_full
importance(rf_0_fit_full)
varImpPlot(rf_0_fit_full)

#+ rf_fit_full
set.seed(42)
rf_fit_full <-
  randomForest(
    as.formula(fmla), mtry = length(names_x), data = d_model, importance = TRUE
  )
rf_fit_full

#' Don't necessarily need this since I'm using estimates of test set error
#+ rf_pred, include = FALSE
d_y_test <- d_model$result[-vector_train_seq]
rf_train <-
  randomForest(
    as.formula(fmla), mtry = length(names_x), data = d_model, subset = vector_train_seq, importance = TRUE
  )

# TODO: Extract actual predictions from this.
rf_pred_test <- predict(rf_train, newdata = d_model[-vector_train_seq,])
rf_conf_table <- table(rf_pred_test, d_y_test)
rf_conf_table
rf_prune_accuracy_pct <- mean(rf_pred_test == d_model$result)
round(as.numeric(rf_prune_accuracy_pct), 2)

#'
#'
#'
#+ boost_bookmark, include = FALSE
# boost ####
# boost() assumes distribution = bernoulli if none is specified.
#+ boost_0_fit_full
library(gbm)

boost_0_fit_full <- gbm(as.formula(fmla_0), distribution = "bernoulli", data = d_model_non_norm, n.trees = 100)
boost_0_fit_full
# if (do_error) {
#   summary(boost_0_fit_full)
#   par(ask = T)
#   gbm.perf(boost_0_fit_full)
#   boost_0_pred <- predict(boost_0_fit_full, newdata = d_model_non_norm, n.trees = 100)
#   boost_0_pred
#   mean((boost_0_pred - d_model$result) ^ 2)
# }

#+ pca_bookmark, include = FALSE
# pca ####
#+ pca_0
summary(d_model_non_norm)
pca_0 <- prcomp(d_model_non_norm, scale = TRUE)
pca_0$center
pca_0$rotation
pca_0_var <- pca_0$sdev^2
pca_0_pve <- pca_0_var / sum(pca_0_var)
pca_0_pve

#+ pca_plot
plot(
  pca_0_pve, xlab = "Principal Component",
  ylab = "Proportion of Variance Explained",
  ylim = c(0, 1),
  type = "b"
)
plot(
  cumsum(pca_0_pve), xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = "b"
)

#+ kmeans_bookmark, include = FALSE
# kmeans_fit_full ####
# kmeans() can deal with both categorical and continuous response variables
#+ kmeans_0_fit_full
set.seed(1)
# Normally, need to set nstart to something like 20, 50, or 100 in order
# to avoid local optimums. However, there is no difference if limiting
# the number of centers to 2.
d_model_0_unsupervised <- dplyr::select(d_model, one_of(c(names_x_plausible, name_y)))
kmeans_0_fit_full <- kmeans(d_model_0_unsupervised, centers = 2, nstart = 100)
mean((kmeans_0_fit_full$cluster - 1) == d_model_0_unsupervised$result)

#+ kmeans_fit_full
set.seed(1)
d_model_unsupervised <- dplyr::select(d_model, one_of(c(names_x, name_y)))
kmeans_fit_full <- kmeans(d_model_unsupervised, centers = 2, nstart = 100)
mean((kmeans_fit_full$cluster - 1) == d_model$result)
plot(dplyr::select(d_model_unsupervised, -result),
     col = (kmeans_fit_full$cluster + 1),
     main = "K-Means Clustering Results with K = 2",
     pch = 20, cex = 2)

#+ kclust_0_fit_full
library(mclust)
kclust_0_fit_full <- Mclust(d_model_0_unsupervised)
summary(kclust_0_fit_full)
# plot(kclust_0_fit_full)
#+ kclust_fit_full
kclust_fit_full <- Mclust(d_model_unsupervised)
summary(kclust_fit_full)
# plot(kclust_fit_full)


#+ hclust_bookmark, include = FALSE
# hclust ####
#+ hclust_0

#+ conclusion_bookmark, include = FALSE
# conclusion ####
#+ compare_picks

d_y_test_compare <- data.frame(lr_pred_test, lda_pred_test$class, svm_pred_test, rf_pred_test)
d_y_test_compare
#'
#' # Conclusion
#'
#' That's all the analysis that I'm going to do on this subject for now.
#' Of course, much more data could be incorporated to potentially improve the
#' models that I've experimented with here. I think some other useful data
#' might be variables indicating the change in team salary from one year to
#' another, the change in mean/medain team age, the amount of injury "luck" the
#' team had in the previous year, etc. Nevertheless, even with more data and
#' better models, I think there is probably a theoretical limit on the accuracy
#' of projecting team win totals around 60%. Sports like basketball are just
#' too random to be able to predict extremely accurately.
#'
#'
#'
#'
