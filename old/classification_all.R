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
#' or under the number of wins projected by sportsbooks.
#' Correctly identifying the outcome of a team's season relative to the
#' sportsbook's win total projection is a highly random task.
#' This is due to the unpredictability of or roster make-up due to
#' in-season injuries to key players,
#' roster changes, player cohesion, etc.
#' The best
#' (professional) sports bettors might only correctly predict
#' 18 (or 60%) of outcomes
#' in a given season on a consistent basis
#' (if they were to bet on all team outcomes),
#' and public (less experienced) bettors are more than likely
#' to hover around 50% in aggreggate.
#' Hence, creating a statistical model that can correctly predict outcomes
#' consistently around 60% would be impressive.
#'
#' The inspiration for this project is multi-faceted:
#'   1. My brother and I record our picks for the over/unders
#'   for each team win total at the
#'   beginning of each NBA (and NFL) season and have wondered how well we might
#'   do if we actaully gambled.
#'   [BEGIN FOOTNOTE]
#'   My brother and I have been picking over/unders for NBA teams dating
#'   back to 2012 and have always been fascinated with sports gambling (although
#'   we have never actually participated ourselves).
#'   I wanted to evaluate
#'   our picks in some way, perhaps to create a model that could make
#'   accurate win total picks for me. If I can create something good enough,
#'   then maybe one day I will be confident enough to actually place a bet!
#'   [END FOOTNOTE]]
#'   2. I wanted a project to help me develop my R skills.
#'   3. I wanted to practice some of the statistical and machine learning
#'   techniques that I have studied.
#'
#' Although we have only been recording my over/under picks
#' since 2012, I will be looking at NBA seasons from 2007 to 2016 because
#' I have sportsbook win total data going back to the 2007 season.
#'
#' [BEGIN FOOTNOTE]
#' Notably, my brother and I also submitted "confidence ratings"
#' gauging our level of confidence
#' in our picks starting in 2014. (These ratings go from 1 to 30, where 1 is
#' our most confident pick and 30 is our least.)
#' Although I would like to use and/or evaluate our "personal" data (i.e. picks)
#' to possibly create better models (perhaps by exposing our internal biases/
#' tendencies and identifying where we might "fade" our initial picks), I won't
#' be using this pick data in this part of the project. I'll leave that
#' for another time. Accordingly, it is not problemmatic that I will be
#' investigating years which we did not record picks
#' (or submit confidence ratings).
#' [END FOOTNOTE]
#'
#' In this portion of the project, I'll evaulate different
#' models while trying to be as consistent as possible in my choice of
#' predictors and my implementation of training, testing and validating
#' data sets. Nevertheless, due to the nature of each model,
#' it can be difficult to be completely consistent.
#'
#' I only do some experimentation with choice of predictors and model
#' formulations in this portion of the project; I'll leave more extensive
#' analysis of model specifications for another time.
#' My primary intent here is to implement different model families
#' used for classification. Nevertheless, I'll use my
#' best judgement and intuition in my selection of model parameters.
#'
#' [BEGIN FOOTNOTE]
#' Finally, as a note to those who are interesting in code style, I attempt to
#' implement the "tidy" workflow
#' promoted by Hadley Wickham and many other prominent R community members.
#' This approach advocates initial steps of
#' importing and tidying data; followed by a cycle
#' of transforming, modeling, and analyzing the data; and, finally,
#' communicating the results.
#' [END FOOTNOTE]
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
#' [BEGIN FOOTNOTE]
#' One might notice a couple of things about my naming of variables:
#'   + I generally use the prefix "d_" in order to distinguish data. This
#' is a personal convention that I picked up from traces of things in other
#' languages (variable naming with VBA and table naming with Oracle SQL).
#'   + I may use the suffix "_raw" if the variable is going to be
#'   manipulated/transformed in some way. Likewise, I may use a suffix like
#'   "_all", "_select", etc. in order to indicate that one variable is a
#'   larger grouping or subset of another similarly named variable.
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
#' [END FOOTNOTE]
#'
#+ test_vars, include = FALSE
do_extra <- FALSE
do_error <- FALSE

#+ import_data
dir_data <- paste0(getwd(), "/data/")
d_win_totals_raw <-
  readRDS(paste0(dir_data, "d_win_totals.rds"))
names(d_win_totals_raw)

d_scraped_stats_classification_raw <-
  readRDS(paste0(dir_data, "d_scraped_stats_classification_raw.rds"))
names(d_scraped_stats_classification_raw)

#'
#' The values in the imported data sets should be fairly self-explanatory
#' given the column names. Perhaps the only ambiguous ones are the `record_id`
#' and `pick_id` fields in `d_win_totals_raw`. These are used for
#' database purposes and are not significant for modeling.
#'
#' # Tidying and Transforming the Data
#'
#' Because I'm attempting to predict results for a given season, it would
#' be incorrect to use team stats recorded during the season. Thus, I create
#' lagged versions of the team stats.
#'
#+ d_scraped_stats_classification
library(tidyverse)

# Creating lagged versions of the team stats.
d_scraped_stats_classification <- d_scraped_stats_classification_raw %>%
  group_by(team) %>%
  mutate(avg_scoring_margin_lag1 = lag(avg_scoring_margin),
         win_pct_close_games_lag1 = lag(win_pct_close_games)) %>%
  ungroup()

# Selecting only the desired columns.
# dplyr's select() function may conflict with other packages.
d_scraped_stats_classification %>% d_scraped_stats_classification
  dplyr::select(-avg_scoring_margin,
                -avg_margin_thru_three_quarters,
                -win_pct_close_games)

#'
#' As I did with the stats data, I create some new variables
#' from the win totals data. These deserve some explanation.
#' + I create a variable called `result` to distinguish
#'   whether or not a team was over or under.
#'   This will be the response variable for the models.
#'   I use a binary 1 and 0 to indicate over and under respectively.
#'   Alternatively, I could use "O" and "U" factors. However, because some
#'   model functions either have trouble with factors or do not
#'   coerce factors naturally, I'll stick with a binary categorical variable,
#'   which can easily be translated to and from a numeric and a factor.
#' + I create lagged variable for team wins and the  for the
#'   same reason that I create lagged variables for the teams stats.
#' + I create a variable called `w_diff2'
#'   for the difference in team wins from the prior year
#'   and the year before that. This variable might have some predictive
#'   significance if it can be deduced (by the model)
#'   that there is a discernable pattern
#'   in the way sportsbooks respond to changes in a given team's wins from one
#'   season to another. In order to make this kind of metric useful for
#'   predictive purposes, it cannot simply use the difference in the number
#'   of wins from the current season and the previous season; otherwise, this
#'   variable would be highly correlated with the `result` variable and would
#'   be identified as a "descriptive" variable.
#' + I create a variable called `w_sportsbook_err1`
#'   for the difference in the team's actual number of wins and the
#'   sportsbook's win total from the previous year for similar reasons.
#'   This variable could have some explanatory power if it can be deduced
#'   (by the model) that sportsbooks are influenced by
#'   their relative performance
#'   for a given team in the previous year when determining
#'   the win total for that team.
#'   Notabley, this variable may be found to be
#'   highly collinear with the `w_diff2` variable because they
#'   portray the same kind of information.
#' + I create variables `prob_implied_over` and `prob_implied_under`
#'   for the probabilites of a team going over or under
#'   its win total implied by the money lines for each outcome.
#'   [BEGIN FOOTNOTE]
#'   Insert link to a website explaining implied probability.
#'   [END FOOTNOTE]
#'   Then, because these variables are typically highly collinear, I combine
#'   them into a singular variable `prob_implied_combo`
#'   [BEGIN FOOTNOTE]
#'   If the money line for a given outcome
#'   is higher than its typical value of -110 (which is
#'   typically an indiciation that the public is heavily betting
#'   that outcome), then the money line for the opposite outcome is
#'   likely to be lower than the typical value of -110
#'   (because sportsbooks are attempting to induce more even
#'   action on the outcome).
#'   Possibly insert a link here.
#'   [END FOOTNOTE]
#' + I create a binary `underdog_combo` variable
#'   to indicate whether or not the over/under money lines
#'   (and their corresponding implied probabilities) indicate
#'   that the team is an "underdog" to achieve the given outcome
#'   (either over or under). This is implied by the money line
#'   being less than the nominal value of -110.
#'   As with the `w_diff2` and `w_sporstbook_err1` variables, it might
#'   be found that this variable is highly collinear with the
#'   `prob_implied_combo` variable because it essentially portrays the same
#'   kind of information. Nevertheless, it might be useful as a
#'   separate varaible because the granularity offered by the value
#'   of the `prob_implied_combo` variable may or may not be useful.
#'
#+ d_win_totals
# The inner_join might not work if the team columns is not of the same type
# in each data set. (It must be either a character/factor in both data sets.)
d_win_totals <- d_win_totals_raw %>%
  # mutate(result = ifelse(w > w_sportsbook, "O", "U")) %>%
  mutate(result = ifelse(w > w_sportsbook, 1, 0)) %>%
  group_by(team) %>%
  mutate(result_lag1 = lag(result),
         w_lag1 = lag(w),
         w_diff2 = (lag(w) - lag(w, 2)),
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
    underdog_combo = ifelse(odds_over < -110, 1, 0))
  )

# Could replace years with no odds with a constant values such as -110,
# but choosing to exclude them instead.
# d_win_totals$odds_over <-
#   as.numeric(str_replace_all(d_win_totals$odds_over, "na", -110))
# d_win_totals$odds_under <-
#   as.numeric(str_replace_all(d_win_totals$odds_under, "na", -110))
#'
#' Finally, I join the stats and win totals data sets and reduce the
#' combined data sets to only the predictor variables
#' and the response variable `result` that will be used for modeling.
#' The `yr_start` and `team` variables are only needed to perform the
#' join and are not required for modeling. (Although the
#' `yr_start` variable can and will be used for splitting the entire data set
#' into training and testing sets, random sampling could be used if it
#' were decided that `yr_start` should not be used whatsoever).
#'
#+ d_join_bookmark, include = FALSE
# d_join ####
#+ d_join
cols_keep <- c("yr_start", "team",
               "w_lag1", "w_diff2", "result_lag1", "w_sportsbook_err1",
               "prob_implied_combo", "underdog_combo",
               "avg_scoring_margin_lag1", "win_pct_close_games_lag1",
               "result")

d_join <- d_win_totals %>%
  inner_join(d_scraped_stats_classification, by = c("yr_start", "team")) %>%
  dplyr::select(one_of(cols_keep))

#'
#+ d_scraped_stats_raw, include = FALSE
# d_scraped_stats_raw <-
#   readRDS(paste0(dir_data, "d_scraped_stats_raw.rds"))
# cols_join <- c("team", "yr_start")  # essential variables for joins
# cols_stats <- c("off_efficiency", "def_efficiency")
# d_scraped_stats <- d_scraped_stats_raw %>%
#   dplyr::select(one_of(c(cols_join, cols_stats)))
#'
#' In order to make my joined data set more friendly for modeling,
#' I'm going to remove the rows that have NAs and make sure that there
#' are no duplicate rows.
#'
#' Additionally, I think it is better to use normalized (or standardized)
#' data. There is never really a right answer to whether or not normalizing
#' the data is the correct option. The choice can vary depending on the type
#' of model, the choice of predictors, the nature of the response variable,
#' etc. Nevertheless, I'm choosing to normalize here, primarily because
#' the raw values for differences in wins and implied probabilities are so
#' different that the results of any given model may be more difficult to
#' interpret than they would be otherwise.
#'
#' Finally, I'm converting the response variable `result` to a factor because
#' the majority of model functions used for classification rely on the
#' response variable being distinguised as a factor.
#'
#' [BEGIN FOOTNOTE]
#' This may seem at odds
#' with my previous choice to use 0 and 1 to represent under and over. However,
#' the reality is that factors automatically get indexed beginning at 1
#' (such that the next factor is labeled internally as 2), so,
#' in my opintion, using
#' "U" and "O" gives me less flexibility with converting to and from numerics
#' because I would like to normally use the
#' levels created by the factor coercion (i.e. "1" and "2"), but these are
#' not ideal for me in this case. I prefer the labels "0" and "1", which are
#' still coerced to "1" and "2" internally, but if I need to use numerics, then
#' character labels such as "U" and "O" can be difficult to work with.
#' [END FOOTNOTE]
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

#+ save_data, include = FALSE
saveRDS(d_join, file = paste0(dir_data, "d_join_classification", ".rds"))
saveRDS(d_model, file = paste0(dir_data, "d_model_classification", ".rds"))
#'
#+ remove_temp_data, include = FALSE
# rm(list = ls()[!(ls() %in% c("dir_data", "d_model"))])
#'
#' Ok. It looks like my data is "tidied" enough to finally get to modeling.
#'
#' # Modeling the Data
#'
#' As I stated before, I'm going to be using the
#' `result` variable as the response variable that I'll be trying to predict.
#' But which predictors to choose? That choice depends on the type of model.
#'
#' ## Choosing the "Optimal" Predictors
#'
#' ### Co-Variance
#'
#' In order to create robust models, it is best to reduce the
#' set of predictors
#' to only those which are most predictive and, consequently, most useful for
#' models; otherwise, one may incidentally "overfit" a model and introduce
#' undesireable variance.
#' One might say that
#' Thus, it is often necessary (or, at the very least, highly
#' recommended) to remove or transform variables exhibiting collinearity.
#' (Another option  might be to modify the redundant variables in some way.)
#'
#' With this in mind, I'll start by looking at correlations
#' between all possible predictor variables in my data.
#'
#' [BEGIN FOOTNOTE]
#' This is one of the places where converting all variables to numerics
#' (including the lagged version of `result` is useful because I can
#' use the `cor` function from the `stats` package without encountering
#' an error due to factors.)
#' [END FOOTNOTE]
#'
#+ correlations_bookmark, include = FALSE
# correlations ####
#+ correlations
names_x_all <-
  c(
    "w_lag1", "w_diff2", "result_lag1", "w_sportsbook_err1",
    "prob_implied_combo", "underdog_combo",
    "avg_scoring_margin_lag1", "win_pct_close_games_lag1"
  )
d_model_x <- d_model %>% dplyr::select(one_of(names_x_all))

d_cor <- round(cor(d_model_x), 2)
d_cor
#'
#' Furthermore, as another method of gauging independence among predictors, the
#' residuals among variables can be plotted and reviewed
#' for non-zero variance.
#'
#+ correlations_plot
# A visualization of variable relationships.
# library(GGally)
# ggpairs(d_model_x, lower = list(combo = wrap("facethist", binwidth = 30))

#'
#' It turns out that there is some strong collinearity between
#' `result_lag1` and `w_sportsbook_err1`, as well as between
#' `prob_implied_combo` and `underdog_combo`. As I noted before,
#' this actually isn't all that unexpected since each pair of variables
#' portrays similar information.
#'
#' ### Subsetting
#'
#' Next, as an alternative (or supplement)
#' to reviewing co-variance values and vizualizing
#' residuals among the possible predictors in an attempt to
#' determine the "optimal" set of predictors,
#' I could take a more systematic approach.
#' I could run a subsetting algorithm on all possible predictors
#' and review its results. Because the number of predictors and observations
#' is relatively small, I'll use a rigorous algorithm instead of a
#' faster forward- or backward-step approach.
#'
#' I'll use the `leaps` package to perform subsetting (although the
#' `regsubset` function requires inputs to be matrices).
#'
#+ subset_fit_full
library(leaps)
name_y <- "result"
d_x <- data.matrix(dplyr::select(d_model, one_of(names_x_all)))
d_y <- data.matrix(dplyr::select(d_model, one_of(name_y)))

# Set nvmax equal to the maximum allowable number of variables.
subset_fit_full <- regsubsets(d_x, d_y, nvmax = length(names_x_all))

#'
#' There are several different criteria that could be used to choose model
#' parameters. I'll look at Adjusted R^2, Cp, and BIC, which are each
#' conveniently calculated by `regsubsets`. These values provide
#' estimates of the test set error of the model.
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

#+ subset_plots, include = FALSE
# par(mfrow = c(3, 1))
# plot(subset_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
# points(which.max(subset_summary$adjr2), subset_summary$adjr2[which.max(subset_summary$adjr2)],
#        col = "red", cex = 2, pch = 20)
# plot(subset_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
# points(which.min(subset_summary$cp), subset_summary$cp[which.min(subset_summary$cp)],
#        col = "red", cex = 2, pch = 20)
# plot(subset_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
# points(which.min(subset_summary$bic), subset_summary$bic[which.min(subset_summary$bic)],
#        col = "red", cex = 2, pch = 20)
# par(mfrow = c(1, 1))
# plot(subset_fit_full, scale = "adjr2")
# plot(subset_fit_full, scale = "Cp")
# plot(subset_fit_full, scale = "bic")
#'
#' It looks like these critiera each select a small number of parameters, and
#' the parameters that they identify are inclusive with one another.
#' If I had to choose a "best" set of predictors given the information from
#' this statistical approach combined with my own intuition, I would
#' choose the `w_sportsbook_err1` and `prob_implied_combo` variables.
#'
#' These variables are indicated to be among the best for prediction
#' purposes by the subsetting algorithm. Also, I favor the potential
#' informational value added by the granularity
#' provided by these variables relative to their collinear
#' categorical counterparts (i.e. `result_lag1` are `underdog_combo`).
#'
#' Regarding my choice of two variables instead of one or three or more,
#' I think using a single variable is a bit too simplistic.
#' On the other hand, using
#' three or more predictors might create introduce more uncertainty
#' (in a given model) than desired.
#'
#' Now, because the previous outputs were fairly dense and may have
#' been difficult to process, I'll review the co-variances
#' and residuals among  only my choice of predictors in order to verify
#' that they are independent of one another.
#'
#+ correlations_2
names_x <- c("w_sportsbook_err1", "prob_implied_combo")
d_model_x_2 <- d_model %>% dplyr::select(one_of(names_x))

d_cor_2 <- hetcor(as.data.frame(d_model_x_2))
d_cor_2$correlations <- round(d_cor_2$correlations, 2)
d_cor_2$correlations

#+ correlations_plot_2
# library(GGally)
# ggpairs(d_model_x_2, lower = list(combo = wrap("facethist", binwidth = 30)))
#'
#'
#' Alright, that's enough of that. Let's get to some model experimentation.
#'
#' ## Logisitic Regression
#'
#' I'll start with a fairly well known family of models: logistic
#' regression. I'm simply going to use my choice of predictors identified
#' previously. Before I attempt any kind of training and testing
#' (in order to avoid over-fitting and undestimating the
#' test error of the model), I'll naively fit a model on the entire data set.
#' The `glm` function can be used to implement lositic regression when its
#' agrument `family = binomial`.
#'
#' model_fmla_paste
name_y <- "result"
names_x <- c("w_sportsbook_err1", "prob_implied_combo")
fmla <- paste(name_y, paste(names_x, collapse = " + "), sep = " ~ ")
#' model_fmla_explicit, include = FALSE
# fmla <- "result ~ w_sportsbook_err1 + prob_implied_combo"
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
#+ lr_choice_fit_full
lr_choice_fit_full <- glm(fmla, data = d_model, family = binomial)
summary(lr_choice_fit_full)$coef
coef(lr_choice_fit_full)
#'
#' It looks like this model isn't all too bad. It certainly could be worse.
#' However, even after my process of trying to identify the best predictors
#' to use heuristically, the p-values indicate that only one of the predictors
#' is signficant. This actually might not be a flaw in my selection process.
#' It could just be that there isn't a great combination of predictors out of
#' those available. Anyways, I'll disregard further
#' formula experimentation for now.
#'
#' Now, I'll use `predict` to calculate the mean percentage error in the
#' fitted values of this model.
#' Traditionally, one might choose the value for the barrier
#' between choosing between two categories for a categorical response
#' variable to be the midpoint of the two values (i.e. in this case, 0.5).
#' However, in this context, I think choosing 50% for each side is arguably
#' more appropriate. Also, one might argue that using the mean probability
#' reduces sensitivity to model parameters if the model predicts
#' probabilities that are distributed heavily toward one response value.
#'
#+ lr_choice_pred_full
lr_choice_prob_full <- predict(lr_choice_fit_full, type = "response")
# lr_choice_pred_full <- rep("U", dim(d_model)[1])
lr_choice_pred_full <- rep(0, dim(d_model)[1])
# lr_choice_pred_full[lr_choice_prob_full > 0.5] = 1
# lr_choice_pred_full[lr_choice_prob_full > mean(lr_choice_prob_full)] = "O"
lr_choice_pred_full[lr_choice_prob_full > mean(lr_choice_prob_full)] = 1
table(lr_choice_pred_full, d_model$result)
round(mean(lr_choice_pred_full == d_model$result), 2)
#'
#' So it isn't the worst model one could possibly come up with. Atleast it
#' doest better than 50% in predicting over/unders when
#' trained and tested on the full data set.
#'
#' But how much worse (or better) does the prediction performance get when
#' properly trained and tested on different sets? I'll forego the creation of
#' a validation set for now since I'm not explicity trying to refine
#' some cost parameter or another kind of tuning parameter. Instead,
#' I'm simply going to split the data into train and test sets. This could be
#' done in a number of ways, such as random sampling. However,
#' I'm going to use a single year's worth of data
#' as a test set and the rest as the train set. I'll do this for every year.
#' This is essentially k-fold cross validation (CV).
#' More specifically, because there
#' are eight years in this data set, this is 8-fold CV.

#+ lr_accuracy_pct
yrs_cv <- 2009:2016
lr_choice_accuracy_pct <- list()
for (i in 1:length(yrs_cv)) {
  vector_train <- !(d_model$yr_start %in% yrs_cv[i])

  d_model_test <- d_model[!vector_train,]
  d_y_test <- d_model$result[!vector_train]
  d_model_train <- d_model[vector_train,]
  lr_choice_fit_train <-
    glm(fmla, data = d_model_train, family = binomial)
  lr_choice_prob_test <-
    predict(lr_choice_fit_train, d_model_test, type = "response")
  # lr_choice_pred_test <- rep("U", dim(d_model_test)[1])
  lr_choice_pred_test <- rep(0, dim(d_model_test)[1])
  # lr_choice_pred_test[lr_choice_prob_test > mean(lr_choice_prob_test)] = "O"
  lr_choice_pred_test[lr_choice_prob_test > mean(lr_choice_prob_test)] = 1
  lr_choice_accuracy_pct[i] <- mean(lr_choice_pred_test == d_y_test)
  lr_choice_conf_matrix <- table(lr_choice_pred_test, d_y_test)
  if (i > 1) {
    lr_choice_conf_matrix_all <-
      data.frame(
        rbind(lr_choice_conf_matrix_all,
              cbind(
                data.frame(yr_start = rep(yrs_cv[i], length(lr_choice_conf_matrix))),
                lr_choice_conf_matrix
              )))
  } else {
    lr_choice_conf_matrix_all <-
      data.frame(
        cbind(data.frame(yr_start = rep(yrs_cv[i], length(lr_choice_conf_matrix)
        )),
        lr_choice_conf_matrix))
  }
}

round(mean(as.numeric(lr_choice_accuracy_pct)), 2)
round(as.numeric(lr_choice_accuracy_pct), 2)
lr_choice_conf_matrix_all <- lr_choice_conf_matrix_all %>%
  rename(pred_test = lr_choice_pred_test, count = Freq)
# lr_choice_conf_matrix_all
#'
#' Actually, I can use the `cv.glm` function in the `boot` package
#' to perform cross validation. It performs k-fold cross validation with
#' a default value of k = n, where n = # of observations. This is also known
#' as leave-one-out-cross-validation (LOOCV).
#'
#+ lr_choice_cv_loocv
library(boot)
# Setting the seed to create reproducible results.
set.seed(42)
lr_choice_cv_loocv <- cv.glm(data = d_model, glmfit = lr_choice_fit_full)
summary(lr_choice_cv_loocv)
lr_choice_cv_loocv$delta[1]
#'
#' Now I'll do a k-fold cross-fold validation with k =- 10.
#+ lr_choice_cv_k10
set.seed(42)
lr_choice_cv_k10 <- cv.glm(data = d_model, glmfit = lr_choice_fit_full, K = 10)
summary(lr_choice_cv_k10)
1 - sqrt(lr_choice_cv_k10$delta[1])
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
lda_conf_matrix <- table(lda_pred_full$class, d_model$result)
lda_conf_matrix

# The line below doesn't work due to NA's in the data set.
# mean((lda_pred_full$posterior == d_model$result), na.rm = TRUE)
(sum(lda_conf_matrix[1, 1] + lda_conf_matrix[2, 2]) / sum(lda_conf_matrix))

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
  lda_conf_matrix <- table(lda_pred_test$class, d_y_test)
  lda_accuracy_pct[i] <-
    (sum(lda_conf_matrix[1, 1] + lda_conf_matrix[2, 2]) / sum(lda_conf_matrix))
  if (i > 1) {
    lda_conf_matrix_all <-
      data.frame(
        rbind(lda_conf_matrix_all,
              cbind(
                data.frame(yr_start = rep(yrs_cv[i], length(lda_conf_matrix))),
                lda_conf_matrix
              )))
  } else {
    lda_conf_matrix_all <-
      data.frame(
        cbind(data.frame(yr_start = rep(yrs_cv[i], length(lda_conf_matrix)
        )),
    lda_conf_matrix))
  }
}
lda_accuracy_pct <- unlist(lda_accuracy_pct)
round(mean(as.numeric(lda_accuracy_pct), na.rm = TRUE), 2)
round(as.numeric(lda_accuracy_pct), 2)
lda_conf_matrix_all
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
lda_conf_matrix <- table(lda_pred_test$class, d_y_test)
lda_conf_matrix

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
  svm_conf_matrix <- table(svm_pred_test, d_y_test)
  svm_accuracy_pct[i] <- mean(svm_pred_test == d_y_test)
  if (i > 1) {
    svm_conf_matrix_all <-
      data.frame(rbind(
        svm_conf_matrix_all,
        cbind(data.frame(yr_start = rep(
          yrs_cv[i], length(svm_conf_matrix)
        )),
        svm_conf_matrix)
      ))
  } else {
    svm_conf_matrix_all <-
      data.frame(cbind(data.frame(yr_start = rep(
        yrs_cv[i], length(svm_conf_matrix)
      )),
      svm_conf_matrix))
  }
}

round(mean(as.numeric(svm_accuracy_pct), na.rm = TRUE), 2)
round(as.numeric(svm_accuracy_pct), 2)
svm_conf_matrix_all <- svm_conf_matrix_all %>%
  rename(pred_test = svm_pred_test, count = Freq)
# svm_conf_matrix_all
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
#+ tree_all_fit_full
library(tree)
fmla_all <- paste(name_y, paste(names_x_all, collapse = " + "), sep = " ~ ")
tree_all_fit_full <- tree(fmla_all, data = d_model)
summary(tree_all_fit_full)
plot(tree_all_fit_full)
text(tree_all_fit_full, pretty = 0)

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
# d_model_no_fctrs <- d_model
# d_model_no_fctrs$result <- as.numeric(d_model_no_fctrs$result)
# if(do_error) {
#   vector_train_seq <- seq_along(1:((length(yrs_cv) - 1) * 30))
#   d_model_test <- d_model_no_fctrs[-vector_train_seq,]
#   d_y_test <- d_model_no_fctrs$result[-vector_train_seq]
#   d_model_train <- d_model_no_fctrs[vector_train_seq,]
#   tree_train <- tree(fmla, d_model_train)
#   # tree_pred_test <- predict(tree_train, d_model_test, type = "class")
#   tree_pred_test <- predict(tree_train, d_model_test)
#   tree_conf_matrix <- table(tree_pred_test, d_y_test)
# tree_conf_matrix
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
#   tree_prune_conf_matrix <- table(tree_prune_pred_test, d_y_test)
#   tree_prune_conf_matrix
#   tree_prune_accuracy_pct <- mean(tree_pred_test == d_model$result)
#   round(as.numeric(tree_prune_accuracy_pct), 2)
# }
# tree_prune_pred_test <- predict(tree_prune, d_model, type = "class")
# tree_prune_conf_matrix <- table(tree_prune_pred_test, d_model$result)
# tree_prune_conf_matrix
# tree_prune_accuracy_pct <- mean(tree_pred_test == d_model$result)
# round(as.numeric(tree_prune_accuracy_pct), 2)
#'
#'
#'
#+ rf_bookmark, include = FALSE
# rf ####
# randomForest() will attempt to use regression if the response variable is not
# coded as a factor.
#+ rf_all_fit_full
library(randomForest)
set.seed(42)
rf_all_fit_full <-
  randomForest(
    as.formula(fmla_all), mtry = length(names_x_all), data = d_model, importance = TRUE
  )
rf_all_fit_full
importance(rf_all_fit_full)
varImpPlot(rf_all_fit_full)

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
rf_conf_matrix <- table(rf_pred_test, d_y_test)
rf_conf_matrix
rf_prune_accuracy_pct <- mean(rf_pred_test == d_model$result)
round(as.numeric(rf_prune_accuracy_pct), 2)

#'
#'
#'
#+ boost_bookmark, include = FALSE
# boost ####
# boost() assumes distribution = bernoulli if none is specified.
#+ boost_all_fit_full
library(gbm)

boost_all_fit_full <- gbm(as.formula(fmla_all), distribution = "bernoulli", data = d_model_non_norm, n.trees = 100)
boost_all_fit_full
# if (do_error) {
#   summary(boost_all_fit_full)
#   par(ask = T)
#   gbm.perf(boost_all_fit_full)
#   boost_all_pred <- predict(boost_all_fit_full, newdata = d_model_non_norm, n.trees = 100)
#   boost_all_pred
#   mean((boost_all_pred - d_model$result) ^ 2)
# }

#+ pca_bookmark, include = FALSE
# pca ####
#+ pca_all
summary(d_model_non_norm)
pca_all <- prcomp(d_model_non_norm, scale = TRUE)
pca_all$center
pca_all$rotation
pca_all_var <- pca_all$sdev^2
pca_all_pve <- pca_all_var / sum(pca_all_var)
pca_all_pve

#+ pca_plot
plot(
  pca_all_pve, xlab = "Principal Component",
  ylab = "Proportion of Variance Explained",
  ylim = c(0, 1),
  type = "b"
)
plot(
  cumsum(pca_all_pve), xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = "b"
)

#+ kmeans_bookmark, include = FALSE
# kmeans_fit_full ####
# kmeans() can deal with both categorical and continuous response variables
#+ kmeans_all_fit_full
set.seed(1)
# Normally, need to set nstart to something like 20, 50, or 100 in order
# to avoid local optimums. However, there is no difference if limiting
# the number of centers to 2.
d_model_all_unsupervised <- dplyr::select(d_model, one_of(c(names_x_all)))
kmeans_all_fit_full <- kmeans(d_model_all_unsupervised, centers = 2, nstart = 100)
mean((kmeans_all_fit_full$cluster - 1) == d_model$result)

#+ kmeans_fit_full
set.seed(1)
d_model_unsupervised <- dplyr::select(d_model, one_of(c(names_x)))
kmeans_fit_full <- kmeans(d_model_unsupervised, centers = 2, nstart = 100)
mean((kmeans_fit_full$cluster - 1) == d_model$result)
plot(d_model_unsupervised,
     col = (kmeans_fit_full$cluster + 1),
     main = "K-Means Clustering Results with K = 2",
     pch = 20, cex = 2)

#+ kclust_all_fit_full
library(mclust)
kclust_all_fit_full <- Mclust(d_model_all_unsupervised)
summary(kclust_all_fit_full)
# plot(kclust_all_fit_full)
#+ kclust_fit_full
kclust_fit_full <- Mclust(d_model_unsupervised)
summary(kclust_fit_full)
# plot(kclust_fit_full)


#+ hclust_bookmark, include = FALSE
# hclust ####
#+ hclust_all_fit_full
hclust_all_fit_full_complete <- hclust(dist(d_model_all_unsupervised), method = "complete")
plot(hclust_all_fit_full_complete, main = "Complete", sub = "")
cutree(hclust_all_fit_full_complete, 2)

#+ hclust_fit_full
hclust_fit_full_complete <- hclust(dist(d_model_unsupervised), method = "complete")
plot(hclust_fit_full_complete, main = "Complete", sub = "")
cutree(hclust_fit_full_complete, 2)

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
