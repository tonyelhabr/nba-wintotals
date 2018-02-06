# Classification
Tony  




# Introduction

This project describes my experimentation/experience with creating
classification models to predict whether or not an NBA team will go over
or under the number of wins projected by sportsbooks. The inspiration for
this project is multi-faceted:
  1. My brother and I have been picking over/unders for NBA teams dating
  back to 2012 and have always been fascinated with sports gambling (alhtough
  we have never actually participated ourselves). I wanted to evaluate
  our picks in some way, perhaps to create a model that could make
  accurate win total picks for me. If I can create something good enough,
  then maybe one day I will be confident enough to actually place a bet!
  2. I wanted a project to help me develop my R skills.
  3. I wanted to practice some of the statistical and machine learning
  techniques that I have studied and read about all the time.

Alhtough my brother and I have only been recording our over/under picks
since 2012, I will be looking at NBA seasons from 2007 to 2016 because
I have win totals going back to the 2007 season. Notably, my brother and I
also submitted "confidence ratings" gauging our level of confidence
in our picks starting in 2014. (These ratings go from 1 to 30, where 1 is
our most confident pick and 30 is our least.)

Although I would like to use and/or evaluate our "personal" data (i.e. picks)
to possibly create better models (perhaps by exposing our internal biases/
tendencies and identifying where we might "fade" our initial picks), I won't
be using this pick data in this part of the project. I'll leave that
for another time. Thus, it is not problemmatic that I will be
investigating years which we did not record picks
(or submit confidence ratings). Nevertheless, some evidence of code that
includes some of this "personal" data may be evident in the code that follows
because I evaluated it during the development of this project.

In this portion of the project, I'll evaulate different
models while keeping the model formula constant. Although choice of model
formula is a significant decision in and of itself, my purpose here is to
compare classification models. I'll evaluate differen formulas
in another portion of this project. Nevertheless, I'll use my
best judgement and intuition in my selection of the model formula to be used.

Finally, as a note to those who are interesting in code style, I attempt to
implement the "tidy" workflow
promoted by Hadley Wickham and many other prominent R community members.
This approach advocates initial steps of
importing and tidying data; followed by a cycle
of transforming, modeling, and analyzing the data; and, finally,
communicating the results.


# Importing the Data






I start by loading in the data set for stats and team win totals. I
have saved this data in local rds files for use across multiple projects.

One might notice a couple of things about my naming of variables:
  + I generally use the prefix "d_" in order to distinguish data. This
is a personal convention that I picked up from traces of things in other
languages (variable naming with VBA and table naming with Oracle SQL).
  + I tend to use the suffix "_raw" if the variable is going to be
  manipulated/transformed in some way.
  + Sometimes, when I create a new variable that copies all or most of the
  data from an existing variable, I append the new variable's name with
  the suffix "_2", "_3", etc. I do this for one of two reason:
    1. This might be done for ease in debugging/developing the code because
    it helps me confirm that the changes that I intended to make actually
    do take effect when re-running a certain chunk of code multiple times.
    The variables that get created in this manner might be deemed
    "temporary".
    I realize that this is not exactly a "best practice", but it works
    for me.
    2. At other times, such as when I'm looking at different models that are
    from the same family of models, then I'll use the suffix to simply
    distinguish that the variable is similar to another variable (and has
    the same dimenstions, etc.) but there is something intentionally
    different about the way it has been defined.



```r
dir_data <- paste0(getwd(), "/data/")
d_win_totals_raw <- readRDS(paste0(dir_data, "d_win_totals.rds"))
p_win_totals_raw <- readRDS(paste0(dir_data, "p_win_totals.rds"))
```


# Tidying and Transforming the Data

Now I'll tidy and transform.



```r
library(tidyverse)

# Selecting only the desired columns.
# dplyr's select() function may conflict with other packages.


p_win_totals <- p_win_totals_raw %>%
  dplyr::select(one_of(c("pick_id",
                         "person",
                         "pick",
                         "confidence")))

yrs <- 2007:2016
# Not only selecting the desired columns, but also adding
# some variables to (possibly) be used in models.
# The inner_join might not work if the team columns is not of the same type
# in each data set. (It must be either a character/factor in both data sets.)
d_win_totals <- d_win_totals_raw %>%
  filter(yr_start %in% yrs) %>%
  dplyr::select(
    one_of(
      "pick_id",
      "yr_start",
      "team",
      "w",
      "w_sportsbook",
      "odds_over",
      "odds_under"
    )
  ) %>%
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
  mutate(result =
           ifelse(w > w_sportsbook, "O", "U")) %>%
  # ifelse(w > w_sportsbook, 1, -1)) %>%
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

d_win_totals$result <- factor(d_win_totals$result)
d_win_totals$result_lag1 <- factor(d_win_totals$result_lag1)
d_win_totals$underdog_combo <- factor(d_win_totals$underdog_combo)

# Could replace years with no odds with a constant values such as -110,
# but choosing to exclude them instead.
# d_win_totals$odds_over <-
#   as.numeric(str_replace_all(d_win_totals$odds_over, "na", -110))
# d_win_totals$odds_under <-
#   as.numeric(str_replace_all(d_win_totals$odds_under, "na", -110))

# The "pick_id" field in the rds data sets serves as a primary key
# upon which the pick data can be joined with the other data.
# Converting a confidence rating to a win number for potential use in modeling.
d_join <- d_win_totals %>%
  left_join(p_win_totals, by = "pick_id") %>%
  dplyr::select(-pick_id) %>%
  mutate(
    pick_result =
      ifelse(pick == result, 1, 0),
    w_pick_confidence =
      ifelse(
        confidence == 0,
        ifelse(
          pick == "O",
          round(w_sportsbook + 1.5, 0),
          round(w_sportsbook - 1.5, 0)
        ),
        ifelse(pick == "O",
               round(w_sportsbook + (3 - confidence / 10), 0),
               round(w_sportsbook - (3 - confidence / 10), 0))
      )
  )
# pick_int = ifelse(pick == "O", 1, -1)) %>%
# dplyr::select(-pick) %>%
# rename(pick = pick_int)
```






Now I'll save this data for potential use in other projects.



```r
saveRDS(d_join, file = paste0(dir_data, "d_join_classification", ".rds"))
```


# Modeling the Data

Because there is duplicate information in my final tidy data set due
to there being two people for whom picks were tracked, I'll filter the
data set to just distinct results. I'm not going to evaluate the picks that
my brother and I made yet (even though I did some calculations with this
data above).



```r
d_model <- d_join %>% dplyr::select(-team, -person, -pick, -confidence, -pick_result, 
    -w_pick_confidence) %>% distinct()
```



For my analysis, I'm going to use the `result` variable as the response
variable that I'll be trying to predict, and only a handful of the
remaining variables as predictors. But which predictors to choose?
Let's look at some correlations between possible predictor variables.



```r
names_x_plausible <- c("w_lag1", "w_diff2", "result_lag1", "w_sportsbook_err1", 
    "prob_implied_combo", "underdog_combo")
d_model_x <- d_model %>% dplyr::select(one_of(names_x_plausible))

library(polycor)
# Need as.data.frame() because tibbles are interpreted as lists.
d_cor <- hetcor(as.data.frame(d_model_x))
d_cor$correlations <- round(d_cor$correlations, 2)
d_cor$correlations
```

```
##                    w_lag1 w_diff2 result_lag1 w_sportsbook_err1
## w_lag1               1.00    0.45       -0.50              0.56
## w_diff2              0.45    1.00       -0.67              0.65
## result_lag1         -0.50   -0.67        1.00             -0.99
## w_sportsbook_err1    0.56    0.65       -0.99              1.00
## prob_implied_combo  -0.08    0.00       -0.01             -0.03
## underdog_combo       0.12    0.02       -0.02              0.06
##                    prob_implied_combo underdog_combo
## w_lag1                          -0.08           0.12
## w_diff2                          0.00           0.02
## result_lag1                     -0.01          -0.02
## w_sportsbook_err1               -0.03           0.06
## prob_implied_combo               1.00          -0.98
## underdog_combo                  -0.98           1.00
```


It turns out that there is some strong collinearity between
`result_lag1` and `w_sportsbook_err1`, as well as between
`prob_implied_combo` and `underdog_combo`.
This actually isn't all that expected since each pair of variables
essentially portrays the same information.

In order to create robust models (which often depend on
independence among predictors), it would be a good idea to remove
one of the variables in a pair of variables exhibiting collinearity.
Another option  might be to modify the redundant variables in some way.
I'll opt for the latter option and neglect some variables for future
consideration. I'll stick with the `w_sporstbook_err1` and
`prob_iimplied_combo` variables since they are more granular.
`result_lag1` are `underdog_combo` are both factors, while their
counterparts are not.

I'm also going to remove the `w_lag1` variable for intuitional reasons.
The raw number of wins that a team had in the previous season
shouldn't have any effect on  whether or not the team exceeds
its win total in the current season.



```r
names_x <- c("w_sportsbook_err1", "prob_implied_combo")
d_model_x_2 <- d_model %>% dplyr::select(one_of(names_x))

d_cor_2 <- hetcor(as.data.frame(d_model_x_2))
d_cor_2$correlations <- round(d_cor_2$correlations, 2)
d_cor_2$correlations
```

```
##                    w_sportsbook_err1 prob_implied_combo
## w_sportsbook_err1               1.00              -0.04
## prob_implied_combo             -0.04               1.00
```

```r
# A visualization of variable relationships.
library(GGally)
ggpairs(d_model_x_2)
```

![](figs/correlations_plot-1.png)<!-- -->

```r
# A visualization of just the correlations.  ggcorr(d_cor$correlations,
# palette = 'RdBu', label = TRUE)
```



Alternatively, I could have taken a more systematic approach.
I could run a subsetting algorithm on all possible predictors
and see what turns out to be "optimal".



```r
library(leaps)
name_y <- "result"
names_exclude <- c("yr_start", "result", "w", "w_sportsbook")
names_x_all <- names(d_model)[!(names(d_model) %in% names_exclude)]

# Cannot have NA's in data.matrix
d_x <- data.matrix(dplyr::select(filter(d_model, yr_start > 2008), one_of(names_x_all)))
d_y <- data.matrix(dplyr::select(filter(d_model, yr_start > 2008), result))
subset_fit_full <- regsubsets(d_x, d_y, nvmax = length(names_x_all))
```

```
## Reordering variables and trying again:
```


There are several different criteria that could be used to choose model
parameters. I'll look at Adjusted R^2, Cp, and BIC.



```r
subset_summary <- summary(subset_fit_full)
subset_summary
```

```
## Subset selection object
## 10 Variables  (and intercept)
##                    Forced in Forced out
## odds_over              FALSE      FALSE
## odds_under             FALSE      FALSE
## w_lag1                 FALSE      FALSE
## w_diff2                FALSE      FALSE
## result_lag1            FALSE      FALSE
## w_sportsbook_err1      FALSE      FALSE
## prob_implied_over      FALSE      FALSE
## prob_implied_under     FALSE      FALSE
## underdog_combo         FALSE      FALSE
## prob_implied_combo     FALSE      FALSE
## 1 subsets of each size up to 9
## Selection Algorithm: exhaustive
##          odds_over odds_under w_lag1 w_diff2 result_lag1 w_sportsbook_err1
## 1  ( 1 ) " "       " "        " "    " "     " "         " "              
## 2  ( 1 ) " "       " "        " "    "*"     " "         " "              
## 3  ( 1 ) " "       "*"        " "    "*"     " "         " "              
## 4  ( 1 ) " "       " "        " "    "*"     "*"         "*"              
## 5  ( 1 ) " "       "*"        " "    "*"     "*"         "*"              
## 6  ( 1 ) "*"       "*"        " "    "*"     " "         "*"              
## 7  ( 1 ) "*"       "*"        " "    "*"     "*"         "*"              
## 8  ( 1 ) "*"       "*"        " "    "*"     "*"         "*"              
## 9  ( 1 ) "*"       "*"        "*"    "*"     "*"         "*"              
##          prob_implied_over prob_implied_under prob_implied_combo
## 1  ( 1 ) " "               " "                "*"               
## 2  ( 1 ) " "               " "                "*"               
## 3  ( 1 ) "*"               " "                " "               
## 4  ( 1 ) " "               " "                "*"               
## 5  ( 1 ) "*"               " "                " "               
## 6  ( 1 ) " "               " "                "*"               
## 7  ( 1 ) " "               " "                "*"               
## 8  ( 1 ) " "               "*"                "*"               
## 9  ( 1 ) "*"               "*"                " "               
##          underdog_combo
## 1  ( 1 ) " "           
## 2  ( 1 ) " "           
## 3  ( 1 ) " "           
## 4  ( 1 ) " "           
## 5  ( 1 ) " "           
## 6  ( 1 ) "*"           
## 7  ( 1 ) "*"           
## 8  ( 1 ) "*"           
## 9  ( 1 ) "*"
```

```r
which.max(subset_summary$adjr2)
```

```
## [1] 3
```

```r
which.min(subset_summary$cp)
```

```
## [1] 2
```

```r
which.min(subset_summary$bic)
```

```
## [1] 1
```

```r
coef(subset_fit_full, which.max(subset_summary$adjr2))
```

```
##       (Intercept)        odds_under           w_diff2 prob_implied_over 
##       2.520385348      -0.000575764       0.004507513      -1.901128151
```

```r
coef(subset_fit_full, which.min(subset_summary$cp))
```

```
##    (Intercept)        w_diff2 underdog_combo 
##    1.224532667    0.004622503    0.160496068
```

```r
coef(subset_fit_full, which.min(subset_summary$bic))
```

```
##    (Intercept) underdog_combo 
##      1.2216981      0.1619497
```


It looks like these critiera each select a small number of parameters,
including some of the variables that I identified as signficant. Each
criterion suggests variables that are not collinear with others.
I'll stick with my heurestically chosen formula for now, but when it comes
time to expermenting with different formulas, I'll take a second look
at the variables identified by this subsetting algorithm.


Alright, that's enough of that. Let's get to some model experimentation.
Here are the variables and formula that I'll be using.
To predict whether or not a given team will go over or under its
win total in a given year, I will be using the
difference between the sportbook win total in predicting
a given team's number of wins from the previous year
and a singular number that combines the implied
probability of the moneyline odds for the over and under.

model_fmla_paste


```r
name_y <- "result"
names_x <- c("w_sportsbook_err1", "prob_implied_combo")
fmla <- paste(name_y, paste(names_x, collapse = " + "), sep = " ~ ")
```

model_fmla_explicit, include = FALSE


```r
# fmla <- 'result ~ w_sportsbook_err1 + prob_implied_combo'
```


## Logisitic Regression

Now I'll start with a fairly well known family of models: logistic
regression. I'll fit a model on the entire data set just to check that
everything works.



```r
lr_fit_full <- glm(fmla, data = d_model, family = binomial)
summary(lr_fit_full)$coef
```

```
##                      Estimate Std. Error    z value     Pr(>|z|)
## (Intercept)         0.2285968 0.12815565  1.7837432 0.0744654040
## w_sportsbook_err1   0.0130851 0.01499308  0.8727425 0.3828034291
## prob_implied_combo -7.3953404 2.08406617 -3.5485152 0.0003874097
```

```r
coef(lr_fit_full)
```

```
##        (Intercept)  w_sportsbook_err1 prob_implied_combo 
##          0.2285968          0.0130851         -7.3953404
```


It looks like this model isn't all too bad. It certainly could be worse.
However, even after my process of trying to identify the best predictors
to use heuristically, the p-values indicate that only one of the predictors
is signficant. This actually might not be a flaw in my selection process.
It could just be that there isn't a great combination of predictors out of
those available. Anyways, I'll leave formula experimentation for another
portion of this project.




```r
lr_prob_full <- predict(lr_fit_full, type = "response")
lr_pred_full <- rep("U", dim(d_model)[1])
```


TODO: Needs more explanation.
The naive value for the barrier between choosing "O" or "U" would
be 0.5
However, in this context, I think choosing 50% for each side is arguably
more appropriate. Also, using the mean probability
reduces sensitivity to model parameters.


```r
# lr_pred_full[lr_prob_full > 0.5] = 'O'
lr_pred_full[lr_prob_full > mean(lr_prob_full)] = "O"
table(lr_pred_full, d_model$result)
```

```
##             
## lr_pred_full  O  U
##            O 88 74
##            U 54 84
```

```r
round(mean(lr_pred_full == d_model$result), 2)
```

```
## [1] 0.57
```


So it isn't the worst model one could possibly come up with. Atleast it
better than 50% in predicitng over/unders when trained and tested on the
full data set.

But how much worse (or better) might the prediction performance get when
properly trained and tested on different sets? I'll forego the creation of
a validation set for now since I'm not explicity trying to refine
some cost parameter or another kind of tuning parameter. Instead,
I'm simply going to split the data into train and test sets. This could be
done in a number of ways, such as random sampling. However,
I'm simply going to use a single year's worth of data
as a test set and the rest as the train set. I'll do this for every year.
This is essentially "leave-one-out cross validation" (LOOCV).

I'll save some of the prediction performance data 
so that I can compare other models with this one.



```r
lr_mean_pct <- list()
for (i in 1:length(yrs)) {
    vector_train <- !(d_model$yr_start %in% yrs[i])
    
    d_model_test <- d_model[!vector_train, ]
    d_y_test <- d_model$result[!vector_train]
    
    # Not sure why this isn't working.  lr_fit_train <- glm(fmla, data =
    # d_model, family = binomial, subset = vector_train) This is an alternative.
    d_model_train <- d_model[vector_train, ]
    lr_fit_train <- glm(fmla, data = d_model_train, family = binomial)
    lr_prob_test <- predict(lr_fit_train, d_model_test, type = "response")
    lr_pred_test <- rep("U", dim(d_model_test)[1])
    # lr_pred_test[lr_prob_test > 0.5] = 'O'
    lr_pred_test[lr_prob_test > mean(lr_prob_test)] = "O"
    lr_mean_pct[i] <- mean(lr_pred_test == d_y_test)
    lr_conf_table <- table(lr_pred_test, d_y_test)
    if (i > 1) {
        lr_conf_table_all <- data.frame(rbind(lr_conf_table_all, cbind(data.frame(yr_start = rep(yrs[i], 
            length(lr_conf_table))), lr_conf_table)))
    } else {
        lr_conf_table_all <- data.frame(cbind(data.frame(yr_start = rep(yrs[i], 
            length(lr_conf_table))), lr_conf_table))
    }
    
    # # This is an alternative.  lr_mean_pct[i] <- (sum(lr_conf_table[1, 1] +
    # lr_conf_table[2, 2]) / sum(lr_conf_table))
}

round(mean(as.numeric(lr_mean_pct)), 2)
```

```
## [1] 0.42
```

```r
round(as.numeric(lr_mean_pct), 2)
```

```
##  [1] 0.47 0.30 0.50 0.50 0.37 0.13 0.33 0.57 0.53 0.47
```

```r
lr_conf_table_all <- lr_conf_table_all %>% rename(pred_test = lr_pred_test, 
    count = Freq)
lr_conf_table_all
```

```
##    yr_start pred_test d_y_test count
## 1      2007         U        O    16
## 2      2007         U        U    14
## 3      2008         O        O     5
## 4      2008         U        O    10
## 5      2008         O        U    11
## 6      2008         U        U     4
## 7      2009         O        O     8
## 8      2009         U        O     6
## 9      2009         O        U     9
## 10     2009         U        U     7
## 11     2010         O        O     7
## 12     2010         U        O     6
## 13     2010         O        U     9
## 14     2010         U        U     8
## 15     2011         O        O     5
## 16     2011         U        O    10
## 17     2011         O        U     9
## 18     2011         U        U     6
## 19     2012         O        O     3
## 20     2012         U        O    13
## 21     2012         O        U    13
## 22     2012         U        U     1
## 23     2013         O        O     3
## 24     2013         U        O     7
## 25     2013         O        U    13
## 26     2013         U        U     7
## 27     2014         O        O     8
## 28     2014         U        O     5
## 29     2014         O        U     8
## 30     2014         U        U     9
## 31     2015         O        O     7
## 32     2015         U        O     8
## 33     2015         O        U     6
## 34     2015         U        U     9
## 35     2016         O        O     7
## 36     2016         U        O     8
## 37     2016         O        U     8
## 38     2016         U        U     7
```


And look at that! As I feared, the model did worse (much worse) when
properly trained and test. In fact, it performs worse than random guessing!
I'll intepret this to indicate that the model isn't very good.

To verify why training and testing on the entire data set is usually not
a good thing, I want to briefly look at a model that shows horrible
"over-fitting".



```r
fmla_overfit <- "result ~ result_lag1"
lr_overfit_fit_full <- glm(fmla_overfit, data = d_model, family = binomial)
coef(lr_overfit_fit_full)
```

```
##  (Intercept) result_lag1U 
##    0.3014754   -0.3154616
```

```r
summary(lr_overfit_fit_full)$coef
```

```
##                Estimate Std. Error   z value   Pr(>|z|)
## (Intercept)   0.3014754  0.1794869  1.679651 0.09302523
## result_lag1U -0.3154616  0.2453344 -1.285844 0.19849763
```

```r
lr_overfit_prob_full <- predict(lr_overfit_fit_full, type = "response")
lr_overfit_pred_full <- rep("U", dim(d_model)[1])
lr_overfit_pred_full[lr_overfit_prob_full > mean(lr_overfit_prob_full)] = "O"
table(lr_overfit_pred_full, d_model$result)
```

```
##                     
## lr_overfit_pred_full   O   U
##                    O 129  14
##                    U  13 144
```

```r
round(mean(lr_overfit_pred_full == d_model$result), 2)
```

```
## [1] 0.91
```


Wow! That's outstanding performance. But... what if I don't train
and predict on the full data set?



```r
lr_overfit_mean_pct <- list()
for (i in 1:length(yrs)) {
    vector_train <- !(d_model$yr_start %in% yrs[i])
    d_model_test <- d_model[!vector_train, ]
    d_y_test <- d_model$result[!vector_train]
    # Not sure why this isn't working.  lr_overfit_fit_train <- glm(fmla, data =
    # d_model, family = binomial, subset = vector_train) This is an alternative.
    d_model_train <- d_model[vector_train, ]
    lr_overfit_fit_train <- glm(fmla, data = d_model_train, family = binomial)
    lr_overfit_prob_test <- predict(lr_overfit_fit_train, d_model_test, type = "response")
    lr_overfit_pred_test <- rep("U", dim(d_model_test)[1])
    # lr_overfit_pred_test[lr_overfit_prob_test > 0.5] = 'O'
    lr_overfit_pred_test[lr_overfit_prob_test > mean(lr_overfit_prob_test)] = "O"
    lr_overfit_mean_pct[i] <- mean(lr_overfit_pred_test == d_y_test)
}
round(mean(as.numeric(lr_overfit_mean_pct)), 2)
```

```
## [1] 0.42
```

```r
round(as.numeric(lr_overfit_mean_pct), 2)
```

```
##  [1] 0.47 0.30 0.50 0.50 0.37 0.13 0.33 0.57 0.53 0.47
```


It turns out that this model looks very average when properly evaluated.

Let's try a different kind of model now.

## Linear Discriminant Analysis (LDA)

Now I'm going to try a different model framework: Linear Discriminant
Analysis (LDA). I'll be using the `lda` function from the `MASS` package.

As I mentioned before, I'll be using the same formula with all of the
models that I experiment with in this part of the project.

Also, as I did with logisitic regression model, I'll start by evaluating
a model fit on the entire data set even thought its predictions will be
deceivingly more accurate than they should be.



```r
library(MASS)
# Need as.formula() for lda function.
lda_fit_full <- lda(as.formula(fmla), data = d_model)
lda_fit_full
```

```
## Call:
## lda(as.formula(fmla), data = d_model)
## 
## Prior probabilities of groups:
##         O         U 
## 0.4666667 0.5333333 
## 
## Group means:
##   w_sportsbook_err1 prob_implied_combo
## O        -0.8730159        0.027022044
## U         0.1840278       -0.001877374
## 
## Coefficients of linear discriminants:
##                            LD1
## w_sportsbook_err1    0.0280927
## prob_implied_combo -15.3877615
```


Simple enough. Now let's evaluate this model on the entire data set.



```r
# Using suppressWarnings() to ignore annoying 'NA' warnings.
suppressWarnings(lda_pred_full <- predict(lda_fit_full, d_model))
lda_conf_table <- table(lda_pred_full$class, d_model$result)
lda_conf_table
```

```
##    
##       O   U
##   O  54  31
##   U  72 113
```

```r
# The line below doesn't work due to NA's in the data set.
# mean((lda_pred_full$posterior == d_model$result), na.rm = TRUE)
(sum(lda_conf_table[1, 1] + lda_conf_table[2, 2])/sum(lda_conf_table))
```

```
## [1] 0.6185185
```


It appears that this model performs better than its logistic
 regression counterpart when trained and tested on the entire data set.
However, its true robustness will be shown when it is trained and tested in
a more proper manner. I'll use the same method as shown before.



```r
lda_mean_pct <- list()
for (i in 1:length(yrs)) {
    vector_train <- !(d_model$yr_start %in% yrs[i])
    d_model_test <- d_model[!vector_train, ]
    d_y_test <- d_model$result[!vector_train]
    lda_fit_train <- lda(as.formula(fmla), data = d_model, subset = vector_train)
    # This is an alternative.  d_model_train <- d_model[vector_train, ]
    # lda_fit_train <- lda(as.formula(fmla), data = d_model_train)
    suppressWarnings(lda_pred_test <- predict(lda_fit_train, d_model_test))
    lda_conf_table <- table(lda_pred_test$class, d_y_test)
    lda_mean_pct[i] <- (sum(lda_conf_table[1, 1] + lda_conf_table[2, 2])/sum(lda_conf_table))
    if (i > 1) {
        lda_conf_table_all <- data.frame(rbind(lda_conf_table_all, cbind(data.frame(yr_start = rep(yrs[i], 
            length(lda_conf_table))), lda_conf_table)))
    } else {
        lda_conf_table_all <- data.frame(cbind(data.frame(yr_start = rep(yrs[i], 
            length(lda_conf_table))), lda_conf_table))
    }
}
lda_mean_pct <- unlist(lda_mean_pct)
round(mean(as.numeric(lda_mean_pct), na.rm = TRUE), 2)
```

```
## [1] 0.61
```

```r
round(as.numeric(lda_mean_pct), 2)
```

```
##  [1]  NaN 0.70 0.47 0.63 0.53 0.83 0.70 0.60 0.47 0.60
```

```r
lda_conf_table_all
```

```
##    yr_start Var1 d_y_test Freq
## 1      2007    O        O    0
## 2      2007    U        O    0
## 3      2007    O        U    0
## 4      2007    U        U    0
## 5      2008    O        O    9
## 6      2008    U        O    6
## 7      2008    O        U    3
## 8      2008    U        U   12
## 9      2009    O        O    4
## 10     2009    U        O   10
## 11     2009    O        U    6
## 12     2009    U        U   10
## 13     2010    O        O    6
## 14     2010    U        O    7
## 15     2010    O        U    4
## 16     2010    U        U   13
## 17     2011    O        O    2
## 18     2011    U        O   13
## 19     2011    O        U    1
## 20     2011    U        U   14
## 21     2012    O        O   12
## 22     2012    U        O    4
## 23     2012    O        U    1
## 24     2012    U        U   13
## 25     2013    O        O    3
## 26     2013    U        O    7
## 27     2013    O        U    2
## 28     2013    U        U   18
## 29     2014    O        O    4
## 30     2014    U        O    9
## 31     2014    O        U    3
## 32     2014    U        U   14
## 33     2015    O        O    8
## 34     2015    U        O    7
## 35     2015    O        U    9
## 36     2015    U        U    6
## 37     2016    O        O    8
## 38     2016    U        O    7
## 39     2016    O        U    5
## 40     2016    U        U   10
```


Not too bad ... Maybe this is an acceptable model.

Now, I want to take a closer look at the train/test output of this LDA
model. I'll use the data from the 2016 season as the tet set
and the data from the years prior to it as the train set.

Here, I'm basically removing the iteration from the train/test function.



```r
vector_train <- !(d_model$yr_start %in% 2016)
d_model_test <- d_model[!vector_train, ]
d_y_test <- d_model$result[!vector_train]
lda_fit_train <- lda(as.formula(fmla), data = d_model, subset = vector_train)
lda_pred_test <- predict(lda_fit_train, d_model_test)
lda_conf_table <- table(lda_pred_test$class, d_y_test)
lda_conf_table
```

```
##    d_y_test
##      O  U
##   O  8  5
##   U  7 10
```

```r
sum(lda_pred_test$posterior[, 1] > 0.5)
```

```
## [1] 13
```

```r
sum(lda_pred_test$posterior[, 1] <= 0.5)
```

```
## [1] 17
```

```r
# lda_pred_test$posterior[1:30, 1]
sum(lda_pred_test$posterior[, 1] > 0.6)
```

```
## [1] 9
```

```r
sum(lda_pred_test$posterior[, 1] < 0.4)
```

```
## [1] 12
```


It seems like this model generates fairly reasonable probabilities.
After all, I wouldn't expect the posterior probabilities to deviate
very far from 0.5.

But which teams does this model project have the highest probability of
outperforming/underperforming their expectations (as determined by the
sportsbook win total)?



```r
g_teams <- d_win_totals_raw <- readRDS(paste0(dir_data, "g_teams.rds"))
max(lda_pred_test$posterior[, 1])
```

```
## [1] 0.7833456
```

```r
# This assumes that the teams are listed in alphabetical order in d_model.
# Can't use team column since it was removed from d_join when creating
# d_model.  d_model_test$team[which.max(lda_pred_test$posterior[, 1])]
g_teams$team[which.max(lda_pred_test$posterior[, 1])]
```

```
## [1] MIN
## 30 Levels: ATL BKN BOS CHA CHI CLE DAL DEN DET GS HOU IND LAC LAL ... WSH
```

```r
min(lda_pred_test$posterior[, 1])
```

```
## [1] 0.1393461
```

```r
# d_model_test$team[which.min(lda_pred_test$posterior[, 1])]
g_teams$team[which.min(lda_pred_test$posterior[, 1])]
```

```
## [1] SA
## 30 Levels: ATL BKN BOS CHA CHI CLE DAL DEN DET GS HOU IND LAC LAL ... WSH
```


UPDATE: Actually, it appears that this LDA model is weighing the w_diff2
factor heavily, so the teams with the largest +- w_diff2 have the largest
posterior probabilities.

So this LDA model picks POR to have the best chance of going over its
win total and NYK to have the worst chance of going under its win total in
the year 2016. This actually makes sense. Although POR was projected to do
relatively well, and did, in fact, end up with a good win-loss
record in 2016, one might expect that bookies set the win total lower than
it should be for bad and/or "non-public" teams in order to try to
even out the money placed on each betting side.

In general, public bettors are more likely to make
bets purely on perception, so they are more likely to believe a bad team
will do very bad and a good team will do very good, and so they are more
likely to bet the under on a bad team and the over on a good team. Although
this deduction is fairly anecdotal, one can easily find research proving
this statement. Thus, in order to even out the betting sides, bookies are
more likely to exaggerate the win totals for the teams at the top and bottom
of the league.

Another common bettor behavior may explain the why LDA model shows that
NYK is most likely to go under its projected win total (and, conversely, why
POR is most likely to exceed its win total). Teams in big cities
like New York are more likely to have "fanatical" fans that set overly-
optimistic expectations for their teams. Thus, to take advantage of their
potential foolishness and willingness to bet the over, bookies generally tend
to set the win totals for these "public" (i.e. big city or very popular)
teams
higher than they should be. (For example, this is also true of the
Dallas Cowboys in the NFL and the New York Yankees and Chicago Cubs in
the MLB.)

Support Vector Machine (SVM) Attempt # 1

Now, I'll look experiment with another family of models that can be
used to predict categorical response variables: support vector machines (SVMs).



```r
# # Option #1 d_model_no_fctrs <- d_model indx <- sapply(d_model_no_fctrs,
# is.factor) d_model_no_fctrs[indx] <- lapply(d_model_no_fctrs[indx],
# function(x) as.numeric(x))

# # Option #2 d_model_no_fctrs <- d_model d_model_no_fctrs$result_lag1 <-
# as.numeric(d_model_no_fctrs$result_lag1) d_model_no_fctrs$team <-
# as.numeric(d_model_no_fctrs$team)
```

```r
library(e1071)
# Predictors can't be factors, so remove them. Alternatively, could convert
# factors to numerics.
d_model_no_fctrs <- d_model %>% dplyr::select(-result_lag1, -underdog_combo) %>% 
    filter(yr_start > 2008)

# Need as.formula() here.
svm_fit_full <- svm(as.formula(fmla), data = d_model_no_fctrs, kernel = "linear", 
    cost = 1, scale = TRUE)
svm_fit_full
```

```
## 
## Call:
## svm(formula = as.formula(fmla), data = d_model_no_fctrs, kernel = "linear", 
##     cost = 1, scale = TRUE)
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  1 
##       gamma:  0.5 
## 
## Number of Support Vectors:  211
```



This package has a `tune` function to perform k-fold cross validation, so
I don't necessarily need to create a function to do training/testing.
(The default number of folds is 10.)

But, to be consistent, I'll try out the implement the train/test evaluation 
that I've been using and I'll use the cost value identified by
the `tune` function.



```r
set.seed(1)
range_cost <- 10^seq(-3, 2, 1)
svm_tune <- tune(svm, as.formula(fmla), data = d_model_no_fctrs, kernel = "linear", 
    ranges = list(cost = range_cost))
summary(svm_tune)
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost
##   0.1
## 
## - best performance: 0.4 
## 
## - Detailed performance results:
##    cost     error dispersion
## 1 1e-03 0.4625000 0.08882376
## 2 1e-02 0.4750000 0.08379502
## 3 1e-01 0.4000000 0.08827916
## 4 1e+00 0.4041667 0.09830744
## 5 1e+01 0.4041667 0.09830744
## 6 1e+02 0.4041667 0.09830744
```

```r
svm_best <- svm_tune$best.model
svm_best$cost
```

```
## [1] 0.1
```

```r
svm_mean_pct <- list()
for (i in 1:length(yrs)) {
    vector_train <- !(d_model_no_fctrs$yr_start %in% yrs[i])
    d_model_no_fctrs_test <- d_model_no_fctrs[!vector_train, ]
    d_y_test <- d_model_no_fctrs$result[!vector_train]
    svm_pred_test <- predict(svm_best, d_model_no_fctrs_test)
    svm_conf_table <- table(svm_pred_test, d_y_test)
    svm_mean_pct[i] <- mean(svm_pred_test == d_y_test)
    if (i > 1) {
        svm_conf_table_all <- data.frame(rbind(svm_conf_table_all, cbind(data.frame(yr_start = rep(yrs[i], 
            length(svm_conf_table))), svm_conf_table)))
    } else {
        svm_conf_table_all <- data.frame(cbind(data.frame(yr_start = rep(yrs[i], 
            length(svm_conf_table))), svm_conf_table))
    }
}

round(mean(as.numeric(svm_mean_pct), na.rm = TRUE), 2)
```

```
## [1] 0.6
```

```r
round(as.numeric(svm_mean_pct), 2)
```

```
##  [1]  NaN  NaN 0.57 0.57 0.50 0.83 0.70 0.53 0.53 0.60
```

```r
svm_conf_table_all <- svm_conf_table_all %>% rename(pred_test = svm_pred_test, 
    count = Freq)
```


It turns out that the best cost value from the several that I tried out is 1,
so it looks like using the default cost value in my custom train/test
function wasn't so bad.

## Trees

Now, I'll look at trees. Just because I'm curious, I want to see how a tree
using all of the predictors looks.



```r
# BOOKMARK ####
library(tree)
fmla_0 <- paste(name_y, paste(names_x_all, collapse = " + "), sep = " ~ ")
tree_0_fit_full <- tree(fmla_0, data = d_model)
summary(tree_0_fit_full)
```

```
## 
## Classification tree:
## tree(formula = fmla_0, data = d_model)
## Variables actually used in tree construction:
## [1] "odds_over"         "w_diff2"           "w_lag1"           
## [4] "w_sportsbook_err1" "odds_under"       
## Number of terminal nodes:  22 
## Residual mean deviance:  0.9392 = 204.7 / 218 
## Misclassification error rate: 0.2333 = 56 / 240
```

```r
plot(tree_0_fit_full)
text(tree_0_fit_full, pretty = 0)
```

![](figs/tree_0_fit_full-1.png)<!-- -->


That's quite a "busy" tree. It's difficult to make much from its output.

Now, I'll look at a tree for the `fmla` that I've been using before.

#+ tree_fit_full


```r
set.seed(1)
tree_fit_full <- tree(fmla, d_model)
summary(tree_fit_full)
```

```
## 
## Classification tree:
## tree(formula = fmla, data = d_model)
## Number of terminal nodes:  8 
## Residual mean deviance:  1.239 = 324.7 / 262 
## Misclassification error rate: 0.3407 = 92 / 270
```

```r
plot(tree_fit_full)
text(tree_fit_full, pretty = 0)
```

![](figs/unnamed-chunk-6-1.png)<!-- -->



Now I'll train and test the tree.



```r
vector_train_seq <- 1:270
d_model_test <- d_model[-vector_train_seq, ]
d_y_test <- d_model$result[-vector_train_seq]
d_model_train <- d_model[vector_train_seq, ]
tree_train <- tree(fmla, d_model_train)
tree_pred_test <- predict(tree_train, d_model_test, type = "class")
tree_conf_table <- table(tree_pred_test, d_y_test)
tree_conf_table
```

```
##               d_y_test
## tree_pred_test  O  U
##              O  8  4
##              U  7 11
```

```r
tree_cv <- cv.tree(tree_fit_full, FUN = prune.misclass)
tree_cv$dev
```

```
## [1] 105 105 113 112 129
```

```r
tree_prune <- prune.misclass(tree_fit_full, best = length(tree_cv$dev))

plot(tree_prune)
text(tree_prune, pretty = 0)
```

![](figs/tree_cv-1.png)<!-- -->

```r
tree_prune_pred_test <- predict(tree_prune, d_model_test, type = "class")
tree_prune_conf_table <- table(tree_prune_pred_test, d_y_test)
tree_prune_conf_table
```

```
##                     d_y_test
## tree_prune_pred_test  O  U
##                    O  8  4
##                    U  7 11
```






```r
library(randomForest)
set.seed(1)
d_model_no_nas <- na.omit(d_model)
rf_bag_fit_full <- randomForest(as.formula(fmla), mtry = length(names_x), data = d_model_no_nas, 
    importance = TRUE)
rf_bag_fit_full
```

```
## 
## Call:
##  randomForest(formula = as.formula(fmla), data = d_model_no_nas,      mtry = length(names_x), importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 44.58%
## Confusion matrix:
##    O  U class.error
## O 57 54   0.4864865
## U 53 76   0.4108527
```

```r
fmla_ext <- paste(name_y, paste(names_x_plausible, collapse = " + "), sep = " ~ ")
rf_bag_ext_fit_full <- randomForest(as.formula(fmla_ext), mtry = length(names_x_plausible), 
    data = d_model_no_nas, importance = TRUE)
rf_bag_ext_fit_full
```

```
## 
## Call:
##  randomForest(formula = as.formula(fmla_ext), data = d_model_no_nas,      mtry = length(names_x_plausible), importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 6
## 
##         OOB estimate of  error rate: 50.83%
## Confusion matrix:
##    O  U class.error
## O 43 68   0.6126126
## U 54 75   0.4186047
```

```r
vector_train_seq_no_nas <- 1:210
rf_bag_train <- randomForest(as.formula(fmla), mtry = length(names_x), data = d_model_no_nas, 
    subset = vector_train_seq_no_nas, importance = TRUE)
rf_bag_pred <- predict(rf_bag_train, newdata = d_model_no_nas[-vector_train_seq_no_nas, 
    ])
```


# Conclusion

That's all the analysis that I'm going to do on this subject for now.
Of course, much more data could be incorporated to potentially improve the
models that I've experimented with here. I think some other useful data
might be variables indicating the change in team salary from one year to
another, the change in mean/medain team age, the amount of injury "luck" the
team had in the previous year, etc. Nevertheless, even with more data and
better models, I think there is probably a theoretical limit on the accuracy
of projecting team win totals around 60%. Sports like basketball are just
too random to be able to predict extremely accurately.





---
title: "classification.R"
author: "aelhabr"
date: "Tue Jun 13 18:44:50 2017"
---
