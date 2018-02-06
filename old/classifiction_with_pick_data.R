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
  tidy = TRUE,
  tidy.opts=list(width.cutoff = 80),
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
#+ import_data
dir_data <- paste0(getwd(), "/data/")
d_win_totals_raw <-
  readRDS(paste0(dir_data, "d_win_totals.rds"))
d_scraped_stats_classification_raw <-
  readRDS(paste0(dir_data, "d_scraped_stats_classification_raw.rds"))
p_win_totals_raw <-
  readRDS(paste0(dir_data, "p_win_totals.rds"))
#'
#' # Tidying and Transforming the Data
#'
#' Now I'll tidy and transform.
#'
#+ tidy_and_transform_data
library(tidyverse)

# Selecting only the desired columns.
# dplyr's select() function may conflict with other packages.


p_win_totals <- p_win_totals_raw %>%
  dplyr::select(one_of(c("pick_id",
                         "person",
                         "pick",
                         "confidence")))

d_scraped_stats_classification <- d_scraped_stats_classification_raw %>%
  group_by(team) %>%
  mutate(avg_scoring_margin_lag1 = lag(avg_scoring_margin),
         win_pct_close_games_lag1 = lag(win_pct_close_games)) %>%
  ungroup() %>%
  dplyr::select(-avg_scoring_margin, -avg_margin_thru_three_quarters, -win_pct_close_games)

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
  ) %>%
  dplyr::select(-odds_over, -odds_under, -prob_implied_over, -prob_implied_under)

d_win_totals$result <- as.factor(d_win_totals$result)
d_win_totals$result_lag1 <- as.factor(d_win_totals$result_lag1)
d_win_totals$underdog_combo <- as.factor(d_win_totals$underdog_combo)

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
  ) %>%
  dplyr::select(-w, -w_sportsbook)
# pick_int = ifelse(pick == "O", 1, -1)) %>%
# dplyr::select(-pick) %>%
# rename(pick = pick_int)

d_join_2 <- d_join %>%
  inner_join(d_scraped_stats_classification, by = c("yr_start", "team"))

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
#' Because there is duplicate information in my final tidy data set due
#' to there being two people for whom picks were tracked, I'll filter the
#' data set to just distinct results. I'm not going to evaluate the picks that
#' my brother and I made yet (even though I did some calculations with this
#' data above).
#'
cols_remove <- c("team", "person", "pick", "confidence", "pick_result", "W_pick_confidence")
d_model <- d_join %>%
  dplyr::select(-team, -person, -pick,-confidence,-pick_result,-w_pick_confidence) %>%
  # dplyr::select(which(!names(.) %in% grep(cols_remove, names(.), value = T))) %>%
  filter(yr_start > 2008) %>%
  distinct()
