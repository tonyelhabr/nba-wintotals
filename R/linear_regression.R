#' ---
#' title: "R Notebook"
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
#' The goal of this project is to experiment with linear regression models
#' to predict NBA team win totals. Team statistics scraped from
#' www.teamrankings.com will be used. (The process of retrieving this
#' data will not be discussed here.)
#'
#' In this project I attempt to implement the "tidy" workflow
#' promoted by Hadley Wickham and many other prominent R community members.
#' This approach advocates initial steps of
#' importing and tidying data; followed by a cycle
#' of transforming, modeling, and analyzing the data; and, finally,
#' communicating the results.
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

#'
#'
#'
#+ import_data
dir_data <- paste0(getwd(), "/data/")
d_scraped_stats_raw <-
  readRDS(paste0(dir_data, "d_scraped_stats_raw.rds"))
d_win_totals_raw <- readRDS(paste0(dir_data, "d_win_totals.rds"))
colnames(d_scraped_stats_raw)
colnames(d_win_totals_raw)
#'
#' # Tidying the Data
#'
#' Here I'm simply defining some variables that will be used either for
#' tidying/transforming the data and/or, later, for modeling the data.
#'
#+ define_variables
yrs <- 2012:2016
cols_join <- c("team", "yr_start")  # essential variables for joins
cols_join_2 <-
  c(cols_join, "w")  # preserves response variable when joining
cols_stats <- c(
  "pts_pg",
  "effective_fg_pct",
  "off_efficiency",
  "def_efficiency",
  "three_pt_rate",
  "ft_rate",
  "def_reb_pct",
  "off_reb_pct",
  "blk_pct",
  "stl_pct",
  "ast_per_poss",
  "turn_pct"
)

#'
#' Now I'll tidy.
#'
#+ tidy_data
library(tidyverse)

# Selecting only the desired columns.
# dplyr's select may conflict with other packages.
d_x_raw <- d_scraped_stats_raw %>%
  dplyr::select(one_of(c(cols_join, cols_stats)))

d_y_raw <- d_win_totals_raw %>%
  filter(yr_start %in% yrs) %>%
  dplyr::select(one_of(cols_join_2))

# The inner_join might not work if the team columns is not of the same type
# in each data set. (It must be either a character/factor in both data sets.)
d_model_raw <-
  inner_join(d_y_raw, d_x_raw, by = cols_join)
# knitr::kable(d_model_raw)

#'
#' Before I continue, I want to take a quick look at some of the data.
#' This is a visualization of what might be deemed "Moreyball" statistics.
#'
#+ plot_moreyball
plot_moreyball <-
  d_model_raw %>%
  ggplot(aes(x = w)) +
  geom_point(aes(y = effective_fg_pct), colour = "red") +
  geom_smooth(aes(y = effective_fg_pct), colour = "red", se = FALSE) +
  geom_point(aes(y = three_pt_rate), colour = "blue") +
  geom_smooth(aes(y = three_pt_rate), colour = "blue", se = FALSE) +
  geom_point(aes(y = ft_rate), colour = "green") +
  geom_smooth(aes(y = ft_rate), colour = "green", se = FALSE) +
  geom_label(
    data = top_n(d_model_raw, 1, w),
    aes(label = "effective_fg_pct", y = effective_fg_pct),
    nudge_x = -3,
    colour = "red"
  ) +
  geom_label(
    data = top_n(d_model_raw, 1, w),
    aes(label = "three_pt_rate", y = three_pt_rate),
    nudge_x = -3,
    colour = "blue"
  ) +
  geom_label(
    data = top_n(d_model_raw, 1, w),
    aes(label = "ft_rate", y = ft_rate),
    nudge_x = -3,
    colour = "green"
  ) +
  geom_text(
    data = top_n(d_model_raw, 1, w),
    aes(label = "ok fit", y = effective_fg_pct),
    nudge_y = -3
  ) +
  geom_text(
    data = top_n(d_model_raw, 1, w),
    aes(label = "bad fit", y = three_pt_rate),
    nudge_y = -3
  ) +
  geom_text(
    data = top_n(d_model_raw, 1, w),
    aes(label = "worse fit", y = ft_rate),
    nudge_y = -3
  ) +
  labs(title = "Moreyball Stats") +
  theme(
    axis.text = element_text(size = 8),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  )
plot_moreyball

#+ remove imported data, include = FALSE
rm(list = ls()[!(ls() %in% c("d_model_raw", "yrs", "cols_join"))])
#'
#' # Transforming the Data
#'
#' I intend to create a multitude of single variable
#' linear regression models in order to gauge the correlation of the
#' selected team statistics with team wins. But before I can do that, I
#' need to transform the data a bit.
#'
#' I will using the example analysis shown at
#' http://r4ds.had.co.nz/many-models.html as a guide. Notably,
#' Hadley's walkthrough utilizes "nested" list-columns in order to facilitate
#' the creation of many models simultaenously. The response variable is not
#' included in the nesting in order to facilitate the use of broom's
#' `map()` function.
#'
#+ transform_data
library(broom)
library(modelr)
library(purrr)

d_model_gather_for_nest <- d_model_raw %>%
  gather(key = stat,
         value = value,
         4:(length(names(d_model_raw))),
         factor_key = TRUE)
#'
#' Here, I extend the nested data set by calculating lagged and
#' differenced values of the response and predictor variables.
#' These values are also normalized to facilitate modeling and
#' plotting.
#'
#+ old_chunk, show = FALSE
# A couple of different ways to normalize shown here.
# Note that R's scale function scales from 0 to 1, which is not done here.
# normalize <- function(col) (max(col) - col) / (max(col) - min(col))
# normalize <- function(col) (col - mean(col)) / (max(col) - min(col))
# normalize <- function(col) col - mean(col)
# normalize <- function(col) col / mean(col)

#+ transform_data_2
# This is the z transform.
normalize <- function(col)
  (col - mean(col)) / sd(col)

# I decided not to use the rnk variable, so it's commented out here
# and in some other code chunks.
d_stats_gather_for_nest_2 <-
  d_model_gather_for_nest %>%
  group_by(team, stat) %>%
  mutate(
    w_lag1 = dplyr::lag(w),
    w_diff1 = w - dplyr::lag(w),
    value_lag1 = dplyr::lag(value),
    value_diff1 = value - dplyr::lag(value)
  ) %>%
  ungroup() %>%
  group_by(yr_start, stat) %>%
  mutate(
    # w_rnk = dense_rank(desc(w)),
    w_norm = normalize(w),
    w_lag1_norm = normalize(w_lag1),
    w_diff1_norm = normalize(w_diff1),
    # value_rnk = dense_rank(desc(value)),
    value_norm = normalize(value),
    value_lag1_norm = normalize(value_lag1),
    value_diff1_norm = normalize(value_diff1)
  )
#'
#'
#' Yes, I realize that renaming the data variable is somewhat redundant,
#' but it is useful when testing/debugging to compare/contrast the results
#' from piping several transformation operations with the original data set.
#'
#' Transforming the data in this manner makes it easier to work with. For instance,
#' the previous plot can be re-made fairly easily with `facet_wrap`.
#' In addition , the trends are a bit easier to see.
#'
#+ plot_moreyball_2
plot_moreyball_2 <-
  d_stats_gather_for_nest_2 %>%
  na.omit() %>%
  filter(stat %in% c("effective_fg_pct", "three_pt_rate", "ft_rate")) %>%
  ggplot(aes(x = w, y = value, colour = stat)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap( ~ stat, scales = "free") +
  labs(title = "Moreyball Stats") +
  theme(
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "none"
  )
plot_moreyball_2
#'
#' This is a visualzation that provides an estimation of the "leverage"
#' of each statistic in affecting team wins. Those predictors with
#' differenced values
#' that change in a one-to-one (i.e. linear) manner
#' with the differenced response variable
#'
#'
plot_diff1 <-
  d_stats_gather_for_nest_2 %>%
  na.omit() %>%
  ggplot(aes(value_diff1_norm, w_diff1_norm)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap( ~ stat) +
  labs(title = "Normalized Year-to-Year Change in Stat Values vs. Change in Wins") +
  theme(
    axis.text = element_text(size = 8),
    # axis.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "none",
    axis.text.x = element_blank()
  )
#'
#'
#' This is a visualization of the correlation between a given year's values and
#' the prior year's values for each team statistic. A positive correlation is
#' expected in most cases. A non-evident correlation possibly indicates that the
#' statistic is highly variable/random.
#'
#' As shown in the plot, it appears that each of the statistics do, in fact, have
#' some correlation with their prior year's values.
#'
#'
plot_lag1 <-
  d_stats_gather_for_nest_2 %>%
  na.omit() %>%
  ggplot(aes(value_lag1_norm, value_norm)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap( ~ stat) +
  labs(title = "Normalized Current Year vs. Previous Year Values") +
  theme(
    axis.text = element_text(size = 8),
    # axis.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "none",
    axis.text.x = element_blank()
  )
#'
#'
#' # Inferential Analysis
#'
#' Here, I am "nestting" the predictors in a "tidy" way in order to facilitate
#' the creation of many models with the broom::map() functions.
#'
#'
d_stats_nest <- d_stats_gather_for_nest_2 %>%
  group_by(stat) %>%
  nest()
#'
#'
#' ## Creating Models
#'
#' Now I create the models and list them by descending R^2 values.
#'
#'
# Different single variable linear regression models could be investigated.
# create_lm <- function(tbl) { lm(w ~ value, data = tbl) }
# create_lm <- function(tbl) { lm(value ~ value_diff1, data = tbl) }
# create_lm <- function(tbl) { lm(w_norm ~ value_norm, data = tbl) }
# create_lm <- function(tbl) { lm(value_norm ~ value_lag1_norm, data = tbl) }
# create_lm <- function(tbl) { lm(value_norm ~ value_diff1_norm, data = tbl) }
create_lm <-
  function(tbl) {
    lm(w_diff1_norm ~ value_diff1_norm, data = tbl)
  }

d_stats_models <- d_stats_nest %>%
  # na.omit() %>%
  mutate(model = map(d_stats_nest$data, create_lm)) %>%
  mutate(resids = map2(data, model, add_residuals))

d_stats_models_eval <- d_stats_models %>%
  mutate(eval = map(model, broom::glance)) %>%
  unnest(eval, drop = TRUE)

d_stats_models_eval %>% arrange(desc(r.squared)) %>% select(stat, r.squared, p.value)
#'
#'
#'
d_stats_models_top3 <- d_stats_models_eval %>% top_n(3, r.squared)
d_stats_models_bottom3 <-
  d_stats_models_eval %>% top_n(3, desc(r.squared))
#'
#'
#' ## Plotting the Modeled Data
#'
#' ### Visualizing the Best 3 Model Fits
#'
#' A visualization of the raw predictor variable values and
#' the response variable values.
#'
#'
plot_models_diff1_top3 <-
  d_stats_gather_for_nest_2 %>%
  na.omit() %>%
  semi_join(d_stats_models_eval, by = "stat") %>%
  filter(stat %in% d_stats_models_top3$stat) %>%
  ggplot(aes(w_diff1, value_diff1, colour = stat)) +
  geom_smooth(se = FALSE) +
  geom_point() +
  facet_grid(stat ~ ., scales = "free") +
  # facet_grid(. ~ stat, scales = "free") +  # looks bad
  labs(title = "Top 3 lm() Fits") +
  theme(
    axis.text = element_text(size = 8),
    # axis.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "none"
    # axis.text.x = element_blank()
  )
#'
#'
#' A visualization of the normalized predictor variable values and
#' the response variable values.
#'
#'
plot_models_diff1_norm_top3 <-
  d_stats_gather_for_nest_2 %>%
  na.omit() %>%
  semi_join(d_stats_models_eval, by = "stat") %>%
  filter(stat %in% d_stats_models_top3$stat) %>%
  ggplot(aes(w_diff1_norm, value_diff1_norm, colour = stat)) +
  geom_smooth(se = FALSE) +
  geom_point() +
  facet_grid(stat ~ ., scales = "free") +
  # facet_grid(. ~ stat, scales = "free") +  # looks good too
  labs(title = "Top 3 lm() Fits") +
  theme(
    axis.text = element_text(size = 8),
    # axis.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "none"
    # axis.text.x = element_blank()
  )
#'
#'
#' A visualization of the raw predictor variable values and
#' their residuals for the top 3 fits.
#'
#'
plot_models_diff1_top3_resids <-
  d_stats_models %>%
  unnest(resids) %>%
  na.omit() %>%
  filter(stat %in% d_stats_models_top3$stat) %>%
  ggplot(aes(value_lag1, resid, colour = stat)) +
  geom_smooth(se = FALSE) +
  geom_point() +
  # facet_grid(stat ~ ., scales = "free") +  # looks bad
  facet_grid(. ~ stat, scales = "free") +
  labs(title = "Top 3 lm() Fits") +
  theme(
    axis.text = element_text(size = 8),
    # axis.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "none"
    # axis.text.x = element_blank()
  )
#'
#'
#' A visualization of the normalized predictor variable values and
#' their residuals for the top 3 fits.
#'
#'
#'
plot_models_diff1_norm_top3_resids <-
  d_stats_models %>%
  unnest(resids) %>%
  na.omit() %>%
  filter(stat %in% d_stats_models_top3$stat) %>%
  ggplot(aes(x = value_lag1_norm, y = resid, colour = stat)) +
  geom_smooth(se = FALSE) +
  geom_point() +
  # facet_grid(stat ~ ., scales = "free") +  # looks good too
  facet_grid(. ~ stat, scales = "free") +
  labs(title = "Top 3 lm() Fits") +
  theme(
    axis.text = element_text(size = 8),
    # axis.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "none"
  )
#'
#'
#' Similar plots could be made for the bottom 3 fits, but that would be ugly.
#'
#' # Inferential Analysis, Part II
#' This section uses more linear regression models. However, it expands on the
#' above modeling by developing multivariate models. These models are refined
#' using techniques such as subsetting, ridge regression (RR), and the "lasso."
#'
#' The ISLR book was used as a guide.
#'
#' These data wrangling steps are similar to those from above.
#' However, the major difference is that the response variable is not treated
#' distinctly from the predictor variables since nested list-columns are not used.
#'
#'
d_stats_gather <- d_model_raw %>%
  gather(
    key = stat,
    value = value,
    3:length(names(d_model_raw)),
    factor_key = TRUE
  )

d_stats_gather_2 <-
  d_stats_gather %>%
  group_by(team, stat) %>%
  mutate(value_lag1 = dplyr::lag(value),
         value_diff1 = value - dplyr::lag(value)) %>%
  ungroup() %>%
  group_by(yr_start, stat) %>%
  mutate(
    # value_rnk = dense_rank(desc(value)),
    value_norm = normalize(value),
    value_lag1_norm = normalize(value_lag1),
    value_diff1_norm = normalize(value_diff1)
  ) %>%
  ungroup()
#'
#'
#' Here I create several data sets that can/will be appended/joined together to
#' give a "wide" data set that could be used to model values across the
#' different dta sets.
#'
#'
stat_names <- c("w", cols_stats)
# stat_names <- unique(as.character(d_stats_gather_2$stat))

# Commented out the data sets that are not being used.
d_stats_spread <- d_stats_gather_2 %>%
  select(team, yr_start, stat, value) %>%
  spread(stat, value) # %>%
# select(one_of(stat_names)) %>%
# setNames(sprintf("%s_%s", names(.), "value"))

d_stats_spread_norm <- d_stats_gather_2 %>%
  select(team, yr_start, stat, value_norm) %>%
  spread(stat, value_norm) %>%
  select(one_of(stat_names)) %>%
  setNames(sprintf("%s_%s", names(.), "norm")) %>%
  cbind(select(d_stats_spread, team, yr_start, w)) %>%
  as_tibble()

# d_stats_spread_rnk <- d_stats_gather_2 %>%
#   select(team, yr_start, stat, value_rnk) %>%
#   spread(stat, value_rnk) %>%
#   select(one_of(stat_names)) %>%
#   setNames(sprintf("%s_%s", names(.), "rnk")) %>%
#   cbind(select(d_stats_spread, team, yr_start, w)) %>%
#   as_tibble()

d_stats_spread_lag1 <- d_stats_gather_2 %>%
  select(team, yr_start, stat, value_lag1) %>%
  spread(stat, value_lag1) %>%
  select(one_of(stat_names)) %>%
  setNames(sprintf("%s_%s", names(.), "lag1")) %>%
  cbind(select(d_stats_spread, team, yr_start, w)) %>%
  as_tibble()

# d_stats_spread_diff1 <- d_stats_gather_2 %>%
#   select(team, yr_start, stat, value_diff1) %>%
#   spread(stat, value_diff1) %>%
#   select(one_of(stat_names)) %>%
#   setNames(sprintf("%s_%s", names(.), "diff1")) %>%
#   cbind(select(d_stats_spread, team, yr_start, w)) %>%
#   as_tibble()

# d_stats_spread_lag1_norm <- d_stats_gather_2 %>%
#   select(team, yr_start, stat, value_lag1_norm) %>%
#   spread(stat, value_lag1_norm) %>%
#   select(one_of(stat_names)) %>%
#   setNames(sprintf("%s_%s", names(.), "lag1_norm")) %>%
#   cbind(select(d_stats_spread, team, yr_start, w)) %>%
#   as_tibble()

d_stats_spread_diff1_norm <- d_stats_gather_2 %>%
  select(team, yr_start, stat, value_diff1_norm) %>%
  spread(stat, value_diff1_norm) %>%
  select(one_of(stat_names)) %>%
  setNames(sprintf("%s_%s", names(.), "diff1_norm")) %>%
  cbind(select(d_stats_spread, team, yr_start, w)) %>%
  as_tibble()

# No need to join all the data together, although it is possible.
# d_stats_spread_join <- d_stats_spread %>%
#   inner_join(d_stats_spread_norm, by = cols_join_2) %>%
#   # inner_join(d_stats_spread_rnk, by = cols_join_2) %>%
#   inner_join(d_stats_spread_lag1, by = cols_join_2) %>%
#   inner_join(d_stats_spread_diff1, by = cols_join_2)
#   inner_join(d_stats_spread_lag1_norm, by = cols_join_2) %>%
#   inner_join(d_stats_spread_diff1_norm, by = cols_join_2)
#'
#'
#' Note that I could have joined all of the data sets together, but that's
#' uncessary because each data set has the needed variables to perform
#' model evaluation. Only if I wish to compare variables of different types
#' (i.e. raw and normalized, lag1 and diff1, etc.).
#'
#' ## Diff1 Model
#'
#' First, I can simply look at the correlation among all predictor variables.
#' The response variable is also included. I'm ommitting the na values because
#' they can make things look ugly and unintentionally mess up analysis
#' when performing training/testing and/or validation.#'
#'
#' To create the correlation plot, I followed the example shown at
#' http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization.
#'
#' Other tips were found at
#' http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software.
#'
#'
d_model_diff1 <-
  na.omit(d_stats_spread_diff1_norm[1:length(stat_names)])

suppressWarnings(suppressMessages(library(reshape2)))

get_lower_tri <- function(d_cor_diff1) {
  d_cor_diff1[upper.tri(d_cor_diff1)] <- NA
  return(d_cor_diff1)
}

get_upper_tri <- function(d_cor_diff1) {
  d_cor_diff1[lower.tri(d_cor_diff1)] <- NA
  return(d_cor_diff1)
}

reorder_d_cor <- function(d_cor_diff1) {
  # Use correlation between variables as distance
  dd <- as.dist((1 - d_cor_diff1) / 2)
  hc <- hclust(dd)
  d_cor_diff1 <- d_cor_diff1[hc$order, hc$order]
}

d_cor_diff1 <- round(cor(d_model_diff1), 1)

# This is probably the simplest way to visualize correlations.
# symnum(d_cor_diff1)

# This is another way to visualize correlations. The output is fairly complex.
# suppressWarnings(suppressMessages(library(PerformanceAnalytics)))
# chart.Correlation(d_cor_diff1, histogram = TRUE, pch = 19)

# suppressWarnings(suppressMessages(library(Hmisc)))
# flatten_d_cor <- function(d_cor, m_p) {
#   ut <- upper.tri(d_cor)
#   data.frame(
#     row = rownames(d_cor)[row(d_cor)[ut]],
#     column = rownames(d_cor)[col(d_cor)[ut]],
#     cor  =(d_cor)[ut],
#     p = m_p[ut]
#   )
# }


# # This is yet another way to visualize correlations.
# # Besides the ggplot2 way shown below, this is probably my favorite.
library(corrplot)
corrplot(d_cor_diff1, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# # A plot with corrplot() again, this time using the p values.
# d_cor_2 <- rcorr(as.matrix(round(cor(d_model_diff1), 2)))
# # d_cor_2_flat <- flatten_d_cor(d_cor_2$r, d_cor_2$P)
#
# plot_model_diff1_cor <- corrplot(d_cor_2$r, type="upper", order="hclust",
#          p.mat = d_cor_2$P, sig.level = 0.01, insig = "blank")
# plot_model_diff1_cor

# A visualization of correlations using ggplot2.
d_cor_diff1_melt <- melt(get_lower_tri(reorder_d_cor(d_cor_diff1)))

# Renaming the variables isn't working for some reason...
# library(stringr)
# d_cor_diff1_melt_2 <- d_cor_diff1_melt
# d_cor_diff1_melt_2$Var1 <- factor(str_replace(d_cor_diff1_melt$Var1, "_norm", ""),
#                                   ordered = is.ordered(d_cor_diff1_melt_2$Var1))
# d_cor_diff1_melt_2$Var2 <- factor(str_replace(d_cor_diff1_melt$Var2, "_norm", ""),
#                                   ordered = is.ordered(d_cor_diff1_melt_2$Var2))


plot_model_diff1_cor <-
  d_cor_diff1_melt %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    # mipoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    size = 8,
    hjust = 1
  )) +
  coord_fixed() +
  geom_text(aes(x = Var1, y = Var2, label = value),
            color = "black",
            size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank() #,
    # legend.justification = c(1, 0),
    # legend.position = c(0.6, 0.7),
    # legend.direction = "horizontal"
  ) # +
# guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
#                              title.position = "top", title.hjust = 0.5))
plot_model_diff1_cor

#'
#'
#'
y_name <- names(d_model_diff1)[1]
x_names <- names(d_model_diff1)[2:(length(stat_names))]
for (i in 1:length(x_names)) {
  if (i == 1) {
    fmla_diff1 <- paste("`", y_name, "` ~", "`", x_names[i], "`")
  } else {
    fmla_diff1 <- paste(fmla_diff1, "+", "`", x_names[i], "`")
  }
}
fmla_diff1 <- gsub(" ", "", fmla_diff1)
lm_diff1_fit_full <- lm(fmla_diff1, data = d_model_diff1)
summary(lm_diff1_fit_full)
#'
#'
#' Now I attempt to subset the predictor variables in an attempt
#' to create a better model that is less vulernable to "overfitting."
#'
#'

suppressWarnings(suppressMessages(library(leaps)))
d_y <-
  na.omit(data.matrix(dplyr::select(d_model_diff1, matches(y_name))))
d_x <-
  na.omit(data.matrix(dplyr::select(d_model_diff1, one_of(x_names))))
subset_fit_full <- regsubsets(d_x, d_y, nvmax = length(x_names))

subset_summary <- summary(subset_fit_full)
# names(subset_summary)
subset_summary$rsq
#'
#'
#' I can plot some of the evaluation statistics to get a better understaning
#' of the model output and its implications.
#'
#'
# graphics.off()
# par(mar = c(1, 1, 1, 1))

par(mfrow = c(2, 2))
# plot(subset_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

# plot(subset_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
# points(which.max(subset_summary$adjr2), subset_summary$adjr2[which.max(subset_summary$adjr2)],
#        col = "red", cex = 2, pch = 20)
which.max(subset_summary$adjr2)

# plot(subset_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
# points(which.min(subset_summary$cp), subset_summary$cp[which.min(subset_summary$cp)],
#        col = "red", cex = 2, pch = 20)
which.min(subset_summary$cp)

# plot(subset_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
# points(which.min(subset_summary$bic), subset_summary$bic[which.min(subset_summary$bic)],
#        col = "red", cex = 2, pch = 20)
which.min(subset_summary$bic)

#'
#'
#' Below is a way to visualize the variables chosen in the subsetting algorithm
#' for each iteration.
#'
#'
# graphics.off()
# par(mar = c(1, 1, 1, 1))
#
# par(mfrow = c(1, 1))
# # plot(subset_fit_full, scale = "r2")
# # plot(subset_fit_full, scale = "adjr2")
# # plot(subset_fit_full, scale = "Cp")
# # plot(subset_fit_full, scale = "bic")

# Insert a ggplot here.

#'
#'
#'
#'
#'
coef(subset_fit_full, which.max(subset_summary$adjr2))
coef(subset_fit_full, which.min(subset_summary$cp))
coef(subset_fit_full, which.min(subset_summary$bic))
#'
#'
#' Forward and backwards step-wise subsetting.
#'
#'
subset_fit_fwd = regsubsets(d_x, d_y, nvmax = length(x_names), method = "forward")
which.min(summary(subset_fit_fwd)$bic)
subset_fit_bwd = regsubsets(d_x, d_y, nvmax = length(x_names), method = "backward")
which.min(summary(subset_fit_bwd)$bic)
#'
#'
#' Here, I'm doing some "manual" training and testing of the subset model.
#' I'm choosing to not use the value identified by the BIC on the full dataset.
#' Instead, I'm allowing the `regsubsets()` function re-calculate the opimal number
#' of predictors to use.
#'
set.seed(1)
vector_train <-
  sample(c(TRUE, FALSE), nrow(d_model_diff1), replace = TRUE)
vector_test <- (!vector_train)
subset_fit_train <-
  regsubsets(d_x[vector_train, ], d_y[vector_train, ],
             nvmax = length(x_names))
# subset_fit_train <- regsubsets(d_x[vector_train, ], d_y[vector_train, ],
#                               nvmax = which.min(subset_summary$bic)+1)
subset_fit_test <- model.matrix(w_diff1_norm ~ .,
                                data = d_model_diff1[vector_test, ])
#'
#'
#'
#'
#'

subset_fit_val_errors <- rep(NA, length(x_names))
for (i in 1:length(x_names)) {
  subset_fit_coef_i <- coef(subset_fit_train, id = i)
  subset_fit_pred <-
    subset_fit_test[, names(subset_fit_coef_i)] %*% subset_fit_coef_i
  subset_fit_val_errors[i] <-
    mean((d_model_diff1$w_diff1_norm[vector_test] - subset_fit_pred) ^ 2)
}

which.min(subset_fit_val_errors)
coef(subset_fit_train, which.min(subset_fit_val_errors))
#'
#' The fit isn't too bad, but it also isn't all that great. There's some more work
#' that could be done. Anyways, it's good to see that the model infers what most
#' NBA fans assume: that offensive and deffensive efficiency are really important
#' and are probably two of the best indicators of a team's performance.
#'
#' Notably, I could also do some manual cross validation here too, but I'll leave that
#' for something else.
#'
#' ## Lag1 Model
#'
#' Now I'll take a look a closer look at the two variables indicated to be most
#' significant in the above analysis: offensive and defensive efficiency.
#' This time, I'm going to use the lag1 data set to predict current year win total.
#'
#'
#'
plot_off_deff_efficiency_norm <-
  ggplot(d_stats_spread_norm,
         aes(x = off_efficiency_norm, y = def_efficiency_norm)) +
  geom_point(aes(size = w, colour = w)) +
  geom_smooth(se = FALSE) +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) +
  ggtitle("Offensive vs. Defensive Efficiency") +
  theme(
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12)
  )
#'
#'
#' This model gauges the predictive value of the prior year's
#' offensive and defensive efficiency in predicting this year's win total.
#'
#'
d_model_lag1 <- d_stats_spread_lag1
fmla_1 <- "w ~ off_efficiency_lag1 + def_efficiency_lag1"
lm_lag1_fit_full <- lm(fmla_1, data = d_model_lag1)
summary(lm_lag1_fit_full)
#'
#'
#' Now look at the uncertainty intervals for reasonable values of the statistics.
#'
#'
df_sample <- data.frame(
  off_efficiency_lag1 = seq(0.8, 1.2, 0.1),
  def_efficiency_lag1 = seq(0.8, 1.2, 0.1)
)

confint(lm_lag1_fit_full)
predict(lm_lag1_fit_full, df_sample, interval = "confidence")
predict(lm_lag1_fit_full, df_sample, interval = "prediction")
#'
#'
#' Note the differences in confidence and prediction intervals.
#' Confidence intervals should always be smaller because they quantify
#' the average uncertainty for all points.
#' On the other hand, prediciton intervals quantify uncertainty for a given
#' point.
#'
#'
par(mfrow = c(2, 2))
# plot(lm_lag1_fit_full)
#'
#'
#' This is just a visualization of the residuals. Note that I have not split
#' the data set into testing and training sets and have not attempted any kind
#' of cross-validation yet.
#'
#'
par(mfrow = c(1, 1))
# plot(predict(lm_lag1_fit_full), residuals(lm_lag1_fit_full))
# # plot(predict(lm_lag1_fit_full), rstudent(lm_lag1_fit_full))
# # plot(hatvalues(lm_lag1_fit_full))
# which.max(hatvalues(lm_lag1_fit_full))
#'
#'
#' There are several ways of evaluating models. Previously, I used the `R^2`
#' value. Here, I show it again, in addition to looking at the
#' variance explained using the `vif()` function from the `car` package
#'
#'
summary(lm_lag1_fit_full)$r.sq
# suppressWarnings(suppressMessages(library(car)))
# vif(lm_lag1_fit_full)
#'
#' ## Ridge Regression (RR)
#'
suppressWarnings(suppressMessages(library(glmnet)))
# Note that alpha = 0 for RR in glmnet() function.
vector_lambdas <- 10 ^ seq(10, -2, length = 100)
ridge_fit_full_0 <-
  glmnet(d_x, d_y, alpha = 0, lambda = vector_lambdas)

ridge_fit_full <- glmnet(d_x, d_y, alpha = 0)
#'
#' Creating varaibles for training/testing.
#'
set.seed(1)
vector_train_2 <- sample(1:nrow(d_x), nrow(d_x) / 2)
vector_test_2 <- (!vector_train)

set.seed(1)
ridge_fit_cv <-
  cv.glmnet(d_x[vector_train, ], d_y[vector_train, ], alpha = 0)
# plot(ridge_fit_cv)
ridge_fit_cv$lambda.min
#'
#' ### Cross Validation
#'
ridge_fit_pred_cv <-
  predict(ridge_fit_full, s = ridge_fit_cv$lambda.min,  newx = d_x[vector_test, ])

# This is the CV test set MSE.
mean((ridge_fit_pred_cv - d_y[vector_test, ]) ^ 2)

#'
#' ### Manual Training/Testing
#'
vector_lambdas <- 10 ^ seq(10, -2, length = 100)
ridge_fit_train <-
  glmnet(d_x[vector_train, ], d_y[vector_train, ], alpha = 0, lambda = vector_lambdas)
ridge_pred <-
  predict(ridge_fit_train, s = ridge_fit_cv$lambda.min, newx = d_x[vector_test, ])

# This is the manual vector_train/test test MSE.
mean((ridge_pred - d_y[vector_test, ]) ^ 2)


# This is the test MSE if only the intercept is used.
mean((mean(d_y[vector_train, ]) - d_y[vector_test, ]) ^ 2)

# This is equivalent to a model with only the intercept.
ridge_pred_intercept <-
  predict(ridge_fit_train, s = 1e10, newx = d_x[vector_test, ])
mean((ridge_pred_intercept - d_y[vector_test, ]) ^ 2)

# This is an unpenalized LS model. Note that s = 0.
# Also, exact must be set to T in order to prevent predict()
# from interpolating over vector_lambdas.
ridge_pred_ls <-
  predict(ridge_fit_train,
          s = 0,
          newx = d_x[vector_test, ],
          exact = T)
mean((ridge_pred_ls - d_y[vector_test, ]) ^ 2)

#'
#' Now I re-fit the model using the lambda identified from
#' cross validation. For the sake of comparison,
#' I evaluate the MSE on the same test set even though the test set
#' is part of the CV training/testing sets. This might
#' cause the test MSE to be slighly lower than it would be otherwise.
#'
# This is the re-fit model's coefficients.
ridge_fit_full_coefs <-
  predict(ridge_fit_full, s = ridge_fit_cv$lambda.min, type = "coefficients")
ridge_fit_full_coefs
#'
#' # The Lasso
#'
lasso_fit_train <-
  glmnet(d_x[vector_train, ], d_y[vector_train], alpha = 1, lambda = vector_lambdas)
str(lasso_fit_train)
sapply(lasso_fit_train, "class")
plot(lasso_fit_train)

set.seed(1)
lasso_fit_cv <-
  cv.glmnet(d_x[vector_train, ], d_y[vector_train], alpha = 1)
plot(lasso_fit_cv)
lasso_fit_cv$lambda.min

lasso_pred <-
  predict(lasso_fit_train, s = lasso_fit_cv$lambda.min, newx = d_x[vector_test, ])
mean((lasso_pred - d_y[vector_test, ]) ^ 2)

lasso_fit_full <-
  glmnet(d_x, d_y, alpha = 1, lambda = vector_lambdas)
lasso_fit_full_coefs <-
  predict(lasso_fit_full, s = lasso_fit_cv$lambda.min,
          type = "coefficients")[1:dim(coef(lasso_fit_full))[[1]],]
lasso_fit_full_coefs
