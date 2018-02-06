
# Import packages. ####
remove(list = ls())
# library(rio) # don't import this since plyr might overwrite dplyr
library(tidyverse)
library(readxl)

# TODO: Need a way to deal with null values.

# Define some variables. ####
dir_nba <- "C:/Users/aelhabr/Dropbox/data_science/projects/nba/"
setwd(dir_nba)
wb_nba <- "db_nba.xlsm"
setwd(dir_nba)

# Import win total data. ####
# options(stringAsFactor = True)
# Must use read_xlsx to import a .xlsm file.
df_d <- read_xlsx(wb_nba, sheet = "d_rs_team_win_totals")
if ("team_abbrv" %in% names(df_d)) {
  df_d <- rename(df_d, team = team_abbrv)
}
g_teams <- read_xlsx(wb_nba, sheet = "g_teams")
# g_teams <- select(g_teams, one_of(c("team_num", "team_abbrv")))
g_teams <- select(g_teams, 2:ncol(g_teams))
df_d <- inner_join(df_d, g_teams, by = c("team" = "team"))

df_d$team_factor <- as.factor(df_d$team)
df_d$team_factor <- factor(df_d$team, ordered = TRUE)
tb_d <- as_tibble(df_d)
tb_d

# Wrangle and explore win total data ####
tb_d <- tb_d %>% mutate(w_diff = (w - w_sportsbook))
tb_d <- tb_d %>% mutate(result_bool_calc2 = ifelse(w > w_sportsbook, "O", ifelse(w < w_sportsbook, "U", "E")))
tb_d <- tb_d %>% mutate(result_bool_calc3 = ifelse(w > w_sportsbook, 1, ifelse(w < w_sportsbook, -1, 0)))
# tb_d
tb_d %>% filter(team == "SA")
tb_d %>% arrange(desc(w_diff))
tb_d %>% group_by(team) %>% summarise(avg = mean(w))
tb_d %>% group_by(team) %>% summarise(avg = mean(w_sportsbook))

# Import pick data. ####
# options(stringsAsFactors = True)
df_p <- read_xlsx(wb_nba, sheet = "p_rs_team_win_totals")
g_persons <- read_xlsx(wb_nba, sheet = "g_persons")
g_persons <- select(g_persons, 2:ncol(g_persons))
df_p <- inner_join(df_p, g_persons, by = c("person" = "person_name"))
df_p$person_factor <- as.factor(df_p$person)
df_p$person_factor <- factor(df_p$person, ordered = TRUE)
df_p <- select(df_p, 2:ncol(df_p))
tb_p <- as_tibble(df_p)
tb_p

# Join win total and pick data. ####
tb_all <- right_join(tb_d, tb_p, by = c("pick_id" = "pick_id"))
tb_all <- tb_all %>% mutate(pick_result_bool_calc2 = ifelse(pick == result_calc, 1, 0))

# Import and join team statistics. ####
wb_nba_teamrankings_scraped <- "db_nba_teamrankings_scraped.xlsx"
# list.files()

# Could make this into a function in order to automate appending data.
# Need a better way of renaming identically named columns.
df_d2 <- read_xlsx(wb_nba_teamrankings_scraped, sheet = "d_ppg")
df_d2 <- select(df_d2, 2:ncol(df_d2))
tb_all <- inner_join(tb_all, df_d2, by = c("yr_start" = "yr_start", "team" = "team"))
tb_all <- rename(tb_all, ppg_rnk = rank)

df_d2 <- read_xlsx(wb_nba_teamrankings_scraped, sheet = "d_win_pct_close_games")
df_d2 <- select(df_d2, 2:ncol(df_d2))
tb_all <- inner_join(tb_all, df_d2, by = c("yr_start" = "yr_start", "team" = "team"))
tb_all <- rename(tb_all, win_pct_close_games_rnk = rank)


# Create a mini tibble to view only important things. ####
cols_mini <- c("yr_start", "team", "w", "w_diff","person", "result_bool_calc", "pick", "pick_result_bool_calc2", "confidence", "ppg_rnk", "ppg", "win_pct_close_games_rnk", "win_pct_close_games")
tb_mini <- select(tb_all, one_of(cols_mini))
# tb_mini[seq(1, nrow(tb_mini), 20),]

# Wrangle and explore joined data. ####
tb_mini %>% group_by(person, team) %>% count(pick)
tb_mini %>% group_by(person, team) %>% summarise(pick_avg = mean(as.numeric(pick_result_bool_calc2))) # %>% filter(person == "tony")

# Write to csv. ####
write_excel_csv(tb_all, "csvs/v_rs_win_totals.csv")
