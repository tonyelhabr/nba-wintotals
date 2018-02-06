
# Import packages. ####
remove(list = ls())
library(tidyverse)
library(readxl)
# library(rio)

# Define some variables. ####
dir_nba <- "C:/Users/aelhabr/Dropbox/data_science/projects/nba/"
setwd(dir_nba)
wb_nba <- "db_nba.xlsm"
setwd(dir_nba)

# Import win total data. ####
# options(stringAsFactor = True)
df_d <- read_xlsx(wb_nba, sheet = "d_rs_team_win_totals")
df_d <- dplyr::rename(df_d, team = team_abbrv)
g_teams <- read_xlsx(wb_nba, sheet = "g_teams")
g_teams <- select(g_teams, one_of(c("team_num", "team")))
df_d <- inner_join(df_d, g_teams, by = c("team" = "team"))

df_d$team_factor <- as.factor(df_d$team)
df_d$team_factor <- factor(df_d$team, ordered = TRUE)
tb_d <- as_tibble(df_d)
tb_d

# Wrangle and explore win total data ####
tb_d <- tb_d %>% mutate(win_diff = abs(w - w_sportsbook))
tb_d <- tb_d %>% mutate(result_bool_calc_2 = ifelse(w > w_sportsbook, "O", ifelse(w < w_sportsbook, "U", "E")))
tb_d <- tb_d %>% mutate(result_bool_calc_3 = ifelse(w > w_sportsbook, 1, ifelse(w < w_sportsbook, 0, "na")))
# tb_d
tb_d %>% filter(team == "SA")
tb_d %>% arrange(desc(win_diff))
tb_d %>% group_by(team) %>% summarise(avg = mean(w))
tb_d %>% group_by(team) %>% summarise(avg = mean(w_sportsbook))

# Import pick data. ####
# options(stringsAsFactors = True)
df_p <- read_xlsx(wb_nba, sheet = "p_rs_team_win_totals")
g_persons <- read_xlsx(wb_nba, sheet = "g_persons")
g_persons <- select(g_persons, one_of(c("person_num", "person_name")))
df_p <- inner_join(df_p, g_persons, by = c("person" = "person_name"))
df_p$person_factor <- as.factor(df_p$person)
df_p$person_factor <- factor(df_p$person, ordered = TRUE)
tb_p <- as_tibble(df_p)
tb_p

# Join win total and pick data. ####
tb_all <- right_join(tb_d, tb_p, by = c("pick_id" = "pick_id"))
tb_all <-
  tb_all %>%
  mutate(pick_result_bool_calc_2 = ifelse(pick == result_calc, 1, 0))
cols <- c("yr_start", "team", "person", "result_bool_calc", "pick", "pick_result_bool_calc_2")
tb_mini <-
  tb_all %>%
  select(one_of(cols))
tb_mini[seq(1, nrow(tb_mini), 20),]

# Wrangle and explore joined data. ####
tb_mini %>% group_by(person, team) %>% dplyr::count(pick)
tb_mini %>% group_by(person, team) %>% summarise(pick_avg = mean(as.numeric(pick_result_bool_calc_2))) # %>% filter(person == "tony")

yrs <- unique(tb_d$yr_start) # returns a vector, not using this yet
tms <- unique(tb_d$team) # returns a vector, not using this yet

yrs_list <- list()

# testing before loop
# i <- 1
# tb_all %>% filter(season_year_start == yrs[i])

# for (i in yrs[1]:yrs[length(yrs)]){
for (i in seq_along(yrs)){
  yrs_list[[i]] <- tb_mini %>% filter(yr_start == yrs[i])
}

theme_format_facet <-
  theme(
    axis.text = element_text(size = 8), 
    axis.title = element_blank(), 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    strip.text = element_text(size = 12), 
    legend.position = "bottom", 
    axis.text.x = element_blank()# , 
    # legend.title = element_blank()
  )

# str(tb_all)

# Write to csv. ####
# Output processed data for use elsewhere.
write_excel_csv(tb_all, paste0(dir_nba, "nba_picks-win_totals_processed.csv"))

# Plot the processed data. ####
plot_facet_1 <- 
  ggplot(tb_d) + 
  geom_point(aes(x = team, y = w), color = "red") + 
  geom_point(aes(x = team, y = w_sportsbook), color = "black") + 
  facet_grid(season_year_start ~ .) + 
  theme_format_facet
plot_facet_1

plot_facet_2 <- ggplot(tb_d) + 
  geom_point(aes(x = team, y = win_diff), color = "blue") +
  facet_grid(season_year_start ~ .) +
  theme_format_facet
plot_facet_2

plot_facet_3 <- ggplot(tb_all) + 
  geom_point(aes(x = team, y = pick)) +
  facet_grid(season_year_start ~ person) +
  theme_format_facet
plot_facet_3
