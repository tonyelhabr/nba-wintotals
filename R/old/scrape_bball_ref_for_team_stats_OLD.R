
remove(list = ls())
library(tidyverse)
library(XML)

dir_data <- "C:/Users/aelhabr/Dropbox/personal/data/"

season <- 2016
base_url <- "http://www.basketball-reference.com/leagues/NBA_"
url <- paste0(base_url, season, ".html")

db <- readHTMLTable(url)
lapply(db, class)
db_colnames <- colnames(as.data.frame(db[1]))
db <- lapply(db, as.data.frame)
db <- db[1:2]
df1 <- as.data.frame(db[1])
df2 <- as.data.frame(db[2])

write_excel_csv(df1, paste0(dir_data, "nba_team_win_totals_east_", season, ".csv"))
write_excel_csv(df2, paste0(dir_data, "nba_team_win_totals_west_", season, ".csv"))
# df <- rbind(df1, df2)
# db <- rbind(as.data.frame(db[1]), as.data.frame(db[2]))
# lapply(db, write_excel_csv, paste0(dir_data, "nba_team_win_totals_", season, ".csv"))
