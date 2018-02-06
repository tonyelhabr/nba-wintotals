
remove(list = ls())
# library(tidyverse)
# library(XML)
library(rvest)
library(tidyr)


dir_data <- "C:/Users/aelhabr/Dropbox/personal/data/"

year <- 2016
base_url <- "http://www.basketball-reference.com/leagues/NBA_"
url <- paste0(base_url, year, ".html")

m <- html_nodes(read_html(url), xpath = '
//*[@id="all_opponent-stats-per_game"]/comment()')
m2 <- html_text(m[1])

t <- html_nodes(read_html(url), 'div id="div_opponent-stats-per_game_clone"')
t <- html_nodes(read_html(url), 'tbody tr td')
html_text(t)
t[200]$node
