
remove(list = ls())
dir_nba <- "C:/Users/aelhabr/Dropbox/data_science/projects/nba/"
setwd(dir_nba)
source("process_rs_team_win_totals.R")

save_ggplot_as_png <- function(input_plot)
{
  ggsave(
    paste0("pngs//", deparse(substitute(input_plot)), ".png"),
    width = 11,
    height = 7,
    units = "in"
  )
}

theme_format_facet <-
  theme(
    axis.text = element_text(size = 8), 
    axis.title = element_blank(), 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    strip.text = element_text(size = 12), 
    legend.position = "none", # legend.position = "bottom",
    axis.text.x = element_blank()
    # legend.title = element_blank()
  )

tb_mini_1 <-
tb_mini %>%
  # group_by(yr_start) %>%
  arrange(desc(yr_start), desc(w))

# Plot the processed data. ####
# Still needs work.
plot_1 <-
  ggplot(filter(tb_mini_1, person == "Tony")) +
  geom_col(aes(x = yr_start, y = w, fill = pick_result_bool_calc2)) +
  # geom_label(aes(label = w)) +
  facet_wrap(~ team) +
  labs(
    title = "Win Total Pick Accuracy",
    subtitle = paste0("Person = ", "Tony")) +
  theme(
    axis.text = element_text(size = 8),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    # axis.text.x = element_blank()
    legend.title = element_blank()
  )
# plot_1
save_ggplot_as_png(plot_1)

# Still needs work.
plot_2 <-
  ggplot(filter(tb_mini_1, yr_start == 2016)) +
  geom_col(aes(x = person, y = pick_result_bool_calc2, fill = pick)) +
  facet_wrap(~ team) +
  labs(
    x = "person",
    y = "correct",
    title = "Win Total Pick Accuracy",
    subtitle = paste0("Year = ", 2016)) +
  theme(
    axis.text = element_text(size = 8), 
    axis.title.y = element_blank(), 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    # axis.text.x = element_blank()
    legend.title = element_blank()
  )
save_ggplot_as_png(plot_2)

plot_3 <-
  ggplot(filter(tb_mini, person == "Tony")) +
  geom_point(aes(x = w, y = w_diff, color = pick_result_bool_calc2, size = pick)) +
  # geom_smooth(aes(x = w, y = w_diff))
  scale_fill_grey()
plot_3

ggplot(filter(tb_all, person == "Tony", yr_start > 2013)) + geom_point(aes(x = w, y = confidence, size = pick_result_bool_calc2))

# plot_facet_1 <- 
#   ggplot(tb_all) + 
#   geom_point(aes(x = team, y = w), color = "red") + 
#   geom_point(aes(x = team, y = w_sportsbook), color = "black") + 
#   facet_grid(yr_start ~ .) + 
#   theme_format_facet
# plot_facet_1
# 
# plot_facet_2 <-
#   ggplot(tb_all) + 
#   geom_point(aes(x = team, y = w_diff, color = team)) +
#   facet_grid(yr_start ~ .) +
#   theme_format_facet
# plot_facet_2
# 
# plot_facet_3 <-
#   ggplot(tb_all) + 
#   geom_point(aes(x = team, y = pick, color = person)) +
#   facet_grid(yr_start ~ person) +
#   theme_format_facet
# plot_facet_3