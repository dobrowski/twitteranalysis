
eds %>%
  filter(created_at > "2019-01-01") %>%
  group_by(screen_name) %>%
  ts_plot( "2 days") +
  theme_hc() +
  scale_color_few("Medium")
