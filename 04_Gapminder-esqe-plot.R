#Inflation versus gdp with cicle sized by population over time by continent
library(gapminder)
gapminder <- gapminder %>% 
  left_join(country_codes)

gapminder_gif <- data_agg %>% 
  left_join(gapminder, by = c("iso_code"="iso_alpha", "year")) %>%
  na.omit() %>% 
  mutate(inflation = ifelse(inflation>30, 30, inflation)) %>% 
  ggplot(aes((inflation), gdpPercap, color = country, size = pop)) +
  geom_point(alpha = 0.7) +
  scale_y_log10() +
  scale_color_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  facet_wrap(~continent) +
  labs(title = "Year: {as.integer(frame_time)}", y = 'GDP per capita', x = 'Annual CPI') +
  transition_time(year) +
  ease_aes('linear') + 
  theme_minimal() +
  theme(legend.position = "none")


anim_save(animation = gapminder_gif, 
          filename = "gapminder_gif.gif", 
          path = here("figures/"),
          nframes = 200, 
          fps = 10, 
          end_pause = 15)

  data_agg %>% 
  left_join(gapminder, by = c("iso_code"="iso_alpha", "year")) %>%
  na.omit() %>% 
  ggplot(aes(year, gdpPercap, color = country)) +
  geom_line() +
  facet_wrap(~continent, scales = "free") +
  theme(legend.position = "none")
