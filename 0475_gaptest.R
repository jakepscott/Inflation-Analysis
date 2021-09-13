#Inflation versus gdp with cicle sized by population over time by continent
library(gapminder)
library(here)
library(tidyverse)
source(here("00_Load-Data.R"))

#Get population after 2007
pop_after_2007 <- read_csv(here("data/population_total.csv")) %>% 
  pivot_longer(-country, names_to = "year", values_to = "pop") %>% 
  mutate(pop = str_remove(pop,"M"),
         pop = as.numeric(pop)*1000000) 

#Get gdp per cap after 2007
gdp_per_cap <- read_csv(here("data/gdppercapita_us_inflation_adjusted.csv")) %>% 
  pivot_longer(-country, names_to = "year", values_to = "gdpPercap") %>% 
  na.omit() %>% 
  mutate(gdpPercap = str_remove(gdpPercap, "k"),
         gdpPercap = as.numeric(gdpPercap)*1000) 

#Get continent country table
count_cont <- gapminder %>% 
  distinct(country, continent)

#Join post 2007 pop and per capita
post_2007 <- pop_after_2007 %>% 
  left_join(gdp_per_cap) %>% 
  left_join(count_cont) %>% 
  left_join(gapminder %>% distinct(country, iso_alpha)) %>% 
  mutate(year = as.integer(year))


full_data <- data_agg %>% 
  left_join(post_2007, by = c("iso_code"="iso_alpha", "year")) %>%
  select(iso_code,year,inflation,country, continent, pop, gdpPercap) %>% 
  na.omit() %>% 
  mutate(inflation = ifelse(inflation>30, 30, inflation)) %>% 
  arrange(iso_code,year)

gapminder_gif <- full_data %>% 
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
          filename = "gapminder_gif_extended_test.gif", 
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
