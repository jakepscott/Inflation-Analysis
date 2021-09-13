# Load Libs and Data ------------------------------------------------------
library(here)
library(ggtext)
library(RColorBrewer)
#Set up font 
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# This creates data and data_agg
source(here("00_Load-Data.R"))

# Aggregate data
data_agg <- data %>% 
  group_by(country_code, year=year(date)) %>% 
  summarise(inflation = mean(inflation, na.rm = T)) %>% 
  ungroup()
  

#Load in world bank population data
population <- read_csv(here("data/population.csv"), skip = 4) %>% 
  pivot_longer(!1:4, names_to = "year", values_to = "population") %>% 
  mutate(year = as.numeric(year)) %>% 
  clean_names() %>% 
  select(country_code, year, population)

#Join population and inflation data
full_data <- data_agg %>% 
  left_join(population)

# Make inflation categories
full_data <- full_data %>% mutate(inflation_level = case_when(inflation > 50 ~ ">50%",
                                   #inflation >= 50 & inflation < 100 ~ "Super High",
                                   #inflation >= 25 & inflation < 50 ~ "Very High",
                                   inflation >= 10 & inflation < 50 ~ "10-50%",
                                   inflation >= 3 & inflation < 10 ~ "3-10%",
                                   inflation >= 1 & inflation < 3 ~ "1-3%",
                                   inflation < 1 ~ "<1%",
                                   T ~ NA_character_))

# Make the inflation categories into an order factor
full_data$inflation_level <- factor(full_data$inflation_level, ordered = T,
                                   labels = c("<1%", '1-3%', "3-10%", "10-50%", ">50%"),
                                   levels = c("<1%", '1-3%', "3-10%", "10-50%", ">50%"))



plot_data <- full_data %>% 
  #filter(country_code %in% countries_in_1985$country_code) %>% 
  na.omit() %>% 
  group_by(year,inflation_level) %>% 
  summarise(population = sum(population, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(total_pop = sum(population)) %>% 
  ungroup() %>% 
  mutate(percent_pop = population/total_pop)

plot_data %>% 
  na.omit() %>% 
  ggplot(aes(year, percent_pop, fill = inflation_level)) +
  geom_area() +
  scale_x_continuous(expand = expansion(c(0,0))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(c(0,0))) +
  theme_minimal()



# Line Plot ---------------------------------------------------------------
# See how many people were in each inflation category by decade, for subtitle
plot_data %>% mutate(decade = case_when(year < 1980 ~ "1970's",
                                        year >= 1980 & year <1990 ~ "1980's",
                                        year >= 1990 & year <2000 ~ "1990's",
                                        year >=2000 & year <2010 ~ "2000's",
                                        year >=2010 ~ "2010's")) %>% 
  group_by(decade, inflation_level) %>% 
  summarise(mean = mean(percent_pop)) 

my_pal = brewer.pal(n = 9, "YlOrRd")[5:9] 

plot_data %>% 
  #filter(inflation_level == "1-3") %>% 
  ggplot(aes(year, percent_pop)) +
  geom_line(aes(color = inflation_level), show.legend = F) +
  geom_smooth(method = "lm", aes(color = inflation_level), show.legend = F) +
  scale_color_manual(values = my_pal) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~inflation_level) +
  labs(title = "The percent of people living in countries with low to moderate inflation rates has increased notably since the 1970s",
       subtitle = "The percent of people living in countries where annual inflation is below 3% has risen from just 12% to 42%, while the number living in countries <br>where annual inflation is 10% or high has declined from 40% to 13%",
       y = "Percent of Population",
       x = NULL,
       caption = "Plot: @jakepscott2020 | Data: World Bank via Ha, Kose, & Ohnsorge (2021)") +
  theme_minimal(base_size = 12, base_family = "Roboto Condensed") +
  theme(plot.title = element_markdown(face = "bold", size = rel(1.2)),
        plot.subtitle = element_markdown(face = "plain", size = rel(1), color = "grey30"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey50"),
        legend.position = "none",
        axis.text.x = element_text(size=rel(1)),
        plot.title.position = "plot")

ggsave(here("figures/percent_of_pop_lines.png"), dpi = 600, bg = "white", 
       units = "in", height = 6, width = 6*1.618)
