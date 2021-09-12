# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(glue)
library(lubridate)
library(maps)
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(gganimate)

# Load Data ---------------------------------------------------------------
data_raw <- read_xlsx(here("data/Inflation-data.xlsx"), sheet = 3)
imf_to_iso <- read_xlsx(here("data/co.xlsx"), skip = 1) %>% 
  select(`IMF Code`, `ISO Code`) %>% 
  clean_names()
world <- ne_countries(scale = "medium", returnclass = "sf")

# Clean Data --------------------------------------------------------------
#Pivot date columns into a single date column and clean column names
data <- data_raw %>% 
  pivot_longer(!(`Country Code`:`Series Name`),
               names_to = "date", 
               #label the values column whatever the variable is
               values_to = data_raw$`Series Name`[1]) %>% 
  clean_names()

#Set up the date column
data <- data  %>% 
  mutate(year = substr(date,1,4),
         month = substr(date, 5,6),
         date = as.Date(glue("{year}-{month}-01")))

#Join in iso codes
data <- data %>% 
  mutate(imf_country_code=as.character(imf_country_code)) %>% 
  left_join(imf_to_iso, by = c("imf_country_code" = "imf_code"))

data <- data %>%
  filter(year>=1980) %>%
  mutate(decade = case_when(year(date) < 1985 ~ "1980-1984",
                            year(date) >= 1985 & year(date) <1990 ~ "1985-1989",
                            year(date) >=1990 & year(date) <1995 ~ "1990-1994's",
                            year(date) >=1995 & year(date) <2000 ~ "1995-1999",
                            year(date) >=2000 & year(date) <2005 ~ "2000-2004",
                            year(date) >=2005 & year(date) <2010 ~ "2005-2009",
                            year(date) >=2010 & year(date) <2015 ~ "2010-2014",
                            year(date) >=2015 ~ "2015-2020")) %>% 
  group_by(iso_code) %>% 
  mutate(inflation = (headline_consumer_price_index - lag(headline_consumer_price_index,12))/lag(headline_consumer_price_index,12)*100) %>% 
  ungroup()

data_agg <- data %>% 
  group_by(decade, iso_code) %>% 
  summarise(inflation = mean(inflation, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(inflation_level = case_when(inflation > 50 ~ ">50",
                                     #inflation >= 50 & inflation < 100 ~ "Super High",
                                     #inflation >= 25 & inflation < 50 ~ "Very High",
                                     inflation >= 10 & inflation < 50 ~ "10-50",
                                     inflation >= 2.5 & inflation < 10 ~ "5-10",
                                     inflation >= 1 & inflation < 2.5 ~ "1-5",
                                     inflation < 1 ~ "<1",
                                     T ~ NA_character_))

data_agg$inflation_level <- factor(data_agg$inflation_level, ordered = T,
       labels = c(">50","10-50","5-10",'1-5',"<1"),
       levels = c(">50","10-50","5-10",'1-5',"<1"))
  
overall_avg <- data %>% 
  group_by(iso_code) %>% 
  summarise(mean_inf = mean(inflation, na.rm = T),
            sd = sd(inflation, na.rm = T))

to_plot <- data_agg %>% 
  left_join(overall_avg) %>% 
  mutate(z_score = (inflation - mean_inf)/sd)

joined <- to_plot %>%
  select(decade, iso_code, inflation, inflation_level, z_score) %>%
  right_join(world, by = c("iso_code"="iso_a3")) %>%
  filter(!is.na(decade)) %>%
  st_as_sf()
# 
# joined %>% 
#   ggplot() +
#   geom_sf(aes(fill = z_score)) +
#   scale_fill_viridis_b(option = "plasma") +
#   facet_wrap(~decade) +
#   theme_void()

# joined %>% 
#   ggplot() +
#   geom_sf(aes(fill = log(inflation))) +
#   scale_fill_viridis_b(option = "plasma") +
#   facet_wrap(~decade) +
#   theme_void()

# joined %>% 
#   ggplot() +
#   geom_sf(aes(fill = inflation_level)) +
#   scale_fill_brewer(palette =  "YlOrRd", direction = -1, na.value = "grey") +
#   guides(fill = guide_legend(nrow = 1)) +
#   facet_wrap(~decade) +
#   theme_void() +
#   theme(legend.position = "top") 

animated <- joined %>% 
  ggplot() +
  geom_sf(aes(fill = inflation_level)) +
  transition_manual(decade) +
  scale_fill_brewer(palette =  "YlOrRd", direction = -1, na.value = "grey") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = 'Frame {frame} of {nframes}') +
  theme_void() +
  theme(legend.position = "top")

animated +
  ggtitle('Now showing {current_frame}',
          subtitle = 'Frame {frame} of {nframes}')


anim_save(animation = animated, filename = "Map.gif", path = here("figures/"))
