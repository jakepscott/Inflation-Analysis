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
  mutate(decade = case_when(year(date) < 1990 ~ "1980's",
                            year(date) >= 1990 & year(date) <2000 ~ "1990's",
                            year(date) >=2000 & year(date) <2010 ~ "2000's",
                            year(date) >=2020 ~ "2010's")) %>% 
  group_by(iso_code) %>% 
  mutate(inflation = (headline_consumer_price_index - lag(headline_consumer_price_index,12))/lag(headline_consumer_price_index,12)*100) %>% 
  ungroup()

data_agg <- data %>% 
  group_by(decade, iso_code) %>% 
  summarise(inflation = mean(inflation, na.rm = T)) %>% 
  ungroup()
  
to_plot <- data_agg %>% 
  left_join(overall_avg) %>% 
  mutate(z_score = (inflation - mean_inf)/sd)

joined <- to_plot %>% 
  select(decade, iso_code, inflation, z_score) %>% 
  right_join(world, by = c("iso_code"="iso_a3")) %>% 
  filter(!is.na(decade)) %>% 
  st_as_sf()

joined %>% 
  ggplot() +
  geom_sf(aes(fill = z_score)) +
  scale_fill_viridis_b(option = "plasma") +
  facet_wrap(~decade) +
  theme_void()

joined %>% 
  ggplot() +
  geom_sf(aes(fill = log(inflation))) +
  scale_fill_viridis_b(option = "plasma") +
  facet_wrap(~decade) +
  theme_void()