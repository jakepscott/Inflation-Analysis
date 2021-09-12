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
library(viridis)


# Load Data ---------------------------------------------------------------
#Read in inflarion data
data_raw <- read_xlsx(here("data/Inflation-data.xlsx"), sheet = 3)
#Get IMF to is code conversion
imf_to_iso <- read_xlsx(here("data/co.xlsx"), skip = 1) %>% 
  select(`IMF Code`, `ISO Code`) %>% 
  clean_names()
#Get the data for a map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Clean Data --------------------------------------------------------------
#Pivot date columns into a single date column and clean column names
data <- data_raw %>% 
  pivot_longer(!(`Country Code`:`Series Name`),
               names_to = "date", 
               #label the values column whatever the variable is
               values_to = data_raw$`Series Name`[1]) %>% 
  clean_names() %>% 
  select(imf_country_code, date, headline_consumer_price_index)

#Set up the date column
data <- data  %>% 
  mutate(year = substr(date,1,4),
         month = substr(date, 5,6),
         date = as.Date(glue("{year}-{month}-01")))

#Join in iso codes
data <- data %>% 
  mutate(imf_country_code=as.character(imf_country_code)) %>% 
  left_join(imf_to_iso, by = c("imf_country_code" = "imf_code"))

#Make a variable for each 5 year period
data <- data %>%
  group_by(iso_code) %>% 
  #Calculate the actual inflation rate
  mutate(inflation = (headline_consumer_price_index - lag(headline_consumer_price_index,12))/lag(headline_consumer_price_index,12)*100) %>% 
  ungroup()

#Get yearly inflation by taking the average of the 12 quarter inflation by quarter
data_agg <- data %>% 
  group_by(iso_code, year) %>% 
  summarise(inflation = mean(inflation, na.rm = T)) %>% 
  ungroup()

# Create inflation categories
data_agg <- data_agg %>% 
  mutate(inflation_level = case_when(inflation > 50 ~ ">50",
                                     #inflation >= 50 & inflation < 100 ~ "Super High",
                                     #inflation >= 25 & inflation < 50 ~ "Very High",
                                     inflation >= 10 & inflation < 50 ~ "10-49",
                                     inflation >= 5 & inflation < 10 ~ "5-9",
                                     inflation >= 0.5 & inflation < 5 ~ "0.5-4",
                                     inflation < 0.5 ~ "<0.5",
                                     T ~ NA_character_))

# Make the inflation categories into an order factor
data_agg$inflation_level <- factor(data_agg$inflation_level, ordered = T,
                               labels = c(">50","10-49","5-9",'0.5-4',"<0.5"),
                               levels = c(">50","10-49","5-9",'0.5-4',"<0.5"))

#Join in the world data
joined <- data_agg %>%
  right_join(world, by = c("iso_code"="iso_a3")) %>%
  st_as_sf()


# Plotting ----------------------------------------------------------------

map_anim <- joined %>% 
  filter(year > 1970 & year < 2021) %>% 
  ggplot(aes(fill = inflation_level)) +
  geom_sf(fill = "grey50") +
  geom_sf(aes(fill = inflation_level)) +
  guides(fill = guide_legend(nrow = 1)) + 
  scale_fill_brewer(palette =  "YlOrRd", direction = -1, na.value = "grey") +
  transition_manual(year) +
  labs(title = "Headline CPI in: {current_frame}") +
  theme_void() +
  theme(legend.position = "top")



#animate(plot = map_anim, 
#          end_pause = 1)

anim_save(animation = map_anim, 
          filename = "Yearly_inflation_map.gif", 
          path = here("figures/"),
          end_pause = 12,
          nframes = 100, 
          fps = 5)
