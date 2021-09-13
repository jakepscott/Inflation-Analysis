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
  select(country, country_code, imf_country_code, date, headline_consumer_price_index)

#Set up the date column
data <- data  %>% 
  mutate(year = as.numeric(substr(date,1,4)),
         month = as.numeric(substr(date, 5,6)),
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
                                     inflation >= 10 & inflation < 50 ~ "10-50",
                                     inflation >= 3 & inflation < 10 ~ "3-10",
                                     inflation >= 1 & inflation < 3 ~ "1-3",
                                     inflation < 1 ~ "<1",
                                     T ~ NA_character_))

# Make the inflation categories into an order factor
data_agg$inflation_level <- factor(data_agg$inflation_level, ordered = T,
                                   labels = c(">50","10-50","3-10",'1-3',"<1"),
                                   levels = c(">50","10-50","3-10",'1-3',"<1"))