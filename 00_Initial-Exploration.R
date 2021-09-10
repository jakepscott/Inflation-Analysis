# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(glue)
library(lubridate)
library(ggiraph)

# Load Data ---------------------------------------------------------------
sheets <- excel_sheets((here("data/Inflation-data.xlsx")))

data_raw <- read_xlsx(here("data/Inflation-data.xlsx"), sheet = 3)


data <- data_raw %>% 
  pivot_longer(!(`Country Code`:`Series Name`),
               names_to = "date", 
               values_to = data_raw$`Series Name`[1]) %>% 
  clean_names()

data <- data  %>% 
  mutate(year = substr(date,1,4),
         month = substr(date, 5,6),
         date = as.Date(glue("{year}-{month}-01")))

data %>% 
  #filter(country %in% c("United States", "Austria", "Argentina")) %>% 
  select(country, date, headline_consumer_price_index) %>% 
  group_by(country) %>% 
  mutate(inflation = (headline_consumer_price_index - lag(headline_consumer_price_index,12))/lag(headline_consumer_price_index,12)*100) %>% 
  na.omit() %>% 
  ggplot(aes(date, inflation, color = country)) +
  geom_line() +
  coord_cartesian(ylim = c(-10,20)) +
  theme(legend.position = "none")
  
ggsave(here("figures/error_art.png"), dpi = 600)

