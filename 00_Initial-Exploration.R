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
for (i in 2:length(sheets)) {
  to_
}


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
  filter(country %in% c("Austria", "Argentina", "United States")) %>% 
  select(country, date, headline_consumer_price_index) %>% 
  group_by(country) %>% 
  mutate(inflation = (headline_consumer_price_index/lag(headline_consumer_price_index,n = 1))/lag(headline_consumer_price_index,n = 1),
         inflation = inflation*100) %>% 
  na.omit() %>% 
  ggplot(aes(date, inflation, color = country)) +
  geom_line()
  

data %>% 
  filter(country %in% c("United States")) %>% 
  select(country, date, headline_consumer_price_index) %>% 
  ggplot(aes(date, headline_consumer_price_index)) +
  geom_line()

# 
# data %>% 
#   filter(year(date) > 2000) %>% 
#   mutate(inflation_level = case_when(headline_consumer_price_index < 1 ~ "low",
#                                      headline_consumer_price_index >= 1 & headline_consumer_price_index < 5~ "moderate",
#                                      headline_consumer_price_index >= 1 & headline_consumer_price_index < 5~ "low",
#                                      headline_consumer_price_index >= 1 & headline_consumer_price_index < 5~ "low",
#                                      )) %>% 
# 
# quantile(data$headline_consumer_price_index, probs = seq(0,1,.01), na.rm = T)
