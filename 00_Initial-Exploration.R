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
imf_to_iso <- read_xlsx(here("data/co.xlsx"), skip = 1) %>% 
  select(`IMF Code`, `ISO Code`) %>% 
  clean_names()
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

pop <- world_bank_pop %>% 
  pivot_longer(!(country:indicator), names_to = "year", values_to = "pop")

full_data <- data %>% 
  mutate(imf_country_code = as.character(imf_country_code)) %>% 
  left_join(imf_to_iso, by = c("imf_country_code" = "imf_code")) %>% 
  left_join(pop %>% filter(year == 2017, indicator == "SP.URB.TOTL"), by = c("iso_code" = "country")) %>% 
  #filter(country %in% c("United States", "Austria", "Argentina")) %>% 
  group_by(country) %>% 
  mutate(inflation = (headline_consumer_price_index - lag(headline_consumer_price_index,12))/lag(headline_consumer_price_index,12)*100) %>% 
  na.omit() 

full_data %>% 
  ggplot(aes(date, inflation, color = country, alpha = "pop")) +
  geom_line() +
  coord_cartesian(ylim = c(-10,20)) +
  theme_bw() +
  theme(legend.position = "none")
  
ggsave(here("figures/error_art2.png"), dpi = 600)


 for_z <- full_data %>% 
   group_by(iso_code) %>% 
   summarise(mean = mean(inflation, na.rm = T),
             sd = sd(inflation, na.rm = T))

 full_data  %>% 
   select(iso_code, date, inflation, pop) %>% 
   left_join(for_z) %>% 
   mutate(z_score = (inflation - mean)/sd) %>% 
   ggplot(aes(date, z_score, color = country, alpha = pop)) +
   geom_line() +
   coord_cartesian(ylim = c(-5,20)) +
   theme_bw() +
   theme(legend.position = "none")
 ggsave(here("figures/error_art3.png"), dpi = 600)
 
 