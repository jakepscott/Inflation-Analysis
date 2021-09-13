library(here)
source(here("00_Load-Data.R"))

#Join in the world data
joined <- data_agg %>%
  right_join(world, by = c("iso_code"="iso_a3")) %>%
  st_as_sf()


# Plotting ----------------------------------------------------------------

map_anim <- joined %>% 
  filter(year > 1980 & year < 2021) %>% 
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



# Make a continuous Inflation map -----------------------------------------
# cutoff <- data_agg %>% 
#   mutate(inflation = ifelse(inflation>20, 20, inflation),
#          inflation = ifelse(inflation < 0, 0, inflation))
# 
# #Join in the world data
# joined_cutoff <- cutoff %>%
#   right_join(world, by = c("iso_code"="iso_a3")) %>%
#   st_as_sf()
# 
# 
# map_anim <- joined_cutoff %>% 
#   filter(year > 2000 & year < 2021) %>% 
#   ggplot() +
#   geom_sf(fill = "grey50") +
#   geom_sf(aes(fill = inflation)) +
#   guides(fill = guide_legend(nrow = 1)) + 
#   scale_fill_viridis(direction = 1, option = "inferno") +
#   transition_time(year) +
#   labs(title = "Headline CPI in: {closest_frame}") +
#   theme_void() +
#   theme(legend.position = "top")
# 
# 
# 
# #animate(plot = map_anim, 
# #          end_pause = 1)
# 
# anim_save(animation = map_anim, 
#           filename = "Yearly_inflation_map_cutoff.gif", 
#           path = here("figures/"),
#           end_pause = 12,
#           nframes = 100, 
#           fps = 5)
