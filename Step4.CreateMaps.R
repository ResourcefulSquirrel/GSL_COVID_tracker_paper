library(sf)
library(ggplot2)
library(urbnmapr)
library(tidyverse)
library(patchwork)

# set the theme so it is reasonable for maps
theme_set(theme_bw())

# Parent folder is "Tracker Analysis"

#LoadData
cumulWaveData <- read.csv("Data/WaveData/cumulative.waves.IRR.IRD.csv", header = TRUE)

# remove the row number column, as it isn't necessary
cumulWaveData <- cumulWaveData[,-1]
cumulWaveData <- cumulWaveData %>% rename(state_name = "state")

# find the min and max of the IRR and IRD for each wave so we can set the scale the same in all imgs
min(cumulWaveData$DeathIRR_W1, na.rm = TRUE)
max(cumulWaveData$DeathIRR_W1, na.rm = TRUE)
min(cumulWaveData$DeathIRR_W2, na.rm = TRUE)
max(cumulWaveData$DeathIRR_W2, na.rm = TRUE)
min(cumulWaveData$DeathIRR_W3, na.rm = TRUE)
max(cumulWaveData$DeathIRR_W3, na.rm = TRUE)
min(cumulWaveData$DeathIRR_CTot, na.rm = TRUE)
max(cumulWaveData$DeathIRR_CTot, na.rm = TRUE)

# get state shape info from urbnmapr
states_sf <- get_urbn_map(map = "states", sf = TRUE)

map_IRR_W1 <- states_sf %>% left_join(cumulWaveData, by = "state_name") %>% rename(MRR = DeathIRR_W1) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = MRR), color = "grey") + 
  scale_fill_gradient2(low = "dark blue", mid = "light yellow", high = "red", midpoint = 1, na.value = "white", limits = c(0, 2.5))+
  theme(axis.ticks =element_blank(), axis.text = element_blank()) +
  labs(subtitle = "Wave 1 (through May 25, 2020)")

map_IRR_W2 <- states_sf %>% left_join(cumulWaveData, by = "state_name") %>% rename(MRR = DeathIRR_W2) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = MRR), color = "grey") + 
  scale_fill_gradient2(low = "dark blue", mid = "light yellow", high = "red", midpoint = 1, na.value = "white", limits = c(0, 2.5))+
  theme(axis.ticks =element_blank(), axis.text = element_blank()) +
  labs(subtitle = "Wave 2 (May 26, 2020 - Aug 31, 2020)")

map_IRR_W3 <- states_sf %>% left_join(cumulWaveData, by = "state_name") %>% rename(MRR = DeathIRR_W3) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = MRR), color = "grey") + 
  scale_fill_gradient2(low = "dark blue", mid = "light yellow", high = "red", midpoint = 1, na.value = "white", limits = c(0, 2.5))+
  theme(axis.ticks =element_blank(), axis.text = element_blank()) + 
  labs(subtitle = "Wave 3 (Sep 1, 2020 - May 17, 2021)")

map_IRR_cumulative <- states_sf %>% left_join(cumulWaveData, by = "state_name") %>% rename(MRR = DeathIRR_CTot) %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = MRR), color = "grey") + 
  scale_fill_gradient2(low = "dark blue", mid = "light yellow", high = "red", midpoint = 1, na.value = "white", limits = c(0, 2.5))+
  theme(axis.ticks =element_blank(), axis.text = element_blank()) + 
  labs(subtitle = "Cumulative Totals")


IRR_combinedMaps <- map_IRR_W1 + 
                    map_IRR_W2 +
                    map_IRR_W3 + 
                    map_IRR_cumulative +  
                    plot_annotation(tag_levels = 'a') + plot_layout(guides = 'collect') + 
                    plot_annotation( title = "Mortality Rate Ratio")

ggsave(filename = "figures/maps/IRR_combinedPlots.png", plot = IRR_combinedMaps, width = 8, height = 5)
