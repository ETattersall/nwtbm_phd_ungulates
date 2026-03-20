#########################
## 05_megafires.R
## Exploring megafire years represented in study areas, and whether these might be used for grouping sites
## Started on March 20 2026
## Created by Erin Tattersall
#########################


#### Environment set up ####
## Load required packages (should already be installed)
list.of.packages <- c("sf",
                      "lwgeom",
                      "data.table",
                      "tidyverse",
                      "dplyr",
                      "ggspatial",
                      "cowplot",
                      "leaflet",
                      "terra", 
                      "maptiles", 
                      "ggplot2", 
                      "tidyterra", 
                      "ggspatial",
                      "viridis",
                      "corrplot",
                      "kableExtra",
                      "lubridate",
                      "purrr")

lapply(list.of.packages, require, character.only = TRUE)


## Load relevant spatial data - Fire data by study area, and camera locations
sa_20km_fires <- st_read("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/nrcan_nbac/NBAC_1972to2024_20250506_shp/NBAC_fires_by_study_area_20kmbuffer.shp")

## Camera locations (not buffered)
cam_locs_sf <- st_read("data/wt_location_data/all_projects_cam_locations_20260310.shp")


## Megafire years ##
# Megafires are defined as >10 000 ha in size. Giga fires are defined as >100 000 ha in size.
# Isolate Megafires or greater (using ADJ_HA column)
sa_megafires <- sa_20km_fires %>%
  filter(ADJ_HA > 10000) %>%
  mutate(size_group = case_when(
    ADJ_HA >= 100000 ~ "Gigafire",
    ADJ_HA >= 10000 & ADJ_HA < 100000 ~ "Megafire",
    TRUE ~ NA_character_  # Default case if no match
  ))

plot(sa_megafires["YEAR"])

hist(sa_megafires$YEAR)
table(sa_megafires$size_group) ## 25 gigafires, 109 megafires

## Add Fire Age to sa_megafires
glimpse(sa_megafires)

## Create Year0 for sa_megafires (corresponding to LAST year of deployment)
sa_megafires <- sa_megafires %>%
  mutate(Year0 = case_when(
    str_detect(study_area, "Edéhzhíe") ~ "2022",
    str_detect(study_area, "FortSmith") ~ "2024",
    str_detect(study_area, "Gameti") ~ "2024",
    str_detect(study_area, "NormanWells") ~ "2024",
    str_detect(study_area, "SambaaK'e") ~ "2023",
    str_detect(study_area, "ThaideneNëné") ~ "2022",
    TRUE ~ NA_character_  # Default case if no match
  ))
class(sa_megafires$Year0)
# convert to numeric
sa_megafires$Year0 <- as.numeric(sa_megafires$Year0)

## Calculate FireAge as Year0 - YEAR
sa_megafires$FireAge <- sa_megafires$Year0 - sa_megafires$YEAR

hist(sa_megafires$FireAge)

## how many (and which ones) are negative values?
neg.fire.age <- sa_megafires[sa_megafires$FireAge < 0, ]
neg.fire.age <- neg.fire.age %>% select(NFIREID, study_area, ADJ_HA, size_group, AG_SDATE, YEAR, Year0, FireAge, Shape_Leng, Shape_Area, geometry) 

## Can remove all the -3 years, and the Sambaa K'e -2. The Norman Wells -2 might overlap with a deployment period so keep it for now

## Create a bar plot of megafires and gigafires by year
gg_sa_megafires <- ggplot(sa_megafires, aes(x = YEAR, fill = size_group)) +
  geom_bar() +
  facet_wrap(~ study_area) +
  labs(title = "Number of Megafires and Gigafires by Year",
       x = "Year",
       y = "Count of Fires",
       fill = "Fire Size") +
  theme_classic() +
  scale_fill_manual(values = c("Megafire" = "orange", "Gigafire" = "darkred")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

gg_sa_megafires
## Save the plot
ggsave("figures/fire_explore/Megafires_hist_bySA.png", gg_sa_megafires, width = 12, height = 8, dpi = 300)


## Which years have giga fires (across study areas)?
#Isolate gigafires
sa_gigafires <- sa_megafires %>% filter(size_group == "Gigafire")

# Plot number of gigafires by year
gg_gigafires <- ggplot(sa_gigafires, aes(x = YEAR, fill = size_group)) +
  geom_bar() +
  labs(title = "Number of Gigafires by Year",
       x = "Year",
       y = "Count of Fires",
       fill = "Fire Size") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
gg_gigafires


class(sa_gigafires)


### MAPPING
# ### Map of all NWT fire history plus sensor locations (including 500m buffers for visibility)
# gg_nwt_fires <- ggplot() +
#   geom_sf(data = nwt_fires, aes(color = YEAR), size = 0.5) + # all NWT fire polygons
#   geom_sf(data = cams_500m_buffer, fill = NA, color = "blue", size = 0.5) + # 500m buffers around camera locations
#   geom_sf(data = cam_locs_sf, color = "black", size = 1) + # camera locations
#   scale_color_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
#   labs(title = "NWT Fire History (1972 - 2024) with Camera Locations",
#        x = "Longitude",
#        y = "Latitude",
#        color = "Fire Year") +
#   theme(legend.position = "right") +
#   coord_sf(xlim = c(-1026000, 580000), ylim = c(8100000, 9360000), expand = FALSE) + # set limits to bounding box of NWT fire layer (plus a little extra buffer)
#   theme_classic() + 
#   # increase size of title text, axis text, and facet titles
#   theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
#   theme(axis.title.x = element_text(size = 16)) +
#   theme(axis.title.y = element_text(size = 16)) +
#   theme(legend.title= element_text(size = 16))