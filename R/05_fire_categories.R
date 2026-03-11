###########################
## 05_fire_categories.R
## Dividing fire age and size variables into categoricals
## Started March 11, 2026
## Created by Erin Tattersall
###########################

#### Environment set up ####
## Load required packages (should already be installed)

list.of.packages <- c("sf",
                      "lwgeom",
                      "data.table",
                      "tidyverse",
                      "dplyr",
                      "osmdata", 
                      "stars",
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

## Load packages (should all be installed)
lapply(list.of.packages, require, character.only = TRUE)

## Read in in cam_locs_sf, cams_100m_buffer, cams_500m_buffer
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/wt_location_data")
cam_locs_sf <- st_read("all_projects_cam_locations_20260310.shp")
cams_100m_buffer <- st_read("cams_100m_buffer.shp")
cams_500m_buffer <- st_read("cams_500m_buffer.shp")

## Read in sa_20k_buffer from R/03_location_data.R (saved in data/study_area_spatial)
sa_20k_buffer <- st_read("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/study_area_spatial/NWTBM_all_study_areas_20km_buffers.shp")

### Load NWT fire data (generated from NBAC data in R/04_nbac_fire_data_explore.R)
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/nrcan_nbac/NBAC_1972to2024_20250506_shp")
nwt_fires <- st_read("nwt_fires_1972to2024.shp")

## Reset working directory to project folder
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates")

## Extract nwt_fires intersecting with sa_20k_buffer
sa_fires <- st_intersection(nwt_fires, sa_20k_buffer)

plot(sa_fires["YEAR"])

## Megafire years ##
# Megafires are defined as >10 000 ha in size. Giga fires are defined as >100 000 ha in size.
# Isolate Megafires or greater (using ADJ_HA column)
sa_megafires <- sa_fires %>%
  filter(ADJ_HA > 10000) %>%
  mutate(size_group = case_when(
    ADJ_HA >= 100000 ~ "Gigafire",
    ADJ_HA >= 10000 & ADJ_HA < 100000 ~ "Megafire",
    TRUE ~ NA_character_  # Default case if no match
  ))

plot(sa_megafires["YEAR"])

hist(sa_megafires$YEAR)
table(sa_megafires$size_group)

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



#### Calculating Fire Ages across entire study areas ####
## Create Year0 for sa_fires, corresponding to the first year of deployment for each study area
sa_fires <- sa_fires %>%
  mutate(Year0 = case_when(
    str_detect(study_area, "Edéhzhíe") ~ "2021",
    str_detect(study_area, "FortSmith") ~ "2023",
    str_detect(study_area, "Gameti") ~ "2023",
    str_detect(study_area, "NormanWells") ~ "2022",
    str_detect(study_area, "SambaaK'e") ~ "2022",
    str_detect(study_area, "ThaideneNëné") ~ "2021",
    TRUE ~ NA_character_  # Default case if no match
  ))

glimpse(sa_fires)
## Convert Year0 to numeric
sa_fires$Year0 <- as.numeric(sa_fires$Year0)

## Calculate FireAge as the difference between YEAR and Year0
sa_fires$fire_age <- sa_fires$Year0 - sa_fires$YEAR
summary(sa_fires)

## how many (and which ones) are negative values?
neg.fire.age <- sa_fires[sa_fires$fire_age < 0, ] ## 57 fires with negative values
neg.fire.age <- neg.fire.age %>% select(study_area, Year0, YEAR, AG_SDATE, fire_age) ## isolate relevant columns to determine which fires have negative fire age values
table(neg.fire.age$study_area)
summary(neg.fire.age)

## Need to check specific deployment dates to determine which ones can be removed
