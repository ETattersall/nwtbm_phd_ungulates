####################################
## 04_fire_variable_explore.R
## Initial exploration of NBAC fire data for all camera locations
## Started on Feb 13 2026
## Created by Erin Tattersall
####################################

#### Environment set up ####
## Load required packages (should already be installed)

list.of.packages <- c("wildrtrax",
                      "sf",
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



# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Read in in cam_locs_sf, cams_100m_buffer, and cams_500m_buffer from R/03_location_data.R
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/wt_location_data")
cam_locs_sf <- st_read("all_projects_cam_locations_20260310.shp")
cams_100m_buffer <- st_read("cams_100m_buffer.shp")
cams_500m_buffer <- st_read("cams_500m_buffer.shp")


#### Load Fire History data ####
## Canada Fire History data between 1972-2024 from NRCan: https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/nbac
## Also have NWT fire history data from GNWT website, but I think that only goes to 2023 (may be a more recent download?)
## Use NRCan data, since this is what Claudia also used
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/nrcan_nbac/NBAC_1972to2024_20250506_shp")
fire_history <- st_read("NBAC_1972to2024_20250506.shp")
head(fire_history)
## Check CRS
st_crs(fire_history) # NAD 83 - Canada Lambert Conformal Conic

summary(fire_history$YEAR) ## all years (1972-2024)

## Transform to NWT Lambert and crop fire history data to NWT boundary 
fire_history <- st_transform(fire_history, crs = 3580)

## First filter fire_history for fires in NT (though this still includes fires in Nunavut prior to it becoming a separate territory in 1999)
nwt_fires <- fire_history %>%
  filter(ADMIN_AREA == "NT") # filter for fires in NT

## Remove fire_history from environment to save memory
rm(fire_history)

## return to base directory
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates")


## Then extract fire for cam buffer areas
fires_100m_buffer <- st_intersection(nwt_fires, cams_100m_buffer)
fires_500m_buffer <- st_intersection(nwt_fires, cams_500m_buffer)


crs(fires_100m_buffer) # check CRS of fire data within buffers


## Quick summary of fire years represented
summary(fires_100m_buffer$YEAR) ## 1972-2024, median 1994
summary(fires_500m_buffer$YEAR) ## 1972-2024, median 1995

hist(fires_100m_buffer$YEAR) # roughly normal distribution, with big spikes in late 1970s, early 1990s
hist(fires_500m_buffer$YEAR) # similar distribution to 100m but with more fires overall (since larger buffer)
hist(fires_100m_buffer$POLY_HA) # total area of fire polygons in hectares using Canada Albers Equal Area projection (pre-calculated by NRCan)
hist(fires_500m_buffer$POLY_HA)
hist(nwt_fires_boun$ADJ_HA) # adjusted area burned (see documentation for details, and Skakun et al. 2021: https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/NBAC_1972to2024_20250506_shp_metadata.pdf)

plot(fires_100m_buffer["YEAR"]) # map of fire years within 100m buffer around camera locations
plot(fires_500m_buffer["YEAR"]) # map of fire years within 500m buffer around camera locations

## Not all sensor locations have fire history data within 100m or 500m buffers. Some locations have multiple fire polygons within the buffers
length(unique(fires_100m_buffer$location)) # 336 locations have fire history data within 100m buffer
length(unique(fires_500m_buffer$location)) # 360 locations have fire history data within 500m buffer

#### Summary statistics ####

## What is the proportion of burned/unburned area around camera locations? Stick to 500m buffer for simplicity
# For 500m around each camera location, calculate total area of fire polygons (in hectares) and divide by total area of 500m buffer (in m^2) to get proportion of burned area within 500m buffer
# total area of 500m buffer = pi *(500^2) m^2 = 785398.16 m^2


## For each unique location in fires_500m_buffer, calculate total area of all fire polygons within 500m buffers
## Result should be a spatial data frame with one row per unique location, and columns for total burned area within 500m buffers

#### Burned area within 100m buffer (not using right now) ####
burned_area_100m <- fires_100m_buffer %>%
  group_by(study_area, location) %>% ## group all polygons with same location and study area together
  summarise(geometry = st_union(geometry)) %>% # combine all fire polygons from the same location
  mutate(burned_area_m2 = st_area(geometry)) %>% # calculate area of combined fire polygons in m^2
  mutate(proportion_burned_100m = as.numeric(burned_area_m2/(pi * (100^2)))) %>%  # calculate proportion of burned area within 100m buffer (convert to numeric to avoid units issues)
 st_drop_geometry() # drop geometry for easier joining with cam_locs_sf


#### Burned area within 500m buffer ####
burned_area_500m <- fires_500m_buffer %>% 
  group_by(study_area, location) %>% ## group all polygons with same location and study area together
  summarise(geometry = st_union(geometry)) %>% # combine all fire polygons from the same location
  mutate(burned_area_m2 = st_area(geometry)) %>% # calculate area of combined fire polygons in m^2
  mutate(proportion_burned_500m = as.numeric(burned_area_m2/(pi * (500^2)))) %>%  # calculate proportion of burned area within 500m buffer (convert to numeric to avoid units issues
 st_drop_geometry() # drop geometry for easier joining with cam_locs_sf
  
str(burned_area_500m$location) 
str(cam_locs_sf$location)

## add proportion_burned_m2 to cam_locs_sf for 100m and 500m buffers, adding a 0 value for locations with no fire history data within the buffer
cam_locs_burnedprop <- cam_locs_sf %>%
  left_join(
    burned_area_100m %>% 
      select(location, proportion_burned_100m),
    by = c("study_area", "location") ## join 100m burn by location to match fire data to camera locations
    ) %>%
  left_join(burned_area_500m %>% 
  select(location, proportion_burned_500m),
  by = c("study_area", "location") ## join 500m burn by location to match fire data to camera locations
    ) %>%
  mutate(
    proportion_burned_100m = ifelse(is.na(proportion_burned_100m), 0, proportion_burned_100m),
    proportion_burned_500m = ifelse(is.na(proportion_burned_500m), 0, proportion_burned_500m)) ## replace NA values with 0 for locations with no fire history data within the buffer

glimpse(cam_locs_burnedprop) ## check that the new columns have been added correctly
summary(cam_locs_burnedprop) ## most locations have 0 burned area within 100m buffer, but some have up to 100% burned area
class(cam_locs_burnedprop) # sf object with 706 rows and 9 columns (including geometry)


## Histogram of proportion of burned area within 100m and 500m buffers - proportion burned on the x-axis (binned to 0.1 intervals), frequency on the y-axis
hist_burned_100m <- cam_locs_burnedprop %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = proportion_burned_100m)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") + ## Binned to 0.1 intervals
  labs(title = "Distribution of Proportion of Burned Area within 100m Buffer of Camera Locations",
       x = "Proportion of Burned Area",
       y = "Count of Locations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_burned_100m
## save
ggsave("figures/fire_explore/propburned_100mbuffer_20260305.png", hist_burned_100m, width = 12, height = 8, dpi = 300)

hist_burned_500m <- cam_locs_burnedprop %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = proportion_burned_500m)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") + ## Binned to 0.1 intervals
  labs(title = "Distribution of Proportion of Burned Area within 500m Buffer of Camera Locations",
       x = "Proportion of Burned Area",
       y = "Count of Locations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_burned_500m
## save
ggsave("figures/fire_explore/propburned_500mbuffer_20260305.png", hist_burned_500m, width = 12, height = 8, dpi = 300)

## facet by study area
#100m buffer
hist_burned_sa_100m <- cam_locs_burnedprop %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = proportion_burned_100m)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") + ## Binned to 0.1 intervals
  facet_wrap(~ study_area) +
  labs(title = "Distribution of Proportion of Burned Area within 100m Buffer of Camera Locations",
       x = "Proportion of Burned Area",
       y = "Count of Locations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_burned_sa_100m
## save
ggsave("figures/fire_explore/propburned_100mbuffer_studyarea_20260305.png", hist_burned_sa_100m, width = 12, height = 8, dpi = 300)

#500m buffer
hist_burned_sa_500m <- cam_locs_burnedprop %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = proportion_burned_500m)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") + ## Binned to 0.1 intervals
  facet_wrap(~ study_area) +
  labs(title = "Distribution of Proportion of Burned Area within 500m Buffer of Camera Locations",
       x = "Proportion of Burned Area",
       y = "Count of Locations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_burned_sa_500m
## save
ggsave("figures/fire_explore/propburned_500mbuffer_studyarea_20260305.png", hist_burned_sa_500m, width = 12, height = 8, dpi = 300)

## Proportion of burned/unburned areas around cameras has a biphasic distribution with peaks at 0 (unburned) and 1 (fully burned)
## Increasing the scale of measurement from 100m to 500m increases the proportion burned somewhat, but distrib is still uneven
## Removing some of the older fires (e.g., considering them unburned after a certain number of years) would decrease proportion of areas considered burned, which might help even out the distribution
## Could assume that after a certain number of years post-fire, the area would be regenerated to either deciduous shrub/mixedwood or deciduous forest (i.e., remove fire polygons prior to that time)



#### Histogram of fire years within each study area (100m buffer) ####
glimpse(fires_100m_buffer)
## 100m buffer histogram (binned to 10 years to reduce gaps in data, since many years don't have fire representation)
hist_fires_100 <- fires_100m_buffer %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = YEAR)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  facet_wrap(~ study_area) +
  labs(title = "Distribution of Fire Years within 100m Buffer of Camera Locations",
       x = "Fire Year",
       y = "Count of Stations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
hist_fires_100
## save
ggsave("figures/fire_explore/hist_fires_100mbuffer_20260224.png", hist_fires_100, width = 12, height = 8, dpi = 300)

## 500m buffer histogram (binned to 10 years to reduce gaps in data, since many years don't have fire representation)
hist_fires_500 <- fires_500m_buffer %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = YEAR)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  facet_wrap(~ study_area) +
  labs(title = "Distribution of Fire Years within 500m Buffer of Camera Locations",
       x = "Fire Year",
       y = "Count of Stations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
hist_fires_500
## save
ggsave("figures/fire_explore/hist_fires_500mbuffer_20260224.png", hist_fires_500, width = 12, height = 8, dpi = 300)

## 100 and 500m buffers have similar representation of fire years

## NO FACETING - Fire History across all study areas (100m buffer)
nwt_fire_hist_100 <- fires_100m_buffer %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = YEAR)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  labs(title = "Distribution of Fire Years within 100m Buffer of Camera Locations - all study areas",
       x = "Fire Year",
       y = "Count of Stations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
nwt_fire_hist_100
## save
ggsave("figures/fire_explore/allSAs_hist_fires_100mbuffer_20260303.png", nwt_fire_hist_100, width = 12, height = 8, dpi = 300)

## 500m buffer
nwt_fire_hist_500 <- fires_500m_buffer %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = YEAR)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  labs(title = "Distribution of Fire Years within 500m Buffer of Camera Locations - all study areas",
       x = "Fire Year",
       y = "Count of Count of Stations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
nwt_fire_hist_500
## save
ggsave("figures/fire_explore/allSAs_hist_fires_500mbuffer_20260303.png", nwt_fire_hist_500, width = 12, height = 8, dpi = 300)

## Convert fire years to burn age (or time since fire for each year of camera data)
## Need a year from which to calculate time since fire - use the first year of data collection for each study area
## 2021 for Edehzhie and TDN, 2022 for Norman Wells and Sambaa K'e, 2023 for Fort Smith and Gameti
## Need to make sure fire date isn't after deployment date (but cam_locs doesn't have deployment date. Will need to check stations with 0 - or negative! - values later)

glimpse(fires_500m_buffer)

## Create Year0 for fires_500m_buffer
fires_500m_buffer <- fires_500m_buffer %>%
  mutate(Year0 = case_when(
    str_detect(study_area, "Edéhzhíe") ~ "2021",
    str_detect(study_area, "FortSmith") ~ "2023",
    str_detect(study_area, "Gameti") ~ "2023",
    str_detect(study_area, "NormanWells") ~ "2022",
    str_detect(study_area, "SambaaK'e") ~ "2022",
    str_detect(study_area, "ThaideneNëné") ~ "2021",
    TRUE ~ NA_character_  # Default case if no match
  ))

fires_500m_buffer$Year0 <- as.numeric(fires_500m_buffer$Year0)
glimpse(fires_500m_buffer)

fires_500m_buffer$FireAge <- fires_500m_buffer$Year0 - fires_500m_buffer$YEAR
summary(fires_500m_buffer$FireAge)
hist(fires_500m_buffer$FireAge)

## how many (and which ones) are negative values?
neg.fire.age <- fires_500m_buffer[fires_500m_buffer$FireAge < 0, ]
table(neg.fire.age$study_area) # 10 Edehzhie, 6 Sambaa K'e, 6 ThaideneNene
## Can remove the Edehzhie and TDN ones (fire doesn't occur during deployment period)
## Sambaa K'e fires occurred after retrieval (March 2023)

fires_500m_buffer <- fires_500m_buffer[!fires_500m_buffer$FireAge < 0, ]
summary(fires_500m_buffer$FireAge)

## Faceted plot of Fire Age for 500m buffer
fireage_500 <- fires_500m_buffer %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  facet_wrap(~ study_area) +
  labs(title = "Distribution of Fire Age within 500m Buffer of Camera Locations",
       x = "Fire Age",
       y = "Count of Stations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fireage_500
## save
ggsave("figures/fire_explore/fireages_byarea_500mbuffer_20260306.png", fireage_500, width = 12, height = 8, dpi = 300)

## Not faceted
fireage_all_500 <- fires_500m_buffer %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  labs(title = "Distribution of Fire Age within 500m Buffer of Camera Locations",
       x = "Fire Age",
       y = "Count of Stations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fireage_all_500
## save
ggsave("figures/fire_explore/fireages_500mbuffer_20260306.png", fireage_all_500, width = 12, height = 8, dpi = 300)

## How many sites have more than 1 fire polygon within 500m?
table(duplicated(fires_500m_buffer$location)) #78 sites have multiple fire polygons
## How many polygons are duplicated across sites (i.e. polygons that include multiple sites)
table(duplicated(fires_500m_buffer$NFIREID)) #379 polygon IDs match another (i.e., 379 sites are located within the same fire polygon as another site)

length(unique(fires_500m_buffer$NFIREID)) # 48 unique fire polygons represented

#### Fire Size ####
## Can use ADJ_HA in fires_500m_buffer, but there will be duplicates because some fires cover multiple sites. But that's okay for this exploration
hist(fires_500m_buffer$ADJ_HA) ## fire sizes follow a general poisson distribution, with many small fires and few large fires (outliers up to 800 000 ha) 

## ggplot
hist_fire_size <- fires_500m_buffer %>% 
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = ADJ_HA)) +
  geom_histogram(binwidth = 10000, fill = "orange", color = "black") + ## Binned to 10,000 ha intervals
  labs(title = "Distribution of Fire Sizes (for fires within 500m of sites)",
       x = "Fire Size (HA)",
       y = "Count of Stations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_fire_size

## save
ggsave("figures/fire_explore/hist_fire_size_500mbuffer_20260310.png", hist_fire_size, width = 12, height = 8, dpi = 300)

## Fire size by study area
hist_fire_size_sa <- fires_500m_buffer %>% 
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = ADJ_HA)) +
  geom_histogram(binwidth = 10000, fill = "orange", color = "black") + ## Binned to 10,000 ha intervals
  facet_wrap(~ study_area) +
  labs(title = "Distribution of Fire Sizes by Study Area (for fires within 500m of sites)",
       x = "Fire Size (HA)",
       y = "Count of Stations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_fire_size_sa

## save
ggsave("figures/fire_explore/hist_fire_size_500mbuffer_studyarea_20260310.png", hist_fire_size_sa, width = 12, height = 8, dpi = 300)

#### Is there a relationship between fire age and fire size?

fire_age_size <- fires_500m_buffer %>% 
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge, y = ADJ_HA)) +
  geom_point() +
  labs(title = "Relationship between Fire Age and Fire Size (for fires within 500m of sites)",
       x = "Fire Age",
       y = "Fire Size (HA)") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fire_age_size

## add regression line with 95% confidence intervals to fire_age_size
fire_age_size <- fire_age_size + geom_smooth(method = "lm", se = TRUE, color = "purple")
fire_age_size

## save
ggsave("figures/fire_explore/fire_age_size_relationship_500mbuffer_20260310.png", fire_age_size, width = 12, height = 8, dpi = 300)

## Assessing relationship between fire age and fire size with a linear model
glimpse(fires_500m_buffer)
fires_500m_buffer$location <- as.factor(fires_500m_buffer$location) ## convert location to factor for use as random effect in GLMs
lm(ADJ_HA ~ FireAge, data = fires_500m_buffer) %>% summary() ## Low R-squared (low correlation) but significant relationship
glm(ADJ_HA ~ FireAge, data = fires_500m_buffer, family = "poisson") %>% summary() ## Similar results with a GLM using a gamma distribution to account for skewed fire size data)

## would ideally use a random effect for location, but it doesn't matter so much for the exploration. 

### Mapping ####
### Initial map of fires buffered to 100m around camera locations (no boundaries or basemaps)
win.graph() # open separate graphics window
gg_fire_100 <- ggplot() +
  geom_sf(data = fires_100m_buffer, aes(color = YEAR), size = 1.5) + # fire polygons
  scale_color_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  labs(title = "Fire History (100m buffer), 1972 - 2024",
       x = "Longitude",
       y = "Latitude",
       color = "Fire Year") +
  theme(legend.position = "right") +
  coord_sf(xlim = c(-1026000, 580000), ylim = c(8100000, 9360000), expand = FALSE) + # set limits to bounding box of NWT fire layer (plus a little extra buffer)
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(legend.title= element_text(size = 16))

gg_fire_100

### Map of all NWT fire history plus sensor locations (including 500m buffers for visibility)
gg_nwt_fires <- ggplot() +
  geom_sf(data = nwt_fires, aes(color = YEAR), size = 0.5) + # all NWT fire polygons
  geom_sf(data = cams_500m_buffer, fill = NA, color = "blue", size = 0.5) + # 500m buffers around camera locations
  geom_sf(data = cam_locs_sf, color = "black", size = 1) + # camera locations
  scale_color_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  labs(title = "NWT Fire History (1972 - 2024) with Camera Locations",
       x = "Longitude",
       y = "Latitude",
       color = "Fire Year") +
  theme(legend.position = "right") +
  coord_sf(xlim = c(-1026000, 580000), ylim = c(8100000, 9360000), expand = FALSE) + # set limits to bounding box of NWT fire layer (plus a little extra buffer)
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(legend.title= element_text(size = 16))

gg_nwt_fires
