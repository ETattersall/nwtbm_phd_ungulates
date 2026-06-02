#############################################
## 03_detection_summaries_single_project.R
## Downloading and summarizing camera detection data
## for a single NWTBMP project
## Started on June 2 2026
## Created by Erin Tattersall
#############################################


list.of.packages <- c( "sf", "maptiles", "ggspatial", "terra","kableExtra", "tidyr", "leaflet", "dplyr", "viridis", "corrplot", "lubridate", "plotly", "ggplot2")
lapply(list.of.packages, require, character.only = TRUE)

## Needed to download station locations
## remotes::install_github("ABbiodiversity/wildrtrax") ## May need to create a new personal access token to retrieve from github
library(wildrtrax)

## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()




## Get project information for my WildTrax projects
cam_projects <- wt_get_projects("CAM")

## Check for project of interest (Sambaa K'e - project ID = 1906)

## Download single project data - Sambaa K'e tags
sk_camdata <- wt_download_report(project_id = 1906,
                               sensor_id = "CAM",
                               report = "main") # main reports include ALL DATA

glimpse(sk_camdata)


### Load station lookup table to correct names from WildTrax
stn_lookup <- read.csv("data/nwtbm_station_name_lookup_table.csv")

glimpse(stn_lookup)

## Remove column X
stn_lookup <- stn_lookup %>% select(-X)

## Remove duplicate rows
stn_lookup <- distinct(stn_lookup)

## Load location data to add station coordinates
cam_locs <- read.csv("data/wt_location_data/nwtbm_cam_locations_20260506.csv")
glimpse(cam_locs)

## Remove column X
cam_locs <- cam_locs %>% select(-X)

## Correct tag data location names using stn_lookup
# Filter lookup to relevant project
sk_lookup <- stn_lookup %>% filter(study_area == "SambaaK'e")


## Join the lookup to the tag data by location and location_wt, then convert location to location_std
sk_camdata <- sk_camdata %>%
  left_join(sk_lookup,
            by = c("location" = "location_wt")) %>% # indicating that the multiple rows in the lookup table will match multiple rows in sk_camdata
  mutate(location = location_std) %>% #converting wt station names to standardized names
  select(-location_std) # removing location_std column from lookup


### Create independent detections from camera data, with a standard threshold of 30 minutes
sk_det <- wt_ind_detect(sk_camdata,
                        threshold = 30,
                        units = "minutes")
glimpse(sk_det)


## Add location coordinates to detection data
sk_det <- sk_det %>% 
  left_join(cam_locs, by = "location")
glimpse(sk_det)



#### 1. Plot total detections of all species detected ####
spp_count <- sk_det %>% 
  group_by(species_common_name) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% ## descending order of detections
  ungroup()

## Plot
plot_det <- ggplot(spp_count,
                   aes(x = count, y = fct_reorder(species_common_name, count))) + # re-orders species into descending count
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Total Species Detections, Sambaa K'e Winter Road 2022-2023",
    x = "Independent Detections (30 min.)",
    y = NULL) + # removes y-axis title
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16))

win.graph()
plot_det

## Save plot
ggsave("figures/sambaake_winterroad_allspecies_detections_2022-2023.png", plot_det, width = 12, height = 8, dpi = 300)



#### 2. Naive occupancy ####
## Create a site by species detection matrix for all sampled locations
site_species_cams <- sk_camdata %>%
  distinct(study_area, location, species_common_name) %>% # get unique combinations of study area, location and species tags
  mutate(detection = 1L) %>% # assign a detection value of 1 for each location-species combination (L for integer)
  pivot_wider(names_from = species_common_name, values_from = detection, values_fill = 0L) %>% # pivot to wide format, filling missing combinations with 0 (non-detection) as an integer
  select( -NONE, -`STAFF/SETUP`, -Vehicle, -Unidentified) # remove non-wildlife columns
# glimpse(site_species_cams)

## Convert to long format for plotting
spp_naive_long <- site_species_cams %>% 
  group_by(study_area, location) %>% 
  pivot_longer(cols = -c(study_area, location), ## all columns except these
               names_to = "species_common_name",
               values_to = "detection")

## For each species, calculate the proportion of locations with detections
spp_naive_summary <- spp_naive_long %>%
  group_by(study_area, species_common_name) %>%
  summarise(naive_occupancy = mean(detection), .groups = "drop") %>% # mean of detection column gives the proportion of locations with detections (naive occupancy)
  arrange(desc(naive_occupancy))

## Plot
plot_naiocc <- ggplot(spp_naive_summary,
                   aes(x = naive_occupancy, y = fct_reorder(species_common_name, naive_occupancy))) + # re-orders species into descending naive_occupancy
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Naive Species Occupancy, Sambaa K'e Winter Road 2022-2023",
    x = "Naive Occupancy",
    y = NULL) + # removes y-axis title
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16))

win.graph()
plot_naiocc

## Save plot
ggsave("figures/sambaake_winterroad_allspecies_naiveoccupancy_2022-2023.png", plot_naiocc, width = 12, height = 8, dpi = 300)


#### 3. Spatial patterns in detections ####
## Only for target species (individual plots)

### Create a site by species count matrix, where values = total number of detections of each species at each station
## get list of all stations (since not all stations are included in sk_det - some stns contained no wildlife)
all_stns <- sk_locs %>% distinct(location)


stn_det <- sk_det %>%
  count(location, species_common_name) # count detections per location and species

## include missing stations in stn_species_count
stn_species_count <- all_stns %>%
  left_join(stn_det, by = "location") %>% 
pivot_wider(names_from = species_common_name, values_from = n, values_fill = 0L) %>%  # pivot to wide format, filling missing combinations with 0 (non-detection) as an integer
select(-`NA`) # remove NA column

## Filter for Sambaa K'e locations in cam_locs
sk_locs <- cam_locs %>% filter(study_area == "SambaaK'e")
glimpse(sk_locs)

tar_spp <- "Woodland Caribou"

## pull target counts from site_species_count
focal_ct <- pull(stn_species_count, tar_spp)

## Plot with leaflet
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldTopoMap, group="Base") %>%     
  addCircleMarkers(lng=sk_locs$longitude, lat=sk_locs$latitude,
                   # Add a popup of the deployment code 
                   popup=paste(sk_locs$location),
                   radius=(focal_ct/max(focal_ct)*10)+1, stroke=F,
                   fillOpacity=0.6) 
m


## Plot with ggplot and basemap

## Convert stn_species_count to sf object (first add coordinates)
stn_species_count <- stn_species_count %>% 
  left_join(sk_locs, by = "location")

sf_stn_spp <- st_as_sf(stn_species_count, coords = c("longitude", "latitude"), crs = 4326)
glimpse(sf_stn_spp)


## Read in and winter road as sf object
sk_wr_sf <- st_read("data/study_area_spatial/sambaake_winter_road_shp.shp")
## Create 20km buffer around winter road to generate basemap
sk_wr_buffer <- st_buffer(sk_wr_sf, 20000)

# Create a basemap to extent of sk_wr_buffer
# Load basemap (e.g., "World_Imagery" or "OpenTopoMap")
basemap <- get_tiles(sk_wr_buffer, provider = "Esri.WorldTopoMap", crop = TRUE, zoom = 1) 
# note: higher resolution base imagery takes longer to download and display (but higher resolution better for smaller areas)


## Caribou counts
caribou_ct_sf <- sf_stn_spp %>% select(`Woodland Caribou`)
colnames(caribou_ct_sf) <- c("caribou_count", "geometry")

caribou_det <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = sk_wr_sf, linewidth = 1, color = "gray50") + # winter road
  geom_sf(
    data = caribou_ct_sf, ## add spatial detection data
    aes(size = caribou_count), # vary point size by count of detections
    color = "red3",
    show.legend = TRUE) +
  labs(x = "Longitude",
       y = "Latitude",
       size = "Caribou count") +
  theme_classic() +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

win.graph()
caribou_det

ggsave("figures/sk_wr2022-2023_spatial_caribou_detections.png", caribou_det, width = 12, height = 8, dpi = 300)

## moose counts
moose_ct_sf <- sf_stn_spp %>% select(Moose)
colnames(moose_ct_sf) <- c("moose_count", "geometry")

moose_det <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = sk_wr_sf, linewidth = 1, color = "gray50") + # winter road
  geom_sf(
    data = moose_ct_sf, ## add spatial detection data
    aes(size = moose_count), # vary point size by count of detections
    color = "red3",
    show.legend = TRUE) +
  labs(x = "Longitude",
       y = "Latitude",
       size = "Moose count") +
  theme_classic() +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

win.graph()
moose_det

ggsave("figures/sk_wr2022-2023_spatial_moose_detections.png", moose_det, width = 12, height = 8, dpi = 300)

## Sharp-tailed grouse counts (won't do SPGR or WIPT for SKFN - only 1 detection each)
stgr_ct_sf <- sf_stn_spp %>% select(`Sharp-tailed Grouse`)
colnames(stgr_ct_sf) <- c("stgr_count", "geometry")

stgr_det <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = sk_wr_sf, linewidth = 1, color = "gray50") + # winter road
  geom_sf(
    data = stgr_ct_sf, ## add spatial detection data
    aes(size = stgr_count), # vary point size by count of detections
    color = "red3",
    show.legend = TRUE) +
  labs(x = "Longitude",
       y = "Latitude",
       size = "Sharp-tailed Grouse count") +
  theme_classic() +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

win.graph()
stgr_det

ggsave("figures/sk_wr2022-2023_spatial_stgr_detections.png", moose_det, width = 12, height = 8, dpi = 300)



#### 4. Temporal patterning in detections #### 
## Not saved for Sambaa K'e - not enough data to be informative
glimpse(sk_det)

## Create 
sk_det_count

### Scatter plot by month-day (ignoring year) to see seasonal patterns across all years
## Create column for month-day
sk_det$det_month_day <- format(sk_det$start_time, "%m-%d") 

# Convert month-day column to factor with levels ordered by calendar days
sk_det$det_month_day <- factor(sk_det$det_month_day,
                               levels = format(seq(as.Date("2000-01-01"),
                                                   as.Date("2000-12-31"),
                                                   by = "day"), "%m-%d"))
class(sk_det$det_month_day) #factor


# Get 30 days for x-axis breaks
every_30_days <- levels(sk_det$det_month_day)[seq(1, length(levels(sk_det$det_month_day)), by = 30)]



## Scatter plot of Caribou detections by month-day
caribou_df <- sk_det %>% filter(species_common_name == "Woodland Caribou")

glimpse(caribou_df)

caribou_months <- ggplot(caribou_df, aes(x = det_month_day, y = after_stat(count), color = "red3")) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0), size = 2) +
  scale_x_discrete(breaks = every_30_days) +
  labs(
    title = "Phenology of Caribou Detections",
    x = "Month-Day",
    y = "Independent Detections (30 min.)") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

caribou_months
## Save plot
ggsave("figures/ungulate_detection_phenology_20260115.jpeg", plot = ung_tagged_months, width = 10, height = 6, units = "in", dpi = 300)
