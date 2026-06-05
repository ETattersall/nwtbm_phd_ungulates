#############################################
## 03_detection_summaries_single_project.R
## Downloading and summarizing camera detection data
## for a single NWTBMP project
## Started on June 2 2026
## Created by Erin Tattersall
#############################################


list.of.packages <- c("tidyverse", "sf", "maptiles", "ggspatial", "terra","kableExtra", "leaflet", "viridis", "corrplot", "lubridate", "plotly", "ggplot2", "ggbreak")
lapply(list.of.packages, require, character.only = TRUE)

## Needed to download station locations
## remotes::install_github("ABbiodiversity/wildrtrax") ## May need to create a new personal access token to retrieve from github
library(wildrtrax)

## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()




## Get project information for my WildTrax projects
cam_projects <- wt_get_projects("CAM")

## Check for project of interest (Edehzhie - project ID = 1465)

## Download single project data - Edehzhie tags
ede_camdata <- wt_download_report(project_id = 1465,
                               sensor_id = "CAM",
                               report = "main") # main reports include ALL DATA

glimpse(ede_camdata)
summary(ede_camdata)

## Do all stations have detections?
length(unique(ede_camdata$location)) #177 
table(is.na(ede_camdata$location))

### Load station lookup table to correct names from WildTrax
stn_lookup <- read.csv("data/nwtbm_station_name_lookup_table.csv")

glimpse(stn_lookup)

## Remove column X
stn_lookup <- stn_lookup %>% select(-X)


## Load location data to add station coordinates
cam_locs <- read.csv("data/wt_location_data/nwtbm_cam_locations_20260506.csv")
glimpse(cam_locs)

## Remove column X
cam_locs <- cam_locs %>% select(-X)

## Filter to Edehzhie cameras only
ede_locs <- cam_locs %>% filter(study_area == "Edéhzhíe")

## Correct tag data location names using stn_lookup
# Filter lookup to relevant project
ede_lookup <- stn_lookup %>% filter(study_area == "Edéhzhíe")


## Join the lookup to the tag data by location and location_wt, then convert location to location_std
ede_camdata <- ede_camdata %>%
  left_join(ede_lookup,
            by = c("location" = "location_wt")) %>% # indicating that the multiple rows in the lookup table will match multiple rows in ede_camdata
 mutate(location = location_std) %>% #converting wt station names to standardized names
  select(-location_std) # removing location_std column from lookup


### Create independent detections from camera data, with a standard threshold of 30 minutes
ede_det <- wt_ind_detect(ede_camdata,
                        threshold = 30,
                        units = "minutes")
glimpse(ede_det)


## Add location coordinates to detection data
ede_det <- ede_det %>% 
  left_join(cam_locs, by = "location")
glimpse(ede_det)

length(unique(ede_det$location)) #173 - should be 179. 6 stations didn't have any detections

#### 1. Plot total detections of all species detected ####
spp_count <- ede_det %>% 
  group_by(species_common_name) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% ## descending order of detections
  ungroup()

## Plot
plot_det <- ggplot(spp_count,
                   aes(x = count, y = fct_reorder(species_common_name, count))) + # re-orders species into descending count
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Total Species Detections, Edéhzhíe 2021-2022",
    x = "Independent Detections (30 min.)",
    y = NULL) + # removes y-axis title
  scale_x_continuous(breaks = c(0, 200, 400, 600, 2800)) + # define x-axis ticks
  scale_x_break(c(650, 2700)) + ## add x-axis break
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme( #remove top axis
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.title.x.top = element_blank())


win.graph()
plot_det

## Save plot
ggsave("figures/Edehzhie_allspecies_detections_2021-2022.png", plot_det, width = 18, height = 12, dpi = 300)



#### 2. Naive occupancy ####
## get list of all stations (since not all stations are included in ede_det - some stns contained no wildlife)
all_stns <- ede_locs %>% distinct(location)

## Create a site by species detection matrix for all sampled locations
stn_species_cams <- ede_camdata %>%
  distinct(study_area, location, species_common_name) %>% # get unique combinations of study area, location and species tags
  mutate(detection = 1L) %>% # assign a detection value of 1 for each location-species combination (L for integer)
  pivot_wider(names_from = species_common_name, values_from = detection, values_fill = 0L) %>% # pivot to wide format, filling missing combinations with 0 (non-detection) as an integer
  select( -NONE, -`STAFF/SETUP`, -Vehicle, -Unidentified, -Human) # remove non-wildlife columns
glimpse(stn_species_cams)

unique(stn_species_cams$location) 


## Convert to long format for plotting
spp_naive_long <- stn_species_cams %>% 
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
    title = "Naive Species Occupancy, Edéhzhíe 2021-2022",
    x = "Naive Occupancy",
    y = NULL) + # removes y-axis title
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12))

win.graph()
plot_naiocc

## Save plot
ggsave("figures/edehzhie_allspecies_naiveoccupancy_2021-2022.png", plot_naiocc, width = 18, height = 12, dpi = 300)


#### 3. Spatial patterns in detections ####
## Only for target species (individual plots)

### Create a stn by species count matrix, where values = total number of detections of each species at each station
## get list of all stations (since not all stations are included in ede_det - some stns contained no wildlife)
all_stns <- ede_locs %>% distinct(location)


stn_det <- ede_det %>%
  count(location, species_common_name) # count detections per location and species

## include missing stations in stn_species_count
stn_species_count <- all_stns %>%
  left_join(stn_det, by = "location") %>% 
pivot_wider(names_from = species_common_name, values_from = n, values_fill = 0L) %>%  # pivot to wide format, filling missing combinations with 0 (non-detection) as an integer
select(-`NA`) # remove NA column


### Leaflet plot
# ## Name target species - Woodland caribou, Moose, Bison, Sharp-tailed Grouse, Spruce Grouse, Ruffed Grouse, Willow Ptarmigan
# tar_spp <- "Woodland Caribou"
# 
# ## pull target counts from site_species_count
# focal_ct <- pull(stn_species_count, tar_spp)
# 
# ## Plot with leaflet
# m <- leaflet() %>% 
#   addProviderTiles(providers$Esri.WorldTopoMap, group="Base") %>%     
#   addCircleMarkers(lng=sk_locs$longitude, lat=sk_locs$latitude,
#                    # Add a popup of the deployment code 
#                    popup=paste(sk_locs$location),
#                    radius=(focal_ct/max(focal_ct)*10)+1, stroke=F,
#                    fillOpacity=0.6) 
# m


## Plot with ggplot and basemap

## Convert stn_species_count to sf object (first add coordinates)
stn_species_count <- stn_species_count %>% 
  left_join(ede_locs, by = "location")

sf_stn_spp <- st_as_sf(stn_species_count, coords = c("longitude", "latitude"), crs = 4326)
glimpse(sf_stn_spp)


## Read in polygons as sf object
list.files("data/study_area_spatial")
ede_sf <- st_read("data/study_area_spatial/Edehzhie.shp")
## Create 20km buffer around polygon to generate basemap
ede_buffer <- st_buffer(ede_sf, 20000)

# Create a basemap to extent of sk_wr_buffer
# Load basemap (e.g., "World_Imagery" or "OpenTopoMap")
basemap <- get_tiles(ede_buffer, provider = "Esri.WorldTopoMap", crop = TRUE, zoom = 8) 
# note: higher resolution base imagery takes longer to download and display (but higher resolution better for smaller areas)


## Caribou counts
caribou_ct_sf <- sf_stn_spp %>% select(`Woodland Caribou`)
colnames(caribou_ct_sf) <- c("caribou_count", "geometry")

caribou_det <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = ede_sf, linewidth = 1, color = "black", fill = NA) + # study area polygon
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

ggsave("figures/edehzhie2021-2022_spatial_caribou_detections.png", caribou_det, width = 12, height = 8, dpi = 300)

## moose counts
moose_ct_sf <- sf_stn_spp %>% select(Moose)
colnames(moose_ct_sf) <- c("moose_count", "geometry")

moose_det <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = ede_sf, linewidth = 1, color = "black", fill = NA) + # study area polygon
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

ggsave("figures/edehzhie2021-2022_spatial_moose_detections.png", moose_det, width = 12, height = 8, dpi = 300)

## Bison counts
bison_ct_sf <- sf_stn_spp %>% select(Bison)
colnames(bison_ct_sf) <- c("bison_count", "geometry")

bison_det <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = ede_sf, linewidth = 1, color = "black", fill = NA) + # study area polygon
  geom_sf(
    data = bison_ct_sf, ## add spatial detection data
    aes(size = bison_count), # vary point size by count of detections
    color = "red3",
    show.legend = TRUE) +
  labs(x = "Longitude",
       y = "Latitude",
       size = "Bison count") +
  theme_classic() +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )


bison_det

ggsave("figures/edehzhie2021-2022_spatial_bison_detections.png", moose_det, width = 12, height = 8, dpi = 300)



###### GROUSE PLOTS NOT CREATED YET FOR EDEHZHIE

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

ggsave("figures/sk_wr2022-2023_spatial_stgr_detections.png", stgr_det, width = 12, height = 8, dpi = 300)



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
