#############################################
## 03_detection_summaries_single_project.R
## Downloading and summarizing camera detection data
## for a single NWTBMP project
## Started on June 2 2026
## Created by Erin Tattersall
#############################################


list.of.packages <- c("tidyverse", "sf", "maptiles", "ggspatial", "terra","kableExtra", "leaflet", "viridis", "corrplot", "lubridate", "plotly", "ggplot2", "ggbreak")
lapply(list.of.packages, require, character.only = TRUE)

### Download raw image data, camera locations, independent detection data, ungulate monthly detections, and gamebird monthly detections
std_data <- read.csv("data/camera_data/nwtbm_allprojects_camera_tags.csv")
cam_locs <- read.csv("data/wt_location_data/nwtbm_cam_locations_20260506.csv")
cam_det <- read.csv("data/camera_data/nwtbm_allprojects_camera_detections_30min.csv")
ung_mon <- read.csv("data/camera_data/nwtbm_ungulate_detections_by_month.csv")
gb_mon <- read.csv("data/camera_data/nwtbm_gamebird_detections_by_month.csv")



## Filter these for single project
tdn_camdata <- std_data |> filter(study_area == "ThaideneNëné")
tdn_det <- cam_det |> filter(study_area == "ThaideneNëné")
tdn_ung_mon <- ung_mon |> filter(study_area == "ThaideneNëné") |> 
                          select(-X, -Bison, -Woodland.Caribou) ## remove species not found in TDN
tdn_gb_mon <- gb_mon |> filter(study_area == "ThaideneNëné") |> 
                        select(-X, -Ruffed.Grouse) ## remove species not found in TDN
summary(tdn_ung_mon) 

summary(tdn_gb_mon)


## Summarize all Ptarmigans into single category (overwrite current Ptarmigan category)
tdn_gb_mon$Ptarmigans <- tdn_gb_mon$Ptarmigans + tdn_gb_mon$Rock.Ptarmigan + tdn_gb_mon$Willow.Ptarmigan



## Add location coordinates to detection data
tdn_det <- tdn_det %>% 
  left_join(cam_locs, by = "location")
glimpse(tdn_det)

length(unique(tdn_det$location)) #305 - should be 307. 2 stations didn't have any detections

## Save standardized tags and independent detections
write.csv(tdn_camdata, "data/camera_data/tdn2021-2022_camera_tags.csv")
write.csv(tdn_det, "data/camera_data/tdn2021-2022_camera_detections_30min.csv")


##### Single project figures (note - single project independent detections and naive occupancy plots for all species already generated in script 02)

#### 1. Plot monthly detections of ungulates and game birds ####

glimpse(tdn_ung_mon)
glimpse(tdn_gb_mon)

## Pivot to long format
l_ung_tdn <- tdn_ung_mon |> 
  pivot_longer(
    cols = c("Moose", "Muskox"),
    names_to = "species",
    values_to = "monthly_det"
  ) |> 
  group_by(study_area, species) |> 
  summarise(
    total_detections = sum(monthly_det, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(l_ung_tdn)

l_gb_tdn <- tdn_gb_mon |> 
  pivot_longer(
    cols = c("Ptarmigans", "Sharp.tailed.Grouse", "Spruce.Grouse"),
    names_to = "species",
    values_to = "monthly_det"
  ) |> 
  group_by(study_area, species) |> 
  summarise(
    total_detections = sum(monthly_det, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(l_gb_tdn)


### Ungulate detections
det_ung <- ggplot(
  l_ung_tdn,
  aes(
    x = total_detections, # sum of all monthly detections
    y = species
  )) +
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    title = "Ungulate Detections, Thaidene Nëné 2021-2022",
    x = "Total Monthly Detections",
    y = NULL,
    fill = NULL
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 32, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.text = element_text(size = 16)) +
  theme(strip.text = element_text(size = 20))

det_ung

## Save plot
ggsave("figures/TDN/tdn_ungulate_seasonal_detections.png", det_ung, width = 18, height = 12, dpi = 300)

### Seasonal Plot for game birds
det_gb <- ggplot(
  l_gb_tdn,
  aes(
    x = total_detections, # sum of all monthly detections in a season
    y = species
  )) +
  geom_col(position = position_dodge(width = 0.8),     fill = "darkgreen") +
  labs(
    title = "Game Bird Detections, Thaidene Nëné 2021-2022",
    x = "Total Game Bird Detections",
    y = NULL
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 32, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.text = element_text(size = 16)) +
  theme(strip.text = element_text(size = 20))

det_gb

## Save plot
ggsave("figures/TDN/tdn_gamebird_seasonal_detections.png", det_gb, width = 18, height = 12, dpi = 300)


#### 2. Naive occupancy ####
## are all stations included in monthly detection dfs?
length(unique(tdn_ung_mon$location)) ## yes
length(unique(tdn_gb_mon$location)) ## yes

glimpse(tdn_ung_mon)


## Create a site by species detection matrix (by season!) for all sampled locations
# specify species columns
ung_cols <- c("Moose", "Muskox")
gb_cols <- c("Ptarmigans", "Sharp.tailed.Grouse", "Spruce.Grouse", "Rock.Ptarmigan", "Willow.Ptarmigan")


location_ung <- tdn_ung_mon  |> 
  group_by(location) |> 
  summarise(
    across(
      all_of(ung_cols),
      ~ as.integer(any(. > 0, na.rm = TRUE))
    ),
    .groups = "drop"
  )

glimpse(location_ung)

location_gb <- tdn_gb_mon  |> 
  group_by(location) |> 
  summarise(
    across(
      all_of(gb_cols),
      ~ as.integer(any(. > 0, na.rm = TRUE))
    ),
    .groups = "drop"
  )

glimpse(location_gb)



## Convert to long format for plotting
ung_naive_long <- location_ung %>% 
  group_by(location) %>% 
  pivot_longer(cols = -c(location), ## all columns except location
               names_to = "species_common_name",
               values_to = "detection")

gb_naive_long <- location_gb %>% 
  group_by(location) %>% 
  pivot_longer(cols = -c(location), ## all columns except location
               names_to = "species_common_name",
               values_to = "detection")

glimpse(ung_naive_long)

## For each species, calculate the proportion of locations with detections
ung_naive_summary <- ung_naive_long %>%
  group_by(species_common_name) %>%
  summarise(naive_occupancy = mean(detection), .groups = "drop") %>% # mean of detection column gives the proportion of locations with detections (naive occupancy)
  arrange(desc(naive_occupancy))

gb_naive_summary <- gb_naive_long %>%
  group_by(species_common_name) %>%
  summarise(naive_occupancy = mean(detection), .groups = "drop") %>% # mean of detection column gives the proportion of locations with detections (naive occupancy)
  arrange(desc(naive_occupancy))

glimpse(ung_naive_summary)
glimpse(gb_naive_summary)


## Plot
ung_naiocc <- ggplot(ung_naive_summary,
                   aes(x = naive_occupancy, 
                       y = fct_reorder(species_common_name, naive_occupancy))) +  # re-orders species into descending naive_occupancy
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    title = "Naive Ungulate Occupancy, Thaidene Nëné 2021-2022",
    x = "Naive Occupancy",
    y = NULL # removes y-axis title
  ) + 
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 32, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.text = element_text(size = 16)) +
  theme(strip.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 18))

ung_naiocc

## Save plot
ggsave("figures/TDN/tdn_ungulate_naiveoccupancy_2021-2022.png", ung_naiocc, width = 18, height = 12, dpi = 300)

gb_naiocc <- ggplot(gb_naive_summary,
                     aes(x = naive_occupancy, 
                         y = fct_reorder(species_common_name, naive_occupancy) # re-orders species into descending naive_occupancy
                         )) + 
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    title = "Naive Game Bird Occupancy, Thaidene Nëné 2021-2022",
    x = "Naive Occupancy",
    y = NULL # removes y-axis title
    ) + 
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 32, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.text = element_text(size = 16)) +
  theme(strip.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 18))
gb_naiocc

## Save plot
ggsave("figures/TDN/tdn_gamebird_naiveoccupancy_2021-2022.png", gb_naiocc, width = 18, height = 12, dpi = 300)

#### 3. Spatial patterns in detections ####
## Only for target species (individual plots)

### Create a stn by species count matrix, where values = total number of detections of each species at each station
tdn_ung_count <- tdn_ung_mon |> 
  group_by(location) |> 
  summarise(
    across(
      all_of(ung_cols),
      ~ sum(. , na.rm = TRUE)
    ),
    .groups = "drop"
  )

glimpse(tdn_ung_count)
## all locations?
length(unique(tdn_ung_count$location)) # yes


tdn_gb_count <- tdn_gb_mon |> 
  group_by(location) |> 
  summarise(
    across(
      all_of(gb_cols),
      ~ sum(. , na.rm = TRUE)
    ),
    .groups = "drop"
  )

glimpse(tdn_gb_count)
length(unique(tdn_gb_count$location)) # yes



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
tdn_ung_count <- tdn_ung_count %>% 
  left_join(cam_locs, by = "location")

sf_tdn_ung <- st_as_sf(tdn_ung_count, coords = c("longitude", "latitude"), crs = 4326)
glimpse(sf_tdn_ung)


## Read in polygons as sf object
list.files("data/study_area_spatial")
tdn_sf <- st_read("data/study_area_spatial/ThaideneNene.shp")
tdn_sf <- st_transform(tdn_sf, crs = 3580)
st_crs(tdn_sf)
  
## Create 20km buffer around polygon to generate basemap
tdn_buffer <- st_buffer(tdn_sf, 20000)
tdn_buffer <- st_transform(tdn_buffer, crs = 3580)

# Create a basemap to extent of sk_wr_buffer
# Load basemap (e.g., "World_Imagery" or "OpenTopoMap")
basemap <- get_tiles(tdn_buffer, provider = "Esri.WorldTopoMap", crop = TRUE, zoom = 8) 
# note: higher resolution base imagery takes longer to download and display (but higher resolution better for smaller areas)

## Add in fire history data, crop to TDN
fire_history <- st_read("data/nrcan_nbac/NBAC_1972to2024_20250506_shp/NBAC_1972to2024_20250506.shp")
fire_history <- st_transform(fire_history, crs = 3580)
tdn_fire <- st_intersection(fire_history, tdn_sf)

## Moose counts
moose_ct_sf <- sf_tdn_ung %>% select(Moose, season)
colnames(moose_ct_sf) <- c("Moose", "season", "geometry")

## Remove locations with 0 moose
moose_ct_sf <- moose_ct_sf |> filter(Moose > 0)

glimpse(moose_ct_sf)
st_crs(moose_ct_sf)

moose_det <- ggplot() +
  #layer_spatial(basemap) + # add basemap
  geom_sf(data = tdn_sf, linewidth = 0, color = NA, fill = "lightgreen") + # study area background color
  geom_sf(data = tdn_fire, aes(fill = YEAR), color = NA, size = 1.5) + # TDN fire polygons
  geom_sf(data = tdn_sf, linewidth = 1, color = "black", fill = NA) + # study area outline
  geom_sf(
    data = moose_ct_sf, ## add spatial detection data
    aes(size = Moose,color = season), # vary point size by count of detections, change color for seasons
    show.legend = TRUE) + 
  scale_fill_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  scale_color_manual(
    values = c(
      "Summer" = "forestgreen",
      "Winter" = "blue"
    )
  ) +
  labs(x = "Longitude",
       y = "Latitude",
       size = "Moose detections",
       color = "Season",
       fill = "Fire Year") +
  theme_classic() +
  ## increase size of points in legend
  guides(
    color = guide_legend(
      override.aes = list(size = 6)
    )
  ) +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14)
  )

win.graph()
moose_det

ggsave("figures/tdn2021-2022_spatial_moose_detections.png", moose_det, width = 12, height = 8, dpi = 300)


## Muskox counts
muskox_ct_sf <- sf_tdn_ung %>% select(Muskox, season)
colnames(muskox_ct_sf) <- c("Muskox", "season", "geometry")

## Remove locations with 0 Muskox
muskox_ct_sf <- muskox_ct_sf |> filter(Muskox > 0)

glimpse(muskox_ct_sf)
st_crs(muskox_ct_sf)

muskox_det <- ggplot() +
  #layer_spatial(basemap) + # add basemap
  geom_sf(data = tdn_sf, linewidth = 0, color = NA, fill = "lightgreen") + # study area background color
  geom_sf(data = tdn_fire, aes(fill = YEAR), color = NA, size = 1.5) + # TDN fire polygons
  geom_sf(data = tdn_sf, linewidth = 1, color = "black", fill = NA) + # study area outline
  geom_sf(
    data = muskox_ct_sf, ## add spatial detection data
    aes(size = Muskox, color = season), # vary point size by count of detections, change color for seasons
    show.legend = TRUE) +
  scale_fill_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  scale_color_manual(
    values = c(
      "Summer" = "forestgreen",
      "Winter" = "blue"
    )
  ) +
  labs(x = "Longitude",
       y = "Latitude",
       size = "Muskox detections",
       color = "Season",
       fill = "Fire Year") +
  theme_classic() +
  ## increase size of points in legend
  guides(
    color = guide_legend(
      override.aes = list(size = 6)
    )
  ) +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14)
  )

win.graph()
muskox_det

ggsave("figures/tdn2021-2022_spatial_muskox_detections.png", muskox_det, width = 12, height = 8, dpi = 300)


### Game birds
## Convert stn_species_count to sf object (first add coordinates)
tdn_gb_count <- tdn_gb_count %>% 
  left_join(cam_locs, by = "location")

sf_tdn_gb <- st_as_sf(tdn_gb_count, coords = c("longitude", "latitude"), crs = 4326)
glimpse(sf_tdn_gb)

## Ptarmigan counts
ptarm_ct_sf <- sf_tdn_gb %>% select(Ptarmigans, season)
colnames(ptarm_ct_sf) <- c("Ptarmigans", "season", "geometry")

## Remove locations with 0 ptarm
ptarm_ct_sf <- ptarm_ct_sf |> filter(Ptarmigans > 0)

glimpse(ptarm_ct_sf)
st_crs(ptarm_ct_sf)

ptarm_det <- ggplot() +
  #layer_spatial(basemap) + # add basemap
  geom_sf(data = tdn_sf, linewidth = 0, color = NA, fill = "lightgreen") + # study area background color
  geom_sf(data = tdn_fire, aes(fill = YEAR), color = NA, size = 1.5) + # TDN fire polygons
  geom_sf(data = tdn_sf, linewidth = 1, color = "black", fill = NA) + # study area outline
  geom_sf(
    data = ptarm_ct_sf, ## add spatial detection data
    aes(size = Ptarmigans, color = season), # vary point size by count of detections, change color for seasons
    show.legend = TRUE) +
  scale_fill_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  scale_color_manual(
    values = c(
      "Summer" = "forestgreen",
      "Winter" = "blue"
    )
  ) +
  labs(x = "Longitude",
       y = "Latitude",
       size = "Ptarmigan detections",
       color = "Season",
       fill = "Fire Year") +
  theme_classic() +
  ## increase size of points in legend
  guides(
    color = guide_legend(
      override.aes = list(size = 6)
    )
  ) +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14)
  )

win.graph()
ptarm_det

ggsave("figures/tdn2021-2022_spatial_ptarmigan_detections.png", ptarm_det, width = 12, height = 8, dpi = 300)

## spgr counts
spgr_ct_sf <- sf_tdn_gb %>% select(Spruce.Grouse, season)
colnames(spgr_ct_sf) <- c("Spruce.Grouse", "season", "geometry")

## Remove locations with 0 spgr
spgr_ct_sf <- spgr_ct_sf |> filter(Spruce.Grouse > 0)

glimpse(spgr_ct_sf)
st_crs(spgr_ct_sf)

spgr_det <- ggplot() +
  #layer_spatial(basemap) + # add basemap
  geom_sf(data = tdn_sf, linewidth = 0, color = NA, fill = "lightgreen") + # study area background color
  geom_sf(data = tdn_fire, aes(fill = YEAR), color = NA, size = 1.5) + # TDN fire polygons
  geom_sf(data = tdn_sf, linewidth = 1, color = "black", fill = NA) + # study area outline
  geom_sf(
    data = spgr_ct_sf, ## add spatial detection data
    aes(size = Spruce.Grouse, color = season), # vary point size by count of detections, change color for seasons
    show.legend = TRUE) +
  scale_fill_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  scale_color_manual(
    values = c(
      "Summer" = "forestgreen",
      "Winter" = "blue"
    )
  ) +
  labs(x = "Longitude",
       y = "Latitude",
       size = "Spruce Grouse detections",
       color = "Season",
       fill = "Fire Year") +
  theme_classic() +
  ## increase size of points in legend
  guides(
    color = guide_legend(
      override.aes = list(size = 6)
    )
  ) +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14)
  )

win.graph()
spgr_det

ggsave("figures/tdn2021-2022_spatial_spgr_detections.png", spgr_det, width = 12, height = 8, dpi = 300)

## stgr counts
stgr_ct_sf <- sf_tdn_gb %>% select(Sharp.tailed.Grouse, season)
colnames(stgr_ct_sf) <- c("Sharp.tailed.Grouse", "season", "geometry")

## Remove locations with 0 stgr
stgr_ct_sf <- stgr_ct_sf |> filter(Sharp.tailed.Grouse > 0)

glimpse(stgr_ct_sf)
st_crs(stgr_ct_sf)

stgr_det <- ggplot() +
  #layer_spatial(basemap) + # add basemap
  geom_sf(data = tdn_sf, linewidth = 0, color = NA, fill = "lightgreen") + # study area background color
  geom_sf(data = tdn_fire, aes(fill = YEAR), color = NA, size = 1.5) + # TDN fire polygons
  geom_sf(data = tdn_sf, linewidth = 1, color = "black", fill = NA) + # study area outline
  geom_sf(
    data = stgr_ct_sf, ## add spatial detection data
    aes(size = Sharp.tailed.Grouse, color = season), # vary point size by count of detections, change color for seasons
    show.legend = TRUE) +
  scale_fill_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  scale_color_manual(
    values = c(
      "Summer" = "forestgreen",
      "Winter" = "blue"
    )
  ) +
  labs(x = "Longitude",
       y = "Latitude",
       size = "Sharp-tailed Grouse detections",
       color = "Season",
       fill = "Fire Year") +
  theme_classic() +
  ## increase size of points in legend
  guides(
    color = guide_legend(
      override.aes = list(size = 6)
    )
  ) +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14)
  )

win.graph()
stgr_det

ggsave("figures/tdn2021-2022_spatial_stgr_detections.png", stgr_det, width = 12, height = 8, dpi = 300)


### Compare ROPT and WIPT locations
glimpse(tdn_gb_count)

## Select only the 2 ptarm spp
ptarm_spp <- tdn_gb_count |> 
  select(-Ptarmigans, -Sharp.tailed.Grouse, -Spruce.Grouse, -X)

glimpse(ptarm_spp)

## Pivot tdn_gb_count longer and only keep 
l_ptarmspp <- ptarm_spp |> 
  pivot_longer(
    cols = c("Willow.Ptarmigan", "Rock.Ptarmigan"),
    names_to = "species",
    values_to = "total_detections"
  )

glimpse(l_ptarmspp)

sf_ptarms <- st_as_sf(l_ptarmspp, coords = c("longitude", "latitude"), crs = 4326)
glimpse(sf_ptarms)

## filter for presence only
sf_ptarms <- sf_ptarms |> filter(total_detections > 0)

### Plot ptarms to compare occurrences
ptarms_in_tdn <- ggplot() +
  #layer_spatial(basemap) + # add basemap
  geom_sf(data = tdn_sf, linewidth = 0, color = NA, fill = "lightgreen") + # study area background color
  geom_sf(data = tdn_fire, aes(fill = YEAR), color = NA, size = 1.5) + # TDN fire polygons
  geom_sf(data = tdn_sf, linewidth = 1, color = "black", fill = NA) + # study area outline
  geom_sf(
    data = sf_ptarms, ## add spatial detection data
    aes(size = total_detections, color = species), # vary point size by count of detections, change color for seasons
    show.legend = TRUE) +
  scale_fill_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  scale_color_manual(
    values = c(
      "Willow.Ptarmigan" = "purple",
      "Rock.Ptarmigan" = "dodgerblue"
    )
  ) +
  labs(x = "Longitude",
       y = "Latitude",
       size = "No. of Detections",
       color = "Species",
       fill = "Fire Year") +
  theme_classic() +
  ## increase size of points in legend
  guides(
    color = guide_legend(
      override.aes = list(size = 6)
    )
  ) +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14)
  )

win.graph()
ptarms_in_tdn
