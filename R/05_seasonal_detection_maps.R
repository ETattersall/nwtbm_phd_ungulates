#############################################
## 05_seasonal_detection_maps.R
## Creating single map of seasonal detections for each target species
## Started 17 Nov 2026 by Erin Tattersall
############################################


list.of.packages <- c("tidyverse", "sf", "maptiles", "ggspatial", "terra", "tidyterra", "kableExtra", "leaflet", "viridis", "corrplot", "lubridate", "plotly", "ggplot2", "ggbreak", "ggmap")
lapply(list.of.packages, require, character.only = TRUE)


### Load seasonal detection data
cam_det <- read.csv("data/camera_data/nwtbm_allspecies_seasonal_detections.csv")

glimpse(cam_det)
cam_det$X <- NULL
summary(cam_det)

## Need location coordinates
cam_locs <- read.csv("data/wt_location_data/nwtbm_cam_locations_20260506.csv")
glimpse(cam_locs)
cam_locs$X <- NULL

## Join coordinates to detections

cam_det <- cam_det |> 
  left_join(cam_locs, by = c("study_area", "location"))

glimpse(cam_det)


### Load spatial polygons for NWT boundary, study areas
nwt_boun <- vect("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/study_area_spatial/NWT_boundary.shp")
crs(nwt_boun)


study_areas <- vect("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/study_area_spatial/NWTBM_all_study_area_polygons.shp")
crs(study_areas)

### Re-project study_areas into same projection as other two
study_areas <- project(study_areas, crs(nwt_boun))
crs(study_areas)

## Convert species detections to long format for plotting - separate ungulates from gamebirds
glimpse(cam_det)
ung_cols <- c("Bison", "Moose", "Muskox", "Woodland.Caribou")
gb_cols <- c("Ptarmigans", "Ruffed.Grouse", "Sharptailed.Grouse", "Spruce.Grouse") # ignore WIPT and ROPT columns


#### Plotting ungulates
ung_long <- pivot_longer(
  cam_det,
  cols = all_of(ung_cols),
  names_to = "ungulates",
  values_to = "detections"
) |> 
  select( ## remove game bird and hare columns
    - Ptarmigans, -Ruffed.Grouse, -Rock.Ptarmigan, -Sharptailed.Grouse, -Spruce.Grouse, -Willow.Ptarmigan, -Snowshoe.Hare
  )

glimpse(ung_long)

## Convert ung_long to vect object
ung_vect <- vect(ung_long, geom = c("longitude", "latitude"))
crs(ung_vect)
## Convert crs
ung_vect <- project(ung_vect, crs(nwt_boun))
plot(ung_vect)
crs(ung_vect)

glimpse(ung_vect)
summary(ung_vect$detections) ## max 79 detections
hist(ung_vect$detections)

### Remove observations with 0 detections (each row is species-specific)
ung_vect <- ung_vect |> filter(detections > 0)

### Separate into 2 datasets by season
snow_vect <- ung_vect |> filter(season == "snow")
nosnow_vect <- ung_vect |> filter(season == "snow-free")

### Mapping detections by species and and season 
## Snow season
p <- ggplot() + ## First map NWT Boundary and study areas
  geom_spatvector(data = nwt_boun) +
  geom_spatvector(data = study_areas, fill = "lightgreen") + 
  geom_spatvector(
    data = snow_vect, ## add detections in snow season
    aes(size = detections) # transform the size mapping so large detections don't dominate
  ) +
  facet_wrap(~ungulates) +
  theme_classic() +
  theme(axis.text = element_text(size = 14)) +
  theme(strip.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 18)) +
  theme(legend.text = element_text(size = 14))

win.graph()
p

### Save
ggsave("figures/ungulate_snowseason_detections_map.png", p, width = 18, height = 12, dpi = 300)


## Snow-free season
q <- ggplot() + ## First map NWT Boundary and study areas
  geom_spatvector(data = nwt_boun) +
  geom_spatvector(data = study_areas, fill = "lightgreen") + 
  geom_spatvector(
    data = nosnow_vect, ## add detections in snow season
    aes(size = detections) # transform the size mapping so large detections don't dominate
  ) +
  facet_wrap(~ungulates) +
  theme_classic() +
  theme(axis.text = element_text(size = 14)) +
  theme(strip.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 18)) +
  theme(legend.text = element_text(size = 14))

win.graph()
q

### Save
ggsave("figures/ungulate_snow-freeseason_detections_map.png", q, width = 18, height = 12, dpi = 300)


#### Plotting gamebirds

gb_long <- pivot_longer(
  cam_det,
  cols = all_of(gb_cols),
  names_to = "game_birds",
  values_to = "detections"
) |> 
  select( ## remove ungulate and hare, WIPT and ROPT columns
    -Bison, -Moose, -Muskox, -Woodland.Caribou, -Rock.Ptarmigan, -Willow.Ptarmigan, -Snowshoe.Hare
  )

glimpse(gb_long)

## Convert ung_long to vect object
gb_vect <- vect(gb_long, geom = c("longitude", "latitude"))
crs(gb_vect)
## Convert crs
gb_vect <- project(gb_vect, crs(nwt_boun))
plot(gb_vect)
crs(gb_vect)

glimpse(gb_vect)
summary(gb_vect$detections) ## max 59 detections
hist(gb_vect$detections)

### Remove observations with 0 detections (each row is species-specific)
gb_vect <- gb_vect |> filter(detections > 0)

### Separate into 2 datasets by season
gbsnow_vect <- gb_vect |> filter(season == "snow")
gbnosnow_vect <- gb_vect |> filter(season == "snow-free")

### Mapping detections by species and and season 
## Snow season
p_gb <- ggplot() + ## First map NWT Boundary and study areas
  geom_spatvector(data = nwt_boun) +
  geom_spatvector(data = study_areas, fill = "lightgreen") + 
  geom_spatvector(
    data = gbsnow_vect, ## add detections in snow season
    aes(size = detections) # transform the size mapping so large detections don't dominate
  ) +
  facet_wrap(~game_birds) +
  theme_classic() +
  theme(axis.text = element_text(size = 14)) +
  theme(strip.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 18)) +
  theme(legend.text = element_text(size = 14))

win.graph()
p_gb

### Save
ggsave("figures/gamebirds_snowseason_detections_map.png", p_gb, width = 18, height = 12, dpi = 300)


## Snow-free season
q_gb <- ggplot() + ## First map NWT Boundary and study areas
  geom_spatvector(data = nwt_boun) +
  geom_spatvector(data = study_areas, fill = "lightgreen") + 
  geom_spatvector(
    data = gbnosnow_vect, ## add detections in snow season
    aes(size = detections) # transform the size mapping so large detections don't dominate
  ) +
  facet_wrap(~game_birds) +
  theme_classic() +
  theme(axis.text = element_text(size = 14)) +
  theme(strip.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 18)) +
  theme(legend.text = element_text(size = 14))

win.graph()
q_gb

### Save
ggsave("figures/gamebirds_snow-freeseason_detections_map.png", q_gb, width = 18, height = 12, dpi = 300)
