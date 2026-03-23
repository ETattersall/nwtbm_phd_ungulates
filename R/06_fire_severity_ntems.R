########################################
## 06_fire_severity_ntems.R
## Exploring NTEMS fire severity data
## Started on March 23 2026
## Created by Erin Tattersall
########################################

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


## Camera locations (not buffered)
cam_locs_sf <- st_read("data/wt_location_data/all_projects_cam_locations_20260320.shp")
glimpse(cam_locs_sf) ## column names have gotten all mixed up - fix (st_write truncates attribute names longer than 10 characters)
colnames(cam_locs_sf) <- c("location", "buffer_m", "location_visibility", "true_coordinates", "location_comments", "internal_wildtrax_id", "study_area", "geometry")

### Load study area polygons
list.files("data/study_area_spatial")
sa_sf <- st_read("data/study_area_spatial/NWTBM_all_study_areas.shp")

### Load study area polygons with 20km buffer
sa_20km <- st_read("data/study_area_spatial/NWTBM_all_study_areas_20km_buffers.shp")
class(sa_20km) #sf object

### Inspect fire severity data before loading - these data only cover up to 2022 - need to see if there's more up-to-date fire severity data
list.files("data/CA_Forest_Wildfire_dNBR_1985-2022")

describe("data/CA_Forest_Wildfire_dNBR_1985-2022/CA_Forest_Wildfire_dNBR_1985-2022.tif")

## load the fire severity raster file
fsev <- rast("data/CA_Forest_Wildfire_dNBR_1985-2022/CA_Forest_Wildfire_dNBR_1985-2022.tif")
fsev #SpatRaster class, projection Lambert_Conformal_Conic_2SP
crs(fsev)

win.graph()
plot(fsev)

## Convert sa_20km to terra object
sa_20km_terra <- sa_20km %>% 
  st_transform(crs = st_crs(fsev)) %>% # match fsev projection
  vect()
sa_20km_terra # SpatVector of projection Lambert_Conformal_Conic_2SP

## Crop fsev to 20km SA buffers
fsev_sa_20km <- fsev %>% 
  crop(sa_20km_terra)

## save study area fire severity data as raster
writeRaster(fsev_sa_20km, "data/CA_Forest_Wildfire_dNBR_1985-2022/NWT_studyareas_20km_wildfire_dNBR_1985-2022.tif")


### Map fire severity in study areas
gg_sa_fsev <- ggplot() +
  geom_spatraster(data= fsev_sa_20km, use_coltab = TRUE) +
  geom_sf(data = sa_sf, fill = NA, color = "black") +
  geom_sf(data = cam_locs_sf, size = 2, color = "black") +
  scale_fill_gradient(na.value = "transparent") +  # Make NA values blank
  coord_sf() +
  theme_minimal() +
  theme(
    axis.text = element_blank(),      # Remove axis text
    axis.title = element_blank(),     # Remove axis titles
    axis.ticks = element_blank(),     # Remove axis ticks
    panel.grid = element_blank()      # Remove gridlines
  ) +
  labs(title = "Fire Severity in NWTBM Study Areas", 
       x = "Longitude",
       y = "Latitude",
       fill = "Fire Severity (dNBR)"
  )

win.graph()
gg_sa_fsev
