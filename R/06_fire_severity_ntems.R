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

## Convert sa_20km to terra object so it can be used to crop fsev
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
  geom_sf(data = sa_sf, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = cam_locs_sf, size = 2, color = "black") +
  scale_fill_gradient(low = "white", high = "red",na.value = "transparent") +  # Make NA values blank
  coord_sf() +
  labs(title = "Fire Severity in NWTBM Study Areas", 
       x = "Longitude",
       y = "Latitude",
       fill = "Fire Severity (dNBR)") +
  theme_classic() +
    # increase size of title text, axis text, and facet titles
    theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
    theme(axis.title.x = element_text(size = 16)) +
    theme(axis.title.y = element_text(size = 16)) +
    theme(axis.text = element_text(size = 12)) +
    theme(legend.title= element_text(size = 16))

win.graph()
gg_sa_fsev

## Save plot
ggsave("figures/fire_explore/ntems_fireseverity_1985-2022_studyareas.jpeg", gg_sa_fsev, width = 12, height = 8, dpi = 300)

###### Summary statistics ####

## summaries of fire severity within each study area, around each camera location (500m buffer)


# Read in current 500m buffer layer as sf object
cams_500m_buffer <- st_read("data/wt_location_data/cams_500m_buffer.shp")

glimpse(cams_500m_buffer) ## rename columns (same as cams_locs_sf)
colnames(cams_500m_buffer) <- c("location", "buffer_m", "location_visibility", "true_coordinates", "location_comments", "internal_wildtrax_id", "study_area", "geometry")

### function to rasterize each buffered camera location and calculate average fire severity within that buffer
calc_fsev <- function(location_id) { # a character vector of all camera locations
  
  # 1) Get the buffer for a specific location
  buf <- cams_500m_buffer %>%
    filter(location == location_id)
  
  # Safety check: if no buffer found
  if (nrow(buf) == 0) {
    stop(paste("No buffer found for location:", location_id))
  }
  
  # 2) Reproject buffer to match fire severity CRS
  buf_reproj <- st_transform(buf, crs(fsev_sa_20km))
  
  # Convert sf → terra SpatVector (terra requires SpatVector)
  buf_vect <- vect(buf_reproj)
  
  # 3) Extract mean fire severity data from each buffer polygon
  mean_fsev <- extract(fsev_sa_20km, # specifies SpatRaster to be extracted
                  buf_reproj, # specifies SpatVector with polygons to use for extraction
                  fun = mean)[[2]] # function to summarize extracted data by calculating mean by polygon, then selecting the 2nd column (containing severity data)
  
  # 4) Return tibble of location + average fire severity
  tibble(
    location = location_id,
    mean_fsev = mean_fsev
  )
}


## create character vector of camera location names
cam_locs_ids <- unique(cams_500m_buffer$location)

fsev_cams_500m <- calc_fsev(cam_locs_ids)

glimpse(fsev_cams_500m)
summary(fsev_cams_500m$mean_fsev) ## IT WORKED!!

hist(fsev_cams_500m$mean_fsev)



## testing ###

# reproject cams_500m_buff to match fsev, convert to SpatVector, 
vcams_500 <- cams_500m_buffer %>% 
  st_transform(crs(fsev_sa_20km)) %>% 
  vect()
  
glimpse(vcams_500)
plot(vcams_500)

## Extract fsev_sa_20km to vcams_500
fsev_500 <- extract(fsev_sa_20km, vcams_500, fun = mean)[[2]]
glimpse(fsev_500)
summary(fsev_500)

win.graph()
plot(fsev_500)




