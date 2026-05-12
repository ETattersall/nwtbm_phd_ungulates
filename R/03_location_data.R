####################################
## 03_location_data.R
## Combining location data from all study areas, converting to spatial data and creating buffers
## Also adding Sambaa K'e winter road to study area shapefile and creating 20km buffers around study areas for spatial data extractions
## Started on Feb 13 2026
## Created by Erin Tattersall
####################################



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



# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


#### Load in camera locations ####
## Location names and coordinates were aggregated and checked in nwtbm_phd_general/02_merge_station_locations.R
cam_locs <- read.csv("data/wt_location_data/nwtbm_cam_locations_20260506.csv")
glimpse(cam_locs)
## Load spatial file
cam_locs_sf <- read_sf("data/wt_location_data/nwtbm_cam_locations_20260506.gpkg")
st_crs(cam_locs_sf) # crs 4326
plot(cam_locs_sf["study_area"]) # plot camera locations colored by study area
glimpse(cam_locs_sf)

## Re-order columns so study_area and site are first
cam_locs <- cam_locs %>% 
  select(study_area, site, location, latitude, longitude, sensor_type)

length(unique(cam_locs$site)) ## 172 unique sites across all study areas

sites_sa <- cam_locs %>% 
  group_by(study_area) %>% 
  summarise(
    n_sites = n_distinct(site),
    n_stations = n_distinct(location),
    .groups = "drop"
  )

glimpse(cam_locs)


### Calculate distances between stations
## Transform sf obj into NWT Lambert
cam_locs_sf <- st_transform(cam_locs_sf, crs = 3580)
st_crs(cam_locs_sf)

# Function to compute pairwise distance summary for one study area
distance_summary <- function(df) {
  # Compute distance matrix
  dmat <- st_distance(df)
  # Convert to numeric matrix
  dmat <- as.matrix(dmat)
  # Keep only lower triangle (no duplicates, no diagonal)
  dvals <- dmat[lower.tri(dmat)]
  tibble(
    mean_dist = mean(dvals),
    min_dist  = min(dvals),
    max_dist  = max(dvals)
  )
}


## Apply distance_summary function to each study area and combine results into a single data frame
dist_sa <- cam_locs_sf %>%
  group_by(study_area) %>%
  group_modify(~ distance_summary(.x)) %>%
  ungroup()


## TDN locations 27m apart - three stations accidentally deployed twice (032-01A/B, 032-02A/B, 032-03A/B)
## Otherwise, minimum distances between cameras = 114m in NW.

### Create polygons around sites
site_polygons <- cam_locs_sf %>%
  group_by(study_area, site) %>%
  summarise(
    geometry = st_convex_hull(st_union(geom)),
    .groups = "drop"
  )
glimpse(site_polygons) ## most are polygons, except Sambaa K'e and one lone station in Ft Smith (kept as points - only one stn with data in the FS site)
plot(site_polygons["study_area"])

## What is the average distance between sites?
dist_sites <- site_polygons %>% 
  group_by(study_area) %>%
  group_modify(~ distance_summary(.x)) %>%
  ungroup()

## What is the average area of the site polygons by study area?
site_polygons$site_area <- as.numeric(st_area(site_polygons))
# Convert to sq km
site_polygons$site_area_sqkm <- site_polygons$site_area/1e+06

site_area_sa <- site_polygons %>% 
  group_by(study_area) %>% 
  summarise(min_area = min(site_area_sqkm),
            mean_area = mean(site_area_sqkm),
            max_area = max(site_area_sqkm))

### Save summaries
cam_spatial_sum <- left_join(dist_sa, dist_sites, by = "study_area")
cam_spatial_sum <- left_join(cam_spatial_sum, site_area_sa, by = "study_area")
# rename columns
colnames(cam_spatial_sum) <- c("study_area", "mean_station_distance_m", "min_station_distance_m", "max_station_distance_m", "mean_site_distance_m", "min_site_distance_m", "max_site_distance_m", "min_site_area_sqkm", "mean_site_area_sqkm", "max_site_area_sqkm")
#remove geometry column
cam_spatial_sum <- cam_spatial_sum[ ,1:10]

write.csv(cam_spatial_sum, "data/wt_location_data/nwtbm_station_site_spatial_summaries.csv")


#### Buffer size selections: ####
## Start with 500m buffers around sites, approximating 3rd order selection

## Create an sf object using site_polygons with 500m buffers around sites for later use in extracting fire history data around camera locations
sites_500m <- st_buffer(site_polygons, dist = 500)

plot(sites_500m["study_area"])

## Plot SambaaK'e to look at buffer overlap
sk_500m <- sites_500m %>% 
  filter(study_area == "SambaaK'e") %>% 
  plot()
#12 buffers overlap, which isn't too bad

## Plot Fort Smith to look at weird line of polygons in NE corner
fs_500m <- sites_500m %>% 
  filter(study_area == "FortSmith") %>% 
  plot()
fs_500m # locations are spread out further than in other areas

## Re-calculate area of 500m buffer polygons
sites_500m$site_area <- as.numeric(st_area(sites_500m))
# Convert to sq km
sites_500m$site_area_sqkm <- sites_500m$site_area/1e+06

site500_area_sa <- sites_500m %>% 
  group_by(study_area) %>% 
  summarise(min_area = min(site_area_sqkm),
            mean_area = mean(site_area_sqkm),
            max_area = max(site_area_sqkm))

## Save site polygons
st_write(site_polygons, "data/wt_location_data/nwtbm_cam_sites.gpkg", delete_layer = TRUE)
### Save 500m site buffer as sf objects (gpkg files) for extracting spatial data around stations
st_write(sites_500m, "data/wt_location_data/nwtbm_cam_sites_500mbuffer.gpkg", delete_layer = TRUE)


### Also save 500m buffer around locations, approximating 4th order selection (ungulates) or 3rd order for game birds?
cam_locs_500 <- st_buffer(cam_locs_sf, dist = 500)

## save 
### Save 500m site buffer as sf objects (gpkg files) for extracting spatial data around stations
st_write(cam_locs_500, "data/wt_location_data/nwtbm_cam_locations_500mbuffer.gpkg", delete_layer = TRUE)

#### Study Area polygons and buffers ####
getwd()
setwd("data/study_area_spatial")
list.files() ## want shapefiles of combined study areas, plus the Sambaa K'e winter road, which will have to be added

## Load NWTBM_all_study_areas.shp (created in nwtbm_phd_general project - R/03_study_area_maps.R)
sa_sf <- st_read("NWTBM_all_study_areas.shp")

## Load Sambaa K'e winter road shapefile (sambaake_winter_road_shp.shp)
sk_wr_sf <- st_read("sambaake_winter_road_shp.shp") ## isolated in nwtbm_phd_general project - R/03b_ind_study_area_maps.R

#return to base directory
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates")

st_crs(sk_wr_sf) ## currently WGS 84 - need to transform to match sa_sf (NWT Lambert, 3580)
sk_wr_sf <- st_transform(sk_wr_sf, crs = 3580)

## sk_wr_sf currently has two features (two segments of the winter road) - combine into one feature using st_union
sk_wr_sf <- st_sf(st_union(sk_wr_sf)) #combines 2 features into one and keeps it as an sf object instead of sfc
class(sk_wr_sf) ## sf

## add study_area column to sk_wr_sf
sk_wr_sf$study_area <- "SambaaK'e"

colnames(sk_wr_sf) <- c("geometry", "study_area")

## assign geometry column name to "geometry" in sk_wr_sf
sk_wr_sf <- st_set_geometry(sk_wr_sf, "geometry")

glimpse(sk_wr_sf)
st_length(sk_wr_sf) ## 96.166 km long

glimpse(sa_sf)

plot(sa_sf)



## Sambaa K'e winter road is a multi-linestring, since it is a linear feature. To turn it into a polygon, add a buffer of 50m to approximate the area surveyed
sk_wr_sf_poly <- st_buffer(sk_wr_sf, dist = 50)
glimpse(sk_wr_sf_poly)
st_area(sk_wr_sf_poly) ## 9.6 sq.km


## Add winter road to sa_sf
sa_sf2 <- rbind(sa_sf, sk_wr_sf_poly)


## Remove Sambaa K'e polygon (3rd row) and keep Sambaa K'e winter road (7th row) in sa_sf2
sa_sf2 <- sa_sf2[-3, ]

plot(sa_sf2["study_area"]) # check that the combined study area shapefile looks correct with the winter road included and the Sambaa K'e polygon removed

### save new shapefile for study areas with only sambaa ke winter road polygon
st_write(sa_sf2, "data/study_area_spatial/NWTBM_all_study_area_polygons.shp", delete_layer = TRUE)

## What is the total area of each study area? Given in m^2 - convert to km^2 (divide by 1 million) and convert to numeric
sa_areas <- cbind.data.frame(
              sa_sf2$study_area,
              as.numeric(st_area(sa_sf2)/1000000))
class(sa_areas[ ,2]) 
sa_areas[2]
colnames(sa_areas) <- c("study_area","area_sqkm") 
colnames(sa_areas)

## Add sites_sa (number of sites and stations per sa) to sa_areas
sa_areas <- left_join(sa_areas, sites_sa, by = "study_area")


## save areas as csv to add to with other summaries
write.csv(sa_areas, "data/study_area_summaries.csv")


### Add 20km buffers to each study area (generous buffer for spatial data extractions)
sa_sf2_buffer <- st_buffer(sa_sf2, dist = 20000)
plot(sa_sf2_buffer["study_area"]) # check that the buffered study area shapefile looks correct

### Save sa_sf2_buffer as shapefiles for later use in extracting spatial data around study areas
st_write(sa_sf2_buffer, "data/study_area_spatial/NWTBM_all_study_areas_20km_buffers.shp", delete_layer = TRUE)

