####################################
## 03_location_data.R
## Combining location data from all study areas, converting to spatial data and creating buffers
## Also adding Sambaa K'e winter road to study area shapefile and creating 20km buffers around study areas for spatial data extractions
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


#### Load in camera locations from WildTrax ####
## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()


## Get project information for my WildTrax projects
cam_projects <- wt_get_projects("CAM")
glimpse(cam_projects) ## lists all the projects I have access to - including public projects I'm not involved in
## Filter to my target projects only, using project IDs: 712 (Thaidene Nene), 2183 (Fort Smith), 2102 (Norman Wells), 1906 (Sambaa K'e), 2935 (Gameti), 1465 (Edehzhie)
cam_projects <- cam_projects %>% filter(project_id == "712" |
                                          project_id == "2183" |
                                          project_id == "2102" |
                                          project_id == "1906" |
                                          project_id == "2935" |
                                          project_id == "1465")


## Get sensor locations for each project ##
# not working currently - use manual downloads (also not included in RDS list of main reports - need to download location reports individually)
# cam_locs <- wt_download_report(project_id = cam_projects$project_id,
#                                sensor_id = "CAM",
#                                reports = "location")

setwd("data/wt_location_data")
list.files()
cam_loc_files <- list.files(pattern = "\\.csv$") ## all projects csv already created - skip to reading this in (line 115)
cam_loc_files

# Read and bind all CSVs, adding a column for the source file
cam_locs <- rbindlist(lapply(cam_loc_files, function(file) {
  dt <- fread(file)
  dt[, source_file := basename(file)]
  return(dt)
}))

summary(cam_locs) ## No NAs in lat/long columns

#return to base directory
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates")


### Add a column for study area
cam_locs <- cam_locs %>%
  mutate(study_area = case_when(
    str_detect(source_file, "Edéhzhíe") ~ "Edéhzhíe",
    str_detect(source_file, "FortSmith") ~ "FortSmith",
    str_detect(source_file, "Gameti") ~ "Gameti",
    str_detect(source_file, "NormanWells") ~ "NormanWells",
    str_detect(source_file, "SambaaK'e") ~ "SambaaK'e",
    str_detect(source_file, "ThaideneNëné") ~ "ThaideneNëné",
    TRUE ~ NA_character_  # Default case if no match
  ))

## Remove source_file column
cam_locs <- cam_locs %>%
  select(-source_file)

glimpse(cam_locs)
table(is.na(cam_locs$latitude))
table(is.na(cam_locs$longitude)) ## Confirmed no NAs in lat/long
table(is.na(cam_locs$study_area)) # no NAs in study area column either, so all cameras were successfully assigned to a study area

class(cam_locs) # data.table

### Save cam_locs as csv file
write.csv(cam_locs, "data/wt_location_data/all_projects_cam_locations_20260224.csv", row.names = FALSE)
table(cam_locs$study_area)

## read in cam_locs from csv file (if already created)
cam_locs <- read.csv("data/wt_location_data/all_projects_cam_locations_20260224.csv")

### Plot camera locations to check they look correct
cam_locs_sf <- st_as_sf(cam_locs, coords = c("longitude", "latitude"), crs = 4326) # convert to sf object with WGS 84 CRS
plot(cam_locs_sf["study_area"]) # plot camera locations colored by study area 

## Within each study area, calculate pairwise distances between locations (in meters) using st_distance function from sf package
cam_locs_sf <- st_transform(cam_locs_sf, crs = 3580) # transform to NWT Lambert Area projection for accurate distance calculations in meters

## save cam_locs_sf as shapefile for later use in extracting spatial data around stations
st_write(cam_locs_sf, "data/wt_location_data/all_projects_cam_locations_20260310.shp", delete_layer = TRUE)


# Function to compute pairwise distance summary for one study area
distance_summary <- function(df) {
  # Compute distance matrix
  dmat <- st_distance(df)
  # Convert to numeric matrix
  dmat <- as.matrix(dmat)
  # Keep only lower triangle (no duplicates, no diagonal)
  dvals <- dmat[lower.tri(dmat)]
  tibble(
    study_area = unique(df$study_area),
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

# ## Which locations are 0m apart in Norman Wells? In cam_locs, which rows have identical lat/long coordinates? 
## (These have been corrected in my downloaded copy of the csv, not yet on WildTrax (19 Feb 2026))
# norman_wells_locs <- cam_locs %>% filter(study_area == "NormanWells")
# norman_wells_locs <- norman_wells_locs %>%
#   mutate(lat_long = paste(latitude, longitude)) # create a combined lat/long column for easier comparison
# duplicate_locs <- norman_wells_locs %>%
#   group_by(lat_long) %>%
#   filter(n() > 1) # keep only rows with duplicate lat/long values 
# 
# ## BMS-NRA-050-16 and BMS-NRA-050-18 have same coordinates - error in WildTrax
# ## 050-18 should be 65.35197, -126.52474


## Total distance summaries (not split by study area)
mean(dist_sa$mean_dist) #65.3 km
min(dist_sa$min_dist) # 27 m (or 114 m, if we exclude the three stations that were accidentally deployed twice in TDN)
max(dist_sa$max_dist) # 282 km

#### Buffer size selections: ####
## could assume 100m, as minimal distance between stations, represents 4th order selection (i.e foraging patch)
## 500 m buffer is fairly standard, representing 3rd order selection (i.e. selecting habitat components in a home range), despite variance across species

## Create an sf object using cam_locs_sf with 100m and 500m buffers around camera locations for later use in extracting fire history data around camera locations
cams_100m_buffer <- st_buffer(cam_locs_sf, dist = 100)
cams_500m_buffer <- st_buffer(cam_locs_sf, dist = 500)

plot(cams_100m_buffer["study_area"]) # test plot 100m buffers around camera locations

### Save cams_100m buffer and cams_500m_buffer as sf objects (shp files) for extracting spatial data around stations
st_write(cams_100m_buffer, "data/wt_location_data/cams_100m_buffer.shp", delete_layer = TRUE)
st_write(cams_500m_buffer, "data/wt_location_data/cams_500m_buffer.shp", delete_layer = TRUE)


#### Study Area polygons and buffers ####
getwd()
setwd("data/study_area_spatial")
list.files() ## want shapefiles of combined study areas, plus the Sambaa K'e winter road, which will have to be added

## Load NWTBM_all_study_areas.shp (created in nwtbm_phd_general project - R/03_study_area_maps.R)
sa_sf <- st_read("NWTBM_all_study_areas.shp")

## Load Sambaa K'e winter road shapefile (sambaake_winter_road_shp.shp)
sk_wr_sf <- st_read("sambaake_winter_road_shp.shp") ## isolated in nwtbm_phd_general project - R/03b_ind_study_area_maps.R

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


## What is the total area of each study area? Given in m^2 - convert to km^2 (divide by 1 million) and convert to numeric
sa_areas <- cbind.data.frame(
              sa_sf2$study_area,
              as.numeric(st_area(sa_sf2)/1000000))
class(sa_areas[ ,2]) 
sa_areas[2]
colnames(sa_areas) <- c("study_area","area_sqkm") 
colnames(sa_areas)

## save areas as csv to add to with other summaries
write.csv(sa_areas, "study_area_summaries.csv")


### Add 20km buffers to each study area (generous buffer for spatial data extractions)
sa_sf2_buffer <- st_buffer(sa_sf2, dist = 20000)
plot(sa_sf2_buffer["study_area"]) # check that the buffered study area shapefile looks correct

### Save sa_sf2_buffer as shapefiles for later use in extracting spatial data around study areas
st_write(sa_sf2_buffer, "NWTBM_all_study_areas_20km_buffers.shp", delete_layer = TRUE)

