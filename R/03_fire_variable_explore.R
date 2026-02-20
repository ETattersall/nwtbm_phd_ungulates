####################################
## 03_fire_variable_explore.R
## Exploring spatial fire data
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
cam_loc_files <- list.files(pattern = "\\.csv$")
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
write.csv(cam_locs, "data/wt_location_data/all_projects_cam_locations_20260219.csv", row.names = FALSE)
table(cam_locs$study_area)


### Plot camera locations to check they look correct
cam_locs_sf <- st_as_sf(cam_locs, coords = c("longitude", "latitude"), crs = 4326) # convert to sf object with WGS 84 CRS
plot(cam_locs_sf["study_area"]) # plot camera locations colored by study area 

## Within each study area, calculate pairwise distances between locations (in meters) using st_distance function from sf package
cam_locs_sf <- st_transform(cam_locs_sf, crs = 3580) # transform to NWT Lambert Area projection for accurate distance calculations in meters


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

## Buffer size selections: could assume 100m, as minimal distance between stations, represents 4th order selection (i.e foraging patch)
## 500 m buffer is fairly standard, representing 3rd order selection (i.e. selecting habitat components in a home range), despite variance across species





#### Load Fire History data ####
## Canada Fire History data between 1972-2024 from NRCan: https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/nbac
## Also have NWT fire history data from GNWT website, but I think that only goes to 2023 (may be a more recent download?)
## Use NRCan data, since this is what Claudia also used
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/nrcan_nbac/NBAC_1972to2024_20250506_shp")
fire_history <- st_read("NBAC_1972to2024_20250506.shp")
head(fire_history)
## Check CRS
st_crs(fire_history) # NAD 83 - Canada Lambert Conformal Conic

summary(fire_history$YEAR) ## all years

## Transform to NWT Lambert and crop fire history data to NWT boundary 
fire_history <- st_transform(fire_history, crs = 3580)

## First filter fire_history for fires in NT (though this still includes fires in Nunavut prior to it becoming a separate territory in 1999)
nwt_fires <- fire_history %>%
  filter(ADMIN_AREA == "NT") # filter for fires in NT


## Then crop to NWT boundary
nwt_fires <- st_crop(nwt_fires, nwt.boun) ## Error from duplicate vertices in geometry ('invalid geometries)
st_is_valid(nwt.boun) # TRUE
table(st_is_valid(nwt_fires)) ## 1 FALSE geometry
invalid_fires <- nwt_fires[!st_is_valid(nwt_fires), ] # get invalid geometries



##plot invalid fire geometry - if it isn't in a study area, then it doesn't really matter
invalid_p <- ggplot() +
  geom_sf(data = invalid_fires, aes(color = YEAR), size = 3) + # fire polygons
  geom_sf(data = nwt.boun, fill = NA, linewidth = 2, color = "black") + # NWT boundary
  geom_sf(data = area_sf, fill = NA, linewidth = 1, color = "black") +
  labs(title = "Invalid Fire Geometry",
       x = "Longitude",
       y = "Latitude",
       color = "Fire Year") +
  theme(legend.position = "right") +
  theme_classic()

invalid_p ## fire is small and not anywhere close to a study area, so we can safely remove it

nwt_fires <- nwt_fires[st_is_valid(nwt_fires), ] # remove invalid geometries


## Now crop to NWT boundary
st_crs(nwt_fires)
st_crs(nwt.boun) ## confirmed both NWT Lambert Area projection
nwt_fires_boun <- st_crop(nwt_fires, nwt.boun)

## Remove nwt_fires from environment to save memory
rm(nwt_fires)


summary(nwt_fires_boun$YEAR) ## 1972-2024, median 2003
glimpse(nwt_fires_boun)

hist(nwt_fires_boun$YEAR) ## fairly uniform distribution of fire years
hist(nwt_fires_boun$POLY_HA) # total area of fire polygons in hectares using Canada Albers Equal Area projection (pre-calculated by NRCan)
hist(nwt_fires_boun$ADJ_HA) # adjusted area burned (see documentation for details)


## Save NWT fire history data
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/nrcan_nbac")

st_write(nwt_fires_boun, "nwt_fires_nrcan_1972_2024.shp", delete_dsn = TRUE) # overwrite existing file
## File saved with warnings - check (and re-read in because RStudio keeps crashing)
nwt_fires_boun <- st_read("nwt_fires_nrcan_1972_2024.shp")

plot(saved_nwtfires["YEAR"]) # plot fire polygons by year

## Plot fire year within study areas using ggplot, with red gradient for fire years
## Find bounding box to set limits for ggplot
st_bbox(nwt_fires_boun) #       xmin       ymin       xmax       ymax 
                          ##  -1020328.1  8121952.1   557220.8  9327478.7 


# Create label positions above each polygon
area_sf <- area_sf %>%
  mutate(label_pos = st_coordinates(st_point_on_surface(geometry))) %>%
  mutate(label_pos_y = label_pos[,2] + 80000,  # shift Y upward by 50,000 units
         label_pos_x = label_pos[,1] - 1000) # shift X left by 10,000 units

# Get ESRI basemap (e.g., "World_Imagery" or "World_Topo_Map")
basemap <- get_tiles(nwt.boun, provider = "Esri.WorldImagery", crop = TRUE, zoom = 8) # NWT boundary polygon used to get extent of basemap, zoom level can be adjusted
# note: higher resolution base imagery takes longer to download and display


win.graph() # open separate graphics window
gg_fire <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = nwt_fires_boun, aes(color = YEAR), size = 1.5) + # fire polygons
  geom_sf(data = nwt.boun, fill = NA, linewidth = 2, color = "black") + # NWT boundary
  geom_sf(data = area_sf, fill = NA, linewidth = 1, color = "black") + # add study area polygons
  geom_text(data = area_sf,
            aes(x = label_pos_x, y = label_pos_y, label = name),
            size = 6, color = "white") + # add study area names
  scale_color_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  labs(title = "Northwest Territories Fire History, 1972 - 2024",
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

gg_fire



## Save fire plot
ggsave("C:/Users/tatterer.stu/OneDrive - UBC/Documents/02.PhD/02.DataAnalysis/maps_figures/nwt_fire_history_1972_2024_basemap.png", plot = gg_fire, width = 10, height = 8, dpi = 300)



#### Fire history summary statistics ####
## Want to summarize fire size and fire age for all study areas

## Generate a 10 km buffer around all study area polygons to extract fire data around study areas (need to use fire history)
## will need to determine appropriate scales for buffer later
sa_list_10km_buffer <- st_buffer(area_sf, dist = 10000)
 
## Crop fire history data to 10 km buffer around study areas
sa_fires_10km <- st_intersection(fire_history, sa_list_10km_buffer)

## Remove fire_history from environment to save memory
#rm(fire_history)

## Also crop fire history data to study areas with no buffers (can just use nwt_fires_boun)
sa_fires <- st_intersection(nwt_fires_boun, area_sf)


win.graph()
gg_fire2 <- ggplot() +
  geom_sf(data = sa_fires_10km, aes(color = YEAR), size = 1.5) + # TDN fire polygons
  geom_sf(data = nwt.boun, fill = NA, linewidth = 2, color = "black") + # NWT boundary
  geom_sf(data = area_sf, fill = NA, linewidth = 1, color = "black") +
  scale_color_gradient(low = "lightyellow", high = "red") + # red gradient for more recent burns
  labs(title = "Northwest Territories Fire History, 1972 - 2024",
       x = "Longitude",
       y = "Latitude",
       color = "Fire Year") +
  theme(legend.position = "right") +
  coord_sf(xlim = c(-1026000, 580000), ylim = c(8100000, 9360000), expand = FALSE) + # set limits to bounding box of NWT fire layer (plus a little extra buffer)
  theme_classic()

gg_fire2


# Fires by study area, no buffer
gg_fire3 <- ggplot() +
  geom_sf(data = sa_fires, aes(color = YEAR), size = 1.5) + # TDN fire polygons
  geom_sf(data = nwt.boun, fill = NA, linewidth = 2, color = "black") + # NWT boundary
  geom_sf(data = area_sf, fill = NA, linewidth = 1, color = "black") +
  scale_color_gradient(low = "lightyellow", high = "red") + # red gradient for more recent burns
  labs(title = "Fire History by Study Area, 1972 - 2024",
       x = "Longitude",
       y = "Latitude",
       color = "Fire Year") +
  theme(legend.position = "right") +
  coord_sf(xlim = c(-1026000, 580000), ylim = c(8100000, 9360000), expand = FALSE) + # set limits to bounding box of NWT fire layer (plus a little extra buffer)
  theme_classic()

gg_fire3


### Generate a histogram of fire Years for each study area
glimpse(sa_fires)
class(sa_fires)

## Generate count of fires by year and study area
fire_years_combined <- sa_fires %>% 
  group_by(YEAR, name) %>% # group by year and study area
  summarise(Count = n(), .groups = 'drop') %>% # count number of fires in each year and study area
  rename(FireYear = YEAR, StudyArea = name) # rename columns

summary(fire_years_combined)

## Generate histograms of fire years, faceted by study area
win.graph()
gg_fire_years <- ggplot(fire_years_combined, aes(x = FireYear, y = Count, fill = StudyArea)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Fire History by Study Area",
       x = "Year",
       y = "Number of Fires") +
  #scale_fill_viridis_d() + # use viridis color scale
  theme_minimal() +
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(text= element_text(size = 16)) +
  facet_wrap(~ StudyArea, scales = "free_y") + # facet by study area
  theme(legend.position = "none")

gg_fire_years
## Save fire years plot
ggsave("C:/Users/tatterer.stu/OneDrive - UBC/Documents/02.PhD/02.DataAnalysis/maps_figures/nwt_fire_years_by_study_area.png", plot = gg_fire_years, width = 10, height = 8, dpi = 300)


### Will need to convert Fire Years to Fire Age, where the last year of data collection for each study area would be 0
Year0_by_sa <- cbind.data.frame(area_names, 
                                 Year0 = c(2024, 2023, 2024, 2023, 2022, 2023)) # last year of data collection for each study area
colnames(Year0_by_sa) <- c("StudyArea", "Year0") # rename columns

## Add Fire Age to fire_years_combined
fire_years_combined <- fire_years_combined %>%
  left_join(Year0_by_sa, by = "StudyArea") %>% # join by StudyArea
  mutate(FireAge = Year0 - FireYear) # calculate FireAge

summary(fire_years_combined$FireAge) # check FireAge values
## Remove negative FireAge values (since burn occurred after sensor data collection)
fire_years_combined <- fire_years_combined %>%
  filter(FireAge >= 0) # keep only positive FireAge values

## Fire Age Plot by Study Area
win.graph()
gg_burn_age <- ggplot(fire_years_combined, aes(x = FireAge, y = Count, fill = "salmon")) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Burn Age by Study Area",
       x = "Burn Age (Years Since Fire)",
       y = "Number of Burns") +
  theme_minimal() +
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(text= element_text(size = 24)) +
  facet_wrap(~ StudyArea, scales = "free_y") + # facet by study area
  theme(legend.position = "none")

gg_burn_age
## Save burn age plot
ggsave("C:/Users/tatterer.stu/OneDrive - UBC/Documents/02.PhD/02.DataAnalysis/maps_figures/nwt_burn_age_by_study_area_25aug2025.png", plot = gg_burn_age, width = 10, height = 8, dpi = 300)


## Get summary statistics for fire size (in hectares) by study area - Generate a histogram of fire Years for each study area

## Generate count of fires by year and study area
fire_size_combined <- sa_fires %>% 
  group_by(ADJ_HA, name) %>% # group by year and study area
  summarise(Count = n(), .groups = 'drop') %>% # count number of fires in each year and study area
  rename(FireSize = ADJ_HA, StudyArea = name) # rename columns


summary(fire_size_combined$FireSize) # check fire size values - between 0.12 - 828 060

## Bin fire size into categories for better visualization
fire_size_combined <- fire_size_combined %>%
  mutate(FireSizeCategory = case_when(
    FireSize < 10 ~ "0-10 ha",
    FireSize >= 10 & FireSize < 100 ~ "10-100 ha",
    FireSize >= 100 & FireSize < 1000 ~ "100-1000 ha",
    FireSize >= 1000 & FireSize < 10000 ~ "1000-10000 ha",
    FireSize >= 10000 & FireSize < 100000 ~ "10000-100000 ha",
    FireSize >= 100000 ~ "100000+ ha"
  ))

class(fire_size_combined$FireSizeCategory) # character
fire_size_combined$FireSizeCategory <- factor(fire_size_combined$FireSizeCategory, 
                                              levels = c("0-10 ha", "10-100 ha", "100-1000 ha", 
                                                         "1000-10000 ha", "10000-100000 ha", "100000+ ha")) # set order of categories
class(fire_size_combined$FireSizeCategory) # factor

### Convert FireSize from ha to km^2 (1 ha - 0.01 km^2)
summary(fire_size_combined$FireSize)
fire_size_combined$FireSize_km2 <- fire_size_combined$FireSize * 0.01
summary(fire_size_combined$FireSize_km2)
hist(fire_size_combined$FireSize_km2, breaks = 50) # most fires were small (left-skewed)

## Bin FireSize_km2 into categories for better visualization
fire_size_combined <- fire_size_combined %>%
  mutate(FireSizeCategory_km2 = case_when(
    FireSize_km2 < 1 ~ "0-1 km²",
    FireSize_km2 >= 1 & FireSize_km2 < 10 ~ "1-10 km²",
    FireSize_km2 >= 10 & FireSize_km2 < 100 ~ "10-100 km²",
    FireSize_km2 >= 100 & FireSize_km2 < 1000 ~ "100-1000 km²",
    FireSize_km2 >= 1000 ~ "1000+ km²"
  ))

## Generate summary statistics for fire size by study area
fire_size_summary <- fire_size_combined %>%
  group_by(StudyArea, FireSizeCategory_km2) %>%
  summarise(TotalBurns = sum(Count), .groups = 'drop') %>% # total number of burns in each category
  arrange(StudyArea, FireSizeCategory_km2) # sort by StudyArea and FireSizeCategory



## Generate histograms of fire size, faceted by study area
win.graph()
gg_fire_size <- ggplot(fire_size_summary, aes(x = FireSizeCategory_km2, y = TotalBurns, fill = "orange3")) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Burn Size by Study Area",
       x = "Burn Size (km²)",
       y = "Number of Burns") +
  theme_minimal() +
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20), axis) +
  theme(text= element_text(size = 24)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # rotate x-axis text for better readability
  facet_wrap(~ StudyArea, scales = "free_y") + # facet by study area
  theme(legend.position = "none") 

gg_fire_size
## Save fire size plot
ggsave("C:/Users/tatterer.stu/OneDrive - UBC/Documents/02.PhD/02.DataAnalysis/maps_figures/nwt_burn_size_by_study_area_25aug2025.png", plot = gg_fire_size, width = 10, height = 8, dpi = 300)




#### Are there NAs in YEAR or ADJ_HA for sa_fires_10km? ####
na_years <- lapply(sa_fires_10km, function(x) sum(is.na(x$YEAR))) # check for NAs in YEAR - none
na_ha <- lapply(sa_fires_10km, function(x) sum(is.na(x$ADJ_HA))) # check for NAs in ADJ_HA - none
