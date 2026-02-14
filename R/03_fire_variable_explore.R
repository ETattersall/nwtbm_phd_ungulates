####################################
## 05_fire_variable_explore.R
## Exploring spatial fire data
## Started on Jul 30 2025
## Created by Erin Tattersall
####################################


#### Environment set up ####
## Load required packages (should already be installed)

list.of.packages <- c("sf",
       "lwgeom",
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

## set working directory to file with NWT shapefiles
getwd()
list.files("data")
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWT_GIS_Data")
list.files()


#### Load NWT_boundary
nwt.boun <- st_read("NWT_boundary.shp")

## Convert to NWT Lambert Conformal Conic projection, which is  the GNWT Standard projection
st_crs(nwt.boun) #NAD 83
nwt.boun <- st_transform(nwt.boun, crs = 3580) # 3580 is the NWT Lambert
st_bbox(nwt.boun)


plot(nwt.boun)

### Load Thaidene Nene, SambaaK'e, Gameti, Edehzhie shapefiles, Fort Smith, Norman Wells polygons as a list ####
area_list <- list("ThaideneNene.shp", 
                  "Sambaa_Ke_protected_area.shp", 
                  "Gameti2023.shp", 
                  "Edehzhie.shp", 
                  "FortSmith2022_polygon.kml", 
                  "NormanWells2022_polygon.kml")

## Will need list of study area names later
area_names <- c("Thaidene Nëné", 
                 "Sambaa K'e", 
                 "Gamètì", 
                 "Edéhzhíe", 
                 "Fort Smith", 
                 "Norman Wells")

## Load polygons as sf objects
area_polys <- lapply(area_list, st_read)
class(area_polys[[1]]) # sf "data frame"


## Convert all to nwt Lambert projection for area calculations
area_polys_3580 <- lapply(area_polys, function(x) st_transform(x, crs = 3580))

## Add study area names to polygons
area_polys_3580 <- lapply(seq_along(area_polys_3580), function(i) {
  area_polys_3580[[i]]$name <- area_names[i] # add study area names
  return(area_polys_3580[[i]])
})


# Step 1: Extract only geometry and name from each polygon, including making geometry valid
area_polys_clean <- lapply(seq_along(area_polys_3580), function(i) {
  geom <- st_make_valid(st_geometry(area_polys_3580[[i]]))
  st_sf(name = area_names[i], geometry = geom)
})

# Step 2: Combine into a single sf object
area_sf <- do.call(rbind, area_polys_clean)


## Test plot
ggplot() +
  geom_sf(data = area_sf, fill = NA, color = "black") +
  geom_sf_text(data = area_sf, aes(label = name), size = 5, color = "blue")


### Save area polygons as shapefile
st_write(area_sf, "NWTBM_6StudyArea_polygons.shp", delete_dsn = TRUE) # overwrite existing file



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
