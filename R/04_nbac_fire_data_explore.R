####################################
## 04_fire_variable_explore.R
## Initial exploration of NBAC fire data for all camera locations
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


## Read in study area shapefiles - both regular and with 20km buffer (in study_area_spatial)
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/study_area_spatial")
list.files()
sa_poly <- st_read("NWTBM_all_study_areas.shp")
sa_20km <- st_read("NWTBM_all_study_areas_20km_buffers.shp")
crs(sa_poly)
crs(sa_20km) # same projection



#### Load Fire History data ####
## Canada Fire History data between 1972-2024 from NRCan: https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/nbac
## Also have NWT fire history data from GNWT website, but I think that only goes to 2023 (may be a more recent download?)
## Use NRCan data, since this is what Claudia also used
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/nrcan_nbac/NBAC_1972to2024_20250506_shp")
fire_history <- st_read("NBAC_1972to2024_20250506.shp")
head(fire_history)
## Check CRS
st_crs(fire_history) # NAD 83 - Canada Lambert Conformal Conic

summary(fire_history$YEAR) ## all years (1972-2024)

## Transform to NWT Lambert and crop fire history data to NWT boundary 
fire_history <- st_transform(fire_history, crs = 3580)

## First filter fire_history for fires in NT (though this still includes fires in Nunavut prior to it becoming a separate territory in 1999)
nwt_fires <- fire_history %>%
  filter(ADMIN_AREA == "NT") # filter for fires in NT

## save NWT fire data as a separate shapefile for faster loading in future
st_write(nwt_fires, "nwt_fires_1972to2024.shp")

### Extract fire data for 20km buffers. Use full fire history because a tiny corner of Fort Smith buffer is in Alberta (one FS station is ~900m from border)
sa_20km_fires <- st_intersection(fire_history, sa_20km)

## save 20km buffered fire data for later use
st_write(sa_20km_fires, "NBAC_fires_by_study_area_20kmbuffer.shp", append = FALSE)


## Calculate total area of study areas (will be needed later)
sa_poly$area_m2 <- st_area(sa_poly)


##### Extract fire data for study areas without buffers - not done yet ###
sa_fires_nobuffer <- st_intersection(sa_20km_fires, sa_poly)

## Remove fire_history from environment to save memory
rm(fire_history)

## return to base directory
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates")


## Create Year0 for sa_20km_fires and sa_poly (corresponding to LAST year of deployment)
## sa_20km_fires
sa_20km_fires <- sa_20km_fires %>%
  mutate(Year0 = case_when(
    str_detect(study_area, "Edéhzhíe") ~ "2022",
    str_detect(study_area, "FortSmith") ~ "2024",
    str_detect(study_area, "Gameti") ~ "2024",
    str_detect(study_area, "NormanWells") ~ "2024",
    str_detect(study_area, "SambaaK'e") ~ "2023",
    str_detect(study_area, "ThaideneNëné") ~ "2022",
    TRUE ~ NA_character_  # Default case if no match
  ))
class(sa_20km_fires$Year0)

sa_20km_fires$Year0 <- as.numeric(sa_20km_fires$Year0)
glimpse(sa_20km_fires)

sa_20km_fires$FireAge <- sa_20km_fires$Year0 - sa_20km_fires$YEAR
summary(sa_20km_fires$FireAge)
hist(sa_20km_fires$FireAge)

## how many (and which ones) are negative values?
neg.fire.age <- sa_20km_fires[sa_20km_fires$FireAge < 0, ]
table(neg.fire.age$study_area) # 14 Edehzhie, 14 ThaideneNene, 1 Sambaa K'e
## Can remove these since fire doesn't occur during deployment period


sa_20km_fires <- sa_20km_fires[!sa_20km_fires$FireAge < 0, ]
summary(sa_20km_fires$FireAge)

#### Summary statistics ####

## Extract fire data for study areas without buffers
sa_fires_nobuffer <- st_intersection(sa_20km_fires, sa_poly)
glimpse(sa_fires_nobuffer)


## Summarize fire ages and sizes an by study area
sa_fire_stats <- sa_fires_nobuffer %>% 
  group_by(study_area) %>% 
  summarise(f_age_min = min(FireAge),
            f_age_mean = mean(FireAge),
            f_age_max = max(FireAge),
            f_size_min = min(ADJ_HA),
            f_size_mean = mean(ADJ_HA),
            f_size_max = max(ADJ_HA)) %>%
  st_drop_geometry()  # drop geometry for easier joining

## Proportion burned/unburned within each study area
pburn_sa <- sa_fires_nobuffer %>%
  group_by(study_area) %>% ## group all polygons by study area
  summarise(geometry = st_union(geometry)) %>%  # combine all fire polygons within same study area
  mutate(burned_area_m2 = st_area(geometry)) %>% # calculate area of combined fire polygons in m^2
  st_drop_geometry()  # drop geometry for easier joining

## Add area of sa from sa_poly to pburn_sa
pburn_sa <- pburn_sa %>%
  left_join(
    sa_poly %>% 
      dplyr::select(study_area, area_m2),
    by = "study_area")

## Add proportion of burned area to each sa
pburn_sa$prop_burned <- pburn_sa$burned_area_m2/pburn_sa$area_m2

## Now add prop_burned to sa_fire_stats
sa_fire_stats <- left_join(sa_fire_stats, 
                           pburn_sa %>% 
                             select(study_area, prop_burned),
                           by = "study_area")

## Arrange from most prop. burned to least
sa_fire_stats <- sa_fire_stats %>% arrange(desc(prop_burned))

### Save summary stats
write.csv(sa_fire_stats,"figures/fire_explore/fire_stats_by_study_area.csv")

## Bar plot of proportion burned by study area
glimpse(sa_fire_stats)

## convert prop_burned to numeric
sa_fire_stats$prop_burned <- as.numeric(sa_fire_stats$prop_burned)
glimpse(sa_fire_stats)

bar_burned <- sa_fire_stats %>%
  ggplot(aes(x = study_area, y = prop_burned)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Proportion of Burned Area by Study Area",
       x = "Study Area",
       y = "Proportion of Burned Area") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16))

win.graph()
bar_burned
## save
ggsave("figures/fire_explore/propburned_by_study_area_202604027.png", bar_burned, width = 12, height = 8, dpi = 300)


## Faceted plot of Burn Age by Study Area (20km buffer)
fireage_sa <- sa_20km_fires %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  facet_wrap(~ study_area) +
  labs(title = "Time Since Fire by Study Area",
       x = "Burn Age",
       y = "Number of Fires") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fireage_sa
## save
ggsave("figures/fire_explore/fireages_byarea_20kbuffer_20260427.png", fireage_sa, width = 12, height = 8, dpi = 300)


### Fire Mapping: NWT and by Study Area ####

### Map of all NWT fire history plus sensor locations (including 500m buffers for visibility)
gg_nwt_fires <- ggplot() +
  geom_sf(data = nwt_fires, aes(color = YEAR), size = 0.5) + # all NWT fire polygons
  geom_sf(data = cams_500m_buffer, fill = NA, color = "blue", size = 0.5) + # 500m buffers around camera locations
  geom_sf(data = cam_locs_sf, color = "black", size = 1) + # camera locations
  scale_color_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  labs(title = "NWT Fire History (1972 - 2024) with Camera Locations",
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

gg_nwt_fires


### Faceted map of fire history for each study area

## Create each map separately (in a list), then plot together using plot_grid() (cowplot package)

## list of study areas
study_areas <- sa_poly$study_area

## Create a function to make one plot per study area
make_sa_plot <- function(sa) {
  
  # get bounding box for that study area (using sa_20km fires polygons)
  sa_bbox <- sa_20km_fires %>%
    dplyr::filter(study_area == sa) %>%
    summarise() %>%
    st_bbox()
  
  ggplot() +
    geom_sf(data = sa_20km_fires %>% filter(study_area == sa),
            aes(fill = FireAge), color = NA,
            size = 0.5) +
    
    geom_sf(data = cam_locs_sf %>% filter(study_area == sa),
            color = "black",
            size = 2) +
    
    scale_fill_gradient(low = "red", high = "yellow", name = "Time Since Fire") +
    
    coord_sf(xlim = c(sa_bbox["xmin"], sa_bbox["xmax"]),
             ylim = c(sa_bbox["ymin"], sa_bbox["ymax"]),
             expand = FALSE) +
    
    labs(title = sa,
         x = "Longitude",
         y = "Latitude") +
    
    theme_classic() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "right"
    )
}


## Create list of ggplots
plot_list <- lapply(study_areas, make_sa_plot)


combined_plot <- plot_grid(plotlist = plot_list,
                           ncol = 3)   # adjust columns as needed

win.graph()
combined_plot


## Save the plot
save_plot(
  "figures/fire_explore/TimeSinceFire_bySA.jpeg",
  combined_plot,
  ncol = 3,
  nrow = 2,
  base_asp = 1.618,
  dpi = 300
)

#### Is there a relationship between fire age and fire size at the study area level?

fire_age_size <- sa_20km_fires %>% 
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge, y = ADJ_HA)) +
  geom_point() +
  labs(title = "Relationship between Fire Age and Fire Size",
       x = "Fire Age",
       y = "Fire Size (HA)") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fire_age_size

## add regression line with 95% confidence intervals to fire_age_size
fire_age_size <- fire_age_size + geom_smooth(method = "lm", se = TRUE, color = "purple")
fire_age_size

## save
ggsave("figures/fire_explore/fire_age_size_relationship_bystudyarea_202604027.png", fire_age_size, width = 12, height = 8, dpi = 300)

## Assessing relationship between fire age and fire size with a linear model
lm(ADJ_HA ~ FireAge, data = sa_20km_fires) %>% summary() ## Low R-squared and non-significant relationship



##### Fire data buffered by site ####

## Read in site polygons and 500m site buffers from R/03_location_data.R
site_polygons <- st_read("data/wt_location_data/nwtbm_cam_sites.gpkg")
sites_500m <- st_read("data/wt_location_data/nwtbm_cam_sites_500mbuffer.gpkg")

glimpse(sites_500m) ## area of 500m site buffer already calculated in sq. m and sq. km

## Then extract fire for site buffer areas (using sa_20km_fires)
fires_500m_buffer <- st_intersection(sa_20km_fires, sites_500m)

glimpse(fires_500m_buffer) ## 116 rows - not all sites contain fire data (expected)
crs(fires_500m_buffer) # check CRS of fire data within buffers


## Quick summary of fire years represented
summary(fires_500m_buffer$YEAR) ## 1972-2023, median 1995
hist(fires_500m_buffer$YEAR) #roughly normal distribution
hist(fires_500m_buffer$POLY_HA) ## area burned calculated per burn, not at site polygon level (see documentation for details, and Skakun et al. 2021: https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/NBAC_1972to2024_20250506_shp_metadata.pdf)
hist(fires_500m_buffer$ADJ_HA) # adjusted area burned (identical to above)

plot(fires_500m_buffer["YEAR"]) # map of fire years within 500m buffer around camera locations

## Not all sensor locations have fire history data within 100m or 500m buffers. Some locations have multiple fire polygons within the buffers
length(unique(fires_500m_buffer$site)) # 94 out of 172 sites have fire history data within 500m buffer

## Where fire geometries overlap, or one is contained within another, the most recent should be kept
## Check for polygon overlap or containment
fires_intersect <- st_intersects(fires_500m_buffer)
any(lengths(fires_intersect) > 1) #TRUE - there are polygons that overlap others 
sum(lengths(fires_intersect) > 1) ##46 geometries intersect

## Plotting intersecting polygons
plot(st_geometry(fires_500m_buffer), border = 'grey')
plot(
  st_geometry(fires_500m_buffer[lengths(fires_intersect) > 1, ]),
  col = 'red', add = TRUE
)


# 1. Sort: most recent FIRST
fires_sorted <- fires_500m_buffer %>%
  arrange(FireAge) %>%
  mutate(id = row_number())

# 2. Iteratively remove overlaps with newer fires
clean_geoms <- vector("list", nrow(fires_sorted))

for (i in seq_len(nrow(fires_sorted))) {
  
  current_geom <- fires_sorted$geometry[i]
  
  if (i > 1) {
    # union of ALL more recent fires
    newer_union <- st_union(fires_sorted$geometry[1:(i - 1)])
    
    # remove any overlap
    current_geom <- st_difference(current_geom, newer_union)
  }
  
  clean_geoms[[i]] <- current_geom[[1]]
}

# 3. Rebuild sf object
fires_clean <- fires_sorted %>%
  mutate(geometry = st_sfc(clean_geoms, crs = st_crs(fires_500m_buffer))) %>%
  filter(!st_is_empty(geometry))

## Rename to keep 500m buffer in name
fires_500m_cleaned <- fires_clean

## What is the proportion of burned/unburned area around camera sites? Stick to 500m buffer for simplicity
# For 500m around each camera site, calculate total area of fire polygons (in hectares) and divide by total area of 500m buffer (in m^2) to get proportion of burned area within 500m buffer
# total area of 500m buffer in sq m saved in site_area column


## For each unique location in fires_500m_buffer, calculate total area of all fire polygons within 500m buffers
## Result should be a spatial data frame with one row per unique location, and columns for proportion burned area within 500m buffers

#### Proportion of burned area within 500m buffer ####
burned_area_500m <- fires_500m_cleaned %>% 
  group_by(study_area, site, site_area) %>% ## group all polygons with same site and study area together (keep site_area too)
  summarise(geometry = st_union(geometry)) %>% # combine all fire polygons from the same location
  mutate(burned_area_m2 = st_area(geometry)) %>% # calculate area of combined fire polygons in m^2
  mutate(proportion_burned_500m = as.numeric(burned_area_m2/site_area)) %>%  # calculate proportion of burned area within 500m buffer (convert to numeric to avoid units issues
  st_drop_geometry() # drop geometry for easier joining with sites_polygons

str(burned_area_500m$site) 
str(site_polygons$site)

## add proportion_burned_m2 to site_polygons for 500m buffers, adding a 0 value for locations with no fire history data within the buffer
cam_sites_burnedprop <- site_polygons %>%
  left_join(
    burned_area_500m %>% 
      select(study_area, site, proportion_burned_500m),
    by = c("study_area", "site") ## join 500m burn by site to match fire data to camera sites
  ) %>%
  mutate(proportion_burned_500m = ifelse(is.na(proportion_burned_500m), 0, proportion_burned_500m)) ## replace NA values with 0 for sites with no fire history data within the buffer

glimpse(cam_sites_burnedprop) ## check that the new columns have been added correctly
hist(cam_sites_burnedprop$proportion_burned_500m) ## bimodal distribution - many sites (>80) with 0 burn, and some up to 100%
class(cam_sites_burnedprop) # sf object with 167 rows and 6 columns (including geometry)

## Histogram of proportion of burned area within 500m buffers - proportion burned on the x-axis (binned to 0.1 intervals), frequency on the y-axis

hist_burned_500m <- cam_sites_burnedprop %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = proportion_burned_500m)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") + ## Binned to 0.1 intervals
  labs(title = "Distribution of Proportion of Burned Area within 500m Buffer of Camera Sites",
       x = "Proportion of Burned Area",
       y = "Count of Sites") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_burned_500m
## save
ggsave("figures/fire_explore/propburned_sites_500mbuffer_20260512.png", hist_burned_500m, width = 12, height = 8, dpi = 300)

## facet by study area

#500m buffer
hist_burned_sa_500m <- cam_sites_burnedprop %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = proportion_burned_500m)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") + ## Binned to 0.1 intervals
  facet_wrap(~ study_area) +
  labs(title = "Distribution of Proportion of Burned Area within 500m Buffer of Camera Sites",
       x = "Proportion of Burned Area",
       y = "Count of Sites") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_burned_sa_500m
## save
ggsave("figures/fire_explore/propburned_sites_500mbuffer_studyarea_20260512.png", hist_burned_sa_500m, width = 12, height = 8, dpi = 300)

## Proportion of burned/unburned areas around cameras has a biphasic distribution with peaks at 0 (unburned) and 1 (fully burned)
## Removing some of the older fires (e.g., considering them unburned after a certain number of years) would decrease proportion of areas considered burned, which might help even out the distribution
## Could assume that after a certain number of years post-fire, the area would be regenerated to either deciduous shrub/mixedwood or deciduous forest (i.e., remove fire polygons prior to that time)

#### Fire Age at each site ####
## Faceted plot of Fire Age for 500m buffer
fireage_500 <- fires_500m_cleaned %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  facet_wrap(~ study_area) +
  labs(title = "Burn Age by Study Area (500m buffer)",
       x = "Burn Age",
       y = "Count of Sites") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fireage_500
## save
ggsave("figures/fire_explore/fireages_byarea_sites_500mbuffer_20260512.png", fireage_500, width = 12, height = 8, dpi = 300)

## Not faceted
fireage_all_500 <- fires_500m_cleaned %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  labs(title = "Burn Age (500m buffer)",
       x = "Burn Age",
       y = "Count of Sites") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fireage_all_500
## save
ggsave("figures/fire_explore/fireages_sites_500mbuffer_20260512.png", fireage_all_500, width = 12, height = 8, dpi = 300)


## Some sites have multiple ages represented within their polygon (duplicated sites in fires_500m_cleaned)
## For each site with multiple fires, summarize FireAge and proportion of the burn for each fire at that site
multifires <- fires_500m_cleaned %>%
  group_by(site) %>%
  filter(n()>1) %>%
  select(study_area, site, site_area, NFIREID, YEAR, FireAge) %>% 
  mutate(burned_area_m2 = st_area(geometry)) %>% # calculate area of combined fire polygons in m^2
  mutate(proportion_burned_500m = as.numeric(burned_area_m2/site_area)) %>%  # calculate proportion of burned area within 500m buffer (convert to numeric to avoid units issues
  arrange(site, FireAge) %>% 
  ungroup()

## Plot Year by proportion for each site
multifire_sites <- multifires %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge, y = proportion_burned_500m)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  facet_wrap(~ site) +
  labs(x = "Burn Age",
       y = "Proportion burned") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
multifire_sites 
## Not sure how to account for these in modelling yet

#### Map out sites with multiple fires
## Edehzhie
ede_multifires <- ggplot() +
    geom_sf(data = multifires %>% filter(study_area == "Edéhzhíe"),
            aes(fill = FireAge), color = NA,
            size = 0.5) +
    
    scale_fill_gradient(low = "red", high = "yellow", name = "Burn Age") +
    labs(title = "Edéhzhíe",
         x = "Longitude",
         y = "Latitude") +
    
    theme_classic() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "right"
    )

win.graph()
ede_multifires

## Fort Smith
fs_multifires <- ggplot() +
  geom_sf(data = multifires %>% filter(study_area == "FortSmith"),
          aes(fill = FireAge), color = NA,
          size = 0.5) +
  
  scale_fill_gradient(low = "red", high = "yellow", name = "Burn Age") +
  labs(title = "FortSmith",
       x = "Longitude",
       y = "Latitude") +
  
  theme_classic() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

win.graph()
fs_multifires

## Sambaa K'e
sk_multifires <- ggplot() +
  geom_sf(data = multifires %>% filter(study_area == "SambaaK'e"),
          aes(fill = FireAge), color = NA,
          size = 0.5) +
  
  scale_fill_gradient(low = "red", high = "yellow", name = "Burn Age") +
  labs(title = "SambaaK'e",
       x = "Longitude",
       y = "Latitude") +
  
  theme_classic() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

win.graph()
sk_multifires ## only 2 sites - and it is mostly newer fire (2023) so could probably ignore the effect of the 2013 fire...

## Gameti
gam_multifires <- ggplot() +
  geom_sf(data = multifires %>% filter(study_area == "Gameti"),
          aes(fill = FireAge), color = NA,
          size = 0.5) +
  
  scale_fill_gradient(low = "red", high = "yellow", name = "Burn Age") +
  labs(title = "Gameti",
       x = "Longitude",
       y = "Latitude") +
  
  theme_classic() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

win.graph()
gam_multifires ## BMS-CRU-183 is mostly 2014 fire, BMS-CRU-174 is mostly 2012 fire. BMS-CRU-089 is 25% 2014, 43% 1979 

## What is the difference in proportions in sites with multiple fires?
diff_by_site <- multifires %>%
  group_by(site) %>%
  summarise(
    max_prop = max(proportion_burned_500m, na.rm = TRUE),
    min_prop = min(proportion_burned_500m, na.rm = TRUE),
    diff_prop = max_prop - min_prop,
    max_year = max(FireAge, na.rm = TRUE),
    min_year = min(FireAge, na.rm = TRUE),
    diff_year = max_year - min_year,
    .groups = "drop"
  )
summary(diff_by_site$diff_prop)
win.graph()
hist(diff_by_site$diff_prop)
## 19 sites total, 5 sites have a difference < 0.2. Small enough proportion of total sites (172) that I could just choose the age of the fire with the greater proportion
hist(diff_by_site$diff_year)

######## NEED TO MAKE A CALL ON HOW TO ASSIGN FIRE AGE WITHIN THESE BUFFERS ########
## Save cam_sites_burnedprop in meantime (no fire age data) and fires_500m_cleaned (fire data for site with fire)
write.csv(cam_sites_burnedprop, "data/nrcan_nbac/propburned_cam_sites500m_20260513.csv")
write.csv(fires_500m_cleaned, "data/nrcan_nbac/nbac_firedata_cam_sites500m_20260513.csv")

##### Fire data buffered by station ####

## Read in site polygons and 500m site buffers from R/03_location_data.R
cam_locs_sf <- st_read("data/wt_location_data/nwtbm_cam_locations_20260506.gpkg")
cam_locs_500m <- st_read("data/wt_location_data/nwtbm_cam_locations_500mbuffer.gpkg")

glimpse(cam_locs_500m) ## area of 500m site buffer already calculated in sq. m and sq. km

## Then extract fire for site buffer areas (using sa_20km_fires)
stn_fires500 <- st_intersection(sa_20km_fires, cam_locs_500m)

glimpse(stn_fires500) ## 453 rows - not all sites contain fire data (expected)
crs(stn_fires500) # check CRS of fire data within buffers


## Quick summary of fire years represented
summary(stn_fires500$YEAR) ## 1972-2023, median 1995
hist(stn_fires500$YEAR) #roughly normal distribution (VERY roughly...)

## Not all sensor locations have fire history data within 100m or 500m buffers. Some locations have multiple fire polygons within the buffers
length(unique(stn_fires500$location)) # 373 out of 730 locations have fire history data within 500m buffer

## Where fire geometries overlap, or one is contained within another, the most recent should be kept
## Check for polygon overlap or containment
stn_fires_intersect <- st_intersects(stn_fires500)
any(lengths(stn_fires_intersect) > 1) #TRUE - there are polygons that overlap others 
sum(lengths(stn_fires_intersect) > 1) ## 439 geometries intersect

## Plotting intersecting polygons
win.graph()
plot(st_geometry(stn_fires500), border = 'grey')
plot(
  st_geometry(stn_fires500[lengths(stn_fires_intersect) > 1, ]),
  col = 'red', add = TRUE
)


# 1. Sort: most recent FIRST
stn_fires_sorted <- stn_fires500 %>%
  arrange(FireAge) %>%
  mutate(id = row_number())

# 2. Iteratively remove overlaps with newer fires

clean_geoms <- vector("list", nrow(stn_fires_sorted))

for (i in seq_len(nrow(stn_fires_sorted))) {
  
  current_geom <- stn_fires_sorted$geometry[i]
  
  if (i > 1) {
    newer_union <- st_union(stn_fires_sorted$geometry[1:(i - 1)])
    current_geom <- st_difference(current_geom, newer_union)
  }
  
  clean_geoms[[i]] <- current_geom
}

fires_clean <- stn_fires_sorted
fires_clean$geometry <- st_sfc(
  do.call(c, clean_geoms),  # safely flatten
  crs = st_crs(stn_fires500)
)

stn_fires_clean <- fires_clean[!st_is_empty(fires_clean), ]


## Rename to keep 500m buffer in name
stn_fires500_cleaned <- stn_fires_clean

## What is the proportion of burned/unburned area around camera locations?
# For 500m around each camera location, calculate total area of fire polygons (in hectares) and divide by total area of 500m buffer (in m^2) to get proportion of burned area within 500m buffer
# total area of 500m buffer in sq m = 500^2*pi

## For each unique location in fires_500m_buffer, calculate total area of all fire polygons within 500m buffers
## Result should be a spatial data frame with one row per unique location, and columns for proportion burned area within 500m buffers

#### Proportion of burned area within 500m buffer ####
burned_area_stn500 <- stn_fires500 %>% 
  group_by(study_area, location) %>% ## group all polygons with same site and study area together (keep site_area too)
  summarise(geometry = st_union(geometry)) %>% # combine all fire polygons from the same location
  mutate(burned_area_m2 = st_area(geometry)) %>% # calculate area of combined fire polygons in m^2
  mutate(proportion_burned_500m = as.numeric(burned_area_m2/((500^2)*pi))) %>%  # calculate proportion of burned area within 500m buffer (convert to numeric to avoid units issues)
  st_drop_geometry() # drop geometry for easier joining with sites_polygons



## add proportion_burned_m2 to cam_locs for 500m buffers, adding a 0 value for locations with no fire history data within the buffer
cam_stns_burnedprop <- cam_locs_sf %>%
  left_join(
    burned_area_stn500 %>% 
      select(study_area, location, proportion_burned_500m),
    by = c("study_area", "location") ## join 500m burn by site to match fire data to camera sites
  ) %>%
  mutate(proportion_burned_500m = ifelse(is.na(proportion_burned_500m), 0, proportion_burned_500m)) ## replace NA values with 0 for sites with no fire history data within the buffer

glimpse(cam_stns_burnedprop) ## check that the new columns have been added correctly
hist(cam_stns_burnedprop$proportion_burned_500m) ## zero-inflated distribution - majority sites (>80) with 0 burn, and some up to 100%
class(cam_stns_burnedprop) # sf object with 731 rows and 6 columns (including geometry)


## Histogram of proportion of burned area within 500m buffers - proportion burned on the x-axis (binned to 0.1 intervals), frequency on the y-axis

hist_burned_500m <- cam_stns_burnedprop %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = proportion_burned_500m)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") + ## Binned to 0.1 intervals
  labs(title = "Distribution of Proportion of Burned Area within 500m Buffer of Camera Locations",
       x = "Proportion of Burned Area",
       y = "Count of Locations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_burned_500m
## save
ggsave("figures/fire_explore/propburned_stations_500mbuffer_20260513.png", hist_burned_500m, width = 12, height = 8, dpi = 300)

## facet by study area

#500m buffer
hist_burned_sa_500m <- cam_stns_burnedprop %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = proportion_burned_500m)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") + ## Binned to 0.1 intervals
  facet_wrap(~ study_area) +
  labs(title = "Distribution of Proportion of Burned Area within 500m Buffer of Camera Locations",
       x = "Proportion of Burned Area",
       y = "Count of Locations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))


hist_burned_sa_500m
## save
ggsave("figures/fire_explore/propburned_stations_500mbuffer_studyarea_20260513.png", hist_burned_sa_500m, width = 12, height = 8, dpi = 300)

## Proportion of burned/unburned areas around cameras has a biphasic distribution with peaks at 0 (unburned) and 1 (fully burned)

#### Fire Age at each site ####
## Faceted plot of Fire Age for 500m buffer
fireage_500 <- stn_fires500_cleaned %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  facet_wrap(~ study_area) +
  labs(title = "Burn Age by Study Area (500m buffer)",
       x = "Burn Age",
       y = "Count of Locations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fireage_500
## save
ggsave("figures/fire_explore/fireages_byarea_stations_500mbuffer_20260513.png", fireage_500, width = 12, height = 8, dpi = 300)

## Not faceted
fireage_all_500 <- stn_fires500_cleaned %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  labs(title = "Burn Age (500m buffer)",
       x = "Burn Age",
       y = "Count of Locations") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

fireage_all_500
## save
ggsave("figures/fire_explore/fireages_stations_500mbuffer_20260513.png", fireage_all_500, width = 12, height = 8, dpi = 300)


## Some locations have multiple ages represented within their polygon (duplicated sites in stns_fires500_cleaned)
## For each site with multiple fires, summarize FireAge and proportion of the burn for each fire at that location
stn_multifires <- stn_fires500_cleaned %>%
  group_by(location) %>%
  filter(n()>1) %>%
  select(study_area, site, location, NFIREID, YEAR, FireAge) %>% 
  mutate(burned_area_m2 = st_area(geometry)) %>% # calculate area of combined fire polygons in m^2
  mutate(proportion_burned_500m = as.numeric(burned_area_m2/((500^2)*pi))) %>%  # calculate proportion of burned area within 500m buffer (convert to numeric to avoid units issues
  arrange(site, location, FireAge) %>% 
  ungroup()

## What is the difference in proportions in locations with multiple fires?
diff_by_stn <- stn_multifires %>%
  group_by(location) %>%
  summarise(
    max_prop = max(proportion_burned_500m, na.rm = TRUE),
    min_prop = min(proportion_burned_500m, na.rm = TRUE),
    diff_prop = max_prop - min_prop,
    max_year = max(FireAge, na.rm = TRUE),
    min_year = min(FireAge, na.rm = TRUE),
    diff_year = max_year - min_year,
    .groups = "drop"
  )
summary(diff_by_stn$diff_prop)
win.graph()
hist(diff_by_stn$diff_prop) ## About 20 stations with <0.10 difference in proportions between fires, 30 with <0.20

hist(diff_by_stn$diff_year)

######## NEED TO MAKE A CALL ON HOW TO ASSIGN FIRE AGE WITHIN THESE BUFFERS ########
## Save cam_sites_burnedprop in meantime (no fire age data) and fires_500m_cleaned (fire data for site with fire)
write.csv(cam_stns_burnedprop, "data/nrcan_nbac/propburned_cam_locations500_20260513.csv")
write.csv(stn_fires500_cleaned, "data/nrcan_nbac/nbac_firedata_cam_locations500_20260513.csv")
