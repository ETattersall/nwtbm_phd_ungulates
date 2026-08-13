#############################################
## 04_ind_det_response.R
## Downloading and summarizing into no. of independent detections
## for all NWTBMP projects
## Started on August 12 2026
## Created by Erin Tattersall
#############################################


list.of.packages <- c("tidyverse", "sf", "maptiles", "ggspatial", "terra","kableExtra", "leaflet", "viridis", "corrplot", "lubridate", "plotly", "ggplot2", "ggbreak")
lapply(list.of.packages, require, character.only = TRUE)

## Needed to download station locations
library(wildrtrax)

packageVersion("wildrtrax")

## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()


## Get project information for my WildTrax projects
cam_projects <- wt_get_projects("CAM")


## Filter to my target projects only, using project IDs: 712 (Thaidene Nene), 2183 (Fort Smith), 2102 (Norman Wells), 1906 (Sambaa K'e), 2935 (Gameti), 1465 (Edehzhie)
cam_projects <- cam_projects %>% filter(project_id == "712" |
                                          project_id == "2183" |
                                          project_id == "2102" |
                                          project_id == "1906" |
                                          project_id == "2935" |
                                          project_id == "1465")

## Likely won't be able to download all 6 at same time - try downloading 2 batches of 3
cam_projects$project_id[1:3]

## Download raw data for 3 projects
raw_data1 <- wt_download_report(project_id = cam_projects$project_id[1:3],
                                  sensor_id = "CAM",
                                  report = "main") # main reports include ALL DATA

## Download raw data for next 3 projects
raw_data2 <- wt_download_report(project_id = cam_projects$project_id[4:6],
                                sensor_id = "CAM",
                                report = "main") # main reports include ALL DATA

## results in lists of dataframes - rename each df (will pull names as study_area later). Check objects for order - not in same order as above!
names(raw_data1) <- c("NormanWells", "FortSmith", "ThaideneNëné")
names(raw_data2) <- c("Edéhzhíe", "SambaaK'e", "Gameti")

glimpse(raw_data2$Gameti)

## Combine the lists, then bind those dfs and add df names in a study_area column (.id argument)
raw_data <- c(raw_data1,raw_data2)
raw_data_df <- bind_rows(raw_data, .id = "study_area")
glimpse(raw_data_df)

## How many total locations?
length(unique(raw_data_df$location)) # 732
unique(raw_data_df$location)

## Location names need to be standardized and coordinates added
stn_lookup <- read.csv("data/nwtbm_station_name_lookup_table.csv")

glimpse(stn_lookup)
length(unique(stn_lookup$location_std))

## Remove column X and study_area (already added to wt data above)
stn_lookup <- stn_lookup %>% select(-X, -study_area)

## Remove duplicate rows
stn_lookup <- distinct(stn_lookup)

## Load location data to add station coordinates
cam_locs <- read.csv("data/wt_location_data/nwtbm_cam_locations_20260506.csv")
glimpse(cam_locs)

## Remove column X
cam_locs <- cam_locs %>% select(-X)

## Join lookup to raw_data then convert location to location_std
std_data_df <- raw_data_df |> 
  left_join(stn_lookup, by = c("location" = "location_wt")) |> 
  mutate(location = location_std) |>  #converting wt station names to standardized names
  select(-location_std) # removing location_std column from lookup

length(unique(std_data_df$location)) #717 
glimpse(std_data_df)

## Which stations are in std_data_df and not in cam_locs (15?)?
std_data_locs <- unique(std_data_df$location)
missing_locs <- setdiff(cam_locs$location, std_data_locs)
missing_locs # Looks like some of the missing locations might be because the lookup table is inaccurate for the WildTrax Fort Smith stations deployed by FSMC (suffix dropped before WT names recorded maybe?)
## Ideally I'd go back to 02_merge_station_locations.R and fix there, but since it appears to only be the FSMC camera sites, I will fix the WildTrax location names manually in the station lookup table

## Re-load new lookup table
stn_lookup <- read.csv("data/nwtbm_station_name_lookup_table_20260812.csv")

glimpse(stn_lookup)
length(unique(stn_lookup$location_std))

## Remove column X and study_area (already added to wt data above)
stn_lookup <- stn_lookup %>% select(-X, -study_area)

## Remove duplicate rows
stn_lookup <- distinct(stn_lookup)

## Join lookup to raw_data then convert location to location_std
std_data_df <- raw_data_df |> 
  left_join(stn_lookup, by = c("location" = "location_wt")) |> 
  mutate(location = location_std) |>  #converting wt station names to standardized names
  select(-location_std) # removing location_std column from lookup

length(unique(std_data_df$location)) #732 - all locations now represented in standard data 

## Replace lat long in std_data_df with coords from cam_locs. Add other columns from cam_locs too (except study_area)
std_data_df <- std_data_df %>%
  select(-latitude, -longitude) %>%   # remove incorrect coords
  left_join(cam_locs %>% 
              select(-study_area), # study_area column already exists
            by = "location")

glimpse(std_data_df)


### Create independent detections from camera data, with a standard threshold of 30 minutes
cam_det <- wt_ind_detect(std_data_df,
                        threshold = 30,
                        units = "minutes",
                        remove_human = TRUE) # removes humans and vehicles
glimpse(cam_det)
unique(cam_det$species_common_name) #106 different species
length(unique(cam_det$location)) #717 - so not all 732 locations in cam_locs. What is the difference?

## Which stations are in cam_locs and not in cam_det?
cam_det_locs <- unique(cam_det$location)
missing_det_locs <- setdiff(cam_locs$location, cam_det_locs)
missing_det_locs ## Cross-checked all these locations with data on WT - none of them have wildlife detections, so makes sense they would not appear in detection df (will need to be added as no data)


## Add location coordinates to detection data
cam_det <- cam_det %>% 
  left_join(cam_locs, by = "location")
glimpse(cam_det)

## Save std_data_df and cam_det
write.csv(std_data_df, "data/camera_data/nwtbm_allprojects_camera_tags.csv")
write.csv(cam_det, "data/camera_data/nwtbm_allprojects_camera_detections_30min.csv")

## Generate number of detections per station by month for ungulates
ung_spp <- c("Barren-ground Caribou", "Bison", "Moose", "Muskox", "Woodland Caribou")

ung_data <- cam_det |> filter(species_common_name %in% ung_spp)
glimpse(ung_data)
class(ung_data$start_time)




#### 1. Plot total detections of all species detected ####
spp_count <- cam_det %>% 
  group_by(study_area, species_common_name) %>% # group by study area so these can be plotted separately
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% ## descending order of detections
  ungroup()

## Faceting is messy, so generate each study area plot separately. Edehzhie and Sambaa K'e have already been done in single project scripts

### TDN Plot
spp_count_tdn <- spp_count |> filter(study_area == "ThaideneNëné")
tdn_det <- ggplot(spp_count_tdn,
                   aes(x = count, y = fct_reorder(species_common_name, count))) + # re-orders species into descending count
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Total Species Detections, Thaidene Nëné 2021-2022",
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
tdn_det

## Save plot
ggsave("figures/ThaideneNene_allspecies_detections_2021-2022.png", tdn_det, width = 18, height = 12, dpi = 300)


### Fort Smith
spp_count_fs <- spp_count |> filter(study_area == "FortSmith")
fs_det <- ggplot(spp_count_fs,
                  aes(x = count, y = fct_reorder(species_common_name, count))) + # re-orders species into descending count
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Total Species Detections, Fort Smith 2022-2024",
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
fs_det

## Save plot
ggsave("figures/FortSmith_allspecies_detections_2022-2024.png", fs_det, width = 18, height = 12, dpi = 300)


### Norman Wells
spp_count_nw <- spp_count |> filter(study_area == "NormanWells")
nw_det <- ggplot(spp_count_nw,
                  aes(x = count, y = fct_reorder(species_common_name, count))) + # re-orders species into descending count
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Total Species Detections, Norman Wells 2022-2023",
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
nw_det

## Save plot
ggsave("figures/NormanWells_allspecies_detections_2022-2023.png", nw_det, width = 18, height = 12, dpi = 300)

### Gameti
spp_count_gam <- spp_count |> filter(study_area == "Gameti")
gam_det <- ggplot(spp_count_gam,
                  aes(x = count, y = fct_reorder(species_common_name, count))) + # re-orders species into descending count
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Total Species Detections, Gameti 2023-2024",
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
gam_det

## Save plot
ggsave("figures/Gameti_allspecies_detections_2023-2024.png", gam_det, width = 18, height = 12, dpi = 300)

#### Total detections for ungulates only
ung_spp <- c("Barren-ground Caribou", "Bison", "Moose", "Muskox", "Woodland Caribou")

ung_count <- spp_count |> filter(species_common_name %in% ung_spp)

## Faceted plot for all study areas (keep axis fixed for comparison)
ung_det <- ggplot(ung_count,
                  aes(x = count, y = fct_reorder(species_common_name, count))) + # re-orders species into descending count
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  facet_wrap(~study_area) +
  labs(
    title = "Total Ungulate Detections",
    x = "Independent Detections (30 min.)",
    y = NULL) + # removes y-axis title
  scale_x_continuous(breaks = c(0, 200, 400, 600, 2800)) + # define x-axis ticks
  scale_x_break(c(650, 2700)) + ## add x-axis break
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme( #remove top axis
    axis.text.x.top = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.title.x.top = element_blank())


win.graph()
ung_det

## Save ungulate plot
ggsave(ung_det, "figures/nwtbmp_allprojects_ungulate_detections.png")


#### 2. Naive occupancy ####
length(unique(cam_det$location)) ## 15 locations with no detections missing, will need to be added

## Create a site by species detection matrix for all sampled locations
stn_species_cams <- cam_det %>%
  distinct(study_area, location, species_common_name) %>% # get unique combinations of study area, location and species tags
  mutate(detection = 1L) %>% # assign a detection value of 1 for each location-species combination (L for integer)
  pivot_wider(names_from = species_common_name, values_from = detection, values_fill = 0L)

## Create empty data frame for missing locations - 15 rows, 108 columns
# Missing locations with their study areas
no_spp_locs <- cam_locs |> 
  filter(location %in% missing_det_locs) |> 
  select(study_area, location)

no_det <- as.data.frame(matrix(nrow = nrow(no_spp_locs), # same number of rows as missing locations
                               ncol = ncol(stn_species_cams))) # same columns as stn_species_cams
colnames(no_det) <- colnames(stn_species_cams) # match column names
# Add study area and location data
no_det$study_area <- no_spp_locs$study_area
no_det$location <- no_spp_locs$location
# Populate remaining columns with 0s (no detections of any wildlife)
no_det[3:108] <- 0
summary(no_det)

## Add to stn_species_cams and sort by study_area, location
stn_species_cams <- bind_rows(stn_species_cams, no_det) |> 
                      arrange(study_area, location)

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

## Remove rows with 0 occupancy
spp_naive_summary <- spp_naive_summary |> filter(naive_occupancy > 0)

## Plot naive occupancy for all study areas separately (Ede and SK plots didn't include missing sites)

### Edehzhie
spp_naiocc_ede <- spp_naive_summary |> filter(study_area == "Edéhzhíe")
ede_naiocc <- ggplot(spp_naiocc_ede,
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
ede_naiocc

## Save plot
ggsave("figures/edehzhie_allspecies_naiveoccupancy_2021-2022.png", ede_naiocc, width = 18, height = 12, dpi = 300)

### Sambaa K'e
spp_naiocc_sk <- spp_naive_summary |> filter(study_area == "SambaaK'e")
sk_naiocc <- ggplot(spp_naiocc_sk,
                     aes(x = naive_occupancy, y = fct_reorder(species_common_name, naive_occupancy))) + # re-orders species into descending naive_occupancy
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Naive Species Occupancy, Sambaa K'e 2022-2023",
    x = "Naive Occupancy",
    y = NULL) + # removes y-axis title
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12))

win.graph()
sk_naiocc

## Save plot
ggsave("figures/sambaake_winterroad_allspecies_naiveoccupancy_2022-2023.png", sk_naiocc, width = 18, height = 12, dpi = 300)

### TDN
spp_naiocc_tdn <- spp_naive_summary |> filter(study_area == "ThaideneNëné")
tdn_naiocc <- ggplot(spp_naiocc_tdn,
                     aes(x = naive_occupancy, y = fct_reorder(species_common_name, naive_occupancy))) + # re-orders species into descending naive_occupancy
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Naive Species Occupancy, Thaidene Nëné 2021-2022",
    x = "Naive Occupancy",
    y = NULL) + # removes y-axis title
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12))

win.graph()
tdn_naiocc

## Save plot
ggsave("figures/ThaideneNene_allspecies_naiveoccupancy_2021-2022.png", tdn_naiocc, width = 18, height = 12, dpi = 300)

### Fort Smith
spp_naiocc_fs <- spp_naive_summary |> filter(study_area == "FortSmith")
fs_naiocc <- ggplot(spp_naiocc_fs,
                     aes(x = naive_occupancy, y = fct_reorder(species_common_name, naive_occupancy))) + # re-orders species into descending naive_occupancy
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Naive Species Occupancy, Fort Smith 2022-2024",
    x = "Naive Occupancy",
    y = NULL) + # removes y-axis title
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12))

win.graph()
fs_naiocc

## Save plot
ggsave("figures/FortSmith_allspecies_naiveoccupancy_2022-2024.png", fs_naiocc, width = 18, height = 12, dpi = 300)

### Norman Wells
spp_naiocc_nw <- spp_naive_summary |> filter(study_area == "NormanWells")
nw_naiocc <- ggplot(spp_naiocc_nw,
                     aes(x = naive_occupancy, y = fct_reorder(species_common_name, naive_occupancy))) + # re-orders species into descending naive_occupancy
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Naive Species Occupancy, Norman Wells 2022-2023",
    x = "Naive Occupancy",
    y = NULL) + # removes y-axis title
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12))

win.graph()
nw_naiocc

## Save plot
ggsave("figures/NormanWells_allspecies_naiveoccupancy_2022-2023.png", nw_naiocc, width = 18, height = 12, dpi = 300)


### Gameti
spp_naiocc_gam <- spp_naive_summary |> filter(study_area == "Gameti")
gam_naiocc <- ggplot(spp_naiocc_gam,
                     aes(x = naive_occupancy, y = fct_reorder(species_common_name, naive_occupancy))) + # re-orders species into descending naive_occupancy
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  labs(
    title = "Naive Species Occupancy, Gameti 2023-2024",
    x = "Naive Occupancy",
    y = NULL) + # removes y-axis title
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12))

win.graph()
gam_naiocc

## Save plot
ggsave("figures/Gameti_allspecies_naiveoccupancy_2023-2024.png", gam_naiocc, width = 18, height = 12, dpi = 300)


#### Ungulate only naive occupancy
ung_naiocc <- spp_naive_summary |> filter(species_common_name %in% ung_spp)

ung_naiocc_plot <- ggplot(ung_naiocc,
                     aes(x = naive_occupancy, y = fct_reorder(species_common_name, naive_occupancy))) + # re-orders species into descending naive_occupancy
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  facet_wrap(~study_area) +
  labs(
    title = "Naive Species Occupancy of Ungulates",
    x = "Naive Occupancy",
    y = NULL) + # removes y-axis title
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16))

ung_naiocc_plot

## Save
ggsave("figures/ungulate_naive_occupancy_by_sa.png", ung_naiocc_plot, width = 18, height = 12, dpi = 300)
