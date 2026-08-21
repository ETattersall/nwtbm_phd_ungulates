#############################################
## 02_ind_det_response.R
## Downloading and summarizing into no. of independent detections
## for all NWTBMP projects
## Started on August 12 2026
## Created by Erin Tattersall
#############################################


list.of.packages <- c("tidyverse", "data.table", "lubridate", "plotly", "ggplot2", "ggbreak")
lapply(list.of.packages, require, character.only = TRUE)

## Needed to download station locations
library(wildrtrax)


## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()


### Load raw data generated in 01_ind_det_response.R
std_data <- read.csv("data/camera_data/nwtbm_allprojects_camera_tags.csv")
## Load camera location data
cam_locs <- read.csv("data/wt_location_data/nwtbm_cam_locations_20260506.csv")


## Set as data.tables (for large datasets)
setDT(std_data)


glimpse(std_data)
## class of date/time data?
class(std_data$image_date_time) # character - loses POSIX format when saved as CSV
table(is.na(std_data$image_date_time)) ## No NAs in CSV for image_date_time


## Re-format date time data
std_data2 <- std_data |> 
  mutate(image_date_time = ymd_hms(image_date_time)) # warning that 66 failed to parse. Might be different date formats present

class(std_data2$image_date_time) # POSIX now
summary(std_data2$image_date_time) # 66 NAs

## Find rows with bad dates
failed_rows <- std_data %>%
  semi_join(
    std_data2 %>%
      filter(is.na(image_date_time)) %>%
      select(image_id),
    by = "image_id"
  ) ## the NA rows only have date information, no time (yyyy-mm-dd)

## Re-format date times with parse_date_time
std_data2 <- std_data %>%
  mutate(
    image_date_time = parse_date_time(
      image_date_time,
      orders = c(
        "ymd HMS",  # 2023-05-18 14:23:01
        "ymd"       # 2023-05-18
      )
    )
  )
class(std_data2$image_date_time) # POSIX now
summary(std_data2$image_date_time) # no NAs

# ## Do the same with cam_det start time and end time (which is based off of std_data, so should have the same formats)
# cam_det2 <- cam_det %>%
#   mutate(
#     start_time = parse_date_time(
#       start_time,
#       orders = c(
#         "ymd HMS",  # 2023-05-18 14:23:01
#         "ymd"       # 2023-05-18
#       )
#     )
#   ) |> 
#   mutate(
#     end_time = parse_date_time(
#       end_time,
#       orders = c(
#         "ymd HMS",  # 2023-05-18 14:23:01
#         "ymd"       # 2023-05-18
#       )
#     )
#   )
# 
# class(cam_det2$start_time)
# class(cam_det2$end_time) # both POSIX
# summary(cam_det2$start_time)
# summary(cam_det2$end_time) # no NAs

# Re-name back to originals
std_data <- std_data2


## Remove copies to save space
rm(std_data2, failed_rows)

## remove any duplicate rows
std_data <- distinct(std_data)

### Create independent detections from raw camera data, with a standard threshold of 30 minutes
cam_det <- wt_ind_detect(std_data,
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

write.csv(cam_det, "data/camera_data/nwtbm_allprojects_camera_detections_30min.csv")

## Isolating ungulates
ung_spp <- c("Barren-ground Caribou", "Bison", "Moose", "Muskox", "Woodland Caribou")

ung_data <- cam_det |> filter(species_common_name %in% ung_spp)
glimpse(ung_data)

## isolating game birds - also include column Ptarmigans (ID'd to Genus only)
gb_spp <- c("Ptarmigans", "Rock Ptarmigan", "Ruffed Grouse", "Sharp-tailed Grouse", "Spruce Grouse", "Willow Ptarmigan")

gb_data <- cam_det |> filter(species_common_name %in% gb_spp)

## Calculating station-month detection summaries with wt_summarise_cam
det_mon <- wt_summarise_cam(
  detect_data = cam_det,
  raw_data = std_data, ## including raw data so effort per period can be calculated
  time_interval = "month",
  variable = "detections",
  exclude_out_of_range = TRUE, # IMPORTANT: Remove days from effort when image is tagged 'out of FOV' 
  project_col = study_area # include study area as project
  )

glimpse(det_mon)

## All stations?
length(unique(det_mon$location)) # yes, including stations with no detections

## add station data from cam_locs
det_mon <- det_mon |> select(-study_area) #remove study area, since this is included in cam_locs
det_mon <- cam_locs |> # station data should be before detection data
  left_join(det_mon, by = "location")

glimpse(det_mon)

# Adding season to monthly data, with June - July as summer and October - May as winter (snow-free/snow)
 det_mon <- det_mon %>%
  mutate(
    month = as.character(month),
    season = case_when(
      month %in% c("June", "July", "August", "September") ~ "Summer",
      month %in% c("October", "November", "December", "January", "February", 
                   "March", "April", "May") ~ "Winter",
      TRUE ~ NA_character_),
    season = factor(season, levels = c("Summer", "Winter")))

 glimpse(det_mon)

 ## Save number of detections by month
write.csv(det_mon, "data/camera_data/nwtbm_allspecies_detections_by_month.csv")

## Save ungulates and gamebirds
## ungulates
ung_mon <- det_mon |> 
  relocate(season, .before = all_of(ung_spp)) |>    # move season before ungulate columns
  select(
    study_area, location, latitude, longitude,
    sensor_type, site, year, month, n_days_effort,
    season,
    all_of(ung_spp)
  )

glimpse(ung_mon)
## save
write.csv(ung_mon, "data/camera_data/nwtbm_ungulate_detections_by_month.csv")

## Save game birds
gb_mon <- det_mon |> 
  relocate(season, .before = all_of(gb_spp)) |>    # move season before game bird columns
  select(
    study_area, location, latitude, longitude,
    sensor_type, site, year, month, n_days_effort,
    season,
    all_of(gb_spp)
  ) 

glimpse(gb_mon)
write.csv(gb_mon, "data/camera_data/nwtbm_gamebird_detections_by_month.csv")


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
ggsave("figures/nwtbm_ungulate_detections_by_studyarea.png", ung_det, width = 18, height = 12, dpi = 300)


#### Total detections for game birds only
gb_count <- spp_count |> filter(species_common_name %in% gb_spp)

## Faceted plot for all study areas (keep axis fixed for comparison)
gb_det <- ggplot(gb_count,
                  aes(x = count, y = fct_reorder(species_common_name, count))) + # re-orders species into descending count
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  facet_wrap(~study_area) +
  labs(
    title = "Total Game Bird Detections",
    x = "Independent Detections (30 min.)",
    y = NULL) + # removes y-axis title
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
gb_det

## Save game bird plot
ggsave("figures/nwtbm_gamebird_detections_by_studyarea.png", gb_det, width = 18, height = 12, dpi = 300)


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


#### Game bird only naive occupancy
gb_naiocc <- spp_naive_summary |> filter(species_common_name %in% gb_spp)

gb_naiocc_plot <- ggplot(gb_naiocc,
                          aes(x = naive_occupancy, y = fct_reorder(species_common_name, naive_occupancy))) + # re-orders species into descending naive_occupancy
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  facet_wrap(~study_area) +
  labs(
    title = "Naive Species Occupancy of Game birds",
    x = "Naive Occupancy",
    y = NULL) + # removes y-axis title
  theme_classic() + 
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16))

gb_naiocc_plot

## Save
ggsave("figures/gamebird_naive_occupancy_by_sa.png", gb_naiocc_plot, width = 18, height = 12, dpi = 300)


#####