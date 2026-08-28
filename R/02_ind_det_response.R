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


### Load raw data generated in 01_camera_deployment_data.R
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

# Re-name back to original
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
cam_det <- cam_det |>  
  left_join(cam_locs, by = "location")
glimpse(cam_det)

write.csv(cam_det, "data/camera_data/nwtbm_allprojects_camera_detections_30min.csv")

## Isolating ungulates - don't include barren-ground caribou, since they don't overlap with fire areas
ung_spp <- c("Bison", "Moose", "Muskox", "Woodland Caribou")

ung_data <- cam_det |> filter(species_common_name %in% ung_spp)
glimpse(ung_data)

## isolating game birds - also include column Ptarmigans (ID'd to Genus only)
gb_spp <- c("Ptarmigans", "Rock Ptarmigan", "Ruffed Grouse", "Sharp-tailed Grouse", "Spruce Grouse", "Willow Ptarmigan")

gb_data <- cam_det |> filter(species_common_name %in% gb_spp)


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

## try faceting by species instead, with study_area on y
ung_det2 <- ggplot(ung_count,
                  aes(x = count, y = fct_reorder(study_area, count))) + # re-orders species into descending count
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  facet_wrap(~species_common_name, scales = "free_x") + ## allow each facet its own x-axis
  labs(
    title = "Total Ungulate Detections",
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
ung_det2

## Save ungulate plot
ggsave("figures/nwtbm_ungulate_detections.png", ung_det2, width = 18, height = 12, dpi = 300)


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


## facet by species
gb_det2 <- ggplot(gb_count,
                   aes(x = count, y = fct_reorder(study_area, count))) + # re-orders species into descending count
  geom_bar(stat = "identity", fill = "seagreen4", color = "black") +
  facet_wrap(~species_common_name, scales = "free_x") + ## allow each facet its own x-axis
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
gb_det2

## Save ungulate plot
ggsave("figures/nwtbm_gamebird_detections.png", gb_det2, width = 18, height = 12, dpi = 300)


#### 2. Naive occupancy by Study Area ####
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

glimpse(stn_species_cams)

## Convert to long format for plotting
spp_naive_long <- stn_species_cams %>% 
  group_by(study_area, location) %>% 
  pivot_longer(cols = -c(study_area, location), ## all columns except these
               names_to = "species_common_name",
               values_to = "detection")

glimpse(spp_naive_long)

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



##### 3. Monthly detections #####
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

## Check out survey effort
summary(det_mon$n_days_effort) ## n_days_effort should be the number of active days within the time interval - why does it have values > 31???
head(det_mon)
hist(det_mon$n_days_effort) ## error in how function is calculating survey effort

## which rows are greater than 31 (and thus definitely errors?)
mon_eff_bad <- det_mon |> 
  filter(n_days_effort > 31) ## 6295 errors
## how many cameras affected?
length(unique(mon_eff_bad$location)) ## all of them except 3...
## Which 3 are not affected?
bad_eff_cam <- unique(mon_eff_bad$location)
good_eff_cams <- setdiff(cam_locs$location, bad_eff_cam)

## Check good effort cams in deployment summary (not that necessary to read in, could just check manually?)
dep_eff <- read.csv("data/camera_data/nwtbm_camera_deployment_summary.csv")

## Cameras without any effort errors all have at least 1 OOR day, with no clear similarities - mystery

## Calculate monthly active days from the wide format camera summary to replace n_days_effort in det_mon
## Load in wide format camera summary
camera_summary <- read.csv("data/camera_data/nwtbm_camera_deployment_wideformat.csv")
glimpse(camera_summary)
table(is.na(camera_summary$deploy_start))
table(is.na(camera_summary$deploy_end)) ## no NAs

## Re-format all date columns as dates
camera_summary <- camera_summary |> 
  mutate(
    across(
      c(
        deploy_start,
        deploy_end,
        matches("^oor\\d+_(start|end)$")
      ),
      as.Date
    )
  )

glimpse(camera_summary)

## Regenerate long format of OOR intervals and clip them to deployment bounds
oor_long <- camera_summary |> 
  select(
    study_area,
    location,
    deploy_start,
    deploy_end,
    matches("^oor\\d+_(start|end)$")
  ) |> 
  pivot_longer(
    cols = matches("^oor\\d+_(start|end)$"),
    names_to = c("oor_num", ".value"),
    names_pattern = "oor(\\d+)_(start|end)"
  ) |> 
  filter(!is.na(start), !is.na(end)) %>%
  
  # Keep only OOR intervals that overlap the deployment
  filter(
    end >= deploy_start,
    start <= deploy_end
  ) |> 
  
  # Clip OOR intervals to deployment boundaries
  mutate(
    start = pmax(start, deploy_start),
    end   = pmin(end, deploy_end)
  )

## Generate deployment dates - all dates when a camera was deployed
deployment_dates <- camera_summary |> 
  select(study_area, location, deploy_start, deploy_end) |> 
  rowwise() |> #group by rows
  mutate(
    active_date = list(seq(deploy_start, deploy_end, by = "day")) # list the days between start and end in sequence
  ) |> 
  unnest(active_date) |> 
  ungroup()


## Create OOR dates based on the deployment period only
oor_dates <- oor_long |> 
  rowwise() |> 
  mutate(
    active_date = list(seq(start, end, by = "day")) ## active_date here is the OOR dates
  ) |> 
  unnest(active_date) |> 
  ungroup() |> 
  select(study_area, location, active_date)

## Remove OOR dates from deployment dates
active_dates <- deployment_dates %>%
  anti_join( ## anti_join returns rows that don't match the oor_dates 
    oor_dates,
    by = c("study_area", "location", "active_date")
  )

summary(active_dates)

## summarize by month
monthly_effort <- active_dates %>%
  mutate(
    year = year(active_date),
    month_num = month(active_date),
    month = format(active_date, "%B"),      # January, February, etc.
    year_month = floor_date(active_date, "month")
  ) %>%
  group_by(
    study_area,
    location,
    year,
    month_num,
    month,
    year_month
  ) %>%
  summarise(
    active_days = n(),
    .groups = "drop"
  ) %>%
  arrange(
    study_area,
    location,
    year,
    month_num
  )

glimpse(monthly_effort)
hist(monthly_effort$active_days)
summary(monthly_effort$active_days) ## 1-31

glimpse(det_mon)

## monthly effort has more rows than det_month (35 more), suggesting that det_month is dropping some months for certain stations (months with no animal detections?)
## Find missing rows in det_mon with anti_join
missing_rows <- monthly_effort %>%
  anti_join(
    det_mon,
    by = c(
      "study_area",
      "location",
      "year",
      "month"
    )
  )
nrow(missing_rows)
head(missing_rows) ## ENWA-O-03-04 had an untagged office image - revised to OOR in WT
### Cross-check of WT images and a couple of the stations in missing_rows suggests that these months contained no images within FOV, so they have 0 survey effort those months. Can be removed from analysis

## Also some rows in det_mon that didn't occur in monthly_effort - which ones?
missing_det_rows <- det_mon %>%
  anti_join(
    monthly_effort,
    by = c(
      "study_area",
      "location",
      "year",
      "month"
    )
  )
nrow(missing_det_rows)
head(missing_det_rows) ## Save and Cross-check in WT - more OOR images revised, but shouldn't be a problem. Convert NA values to 0

## Save for WT cross-check
# write.csv(missing_det_rows, "data/camera_data/nwtbm_missing_active_days.csv")


glimpse(monthly_effort)
summary(monthly_effort)


### Join monthly_effort to det_mon by study_area, location, year, month. Use right_join to keep all observations in det_mon. Remove n_days_effort
det_mon2 <- monthly_effort |> 
  right_join(det_mon,
             by = c(
               "study_area",
               "location",
               "year",
               "month"
             )
  ) |> 
  select(-n_days_effort)
glimpse(det_mon2)


glimpse(det_mon2)
summary(det_mon2) ## 12 NAs in month_num, year_month, active_days - 12 station-months that contained no active days
table(is.na(det_mon2$month))

## Revise NAs in active days to 0, fill month_num and year_month
det_mon2 <- det_mon2 %>%
  mutate(
    # Replace missing active days with 0
    active_days = coalesce(active_days, 0),
    
    # Recalculate month_num where missing
    month_num = if_else(
      is.na(month_num),
      match(month, month.name),
      month_num
    ),
    
    # Recalculate year_month where missing
    year_month = if_else(
      is.na(year_month),
      as.Date(sprintf("%d-%02d-01", year, month_num)),
      year_month
    )
  )

## Check the corrections
det_mon2 %>%
  filter(active_days == 0) %>%
  select(study_area, location, year, month, month_num, year_month, active_days) ## same rows as in missing_det_rows

## Convert study_area, location, and month to factors (month levelled chronologically) 
det_mon2 <- det_mon2 %>%
  mutate(
    study_area = factor(study_area),
    location = factor(location),
    month = factor(
      month,
      levels = month.name,  # January → December
      ordered = TRUE
    )
  )

glimpse(det_mon2)

## Save full monthly detections df, ungulate df, and game bird df

## ungulates
ung_mon <- det_mon2 |> 
  select(
    study_area, location, year, month, year_month, active_days,
    all_of(ung_spp)
  )

glimpse(ung_mon)


## Save game birds
gb_mon <- det_mon2 |> 
  select(
    study_area, location, year, month, year_month, active_days,
    all_of(gb_spp)
  ) 

glimpse(gb_mon)

## Add all ROPT and WIPT detections to Ptarmigans column
gb_mon$Ptarmigans <- gb_mon$Ptarmigans + gb_mon$`Rock Ptarmigan` + gb_mon$`Willow Ptarmigan`

summary(gb_mon$Ptarmigans)

## Save number of detections by month
write.csv(det_mon2, "data/camera_data/nwtbm_allspecies_detections_by_month.csv")

## save ungulate and game bird monthly detections
write.csv(ung_mon, "data/camera_data/nwtbm_ungulate_detections_by_month.csv")
write.csv(gb_mon, "data/camera_data/nwtbm_gamebird_detections_by_month.csv")

### Plot total detections by month for target ungulates and game birds to check seasonal cut-offs
## one plot per species, different colors for each study area
## Individual plots (not faceted - not ready to pivot the data longer or divide into different species dfs)

## Set colors for each study area
study_area_cols <- c(
  "Edéhzhíe" = "#FA8072",
  "FortSmith" = "#EEb422",
  "Gameti" = "#00cd66",
  "NormanWells" = "#00EEEE",
  "SambaaK'e" = "#1E90FF",
  "ThaideneNëné" = "#FF69B4"
)

## Moose
moose_mon <- ggplot(
  det_mon2,
  aes(
    x = month, # sum of all monthly detections in a season
    y = Moose,
    fill = study_area
  )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = study_area_cols,
    drop = TRUE
  ) +
  labs(
    x = "Month",
    y = "Moose Detections",
    fill = "Study Area"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

win.graph()
moose_mon ## Increase in detections March - October

ggsave("figures/moose_monthly_detections_by_studyarea.png", moose_mon, width = 18, height = 12, dpi = 300)

## Woodland caribou
caribou_mon <- det_mon2 |> 
  filter(study_area %in% det_mon2$study_area[det_mon2$`Woodland Caribou` > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
  aes(
    x = month, # sum of all monthly detections in a season
    y = `Woodland Caribou`,
    fill = study_area
  )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = study_area_cols,
    drop = TRUE
  ) +
  labs(
    x = "Month",
    y = "Woodland Caribou Detections",
    fill = "Study Area"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

caribou_mon ## No detections on Sambaa K'e winter road when road is open!!! Plot separately and send to SKFN 

ggsave("figures/wcaribou_monthly_detections_by_studyarea.png", caribou_mon, width = 18, height = 12, dpi = 300)

## Muskox
muskox_mon <- det_mon2 |> 
  filter(study_area %in% det_mon2$study_area[det_mon2$Muskox > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
  aes(
    x = month, # sum of all monthly detections in a season
    y = Muskox,
    fill = study_area
  )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = study_area_cols,
    drop = TRUE
  ) +
  labs(
    x = "Month",
    y = "Muskox Detections",
    fill = "Study Area"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

muskox_mon 

ggsave("figures/muskox_monthly_detections_by_studyarea.png", muskox_mon, width = 18, height = 12, dpi = 300)

## Bison
bison_mon <- det_mon2 |> 
filter(study_area %in% det_mon2$study_area[det_mon2$Bison > 0]) |> ## filter for and drop unused study areas
mutate(study_area = droplevels(study_area)) |> 
  ggplot(
  aes(
    x = month, # sum of all monthly detections in a season
    y = Bison,
    fill = study_area
  )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = study_area_cols,
    drop = TRUE
  ) +
  labs(
    x = "Month",
    y = "Bison Detections",
    fill = "Study Area"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

bison_mon 
ggsave("figures/bison_monthly_detections_by_studyarea.png", bison_mon, width = 18, height = 12, dpi = 300)



 ##### Game birds

## Ptarmigans (aggregated)
ptarm_mon <- gb_mon |> 
  filter(study_area %in% gb_mon$study_area[gb_mon$Ptarmigans > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month, # sum of all monthly detections in a season
      y = Ptarmigans,
      fill = study_area
    )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = study_area_cols,
    drop = TRUE
  ) +
  labs(
    x = "Month",
    y = "Ptarmigan Detections",
    fill = "Study Area"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))


win.graph()
ptarm_mon 
ggsave("figures/ptarmigan_monthly_detections_by_studyarea.png", ptarm_mon, width = 18, height = 12, dpi = 300)

## Spruce Grouse
spgr_mon <- gb_mon |> 
  filter(study_area %in% gb_mon$study_area[gb_mon$`Spruce Grouse` > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month, # sum of all monthly detections in a season
      y = `Spruce Grouse`,
      fill = study_area
    )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = study_area_cols,
    drop = TRUE
  ) +
  labs(
    x = "Month",
    y = "Spruce Grouse Detections",
    fill = "Study Area"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))


spgr_mon 
ggsave("figures/spgr_monthly_detections_by_studyarea.png", spgr_mon, width = 18, height = 12, dpi = 300)

### Sharp-tailed Grouse
stgr_mon <- gb_mon |> 
  filter(study_area %in% gb_mon$study_area[gb_mon$`Sharp-tailed Grouse` > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month, # sum of all monthly detections in a season
      y = `Sharp-tailed Grouse`,
      fill = study_area
    )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = study_area_cols,
    drop = TRUE
  ) +
  labs(
    x = "Month",
    y = "Sharp-tailed Grouse Detections",
    fill = "Study Area"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))


stgr_mon 
ggsave("figures/stgr_monthly_detections_by_studyarea.png", stgr_mon, width = 18, height = 12, dpi = 300)

### Ruffed Grouse
rugr_mon <- gb_mon |> 
  filter(study_area %in% gb_mon$study_area[gb_mon$`Ruffed Grouse` > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month, # sum of all monthly detections in a season
      y = `Ruffed Grouse`,
      fill = study_area
    )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = study_area_cols,
    drop = TRUE
  ) +
  labs(
    x = "Month",
    y = "Ruffed Grouse Detections",
    fill = "Study Area"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))


rugr_mon 
ggsave("figures/rugr_monthly_detections_by_studyarea.png", rugr_mon, width = 18, height = 12, dpi = 300)

#### Detections by Month across all study areas ####
## Moose
moose_mon2 <- ggplot(
  det_mon2,
  aes(
    x = month, # sum of all monthly detections in a season
    y = Moose
  )) +
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    x = "Month",
    y = "Moose Detections"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

win.graph()
moose_mon2 ## Increase in detections March - October

ggsave("figures/moose_monthly_detections.png", moose_mon2, width = 18, height = 12, dpi = 300)

## Woodland caribou
caribou_mon2 <- det_mon2 |> 
  filter(study_area %in% det_mon2$study_area[det_mon2$`Woodland Caribou` > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month, # sum of all monthly detections in a season
      y = `Woodland Caribou`
    )) +
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    x = "Month",
    y = "Woodland Caribou Detections"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

caribou_mon2

ggsave("figures/wcaribou_monthly_detections.png", caribou_mon2, width = 18, height = 12, dpi = 300)

## Muskox
muskox_mon2 <- det_mon2 |> 
  filter(study_area %in% det_mon2$study_area[det_mon2$Muskox > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month, # sum of all monthly detections in a season
      y = Muskox
    )) +
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    x = "Month",
    y = "Muskox Detections"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

muskox_mon2 

ggsave("figures/muskox_monthly_detections.png", muskox_mon2, width = 18, height = 12, dpi = 300)

## Bison
bison_mon2 <- det_mon2 |> 
  filter(study_area %in% det_mon2$study_area[det_mon2$Bison > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month,
      y = Bison
    )) +
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    x = "Month",
    y = "Bison Detections"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

bison_mon2

ggsave("figures/bison_monthly_detections.png", bison_mon2, width = 18, height = 12, dpi = 300)



##### Game birds

## Ptarmigans (aggregated)
ptarm_mon2 <- gb_mon |> 
  filter(study_area %in% gb_mon$study_area[gb_mon$Ptarmigans > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month, # sum of all monthly detections in a season
      y = Ptarmigans
    )) +
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    x = "Month",
    y = "Ptarmigan Detections"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

ptarm_mon2 
ggsave("figures/ptarmigan_monthly_detections.png", ptarm_mon2, width = 18, height = 12, dpi = 300)

## Spruce Grouse
spgr_mon2 <- gb_mon |> 
  filter(study_area %in% gb_mon$study_area[gb_mon$`Spruce Grouse` > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month,
      y = `Spruce Grouse`
    )) +
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    x = "Month",
    y = "Spruce Grouse Detections"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))


spgr_mon2 
ggsave("figures/spgr_monthly_detections.png", spgr_mon2, width = 18, height = 12, dpi = 300)

### Sharp-tailed Grouse
stgr_mon2 <- gb_mon |> 
  filter(study_area %in% gb_mon$study_area[gb_mon$`Sharp-tailed Grouse` > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month, # sum of all monthly detections in a season
      y = `Sharp-tailed Grouse`
    )) +
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    x = "Month",
    y = "Sharp-tailed Grouse Detections"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))


stgr_mon2 
ggsave("figures/stgr_monthly_detections.png", stgr_mon2, width = 18, height = 12, dpi = 300)

### Ruffed Grouse
rugr_mon2 <- gb_mon |> 
  filter(study_area %in% gb_mon$study_area[gb_mon$`Ruffed Grouse` > 0]) |> ## filter for and drop unused study areas
  mutate(study_area = droplevels(study_area)) |> 
  ggplot(
    aes(
      x = month, # sum of all monthly detections in a season
      y = `Ruffed Grouse`
    )) +
  geom_col(position = position_dodge(width = 0.8), fill = "darkgreen") +
  labs(
    x = "Month",
    y = "Ruffed Grouse Detections"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))


rugr_mon2 
ggsave("figures/rugr_monthly_detections.png", rugr_mon2, width = 18, height = 12, dpi = 300)



#### Seasons should differ for different species to account for species-specific phenology
# Adding season to monthly data, with June - July as summer and October - May as winter (snow-free/snow)
# monthly_effort <- monthly_effort %>%
#   mutate(
#     month = as.character(month),
#     season = case_when(
#       month %in% c("June", "July", "August", "September") ~ "Summer",
#       month %in% c("October", "November", "December", "January", "February", 
#                    "March", "April", "May") ~ "Winter",
#       TRUE ~ NA_character_),
#     season = factor(season, levels = c("Summer", "Winter")))








## Plot total monthly detections for all study areas for each species. 1 plot = 1 species, 1 season, six study areas

## Re-format into long format
ung_mon_long <- ung_mon |> 
  pivot_longer(
    cols = all_of(ung_spp),
    names_to = "species",
    values_to = "monthly_det"
  ) |> 
  group_by(study_area, season, species) |> 
  summarise(
    total_detections = sum(monthly_det, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(ung_mon_long)


### Faceted seasonal plot (remember summer is 4 months - June-Sep - while winter is 8 - Oct-Apr)
seas_ung <- ggplot(
  ung_mon_long,
  aes(
    x = total_detections, # sum of all monthly detections in a season
    y = study_area,
    fill = season
  )) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~species, scales = "free_x") +
  labs(
    x = "Total Monthly Detections",
    y = NULL,
    fill = "Season"
  ) +
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

seas_ung

## save
ggsave("figures/species_monthly_detections_by_season.png", seas_ung, width = 18, height = 12, dpi = 300)


### Visualizing distribution of response variable
glimpse(ung_mon)



## Remember that only subsets of the full dataset apply to each species!
## That means that for each species, don't count the study areas they are not expected in

## Datasets for each ungulate species
## Moose present in all six, no filtering needed (use full spp_naive_summary)
muskox_studyareas <- c("ThaideneNëné", "NormanWells", "Gameti")
bison_studyareas <- c("Edéhzhíe", "FortSmith")
wcaribou_studyareas <- c("Edéhzhíe", "NormanWells", "SambaaK'e")

## Moose - all study areas (use full dataset)
# Moose density plot (density = probability distribution, or likelihood of a value of monthly detections occurring)

moose_dens <- ggplot(ung_mon,
                     aes(x = Moose, colour = season, fill = season)) +
  geom_density(alpha = 0.8) +
  labs(
    title = "Distribution of Moose Detections",
    x = "Monthly Detections",
    y = "Density"
  ) +
  scale_x_continuous(trans = "log1p") + # log transform x axis to visualize tail better
theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

moose_dens

ggsave("figures/moose_monthly_detections_distribution.png", moose_dens, width = 18, height = 12, dpi = 300)

## Muskox - filtered for TDN, NW, and Gameti

muskox_mon <- ung_mon |> 
  filter(study_area %in% muskox_studyareas)

muskox_dens <- ggplot(muskox_mon,
                     aes(x = Muskox, colour = season, fill = season)) +
  geom_density(alpha = 0.8) +
  labs(
    title = "Distribution of Muskox Detections",
    x = "Monthly Detections",
    y = "Density"
  ) +
  scale_x_continuous(trans = "log1p") + # log transform x axis to visualize tail better
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

muskox_dens

ggsave("figures/muskox_monthly_detections_distribution.png", muskox_dens, width = 18, height = 12, dpi = 300)

## Bison - filtered for Edehzhie and FortSmith

bison_mon <- ung_mon |> 
  filter(study_area %in% bison_studyareas)

bison_dens <- ggplot(bison_mon,
                      aes(x = Bison, colour = season, fill = season)) +
  geom_density(alpha = 0.8) +
  labs(
    title = "Distribution of Bison Detections",
    x = "Monthly Detections",
    y = "Density"
  ) +
  scale_x_continuous(trans = "log1p") + # log transform x axis to visualize tail better
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

bison_dens

ggsave("figures/bison_monthly_detections_distribution.png", bison_dens, width = 18, height = 12, dpi = 300)

## Woodland caribou - filtered for NW, Edehzhie, and SambaaK'e

wcaribou_mon <- ung_mon |> 
  filter(study_area %in% wcaribou_studyareas)

wcaribou_dens <- ggplot(wcaribou_mon,
                     aes(x = `Woodland Caribou`, colour = season, fill = season)) +
  geom_density(alpha = 0.8) +
  labs(
    title = "Distribution of Woodland Caribou Detections",
    x = "Monthly Detections",
    y = "Density"
  ) +
  scale_x_continuous(trans = "log1p") + # log transform x axis to visualize tail better
  theme_classic() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14))

wcaribou_dens

ggsave("figures/woodlandcaribou_monthly_detections_distribution.png", wcaribou_dens, width = 18, height = 12, dpi = 300)


####4. Survey Effort ####

## Axes are very weird on this one
# ggplot(det_mon, aes(x = n_days_effort, fill = season)) +
# geom_histogram(binwidth = 30, alpha = 0.6, position = "identity") +
# labs(
#   x = "Camera Effort by Month",
#   y = "Frequency"
# ) +
# theme_classic() +
#   # increase size of title text, axis text
#   theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
#   theme(axis.title.x = element_text(size = 16)) +
#   theme(axis.text = element_text(size = 12)) +
#   theme(strip.text = element_text(size = 16)) +
#   theme(legend.title = element_text(size = 16)) +
#   theme(legend.text = element_text(size = 14))
