#################################
## 01_detection_data_explore.R
## Downloading camera data from WildTrax and preliminary exploration of ungulate detections
## Started by Erin Tattersall on 14 January 2026
################################################

library(tidyverse)

## Needed to download station locations
## remotes::install_github("ABbiodiversity/wildrtrax") ## May need to create a new personal access token to retrieve from github
library(wildrtrax)

## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()



# #### Station locations from wildtrax ####

cam_projects <- wt_get_projects("CAM")
glimpse(cam_projects) ## lists all the projects I have access to - including public projects I'm not involved in
## Filter to my target projects only, using project IDs: 712 (Thaidene Nene), 2183 (Fort Smith), 2102 (Norman Wells), 1906 (Sambaa K'e), 2935 (Gameti), 1465 (Edehzhie)
cam_projects <- cam_projects %>% filter(project_id == "712" |
                                          project_id == "2183" |
                                          project_id == "2102" |
                                          project_id == "1906" |
                                          project_id == "2935" |
                                          project_id == "1465")


## Get raw camera data from all projects (note that this is PRE- species verification for game birds (13 Jan 2026), so will need to be re-downloaded once that's complete)
### Camera project reports already downloaded for nwtbm_phd_gamebirds - data frame list saved as RDS file and copied for this project
# cam_data <- wt_download_report(project_id = cam_projects$project_id,
#                                sensor_id = "CAM",
#                                report = "main") # main reports include ALL DATA
# 
# 
# 
# ## isolate tag data from list of data frames
# glimpse(cam_data) ## list of 6 data frames, one per project (it's a list so it can be indexed)
# 
# ## Rename each data frame in cam_data list to simpler study area names
# names(cam_data) <- c("Edéhzhíe", "SambaaK'e", "FortSmith", "Gamètì", "NormanWells", "ThaideneNëné")
# names(cam_data)

## Open RDS file if already downloaded
cam_data <- readRDS(file = "data/cam_data_all_projects_20260112.rds")

## Note that for most projects, station coordinate data is not stored on WildTrax
## Station location data will need to be uploaded separately

## For each data frame in cam_data list, get individual species detections within a 30-minute independence threshold
## note that 30min independence threshold might not make sense for barren-ground caribou, maybe not muskox either

spp_det <- lapply(cam_data, 
                  wt_ind_detect, 
                  threshold = 30, 
                  units = "minutes")

glimpse(spp_det)

## Query a few dfs to get the right common names (spacing and capitalization)
unique(spp_det$Edéhzhíe$species_common_name)
unique(spp_det$ThaideneNëné$species_common_name)


# How many species are named in total?
spp_count <- lapply(spp_det, function(df) {
  length(unique(df$species_common_name))
})



##### filter to target ungulate species only ####
target_spp <- c("Barren-ground Caribou", "Bison", "Moose", "Muskox", "Woodland Caribou")
spp_det_ung <- lapply(spp_det, function(df) {
  df %>% filter(species_common_name %in% target_spp)
})


spp_det_ung_count <- lapply(spp_det_ung, function(df) {
  length(unique(df$species_common_name))
}) ## returns a list of total ungulate species per study area
spp_det_ung_count ## none of the study areas have all five


## Bind all ungulate dfs in spp_det_ung list into a single data frame with study area as an identifier column
ung_df <- bind_rows(spp_det_ung, .id = "study_area")
glimpse(ung_df)

### Save ungulate independent detection data frame
write.csv(ung_df, "data/all_projects_ungulate_30min_ind_detections_20260114.csv", row.names = FALSE)

## Summarise the ungulate data by study area and detection count (ordered by study area and species)
ung_count <- ung_df %>%
  group_by(study_area, species_common_name) %>%
  summarise(count = n()) %>%
  mutate(study_area = fct_reorder(study_area, count, .desc = TRUE)) %>% 
  ungroup()

ung_count


## testing - is this pushing to github??

