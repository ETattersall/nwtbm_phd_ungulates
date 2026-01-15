#################################
## 01_detection_data_explore.R
## Downloading camera data from WildTrax and preliminary exploration of ungulate detections
## Started by Erin Tattersall on 12 January 2026
################################################

library(tidyverse)

## Needed to download station locations
## remotes::install_github("ABbiodiversity/wildrtrax") ## May need to create a new personal access token to retrieve from github
library(wildrtrax)

## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()



#### Station locations from wildtrax ####
cam_projects <- wt_get_projects("CAM")
glimpse(cam_projects) ## lists all the projects I have access to - including public projects I'm not involved in

## Filter to my target projects only, using project IDs: 712 (Thaidene Nene), 2183 (Fort Smith), 2102 (Norman Wells), 1906 (Sambaa K'e), 2935 (Gameti), 1465 (Edehzhie)
cam_projects <- cam_projects %>% filter(project_id == "712" |
                                          project_id == "2183" |
                                          project_id == "2102" |
                                          project_id == "1906" |
                                          project_id == "2935" |
                                          project_id == "1465")



## Get raw camera data from all projects (note that this is PRE- species verification for game birds (12 Jan 2026), so will need to be re-downloaded once that's complete)
cam_data <- wt_download_report(project_id = cam_projects$project_id,
                               sensor_id = "CAM",
                               report = "main") # main reports include ALL DATA



## isolate tag data from list of data frames
glimpse(cam_data) ## list of 6 data frames, one per project (it's a list so it can be indexed)

## Rename each data frame in cam_data list to simpler study area names
names(cam_data) <- c("Edéhzhíe", "SambaaK'e", "FortSmith", "Gamètì", "NormanWells", "ThaideneNëné")
names(cam_data)



## Note that for most projects, station coordinate data is not stored on WildTrax
## Station location data will need to be uploaded separately