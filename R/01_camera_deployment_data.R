###########################
## 01_camera_deployment_data.R
## Downloading raw WildTrax data, post-processing of date/time and FOV errors, and checking camera activity
## Started on Mar 12 2026, updated August 2026
## Created by Erin Tattersall
###########################


#### Environment set up #### 

## List of packages 
x <- c("sf",
       "terra", 
       "ggplot2", 
       "tidyterra", 
       "ggspatial",
       "wildrtrax",
       "tidyverse",
       "lubridate",
       "data.table",
       "plotly",
       "stringr")

## install.packages(x) ## should already be installed ####
lapply(x, require, character.only = TRUE)


## Needed to download station locations
library(wildrtrax)


## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()

#### Download Wildtrax data ####

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

## remove rawest forms of data and duplicates to save space
rm(raw_data_df, raw_data1, raw_data2, std_data_df2)

#### Correcting dates, times, and OOR ####

## Aug 14 - 20: Untagged timelapse images tagged, FOVs revised for images outside of deployment
## There are still known errors in 2 Edehzhie cameras when the date_time recorded by the camera is incorrect
## Also a few stations that need OOR to be revised with post-processing for longer time periods (most fixed in Wildtrax, but a few had too many images to re-tag manually)


## Check max and min start time for each study area and location, look for dates outside of known deployment
dep_time_summary <- std_data_df |> 
  group_by(study_area, location) |> 
  summarise(loc_first_date_time = min(image_date_time),
            loc_last_date_time = max(image_date_time))


## Edehzhie cameras with incorrect date/times:
## ENWA-O-09-02: deployed Nov 8 2021 @ 10:57, retrieved Nov 6 2022 @ 10:33. Camera says deployed Jan 1 2020 @ 01:12:32, retrieved Dec 29 2020 @ 00:56:33
## ENWA-O-15-03: deployed Nov 7 2021 @ 15:51, retrieved Oct 23 2022 @ 12:33. Camera says deployed Jan 7 2022 @ 15:59:24, retrieved Dec 23 2022 @ 11:31:33 (for simplicity, assume time on camera is correct)

## Correcting these date/time errors - assume the first image from each deployment matches the deployment time
## Calculate the offset for each cam in a tibble

cam_corrections <- tibble(
  location = c("ENWA-O-09-02", "ENWA-O-15-03"),
  offset = c(
    ymd_hms("2021-11-08 10:57:00") - ymd_hms("2020-01-01 01:12:32"),
    ymd_hms("2021-11-07 15:59:24") - ymd_hms("2022-01-07 15:59:24")
  )
)

class(cam_corrections$offset) ##difftime
class(std_data_df$image_date_time)

## Correct image_date_time by left joining cam_corrections and adding the offset (which only applies to the 2 cams)
std_data_df2 <- std_data_df |> 
  left_join(cam_corrections, by = "location") |> 
  mutate(
    offset = replace_na(offset, as.difftime(0, units = "secs")),
    image_date_time = image_date_time + offset
  ) |> 
  select(-offset)

## Check deployment start and end for affected cams
cam_correct_check <- std_data_df2 |>
  filter(location == "ENWA-O-09-02" | 
           location == "ENWA-O-15-03") |> 
  group_by(study_area, location) |> 
  summarise(loc_first_date_time = min(image_date_time),
            loc_last_date_time = max(image_date_time)) ##corrected :)

std_data_df <- std_data_df2

glimpse(std_data_df) 


### Fix OOR periods for 4 cameras with long OOR sequences
## Cameras and dates to revise to OOR
## ENWA-O-08-04: all images starting at 2022-10-22 12:00:00
## ENWA-O-10-04: all images starting at 2022-05-14 13:29:28
## ENWA-O-14-01: all images starting at 2023-10-24 13:21:16
## BMS-KLP-047-05: all images starting at 2023-09-10 14:59:48

class(std_data_df$image_date_time)

## tibble of oor_cutoffs
oor_cutoffs <- tibble(
  study_area = c("Edéhzhíe", "Edéhzhíe", "Edéhzhíe", "Gameti"),
  location = c("ENWA-O-08-04", "ENWA-O-10-04", "ENWA-O-14-01", "BMS-KLP-047-05"),
  oor_from = ymd_hms(c("2022-10-22 12:00:00", "2022-05-14 13:29:28", "2023-10-24 13:21:16", "2023-09-10 14:59:48"))
)

## Revise OORs for these cameras
std_data_df_updated <- std_data_df %>%
  left_join(
    oor_cutoffs,
    by = c("study_area", "location")
  ) %>%
  mutate(
    image_fov = if_else(
      !is.na(oor_from) &
        image_date_time >= oor_from,
      "OOR",
      image_fov
    )
  ) %>%
  select(-oor_from)

## Count revised records
n_changed <- sum(
  coalesce(std_data_df$image_fov, "") !=
    coalesce(std_data_df_updated$image_fov, "")
)

n_changed ## 116 340 records changed


## Inspect changed records
revisions <- std_data_df %>%
  select(
    image_id,
    study_area,
    location,
    image_date_time,
    image_fov_old = image_fov
  ) %>%
  left_join(
    std_data_df_updated %>%
      select(
        image_id,
        image_fov_new = image_fov
      ),
    by = "image_id"
  ) %>%
  filter(
    coalesce(image_fov_old, "") !=
      coalesce(image_fov_new, "")
  )

head(revisions, 20)
tail(revisions, 20)

## Confirm no untagged FOVs after cutoffs
verification <- std_data_df_updated %>%
  left_join(
    oor_cutoffs,
    by = c("study_area", "location")
  ) %>%
  filter(
    !is.na(oor_from),
    image_date_time >= oor_from,
    image_fov != "OOR"
  )

nrow(verification) # 0 means there are no image_date_times in the revised set that are greater than the cutoffs

## resave and delete confirmation objects to save space
std_data_df <- std_data_df_updated

rm(std_data_df_updated, revisions, verification, n_changed)

## Save std_data_df
write.csv(std_data_df, "data/camera_data/nwtbm_allprojects_camera_tags.csv")


#### Camera activity ####
## Wildtrax has a function for summarising camera data for analysis that will calculate survey effort based on image_date_time and image_fov.
## BUT this doesn't include effort for a full camera deployment like it does for shorter time periods

### Use std_data_df to calculate survey effort. Find start and end dates of non-OOR images

# Create date column
std_data2 <- std_data_df %>%
  mutate(img_date = as.Date(image_date_time))

class(std_data2$img_date)
table(is.na(std_data2$image_fov))


#--------------------------------------------------
# Deployment start/end from NON-OOR images only
#--------------------------------------------------

deployment <- std_data2 |> 
  filter(is.na(image_fov)) |>  # filter for images that aren't OOR
  group_by(study_area, location) |> 
  summarise(
    deploy_start = min(img_date),
    deploy_end   = max(img_date),
    .groups = "drop"
  )

#--------------------------------------------------
# Identify OOR periods
#--------------------------------------------------

oor_ranges <- std_data2 |> 
  filter(image_fov == "OOR", !is.na(img_date)) |> 
  distinct(study_area, location, img_date) |> 
  arrange(study_area, location, img_date) |> 
  group_by(study_area, location) |> 
  mutate(
    grp = cumsum(
      c(TRUE, diff(img_date) > 1) # creating a new group ID anytime the difference in consecutive image dates is greater than 1
    )
  ) |> 
  group_by(study_area, location, grp) |> 
  summarise(
    start = min(img_date), # start of OOR range
    end   = max(img_date), # end of OOR range
    .groups = "drop"
  ) |> 
  group_by(study_area, location) |> 
  mutate(
    oor_num = row_number() # adding a number to each OOR ranges
  ) |> 
  ungroup()

#--------------------------------------------------
# Convert OOR periods to wide format
#--------------------------------------------------

oor_wide <- oor_ranges %>%
  pivot_longer(
    cols = c(start, end),
    names_to = "type",
    values_to = "date"
  ) %>%
  mutate(
    variable = paste0("oor", oor_num, "_", type)
  ) %>%
  select(study_area, location, variable, date) %>%
  pivot_wider(
    names_from = variable,
    values_from = date
  )

#--------------------------------------------------
# Combine deployment dates and OOR periods
#--------------------------------------------------

camera_summary <- deployment %>%
  left_join(
    oor_wide,
    by = c("study_area", "location")
  )

glimpse(camera_summary)

## Some locations with up to 30 OOR ranges - which is possible, but unlikely. More likely result of some images not being tagged OOR
## Lots of locations have >10 OOR ranges

## Save camera summary to inspect stations with multiple consecutive OOR ranges (already done on Aug 19/20 2026, only repeat if necessary)
write.csv(camera_summary, "data/camera_data/nwtbm_camera_deployment_wideformat.csv")
## OOR ranges checked and revised in WildTrax on Aug 20-21, 2026

## Calculate camera activity for entire deployment

# Convert OOR periods back to long format - same as oor_ranges above, but includes deploy_start, deploy_end, and oor_days
# also removes OOR periods occuring before or after deployment start and end
oor_long <- camera_summary |> 
  select(
    study_area,
    location,
    deploy_start,
    deploy_end,
    matches("^oor\\d+_(start|end)$")
  ) |> 
  pivot_longer(
    cols = matches("^oor\\d+_(start|end)$"), ## aggregates the OOR columns
    names_to = c("oor_num", ".value"), ## creates a column for the numeric ID of the OOR interval
    names_pattern = "oor(\\d+)_(start|end)"
  ) |> 
  filter(!is.na(start), !is.na(end)) |> 
  
  # Keep only intervals that overlap the deployment
  filter(
    end >= deploy_start,
    start <= deploy_end
  ) |> 
  
  # Clip intervals to deployment bounds
  mutate(
    start = pmax(start, deploy_start),
    end   = pmin(end, deploy_end)
  ) |> 
  
  # Calculate inclusive OOR days - add 1 to account for the day subtracted by end-start - that is, if end and start are the same day, oor_days would be 0 but it should be 1
  mutate(
    oor_days = as.numeric(end - start) + 1 
  )

## Check that no intervals are outside deployment
oor_long |> 
  summarise(
    bad_start = sum(start < deploy_start),
    bad_end   = sum(end > deploy_end)
  )


glimpse(oor_long)
summary(oor_long)
table(is.na(oor_long$oor_days)) ## no NAs

## Calculate deployment effort
deployment_effort <- camera_summary |> 
  select(study_area, location, deploy_start, deploy_end) |> 
  mutate(
    deploy_days = as.numeric(deploy_end - deploy_start) + 1 ## Calculate total time from deployment to retrieval
  ) |> 
  left_join(  ## add the total oor_days for each location to camera summary
    oor_long |> 
      group_by(study_area, location) |> 
      summarise(
        oor_days = sum(oor_days),
        .groups = "drop"
      ),
    by = c("study_area", "location")
  ) |> 
  mutate(
    oor_days = coalesce(oor_days, 0), ## fills missing values with 0 (even though there shouldn't be any NAs)
    active_days = deploy_days - oor_days
  )

glimpse(deployment_effort)
summary(deployment_effort)
hist(deployment_effort$active_days)

## Save deployment effort
write.csv(deployment_effort, "data/camera_data/nwtbm_camera_deployment_summary.csv")

### Camera activity per month to be calculated in next script to add to monthly detection summary

### Camera activity plots ###

## Plot for single study area

## Sambaa K'e (smallest)
study <- "SambaaK'e"

# Filter to single study area, Create deployment segments
deploy_df <- camera_summary |> 
  filter(study_area == study) |> 
  select(location, deploy_start, deploy_end)

## Filter oor_ranges (oor in long format) for study area
oor_df <- oor_ranges |> filter(study_area == study)

# Plot for Sambaa K'e
sk_dep <- ggplot() +
  
  # Full deployment period
  geom_segment(
    data = deploy_df,
    aes(
      x = deploy_start,
      xend = deploy_end,
      y = location,
      yend = location
    ),
    linewidth = 2,
    colour = "darkgreen"
  ) +
  
  # OOR periods
  geom_segment(
    data = oor_df,
    aes(
      x = start,
      xend = end,
      y = location,
      yend = location
    ),
    linewidth = 3,
    colour = "red"
  ) +
  
  # Deployment start point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_start,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  # Deployment end point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_end,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  labs(
    x = "Date",
    y = "Camera Location",
    title = paste("Camera Deployment Timeline:", study),
    subtitle = "Green = active deployment, Red = OOR periods"
  ) +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()
  )

win.graph()
sk_dep

ggsave("figures/sambaake_cameraactivity.png", sk_dep, width = 18, height = 12, dpi = 300)

## Fort Smith
study <- "FortSmith"

# Filter to single study area, Create deployment segments
deploy_df <- camera_summary |> 
  filter(study_area == study) |> 
  select(location, deploy_start, deploy_end)

## Filter oor_ranges (oor in long format) for study area
oor_df <- oor_ranges |> filter(study_area == study)

# Plot for Fort Smith
fs_dep <- ggplot() +
  
  # Full deployment period
  geom_segment(
    data = deploy_df,
    aes(
      x = deploy_start,
      xend = deploy_end,
      y = location,
      yend = location
    ),
    linewidth = 2,
    colour = "darkgreen"
  ) +
  
  # OOR periods
  geom_segment(
    data = oor_df,
    aes(
      x = start,
      xend = end,
      y = location,
      yend = location
    ),
    linewidth = 3,
    colour = "red"
  ) +
  
  # Deployment start point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_start,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  # Deployment end point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_end,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  labs(
    x = "Date",
    y = "Camera Location",
    title = paste("Camera Deployment Timeline:", study),
    subtitle = "Green = active deployment, Red = OOR periods"
  ) +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()
  )


fs_dep

ggsave("figures/fortsmith_cameraactivity.png", fs_dep, width = 18, height = 12, dpi = 300)

## Norman Wells
study <- "NormanWells"

# Filter to single study area, Create deployment segments
deploy_df <- camera_summary |> 
  filter(study_area == study) |> 
  select(location, deploy_start, deploy_end)

## Filter oor_ranges (oor in long format) for study area
oor_df <- oor_ranges |> filter(study_area == study)

# Plot for Norman Wells
nw_dep <- ggplot() +
  
  # Full deployment period
  geom_segment(
    data = deploy_df,
    aes(
      x = deploy_start,
      xend = deploy_end,
      y = location,
      yend = location
    ),
    linewidth = 2,
    colour = "darkgreen"
  ) +
  
  # OOR periods
  geom_segment(
    data = oor_df,
    aes(
      x = start,
      xend = end,
      y = location,
      yend = location
    ),
    linewidth = 3,
    colour = "red"
  ) +
  
  # Deployment start point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_start,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  # Deployment end point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_end,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  labs(
    x = "Date",
    y = "Camera Location",
    title = paste("Camera Deployment Timeline:", study),
    subtitle = "Green = active deployment, Red = OOR periods"
  ) +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()
  )

nw_dep

ggsave("figures/normanwells_cameraactivity.png", nw_dep, width = 18, height = 12, dpi = 300)


### Gameti
study <- "Gameti"

# Filter to single study area, Create deployment segments
deploy_df <- camera_summary |> 
  filter(study_area == study) |> 
  select(location, deploy_start, deploy_end)

## Filter oor_ranges (oor in long format) for study area
oor_df <- oor_ranges |> filter(study_area == study)

# Plot for Gameti
gam_dep <- ggplot() +
  
  # Full deployment period
  geom_segment(
    data = deploy_df,
    aes(
      x = deploy_start,
      xend = deploy_end,
      y = location,
      yend = location
    ),
    linewidth = 2,
    colour = "darkgreen"
  ) +
  
  # OOR periods
  geom_segment(
    data = oor_df,
    aes(
      x = start,
      xend = end,
      y = location,
      yend = location
    ),
    linewidth = 3,
    colour = "red"
  ) +
  
  # Deployment start point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_start,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  # Deployment end point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_end,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  labs(
    x = "Date",
    y = "Camera Location",
    title = paste("Camera Deployment Timeline:", study),
    subtitle = "Green = active deployment, Red = OOR periods"
  ) +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()
  )


gam_dep

ggsave("figures/gameti_cameraactivity.png", gam_dep, width = 18, height = 12, dpi = 300)


### Thaidene Nene
study <- "ThaideneNëné"

# Filter to single study area, Create deployment segments
deploy_df <- camera_summary |> 
  filter(study_area == study) |> 
  select(location, deploy_start, deploy_end)

## Filter oor_ranges (oor in long format) for study area
oor_df <- oor_ranges |> filter(study_area == study)

# Plot for TDN
tdn_dep <- ggplot() +
  
  # Full deployment period
  geom_segment(
    data = deploy_df,
    aes(
      x = deploy_start,
      xend = deploy_end,
      y = location,
      yend = location
    ),
    linewidth = 2,
    colour = "darkgreen"
  ) +
  
  # OOR periods
  geom_segment(
    data = oor_df,
    aes(
      x = start,
      xend = end,
      y = location,
      yend = location
    ),
    linewidth = 3,
    colour = "red"
  ) +
  
  # Deployment start point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_start,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  # Deployment end point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_end,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  labs(
    x = "Date",
    y = "Camera Location",
    title = paste("Camera Deployment Timeline:", study),
    subtitle = "Green = active deployment, Red = OOR periods"
  ) +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()
  )

tdn_dep

ggsave("figures/thaidenenene_cameraactivity.png", tdn_dep, width = 18, height = 12, dpi = 300)

## Edehzhie
study <- "Edéhzhíe"

# Filter to single study area, Create deployment segments
deploy_df <- camera_summary |> 
  filter(study_area == study) |> 
  select(location, deploy_start, deploy_end)

## Filter oor_ranges (oor in long format) for study area
oor_df <- oor_ranges |> filter(study_area == study)

# Plot for Edehzhie
ede_dep <- ggplot() +
  
  # Full deployment period
  geom_segment(
    data = deploy_df,
    aes(
      x = deploy_start,
      xend = deploy_end,
      y = location,
      yend = location
    ),
    linewidth = 2,
    colour = "darkgreen"
  ) +
  
  # OOR periods
  geom_segment(
    data = oor_df,
    aes(
      x = start,
      xend = end,
      y = location,
      yend = location
    ),
    linewidth = 3,
    colour = "red"
  ) +
  
  # Deployment start point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_start,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  # Deployment end point
  geom_point(
    data = deploy_df,
    aes(
      x = deploy_end,
      y = location
    ),
    size = 3,
    colour = "darkgreen"
  ) +
  
  labs(
    x = "Date",
    y = "Camera Location",
    title = paste("Camera Deployment Timeline:", study),
    subtitle = "Green = active deployment, Red = OOR periods"
  ) +
  
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()
  )


ede_dep

ggsave("figures/edehzhie_cameraactivity.png", ede_dep, width = 18, height = 12, dpi = 300)
