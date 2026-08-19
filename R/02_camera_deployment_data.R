###########################
## 02_camera_deployment_data.R
## Started on Mar 12 2026
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
       "plotly")

## install.packages(x) ## should already be installed ####
lapply(x, require, character.only = TRUE)


### Load WildTrax image set data
setwd("data/wt_camera_reports_allprojects")
list.files() ## want to load in all reports ending in "image_set_report.csv"
img_set_list <- list.files(pattern = "image_set_report.csv")
img_set_list ## 6 reports to load in (1 per project)

## Load in each report, add a column for the source file, and combine into one data frame
dep_df <- lapply(img_set_list, function(x) {
  df <- fread(x)
  df$source_file <- x
  return(df)
}) %>% bind_rows()



## Also want to load in all image reports
img_rep_list <- list.files(pattern = "image_report.csv")
## Load in each image report, add a column for the source file (can't yet combine into one df because some column classes differ)
img_df <- lapply(img_rep_list, function(x) {
  df <- fread(x)
  df$source_file <- x
  return(df)
})

glimpse(img_df) ## image_fire in the Sambaa K'e df is DNC (did not collect?), character class instead of logical. Convert to logical
table(img_df[[2]]$image_fire) ## all DNC - convert to FALSE
img_df[[2]]$image_fire <- as.logical(img_df[[2]]$image_fire)
glimpse(img_df)
## convert image_snow in Edehzhie to logical
img_df[[1]]$image_snow <- as.logical(img_df[[1]]$image_snow)

## Now combine all dfs in img_df into one biiiiiig df
img_df <- bind_rows(img_df)

## return to base directory
setwd("../../")

### Add a column for study area
dep_df <- dep_df %>%
  mutate(study_area = case_when(
    str_detect(source_file, "Edéhzhíe") ~ "Edéhzhíe",
    str_detect(source_file, "Fort_Smith") ~ "FortSmith",
    str_detect(source_file, "Gameti") ~ "Gameti",
    str_detect(source_file, "Norman_Wells") ~ "NormanWells",
    str_detect(source_file, "Sambaa_K'e") ~ "SambaaK'e",
    str_detect(source_file, "Thaidene_Nëné") ~ "ThaideneNëné",
    TRUE ~ NA_character_  # Default case if no match
  ))

table(is.na(dep_df$study_area)) ## all study area names copied correctly

img_df <- img_df %>%
  mutate(study_area = case_when(
    str_detect(source_file, "Edéhzhíe") ~ "Edéhzhíe",
    str_detect(source_file, "Fort_Smith") ~ "FortSmith",
    str_detect(source_file, "Gameti") ~ "Gameti",
    str_detect(source_file, "Norman_Wells") ~ "NormanWells",
    str_detect(source_file, "Sambaa_K'e") ~ "SambaaK'e",
    str_detect(source_file, "Thaidene_Nëné") ~ "ThaideneNëné",
    TRUE ~ NA_character_  # Default case if no match
  ))

table(img_df$study_area) ## all study area names copied correctly

## Remove source_file column
dep_df <- dep_df %>%
  select(-source_file)

img_df <- img_df %>%
  select(-source_file)

#### Explore image sets (or deployments) ####

## 722 rows - one per location? how many unique locations?
length(unique(dep_df$location)) ## 706 unique locations - which ones are duplicated?
length(unique(dep_df$image_set_id)) ## 722 - 16 locations that are duplicates of others
## Subset dep_df for duplicated locations
dup_locs <- dep_df[duplicated(dep_df$location) | duplicated(dep_df$location, fromLast = TRUE), ] ## Selecting all rows where the location is duplicated, either by starting at the top or the bottom of the df (which ensures inclusion of rows that appear more than twice)
# Group rows with the same location
dup_locs <- dup_locs[order(dup_locs$location), ]
## Keep only columns for study_area, location, image_set_id, image_set_start_date_time, and image_set_end_date_time
dup_locs <- dup_locs %>%
  select(study_area, location, image_set_id, image_set_start_date_time, image_set_end_date_time)

## Each image set represents a camera check - confirmed that all checks are successive with no date gaps
## In dep_df, merge rows with the same location by keeping the earliest start date and latest end date for each location, summarizing values for image_set_count_motion, image_set_count_timelapse, and image_set_count_total by summing them across rows with the same location, and keeping the first value for study_area and image_set_id
dep_df <- dep_df %>%
  group_by(study_area, location) %>%
  summarise(study_area = first(study_area), 
            image_set_id = first(image_set_id), 
            image_set_start_date_time = min(image_set_start_date_time), 
            image_set_end_date_time = max(image_set_end_date_time),
            image_set_count_motion = sum(image_set_count_motion),
            image_set_count_timelapse = sum(image_set_count_timelapse),
            image_set_count_total = sum(image_set_count_total)) %>%
ungroup()
# dep_df now has 706 rows, one per unique location!

glimpse(dep_df)

## Summarise minimum start time and maximum end time for each study area
dep_time_summary <- dep_df %>%
  group_by(study_area) %>%
  summarise(image_set_start_date_time = min(image_set_start_date_time),
            image_set_end_date_time = max(image_set_end_date_time))

## Earliest Edehzhie date doesn't look right (2020-01-01) - ENWA-O-9-2. Deployment data shows camera deployed Nov 8 2021 at 10:57. Retrieved on Nov 6 2022 at 10:33
## Manual checks also show that ENWA-O-3-2 and ENWA-O-3-3 were also turned on early
## ENWA-O-3-2 start date: 2021-11-06 15:32:24
## ENWA-O-3-3 start date: 2021-11-06 15:12:52
## Similarly, earliest TDN date is (2021-03-03) - should be 2021-08-28


## Adjust start time for BMS-CRU-004-001 in TDN and start and end times for ENWA-O-9-2
dep_df <- dep_df %>%
  mutate(
    image_set_start_date_time = ymd_hms(image_set_start_date_time),
    image_set_end_date_time = ymd_hms(image_set_end_date_time)) %>%
  mutate(
    image_set_start_date_time = if_else(   #this is because the camera was turned on in March so start date is incorrect
      location == "BMS-CRU-004-01",
      ymd_hms("2021-08-28 12:00:00"), if_else(
        location == "ENWA-O-9-2",
        ymd_hms("2021-11-08 10:57:00"), if_else(
        location == "ENWA-O-3-2",
        ymd_hms("2021-11-06 15:32:24"), if_else(
          location == "ENWA-O-3-3",
          ymd_hms("2021-11-06 15:12:52"), image_set_start_date_time))))) %>% 
  mutate(
    image_set_end_date_time = if_else(
      location == "ENWA-O-9-2",
      ymd_hms("2022-11-06 10:33:00"),
      image_set_end_date_time))

## Rerun dep_time_summary to check again
dep_time_summary <- dep_df %>%
  group_by(study_area) %>%
  summarise(image_set_start_date_time = min(image_set_start_date_time),
            image_set_end_date_time = max(image_set_end_date_time))
## Looks okay (Edehzhie did have a few deployments in Feb 2021)

## Note that for ENWA-O-9-2 the date and time for all species detections will be wrong!! (confirmed that detection phenology plot does have caribou detections in 2020, which is wrong)

glimpse(dep_df)
## Want study_area and location to be factors in both dep_df and img_df
dep_df$study_area <- as.factor(dep_df$study_area)
dep_df$location <- as.factor(dep_df$location)

glimpse(img_df)
img_df$study_area <- as.factor(img_df$study_area)
img_df$location <- as.factor(img_df$location)

## Save deployment data across projects
write.csv(dep_df, "data/all_projects_deployment_data_20260312.csv")

## Save dep_time_summary for easy referencing
write.csv(dep_time_summary, "data/all_projects_dep_period_summary_20260312.csv")


### Check out of range dates in img_df
table(img_df$image_fov)


## Calculate survey effort by creating a daily camera activity matrix
glimpse(dep_df)
class(dep_df$image_set_start_date_time)


## Create a daily camera activity lookup
#convert deployment start and end date and time columns to date objects
dep_df$start_date <- as.Date(dep_df$image_set_start_date_time, format = "%Y-%m-%d")

dep_df$end_date <- as.Date(dep_df$image_set_end_date_time, format = "%Y-%m-%d")

#create a column of number of days deployed
dep_df$days_active <- interval(dep_df$start_date, dep_df$end_date)/ddays(1)

hist(dep_df$days_active)

# Remove any deployments without end dates
tmp <- dep_df[is.na(dep_df$end_date)==F,]

# Create an empty list to store our days
daily_lookup <- list()

# Loop through the deployment dataframe and create a row for every day the camera is active
for(i in 1:nrow(tmp))
{
  if(ymd(tmp$start_date[i])!=ymd(tmp$end_date[i]))
  {
    daily_lookup[[i]] <- data.frame("date"=seq(ymd(tmp$start_date[i]), ymd(tmp$end_date[i]), by="days"), "location"=tmp$location[i])
  }
}

# Merge the lists into a dataframe
row_lookup <- bind_rows(daily_lookup)

# Remove duplicates - when start and end days are the same for successive deployments
row_lookup <- row_lookup[duplicated(row_lookup)==F,]

#### Plotting camera activity per study area


## are there NAs in any location, start date or end date?
table(is.na(dep_df$location)) #no
table(is.na(dep_df$start_date)) #no
table(is.na(dep_df$end_date)) #no
table(is.na(dep_df$study_area)) #no

# Ensure location is a factor with desired ordering
dep_df <- dep_df %>%
  mutate(location = factor(location))

## faceted ggplot for camera activity by study area
facet_cam_activity <- ggplot(dep_df) +
  geom_segment( # add a horizontal segment for each location
    aes(
      x = start_date, # start_date for start of segment
      xend = end_date, #end_date for end
      y = location, #location for position on y-axis
      yend = location
    ),
    linewidth = 1
  ) +
  geom_point(aes(x = start_date, y = location), size = 2) + # add a point at start date
  geom_point(aes(x = end_date, y = location), size = 2) + # add a point at end date
  facet_wrap(~ study_area, scales = "free_y") +
  labs(
    title = "Camera Activity by Study Area",
    x = "Date",
    y = "Location"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 9)
  )

win.graph()
facet_cam_activity

## one TDN camera active until 2023, which one?
tdn_check <- dep_df %>% filter(study_area == "ThaideneNëné")
max(tdn_check$end_date)
tdn_check$location[tdn_check$end_date == "2023-07-10"] # BIO-TDN-29-02 - true, it was deployed until 2023

## Now I want to add the out of range dates to each location

## Split image_date_time into a separate date column
glimpse(img_df)
class(img_df$image_date_time)
img_df$image_date <- as.Date(img_df$image_date_time, format = "%Y-%m-%d")
glimpse(img_df)

oor_tl <- img_df %>%
  # keep only OOR time-lapse images
  filter(image_fov == "OOR",
         image_trigger_mode == "Time Lapse")


## Filter for out of range images in img_df and sort them into distinct intervals per location
oor_intervals <- img_df %>%
  # keep only OOR time-lapse images
  filter(image_fov == "OOR",
         image_trigger_mode == "Time Lapse") %>%
  
  # ensure we are working with a Date
  mutate(oor_date = as.Date(image_date)) %>%
  
  arrange(study_area, location, oor_date) %>%
  group_by(study_area, location) %>%
  
  # group consecutive days: same value => same run
  mutate(
    consec_group = as.integer(oor_date) - row_number()
  ) %>%
  
  group_by(study_area, location, consec_group) %>%
  summarise(
    oor_start = min(oor_date),
    oor_end   = max(oor_date),
    .groups = "drop"
  ) %>%
  
  arrange(study_area, location, oor_start, oor_end)



glimpse(oor_intervals)
summary(oor_intervals)


## Convert to a df with one row per location and multiple OOR intervals (if applicable)
oor_wide <- oor_intervals %>%
  group_by(study_area, location) %>%
  mutate(interval_num = row_number()) %>% # number intervals per location
  ungroup() %>%
  pivot_wider(
    id_cols = c(study_area, location),
    names_from = interval_num, #generate interval number
    values_from = c(oor_start, oor_end),
    names_glue = "{.value}_{interval_num}" ## add interval number to interval_start and _end
  )

## Re-order so start and end of same interval are together
oor_wide <- oor_wide %>%
  # extract interval numbers from column names
  {
    interval_nums <- names(.) %>%
      str_extract("^oor_start_(\\d+)$") %>%
      str_extract("\\d+") %>%
      na.omit() %>%
      as.integer() %>%
      sort()
    
    # build desired column order: start_1, end_1, start_2, end_2, ...
    interval_cols <- unlist(
      lapply(interval_nums, function(i) {
        c(paste0("oor_start_", i),
          paste0("oor_end_", i))
      })
    )
    
    # select in the correct order
    select(., study_area, location, all_of(interval_cols))
  }


glimpse(oor_wide) ## 17 separate OOR intervals



## Checking locations with multiple intervals
## ENWA-O-24-4 has non-noon timelapse images after bear moves camera, which mixes up the intervals. Fix manually to one continuous OOR interval
oor_wide[oor_wide$location == "ENWA-O-24-4", "oor_end_1"] <- as.Date("2022-10-21")
## Revise other OOR intervals to NA

# Identify the columns from oor_start_2 through oor_end_5
cols_to_na <- names(oor_wide)[
  match("oor_start_2", names(oor_wide)) :
    match("oor_end_5",   names(oor_wide))
]

# Set them to NA for the target location
oor_wide[oor_wide$location == "ENWA-O-24-4", cols_to_na] <- NA

###### REVISED UP TO HERE - keep checking locations with LOTS of OOR intervals? Skip manual check for now (Mar 19 2026) ###

# ## BMS-NRA-050-16 has 2 OOR intervals, but not parsed as shown here for some reason. Revise manually:
# oor_wide[oor_wide$location == "BMS-NRA-050-16", "interval_end_1"] <- as.POSIXct("2023-03-20 12:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC") # specify UTC time zone so it doesn't automatically correct
# oor_wide[oor_wide$location == "BMS-NRA-050-16", "interval_start_2"] <- as.POSIXct("2023-04-23 12:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# oor_wide[oor_wide$location == "BMS-NRA-050-16", "interval_end_2"] <- as.POSIXct("2023-04-27 12:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

summary(oor_wide)
summary(oor_wide$interval_start_2)

## Add oor_wide intervals to dep_df with left_join
glimpse(dep_df)
dep_df_oor <- dep_df %>% left_join(oor_wide, by = c("study_area", "location"))

glimpse(dep_df_oor)


### Removing OOR periods from camera activity plot ####


# 1. Pivot OOR intervals back into long format
oor_long <- dep_df_oor %>%
  pivot_longer(
    cols = matches("oor_(start|end)_"), # look for any oor_start_n or oor_end_n columns
    names_to = c(".value", "interval_id"), # turn the value of interest (start/end) and the n (interval_id) into columns
    names_pattern = "oor_(start|end)_(\\d+)" #name new columns with start, end, and interval (ID'd by \\d+)
  ) %>%
  rename(
     oor_start = start,
     oor_end   = end
   ) #%>%
   #filter(!is.na(oor_start) & !is.na(oor_end)) # removing any NA oor_starts and oor_ends

length(unique(oor_long$location)) # so only locations with OOR  intervals appear in this dataframe

table(is.na(oor_long$interval_id))

# 2. For each camera deployment, subtract OOR intervals from active interval
# This function takes:
# - start:      the camera deployment start time
# - end:        the camera deployment end time
# - gaps_df:    a dataframe containing OOR start/end times for that camera
#
# It returns a tibble of "in‑range" time intervals that exclude OOR gaps.
#
# Example:
# Deployment: 0 -------------------------- 100
# OOR gaps:        20---30      50----70
#
# Output segments:
#   0---20   30---50   70---100

make_inrange_segments <- function(start, end, gaps_df) {
  
  # ------------------------------------------------------------
  # 1. If there are NO OOR intervals for this camera:
  #    Return the full deployment interval unchanged.
  # ------------------------------------------------------------
  if (nrow(gaps_df) == 0) {
    return(tibble(x = start, xend = end))
  }
  
  # ------------------------------------------------------------
  # 2. Begin with one segment: the full deployment interval.
  #    As we process each gap, this may be split into multiple segments.
  # ------------------------------------------------------------
  segs <- tibble(x = start, xend = end)
  
  # ------------------------------------------------------------
  # 3. Loop through EACH OOR interval for this camera.
  #    Each gap may split some segments into two pieces.
  # ------------------------------------------------------------
  for (i in seq_len(nrow(gaps_df))) {
    
    # Extract the ith OOR interval:
    # g1 = OOR start, g2 = OOR end
    g1 <- gaps_df$oor_start[i]
    g2 <- gaps_df$oor_end[i]
    
    # ------------------------------------------------------------
    # 4. Apply this gap to ALL existing segments one by one.
    #    rowwise() ensures x and xend represent ONE segment at a time.
    # ------------------------------------------------------------
    segs <- segs %>%
      rowwise() %>%
      do({
        
        # Current segment boundaries
        s <- .$x
        e <- .$xend
        
        # ------------------------------------------------------------
        # 4a. CASE: Gap does NOT overlap the segment at all.
        #      Keep the segment unchanged.
        # ------------------------------------------------------------
        if (g2 <= s | g1 >= e) {
          tibble(x = s, xend = e)
          
        } else {
          
          # ------------------------------------------------------------
          # 4b. CASE: Gap DOES overlap the segment.
          #
          #     Example:
          #     Segment: 10 ---------------------- 50
          #     Gap:              25 ---- 35
          #
          #     Output segments:
          #       10 --- 25    and    35 --- 50
          #
          #     More edge case handling inside the bounds.
          # ------------------------------------------------------------
          
          tibble(
            # left piece before the gap
            x    = c(s, max(g2, s)),
            # right piece after the gap
            xend = c(min(g1, e), e)
          ) %>%
            # Discard invalid segments (where start >= end)
            filter(x < xend)
        }
        
      }) %>% 
      ungroup()
  }
  
  # ------------------------------------------------------------
  # 5. After all gaps have been processed, segs contains ONLY
  #    the "in‑range" time intervals. Return that.
  # ------------------------------------------------------------
  return(segs)
}

# 3. Build final table of "in-range" segments
inrange_df <- dep_df_oor %>%
  rowwise() %>%
  mutate(
    segments = list(
      make_inrange_segments(
        start_date, end_date,
        oor_long %>% filter(.data$location == !!location)
      )
    )
  ) %>%
  unnest(segments) %>%
  ungroup()

# 4. Plot with gaps where OOR occurred
facet_cam_activity <- ggplot(inrange_df) +
  geom_segment(aes(
    x = x, xend = xend,
    y = location, yend = location
  ), linewidth = 1) +
  geom_point(aes(x = start_date, y = location), size = 2) +
  geom_point(aes(x = end_date, y = location), size = 2) +
  facet_wrap(~ study_area, scales = "free_y") +
  labs(
    title = "Camera Activity by Study Area",
    x = "Date",
    y = "Location"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 9)
  )

facet_cam_activity


# ## Still need to adjust days_active with OOR days, and update daily_lookup/row_lookup, camera activity plots, etc
# glimpse(dep_df)
# 
# 
# ### Calculate the total number of OOR days per location
# int1_days <- interval(dep_df$interval_start_1, dep_df$interval_end_1)/ddays(1)
# int2_days <- interval(dep_df$interval_start_2, dep_df$interval_end_2)/ddays(1)
# inactive_intervals <- cbind.data.frame(int1_days, int2_days)
# inactive_intervals$total_inactive <- rowSums(inactive_intervals, na.rm = TRUE)
# 
# summary(inactive_intervals)
# ## Add to dep_df
# dep_df$total_oor <- inactive_intervals$total_inactive
# glimpse(dep_df)
# 
# 
# ## Add total active days, considering oor days
# dep_df$days_active_within_range <- dep_df$days_active - dep_df$total_oor
# 
# summary(dep_df)
# 
# ## Manually confirm cameras if days_active_within_range < 50
# inactive_50 <- dep_df[dep_df$days_active_within_range < 50, ] ## too many have low active days, need to confirm OOR intervals were calculated correctly above
