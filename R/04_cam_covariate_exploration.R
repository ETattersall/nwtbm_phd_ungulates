#######################################
### 04_cam_covariate_exploration.R
### Exploring covariate data in preparation for modelling (following Zuur et al. 2010)
### Started by Erin Tattersall on Oct 7 2026
#######################################


list.of.packages <- c("tidyverse", "ggplot2", "lattice", "here", "stringr", "purrr", "ggpubr", "car", "lme4", "interactions")
lapply(list.of.packages, require, character.only = TRUE)


##### Load the station data from cameras, land, and climate covariates. This analysis will be at the station level, so load station-level covariates #####
## Camera data - all species
cam_det <- read.csv("data/camera_data/nwtbm_allspecies_seasonal_detections.csv")

glimpse(cam_det)
cam_det$X <- NULL
summary(cam_det)



## Landcover covariates-  stn
stn_cov <- read.csv("data/cov_data/nwtbm_sensor_covariate_data.csv")
glimpse(stn_cov) # includes topography, prop_burned, and fireage, but NOT severity
summary(stn_cov) #fireage has 423 (>50%) NAs - unburned sites


## load fire severity data
stn_fsev <- read.csv("data/cov_data/nwtbm_sensor_ntems_fireseverity.csv")
glimpse(stn_fsev)
summary(stn_fsev)


## mean_severity is the mean calculated after non-fire cells were removed and the fire selection rule was applied
## fsev_mean_raw is the raw mean calculated directly from fsev data
## fsev_mean_max is the raw max

### Climate data -stn
stn_clim <- read.csv("data/cov_data/nwtbm_sensor_climate_covariate_data.csv")
summary(stn_clim)
glimpse(stn_clim)


## multiple rows per station/site if the sensor was active in the same month during consecutive years


## camera data don't have columns for site - needs to be added to aggregate at site level (add from sites in landcover dataset below)
stn_sites <- stn_cov |> select(study_area, location, site)

cam_det <- cam_det |> 
  left_join(stn_sites,
            by = c("study_area", "location")) |> 
  select(study_area, location, site, season, active_days, Bison, Moose, Muskox, Woodland.Caribou, Snowshoe.Hare, Ptarmigans, Rock.Ptarmigan, Ruffed.Grouse, Sharptailed.Grouse, Spruce.Grouse, Willow.Ptarmigan) # re-ordering columns 

glimpse(cam_det)
summary(cam_det)



#### Combine response variables (camera data) with covariates #####

#### Include climate data? ####
## note: climate data extracted with the climr package, which is not available for the most recent version of R. Extracted with R 4.5.1
## Climate data aggregated at the seasonal level may not have much variance, nor will it likely tell us much about seasonal habitat use

## convert to long format (this will duplicate any annual climate data)
## Station data
stn_clim_long <- stn_clim |>
  pivot_longer(
    cols = matches("_(0[1-9]|1[0-2])$"), ## for any column with a monthly integer (01 - 12)
    names_to = c(".value", "month"), ## column names will be the initial column identifier and month
    names_pattern = "(.*)_(\\d{2})" ## pattern to follow for the column values will be the column identifier itself and the the monthly 2-digit integer
  ) |>
  mutate(
    month = as.integer(month))

## Remove any annual or quarterly data, Tmin, and Tmax (will only focus on PAS, PAT, and Tave)
stn_clim_long <- stn_clim_long[ , c(3:7, 12, 41:44)]

glimpse(stn_clim_long)
summary(stn_clim_long) # No NAs



######## Quick aside: Determining Seasonal breakpoints (dividing into seasonal models) ####
## Summarize across locations to plot precipitation as snow
precip_plot_dat <- stn_clim_long |>
  group_by(study_area,month) |>
  summarise(
    PAS = mean(PAS, na.rm = TRUE), ## averaging across all years
    .groups = "drop"
  )
glimpse(precip_plot_dat)


## plot Snow
snow.plot <- ggplot(precip_plot_dat,
                    aes(x = month, y = PAS, colour = study_area)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    x = "Month",
    y = "Precipitation as Snow (cm)", ## the variable says it's measured in mm, but that seems highly unlikely (max 40 mm snow?! no way)
    colour = NULL
  ) +
  theme_bw() +
  # increase size of title text, axis text
  theme(plot.title = element_text(size = 32, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 16)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8)) +
  theme(strip.text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 18))

win.graph()
snow.plot
## Snow-free period is on average June - Sep. Snow season = October - May ###
ggsave("figures/Monthly_snow_by_study_area.png", snow.plot, width = 18, height = 12, dpi = 300)

### Plot min, mean, and max temperatures
temp_plot_dat <- stn_clim_long |>
  group_by(month) |>
  summarise(
    Tave = mean(Tave, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(temp_plot_dat,
       aes(x = month, y = Tave)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    x = "Month",
    y = expression(Temperature~(degree*C)),
    colour = NULL
  ) +
  theme_bw()

## no real temperature breakpoints - season better determined by snow

## expect linear relationship between temperature and snow (negative)

hist(stn_clim_long$Tave)
hist(stn_clim_long$PAS)

lm(PAS ~ Tave, data = stn_clim_long) |> summary()

glimpse(stn_clim_long)

snow_temp <- ggplot(stn_clim_long) +
  aes(x = Tave, y = PAS) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  stat_regline_equation(
    aes(label = paste(..rr.label..)),
    label.x.npc = "left",
    label.y.npc = 0.95,
    size = 10
  ) +
  labs(title = "Climate data relationships",
       x = "Mean monthly temperature (C)",
       y = "Mean Snowfall (cm)",) +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 32, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 24)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(axis.text = element_text(size = 20))

win.graph()
snow_temp

ggsave("figures/data_exploration/climate_data_relationships.png", snow_temp, width = 18, height = 12, dpi = 300)




## Examine climate data by season for variance
glimpse(stn_clim_long)

# create vector of columns for seasonal data
seas_cols <- c("PAS", "PPT", "Tave")

## Lump months into seasons
seasonal_clim <- stn_clim_long |>
  mutate(
    season = factor( ## adding a variable for snow and snow-free
      case_when(
        month %in% 6:9 ~ "snow-free",
        month %in% c(1:5, 10:12) ~ "snow"
      ),
      levels = c("snow", "snow-free")
    )) |> 
  group_by(study_area, site, location, lon, lat, season) |> ## groups that won't be summarized (all temporal variables will be lumped)
  summarise(across(all_of(seas_cols), ## calculate the mean of each of the columns in seas_cols by season, location, and study_area
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

glimpse(seasonal_clim)


## Box plots of average temperature by season

seas_temp <- ggplot(seasonal_clim,
                 aes(x = season, y = Tave)) +
  geom_boxplot() +
  labs(y = "Mean Temp (C)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24))

win.graph()
seas_temp # mean snow season temperature between -17 - -7; snow-free between 10 - 15. Do we expect an ecological difference in habitat use across this range? (No)

##### Don't include climate data as covariates


#### Add fire severity to station data ####
glimpse(stn_cov)
stn_cov$X <- NULL


## Add binary variable for burned/unburned based on FireAge
stn_cov <- stn_cov |> 
  mutate(burned_binary = ifelse(is.na(FireAge), 0, 1)) # if FireAge is NA, burned_binary should be 0, otherwise, 1

table(stn_cov$burned_binary) # 423 NAs in FireAge, so 423 0s (unburned)

## Add fire severity data
glimpse(stn_fsev) # keep only location/site/study_are keys, fire_year, mean_severity, fsev_max_raw, and fsev_mean_raw
hist(stn_fsev$mean_severity)
hist(stn_fsev$fsev_mean_raw) ## almost identical. Remove raw mean 

stn_fsev <- stn_fsev |> 
  select(study_area, site, location, fire_year, mean_severity, fsev_max_raw)

stn_cov <- stn_cov |> 
  left_join(stn_fsev,
            by = 
              c("study_area", "site", "location"))

glimpse(stn_cov) ### Contains all station landcover, topography, and fire covariates

## Landcover and topography covariates will likely be dropped. The focus question is the effect of fire disturbance, so these are the primary covariates
## Since landcover is also affected by disturbance (fire), it would not be included in the adjustment set of the causal model (see DAGs)

## Test topography variance
topo <- ggplot(stn_cov,
                    aes(y = tri_500m)) +
  geom_boxplot() +
  labs(y = "Terrain Ruggedness Index") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24))

win.graph()
topo ## values between 0-20, indicating very gradual changes in ruggedness

## Topography has very low variation and likely would not produce an ecological effect on space use, so shouldn't be included in modelling

glimpse(stn_cov) ## keep the fire variables only
fire_cov <- stn_cov |> 
  mutate( ## convert to factors
  study_area = factor(study_area),
  location   = factor(location),
  site       = factor(site)
) |> 
  select(study_area, location, site, burned_binary, proportion_burned_500m, fire_year, FireAge, mean_severity, fsev_max_raw)

glimpse(fire_cov)
summary(fire_cov)

## rename fire variables for simplicity
colnames(fire_cov) <- c("study_area", "location", "site", "burned", "prop_burned", "fire_year", "burn_age", "burn_severity", "fsev_max")

### Add fire covariates to seasonal camera data
glimpse(cam_det)

## convert study_area, location, site, season into factors in cam_det
cam_det <- cam_det |>
  mutate(
    study_area = factor(study_area),
    location   = factor(location),
    site       = factor(site),
    season     = factor(season),
  )


cam_stn_cov <- cam_det |> 
  left_join(fire_cov, by = c("study_area", "location", "site"))

glimpse(cam_stn_cov)
summary(cam_stn_cov) ## only time since fire and burn severity columns have NAs (which is correct)



### Can't figure out why there are fewer Ptarmigan seasonal detections than WIPT
sum(cam_stn_cov$Ptarmigans)
sum(cam_stn_cov$Willow.Ptarmigan)

table(cam_stn_cov$Ptarmigans)
table(cam_stn_cov$Willow.Ptarmigan)

## Correct here - so why does the range for Ptarmigans only go up to 5 in the box plot?

## Fort Smith 2023 fire stations - 13 Fort Smith stations experienced fire in May and July of 2023. Remove these from analysis (fire variables changed during study period)
summary(cam_stn_cov$burn_age)
## Only remove stations with burn age 1 (only stations affected by fire during study period)
cam_stn_cov <- cam_stn_cov |> filter_out(burn_age == 1)


## Are stations with burn age 2 okay? Some areas were surveyed for multiple years
cam_stn_cov |> filter(burn_age ==2) # yes - these burned in 2022, and monitoring began post-fire season.

## Burn severity data only exists for years 1985 - 2022 in this dataset. Severity data outside that range should be NA
pre_ntems <- cam_stn_cov |> filter(burn_age > 37) ## 1985 is 37 years before 2022

### Correct - fires pre-1985 have NA for severity. Fire_year and burn_Age don't always line up, probably because of the rule we applied for which polygon to use. Disregard fire year

## Save station data with categoricals as factors and without fire-affected data
write.csv(cam_stn_cov, "data/nwtbm_camera_stations_seasonal_model_data.csv")

###### Data Exploration #####

cam_stn_cov <- read.csv("data/nwtbm_camera_stations_seasonal_model_data.csv")
glimpse(cam_stn_cov)

## If re-reading in, check that factor variables are correct!
cam_stn_cov$X <- NULL
cam_stn_cov <- cam_stn_cov |> 
  mutate(
    study_area = factor(study_area),
    location   = factor(location),
    site       = factor(site),
    season     = factor(season)
         )

## Split into snow and snow free seasons
snow_data <- cam_stn_cov |> filter(season == "snow")
snowfree_data <- cam_stn_cov |> filter(season == "snow-free")

summary(snow_data)
summary(snowfree_data)

## Following Zuur et al. 2010, following the script originally created by Val Lucet: https://github.com/VLucet/Zuur-et-al-10-steps/tree/main

## Identifying sets of variables in the dataframe (same in summer and winter datasets)
resp.vars <- colnames(cam_stn_cov[ , 6:16]) ## columns containing camera data
cov.vars <- colnames(cam_stn_cov[ , c(5, 17:22)]) ## columns for covariate data (include active_days!)

#### 1. Outliers in the covariates ####

## Covariates should be the same for snow and snow free datasets, except for number of active days

## Different study areas are included for different species. Start with moose, which include all study areas

## SPGR, STGR, and Ptarmigans also occur in all study areas

## Faceted Cleveland dot plots

## Pivot longer to plot all variables together
## covariates
cov_long_ranked <- snow_data |>
  pivot_longer(
    cols = all_of(cov.vars),
    names_to = "variable",
    values_to = "value"
  ) |>
  arrange(value, .by_group = TRUE) |>
  mutate(rank = row_number()) |> # ranking observation to better view patterns
  ungroup()


### Ranked Cleveland Dot plot for fire covariates
glimpse(cov_long_ranked)

cov_cleve <- cov_long_ranked |> 
  ggplot(aes(x = value, y = rank)) +
  geom_point(alpha = 0.6, size = 1) +
  facet_wrap(~ variable,
             scales = "free_x") +
  labs(
    x = xlab, # xlab = summer/winter detections/covariates
    y = "Ranked observation"
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 24)) +
  theme(strip.text = element_text(size = 18))

win.graph()
cov_cleve

### save
ggsave("figures/data_exploration/covariate_cleveland_dot_plots.png", cov_cleve, width = 18, height = 12, dpi = 300)


#### Response variables - Moose


## Snow data

## Cleveland dot plot
ggplot(snow_data) + 
  geom_point(aes(x = Moose, y = 1:nrow(snow_data))) +
  labs(x = "Moose",
       y = "Order of the data",
       title = "Snow Season") +
  theme_bw()

## Snow-free data
ggplot(snowfree_data) + 
  geom_point(aes(x = Moose, y = 1:nrow(snowfree_data))) +
  labs(x = "Moose",
       y = "Order of the data",
       title = "Snow-free Season") +
  theme_bw()

#### 2. Homogeneity of variance - are there patterns in how the data vary?

glimpse(cam_stn_cov)

## Check each variable's variance across study areas using box plots

## Pivot longer for plotting (not ranked)
cov_long <- snow_data |>
  pivot_longer(
    cols = all_of(cov.vars),
    names_to = "variable",
    values_to = "value"
  )


## Multi-variable box plot for study area
cov_sa <- ggplot(cov_long,
       aes(x = study_area, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(y = "Covariate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 18))

win.graph()
cov_sa

## Save 
ggsave("figures/data_exploration/covariate_studyarea_variance.png", cov_sa, width = 18, height = 12, dpi = 300)



### Levene's test for study area - p > 0.05 indicates variance across groups is similar enough to be considered equal
## Create tibble testing variance across multiple grouping variables

group_vars <- c(
  "study_area",
  "site"
)

levene_results <- expand_grid(
  variable = cov.vars, # test for all variables
  group = group_vars
) |>
  rowwise() |>
  mutate(
    p_value = {
      f <- reformulate(group, response = variable)
      car::leveneTest(f, data = cam_stn_cov)$`Pr(>F)`[1]
    }
  ) |>
  ungroup() |>
  pivot_wider(
    names_from = group,
    values_from = p_value
  )

levene_results |>
  filter(if_any(-variable, ~ .x < 0.05))

## High degree of variance for all covariates across study_areas and sites (use site and study_area as random effects)


## Response variables - Moose detections across seasons (use full df to plot seasons together)
Moose_season <- ggplot(cam_stn_cov,
       aes(x = study_area, y = Moose)) +
  geom_boxplot() +
  facet_wrap(~season, scales = "free_y") +
  labs(y = "Moose Detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))

Moose_season

ggsave("figures/data_exploration/Moose_season_variance.png", Moose_season, width = 18, height = 12, dpi = 300)


## Ptarmigans across seasons
ptarm_season <- ggplot(cam_stn_cov,
                       aes(x = study_area, y = Ptarmigans)) +
  geom_boxplot() +
  facet_wrap(~season, scales = "free_y") +
  labs(y = "Ptarmigan Detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))

ptarm_season

ggsave("figures/data_exploration/Ptarmigan_season_variance.png", ptarm_season, width = 18, height = 12, dpi = 300)

## Spruce grouse across seasons
spgr_season <- ggplot(cam_stn_cov,
                       aes(x = study_area, y = Spruce.Grouse)) +
  geom_boxplot() +
  facet_wrap(~season, scales = "free_y") +
  labs(y = "SPGR Detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))

spgr_season

ggsave("figures/data_exploration/spgr_season_variance.png", spgr_season, width = 18, height = 12, dpi = 300)

## Sharp-tailed grouse across seasons
stgr_season <- ggplot(cam_stn_cov,
                      aes(x = study_area, y = Sharptailed.Grouse)) +
  geom_boxplot() +
  facet_wrap(~season, scales = "free_y") +
  labs(y = "STGR Detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))

stgr_season

ggsave("figures/data_exploration/stgr_season_variance.png", stgr_season, width = 18, height = 12, dpi = 300)

### 3. Normality and 4. Zero-inflation
## (don't really expect (m)any of the variables to be normal)
## Zero-inflation is unavoidable

## Histogram of covariates
cov_hist <- ggplot(cov_long,
                  aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = "free_x") +
  labs(y = "Covariates") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))


win.graph()
cov_hist

## save
ggsave("figures/data_exploration/covariate_histograms.png", cov_hist, width = 18, height = 12, dpi = 300)


## Moose
Moose_hist <- ggplot(cam_stn_cov,
                   aes(x = Moose)) +
  geom_histogram(bins = 30) +
  facet_wrap(~season, scales = "free_x") +
  labs(y = "Moose detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))


win.graph()
Moose_hist ## soooo zero-inflated!!

# save
ggsave("figures/data_exploration/Moose_histograms.png", Moose_hist, width = 18, height = 12, dpi = 300)

## Ptarmigans
Ptarmigans_hist <- ggplot(cam_stn_cov,
                     aes(x = Ptarmigans)) +
  geom_histogram(bins = 30) +
  facet_wrap(~season, scales = "free_x") +
  labs(y = "Ptarmigan detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))


win.graph()
Ptarmigans_hist ## soooo zero-inflated!!

# save
ggsave("figures/data_exploration/Ptarmigan_histograms.png", Ptarmigans_hist, width = 18, height = 12, dpi = 300)

## SPGR
SPGR_hist <- ggplot(cam_stn_cov,
                     aes(x = Spruce.Grouse)) +
  geom_histogram(bins = 30) +
  facet_wrap(~season, scales = "free_x") +
  labs(y = "SPGR detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))


win.graph()
SPGR_hist ## soooo zero-inflated!!

# save
ggsave("figures/data_exploration/SPGR_histograms.png", SPGR_hist, width = 18, height = 12, dpi = 300)

## STGR
STGR_hist <- ggplot(cam_stn_cov,
                    aes(x = Sharptailed.Grouse)) +
  geom_histogram(bins = 30) +
  facet_wrap(~season, scales = "free_x") +
  labs(y = "STGR detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))


win.graph()
STGR_hist ## soooo zero-inflated!!

# save
ggsave("figures/data_exploration/STGR_histograms.png", STGR_hist, width = 18, height = 12, dpi = 300)



#### 5. Collinearity ####

# Compute the correlation matrix
glimpse(snow_data) ## only test continuous variables that will be included in models

model.var <- cam_stn_cov |> 
  select(active_days, Bison, Moose, Muskox, Woodland.Caribou, Snowshoe.Hare, Ptarmigans, Ruffed.Grouse, Sharptailed.Grouse, Spruce.Grouse, 
         prop_burned, burn_age, burn_severity, fsev_max)

cor_matrix <- cor(
  model.var,
  use = "pairwise.complete.obs" # remove NAs
)

# Visualize the correlation matrix
win.graph()
corplot <- corrplot::corrplot(
  cor_matrix,
  method = "color", # fill full cell
  type = "upper", # only upper half of pairs
  order = "hclust", # hierarchical clustering
  addCoef.col = "black", # black coefficients added
  number.cex = 1.0,
  tl.col = "black",
  tl.cex = 1.1,
  tl.srt = 45
)



## To save, open a graphics device with defined ratio and resolution first
png(
  filename = "figures/data_exploration/fire_variables_correlation_plot.png",
  width = 12000,
  height = 8000,
  res = 600
)
corrplot::corrplot(
  cor_matrix,
  method = "color", # fill full cell
  type = "upper", # only upper half of pairs
  order = "hclust", # hierarchical clustering
  addCoef.col = "black", # black coefficients added
  number.cex = 1.2, # size of coefficients
  cl.cex = 1.2, # size of numbers in legend
  tl.col = "black", #axis text
  tl.cex = 1.4,
  tl.srt = 45
)

dev.off()

## only strongly correlated variables are the two severity variables


### 7. Interactions
## We hypothesize that the effect of prop_burned will vary with age and severity

m1.snow <- glm(Moose ~ prop_burned*burn_age, data = snow_data, family = "poisson")
summary(m1.snow) # slightly positive interaction

## Plot interaction
interact_plot(m1.snow, pred = prop_burned, modx = burn_age, modx.values = "terciles")
# suggested moose detections are higher with higher proportion burned, and that this effect is greater the older a fire is

m1.sf <- glm(Moose ~ prop_burned*burn_age, data = snowfree_data, family = "poisson")
summary(m1.sf) # slightly positive interaction

## Plot interaction
interact_plot(m1.sf, pred = prop_burned, modx = burn_age, modx.values = "terciles")
# suggested moose detections are higher with higher proportion burned, and that this effect is greater the older a fire is. Effect stronger in the summer

m2.snow <- glm(Moose ~ prop_burned*burn_severity, data = snow_data, family = "poisson")
summary(m2.snow)

interact_plot(m2.snow, pred = prop_burned, modx = burn_severity, modx.values = c(0.25, 0.5, 0.75)) #specify burn severity values to test
## so moose detections may be higher with higher proportions burned, but only at low severity burns. At higher severity, effect reverses
# at moderate severity, the positive effect of burn size is diminished

## snow-free
m2.sf <- glm(Moose ~ prop_burned*burn_severity, data = snowfree_data, family = "poisson")
summary(m2.sf)

interact_plot(m2.sf, pred = prop_burned, modx = burn_severity, modx.values = c(0.25, 0.5, 0.75)) #specify burn severity values to test
## no sig. interaction in the summer




#### Snow and snow-free datasets for other species ####
## Datasets for non-moose ungulate species and ruffed grouse


#### Woodland caribou
wcaribou_studyareas <- c("Edéhzhíe", "NormanWells", "SambaaK'e")
caribou_stns <- cam_stn_cov |> 
  filter(study_area %in% wcaribou_studyareas)

## Subset by season
snow_caribou <- caribou_stns |> filter(season == "snow")
snowfree_caribou <- caribou_stns |> filter(season == "snow-free")

## How many data points do we have for each model? Subtract NAs in burn_age and burn_severity from total observations
summary(snow_caribou)
summary(snowfree_caribou)

## Distribution of caribou data in snow and snow-free seasons
caribou_hist <- ggplot(caribou_stns,
                     aes(x = Woodland.Caribou)) +
  geom_histogram(bins = 30) +
  facet_wrap(~season, scales = "free_x") +
  labs(y = "Caribou detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))


win.graph()
caribou_hist ## soooo zero-inflated!!

# save
ggsave("figures/data_exploration/Caribou_histograms.png", caribou_hist, width = 18, height = 12, dpi = 300)


### Muskox

muskox_studyareas <- c("ThaideneNëné", "NormanWells", "Gameti")

muskox_stns <- cam_stn_cov |> 
  filter(study_area %in% muskox_studyareas)

## Subset by season
snow_muskox <- muskox_stns |> filter(season == "snow")
snowfree_muskox <- muskox_stns |> filter(season == "snow-free")

## How many data points do we have for each model? Subtract NAs in burn_age and burn_severity from total observations
summary(snow_muskox)
summary(snowfree_muskox)

## Distribution of muskox data in snow and snow-free seasons
muskox_hist <- ggplot(muskox_stns,
                       aes(x = Muskox)) +
  geom_histogram(bins = 30) +
  facet_wrap(~season, scales = "free_x") +
  labs(y = "Muskox detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))


win.graph()
muskox_hist ## soooo zero-inflated!!

# save
ggsave("figures/data_exploration/muskox_histograms.png", muskox_hist, width = 18, height = 12, dpi = 300)


### Bison
bison_studyareas <- c("Edéhzhíe", "FortSmith")

bison_stns <- cam_stn_cov |> 
  filter(study_area %in% bison_studyareas)

## Subset by season
snow_bison <- bison_stns |> filter(season == "snow")
snowfree_bison <- bison_stns |> filter(season == "snow-free")

## How many data points do we have for each model? Subtract NAs in burn_age and burn_severity from total observations
summary(snow_bison)
summary(snowfree_bison)

## Distribution of bison data in snow and snow-free seasons
bison_hist <- ggplot(bison_stns,
                      aes(x = Bison)) +
  geom_histogram(bins = 30) +
  facet_wrap(~season, scales = "free_x") +
  labs(y = "Bison detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))


win.graph()
bison_hist ## soooo zero-inflated!!

# save
ggsave("figures/data_exploration/bison_histograms.png", bison_hist, width = 18, height = 12, dpi = 300)


### Ruffed Grouse - based on distribution map on Birds of The World, could expect to find RUGR in Sambaa K'e, Edehzhie, and Fort Smith
## we only detected them on camera in EDE and FS, but detected on ARUs in Gameti and Sambaa K'e as well
## Include SK, EDE, FS, and Gam (exclude TDN and NW)
RUGR_studyareas <- c("Edéhzhíe", "FortSmith", "SambaaK'e", "Gameti")

RUGR_stns <- cam_stn_cov |> 
  filter(study_area %in% RUGR_studyareas)

## Subset by season
snow_RUGR <- RUGR_stns |> filter(season == "snow")
snowfree_RUGR <- RUGR_stns |> filter(season == "snow-free")

## How many data points do we have for each model? Subtract NAs in burn_age and burn_severity from total observations
summary(snow_RUGR)
summary(snowfree_RUGR)

## Distribution of RUGR data in snow and snow-free seasons
RUGR_hist <- ggplot(RUGR_stns,
                     aes(x = Ruffed.Grouse)) +
  geom_histogram(bins = 30) +
  facet_wrap(~season, scales = "free_x") +
  labs(y = "RUGR detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24)) +
  theme(strip.text = element_text(size = 16))


win.graph()
RUGR_hist ## soooo zero-inflated, probably won't be able to model

# save
ggsave("figures/data_exploration/RUGR_histograms.png", RUGR_hist, width = 18, height = 12, dpi = 300)

