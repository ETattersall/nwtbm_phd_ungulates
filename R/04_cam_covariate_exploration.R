#######################################
### 04_cam_covariate_exploration.R
### Exploring covariate data in preparation for modelling (following Zuur et al. 2010)
### Started by Erin Tattersall on Oct 7 2026
#######################################


list.of.packages <- c("tidyverse", "ggplot2", "lattice", "here", "climr", "stringr", "purrr", "ggpubr", "Rcmdr", "car", "lme4", "interactions")
lapply(list.of.packages, require, character.only = TRUE)


##### Load the station data from cameras, land (by site and by station), and climate covariates (by site and by station) #####
## Camera data - all species
cam_mon <- read.csv("data/camera_data/nwtbm_allspecies_detections_by_month.csv")

glimpse(cam_mon)
cam_mon$X <- NULL

## Add all ROPT and WIPT detections to Ptarmigans column
cam_mon$Ptarmigans <- cam_mon$Ptarmigans + cam_mon$Rock.Ptarmigan + cam_mon$Willow.Ptarmigan

## Landcover covariates- site and stn
stn_cov <- read.csv("data/cov_data/nwtbm_sensor_covariate_data.csv")
glimpse(stn_cov) # includes topography, prop_burned, and fireage, but NOT severity
summary(stn_cov) #fireage has 423 (>50%) NAs - unburned sites

site_cov <- read.csv("data/cov_data/nwtbm_sites_covariate_data.csv")
glimpse(site_cov)
summary(site_cov)

## load fire severity data
stn_fsev <- read.csv("data/cov_data/nwtbm_sensor_ntems_fireseverity.csv")
glimpse(stn_fsev)
summary(stn_fsev)

site_fsev <- read.csv("data/cov_data/nwtbm_sites_ntems_fireseverity.csv")
glimpse(site_fsev)
summary(site_fsev)

## mean_severity is the mean calculated after non-fire cells were removed and the fire selection rule was applied
## fsev_mean_raw is the raw mean calculated directly from fsev data
## fsev_mean_max is the raw max

### Climate data - site and stn
stn_clim <- read.csv("data/cov_data/nwtbm_sensor_climate_covariate_data.csv")
summary(stn_clim)
glimpse(stn_clim)
site_clim <- stn_cov <- read.csv("data/cov_data/nwtbm_sites_climate_covariate_data.csv")
glimpse(site_clim)

## multiple rows per station/site if the sensor was active in the same month during consecutive years

## refresher on what all the climate variables are
View(variables)
#MAP = mean annual precipitation
#MAT = mean annual temperature
#PAS = precip. as snow
#PPT = precipitation
#_an = annual
#_at = autumn
#_sm = summer
#_sp = spring
#_wt = winter
#_01-12 = months of year


## camera data don't have columns for site - needs to be added to aggregate at site level (add from sites in landcover dataset below)
stn_sites <- stn_cov |> select(study_area, location, site)

cam_mon <- cam_mon |> 
  left_join(stn_sites,
            by = c("study_area", "location")) |> 
  mutate(month_num = match(month, month.name)) |> ## adding month as numeric (to align with months in climate data)
  select(study_area, location, site, year, month, month_num, year_month, active_days, Bison, Moose, Muskox, Woodland.Caribou, Snowshoe.Hare, Ptarmigans, Rock.Ptarmigan, Ruffed.Grouse, Sharp.tailed.Grouse, Spruce.Grouse, Willow.Ptarmigan) # re-ordering columns and selecting target species

glimpse(cam_mon)
summary(cam_mon)


### Aggregate detection data by site - sum detections at each stations within a site (including active days)
#specify which columns to sum
spp_var <- names(cam_mon)[8:19]


cam_site <- cam_mon |> 
  group_by(study_area, site, year, year_month, month, month_num) |> 
  summarise(
    across(all_of(spp_var), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

glimpse(cam_site)
summary(cam_site)

#### Combine response variables (camera data) with covariates #####

#### Combining detections with Climate data ####
## convert to long format (this will duplicate any annual climate data)
## Station data
stn_clim_long <- stn_clim |>
  pivot_longer(
    cols = matches("_(0[1-9]|1[0-2])$"),
    names_to = c(".value", "month"),
    names_pattern = "(.*)_(\\d{2})"
  ) |>
  mutate(
    month = as.integer(month))

## Remove any annual or quarterly data
stn_clim_long <- stn_clim_long[ , c(3:7, 12, 41:46)]

glimpse(stn_clim_long)
summary(stn_clim_long) # No NAs


### Join climate data to monthly species detections
cam_clim <- cam_mon %>%
  left_join(
    stn_clim_long,
    by = c(
      "study_area",
      "location",
      "site",
      "year" = "PERIOD",
      "month_num" = "month"
    )
  )


glimpse(cam_clim)
summary(cam_clim) ## Climate data contains NAs - need to determine which ones and why

## Which rows are NA? filtered for PAS, but all climate columns are NA
na.clim <- cam_clim |> filter(is.na(PAS))
# NAs for year_months with no climate data - ThaideneNene and Edehzhie both had a few stations deployed until 2023. Revised the end date in the climate script and re-ran


######## Quick aside: Determining Seasonal breakpoints (dividing into seasonal models) ####
## Summarize across locations to plot precipitation as snow
precip_plot_dat <- cam_clim |>
  group_by(study_area,month_num) |>
  summarise(
    PAS = mean(PAS, na.rm = TRUE), ## averaging across all years
    .groups = "drop"
  )
glimpse(precip_plot_dat)


## plot Snow
snow.plot <- ggplot(precip_plot_dat,
                    aes(x = month_num, y = PAS, colour = study_area)) +
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
  theme(strip.text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 18))

win.graph()
snow.plot
## Snow-free period is on average June - Sep. Snow season = October - May ###
ggsave("figures/Monthly_snow_by_study_area.png", snow.plot, width = 18, height = 12, dpi = 300)

### Plot min, mean, and max temperatures
temp_plot_dat <- cam_clim |>
  group_by(month_num) |>
  summarise(
    Tmin = mean(Tmin, na.rm = TRUE),
    Tave = mean(Tave, na.rm = TRUE),
    Tmax = mean(Tmax, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(Tmin, Tave, Tmax),
    names_to = "variable",
    values_to = "temperature"
  )

ggplot(temp_plot_dat,
       aes(x = month_num, y = temperature,
           colour = variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    x = "Month",
    y = expression(Temperature~(degree*C)),
    colour = NULL
  ) +
  theme_bw()

## no real temperature breakpoints - season better determined by snow

## expect linear relationship between temperature and snow (negative)

hist(cam_clim$Tave)
hist(cam_clim$PAS)

lm(PAS ~ Tave, data = cam_clim) |> summary()

glimpse(cam_clim)

snow_temp <- ggplot(cam_clim) +
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


######

## Site data
site_clim_long <- site_clim |>
  pivot_longer(
    cols = matches("_(0[1-9]|1[0-2])$"),
    names_to = c(".value", "month"),
    names_pattern = "(.*)_(\\d{2})"
  ) |>
  mutate(
    month = as.integer(month))
## Remove any annual or quarterly data
site_clim_long <- site_clim_long[ , c(2:3, 32:37)]

### Join climate data to ung_site
cam_clim_site <- cam_site %>%
  left_join(
    site_clim_long,
    by = c(
      "site",
      "year" = "PERIOD",
      "month_num" = "month"
    )
  )

glimpse(cam_clim_site)

#### Add landcover and fire severity to station data ####
glimpse(stn_cov)
stn_cov$X <- NULL

glimpse(cam_clim)
## remove lon and lat from cam_clim
cam_clim <- cam_clim |> 
  select(-lon, -lat)

cam_stn_cov <- cam_clim |> 
  left_join(stn_cov,
            by = c("study_area", "location", "site"))

glimpse(cam_stn_cov)
summary(cam_stn_cov) # only variable with NAs is FireAge, which is right

## Add binary variable for burned/unburned based on FireAge
cam_stn_cov <- cam_stn_cov |> 
  mutate(burned_binary = ifelse(is.na(FireAge), 0, 1)) # if FireAge is NA, burned_binary should be 0, otherwise, 1

table(cam_stn_cov$burned_binary) # 4453 0s, which corresponds to NAs in Fire Age

## Add fire severity data
glimpse(stn_fsev) # keep only location/site/study_are keys, fire_year, mean_severity, fsev_max_raw, and fsev_mean_raw
stn_fsev <- stn_fsev |> 
  select(study_area, site, location, fire_year, mean_severity, fsev_mean_raw, fsev_max_raw)

cam_stn_cov <- cam_stn_cov |> 
  left_join(stn_fsev,
            by = 
              c("study_area", "site", "location"))

glimpse(cam_stn_cov)


#### Add landcover and fire severity to site data
glimpse(site_cov)
site_cov$X <- NULL

cam_site_cov <- cam_clim_site |> 
  left_join(site_cov,
            by = c("study_area", "site"))

glimpse(cam_site_cov)
summary(cam_site_cov) # only variable with NAs is FireAge, which is right

## Add binary variable for burned/unburned based on FireAge
cam_site_cov <- cam_site_cov |> 
  mutate(burned_binary = ifelse(is.na(FireAge), 0, 1)) # if FireAge is NA, burned_binary should be 0, otherwise, 1

table(cam_site_cov$burned_binary) # 1044 0s, which corresponds to NAs in Fire Age

## Add fire severity data
glimpse(site_fsev) # keep only site/study_are keys, fire_year, mean_severity, fsev_max_raw, and fsev_mean_raw
site_fsev <- site_fsev |> 
  select(study_area, site, fire_year, mean_severity, fsev_mean_raw, fsev_max_raw)

cam_site_cov <- cam_site_cov |> 
  left_join(site_fsev,
            by = 
              c("study_area", "site"))

glimpse(cam_site_cov)
summary(cam_site_cov)

## cam_site_cov and cam_stn_cov now contain all response and covariate data! Divide into snow and snow-free datasets prior to modelling

## convert study_area, location, site, and month into factors
cam_stn_cov <- cam_stn_cov |>
  mutate(
    study_area = factor(study_area),
    location   = factor(location),
    site       = factor(site),
    season = factor( ## adding a variable for snow and snow-free
      case_when(
        month %in% c("June", "July", "August", "September") ~ "snow-free",
        month %in% c("October", "November", "December",
                     "January", "February", "March",
                     "April", "May") ~ "snow"
      ),
      levels = c("snow", "snow-free")
    ),
    month      = factor(
      month,
      levels = c(
        "January", "February", "March", "April",
        "May", "June", "July", "August",
        "September", "October", "November", "December"
      ),
      ordered = TRUE
    ),
    year = factor(year)
  )

glimpse(cam_stn_cov)
table(cam_stn_cov$season)

## Save station data with categoricals as factors
write.csv(cam_stn_cov, "data/nwtbm_camera_stations_model_data.csv")
write.csv(cam_site_cov, "data/nwtbm_camera_sites_model_data.csv") ## not factorized yet


###### Data Exploration #####

## Following Zuur et al. 2010, following the script originally created by Val Lucet: https://github.com/VLucet/Zuur-et-al-10-steps/tree/main

### Exploring cam_stn_cov first

## cam_stn_cov needs to be separated into summer and winter datasets
winter_months <- c(1:5, 10:12)
summer_months <- c(6:9)

## filter for winter stations
win_stns <- cam_stn_cov |> filter(month_num %in% winter_months)
sum_stns <- cam_stn_cov |> filter(month_num %in% summer_months)




# Store datasets in a list
datasets <- list(
  Winter = win_stns,
  Summer = sum_stns
)




#### 1. Outliers in the response variable ####
glimpse(win_stns) # columns 8 - 44 are response and predictor variables

## Box plot (single variable)
ggplot(cam_stn_cov) + 
  geom_boxplot(aes(y = Moose)) +
  labs(y = "Monthly moose") +
  theme_bw()

## Cleveland dot plot (single variable)
ggplot(cam_stn_cov) + 
  geom_point(aes(x = Moose, y = 1:nrow(cam_stn_cov))) +
  labs(x = "Monthly Moose",
       y = "Order of the data") +
  theme_bw()

## Identifying sets of variables in the dataframe (same in summer and winter datasets)
resp.vars <- colnames(cam_stn_cov[ , 9:19]) ## columns containing camera data
cov.vars <- colnames(cam_stn_cov[ , c(20:24, 28:44)]) ## columns for covariate data

## Faceted Cleveland dot plots - create function to streamline plotting

make_cleveland <- function(dat, vars, facet_name, xlab) {
  
  dat |> # generating a long-format df from seasonal dataset
    pivot_longer(
      cols = all_of(vars), # vars = object with name of response or covariate variables
      names_to = facet_name, # facet_name = either response or covariate
      values_to = "value"
    ) |>
    group_by(across(all_of(facet_name))) |>
    arrange(value, .by_group = TRUE) |>
    mutate(rank = row_number()) |> # ranking observation to better view patterns
    ungroup() |>
    ggplot(aes(x = value, y = rank)) +
    geom_point(alpha = 0.6, size = 1) +
    facet_wrap(as.formula(paste("~", facet_name)),
               scales = "free_x") +
    labs(
      x = xlab, # xlab = summer/winter detections/covariates
      y = "Ranked observation"
    ) +
    theme_bw()
}

# Generate plots
cleve_dotplots <- imap(datasets, \(dat, season) { # 'indexed map' applying the specified function to either response or covariate data in dat for each season df in the datasets list

  list(
    response = make_cleveland(
      dat, 
      resp.vars,
      "taxon",
      paste(season, "detections")
    ),
    
    covariate = make_cleveland(
      dat,
      cov.vars,
      "covariate",
      paste(season, "covariate value")
    )
  )
})

cleve_dotplots$Winter$response
cleve_dotplots$Summer$response


## All response variables are heavily zero-inflated

## Only major outlier seems to be a single spruce grouse month with over 50 detections
ggplot(cam_stn_cov) + 
  geom_boxplot(aes(y = Spruce.Grouse)) +
  labs(y = "Monthly SPGR") +
  theme_bw()

## Identify the SPGR outlier
cam_stn_cov |> slice_max(Spruce.Grouse, n = 5) ## find the observations with the 5 highest SPGR detections
## One Fort Smith site (BMS-TLU-126-084, May 2023) with a male displaying actively - keep in dataset, but may need to think about how to handle distribution

### Save all plots
iwalk(cleve_dotplots, \(season_plots, season) {
  
  iwalk(season_plots, \(p, plot_type) {
    
    ggsave(
      filename = paste0("figures/data_exploration/", tolower(season), "_", plot_type, ".png"),
      plot = p,
      width = 12,
      height = 10,
      dpi = 300
    )
    
  })
  
})

#### 2. Homogeneity of variance - are there patterns in how the data vary?
## We KNOW that the landscape variables will have spatial patterns and the climate variables will have temporal patterns

glimpse(cam_stn_cov)

## Check each variable's variance across study areas using box plots

## Pivot longer for plotting
cov_long <- cam_stn_cov |>
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
  theme(axis.title.y = element_text(size = 24))

win.graph()
cov_sa

## Save 
ggsave("figures/data_exploration/covariate_studyarea_variance.png", cov_sa, width = 18, height = 12, dpi = 300)

## For season
cov_season <- ggplot(cov_long,
                     aes(x = season, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(y = "Covariate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24))

cov_season
ggsave("figures/data_exploration/covariate_season_variance.png", cov_season, width = 18, height = 12, dpi = 300)

## Response variable variance across study areas
resp_long <- cam_stn_cov |>
  pivot_longer(
    cols = all_of(resp.vars),
    names_to = "variable",
    values_to = "value"
  )

resp_sa <- ggplot(resp_long,
                 aes(x = study_area, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(y = "Response variable") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24))

win.graph()
resp_sa

## Save 
ggsave("figures/data_exploration/response_studyarea_variance.png", resp_sa, width = 18, height = 12, dpi = 300)


## Response variables across seasons
resp_season <- ggplot(resp_long,
       aes(x = season, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(y = "Covariate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24))

resp_season

ggsave("figures/data_exploration/resp_season_variance.png", resp_season, width = 18, height = 12, dpi = 300)

## Heat map of group-wise variability - darker indicates higher coefficients of variation

## across sites
site_var <- cov_long |>
  group_by(variable, site) |>
  summarize(
    cv = sd(value, na.rm = TRUE) /
      mean(value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(site_var,
       aes(variable, site, fill = cv)) +
  geom_tile()
## doesn't seem like there's actually too much variation across sites


## Caterpillar plots for locations and sites - summarizes and ranks variability (sd)
loc_var <- cov_long |>
  group_by(location, variable) |>
  summarize(
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

location_var <- ggplot(loc_var,
       aes(x = sd,
           y = reorder(location, sd))) +
  geom_point() +
  facet_wrap(~ variable, scales = "free_x") +
  labs(y = "Locations (Ranked)",
       x = "Standard Deviation") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title = element_text(size = 24))
location_var
ggsave("figures/data_exploration/covariate_location_sd.png", location_var, width = 18, height = 12, dpi = 300)

### Low standard deviations across locations, except for climate variables

### Levene's test for study area - p > 0.05 indicates variance across groups is similar enough to be considered equal
## Create tibble testing variance across multiple grouping variables

group_vars <- c(
  "study_area",
  "site",
  "location",
  "month",
  "year",
  "season"
)

levene_results <- expand_grid(
  variable = c(resp.vars, cov.vars), # test for all variables
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

summary(cam_stn_cov)


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
  theme(axis.title.y = element_text(size = 24))


win.graph()
cov_hist

## save
ggsave("figures/data_exploration/covariate_histograms.png", cov_hist, width = 18, height = 12, dpi = 300)

resp_hist <- ggplot(resp_long,
                   aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = "free_x") +
  labs(y = "Response variable") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9,size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 24))


win.graph()
resp_hist ## soooo zero-inflated!!

# save
ggsave("figures/data_exploration/response_histograms.png", resp_hist, width = 18, height = 12, dpi = 300)


#### 5. Collinearity ####

# Compute the correlation matrix
glimpse(cam_stn_cov) ## only test continuous variables that will be included in models

model.var <- cam_stn_cov |> 
  select(active_days, Bison, Moose, Muskox, Woodland.Caribou, Snowshoe.Hare, Ptarmigans, Ruffed.Grouse, Sharp.tailed.Grouse, Spruce.Grouse, 
         PAS, Tave, tri_500m, FireAge, mean_severity, fsev_max_raw, proportion_burned_500m, conifer, deciduous, mixed, shrubland, grassland, wetland, barren)

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
  filename = "figures/data_exploration/correlation_plot.png",
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


#### Examine VIFs (variance inflation factors)

## only include variables that would be included in the model
moose_vif <- lm(Moose ~ ., data = cam_stn_cov |> 
                select(Moose, Tave, tri_500m, FireAge, mean_severity, proportion_burned_500m, mixed, wetland))


vif(moose_vif) 
## Tave               tri_500m                FireAge          mean_severity proportion_burned_500m                  mixed                wetland 
## 1.002814               1.329209               1.327969               1.242029               1.043838               1.223853               1.389878 

caribou_vif <- lm(Woodland.Caribou ~ ., data = cam_stn_cov |> 
                  select(Woodland.Caribou, Tave, tri_500m, FireAge, mean_severity, proportion_burned_500m, conifer))

vif(caribou_vif)

## Tave               tri_500m                FireAge          mean_severity proportion_burned_500m                conifer 
## 1.002388               1.056968               1.254089               1.592201               1.049258               1.485476 

muskox_vif <- lm(Muskox ~ ., data = cam_stn_cov |> 
                    select(Muskox, Tave, tri_500m, FireAge, mean_severity, proportion_burned_500m, shrubland, barren))

vif(muskox_vif)
## Tave               tri_500m                FireAge          mean_severity proportion_burned_500m              shrubland                 barren 
## 1.004710               1.114559               1.346867               1.331037               1.045815               1.172727               1.134364

bison_vif <- lm(Bison ~ ., data = cam_stn_cov |> 
                   select(Bison, Tave, tri_500m, FireAge, mean_severity, proportion_burned_500m, shrubland, grassland))

vif(bison_vif)
## Tave               tri_500m                FireAge          mean_severity proportion_burned_500m              shrubland              grassland 
## 1.005997               1.127149               1.503841               1.498720               1.074208               1.525912               1.849290

## aliased coefficients (one variable perfectly correlated with another)
## which are the 'aliased' coefficients? (Removed, weren't going to be included anyway)
## alias(mod_vif) ## Tmin, water, urban, burned_binary


### 7. Interactions
## We hypothesize that the effect of prop_burned will vary with age and severity

m1 <- glm(Moose ~ proportion_burned_500m*FireAge, data = cam_stn_cov, family = "poisson")
summary(m1) # slightly positive interaction

## Plot interaction
interact_plot(m1, pred = FireAge, modx = proportion_burned_500m, modx.values = "terciles")
# suggested moose detections are higher with higher proportion burned, and that this effect is greater the older a fire is

m2 <- glm(Moose ~ proportion_burned_500m*mean_severity, data = cam_stn_cov, family = "poisson")
summary(m2)

interact_plot(m2, pred = mean_severity, modx = proportion_burned_500m, modx.values = "terciles")
## so moose detections may be higher with higher proportions burned, but only at low severity burns (though in this case, low severity = 0, middle = 0.129, and upper = 0.488)
# at moderate severity, the positive effect of burn size is diminished

## interaction between fire age and mixed?
m3 <- glm(Moose ~ FireAge*mixed, data = cam_stn_cov, family = "poisson")
summary(m3) # no likely interaction

interact_plot(m3, pred = mixed, modx = FireAge, modx.values = "terciles")

m4 <- glm(Moose ~ mean_severity*mixed, data = cam_stn_cov, family = "poisson")
summary(m4) # potential interaction between severity and mixed forest

interact_plot(m4, pred = mixed, modx = mean_severity, modx.values = "terciles")
## moose prefer mixed wood only at no to low severity burn?

## Caribou - only occur in Sambaa K'e, Norman Wells, and Edehzhie
unique(cam_stn_cov$study_area)

caribou_stn_cov <- cam_stn_cov |> filter(study_area == "Edéhzhíe" |
                                           study_area == "SambaaK'e" |
                                           study_area == "NormanWells")

c1 <- glm(Woodland.Caribou ~ proportion_burned_500m*FireAge, data = caribou_stn_cov, family = "poisson")
summary(c1)
interact_plot(c1, pred = proportion_burned_500m, modx = FireAge, modx.values = "terciles")
## suggests that caribou decrease with burn size but only in old burns (40). In 27/28 year old burns, caribou prefer more burned area


c2 <- glm(Woodland.Caribou ~ proportion_burned_500m*mean_severity, data = caribou_stn_cov, family = "poisson")
summary(c2)
interact_plot(c2, pred = proportion_burned_500m, modx = mean_severity)

# interaction between fire age and conifer?
c3 <- glm(Woodland.Caribou ~ FireAge*conifer, data = caribou_stn_cov, family = "poisson")
summary(c3) ## no likely interaction

c4 <- glm(Woodland.Caribou ~ mean_severity*conifer, data = caribou_stn_cov, family = "poisson")
summary(c4) ## negative interaction

interact_plot(c4, pred = conifer, modx = mean_severity, modx.values = "terciles")
## negative effect of conifer is diminished at low/no severity (why is conifer negative)


## Muskox - only occur in NormanWells, Gameti, and ThaideneNene
muskox_stn_cov <- cam_stn_cov |> filter(study_area == "ThaideneNëné" |
                                           study_area == "Gameti" |
                                           study_area == "NormanWells")

mx1 <- glm(Muskox ~ proportion_burned_500m*mean_severity, data = muskox_stn_cov, family = "poisson")
summary(mx1)
interact_plot(mx1, pred = proportion_burned_500m, modx = mean_severity)
## Muskox only prefer burns at low severity, don't use areas with moderate severity

mx2 <- glm(Muskox ~ proportion_burned_500m*FireAge, data = muskox_stn_cov, family = "poisson")
summary(mx2)
interact_plot(mx2, pred = proportion_burned_500m, modx = FireAge, modx.values = "terciles")
## Muskox detections decrease with burn size, strongest effect with older burns


### Still need to test interactions with landcover for muskox and all interactions for bison












#### Testing random effects structures with generalized linear mixed-effects models ####
moose.lmer <- glmer(Moose ~ 1 + (1|study_area/site/location), data = cam_stn_cov, family = "poisson")

summary(moose.lmer) ## study_area random effect is singular (variance is 0). All 

moose.lmer2 <- glmer(Moose ~ 1 + (1|site/location), data = cam_stn_cov, family = "poisson")
summary(moose.lmer2) ## some variability across sites and locations, but not much
VarCorr(moose.lmer2)


caribou.lmer <- glmer(Woodland.Caribou ~  1 + (1|study_area/site/location), data = caribou_stn_cov, family = "poisson")

summary(caribou.lmer) ## location is singular
caribou.lmer2 <- glmer(Woodland.Caribou ~  1 + (1|study_area/site), data = caribou_stn_cov, family = "poisson")
summary(caribou.lmer2)

## Would we expect the effect of fire to differ across study areas?
moose.me.fire <- glmer(Moose ~ proportion_burned_500m + (1|study_area), data = cam_stn_cov, family = "poisson")
summary(moose.me.fire)
