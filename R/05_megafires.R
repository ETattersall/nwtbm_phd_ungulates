#########################
## 05_megafires.R
## Exploring megafire years represented in study areas, and whether these might be used for grouping sites
## Started on March 20 2026
## Created by Erin Tattersall
#########################


#### Environment set up ####
## Load required packages (should already be installed)
list.of.packages <- c("sf",
                      "lwgeom",
                      "data.table",
                      "tidyverse",
                      "dplyr",
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

lapply(list.of.packages, require, character.only = TRUE)


## Load relevant spatial data - Fire data by study area, and camera locations
sa_20km_fires <- st_read("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/nrcan_nbac/NBAC_1972to2024_20250506_shp/NBAC_fires_by_study_area_20kmbuffer.shp")

## Camera locations (not buffered)
cam_locs_sf <- st_read("data/wt_location_data/all_projects_cam_locations_20260310.shp")
glimpse(cam_locs_sf) ## column names have gotten all mixed up - fix 
colnames(cam_locs_sf) <- c("location", "buffer_m", "location_visibility", "true_coordinates", "location_comments", "internal_wildtrax_id", "study_area", "geometry")

## save again
st_write(cam_locs_sf, "data/wt_location_data/all_projects_cam_locations_20260320.shp")

## Megafire years ##
# Megafires are defined as >10 000 ha in size. Giga fires are defined as >100 000 ha in size.
# Isolate Megafires or greater (using ADJ_HA column)
sa_megafires <- sa_20km_fires %>%
  filter(ADJ_HA > 10000) %>%
  mutate(size_group = case_when(
    ADJ_HA >= 100000 ~ "Gigafire",
    ADJ_HA >= 10000 & ADJ_HA < 100000 ~ "Megafire",
    TRUE ~ NA_character_  # Default case if no match
  ))

plot(sa_megafires["YEAR"])

hist(sa_megafires$YEAR)
table(sa_megafires$size_group) ## 25 gigafires, 109 megafires

## Add Fire Age to sa_megafires
glimpse(sa_megafires)

## Create Year0 for sa_megafires (corresponding to LAST year of deployment)
sa_megafires <- sa_megafires %>%
  mutate(Year0 = case_when(
    str_detect(study_area, "Edéhzhíe") ~ "2022",
    str_detect(study_area, "FortSmith") ~ "2024",
    str_detect(study_area, "Gameti") ~ "2024",
    str_detect(study_area, "NormanWells") ~ "2024",
    str_detect(study_area, "SambaaK'e") ~ "2023",
    str_detect(study_area, "ThaideneNëné") ~ "2022",
    TRUE ~ NA_character_  # Default case if no match
  ))
class(sa_megafires$Year0)
# convert to numeric
sa_megafires$Year0 <- as.numeric(sa_megafires$Year0)

## Calculate FireAge as Year0 - YEAR
sa_megafires$FireAge <- sa_megafires$Year0 - sa_megafires$YEAR

hist(sa_megafires$FireAge)

## how many (and which ones) are negative values?
neg.fire.age <- sa_megafires[sa_megafires$FireAge < 0, ]
neg.fire.age <- neg.fire.age %>% select(NFIREID, study_area, ADJ_HA, size_group, AG_SDATE, YEAR, Year0, FireAge, Shape_Leng, Shape_Area, geometry) 

## Can remove all these from sa_megafires
sa_megafires <- sa_megafires[!sa_megafires$FireAge < 0 , ]

## Create a bar plot of megafires and gigafires by year
gg_sa_megafires <- ggplot(sa_megafires, aes(x = FireAge, fill = size_group)) +
  geom_bar() +
  facet_wrap(~ study_area) +
  labs(title = "Number of Megafires and Gigafires by Time Since Fire",
       x = "Time Since Fire",
       y = "Count of Fires",
       fill = "Fire Size") +
  theme_classic() +
  scale_fill_manual(values = c("Megafire" = "orange", "Gigafire" = "darkred")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
gg_sa_megafires
## Save the plot
ggsave("figures/fire_explore/Megafires_hist_bySA.png", gg_sa_megafires, width = 12, height = 8, dpi = 300)


## Which years have giga fires (across study areas)?
#Isolate gigafires
sa_gigafires <- sa_megafires %>% filter(size_group == "Gigafire")

# Plot number of gigafires by year
gg_gigafires <- ggplot(sa_gigafires, aes(x = YEAR, fill = size_group)) +
  geom_bar() +
  labs(title = "Number of Gigafires by Year",
       x = "Year",
       y = "Count of Fires",
       fill = "Fire Size") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
gg_gigafires


class(sa_gigafires)


### MAPPING
## Want to map the megafires and gigafires by study area with camera locations (faceted)

## Create each map separately (in a list), then plot together using plot_grid() (cowplot package)

## list of study areas
study_areas <- unique(cam_locs_sf$study_area)

## Create a function to make one plot per study area
make_sa_plot <- function(sa) {
  
  # get bounding box for that study area (using sa_20km fires polygons)
  sa_bbox <- sa_20km_fires %>%
    dplyr::filter(study_area == sa) %>%
    summarise() %>%
    st_bbox()
  
  ggplot() +
    geom_sf(data = sa_megafires %>% filter(study_area == sa),
            aes(fill = FireAge), color = NA,
            size = 0.5) +
    
    geom_sf(data = cam_locs_sf %>% filter(study_area == sa),
            color = "black",
            size = 2) +
    
    scale_fill_gradient(low = "red", high = "yellow", name = "Time Since Fire") +
    
    coord_sf(xlim = c(sa_bbox["xmin"], sa_bbox["xmax"]),
             ylim = c(sa_bbox["ymin"], sa_bbox["ymax"]),
             expand = FALSE) +
    
    labs(title = paste("Megafires in", sa),
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
  "figures/fire_explore/Megafires_map_bySA.jpeg",
  combined_plot,
  ncol = 3,
  nrow = 2,
  base_asp = 1.618,
  dpi = 300
)


