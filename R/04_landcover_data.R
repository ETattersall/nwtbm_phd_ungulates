########################################
## 04_landcover_data.R
## Exploring landcover type by study area
## Started on Mar 31 2026
## Created by Erin Tattersall
########################################

#### Environment set up ####
## Load required packages (should already be installed)

list.of.packages <- c("sf",
                      "lwgeom",
                      "data.table",
                      "tidyverse",
                      "dplyr",
                      "stars",
                      "ggspatial",
                      "cowplot",
                      "terra",
                      "ggplot2", 
                      "tidyterra", 
                      "ggspatial",
                      "viridis",
                      "corrplot",
                      "kableExtra",
                      "lubridate",
                      "purrr",
                      "ggplotify")



# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Read in study area shapefiles - both regular and with 20km buffer (in study_area_spatial folder)
list.files("data/study_area_spatial")
sa_poly <- st_read("data/study_area_spatial/NWTBM_all_study_area_polygons.shp")
sa_20km <- st_read("data/study_area_spatial/NWTBM_all_study_areas_20km_buffers.shp")

## Split sa_poly and sa_20km into lists of sf objects, 1 object per study area
l_sa_poly <- lapply(seq_len(nrow(sa_poly)), function(i) sa_poly[i, ])
l_sa20 <- lapply(seq_len(nrow(sa_20km)), function(i) sa_20km[i, ])

## Load in Landcover Canada 2020 raster data (downloadable here: https://open.canada.ca/data/en/dataset/ee1580ab-a23d-4f86-a09b-79763677eb47)
list.files("data/LCC_2020")

lcc <- rast("data/LCC_2020/landcover-2020-classification.tif")
lcc ## projection = Canada Atlas Lambert
summary(lcc) ## values 1-19 corresponding to pixel values in the class index (ClassIndex_v2.pdf)

## transform sa_20km projections to crop raster to study areas
sa_20km_canlam <- lapply(l_sa20, st_transform, crs(lcc))

crs(sa_20km_canlam[[1]])

# crop raster to 20km study area
lcc_sa_20 <- lapply(sa_20km_canlam, function(sf_i) {
    v <- vect(sf_i)               # convert sf to SpatVector
    mask(crop(lcc, v), v) # crop lcc to sa polygon, then mask out values outside of polygon
  })
plot(lcc_sa_20[[6]])
lcc_sa_20[[6]]


## Adding LCC attributes from csv (converted the table from pdf to csv manually)
lcc_atts <- read.csv("data/LCC_2020/ClassIndex_v2.csv") %>% 
  separate_wider_delim(RGB, 
                       delim = "-", 
                       names = c("R", "G", "B")) %>%  ## separating RGB values for use in plotting class colors
  mutate(hex = rgb(R,G,B, maxColorValue = 255)) ## creating a column indicating the correct mapping color based on RGB values

## Assigning landcover classes to lcc raster as levels and colors based on pixel values
cats <- data.frame(ids = lcc_atts$pixel_values, cover = lcc_atts$landcover_class)
coltab <- data.frame(ids = lcc_atts$pixel_values, cols = lcc_atts$hex)

## Apply levels and colors to list of lcc data by area
lcc_sa_20 <- lapply(lcc_sa_20, function(r) {
  levels(r)  <- cats
  coltab(r)  <- coltab
  r
})

lcc_sa_20[[1]]

## Reproject lcc rasters to match NWT files
st_crs(sa_20km) ## EPSG 3580

lcc_sa_20 <- lapply(lcc_sa_20, project, "EPSG:3580", method = "near")

## To use rasters later - Save all rasters in list individually, plus their paths as a list to load them in again
paths <- vapply(seq_along(lcc_sa_20), function(i) {
  path <- file.path("data/LCC_2020", paste0("lcc_sa_20_", i, ".tif"))
  writeRaster(lcc_sa_20[[i]], path, overwrite = TRUE)
  path
}, FUN.VALUE = character(1))

saveRDS(paths, "data/LCC_2020/lcc_sa_20_paths.rds")

# Reload
paths <- readRDS("data/LCC_2020/lcc_sa_20_paths.rds")
lcc_sa_20 <- lapply(paths, rast)


#### Raster data cropped to 1_sa_poly (i.e., no buffer)
# Re-project 1_sa_poly
sa_poly_canlam <- lapply(l_sa_poly, st_transform, crs(lcc))


## Crop and mask lcc data to sa polygons (create list)
lcc_sa <- lapply(sa_poly_canlam, function(sf_i) {
  v <- vect(sf_i)               # convert sf to SpatVector
  mask(crop(lcc, v), v) # crop lcc to sa polygon, then mask out values outside of polygon
})

## Apply levels and colors to list of lcc data by area
lcc_sa <- lapply(lcc_sa, function(r) {
  levels(r)  <- cats
  coltab(r)  <- coltab
  r
})

## Reproject lcc rasters to match NWT files

lcc_sa <- lapply(lcc_sa, project, "EPSG:3580", method = "near")

## To use rasters later - Save all rasters in list individually, plus their paths as a list to load them in again
paths_poly <- vapply(seq_along(lcc_sa), function(i) {
  path <- file.path("data/LCC_2020", paste0("lcc_sa_nobuffer", i, ".tif"))
  writeRaster(lcc_sa[[i]], path, overwrite = TRUE)
  path
}, FUN.VALUE = character(1))

saveRDS(paths_poly, "data/LCC_2020/lcc_sa_paths_poly.rds")

# Reload
paths_poly <- readRDS("data/LCC_2020/lcc_sa_paths_poly.rds")
lcc_sa <- lapply(paths_poly, rast)



#### Map landcover classes by study area ####

## Create each map separately (in a list), then plot together using plot_grid() (cowplot package)
## Create list of study area names 
study_area <- unique(sa_20km$study_area)

 
# Create a df of the raster data, with facet IDs for each study area
# requires a lot of RAM, but less than when working with the study areas in a list
ras_df <- map2(
  lcc_sa_20,
  seq_along(lcc_sa_20), # generates numbers 1-6 to index the list
  ~ tidyterra::as_tibble(.x, xy = TRUE, na.rm = TRUE) %>% # creating a tibble of raster data in lcc_sa_20 list
    mutate(study_area = study_area[.y]) # adding study area names from the 'study_area' list
)

ras_df <- bind_rows(ras_df)
class(ras_df)

glimpse(ras_df)
table(is.na(ras_df$cover)) #none
glimpse(sa_poly)


## Use colors already determined in lcc_atts in plot - create a named vector assigning a hex color to each cover class
lc_colors <- set_names(lcc_atts$hex, lcc_atts$landcover_class)
lc_colors


## Create a function to make one plot per study area
make_sa_plot <- function(sa) {
  
  # get bounding box for that study area (using sa_20km fires polygons)
  sa_bbox <- sa_20km %>%
    dplyr::filter(study_area == sa) %>%
    summarise() %>%
    st_bbox()
  
  ggplot() +
    geom_tile(data = ras_df %>% filter(study_area == sa),
                aes(x = x, y = y, fill = cover)) +
    
    geom_sf(data = sa_poly %>% filter(study_area == sa),
            color = "black", 
            fill = NA,
            linewidth = 1) +
    
    coord_sf(xlim = c(sa_bbox["xmin"], sa_bbox["xmax"]),
             ylim = c(sa_bbox["ymin"], sa_bbox["ymax"]),
             expand = FALSE) + 
    
    scale_fill_manual(
      values = lc_colors, ## assigns cover colors
      drop = FALSE) +
    
    labs(title = paste("Landcover in", sa),
         x = "Longitude",
         y = "Latitude") +
    
    theme_classic() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "none" # remove individual legends
    )
}


## Create list of ggplots
plot_list <- lapply(study_area, make_sa_plot)


## Create legend

legend_plot <- ggplot() +
  geom_raster(
    data = ras_df %>% filter(study_area == study_area[1]),
    aes(x = x, y = y, fill = cover)
  ) +
  scale_fill_manual(
    values = lc_colors,
    drop = FALSE,
    name = "Land Cover Classes"
  ) +
  theme_void() +
  theme(legend.position = "right")

shared_legend <- get_legend(legend_plot)


## Assemble plot
combined_plot <- plot_grid(
  plot_grid(plotlist = plot_list, ncol = 3),
  shared_legend,
  ncol = 2,
  rel_widths = c(1, 0.25)
)



win.graph()
combined_plot

## Save the plot
save_plot(
  "figures/LandCover_bySA.jpeg",
  combined_plot,
  ncol = 3,
  nrow = 2,
  base_asp = 1.618,
  dpi = 300
)

######### Calculating landcover proportions per study area ####

## creates a df of proportions of each landcover class per study area
landcover_summary <- map2(
  lcc_sa,
  seq_along(lcc_sa), # generates numbers 1-6 to index the list
  ~ freq(.x) %>% # returns total number of pixels (count) per category (value)
    rename(landcover_class = value, n_pixels = count) %>% # renaming columns intuitively
  mutate(study_area = study_area[.y],  # adding study area names from the 'study_area' list to each df
         area_ha = n_pixels * res(.x)[1] * res(.x)[2] / 10000, # calc area by multiplying the dimensions of each study area (res rows and columns) by n_pixels, dividing by 10 000 for ha
         prop = n_pixels / sum(n_pixels)) %>% 
    arrange(desc(prop)) ## re-arrange dfs so prop is in descending order
  )

## Also adding sa names as df names for added clarity
names(landcover_summary) <- study_area

landcover_summary[[3]]
sum(landcover_summary[[3]]$prop) # adds to 1, as it should!

## Re-format for tidy tables
landcover_summary <- map2(
  landcover_summary,
  seq_along(lcc_sa),
  ~ mutate(.x,
           area_ha = round(area_ha, 0), # round area to nearest integer
           prop = scales::percent(prop, accuracy = 0.1)) %>% # convert prop to percent
    dplyr::select("Landcover" = landcover_class, "Number of Pixels" = n_pixels, 
                  "Hectares" = area_ha, "Overall Proportion" = prop) %>%
    kbl(caption = paste(study_area[.y], "Landcover Class Summary from Original Classification", sep = " ")) %>%
    kable_styling(full_width = FALSE, position = "left")
)

class(landcover_summary[[2]])

landcover_summary[[2]]

## save landcover summaries as an RDS list (still need to export as tables)
saveRDS(landcover_summary, "data/LCC_2020/LCC_proportionsummaries_by_studyarea.RDS")




#### Lumping Landcover classes ####
## are all landcover classes represented?
length(unique(ras_df$cover)) ## only 13 - 2 are missing 
length(table(ras_df$cover)) # cropland (15) and snow and ice (19) are not represented

cats_lumped <- cats %>%
  mutate(
    cover = case_when(
      ids %in% c(1, 2) ~ "Conifer Forest",
      ids %in% c(5) ~ "Deciduous Forest",
      ids %in% c(6) ~ "Mixed Forest",
      ids %in% c(8, 11) ~ "Shrubland",
      ids %in% c(10, 12) ~ "Grassland",
      ids %in% c(14) ~ "Wetland",
      ids %in% c(13, 16) ~ "Barrenlands",
      ids %in% c(17) ~ "Urban",
      ids %in% c(18) ~ "Water",
      ids %in% c(15, 19) ~NA_character_, # Exclude ids 15 and 19 by setting them to NA
      TRUE ~ cover))

## Re-assign colors to new categories
coltab_lumped <- coltab %>%
  left_join(cats_lumped, by = "ids") %>%
  distinct(cover, .keep_all = TRUE) %>% # keep only one of each category
  na.omit() %>% #remove NA rows
  mutate(
    cols = case_when(
      cover == "Conifer Forest" ~ "#003D00",
      cover == "Deciduous Forest" ~ "#148C3D",
      cover == "Mixed Forest" ~ "#5C752B",
      cover == "Shrubland" ~ "#B38A33",
      cover == "Grassland" ~ "#E0CF8A",
      cover == "Barrenlands" ~ "#A8ABAE",
      TRUE ~ cols
    )) %>% 
  dplyr::select(-cover) #remove cover column


## Apply levels and colors to list of lcc data by area
summary(lcc_sa_20[[1]])
## 20km buffered areas
lcc_sa_20_lumped <- lapply(lcc_sa_20, function(r) {
  levels(r)  <- cats_lumped
  coltab(r)  <- coltab_lumped
  r
})

## Study areas with no buffer for statistical summaries
lcc_sa_lumped <- lapply(lcc_sa, function(r) {
  levels(r)  <- cats_lumped
  coltab(r)  <- coltab_lumped
  r
})

summary(lcc_sa_lumped[[2]])

### Proportions of lumped landcover classes
landcover_summary_lumped <- map2(
  lcc_sa_lumped,
  seq_along(lcc_sa_lumped), # generates numbers 1-6 to index the list
  ~ freq(.x) %>% # returns total number of pixels (count) per category (value)
    rename(landcover_class = value, n_pixels = count) %>% # renaming columns intuitively
    group_by(landcover_class) %>% # group same classes together
    summarise(n_pixels = sum(n_pixels), .groups = "drop") %>% ## sum together duplicates
    mutate(study_area = study_area[.y],  # adding study area names from the 'study_area' list to each df
           area_ha = n_pixels * res(.x)[1] * res(.x)[2] / 10000, # calc area by multiplying the dimensions of each study area (res rows and columns) by n_pixels, dividing by 10 000 for ha
           prop = n_pixels / sum(n_pixels)) %>% 
    arrange(desc(prop)) ## re-arrange dfs so prop is in descending order
)

## Also adding sa names as df names for added clarity
names(landcover_summary_lumped) <- study_area

## Re-format for tidy tables
landcover_summary_lumped <- map2(
  landcover_summary_lumped,
  seq_along(lcc_sa),
  ~ mutate(.x,
           area_ha = round(area_ha, 0), # round area to nearest integer
           prop = scales::percent(prop, accuracy = 0.1)) %>% # convert prop to percent
    dplyr::select("Landcover" = landcover_class, "Number of Pixels" = n_pixels, 
                  "Hectares" = area_ha, "Overall Proportion" = prop) %>%
    kbl(caption = paste(study_area[.y], "Landcover Class Summary from Original Classification", sep = " ")) %>%
    kable_styling(full_width = FALSE, position = "left")
)

landcover_summary_lumped[[3]]

## save lumped landcover summaries
saveRDS(landcover_summary_lumped, "data/LCC_2020/LCC_lumped_proportionsummaries_by_studyarea.RDS")

study_area[3]
