#-------------------------------
# LA wildfires project
# author: Lauren Wilner
# date: 2025-07-16
# Compile HYSPLIT trajectories to 
# determine PM2.5 exposure

#-------------------------------
# load packages
library(pacman)
p_load(dplyr, ggplot2, purrr, sf, readr, tigris, terra)

#-------------------------------
# set up paths and parameters

# crs
crs = 32611 # UTM zone 11, becuase it is in meters

# buffer distance for the trajectory points
traj_buffer = 20000 # meters

# thresholds for categorizing census tracts as high, low, or none
  # these are based on the number of buffered trajectory points overlapping 
  # with the centroid of a census tract
thresh_low <- 30
thresh_high <- 75

# how many hours to let a trajectory stay at ground level before cutting it of
gl_hours <- 0 

# last date of trajectories to consider: jan 14 (UTC). will use the day to pull out the relevant trajectories
last_day <- 14

# time range for each trajectory to consider: 1 - 24hours (aka, not the initialization point or else everything within 20km around it looks highly exposed)
traj_time_start <- 1
traj_time_end <- 24

# vertical limit (meters above ground level) for keeping trajectories: 2000 m
agl_limit <- 2000

# counties of interest
counties_map <- data.frame(
  fips = c('06037', '06111', '06083', '06079', '06029', 
           '06059', '06073', '06025', '06065', '06071'),
  name = c('LA', 'Ventura', 'Santa Barbara', 'San Luis Obispo', 'Kern',
            'Orange', 'San Diego', 'Imperial', 'Riverside', 'San Bernardino')
)

# in directory
wd_path = "/Users/laurenwilner/Desktop/Desktop/epidemiology_PhD/00_repos/la-wf/01_data/01_raw"

# out directory
out_path = "/Users/laurenwilner/Desktop/Desktop/epidemiology_PhD/00_repos/la-wf/01_data/02_clean"

# data directory
data_path = "/Users/laurenwilner/Desktop/Desktop/epidemiology_PhD/01_data"


#----------------------------------
# helper functions

## Process trajectories: 
  # read the relevant lines from a trajectory file like a fixed width file
read_traj_file <- function(traj_filename, id_offset = 0){ # allow for id offset if you want to read in multiple trajectory files
  temp = readLines(traj_filename)
  # identify start of fwf section as line after "PRESSURE" and end as line before "New Simulation"
  # refer to 'loop record 6' for the col names for this part
  data.frame(start = grep("PRESSURE", temp), 
             end = c(grep("New Simulation", temp)[-1] - 2, length(temp))) %>% 
    mutate(diff = end - (start + 1),
           traj_id = 1:n()) %>% 
    purrr::pmap(function(start, end, diff, traj_id){
      read_fwf(traj_filename, 
               skip = start, n_max = diff) %>% 
        mutate(traj_id = traj_id + id_offset) %>%
        return
    }) %>% list_rbind() %>% 
    return
}

## Calculate population-weighted centroid of census tracts
calculate_pop_weighted_centroid <- function(tract_geom, pop_raster) {
  # convert tract to SpatVector
  tract_vect <- vect(tract_geom)
  
  # crop and mask population data to tract of interest
  tract_pop <- crop(pop_raster, tract_vect)
  tract_pop <- mask(tract_pop, tract_vect)
  
  # get cell coordinates and population values
  coords <- crds(tract_pop, na.rm = TRUE)
  pop_values <- values(tract_pop, na.rm = TRUE)
  
  # remove NAs
  valid_idx <- !is.na(pop_values)
  coords <- coords[valid_idx, ]
  pop_values <- pop_values[valid_idx]
  
  # calc population-weighted centroid: IS THIS ALL CORRECT? 
    # x-coordinate:
      # weighted_x = (x₁ × pop₁ + x₂ × pop₂ + ... + xₙ × popₙ) / (pop₁ + pop₂ + ... + popₙ)
    # y-coordinate:
      # weighted_y = (y₁ × pop₁ + y₂ × pop₂ + ... + yₙ × popₙ) / (pop₁ + pop₂ + ... + popₙ)
    # whats actually happening -- 
      # multiplication step: each raster cell's coordinate (x or y) gets multiplied by its population value. so if a cell at coordinate x=100 has 500 people, that contributes 100 × 500 = 50,000 to the numerator
      # summation step: all these products get added together across every cell in the tract
      # division step: the sum is divided by the total population across all cells
  if (length(pop_values) > 0 && sum(pop_values) > 0) {
    weighted_x <- sum(coords[, 1] * pop_values) / sum(pop_values)
    weighted_y <- sum(coords[, 2] * pop_values) / sum(pop_values)
    return(c(weighted_x, weighted_y))
  } else {
    # fallback to geometric centroid if no population data
    geom_centroid <- st_centroid(tract_geom)
    coords <- st_coordinates(geom_centroid)
    return(c(coords[1], coords[2]))
  }
}


#-------------------------------
# load data

### load demographic and boundary datasets 
# load counties 
counties <- st_read(paste0(data_path, '/clean/us_cnty_boundaries.geojson'))
# pull of counties to keep
county_fips_to_keep <- counties_map$fips
# strip the '06' from the fips codes to match the counties dataset
county_fips_to_keep <- gsub("^06", "", county_fips_to_keep)

# filter counties dataset
counties_subset <- counties %>% 
  filter(fips %in% county_fips_to_keep) %>% 
  st_as_sf() %>% 
  st_transform(crs)

# load CA shapefile  
ca <- tigris::states(cb = TRUE, year = 2010) %>% filter(NAME == "California") %>% st_transform(crs) 

# load tracts from LA and neighboring counties
la_tracts <- tigris::tracts(state = "CA", year = 2010) %>% filter(COUNTYFP %in% county_fips_to_keep) %>% st_transform(crs)

# load CT centroids from census
  # leaving this for now but going to use pop weighted centroids that i made instead... 
ct_centroids <- read.csv(paste0(wd_path, "/CenPop2010_Mean_TR06.txt")) %>% 
  mutate(TRACTCE = as.character(TRACTCE),
        TRACTCE = sprintf("%06d", as.numeric(TRACTCE)),
        COUNTYFP = as.character(COUNTYFP),
        COUNTYFP = sprintf("%03d", as.numeric(COUNTYFP))) %>% 
  filter(COUNTYFP %in% county_fips_to_keep)
unique_la_cts <- unique(la_tracts$TRACTCE10)
la_ct_centroids <- ct_centroids %>% 
  filter(TRACTCE %in% unique_la_cts) 
la_ct_centroids <- st_as_sf(la_ct_centroids, 
      coords = c("LONGITUDE", "LATITUDE"), 
      crs = 4326) # WGS84 
la_ct_centroids <- st_transform(la_ct_centroids, crs = crs)
utm_coords <- st_coordinates(la_ct_centroids)
la_ct_centroids$centroid_x <- utm_coords[, "X"]
la_ct_centroids$centroid_y <- utm_coords[, "Y"]

## load and process pop data 
# load pop data (to calc CT pop-weighted centroids)
pop_raster <- rast(paste0(wd_path, "/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0/GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0.tif"))

# crop pop data
# its too slow to reproject the entire raster to a projected crs, so we will: 
  # step 1: convert our tracts to the CRS of the pop raster
  # step 2: crop the raster to LA first using a buffered bounding box so we dont miss anything
  # step 3: then reproject both the smaller raster and the tracts to Albers.

  # step 1: convert LA tracts to the CRS of the pop raster
  la_tracts_orig_crs <- st_transform(la_tracts, st_crs(pop_raster))
  # step 2: use a generous buffer to get bounding box and crop raster to buffered box
  la_bbox_buffered <- st_bbox(st_buffer(la_tracts_orig_crs, dist = 100000))
  pop_cropped_buffered <- crop(pop_raster, la_bbox_buffered)
  # step 3: now let's reproject the cropped raster to our crs
  pop_raster_projected <- project(pop_cropped_buffered, paste0("EPSG:", crs))

# convert la_tracts to terra SpatVector obj 
la_tracts_vect <- vect(la_tracts)

# mask the population data to only include areas within census tracts
pop_masked <- mask(pop_raster_projected, la_tracts_vect)

# some checks we can delete later... 
  # plot(pop_masked, main = "Population Data")
  # plot(st_geometry(ca), add = TRUE, border = "black", lwd = 2)
  # plot(st_geometry(la_tracts), add = TRUE, border = "white", lwd = 0.5)


### load fire/pm specific datasets
# load eaton fire
eaton_fire <- st_read(file.path(wd_path, "fire_eaton.geojson")) %>% st_transform(crs)

# load eaton fire centroid for running the HYSPLIT trajectories
st_read(file.path(wd_path, "fire_eaton_centroids.geojson")) %>% st_transform(crs) %>% st_coordinates

# load station data for identifying relevant thresholds of 
epa_station_locs <- st_read(file.path(wd_path, "childs_pm", "epa_station_locations"))
smokePM_avg = readRDS(file.path(wd_path, "childs_pm", "station_smokePM_2025_01.rds")) %>% 
  filter(date >= as.Date("2025-01-08") & date <= as.Date("2025-01-13")) %>% 
  summarise(smokePM = mean(smokePM), 
            .by = id) %>% 
  left_join(epa_station_locs,
            by = c("id" = "stn_id")) %>% 
  st_as_sf() %>% 
  st_transform(crs)

# load hysplit trajectories run from here: https://www.ready.noaa.gov/HYSPLIT.php
# trajectories run with GDAS1 meteorology, as in brey et al
traj_full <- read_traj_file(file.path(wd_path, "hysplit", "trajdump_18691.txt"))


#----------------------------------
# data processing

# limit trajectories to what we need: 
keep_traj <- traj_full %>% 
  # drop any trajectories that go below/to ground level 
  mutate(any_agl = cumsum(X12 <= 0), .by = traj_id) %>%
  filter(any_agl <= 0) %>%
  # drop any trajectories after the 14th
  filter(X5 <= 14) %>%
  # only keep the first 24 hours of trajectories and not the initialization point: after 24 hours the trajectories are pretty far so we don't need them anymore. can plug in 48h to see the diff. 
  # dropped initialization point because things are going out from there and if you don't drop it, you end up with a huge concentration of points right around the buffered trajectory right at the fire area. which isn't true based on wind direction.
  filter(X9 > 0 & X9 <= 24) %>%
  # also drop trajectories that go > 2000m AGL since they're probably not affecting ground concentrations
  filter(X12 <= 2000) %>%
  st_as_sf(coords = c("X11", "X10")) %>%  
  st_set_crs(4326) %>%                    
  st_transform(crs)

# start out by visualizing all the data inputs we have: 
# plot the trajectories with the fire location, and the stations colored by avg smoke PM2.5
# to be a little lazy, save some of the plot layers that we'll keep reusing into a list to add to the plot
plot_layers <- list(geom_sf(data = ca, inherit.aes = FALSE, fill = NA), 
                    geom_sf(data = eaton_fire, col = "red", inherit.aes = FALSE), 
                    geom_sf(data = smokePM_avg, 
                            aes(color = smokePM), inherit.aes = FALSE), 
                    scale_color_viridis_c(limits = c(NA, 25), oob = scales::squish),
                    xlim(200000, 700000), ylim(3650000, 3900000), 
                    theme_classic() + theme(axis.title = element_blank()))

{keep_traj %>%
  ggplot(aes(group = traj_id)) + 
  geom_sf(color = "grey80", alpha = 0.3, size = 0.3)} %>%  # use geom_sf instead
  reduce(.x = plot_layers, .f = `+`, .init = .)

#--------------------------------
# preliminary visualizations 
# now we can start to visualize the trajectories and how they overlap with the smoke PM2
# to convert this to a continuous surface, take the trajectory points, buffer each by 20km(?), 
# define locations as high and moderate exposure if they have > X% of the max
# look at the buffered trajectories to see what this would look like
{keep_traj %>%
  st_buffer(dist = traj_buffer) %>%
  ggplot(aes(group = traj_id)) + 
  geom_sf(alpha = 0.05, color = NA, fill = "grey80")} %>% 
  reduce(.x = plot_layers, .f = `+`, .init = .)

# to figure out what that X% should be, look at how many buffered points overlap each of the stations vs station avg smoke pm2.5
keep_traj %>%
  st_buffer(dist = traj_buffer) %>%
  # intersect with the station locations
  st_intersects(st_as_sf(smokePM_avg), .) %>% 
  # that output of the intersection comes out as a list, 
  # so take the length of each item in the list and tack it onto the station df
  purrr::map_dbl(length) %>% 
  cbind(smokePM_avg, n_point = .) %>% 
  {ggplot(data = ., aes(x = n_point, y = smokePM)) + 
      geom_point(alpha = 0.6) + 
      geom_vline(xintercept = thresh_high) + 
      geom_vline(xintercept = thresh_low) + 
      annotate("text", x = 125, y = 5, label = paste0("cor = ", round(cor(.$n_point, .$smokePM), 3))) + 
      xlab("n buffered traj overlapping") + ylab("average smoke pm2.5") + 
      theme_classic()}

#--------------------------------
# determine CT exposure: 
  # step 1: find population-weighted centroids of the census tracts
  # calc population-weighted centroids for all tracts
  pop_weighted_centroids <- la_tracts %>%
    rowwise() %>%
    mutate(
      centroid_result = list(calculate_pop_weighted_centroid(geometry, pop_masked))
    ) %>%
    ungroup() %>%
    mutate(
      centroid_x = map_dbl(centroid_result, ~ .x[1]),
      centroid_y = map_dbl(centroid_result, ~ .x[2])
    ) %>%
    select(-centroid_result)

  # make point geometries for the centroids
  centroid_points <- pop_weighted_centroids %>%
    st_drop_geometry() %>%
    st_as_sf(coords = c("centroid_x", "centroid_y"), crs = st_crs(la_tracts))

  # add centroid points as a geometry column to the original data
  la_tracts_with_centroids <- pop_weighted_centroids %>%
    mutate(pop_weighted_centroid = st_sfc(
      map2(centroid_x, centroid_y, ~st_point(c(.x, .y))),
      crs = st_crs(la_tracts)
    ))
  
  # step 2: calculate the number of buffered trajectory points 
  # overlapping each of the census tract pop-weighted centroids
  traj_overlaps <- keep_traj %>%
    st_as_sf(coords = c("X11", "X10")) %>%
    st_set_crs(crs) %>%
    st_buffer(dist = traj_buffer) %>%
    st_intersects(centroid_points, .) %>%
    purrr::map_dbl(length) %>% 
    cbind(la_tracts, n_point = .)

  traj_overlaps_geom <- keep_traj %>%
    st_as_sf(coords = c("X11", "X10")) %>%
    st_set_crs(crs) %>%
    st_buffer(dist = traj_buffer) %>%
    st_intersects(st_centroid(la_tracts), .) %>%
    purrr::map_dbl(length) %>% 
    cbind(la_tracts, n_point = .)


# step 3: now we can categorize the CT-level exposure based on 
# the number of buffered trajectory points overlapping 
# with the pop-weighted centroid of the CT
  # exposure_df data dictionary
    # GEOID10: census tract ID
    # n_point: number of buffered trajectory points overlapping with the centroid of the CT
    # smoke_category: category of exposure based on the number of buffered trajectory points overlapping with the centroid of the CT

  exposure_df <- traj_overlaps %>% 
    mutate(smoke_category = case_when(n_point < thresh_low ~ "none", 
                                n_point < thresh_high ~ "mid", 
                                n_point >= thresh_high ~ "high", 
                                T ~ "error")) %>% 
    select(GEOID10, n_point, smoke_category) 
  
  # reassign the smoke_category of a tract if ALL of its neighbors have a different category
  reclassify_tract <- function(exposure_df) {
    exposure_df_corrected <- exposure_df
    
    # find neighbors
    neighbors <- st_touches(exposure_df)
    
    # go through each tract
    for (i in 1:nrow(exposure_df)) {
      # get current tract's category
      current_category <- exposure_df$smoke_category[i]
      
      # get neighbor categories
      neighbor_indices <- neighbors[[i]]
      neighbor_categories <- exposure_df$smoke_category[neighbor_indices]
      
      # check if ALL neighbors have a different category
      if (length(neighbor_categories) > 0 && 
          all(neighbor_categories != current_category)) {
        # find the most common category among neighbors
        new_category <- names(sort(table(neighbor_categories), decreasing = TRUE)[1])
        
        # update the category
        exposure_df_corrected$smoke_category[i] <- new_category
      }
    }
    
    return(exposure_df_corrected)
  }

  exposure_df_corrected <- reclassify_tract(exposure_df)
  
  
#--------------------------------
# visualize exposure categories

# create line geometries from the trajectory points
keep_traj_lines <- keep_traj %>%
  st_as_sf(coords = c("X11", "X10")) %>%
  st_set_crs(crs) %>%
  group_by(traj_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")

# plot 
{exposure_df_corrected %>%
  ggplot(aes(fill = smoke_category)) +
  geom_sf(color = NA, alpha = 0.9) +
  geom_sf(data = keep_traj_lines,
          color = "grey80", alpha = 0.3, inherit.aes = FALSE)} %>% 
  reduce(.x = plot_layers, .f = `+`, .init = .) 



#--------------------------------
# write out the exposure data
pm_exp <- exposure_df_corrected %>% 
  select(c(GEOID10, smoke_category)) %>% 
  rename(geoid = GEOID10, exposed_pm = smoke_category) %>% 
  st_drop_geometry()
write.csv(pm_exp, 
          file = file.path(out_path, "exposed_cts_pm.csv"), 
          row.names = FALSE)









## SOME EXTRA PLOTS 





# map of centroids
geom_centroids <- st_centroid(la_tracts_with_centroids)
geom_coords <- st_coordinates(geom_centroids)
geom_df <- data.frame(x = geom_coords[,1], y = geom_coords[,2], type = "Geometric centroids")
census_df <- data.frame(x = la_ct_centroids$centroid_x, y = la_ct_centroids$centroid_y, type = "Census centroids")
pop_df <- data.frame(x = la_tracts_with_centroids$centroid_x, y = la_tracts_with_centroids$centroid_y, type = "Pop-weighted centroids")

all_points <- rbind(geom_df, census_df, pop_df)

ggplot() +
  geom_sf(data = la_tracts_with_centroids, fill = NA, color = "gray60", size = 0.3, alpha = 0.7) +
  geom_point(data = all_points, aes(x = x, y = y, color = type), size = 0.8, alpha = 0.7) +
  geom_sf(data = eaton_fire, fill = "orange", color = "red", size = 1) +
  scale_color_manual(name = "",
                     values = c("Geometric centroids" = "#008080B3", 
                                "Census centroids" = "#FFDB58B3", 
                                "Pop-weighted centroids" = "#BAB86CB3")) +
  labs(title = "CTs + 3 different centroid types") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5))



## map out the diff between exposure_df and exposure_df_geom
  exposure_df_geom <- traj_overlaps_geom %>% 
    mutate(smoke_category = case_when(n_point < thresh_low ~ "none", 
                                n_point < thresh_high ~ "mid", 
                                n_point >= thresh_high ~ "high", 
                                T ~ "error")) %>% 
    select(GEOID10, n_point, smoke_category) 


# Get unique tracts from each dataset
geom_tracts <- exposure_df_geom %>% 
  filter(smoke_category != "none") %>%
  distinct(GEOID10) %>% 
  pull(GEOID10)

regular_tracts <- exposure_df %>% 
  filter(smoke_category != "none") %>%
  distinct(GEOID10) %>% 
  pull(GEOID10)

# Find the difference
missing_tracts <- setdiff(geom_tracts, regular_tracts)

  exposure_df <- exposure_df %>% 
    mutate(flag = ifelse(GEOID10 %in% missing_tracts, "missing_geom", "ok"))


# Create the flag as you did
exposure_df <- exposure_df %>% 
    mutate(flag = ifelse(GEOID10 %in% missing_tracts, "missing_geom", "ok"))

# Create line geometries from the trajectory points
keep_traj_lines <- keep_traj %>%
  st_as_sf(coords = c("X11", "X10")) %>%
  st_set_crs(crs) %>%
  group_by(traj_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")

# Method 1: Separate the missing_geom tracts and plot them on top
missing_tracts_df <- exposure_df %>% filter(flag == "missing_geom")
ok_tracts_df <- exposure_df %>% filter(flag == "ok")

plot_base <- {
  ok_tracts_df %>%
    ggplot(aes(fill = smoke_category)) +
    geom_sf(color = NA, alpha = 0.9) +
    # Add missing tracts in red on top
    geom_sf(data = missing_tracts_df, fill = "red", color = "darkred", 
            alpha = 0.8, inherit.aes = FALSE) +
    geom_sf(data = keep_traj_lines,
            color = "grey80", alpha = 0.3, inherit.aes = FALSE)
} %>% 
  reduce(.x = plot_layers, .f = `+`, .init = .)

# Method 2: Use a conditional fill with scale_fill_manual
plot_conditional <- {
  exposure_df %>%
    ggplot() +
    # First layer: normal tracts with smoke_category
    geom_sf(data = exposure_df %>% filter(flag == "ok"), 
            aes(fill = smoke_category), color = NA, alpha = 0.9) +
    # Second layer: missing tracts in red
    geom_sf(data = exposure_df %>% filter(flag == "missing_geom"), 
            fill = "red", color = "darkred", alpha = 0.8) +
    geom_sf(data = keep_traj_lines,
            color = "grey80", alpha = 0.3, inherit.aes = FALSE)
} %>% 
  reduce(.x = plot_layers, .f = `+`, .init = .)

# Method 3: Create a combined variable for coloring
exposure_df <- exposure_df %>%
  mutate(display_category = ifelse(flag == "missing_geom", "Missing Geom", smoke_category))

# Get original smoke_category colors and add red for missing
original_colors <- scales::hue_pal()(length(unique(exposure_df$smoke_category[exposure_df$flag == "ok"])))
names(original_colors) <- unique(exposure_df$smoke_category[exposure_df$flag == "ok"])
all_colors <- c(original_colors, "Missing Geom" = "red")

plot_combined <- {
  exposure_df %>%
    ggplot(aes(fill = display_category)) +
    geom_sf(color = NA, alpha = 0.9) +
    scale_fill_manual(values = rev(all_colors), name = "Category") +
    geom_sf(data = keep_traj_lines,
            color = "grey80", alpha = 0.3, inherit.aes = FALSE)
} %>% 
  reduce(.x = plot_layers, .f = `+`, .init = .)

# Display the plots
print("Method 1 - Layered approach:")
print(plot_base)

print("Method 2 - Conditional layers:")
print(plot_conditional)

print("Method 3 - Combined categories:")
print(plot_combined)
