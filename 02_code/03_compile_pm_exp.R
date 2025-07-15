library(dplyr)
library(magrittr)
library(ggplot2)
library(purrr)
library(sf)
library(readr)

# main choices 
traj_buffer = 20000 # in meters

# thresholds for categorizing trajectories
# these are based on the number of buffered trajectory points overlapping with the centroid of a census tract
thresh_low <- 30
thresh_high <- 75

# how many hours to let a trajectory stay at ground level before cutting it of: 0 
# last date of trajectories to consider: jan 14 (UTC)
# time range for each trajectory to consider: 1 - 24hours (aka, not the initialization point or else everything within 20km around it looks highly exposed)
# vertical limit (meters above ground level) for keeping trajectories: 5000 m

# not great, but still need to clean up file organization
wd_path = "/Users/laurenwilner/Desktop/Desktop/epidemiology_PhD/00_repos/la-wf/01_data/01_raw"

# eaton fire centroid for running the HYSPLIT trajectories
st_read(file.path(wd_path, "fire_eaton_centroids.geojson")) %>% st_transform(4326) %>% st_coordinates

# load CA shapefile  
ca <- tigris::states(cb = TRUE) %>% filter(STUSPS == "CA")
# load tracts from LA and neighboring counties
tigris::tracts(state = "CA") %>% filter(COUNTYFP %in% c("059", "037", "111", "071", "029", "083", "065")) -> la_tracts
la_tracts %<>% st_transform(4326) # TO ASK JOAN: WHAT IS THE CATCHMENT AREA? WHAT YEAR CT FILE SHOULD WE USE? 

# load station data for identifying relevant thresholds of 
smokePM_avg = readRDS(file.path(wd_path, "childs_pm", "station_smokePM_2025_01.rds")) %>% 
  filter(date >= as.Date("2025-01-08") & date <= as.Date("2025-01-13")) %>% 
  summarise(smokePM = mean(smokePM), 
            .by = id) %>% 
  left_join(st_read(file.path(wd_path, "childs_pm", "epa_station_locations")),
            by = c("id" = "stn_id"))
eaton_fire <- st_read(file.path(wd_path, "fire_eaton.geojson"))

# make a function to read the relevant lines from a trajectory file like a fixed width file
read_traj_file = function(traj_filename, id_offset = 0){ # allow for id offset if you want to read in multiple trajectory files
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

# hysplit trajectories run from here: https://www.ready.noaa.gov/HYSPLIT.php
# trajectories run with GDAS1 meteorology, as in brey et al
read_traj_file(file.path(wd_path, "hysplit", "trajdump_18691.txt")) -> traj_full

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
  # also drop trajectories that go > 5000m AGL since they're probably not affecting ground concentrations
  filter(X12 <= 5000)

# start out by visualizing all the data inputs we have: 
# plot the trajectories with the fire location, and the stations colored by avg smoke PM2.5
# to be a little lazy, save some of the plot layers that we'll keep reusing into a list to add to the plot
plot_layers <- list(geom_sf(data = ca, inherit.aes = FALSE, fill = NA), 
                    geom_sf(data = eaton_fire, col = "red", inherit.aes = FALSE), 
                    geom_sf(data = st_as_sf(smokePM_avg), 
                            aes(color = smokePM), inherit.aes = FALSE), 
                    scale_color_viridis_c(limits = c(NA, 25), oob = scales::squish),
                    xlim(-120.5, -116), ylim(33, 35), 
                    theme_classic() + theme(axis.title = element_blank()))

{keep_traj %>%
  ggplot(aes(x = X11, y = X10, group = traj_id)) + 
  geom_path(color = "grey80", alpha = 0.3) + 
  geom_point(color = "grey80", alpha = 0.3, pch = 16)} %>%
  reduce(.x = plot_layers, .f = `+`, .init = .)

# to convert this to a continuous surface, take the trajectory points, buffer each by 20km(?), 
# define locations as high and moderate exposure if they have > X% of the max
# look at the buffered trajectories to see what this would look like
{keep_traj %>%
  # convert to sf object to buffer
  st_as_sf(coords = c("X11", "X10")) %>%
  st_set_crs(4326) %>%
  st_buffer(dist = traj_buffer) %>%
  ggplot(aes(group = traj_id)) + 
  geom_sf(alpha = 0.05, color = NA, fill = "grey80")} %>% 
  reduce(.x = plot_layers, .f = `+`, .init = .)

# to figure out what that X% should be, look at how many buffered points overlap each of the stations vs station avg smoke pm2.5
keep_traj %>%
  # convert to sf and buffer again
  st_as_sf(coords = c("X11", "X10")) %>%
  st_set_crs(4326) %>%
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

# calculate number of buffered trajectory points overlapping each of the census tract centroids 
keep_traj %>%
  st_as_sf(coords = c("X11", "X10")) %>%
  st_set_crs(4326) %>%
  st_buffer(dist = traj_buffer) %>%
  st_intersects(st_centroid(la_tracts), .) %>% # could also do pop weighted centroids. due to the cutpoints being defined based on a point, defining this as intersecting the entire tract would probably just mean that things intersect too many things. we'd need some measure of percentage of overlap or something. 
  purrr::map_dbl(length) %>% 
  cbind(la_tracts, n_point = .) -> tract_overlaps

exposure_df <- tract_overlaps %>% 
  mutate(smoke_category = case_when(n_point < thresh_low ~ "none", 
                               n_point < thresh_high ~ "mid", 
                               n_point >= thresh_high ~ "high", 
                               T ~ "error")) %>% 
  select(GEOID, n_point, smoke_category) 
# exposure_df data dictionary
  # GEOID: census tract ID
  # n_point: number of buffered trajectory points overlapping with the centroid of the CT
  # smoke_category: category of exposure based on the number of buffered trajectory points overlapping with the centroid of the CT

{exposure_df %>%
  ggplot(aes(fill = smoke_category)) + 
  geom_sf(color = NA, alpha = 0.9) + 
  geom_path(data = keep_traj,
            aes(x = X11, y = X10, group = traj_id), 
            color = "grey80", alpha = 0.3, inherit.aes = FALSE)} %>% 
  reduce(.x = plot_layers, .f = `+`, .init = .) 