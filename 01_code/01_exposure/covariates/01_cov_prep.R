#-------------------------------
# LA wildfires project
# author: adapted by Lauren Wilner (original code author: Nina Flores)
# date: 2025-09-02
# this code processes data that were generated using google earth engine
# https://code.earthengine.google.com/d2d445308d1bfd4fe810ff138a3e68b3?noload=true
# it merges gridmet data with exposure data and calculates mean gridmet values by exposure level/date

#-------------------------------
# setup
rm(list=ls())
require(dplyr)
require(tidyverse)

# set paths
source(paste0(getwd(), "/01_code/paths.R"))

#-------------------------------
# exp data
# read in gridmet data from gee 
gridmet_dat <- read_csv(paste0(path_onedrive, "01_data/01_raw/gridmet/2025-09-02/gridmet-ct-LA-wf.csv"))
gridmet_dat_clean <- gridmet_dat %>%
  mutate(date = as.Date(sub("_.*", "", `system:index`), format = "%Y%m%d")) %>%
  select(-`system:index`) %>%
  mutate(geoid10 = as.numeric(geoid10))
  
# read in exposure data
pm_exp_data <- read_csv(paste0(path_onedrive, "01_data/02_processed/exposed_cts_pm.csv")) %>%
  rename(exposure_category = exposed_pm) %>%
  select(geoid, exposure_category)
evac_exp_data <- read_csv(paste0(path_onedrive, "01_data/02_processed/exposed_cts_evac.csv")) %>%
  # recode so that we have an exposure_category called "evac" and then only include those tracts
  # the other tracts will be added using the smoke exposure data 
  # we dont need them here bc `not exposed` will come from smoke data
  # evac just overrides any smoke classification
  mutate(exposed_evac = ifelse(exposed_evac == 1, "evac", "no_evac")) %>% 
  rename(exposure_category = exposed_evac) %>%
  filter(exposure_category == "evac") %>%
  select(geoid, exposure_category)

# remove any cts from pm_exp_data that are also in evac_exp_data
pm_exp_data <- pm_exp_data %>%
    filter(!geoid %in% evac_exp_data$geoid)

# combine exposure data
data <- rbind(pm_exp_data, evac_exp_data) %>% rename(geoid10 = geoid)

#-------------------------------
# gridmet data merge and collapse

# merge gridmet data with exposure data
data_with_gridmet <- data %>%
  janitor::clean_names() %>%
  mutate(geoid10 = as.numeric(geoid10)) %>%
  left_join(gridmet_dat_clean) 

# collapse over ct and calculate mean gridmet values by exposure level/date
grouped_gridmet <- data_with_gridmet  %>%
  filter(!is.na(exposure_category)) %>%
  group_by(date, exposure_category) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  select(-geoid10)

#-------------------------------
# write
write_csv(grouped_gridmet, paste0(path_onedrive, "01_data/02_processed/gridmet/2025-09-02/gridmet_cov_exposure_category.csv"))

