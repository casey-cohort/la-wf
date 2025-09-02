#-------------------------------
# LA wildfires project
# author: Nina Flores, adapted by Lauren Wilner
# date: 2025-09-02
# this code processes data that were generated using google earth engine
# https://code.earthengine.google.com/d2d445308d1bfd4fe810ff138a3e68b3?noload=true
# it merges gridmet data with exposure data and calculates mean gridmet values by exposure level/date

#-------------------------------
# setup
# require(data.table)
require(dplyr)
require(tidyverse)

cov_dat_dir <- "/Users/laurenwilner/Library/CloudStorage/OneDrive-SharedLibraries-UW/casey_cohort\ -\ Documents/studies/la_wf_pm_evac_its/01_data/"
exp_dat_dir <- "~/Desktop/Desktop/epidemiology_PhD/00_repos/la-wf/01_data/"

#-------------------------------
# exp data
# read in gridmet data from gee 
gridmet_dat <- fread(paste0(cov_dat_dir, "01_raw/gridmet/gridmet-ct-LA-wf_aug2025.csv"))
gridmet_dat_clean <- gridmet_dat %>%
  mutate(date = as.Date(sub("_.*", "", `system:index`), format = "%Y%m%d")) %>%
  select(-`system:index`) %>%
  mutate(geoid10 = as.numeric(geoid10))
  
# read in exposure data
pm_exp_data <- read_csv(paste0(exp_dat_dir, "02_clean/exposed_cts_pm.csv")) %>%
  rename(exp_level = exposed_pm) %>%
  select(geoid, exp_level)
evac_exp_data <- read_csv(paste0(exp_dat_dir, "02_clean/exposed_cts_evac.csv")) %>%
  # recode so that we have an exp_level called "evac" and then only include those tracts
  # the other tracts will be added using the smoke exposure data 
  # we dont need them here bc `not exposed` will come from smoke data
  # evac just overrides any smoke classification
  mutate(exposed_evac = ifelse(exposed_evac == 1, "evac", "no_evac")) %>% 
  rename(exp_level = exposed_evac) %>%
  filter(exp_level == "evac") %>%
  select(geoid, exp_level)

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
  filter(!is.na(exp_level)) %>%
  group_by(date, exp_level) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  select(-geoid10)

#-------------------------------
# write
write.csv(grouped_gridmet,paste0(cov_dat_dir, "02_processed/gridmet/gridmet_cov_exp_level.csv"))

