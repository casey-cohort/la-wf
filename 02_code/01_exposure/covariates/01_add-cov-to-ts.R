require(data.table)
require(dplyr)
require(tidyverse)

setwd("~/Desktop/projects/casey cohort/LA-wildfires/data/raw-data")
gridmet_dat <- fread("gridmet-ct-LA-wf.csv")

gridmet_dat_clean <- gridmet_dat %>%
  mutate(date = as.Date(sub("_.*", "", `system:index`), format = "%Y%m%d")) %>%
  select(-`system:index`) %>%
  mutate(geoid10 = as.numeric(geoid10))
  

data <- read.csv("ct_exposed_poverty_num_Jan17boundary_2025_01_27.csv") %>%
  select(-X) %>%
  mutate(
    exp_level = case_when(
      exp_pov %in% c(0, 1) ~ "least",
      exp_pov %in% c(2, 3) ~ "moderate",
      exp_pov %in% c(4, 5) ~ "high",
      TRUE ~ NA_character_
    )
  ) %>%
  select(GEOID10, exp_level)

data_with_gridmet <- data %>%
  janitor::clean_names() %>%
  mutate(geoid10 = as.numeric(geoid10)) %>%
  left_join(gridmet_dat_clean) 

grouped_gridmet <- data_with_gridmet  %>%
  filter(!is.na(exp_level)) %>%
  group_by(date, exp_level) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  select(-geoid10)
setwd("~/Desktop/projects/casey cohort/LA-wildfires/data/processed-data")
fwrite(grouped_gridmet,"gridmet_cov_exp_level.csv")

