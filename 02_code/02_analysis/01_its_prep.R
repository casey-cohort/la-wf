#-------------------------------
# LA ITS prep
# author: Lara Schwarz, adapted by Lauren Wilner
# date: 2025-09-02
# this code preps all data for the interrupted time series analysis

#-------------------------------
# load packages
if (!requireNamespace('pacman', quietly = TRUE)) {install.packages('pacman')}
pacman::p_load(tidyverse, readr, tidyr, purrr, lubridate, MMWRweek, here)

# set paths
source(paste0(getwd(), "/02_code/paths.R"))

#-------------------------------
# load data
df_temp <- read_csv(paste0(path_onedrive, "01_data/01_raw/ed_ipt_dat/ENC_EXP_DAILY_08082025.csv")) %>% 
   # clean names so there are no spaces -- this will help since we name datasets based on exp cat
   # if we change this system, we can change this! 
   mutate(exposure_category = str_replace_all(exposure_category, ",.*", ""),
         exposure_category = str_replace_all(exposure_category, " ", "_"),
         exposure_category = ifelse(exposure_category == "no_smoke", "none", exposure_category))

# resp covs
resp_virus<- read_csv(paste0(path_onedrive, "01_data/02_processed/wastewater_resp_illness_data/resp-virus-dat_all.csv"))

# add meterological covariates
cov <- read_csv(paste0(path_onedrive, "01_data/02_processed/gridmet/gridmet_cov_exp_level.csv")) %>%
  mutate(encounter_dt = date) %>%
  rename(exposure_category = exp_level) %>%
  select(-date)

#-------------------------------
# restructure and merge in covariates
resp_virus_long <- resp_virus %>%
  pivot_longer(cols = `2022-2023`:`2024-2025`, 
               names_to = "time_period", 
               values_to = "value")%>%
  pivot_wider(names_from = resp_virus, values_from = value) %>%
  mutate(year = case_when(
    week < 40 ~ as.numeric(sub(".*-(\\d{4})", "\\1", time_period)),  # Extract second part
    week >= 40 ~ as.numeric(sub("(\\d{4})-.*", "\\1", time_period))  # Extract first part
  ))

df <- df_temp %>%
  group_by(exposure_category, enc_type, encounter_dt) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  mutate(encounter_dt = mdy(encounter_dt),
         mmwr_week=MMWRweek(encounter_dt)$MMWRweek,
         year=year(encounter_dt)) # create MMWR week variable

# add in flu/rsv data- merge both datasets based on week and year
df <- df %>%
  left_join(resp_virus_long, by = c("mmwr_week" = "week", "year")) %>%
  select(-c(time_period))

# Merge in environmental covariates
df <- left_join(df, cov, by = c("exposure_category", "encounter_dt"), relationship = "many-to-many")

# Ensure date column is in Date format
df <- df %>% 
  mutate(date = as.Date(encounter_dt, format = "%m/%d/%Y")) %>%
  # Define time periods
  mutate(
    time_period = case_when(
      date >= ymd("2022-11-01") & date <= ymd("2023-01-31") ~ 1,
      date >= ymd("2023-11-01") & date <= ymd("2024-01-31") ~ 2,
      date >= ymd("2024-11-01") & date <= ymd("2025-01-21") ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  drop_na(time_period)  # Remove rows outside defined time periods

#-------------------------------
# split data by encounter type and exposure level and create individual datasets
out_enc_data <- df %>%
  select(enc_type, exposure_category, encounter_dt, num_enc, 
         num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury,
          pr, tmmx, tmmn, rmin, rmax, vs, srad, time_period, `influenza-a`, `influenza-b`, rsv, `sars-cov2`) %>%
  group_by(enc_type, exposure_category) %>%
  nest() %>%
  mutate(dataset_name = paste0("df_", enc_type, "_", exposure_category),
         data = map2(data, dataset_name, ~mutate(.x, dataset_name = .y)))  # Add dataset_name column inside each dataset

# list of datasets
outcome_enc_datasets <- setNames(out_enc_data$data, out_enc_data$dataset_name)

#-------------------------------
# create csvs of data for analysis

# Iterate through datasets and save as CSV
for (dataset_name in names(outcome_enc_datasets)) {
  dataset <- outcome_enc_datasets[[dataset_name]]
  
  # Create df_train_test dataset
  df_train_test <- dataset %>%
    mutate(
      date = as.Date(encounter_dt),
      month_day = format(date, "%m-%d"),
      year = year(date),
      postjan7 = ifelse(month_day < "01-07" | month_day > "01-21", 0, 1)
    ) %>%
    filter(!(month_day > "01-06" & year == 2025)) %>%
    select(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury, date,
           pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period, `influenza-a`, `influenza-b`, rsv, `sars-cov2`) %>%
      mutate(`influenza-a` = `influenza-a` * 10000000,
             `influenza-b` = `influenza-b` * 10000000,
             rsv = rsv * 10000000,
             `sars-cov2` = `sars-cov2` * 10000000) %>%
      mutate(across(where(is.numeric), as.integer)) %>%
      arrange(date)

  
  write.csv(df_train_test, paste0(path_repo, paste0( "01_data/02_clean/test_train/df-train-test_sf_", dataset_name, ".csv")), row.names = FALSE)

  # Create df_all_cases dataset
  df_all_cases <- dataset %>%
    mutate(
      date = as.Date(encounter_dt),
      month_day = format(date, "%m-%d"),
      year = year(date),
      postjan7 = ifelse(month_day < "01-07" | month_day > "01-21", 0, 1)
    ) %>%
    select(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury, date,
           pr, tmmx, tmmn, rmin, rmax, vs, srad, postjan7, time_period, `influenza-a`, `influenza-b`, rsv, `sars-cov2`)
  
  write.csv(df_all_cases, paste0(path_repo, paste0( "01_data/02_clean/test_train/df-predict-sf_", dataset_name, ".csv")), row.names = FALSE)
}
