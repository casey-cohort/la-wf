#-------------------------------
# LA ITS prep
# author: Lara Schwarz, adapted by Lauren Wilner
# date: 2025-09-02
# this code preps all data for the interrupted time series analysis

#-------------------------------
# setup
# Only clear environment if running standalone, not when sourced
if (!exists("pipeline_start_time")) {
  rm(list=ls())
}
if (!requireNamespace('pacman', quietly = TRUE)) {install.packages('pacman')}
pacman::p_load(tidyverse, readr, tidyr, purrr, lubridate, MMWRweek, here, arrow, yaml, readxl)

# set paths
source(paste0(getwd(), "/01_code/paths.R"))

# load config
config <- yaml::read_yaml(paste0(getwd(), "/01_code/02_analysis/model_config.yaml"))
rates_denom <- config$rates_denom  

# merge denoms with df_temp
# 1. pull out year and month in df_temp
# 2. merge with denoms on year and month
# 3. divide num_enc by denom to get rate
# 4. add rate to df_temp

#-------------------------------
# load data
df_temp <- read_csv(paste0(path_onedrive, "01_data/01_raw/ed_ipt_dat/2025-08-08/ENC_EXP_DAILY_2025-08-08.csv")) %>% 
   # clean names so there are no spaces
   mutate(exposure_category = str_replace_all(exposure_category, ",.*", ""),
         exposure_category = str_replace_all(exposure_category, " ", "_"),
         exposure_category = ifelse(exposure_category == "no_smoke", "none", exposure_category))

# load denoms 
denoms <- read_excel(paste0(path_onedrive, "01_data/01_raw/ed_ipt_dat/2025-12-30/denoms_enc_exp_daily_2025-12-30.xlsx")) %>%
  mutate(exposure_category = str_replace_all(exposure_category, ",.*", "")) %>%
  mutate(exposure_category = str_replace_all(exposure_category, " ", "_")) %>% 
  mutate(exposure_category = ifelse(exposure_category == "no_smoke", "none", exposure_category))

# resp covs
resp_virus<- read_csv(paste0(path_onedrive, "01_data/02_processed/wastewater_resp_illness_data/2025-09-02/resp-virus-dat_all.csv"))

# add meterological covariates
cov <- read_csv(paste0(path_onedrive, "01_data/02_processed/gridmet/2025-09-02/gridmet_cov_exposure_category.csv")) %>%
  mutate(encounter_dt = date,
         exposure_category = ifelse(exposure_category == "high", "high_smoke", 
                            ifelse(exposure_category == "mid", "mid_smoke", exposure_category))) %>%
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
  summarise(across(c(num_enc, num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury), sum, na.rm = TRUE)) %>%
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
# 
out_df <- df %>%
  select(enc_type, exposure_category, encounter_dt, num_enc, 
         num_enc_cardio, num_enc_resp, num_enc_neuro, num_enc_injury,
          pr, tmmx, tmmn, rmin, rmax, vs, srad, time_period, `influenza-a`, `influenza-b`, rsv, `sars-cov2`)

#--------------------------------
# create rates dataset
df_rates <- out_df %>%
  mutate(encounter_dt = as.Date(encounter_dt, format = "%m/%d/%Y")) %>%
  mutate(year = year(encounter_dt),
         month = month(encounter_dt)) %>%
  left_join(denoms, by = c("year", "month", "exposure_category")) %>%
  mutate(rate_enc = (num_enc / N) * rates_denom, 
         rate_enc_cardio = (num_enc_cardio / N) * rates_denom,
         rate_enc_resp = (num_enc_resp / N) * rates_denom,
         rate_enc_neuro = (num_enc_neuro / N) * rates_denom,
         rate_enc_injury = (num_enc_injury / N) * rates_denom)

#-------------------------------
# create training and testing dataset
df_train_test <- df_rates %>%
  mutate(
    date = as.Date(encounter_dt),
    month_day = format(date, "%m-%d"),
    year = year(date),
    postjan7 = ifelse(month_day < "01-07" | month_day > "01-21", 0, 1)
  ) %>%
  filter(!(month_day > "01-06" & year == 2025)) %>%
  select(-c('encounter_dt')) %>%
    mutate(influenza.a = `influenza-a` * 10000000,
            influenza.b = `influenza-b` * 10000000,
            rsv = rsv * 10000000,
            sars.cov2 = `sars-cov2` * 10000000) %>%
    select(-c(`influenza-a`, `influenza-b`, `sars-cov2`)) %>%
    mutate(across(where(is.numeric), as.integer)) %>%
    arrange(date)


dir.create(paste0(path_onedrive, "01_data/02_processed/train_test/", Sys.Date()), recursive = TRUE, showWarnings = FALSE)
write_parquet(df_train_test, paste0(path_onedrive, paste0( "01_data/02_processed/train_test/", Sys.Date(), "/df-train-test_sf.parquet")))

#-------------------------------
# create all cases dataset
df_all_cases <- df_rates %>%
  mutate(
    date = as.Date(encounter_dt),
    month_day = format(date, "%m-%d"),
    year = year(date),
    postjan7 = ifelse(month_day < "01-07" | month_day > "01-21", 0, 1)
  ) %>%
  # filter(!(month_day > "01-06" & year == 2025)) %>%
  select(-c('encounter_dt')) %>%
  mutate(influenza.a = `influenza-a` * 10000000,
            influenza.b = `influenza-b` * 10000000,
            rsv = `rsv` * 10000000,
            sars.cov2 = `sars-cov2` * 10000000) %>%
  select(-c(`influenza-a`, `influenza-b`, `sars-cov2`)) %>%
  mutate(across(where(is.numeric), as.integer)) %>%
  arrange(date)

# dont need to recreate bc it goes in the same filepath as the test train data! 
write_parquet(df_all_cases, paste0(path_onedrive, "01_data/02_processed/train_test/", Sys.Date(), "/df-predict-sf.parquet"))
