#-------------------------------
# LA wildfires project
# author: Lauren Wilner
# date: 2026-09-07
# Build supplement-ready covariate summaries and a correlation matrix.
# Writes CSVs to OneDrive for the paper repo to format as tables.
#
# Outputs (in 01_data/02_processed/02_covariates/supplement/YYYY-MM-DD/):
#   - covariate_dictionary.csv      labels, units, sources
#   - covariate_summary_long.csv    mean/SD/min/max by covariate, exposure, period
#   - covariate_table.csv           table-ready mean (SD) by exposure group
#   - covariate_correlation_long.csv Spearman rho among model covariates
#   - covariate_correlation_wide.csv same correlations, one matrix per exposure x period
#-------------------------------

# setup
rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, readr, MMWRweek)

source(file.path(getwd(), "01_code/paths.R"))

# paths.R currently points at "Documents 2", which is not present on this machine
if (!dir.exists(path_onedrive)) {
  path_onedrive <- "/Users/laurenwilner/Library/CloudStorage/OneDrive-SharedLibraries-UW/casey_cohort - Documents/studies/la_wf_pm_evac_its/"
  if (!dir.exists(path_onedrive)) {
    stop("Could not find OneDrive project folder. Check 01_code/paths.R")
  }
  message("Using OneDrive path: ", path_onedrive)
}

#-------------------------------
# labels and rounding for the paper table
cov_dictionary <- tibble::tribble(
  ~covariate,      ~label,                                  ~unit,                         ~source,              ~digits,
  "pr",            "Precipitation",                         "mm",                          "gridMET",            2,
  "tmmx",          "Maximum temperature",                   "°C",                          "gridMET",            1,
  "tmmn",          "Minimum temperature",                   "°C",                          "gridMET",            1,
  "rmin",          "Minimum relative humidity",             "%",                           "gridMET",            1,
  "rmax",          "Maximum relative humidity",             "%",                           "gridMET",            1,
  "vs",            "Wind speed",                            "m/s",                         "gridMET",            2,
  "srad",          "Downward shortwave radiation",          "W/m²",                        "gridMET",            1,
  "influenza_a",   "Influenza A wastewater concentration",  "concentration (dimensionless)","LA County wastewater", 6,
  "influenza_b",   "Influenza B wastewater concentration",  "concentration (dimensionless)","LA County wastewater", 6,
  "rsv",           "RSV wastewater concentration",          "concentration (dimensionless)","LA County wastewater", 6,
  "sars_cov2",     "SARS-CoV-2 wastewater concentration",   "concentration (dimensionless)","LA County wastewater", 6
)

exposure_levels <- c("evac", "high_smoke", "mid_smoke", "none")
exposure_labels <- c(
  evac = "Evacuation",
  high_smoke = "High smoke",
  mid_smoke = "Mid smoke",
  none = "No smoke"
)

# same analysis windows as 01_data_prep.R
in_study_period <- function(date) {
  (date >= ymd("2022-11-01") & date <= ymd("2023-01-31")) |
    (date >= ymd("2023-11-01") & date <= ymd("2024-01-31")) |
    (date >= ymd("2024-11-01") & date <= ymd("2025-01-21"))
}

add_period <- function(df) {
  df %>%
    mutate(
      period = case_when(
        date >= ymd("2025-01-07") & date <= ymd("2025-01-21") ~ "fire_period",
        in_study_period(date) ~ "historical",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period))
}

fmt_mean_sd <- function(mean_val, sd_val, digits) {
  ifelse(
    is.na(mean_val),
    NA_character_,
    sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean_val, sd_val)
  )
}

summarise_covs <- function(df, vars) {
  df %>%
    summarise(
      n_days = n(),
      across(
        all_of(vars),
        list(mean = ~mean(.x, na.rm = TRUE),
             sd = ~sd(.x, na.rm = TRUE),
             min = ~min(.x, na.rm = TRUE),
             max = ~max(.x, na.rm = TRUE)),
        .names = "{.col}__{.fn}"
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = matches("__"),
      names_to = c("covariate", "stat"),
      names_sep = "__",
      values_to = "value"
    ) %>%
    pivot_wider(names_from = stat, values_from = value)
}

#-------------------------------
# load met covariates (tract-mean gridMET by exposure category and date)
met <- read_csv(
  file.path(path_onedrive, "01_data/02_processed/02_covariates/gridmet/2025-09-02/gridmet_cov_exposure_category.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date),
    exposure_category = recode(
      exposure_category,
      high = "high_smoke",
      mid = "mid_smoke"
    ),
    # gridMET stores temperature in Kelvin
    tmmx = tmmx - 273.15,
    tmmn = tmmn - 273.15
  ) %>%
  select(date, exposure_category, pr, tmmx, tmmn, rmin, rmax, vs, srad) %>%
  add_period() %>%
  mutate(exposure_category = factor(exposure_category, levels = exposure_levels))

# load wastewater viruses (region-wide; not exposure-specific)
resp_virus <- read_csv(
  file.path(path_onedrive, "01_data/02_processed/02_covariates/wastewater_resp_illness_data/2025-09-02/resp-virus-dat_all.csv"),
  show_col_types = FALSE
)

virus_daily <- resp_virus %>%
  pivot_longer(cols = `2022-2023`:`2024-2025`, names_to = "season", values_to = "value") %>%
  pivot_wider(names_from = resp_virus, values_from = value) %>%
  mutate(
    year = if_else(
      week < 40,
      as.numeric(sub(".*-(\\d{4})", "\\1", season)),
      as.numeric(sub("(\\d{4})-.*", "\\1", season))
    )
  ) %>%
  # expand MMWR week to daily dates so virus rows line up with met days
  rowwise() %>%
  mutate(week_start = MMWRweek::MMWRweek2Date(year, week)) %>%
  ungroup() %>%
  select(week_start, `influenza-a`, `influenza-b`, rsv, `sars-cov2`) %>%
  mutate(
    influenza_a = `influenza-a`,
    influenza_b = `influenza-b`,
    sars_cov2 = `sars-cov2`
  ) %>%
  select(week_start, influenza_a, influenza_b, rsv, sars_cov2) %>%
  crossing(offset = 0:6) %>%
  mutate(date = week_start + offset) %>%
  select(-week_start, -offset) %>%
  add_period()

# merge so correlations can include viruses as they enter the model
cov_daily <- met %>%
  left_join(virus_daily, by = c("date", "period"))

met_vars <- c("pr", "tmmx", "tmmn", "rmin", "rmax", "vs", "srad")
virus_vars <- c("influenza_a", "influenza_b", "rsv", "sars_cov2")
all_vars <- c(met_vars, virus_vars)

#-------------------------------
# summaries: met by exposure; viruses are region-wide
met_summary <- bind_rows(
  met %>%
    group_by(exposure_category) %>%
    summarise_covs(met_vars) %>%
    mutate(period = "study_period"),
  met %>%
    group_by(exposure_category, period) %>%
    summarise_covs(met_vars)
)

virus_summary <- bind_rows(
  virus_daily %>%
    summarise_covs(virus_vars) %>%
    mutate(period = "study_period", exposure_category = "region_wide"),
  virus_daily %>%
    group_by(period) %>%
    summarise_covs(virus_vars) %>%
    mutate(exposure_category = "region_wide")
)

summary_long <- bind_rows(met_summary, virus_summary) %>%
  left_join(cov_dictionary, by = "covariate") %>%
  mutate(
    exposure_label = recode(
      as.character(exposure_category),
      !!!c(exposure_labels, region_wide = "Region-wide")
    ),
    period_label = recode(
      period,
      study_period = "Full study period",
      historical = "Historical (Nov–Jan, excluding 7–21 Jan 2025)",
      fire_period = "Fire period (7–21 Jan 2025)"
    ),
    mean_sd = fmt_mean_sd(mean, sd, digits)
  ) %>%
  select(
    covariate, label, unit, source, period, period_label,
    exposure_category, exposure_label, n_days,
    mean, sd, min, max, mean_sd
  ) %>%
  arrange(period, match(covariate, cov_dictionary$covariate), match(as.character(exposure_category), c(exposure_levels, "region_wide")))

# table-ready wide file: one row per covariate x period, columns = exposure groups
# viruses are repeated across exposure columns because they do not vary by exposure
virus_table <- virus_summary %>%
  left_join(cov_dictionary, by = "covariate") %>%
  mutate(mean_sd = fmt_mean_sd(mean, sd, digits)) %>%
  select(covariate, period, mean_sd) %>%
  crossing(exposure_category = exposure_levels)

met_table <- met_summary %>%
  left_join(cov_dictionary, by = "covariate") %>%
  mutate(mean_sd = fmt_mean_sd(mean, sd, digits)) %>%
  select(covariate, period, exposure_category, mean_sd)

cov_table <- bind_rows(met_table, virus_table) %>%
  mutate(exposure_category = factor(exposure_category, levels = exposure_levels)) %>%
  left_join(cov_dictionary %>% select(covariate, label, unit, source), by = "covariate") %>%
  mutate(
    period_label = recode(
      period,
      study_period = "Full study period",
      historical = "Historical (Nov–Jan, excluding 7–21 Jan 2025)",
      fire_period = "Fire period (7–21 Jan 2025)"
    )
  ) %>%
  mutate(exposure_label = recode(as.character(exposure_category), !!!exposure_labels)) %>%
  select(covariate, label, unit, source, period, period_label, exposure_label, mean_sd) %>%
  pivot_wider(names_from = exposure_label, values_from = mean_sd) %>%
  arrange(period, match(covariate, cov_dictionary$covariate))

#-------------------------------
# correlation matrix of covariates as used in the models (daily, by exposure)
cor_long <- cov_daily %>%
  bind_rows(cov_daily %>% mutate(period = "study_period")) %>%
  group_by(exposure_category, period) %>%
  group_modify(function(df, key) {
    mat <- df %>%
      select(all_of(all_vars)) %>%
      cor(use = "pairwise.complete.obs", method = "spearman")
    as_tibble(mat, rownames = "covariate_1") %>%
      pivot_longer(-covariate_1, names_to = "covariate_2", values_to = "correlation")
  }) %>%
  ungroup() %>%
  left_join(cov_dictionary %>% select(covariate, label_1 = label), by = c("covariate_1" = "covariate")) %>%
  left_join(cov_dictionary %>% select(covariate, label_2 = label), by = c("covariate_2" = "covariate")) %>%
  mutate(
    exposure_label = recode(as.character(exposure_category), !!!exposure_labels),
    period_label = recode(
      period,
      study_period = "Full study period",
      historical = "Historical (Nov–Jan, excluding 7–21 Jan 2025)",
      fire_period = "Fire period (7–21 Jan 2025)"
    )
  )

cor_wide <- cor_long %>%
  select(exposure_category, exposure_label, period, period_label, covariate_1, covariate_2, correlation) %>%
  pivot_wider(names_from = covariate_2, values_from = correlation) %>%
  arrange(period, match(as.character(exposure_category), exposure_levels), match(covariate_1, all_vars))

#-------------------------------
# write
out_dir <- file.path(path_onedrive, "01_data/02_processed/02_covariates/supplement", as.character(Sys.Date()))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(cov_dictionary %>% select(-digits), file.path(out_dir, "covariate_dictionary.csv"))
write_csv(summary_long, file.path(out_dir, "covariate_summary_long.csv"))
write_csv(cov_table, file.path(out_dir, "covariate_table.csv"))
write_csv(cor_long, file.path(out_dir, "covariate_correlation_long.csv"))
write_csv(cor_wide, file.path(out_dir, "covariate_correlation_wide.csv"))

cat("Wrote supplement covariate files to:\n", out_dir, "\n", sep = "")
cat("  covariate_table.csv              mean (SD) table by exposure\n")
cat("  covariate_summary_long.csv       numeric summary (mean, SD, min, max)\n")
cat("  covariate_correlation_wide.csv   correlation matrix\n")
cat("  covariate_correlation_long.csv   same correlations, long format\n")
cat("  covariate_dictionary.csv         labels and units\n")
