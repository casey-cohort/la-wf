# load libraries ----
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)

# set paths ----
source(here("Codes", "demo", "paths.R"))

# load data ----
enc_data <- arrow::read_parquet(here(path_raw, "df-predict-sf.parquet"))
# enc_data <- read.csv(here(path_raw, "enc_data_daily.csv")) # This just has the outcome, no predictors

# basic data processing ----
enc_data_processed <- enc_data |>
  ungroup() |>
  mutate(date = as.Date(date, format = "%m/%d/%Y")) |>
  arrange(date) |>
  mutate(year = year(date)) |>
  mutate(month = month(date)) |>
  mutate(period = case_when(
    month >= 11 & year == 2022 ~ "2022-2023",
    month <= 3 & year == 2023 ~ "2022-2023",
    month >= 11 & year == 2023 ~ "2023-2024",
    month <= 3 & year == 2024 ~ "2023-2024",
    month >= 11 & year == 2024 ~ "2024-2025",
    month <= 3 & year == 2025 ~ "2024-2025",
    TRUE ~ "Other"
  )) |>
  mutate(count = num_enc_resp)  

head(enc_data_processed)
nrow(enc_data_processed)
colnames(enc_data_processed)
# create sub categories ----

## High smoke, No Evac, Respiratory cases - ED
df_high_smoke_no_evac_resp_ED <- enc_data_processed |> 
  filter(exposure_category == "high_smoke") |>
  filter(enc_type == "ED") |>
  mutate(day_within_period = row_number(), .by = period) 

print(nrow(df_high_smoke_no_evac_resp_ED)) # 266 days bw 2022-11-01 and 2025-01-21
range(df_high_smoke_no_evac_resp_ED$date)

## High smoke, No Evac, Respiratory cases - IP
df_high_smoke_no_evac_resp_IP <- enc_data_processed |> 
  filter(exposure_category == "high_smoke") |>
  filter(enc_type == "IP") |>
  mutate(day_within_period = row_number(), .by = period) 

print(nrow(df_high_smoke_no_evac_resp_IP)) # 266 days bw 2022-11-01 and 2025-01-21

# Save data ----
df_high_smoke_no_evac_resp_ED |> saveRDS(here(path_processed, "df_high_smoke_no_evac_resp_ED.rds"))
df_high_smoke_no_evac_resp_IP |> saveRDS(here(path_processed, "df_high_smoke_no_evac_resp_IP.rds"))


# Create a basic time series plot ----
# ## Prepare data for plotting
df_plot <- df_high_smoke_no_evac_resp_ED

# Create time series plot with different panels for each period
ggplot(df_plot, aes(x = day_within_period, y = count)) +
  geom_line(color = "#2E86AB", size = 1.2, alpha = 0.8) +
  geom_point(color = "#2E86AB", size = 1.5, alpha = 0.6) +
  # add vertical line for holdout date
  geom_vline(xintercept = 28, linetype = "dashed", color = "#E63946", size = 0.8) + # thanks giving
  geom_vline(xintercept = 55, linetype = "dashed", color = "#E63946", size = 0.8) + # christmas
  geom_vline(xintercept = 61, linetype = "dashed", color = "#E63946", size = 0.8) + # new year's day
  geom_vline(xintercept = 68, linetype = "dashed", color = "#E63946", size = 0.8) + # Event
  facet_wrap(~ period, ncol = 1, scales = "free_x") +
  labs(
    title = "Time Series Plot by Study Period",
    subtitle = "Emergency Department Encounters During Wildfire Seasons",
    x = "Day within Period",
    y = "Daily Count",
    caption = "Vertical lines indicate key dates: Thanksgiving (28), Christmas (55), New Year's (61), Event (68)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#1D3557"),
    plot.subtitle = element_text(size = 12, color = "#457B9D", margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "#6C757D", hjust = 0),
    axis.title = element_text(size = 12, color = "#1D3557"),
    axis.text = element_text(size = 10, color = "#495057"),
    strip.text = element_text(size = 11, face = "bold", color = "#1D3557"),
    strip.background = element_rect(fill = "#F8F9FA", color = "#DEE2E6"),
    panel.grid.major = element_line(color = "#E9ECEF", size = 0.5),
    panel.grid.minor = element_line(color = "#F8F9FA", size = 0.3),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(here(path_figures, "time_series_resp_ED.pdf"), width = 10, height = 12, dpi = 300)
