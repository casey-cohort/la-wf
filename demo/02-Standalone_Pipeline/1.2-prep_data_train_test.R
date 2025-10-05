# load libraries ----
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here, ggplot2, timetk, tidymodels)

# set paths ----
source(here("demo/paths.R"))

# load data ----
df_cur <- readRDS(here(path_processed, "df_high_smoke_no_evac_resp_ED.rds"))
range(df_cur$date)
nrow(df_cur)
tabyl(df_cur, period)
colnames(df_cur)
range(df_cur$date)

# create lagged features (lags 2-7) for predictors ----
base_predictors <- c("pr", "tmmx", "tmmn", "rmin", "rmax", "vs", "srad", "rsv", "influenza.a", "influenza.b", "sars.cov2")
lag_days <- 2:7

## Create lagged features for each predictor
df_cur <- df_cur |>
  arrange(date) |>  # Ensure data is sorted by date
  mutate(
    across(
      all_of(base_predictors),
      .fns = purrr::map(lag_days, ~function(x) lag(x, .x)) |> setNames(paste0("lag", lag_days)),
      .names = "{.col}_{.fn}"
    )
  )

## Remove rows with NA values from lagging (first 7 days)
df_cur <- df_cur |> filter(!is.na(pr_lag7))

## Generate full predictor list (base + lagged) for downstream use
lagged_predictors <- purrr::map(base_predictors, function(pred) {
  paste0(pred, "_lag", lag_days)
}) |> unlist()

predictor_cols <- c(base_predictors, lagged_predictors)

cat("Created lagged features (lags 2-7) for", length(base_predictors), "base predictors\n")
cat("Total predictors available:", length(predictor_cols), "(", length(base_predictors), "base +", length(lagged_predictors), "lagged )\n")
cat("Removed", max(lag_days), "rows due to lagging\n")
cat("Final dataset size:", nrow(df_cur), "rows\n")

# constants ----
holdout_date <- as.Date("2025-01-07")

# load functions ----
source(here("demo/utils/func_cv_dates.R"))

# split datasets into holdout and training/testing ----
## first create a true holdout dataset
df_holdout <- df_cur |>
  filter(date >= holdout_date)

## the rest of the data is for training and testing
df_train_test <- df_cur |>
  filter(date < holdout_date)

# split data into training and testing ----
tabyl(df_train_test, period) # i am going to start with the entire period of 2024-2025 as the test set
splits <- df_train_test |>
  time_series_split(
    assess = "67 days",
    cumulative = TRUE,
    date_var = date
  )

range(training(splits)$date)
range(testing(splits)$date)

# create CV folds ----
resamples_kfold <- training(splits) |>
  time_series_cv(
    assess = "30 days",
    skip = "7 days",
    initial = "90 days",
    slice_limit = 5, 
    cumulative = TRUE,
    verbose = FALSE,
    date_var = date
  )

# strategy archive

# resamples_kfold_s1 <- training(splits) |>
#   time_series_cv(
#     assess = "15 days",
#     skip = "7 days",
#     initial = "90 days",
#     slice_limit = 5,
#     cumulative = TRUE,
#     verbose = FALSE,
#     date_var = date
#   )  

# resamples_kfold_s2 <- training(splits) |>
#   time_series_cv(
#     assess = "15 days",
#     skip = "7 days",
#     initial = "90 days",
#     slice_limit = 5,
#     cumulative = TRUE,
#     verbose = FALSE,
#     date_var = date
#   )

plot_time_series_cv_plan(resamples_kfold, date, count)
get_cv_dates(resamples_kfold)
