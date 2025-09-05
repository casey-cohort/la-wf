#-------------------------------
# LA wildfires project
# author: Arnab Dey and Lara Schwarz, adapted by Lauren Wilner
# date: 2025-09-02
# this code loads the modeltime table and calculates training and testing error metrics

#-------------------------------
# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey, Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script loads the modeltime table with best models and calculates training and testing error metrics
# @date: Dec 16, 2024

#-------------------------------
# setup
rm(list = ls())
set.seed(0112358)
pacman::p_load(here, tidymodels, tidyverse, modeltime, Metrics)

# ensure consistent numeric precision 
options(digits = 7)
options(scipen = 999)

# set paths 
source(paste0(getwd(), "/02_code/paths.R"))

# load data ---------------------------------------------------
df_preintervention_all <- read_csv(paste0(path_repo, "01_data/02_clean/test_train/df-train-test_sf.csv"))
all_cases_all <-  read_csv(paste0(path_repo, "01_data/02_clean/test_train/df-predict-sf.csv"))

# Load the consolidated model results
load(paste0(path_repo, "03_output/all_model_tuning_results.RData"))

# an empty list to store results
results_list <- list()

# cause vars to loop over
causes <- colnames(df_preintervention_all) %>% str_subset("^num_enc")

# Iterate over each enc_type -- exposure_category -- cause combination 
for (enc in unique(df_preintervention_all$enc_type)) {
    print(enc)

  # loop through each exposure
  for (exposure in unique(df_preintervention_all$exposure_category)) {
    print(exposure)

    # subset data for this enc_type and exposure_category
    df_preintervention <- df_preintervention_all %>%
      filter(enc_type == enc, exposure_category == exposure)

    # loop through each cause
    for (cause in causes) {
      print(cause)

      # -----------------------------------------------
      # Extract the specific results for this combination
      current_results <- all_results[[enc]][[exposure]][[cause]]

      # Extract individual objects
      splits <- current_results$splits
      wflw_phxgb_tune <- current_results$wflw_phxgb_tune
      tune_results_phxgb <- current_results$tune_results_phxgb

      print(paste("Loaded Prophet-XGBoost model for", enc, exposure, cause))
      #  -----------------------------------------------
      # Tune model
      wflw_fit <- wflw_phxgb_tune |>
                finalize_workflow(select_best(tune_results_phxgb, metric = "rmse")) |>
                fit(training(splits))

      # generate modeltime table ---------------------------------------------------
      model_tbl <- modeltime_table(wflw_fit)

      # Training error metrics ---------------------------------------------------
      training_preds <- model_tbl %>%
        modeltime_calibrate(new_data = training(splits)) %>%
        select(.model_desc, .calibration_data) %>%
        unnest(cols = c(.calibration_data)) %>%
        mutate(.model_desc = "PROPHETXGB")

      df_training_metrics <- training_preds %>%
        group_by(.model_desc) %>%
        summarise(
          mdae = Metrics::mdae(.actual, .prediction),
          mae = Metrics::mae(.actual, .prediction),
          rmse = Metrics::rmse(.actual, .prediction),
          mape = Metrics::mape(.actual, .prediction),
          rse = Metrics::rse(.actual, .prediction),
          smape = Metrics::smape(.actual, .prediction),
          r2 = round(1 - sum((.actual - .prediction)^2) / sum((.actual - mean(.actual))^2), 2)
        ) %>%
        mutate(
          enc_type = enc,
          exposure_category = exposure,
          cause = cause,
          data_type = "training"
        )

      # Testing error metrics ----------------------------------------------------
      test_preds <- model_tbl %>%
        modeltime_calibrate(new_data = testing(splits)) %>%
        select(.model_desc, .calibration_data) %>%
        unnest(cols = c(.calibration_data)) %>%
        mutate(.model_desc = "PROPHETXGB")

      df_testing_metrics <- test_preds %>%
        group_by(.model_desc) %>%
        summarise(
          mdae = Metrics::mdae(.actual, .prediction),
          mae = Metrics::mae(.actual, .prediction),
          rmse = Metrics::rmse(.actual, .prediction),
          mape = Metrics::mape(.actual, .prediction),
          rse = Metrics::rse(.actual, .prediction),
          smape = Metrics::smape(.actual, .prediction),
          r2 = round(1 - sum((.actual - .prediction)^2) / sum((.actual - mean(.actual))^2), 2)
        ) %>%
        mutate(
          enc_type = enc,
          exposure_category = exposure,
          cause = cause,
          data_type = "testing"
        )

      print(df_training_metrics)
      print(df_testing_metrics)

      # Append results
      results_list[[length(results_list) + 1]] <- df_training_metrics
      results_list[[length(results_list) + 1]] <- df_testing_metrics

      # Save individual metrics
      # saveRDS(df_training_metrics, paste0(path_repo, "03_output/model-training-errors", enc, "_", exposure, "_", cause, ".rds"))
      # saveRDS(df_testing_metrics, paste0(path_repo, "03_output/model-test-errors", enc, "_", exposure, "_", cause, ".rds"))
      }
    }
  }

# Combine all metrics into one table ------------------------------------------
results_train_test_metrics <- bind_rows(results_list)

# Save final performance metrics
write.csv(results_train_test_metrics, paste0(path_repo, "03_output/performance_metrics.csv"), row.names = FALSE)
