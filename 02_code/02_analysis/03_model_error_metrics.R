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
pacman::p_load(here, tidymodels, tidyverse, modeltime, Metrics)

# ensure consistent numeric precision 
options(digits = 7)
options(scipen = 999)

# set paths 
source(paste0(getwd(), "/02_code/paths.R"))

#-------------------------------
# load data 
df_preintervention_all <- read_csv(paste0(path_repo, "01_data/02_clean/test_train/df-train-test_sf.csv"))
all_cases_all <-  read_csv(paste0(path_repo, "01_data/02_clean/test_train/df-predict-sf.csv"))

# load latest model tuning results
latest_file <- sort(list.files(paste0(path_repo, "03_output/"), 
                              pattern = "^all_results_nested_.*\\.RData$", 
                              full.names = TRUE), 
                   decreasing = TRUE)[1]
load(latest_file)

# an empty list to store results
results_list <- list()

# cause vars to loop over
causes <- colnames(df_preintervention_all) %>% str_subset("^num_enc")

#-------------------------------
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

      # Check if this combination exists in results
      if (is.null(all_results[[enc]]) || 
          is.null(all_results[[enc]][[exposure]]) || 
          is.null(all_results[[enc]][[exposure]][[cause]])) {
        print(paste("SKIPPING: No results found for", enc, exposure, cause))
        next
      }

      # Extract the specific results for this combination & check if they are valid
      current_results <- all_results[[enc]][[exposure]][[cause]]
      if (is.null(current_results$success) || !current_results$success) {
        print(paste("SKIPPING: Failed results for", enc, exposure, cause))
        next
      }

      # Extract individual objects
      splits <- current_results$splits
      wflw_phxgb_tune <- current_results$wflw_phxgb_tune
      tune_results_phxgb <- current_results$tune_results_phxgb
      print(paste("Loaded Prophet-XGBoost model for", enc, exposure, cause))
      
      # RETRY if it failed -----------------------------------------------
      max_retries <- 5
      retry_count <- 0
      success <- FALSE
      wflw_fit <- NULL

      while (!success && retry_count < max_retries) {
        tryCatch({

          # fit the model 
          wflw_fit_seed <- gen_seed(0112358, c(enc, exposure, cause, "wflw_fit"))
          set.seed(wflw_fit_seed)
          print(paste("  Attempt", retry_count + 1, "with seed", wflw_fit_seed))
          suppressWarnings({
            suppressMessages({
              wflw_fit <- wflw_phxgb_tune |>
                        finalize_workflow(select_best(tune_results_phxgb, metric = "rmse")) |>
                        fit(training(splits))
            })
          })
          
          # test if the model actually works by making a small prediction
          test_pred <- suppressWarnings({
            suppressMessages({
              predict(wflw_fit, new_data = head(training(splits), 5))
            })
          })
          
          # if we get here without error and have valid predictions, it worked
          if (!is.null(test_pred) && nrow(test_pred) > 0 && !any(is.na(test_pred$.pred))) {
            success <- TRUE
            print(paste("  Success on attempt", retry_count + 1))
          } else {
            stop("Model fitted but predictions are invalid")
          }
          
        }, error = function(e) {
          # retry if there was an error
          retry_count <<- retry_count + 1
          print(paste("  Attempt", retry_count, "failed for", enc, exposure, cause, ":", e$message))
          if (retry_count >= max_retries) {
            print(paste("  Max retries reached, skipping", enc, exposure, cause))
          } else {
            print(paste("  Retrying with different seed..."))
            Sys.sleep(1)  # lets take a quick pause between retries
          }
        })
      }

      # if we successfully fitted the model
      if (success && !is.null(wflw_fit)) {
        
        tryCatch({
          # generate modeltime table with error handling
          model_tbl <- suppressWarnings({
            suppressMessages({
              modeltime_table(wflw_fit)
            })
          })

          # training error metrics
          training_preds <- suppressWarnings({
            suppressMessages({
              model_tbl %>%
                modeltime_calibrate(new_data = training(splits)) %>%
                select(.model_desc, .calibration_data) %>%
                unnest(cols = c(.calibration_data)) %>%
                mutate(.model_desc = "PROPHETXGB")
            })
          })

          # Check if we have valid training predictions
          if (nrow(training_preds) > 0 && !all(is.na(training_preds$.prediction))) {
            df_training_metrics <- training_preds %>%
              group_by(.model_desc) %>%
              summarise(
                mdae = Metrics::mdae(.actual, .prediction),
                mae = Metrics::mae(.actual, .prediction),
                rmse = Metrics::rmse(.actual, .prediction),
                mape = Metrics::mape(.actual, .prediction),
                rse = Metrics::rse(.actual, .prediction),
                smape = Metrics::smape(.actual, .prediction),
                r2 = round(1 - sum((.actual - .prediction)^2) / sum((.actual - mean(.actual))^2), 2),
                .groups = 'drop'
              ) %>%
              mutate(
                enc_type = enc,
                exposure_category = exposure,
                cause = cause,
                data_type = "training"
              )

            # testing error metrics
            test_preds <- suppressWarnings({
              suppressMessages({
                model_tbl %>%
                  modeltime_calibrate(new_data = testing(splits)) %>%
                  select(.model_desc, .calibration_data) %>%
                  unnest(cols = c(.calibration_data)) %>%
                  mutate(.model_desc = "PROPHETXGB")
              })
            })

            # do we have valid test predictions?
            if (nrow(test_preds) > 0 && !all(is.na(test_preds$.prediction))) {
              df_testing_metrics <- test_preds %>%
                group_by(.model_desc) %>%
                summarise(
                  mdae = Metrics::mdae(.actual, .prediction),
                  mae = Metrics::mae(.actual, .prediction),
                  rmse = Metrics::rmse(.actual, .prediction),
                  mape = Metrics::mape(.actual, .prediction),
                  rse = Metrics::rse(.actual, .prediction),
                  smape = Metrics::smape(.actual, .prediction),
                  r2 = round(1 - sum((.actual - .prediction)^2) / sum((.actual - mean(.actual))^2), 2),
                  .groups = 'drop'
                ) %>%
                mutate(
                  enc_type = enc,
                  exposure_category = exposure,
                  cause = cause,
                  data_type = "testing"
                )

              print("Training metrics:")
              print(df_training_metrics)
              print("Testing metrics:")
              print(df_testing_metrics)

              # using named list entries to prevent duplicates
              training_key <- paste(enc, exposure, cause, "training", sep = "_")
              testing_key <- paste(enc, exposure, cause, "testing", sep = "_")
              
              results_list[[training_key]] <- df_training_metrics
              results_list[[testing_key]] <- df_testing_metrics


            # error handling
            } else {
              print(paste("FAILED: Invalid test predictions for", enc, exposure, cause))
            }
          } else {
            print(paste("FAILED: Invalid training predictions for", enc, exposure, cause))
          }
        }, error = function(e) {
          print(paste("FAILED: Error in metrics calculation for", enc, exposure, cause, ":", e$message))
        })
      } else {
        print(paste("FAILED: Could not fit model for", enc, exposure, cause, "after", max_retries, "attempts"))
      }

      gc()
    }
  }
}

#-------------------------------
# combine all metrics into one table
if (length(results_list) > 0) {
  # combine all results into one dataframe
  results_train_test_metrics <- bind_rows(results_list)
  print(paste("Final dataset has", nrow(results_train_test_metrics), "rows"))
  
  # save final performance metrics
  # pull out date and timestamp from original rdata obj to save this out with
  timestamp <- tools::file_path_sans_ext(basename(latest_file)) %>%
    str_replace("all_results_nested_", "")
  results_filename <- paste0("performance_metrics_", timestamp, ".csv")
  write.csv(results_train_test_metrics, paste0(path_repo, "03_output/", results_filename), row.names = FALSE)
  print("Performance metrics saved successfully")
} else {
  print("WARNING: No successful results to save")
}