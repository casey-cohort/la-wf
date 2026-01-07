#-------------------------------
# LA wildfires project
# Generate plots, tables, and excess hospitalization calculations
#-------------------------------

cat("\n========================================\n")
cat("STARTING 05_model_outputs.R\n")
cat("========================================\n\n")

# Setup ----
pacman::p_load(tidyverse, ggplot2, patchwork, yardstick, gt, here, Metrics)

# Set paths and source utilities
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_general.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_tuning.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_outputs.R"))

# Validation function for metrics calculation ----
#' Validate data has sufficient observations for metrics
#'
#' @param data Dataframe with y_actual and yhat columns
#' @param data_name Name for logging purposes
#' @param min_n Minimum required observations (default 3)
#' @return TRUE if valid, FALSE otherwise
#'
validate_for_metrics <- function(data, data_name, min_n = 3) {
  if (nrow(data) == 0) {
    cat("  WARNING:", data_name, "is empty. Skipping metrics.\n")
    return(FALSE)
  }
  if (nrow(data) < min_n) {
    cat("  WARNING:", data_name, "has only", nrow(data), 
        "observations (min", min_n, "recommended). Skipping metrics.\n")
    return(FALSE)
  }
  return(TRUE)
}

# Find latest MBB results ----
latest_dir <- get_output_directory(path_onedrive)

# Load MBB results
mbb_results <- load_mbb_results(latest_dir)

# Extract timestamp
mod_ver_suffix <- extract_version_suffix(latest_dir)

# Create output directories ----
output_dirs <- create_output_subdirectories(latest_dir, c("figures", "performance_metrics"))
figures_dir <- output_dirs$figures
performance_metrics_dir <- output_dirs$performance_metrics

# Initialize results storage ----
all_metrics_list <- list()

# Process each model combination ----
cat("=== Generating Outputs ===\n")
cat("Number of encounter types:", length(mbb_results), "\n")
if (length(mbb_results) == 0) {
  stop("No MBB results found in mbb_results object")
}
cat("\n")

# Count total combinations and successful ones
total_combos <- 0
successful_combos <- 0
for (enc in names(mbb_results)) {
  for (exposure in names(mbb_results[[enc]])) {
    for (cause in names(mbb_results[[enc]][[exposure]])) {
      total_combos <- total_combos + 1
      result <- mbb_results[[enc]][[exposure]][[cause]]
      if (isTRUE(result$success)) {
        successful_combos <- successful_combos + 1
      }
    }
  }
}
cat("Total model combinations:", total_combos, "\n")
cat("Successful MBB results:", successful_combos, "\n")
if (successful_combos == 0) {
  cat("WARNING: No successful MBB results found. No outputs will be generated.\n")
  cat("Check MBB results for errors.\n\n")
}
cat("\n")

for (enc in names(mbb_results)) {
  for (exposure in names(mbb_results[[enc]])) {
    for (cause in names(mbb_results[[enc]][[exposure]])) {
      cat("Processing:", enc, "-", exposure, "-", cause, "\n")
      
      result <- mbb_results[[enc]][[exposure]][[cause]]
      
      # Skip if not successful
      if (!isTRUE(result$success)) {
        cat("  Skipping - MBB was not successful\n")
        if (!is.null(result$error)) {
          cat("  Error:", result$error, "\n")
        }
        cat("\n")
        next
      }
      
      tryCatch({
        # Extract data
        train_MBB <- result$train_summary
        test_MBB <- result$test_MBB
        holdout_MBB <- result$holdout_MBB
        train_obs <- result$train_df
        test_obs <- result$test_df
        holdout_obs <- result$holdout_df
        
        # ============================================================
        # 1. Calculate Metrics with MBB CIs
        # ============================================================
        
        cat("  Calculating metrics...\n")
        
        # Training metrics with validation
        if (!validate_for_metrics(train_MBB, "Training")) {
          train_metrics <- tibble(
            window = "train",
            mdae = NA_real_,
            mae = NA_real_,
            rmse = NA_real_,
            mape = NA_real_,
            rse = NA_real_,
            smape = NA_real_,
            mase = NA_real_,
            r2 = NA_real_
          )
        } else {
          train_metrics <- tibble(
            window = "train",
            mdae = Metrics::mdae(train_MBB$y_actual, train_MBB$yhat),
            mae = yardstick::mae_vec(train_MBB$y_actual, train_MBB$yhat),
            rmse = yardstick::rmse_vec(train_MBB$y_actual, train_MBB$yhat),
            mape = yardstick::mape_vec(train_MBB$y_actual, train_MBB$yhat),
            rse = Metrics::rse(train_MBB$y_actual, train_MBB$yhat),
            smape = Metrics::smape(train_MBB$y_actual, train_MBB$yhat),
            # MASE: in-sample for training (scaled by naive forecast on training data)
            mase = calc_mase(train_MBB$y_actual, train_MBB$yhat, train_MBB$y_actual, seasonality = config$mase_seasonality),
            r2 = round(1 - sum((train_MBB$y_actual - train_MBB$yhat)^2) / 
                         sum((train_MBB$y_actual - mean(train_MBB$y_actual))^2), 4)
          )
        }
        
        # Test metrics with validation
        if (!validate_for_metrics(test_MBB$pred_summary, "Test")) {
          test_metrics <- tibble(
            window = "test",
            mdae = NA_real_,
            mae = NA_real_,
            rmse = NA_real_,
            mape = NA_real_,
            rse = NA_real_,
            smape = NA_real_,
            mase = NA_real_,
            r2 = NA_real_
          )
        } else {
          test_metrics <- tibble(
            window = "test",
            mdae = Metrics::mdae(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
            mae = yardstick::mae_vec(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
            rmse = yardstick::rmse_vec(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
            mape = yardstick::mape_vec(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
            rse = Metrics::rse(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
            smape = Metrics::smape(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
            # MASE: test errors scaled by naive forecast errors from training data
            mase = calc_mase(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat, 
                            train_MBB$y_actual, seasonality = config$mase_seasonality),
            r2 = round(1 - sum((test_MBB$pred_summary$y_actual - test_MBB$pred_summary$yhat)^2) / 
                         sum((test_MBB$pred_summary$y_actual - mean(test_MBB$pred_summary$y_actual))^2), 4)
          )
        }
        
        # Holdout metrics with validation
        if (!is.null(holdout_MBB) && nrow(holdout_obs) > 0) {
          if (!validate_for_metrics(holdout_MBB$pred_summary, "Holdout")) {
            holdout_metrics <- tibble(
              window = "holdout",
              mdae = NA_real_,
              mae = NA_real_,
              rmse = NA_real_,
              mape = NA_real_,
              rse = NA_real_,
              smape = NA_real_,
              mase = NA_real_,
              r2 = NA_real_
            )
          } else {
            holdout_metrics <- tibble(
              window = "holdout",
              mdae = Metrics::mdae(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
              mae = yardstick::mae_vec(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
              rmse = yardstick::rmse_vec(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
              mape = yardstick::mape_vec(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
              rse = Metrics::rse(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
              smape = Metrics::smape(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
              # MASE: holdout errors scaled by naive forecast errors from training data
              mase = calc_mase(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat, 
                              train_MBB$y_actual, seasonality = config$mase_seasonality),
              r2 = round(1 - sum((holdout_MBB$pred_summary$y_actual - holdout_MBB$pred_summary$yhat)^2) / 
                           sum((holdout_MBB$pred_summary$y_actual - mean(holdout_MBB$pred_summary$y_actual))^2), 4)
            )
          }
        } else {
          holdout_metrics <- tibble(
            window = "holdout",
            mdae = NA_real_,
            mae = NA_real_,
            rmse = NA_real_,
            mape = NA_real_,
            rse = NA_real_,
            smape = NA_real_,
            mase = NA_real_,
            r2 = NA_real_
          )
        }
        
        # Combine metrics
        metrics_df <- bind_rows(train_metrics, test_metrics, holdout_metrics) %>%
          mutate(
            enc_type = enc,
            exposure_category = exposure,
            cause = cause
          )
        
        # Store for later aggregation
        combo_key <- paste(enc, exposure, cause, sep = "_")
        all_metrics_list[[combo_key]] <- metrics_df
        
        # ============================================================
        # 2. Create Plots
        # ============================================================
        
        cat("  Creating plots...\n")
        
        # Prepare visualization data
        df_train_vis <- train_obs %>%
          left_join(
            train_MBB %>% select(ds, yhat, yhat_lower = conf_lo, yhat_upper = conf_hi),
            by = c("date" = "ds")
          ) %>%
          mutate(count = !!sym(cause))
        
        df_test_vis <- test_obs %>%
          left_join(
            test_MBB$pred_summary %>% select(ds, yhat, yhat_lower = conf_lo, yhat_upper = conf_hi),
            by = c("date" = "ds")
          ) %>%
          mutate(count = !!sym(cause))
        
        # Extract date ranges for plot titles
        train_date_range <- paste0(
          format(min(df_train_vis$date, na.rm = TRUE), "%b %d, %Y"), " - ",
          format(max(df_train_vis$date, na.rm = TRUE), "%b %d, %Y")
        )
        test_date_range <- paste0(
          format(min(df_test_vis$date, na.rm = TRUE), "%b %d, %Y"), " - ",
          format(max(df_test_vis$date, na.rm = TRUE), "%b %d, %Y")
        )
        
        # Create plots
        p1 <- create_fit_plot(df_train_vis, paste0("A) Training (fit) (", train_date_range, ")"))
        p2 <- create_fit_plot(df_test_vis, paste0("B) Test (forecast with MBB CIs) (", test_date_range, ")"))
        
        # Add holdout plot if available
        if (!is.null(holdout_MBB) && nrow(holdout_obs) > 0) {
          df_holdout_vis <- holdout_obs %>%
            left_join(
              holdout_MBB$pred_summary %>% select(ds, yhat, yhat_lower = conf_lo, yhat_upper = conf_hi),
              by = c("date" = "ds")
            ) %>%
            mutate(count = !!sym(cause))
          
          p3 <- create_fit_plot(df_holdout_vis, "C) Holdout (post Jan 7, 2025 with MBB CIs)")
          
          # Combine all three plots
          p_combined <- (p1 / p2 / p3) +
            plot_annotation(
              title = paste0("Prophet + XGBoost: ", enc, " - ", exposure, " - ", cause),
              subtitle = paste0("Block length = ", result$L_block, " days, n_sim = ", result$n_sim, 
                               ", MAPE = ", round(test_metrics$mape, 2), 
                               ", MASE = ", round(test_metrics$mase, 2),
                               ", R² = ", test_metrics$r2)
            )
        } else {
          # Only train and test
          p_combined <- (p1 / p2) +
            plot_annotation(
              title = paste0("Prophet + XGBoost: ", enc, " - ", exposure, " - ", cause),
              subtitle = paste0("Block length = ", result$L_block, " days, n_sim = ", result$n_sim, 
                               ", MAPE = ", round(test_metrics$mape, 2), 
                               ", MASE = ", round(test_metrics$mase, 2),
                               ", R² = ", test_metrics$r2)
            )
        }
        
        # Save plot
        plot_file <- paste0(figures_dir, "model_fit_", enc, "_", exposure, "_", cause, ".pdf")
        cat("  Saving plot to:", plot_file, "\n")
        
        tryCatch({
          ggsave(plot_file, p_combined, width = 12, height = 10, dpi = 300)
          if (file.exists(plot_file)) {
            cat("  Plot saved successfully!\n")
          } else {
            cat("  WARNING: Plot file was not created!\n")
          }
        }, error = function(e) {
          cat("  ERROR saving plot:", as.character(e), "\n")
          print(traceback())
        })
        
        cat("  Completed successfully\n\n")
        
      }, error = function(e) {
        cat("  ERROR processing", enc, "-", exposure, "-", cause, ":\n")
        cat("    ", as.character(e), "\n")
        cat("    Traceback:\n")
        print(traceback())
        cat("\n")
        # Continue processing other combinations even if one fails
      })
    }
  }
}

# ============================================================
# Save Combined Outputs
# ============================================================

cat("\n=== Saving Combined Outputs ===\n")
cat("Number of metrics dataframes collected:", length(all_metrics_list), "\n\n")

# Save combined metrics
if (length(all_metrics_list) > 0) {
  all_metrics <- bind_rows(all_metrics_list)
  metrics_file <- paste0(performance_metrics_dir, "performance_metrics_with_mbb_", mod_ver_suffix, ".csv")
  write.csv(all_metrics, metrics_file, row.names = FALSE)
  cat("Combined metrics saved to:", metrics_file, "\n")
  cat("  Rows:", nrow(all_metrics), "\n")
} else {
  cat("WARNING: No metrics were collected. No metrics file will be saved.\n")
}

cat("\n=== Model Performance Metrics Complete! ===\n")
cat("\nAll outputs saved to:", latest_dir, "\n")
cat("  - Figures:", figures_dir, "\n")
cat("  - Performance metrics:", performance_metrics_dir, "\n")
cat("\nNote: Excess hospitalization calculations are generated separately in 07_gen_final_outputs.R\n")

