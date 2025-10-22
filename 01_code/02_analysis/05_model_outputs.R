#-------------------------------
# LA wildfires project
# Generate plots, tables, and excess hospitalization calculations
#-------------------------------

# Setup ----
pacman::p_load(tidyverse, ggplot2, patchwork, yardstick, gt, here, Metrics)

# Set paths and source utilities
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/utils.R"))
source(paste0(getwd(), "/01_code/utils_outputs.R"))

# Find latest MBB results ----
# Use new folder naming pattern: model_run_YYYY-MM-DD.v###_x##_sim###
find_latest_version <- function(output_path) {
  output_dirs <- list.dirs(output_path, full.names = TRUE, recursive = FALSE)
  # Filter by new pattern
  pattern <- "^model_run_\\d{4}-\\d{2}-\\d{2}\\.v\\d{3}_x\\d+_sim\\d+$"
  output_dirs <- output_dirs[grepl(pattern, basename(output_dirs))]
  if (length(output_dirs) == 0) {
    return(NULL)
  }
  latest_dir <- output_dirs[order(basename(output_dirs), decreasing = TRUE)][1]
  return(latest_dir)
}

latest_dir <- find_latest_version(paste0(path_onedrive, "02_output/"))

if (is.null(latest_dir)) {
  stop("No model output directories found. Please run 04_model_mbb_cis.R first.")
}

cat("Loading MBB results from:", latest_dir, "\n")

# Find the MBB results file
mbb_files <- list.files(latest_dir, pattern = "mbb_results_nested_.*\\.rds", full.names = TRUE)
if (length(mbb_files) == 0) {
  stop("No MBB results file found. Please run 04_model_mbb_cis.R first.")
}
mbb_file <- mbb_files[1]
cat("Loading:", mbb_file, "\n\n")

mbb_results <- readRDS(mbb_file)

# Extract timestamp
mod_ver_suffix <- sub("model_run_", "", basename(latest_dir))

# Create output directories ----
figures_dir <- paste0(latest_dir, "/figures/")
tables_dir <- paste0(latest_dir, "/tables/")
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

cat("Output directories created:\n")
cat("  Figures:", figures_dir, "\n")
cat("  Tables:", tables_dir, "\n\n")

# Initialize results storage ----
all_metrics_list <- list()
all_excess_list <- list()

# Process each model combination ----
cat("=== Generating Outputs ===\n\n")

for (enc in names(mbb_results)) {
  for (exposure in names(mbb_results[[enc]])) {
    for (cause in names(mbb_results[[enc]][[exposure]])) {
      cat("Processing:", enc, "-", exposure, "-", cause, "\n")
      
      result <- mbb_results[[enc]][[exposure]][[cause]]
      
      # Skip if not successful
      if (!isTRUE(result$success)) {
        cat("  Skipping - MBB was not successful\n\n")
        next
      }
      
      tryCatch({
        # Extract data
        train_summary <- result$train_summary
        test_MBB <- result$test_MBB
        holdout_MBB <- result$holdout_MBB
        train_df <- result$train_df
        test_df <- result$test_df
        holdout_df <- result$holdout_df
        
        # ============================================================
        # 1. Calculate Metrics with MBB CIs
        # ============================================================
        
        cat("  Calculating metrics...\n")
        
        # Training metrics
        train_metrics <- tibble(
          window = "train",
          mdae = Metrics::mdae(train_summary$y_actual, train_summary$yhat),
          mae = yardstick::mae_vec(train_summary$y_actual, train_summary$yhat),
          rmse = yardstick::rmse_vec(train_summary$y_actual, train_summary$yhat),
          mape = yardstick::mape_vec(train_summary$y_actual, train_summary$yhat),
          rse = Metrics::rse(train_summary$y_actual, train_summary$yhat),
          smape = Metrics::smape(train_summary$y_actual, train_summary$yhat),
          r2 = round(1 - sum((train_summary$y_actual - train_summary$yhat)^2) / 
                       sum((train_summary$y_actual - mean(train_summary$y_actual))^2), 4)
        )
        
        # Test metrics
        test_metrics <- tibble(
          window = "test",
          mdae = Metrics::mdae(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
          mae = yardstick::mae_vec(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
          rmse = yardstick::rmse_vec(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
          mape = yardstick::mape_vec(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
          rse = Metrics::rse(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
          smape = Metrics::smape(test_MBB$pred_summary$y_actual, test_MBB$pred_summary$yhat),
          r2 = round(1 - sum((test_MBB$pred_summary$y_actual - test_MBB$pred_summary$yhat)^2) / 
                       sum((test_MBB$pred_summary$y_actual - mean(test_MBB$pred_summary$y_actual))^2), 4)
        )
        
        # Holdout metrics
        if (!is.null(holdout_MBB) && nrow(holdout_df) > 0) {
          holdout_metrics <- tibble(
            window = "holdout",
            mdae = Metrics::mdae(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
            mae = yardstick::mae_vec(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
            rmse = yardstick::rmse_vec(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
            mape = yardstick::mape_vec(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
            rse = Metrics::rse(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
            smape = Metrics::smape(holdout_MBB$pred_summary$y_actual, holdout_MBB$pred_summary$yhat),
            r2 = round(1 - sum((holdout_MBB$pred_summary$y_actual - holdout_MBB$pred_summary$yhat)^2) / 
                         sum((holdout_MBB$pred_summary$y_actual - mean(holdout_MBB$pred_summary$y_actual))^2), 4)
          )
        } else {
          holdout_metrics <- tibble(
            window = "holdout",
            mdae = NA_real_,
            mae = NA_real_,
            rmse = NA_real_,
            mape = NA_real_,
            rse = NA_real_,
            smape = NA_real_,
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
        df_train_vis <- train_df %>%
          left_join(
            train_summary %>% select(ds, yhat, yhat_lower = conf_lo, yhat_upper = conf_hi),
            by = c("date" = "ds")
          ) %>%
          mutate(count = !!sym(cause))
        
        df_test_vis <- test_df %>%
          left_join(
            test_MBB$pred_summary %>% select(ds, yhat, yhat_lower = conf_lo, yhat_upper = conf_hi),
            by = c("date" = "ds")
          ) %>%
          mutate(count = !!sym(cause))
        
        # Create plots
        p1 <- create_fit_plot(df_train_vis, "A) Training (fit)")
        p2 <- create_fit_plot(df_test_vis, "B) Test (forecast with MBB CIs)")
        
        # Add holdout plot if available
        if (!is.null(holdout_MBB) && nrow(holdout_df) > 0) {
          df_holdout_vis <- holdout_df %>%
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
              subtitle = paste0("Block length = ", result$L_block, " days, n_sim = ", result$n_sim)
            )
        } else {
          # Only train and test
          p_combined <- (p1 / p2) +
            plot_annotation(
              title = paste0("Prophet + XGBoost: ", enc, " - ", exposure, " - ", cause),
              subtitle = paste0("Block length = ", result$L_block, " days, n_sim = ", result$n_sim)
            )
        }
        
        # Save plot
        plot_file <- paste0(figures_dir, "model_fit_", enc, "_", exposure, "_", cause, ".pdf")
        ggsave(plot_file, p_combined, width = 12, height = 10, dpi = 300)
        
        # ============================================================
        # 3. Calculate Excess Hospitalizations (Holdout period only)
        # ============================================================
        
        if (!is.null(holdout_MBB) && nrow(holdout_df) > 0) {
          cat("  Calculating excess hospitalizations...\n")
          
          # Daily data for holdout period
          df_daily_holdout <- holdout_MBB$pred_summary %>%
            mutate(
              period = as.character(ds),
              observed = y_actual,
              respiratory_pred = yhat
            ) %>%
            rename(conf_lo = conf_lo, conf_hi = conf_hi)
          
          # Total for entire holdout period
          df_period_all_holdout <- holdout_MBB$pred_summary %>%
            summarise(
              observed = sum(y_actual),
              expected = sum(yhat),
              expected_low = sum(conf_lo),
              expected_up = sum(conf_hi),
              period = paste0(
                format(min(ds), "%b %d"), " - ",
                format(max(ds), "%b %d, %Y")
              )
            )
          
          # Calculate excess hospitalizations
          result_daily_holdout <- calc_excess_hosp(
            df_daily_holdout,
            observed = "observed",
            expected = "respiratory_pred",
            expected_conf_lo = "conf_lo",
            expected_conf_hi = "conf_hi"
          )
          
          result_period_all <- calc_excess_hosp(
            df_period_all_holdout,
            observed = "observed",
            expected = "expected",
            expected_conf_lo = "expected_low",
            expected_conf_hi = "expected_up"
          )
          
          # Combine results
          result_combined <- bind_rows(
            result_period_all,
            result_daily_holdout
          ) %>%
            mutate(
              enc_type = enc,
              exposure_category = exposure,
              cause = cause
            )
          
          # Store for aggregation
          all_excess_list[[combo_key]] <- result_combined
          
          # Save individual excess hospitalization table
          excess_csv <- paste0(tables_dir, "excess_hosp_", enc, "_", exposure, "_", cause, ".csv")
          write.csv(result_combined, excess_csv, row.names = FALSE)
        }
        
        cat("  Completed successfully\n\n")
        
      }, error = function(e) {
        cat("  ERROR:", as.character(e), "\n\n")
      })
    }
  }
}

# ============================================================
# Save Combined Outputs
# ============================================================

cat("\n=== Saving Combined Outputs ===\n")

# Save combined metrics
if (length(all_metrics_list) > 0) {
  all_metrics <- bind_rows(all_metrics_list)
  metrics_file <- paste0(tables_dir, "performance_metrics_with_mbb_", mod_ver_suffix, ".csv")
  write.csv(all_metrics, metrics_file, row.names = FALSE)
  cat("Combined metrics saved to:", metrics_file, "\n")
}

# Save combined excess hospitalizations
if (length(all_excess_list) > 0) {
  all_excess <- bind_rows(all_excess_list)
  excess_file <- paste0(tables_dir, "excess_hospitalizations_all_", mod_ver_suffix, ".csv")
  write.csv(all_excess, excess_file, row.names = FALSE)
  cat("Combined excess hospitalizations saved to:", excess_file, "\n")
  
  # Create HTML table for summary (total period only)
  excess_summary <- all_excess %>%
    group_by(enc_type, exposure_category, cause) %>%
    slice(1) %>%  # First row is the total period
    ungroup()
  
  gt_excess <- excess_summary %>%
    select(enc_type, exposure_category, cause, period, observed, expected_CI, excess_CI, excess_pct_CI) %>%
    gt() %>%
    tab_header(
      title = "Excess Hospitalizations Summary with MBB CIs",
      subtitle = "Holdout Period (post Jan 7, 2025)"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    cols_label(
      enc_type = "Encounter Type",
      exposure_category = "Exposure",
      cause = "Outcome",
      period = "Period",
      observed = "Observed",
      expected_CI = "Expected (95% CI)",
      excess_CI = "Excess (95% CI)",
      excess_pct_CI = "Excess % (95% CI)"
    )
  
  excess_html <- paste0(tables_dir, "excess_hospitalizations_summary_", mod_ver_suffix, ".html")
  gtsave(gt_excess, excess_html)
  cat("Excess hospitalizations HTML table saved to:", excess_html, "\n")
}

cat("\n=== Output Generation Complete! ===\n")
cat("\nAll outputs saved to:", latest_dir, "\n")
cat("  - Figures:", figures_dir, "\n")
cat("  - Tables:", tables_dir, "\n")

