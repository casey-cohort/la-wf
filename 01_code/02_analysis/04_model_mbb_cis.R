#-------------------------------
# LA wildfires project
# Generate Moving Block Bootstrap confidence intervals for tuned models
#-------------------------------

# Setup ----
pacman::p_load(tidymodels, modeltime, tidyverse, timetk, arrow, boot, tictoc)

# Set paths and source utilities
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/utils.R"))
source(paste0(getwd(), "/01_code/utils_mbb.R"))

# Check for test mode
test_mode <- exists("TEST_MODE") && TEST_MODE
if (test_mode) {
  cat("*** RUNNING IN TEST MODE - Using 100 MBB simulations ***\n")
  n_sim <- 100
} else {
  cat("Running in production mode - Using 1000 MBB simulations\n")
  n_sim <- 1000
}

# Set MBB parameters
L_block <- 14  # Block length (days)
seed <- 123

cat("MBB Parameters:\n")
cat("  Number of simulations:", n_sim, "\n")
cat("  Block length:", L_block, "days\n\n")

# Find latest model results ----
# Use RUN_MODE if available (set by 00_run_all.R), otherwise default to "prod"
run_mode <- if(exists("RUN_MODE")) RUN_MODE else "prod"

# Use mode-aware function to find latest directory (defined in utils.R)
find_latest_version <- function(output_path, mode = "prod") {
  output_dirs <- list.dirs(output_path, full.names = TRUE, recursive = FALSE)
  # Filter by mode (test or prod)
  pattern <- paste0("model_run_", mode, "_")
  output_dirs <- output_dirs[grepl(pattern, basename(output_dirs))]
  if (length(output_dirs) == 0) {
    return(NULL)
  }
  latest_dir <- output_dirs[order(basename(output_dirs), decreasing = TRUE)][1]
  return(latest_dir)
}

latest_dir <- find_latest_version(paste0(path_onedrive, "02_output/"), mode = run_mode)

if (is.null(latest_dir)) {
  stop(paste0("No model output directories found for mode: ", run_mode, ". Please run 02_model_tune_phxgb_parallel.R first."))
}

cat("Loading model results from:", latest_dir, "\n")
cat("Run mode:", run_mode, "\n")

# Find the nested results file
results_files <- list.files(latest_dir, pattern = "all_results_nested_.*\\.RData", full.names = TRUE)
if (length(results_files) == 0) {
  stop("No nested results file found in ", latest_dir)
}
results_file <- results_files[1]
cat("Loading:", results_file, "\n\n")

load(results_file)

# Load train/test data ----
train_test_date <- max(list.dirs(paste0(path_onedrive, "01_data/02_processed/train_test/"), 
                                  full.names = FALSE, recursive = FALSE))
train_test_path <- paste0(path_onedrive, "01_data/02_processed/train_test/", train_test_date, "/")

cat("Loading train/test data from:", train_test_path, "\n\n")

# Global seed for reproducibility
global_seed <- 0112358

# Initialize results structure ----
mbb_results <- list()

# Process each model combination ----
cat("=== Generating MBB Confidence Intervals ===\n\n")

cat("Total encounter types:", length(names(all_results)), "\n")
cat("Encounter types:", paste(names(all_results), collapse = ", "), "\n\n")

for (enc in names(all_results)) {
  cat("\n=== Processing encounter type:", enc, "===\n")
  mbb_results[[enc]] <- list()
  
  cat("  Exposure categories:", paste(names(all_results[[enc]]), collapse = ", "), "\n")
  
  for (exposure in names(all_results[[enc]])) {
    cat("\n  === Processing exposure:", exposure, "===\n")
    mbb_results[[enc]][[exposure]] <- list()
    
    cat("    Causes:", paste(names(all_results[[enc]][[exposure]]), collapse = ", "), "\n")
    cat("    Number of causes:", length(names(all_results[[enc]][[exposure]])), "\n")
    
    for (cause in names(all_results[[enc]][[exposure]])) {
      cat("\n    Processing:", enc, "-", exposure, "-", cause, "\n")
      
      result <- all_results[[enc]][[exposure]][[cause]]
      
      # Skip if not successful
      if (!isTRUE(result$success)) {
        cat("  Skipping - model was not successful\n")
        next
      }
      
      tryCatch({
        # Load data for this combination
        # Use df-predict-sf.parquet which includes holdout period (post Jan 6, 2025)
        # df-train-test_sf.parquet excludes holdout period
        df_full <- arrow::open_dataset(
          paste0(train_test_path, "df-predict-sf.parquet")
        ) %>%
          dplyr::filter(
            exposure_category == !!exposure & enc_type == !!enc
          ) %>%
          dplyr::collect() %>%
          dplyr::mutate(date = as.Date(date))
        
        # Create train/test/holdout splits
        # Holdout: dates after 2025-01-06
        df_holdout <- df_full %>% 
          dplyr::filter(date > as.Date("2025-01-06"))
        
        df_train_test <- df_full %>%
          dplyr::filter(date <= as.Date("2025-01-06"))
        
        # Split train/test using same parameters as in tuning
        splits <- df_train_test %>%
          timetk::time_series_split(
            assess = "75 days",
            cumulative = TRUE,
            date_var = date
          )
        
        train_df <- rsample::training(splits)
        test_df <- rsample::testing(splits)
        
        # Refit final model with best parameters
        wflw_phxgb_tune <- result$wflw_phxgb_tune
        tune_results_phxgb <- result$tune_results_phxgb
        
        wflw_fit_seed <- gen_seed(global_seed, c(enc, exposure, cause, "wflw_fit_mbb"))
        set.seed(wflw_fit_seed)
        
        wflw_fit <- suppressWarnings({
          suppressMessages({
            wflw_phxgb_tune |>
              finalize_workflow(select_best(tune_results_phxgb, metric = "rmse")) |>
              fit(train_df)
          })
        })
        
        cat("  Model fitted successfully\n")
        
        # Generate predictions for training set (no bootstrap)
        cat("  Generating training predictions...\n")
        train_summary <- generate_simple_predictions(
          wflw_fit = wflw_fit,
          data_df = train_df,
          outcome_col = cause
        )
        
        # Generate MBB CIs for test set
        cat("  Generating test MBB CIs...\n")
        tic()
        test_MBB <- generate_MBB_CIs_tidymodels(
          wflw_fit = wflw_fit,
          train_df = train_df,
          target_df = test_df,
          outcome_col = cause,
          n_sim = n_sim,
          L_block = L_block,
          seed = seed
        )
        toc()
        
        # Generate MBB CIs for holdout set
        if (nrow(df_holdout) > 0) {
          cat("  Generating holdout MBB CIs...\n")
          tic()
          holdout_MBB <- generate_MBB_CIs_tidymodels(
            wflw_fit = wflw_fit,
            train_df = train_df,
            target_df = df_holdout,
            outcome_col = cause,
            n_sim = n_sim,
            L_block = L_block,
            seed = seed
          )
          toc()
        } else {
          cat("  No holdout data available\n")
          holdout_MBB <- NULL
        }
        
        # Store results
        mbb_results[[enc]][[exposure]][[cause]] <- list(
          train_summary = train_summary,
          test_MBB = test_MBB,
          holdout_MBB = holdout_MBB,
          n_sim = n_sim,
          L_block = L_block,
          enc_type = enc,
          exposure_category = exposure,
          cause = cause,
          wflw_fit = wflw_fit,  # Save fitted workflow for outputs script
          train_df = train_df,
          test_df = test_df,
          holdout_df = df_holdout,
          success = TRUE
        )
        
        cat("  Completed successfully\n\n")
        
      }, error = function(e) {
        cat("  ERROR:", as.character(e), "\n\n")
        mbb_results[[enc]][[exposure]][[cause]] <- list(
          error = as.character(e),
          enc_type = enc,
          exposure_category = exposure,
          cause = cause,
          success = FALSE
        )
      })
    }
  }
}

# Save results ----
cat("\n=== Saving MBB results ===\n")

# Extract timestamp from latest_dir
mod_ver_suffix <- sub("model_run_", "", basename(latest_dir))
output_file <- paste0(latest_dir, "/mbb_results_nested_", mod_ver_suffix, ".rds")

saveRDS(mbb_results, output_file)
cat("MBB results saved to:", output_file, "\n")

# Summary
successful <- 0
failed <- 0
for (enc in names(mbb_results)) {
  for (exposure in names(mbb_results[[enc]])) {
    for (cause in names(mbb_results[[enc]][[exposure]])) {
      if (isTRUE(mbb_results[[enc]][[exposure]][[cause]]$success)) {
        successful <- successful + 1
      } else {
        failed <- failed + 1
      }
    }
  }
}

cat("\nSummary:\n")
cat("  Successful MBB calculations:", successful, "\n")
cat("  Failed calculations:", failed, "\n")
cat("\nMBB confidence interval generation complete!\n")
