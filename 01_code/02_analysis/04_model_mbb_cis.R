#-------------------------------
# LA wildfires project
# Generate Moving Block Bootstrap confidence intervals for tuned models
#-------------------------------

cat("\n========================================\n")
cat("STARTING 04_model_mbb_cis.R\n")
cat("========================================\n\n")

# Setup ----
pacman::p_load(tidymodels, modeltime, tidyverse, timetk, arrow, boot, tictoc)

# Set paths and source utilities
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/utils.R"))
source(paste0(getwd(), "/01_code/utils_mbb.R"))

# Read config to get n_sim_mbb
config <- read_config(paste0(getwd(), "/01_code/02_analysis/model_config.yaml"))
n_sim <- config$n_sim_mbb

# Set MBB parameters
L_block <- 14  # Block length (days)
seed <- 123

cat("MBB Parameters:\n")
cat("  Number of simulations:", n_sim, "\n")
cat("  Block length:", L_block, "days\n\n")

# Find latest model results ----
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

# Check if output directory was set by tuning script (for parallel tests)
output_dir_env <- Sys.getenv("MODEL_OUTPUT_DIR", unset = "")
cat("MODEL_OUTPUT_DIR environment variable:", ifelse(output_dir_env == "", "(not set)", output_dir_env), "\n")

if (output_dir_env != "" && dir.exists(output_dir_env)) {
  latest_dir <- output_dir_env
  cat("Using output directory from MODEL_OUTPUT_DIR environment variable:", latest_dir, "\n")
} else {
  cat("MODEL_OUTPUT_DIR not set or directory doesn't exist, falling back to find_latest_version()\n")
  # Fall back to finding latest directory
  latest_dir <- find_latest_version(paste0(path_onedrive, "02_output/"))
  
  if (is.null(latest_dir)) {
    stop("No model output directories found. Please run 02_model_tune_phxgb_parallel.R first.")
  }
  
  cat("Loading model results from:", latest_dir, "\n")
}

# Find the nested results file
results_files <- list.files(latest_dir, pattern = "all_results_nested_.*\\.RData", full.names = TRUE)
if (length(results_files) == 0) {
  cat("ERROR: No nested results file found in:", latest_dir, "\n")
  cat("Looking for pattern: all_results_nested_*.RData\n")
  cat("Files in directory:\n")
  print(list.files(latest_dir))
  stop("No nested results file found in ", latest_dir)
}
results_file <- results_files[1]
cat("Loading model results from:", results_file, "\n")

# Load results
load(results_file)
cat("Results loaded successfully\n")
cat("Number of encounter types in all_results:", length(all_results), "\n\n")

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
        
        # Rebuild and fit workflow from scratch to avoid XGBoost serialization issues
        cat("  Fitting final model...\n")
        wflw_fit <- rebuild_and_fit_workflow(
          result = result,
          train_df = train_df,
          global_seed = global_seed,
          enc = enc,
          exposure = exposure,
          cause = cause
        )
        cat("  Model fitted successfully\n")
        
        # Extract unfitted recipe and create model spec with best params
        # (needed to avoid XGBoost serialization issues in MBB)
        rec_obj_unfitted <- result$rec_obj_phxgb
        best_params <- result$best_params
        
        # Create model spec with best parameters
        model_seed <- gen_seed(global_seed, c(enc, exposure, cause, "model_mbb"))
        model_spec_mbb <- prophet_boost(
          mode = "regression",
          growth = "linear",
          seasonality_yearly = FALSE,
          mtry = best_params$mtry,
          min_n = best_params$min_n,
          tree_depth = best_params$tree_depth,
          learn_rate = best_params$learn_rate,
          loss_reduction = best_params$loss_reduction,
          stop_iter = best_params$stop_iter
        ) %>%
          set_engine("prophet_xgboost",
                     seed = model_seed,
                     early_stop = TRUE,
                     validation = 0.2)
        
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
          seed = seed,
          rec_obj_unfitted = rec_obj_unfitted,
          model_spec = model_spec_mbb
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
            seed = seed,
            rec_obj_unfitted = rec_obj_unfitted,
            model_spec = model_spec_mbb
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
        cat("  ERROR processing", enc, "-", exposure, "-", cause, ":\n")
        cat("    ", as.character(e), "\n")
        cat("    Traceback:\n")
        print(traceback())
        cat("\n")
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

cat("Preparing to save MBB results:\n")
cat("  Output directory:", latest_dir, "\n")
cat("  Output file:", output_file, "\n")
cat("  Number of encounter types:", length(mbb_results), "\n")

# Verify directory exists
if (!dir.exists(latest_dir)) {
  stop("Output directory does not exist:", latest_dir)
}

# Save with error handling
tryCatch({
  saveRDS(mbb_results, output_file)
  if (file.exists(output_file)) {
    file_size <- file.info(output_file)$size
    cat("MBB results saved successfully!\n")
    cat("  File:", output_file, "\n")
    cat("  Size:", round(file_size / 1024 / 1024, 2), "MB\n")
  } else {
    stop("File was not created after saveRDS()")
  }
}, error = function(e) {
  cat("ERROR saving MBB results:", as.character(e), "\n")
  print(traceback())
  stop("Failed to save MBB results")
})

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
