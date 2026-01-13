#-------------------------------
# LA wildfires project
# author: Arnab Dey and Lara Schwarz, adapted by Lauren Wilner
# date: 2025-09-02
# this code configures, tunes, and fits a Prophet-XGBoost model to the aggregated data 

#-------------------------------
# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey, Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script configures, tunes, and fits a Prophet-XGBoost model to the aggregated data
# @date: Dec 16, 2024

#-------------------------------
# setup
pacman::p_load(modeltime, tidymodels, tidyverse, timetk, Metrics,
               tictoc, digest, yaml, arrow, future, furrr, progressr, withr)

# set paths 
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_general.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_tuning.R"))

# read config (TEST_CONFIG_PATH env var will override if set for parallel testing)
config <- read_config(paste0(path_repo, "01_code/02_analysis/model_config.yaml"))

# count number of models to run
n_models <- length(config$models_to_run_flat)
n_sim_mbb <- config$n_sim_mbb

# determine version number, construct folder name, make folder, set suffix for model version
# new format: model_run_YYYY-MM-DD.v###_x##_sim###
models_path <- get_models_path(path_onedrive, user = config$user)
ver <- gen_ver_number(models_path)
folder_name <- paste0("model_run_", Sys.Date(), ".", ver, "_x", n_models, "_sim", n_sim_mbb, "/")
dir.create(paste0(models_path, folder_name), showWarnings = FALSE, recursive = TRUE)
mod_ver_suffix <- paste0(Sys.Date(), ".", ver, "_x", n_models, "_sim", n_sim_mbb)

# write this ver of config back out
write_config(config, paste0(models_path, folder_name, "model_config_", mod_ver_suffix, ".yaml"))

# ensure consistent numeric precision 
options(digits = 7)
options(scipen = 999)

# set global seed for arg to tuning function
global_seed <- config$seed

# train test data to use -- datasets are in dated folders
# Check if TRAIN_TEST_DATE env var is set (from run_pipeline), otherwise prompt user
train_test_date_env <- Sys.getenv("TRAIN_TEST_DATE", unset = "")
if (train_test_date_env != "") {
  train_test_path <- get_train_test_data_path(path_onedrive, prompt_user = FALSE, date = train_test_date_env)
} else {
  train_test_path <- get_train_test_data_path(path_onedrive, prompt_user = TRUE)
}

# Store train/test date in environment variable for use by subsequent pipeline steps
train_test_date <- basename(train_test_path)
Sys.setenv(TRAIN_TEST_DATE = train_test_date)

#------------------------------
# Set up parallel processing
n_cores <- setup_parallel_processing(config)

#------------------------------
# prepare combinations and estimate runtime
all_combinations <- config$models_to_run_flat %>%
  map_dfr(~data.frame(
    encounter_type = .x$encounter_type,
    exposure_category = .x$exposure_category,
    cause = .x$cause
  ))

cat("Total combinations to process:", nrow(all_combinations), "\n")

#------------------------------
# process model tuning in batches with progress monitoring
batch_size <- n_cores * 2
n_batches <- ceiling(nrow(all_combinations) / batch_size)
all_combination_results <- list()

progressr::handlers(progressr::handler_progress(
  format = "[:bar] :percent :current/:total ETA: :eta",
  clear = TRUE,
  width = 60
))

#------------------------------
# Create subdirectories for organized output
results_dir <- paste0(models_path, folder_name, "results/")
performance_metrics_dir <- paste0(models_path, folder_name, "performance_metrics/")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(performance_metrics_dir, recursive = TRUE, showWarnings = FALSE)

for (batch in 1:n_batches) {
  cat("\n=== Processing batch", batch, "of", n_batches, "===\n")
  
  start_idx <- (batch - 1) * batch_size + 1
  end_idx <- min(batch * batch_size, nrow(all_combinations))
  batch_data <- all_combinations[start_idx:end_idx, ]
  
  tic()
  with_progress({
    p <- progressor(steps = nrow(batch_data))
    batch_results <- batch_data %>%
      split(1:nrow(.)) %>%
      future_map(~{
        combination <- list(
          encounter_type = .x$encounter_type,
          exposure_category = .x$exposure_category,
          cause = .x$cause
        )
      result <- run_tuning(combination,
                config$grid_params,
                config$train_test_params,
                global_seed,
                train_test_path,
                config$grid_size)
      p()
      result
    }, .options = furrr_options(seed = TRUE))
  })
  
  toc()
  
  all_combination_results <- c(all_combination_results, batch_results)
  
  # save intermediate results in results/ subdirectory
  save(all_combination_results, 
       file = paste0(results_dir, "intermediate_results_batch_", batch, "_", mod_ver_suffix, ".RData"))

  cat("Completed", length(all_combination_results), "of", nrow(all_combinations), "combinations\n")
  
  # memory cleanup every few batches
  if (batch %% 3 == 0) gc()
}

#------------------------------
# close down parallel processing
plan(sequential)

#------------------------------
# check for errors and summarize results
errors <- sapply(all_combination_results, function(x) !is.null(x$error) || !isTRUE(x$success))
cat("\nSUMMARY:\n")
cat("Total combinations processed:", length(all_combination_results), "\n")
cat("Successful combinations:", sum(!errors), "\n")
cat("Failed combinations:", sum(errors), "\n")

if (sum(errors) > 0) {
  error_details <- all_combination_results[errors]
  cat("Error details saved for debugging\n")
  
  # Print first few error details to help debug
  cat("\n=== ERROR DETAILS ===\n")
  for (i in seq_along(error_details)) {
    error <- error_details[[i]]
    cat("Error", i, ":", error$enc_type, error$exposure_category, error$cause, "\n")
    cat("Message:", error$error, "\n")
    cat("---\n")
  }
}

#------------------------------
# organize results into nested structure
all_results <- list()
for (i in seq_along(all_combination_results)) {
  result <- all_combination_results[[i]]
  if (isTRUE(result$success)) {
    enc <- result$enc_type
    exposure <- result$exposure_category
    cause <- result$cause
    
    if (!enc %in% names(all_results)) all_results[[enc]] <- list()
    if (!exposure %in% names(all_results[[enc]])) all_results[[enc]][[exposure]] <- list()
    
    all_results[[enc]][[exposure]][[cause]] <- result
  }
}

#------------------------------
# performance metrics for successful models
cat("\n=== Calculating error metrics ===\n")

# filter to only successful results for metrics calculation
successful_results <- all_combination_results[!errors]
metrics_results <- list()

if (length(successful_results) > 0) {
  for (i in seq_along(successful_results)) {
    result <- successful_results[[i]]
    
    # calc metrics using the new function
    metrics_result <- calculate_error_metrics(result, global_seed, config)
    
    if (isTRUE(metrics_result$success)) {
      # create unique key for this result
      metrics_key <- paste(result$enc_type, result$exposure_category, result$cause, sep = "_")
      metrics_results[[metrics_key]] <- metrics_result$metrics
    } else {
      cat("Failed to calculate metrics for:", result$enc_type, result$exposure_category, result$cause, "\n")
      if (!is.null(metrics_result$error)) {
        cat("Error:", metrics_result$error, "\n")
      }
    }
  }
  
  # concat all metrics into one dataframe
  if (length(metrics_results) > 0) {
    all_metrics <- bind_rows(metrics_results)
    cat("Successfully calculated metrics for", nrow(all_metrics), "model-dataset combinations\n")
  } else {
    cat("No successful metrics calculations\n")
    all_metrics <- NULL
  }
} else {
  cat("No successful model results to calculate metrics for\n")
  all_metrics <- NULL
}

#------------------------------
# extract best parameters from successful model results
best_params_df <- list()

for (i in seq_along(successful_results)) {
  result <- successful_results[[i]]
  if (!is.null(result$best_params)) {
      # Only select columns that exist (parameters that were tuned)
      param_cols <- c("mtry", "trees", "min_n", "tree_depth", "learn_rate", "loss_reduction", 
                     "stop_iter", "sample_size", "changepoint_num", "changepoint_range", 
                     "prior_scale_changepoints")
      existing_cols <- intersect(param_cols, names(result$best_params))
      
      best_params_row <- result$best_params %>%
      select(any_of(existing_cols)) %>%
      mutate(
        enc_type = result$enc_type,
        exposure_category = result$exposure_category,
        cause = result$cause
      )
    best_params_df[[i]] <- best_params_row
  }
}

# combine all best params into one dataframe
if (length(best_params_df) > 0) {
  all_best_params <- bind_rows(best_params_df)
} else {
  all_best_params <- NULL
}

#------------------------------
# add config hyperparams to output csv of metrics for easy vetting
config_expanded <- config$models_to_run_flat %>%
      map_dfr(~data.frame(
        enc_type = .x$encounter_type,
        exposure_category = .x$exposure_category,
        cause = .x$cause
      ))

config_expanded$mtry_range <- paste0("[", config$grid_params$mtry$range[1], ", ", config$grid_params$mtry$range[2], "]")
config_expanded$trees_range <- paste0("[", config$grid_params$trees$range[1], ", ", config$grid_params$trees$range[2], "]")
config_expanded$min_n_range <- paste0("[", config$grid_params$min_n$range[1], ", ", config$grid_params$min_n$range[2], "]")
config_expanded$tree_depth_range <- paste0("[", config$grid_params$tree_depth$range[1], ", ", config$grid_params$tree_depth$range[2], "]")
config_expanded$learn_rate_range <- paste0("[", config$grid_params$learn_rate$range[1], ", ", config$grid_params$learn_rate$range[2], "]")
config_expanded$loss_reduction_range <- paste0("[", config$grid_params$loss_reduction$range[1], ", ", config$grid_params$loss_reduction$range[2], "]")
config_expanded$stop_iter_range <- paste0("[", config$grid_params$stop_iter$range[1], ", ", config$grid_params$stop_iter$range[2], "]")
config_expanded$sample_size_range <- paste0("[", config$grid_params$sample_size$range[1], ", ", config$grid_params$sample_size$range[2], "]")
config_expanded$changepoint_num_range <- paste0("[", config$grid_params$changepoint_num$range[1], ", ", config$grid_params$changepoint_num$range[2], "]")
config_expanded$changepoint_range_range <- paste0("[", config$grid_params$changepoint_range$range[1], ", ", config$grid_params$changepoint_range$range[2], "]")
config_expanded$prior_scale_changepoints_range <- paste0("[", config$grid_params$prior_scale_changepoints$range[1], ", ", config$grid_params$prior_scale_changepoints$range[2], "]")
config_expanded$assess_split <- config$train_test_params$assess_split
config_expanded$assess_cv <- config$train_test_params$assess_cv
config_expanded$skip_cv <- config$train_test_params$skip_cv
config_expanded$slice_limit_cv <- config$train_test_params$slice_limit_cv
config_expanded$model_description <- config$model_description

if (!is.null(all_metrics) && nrow(all_metrics) > 0) {
  # join with config params first
  all_metrics <- all_metrics %>%
    left_join(config_expanded, by = c("enc_type", "exposure_category", "cause"))
  
  # then join with best params if they exist
  if (!is.null(all_best_params) && nrow(all_best_params) > 0) {
    all_metrics <- all_metrics %>%
      left_join(all_best_params, by = c("enc_type", "exposure_category", "cause"))
  }
}

#------------------------------
# lastly, order columns
if (!is.null(all_metrics) && nrow(all_metrics) > 0) {

  # and order cols - use any_of() to handle missing columns gracefully
  all_metrics <- all_metrics %>%
    select(enc_type, exposure_category, cause, model_description, 
           # XGBoost hyperparameter tuning ranges and their corresponding best values
           any_of(c("mtry_range", "mtry",
                    "trees_range", "trees",
                    "min_n_range", "min_n",
                    "tree_depth_range", "tree_depth",
                    "learn_rate_range", "learn_rate",
                    "loss_reduction_range", "loss_reduction",
                    "stop_iter_range", "stop_iter",
                    "sample_size_range", "sample_size",
                    # Prophet changepoint parameters
                    "changepoint_num_range", "changepoint_num",
                    "changepoint_range_range", "changepoint_range",
                    "prior_scale_changepoints_range", "prior_scale_changepoints")),
           # assess/skip parameters
           assess_split, assess_cv, skip_cv, slice_limit_cv, 
           everything())
}

#------------------------------
# save final results in results/ subdirectory
save(all_results, 
     file = paste0(results_dir, "all_results_nested_", mod_ver_suffix, ".RData"))

save(all_combination_results, 
     file = paste0(results_dir, "all_results_with_errors_flat_", mod_ver_suffix, ".RData"))

# save performance metrics in performance_metrics/ subdirectory
if (!is.null(all_metrics) && nrow(all_metrics) > 0) {
  write.csv(all_metrics, 
            paste0(performance_metrics_dir, "performance_metrics_", mod_ver_suffix, ".csv"), 
            row.names = FALSE)
  cat("Performance metrics saved successfully\n")
} else {
  cat("No performance metrics to save\n")
}

# Save output directory path for subsequent scripts (MBB, outputs)
output_dir_full <- paste0(models_path, folder_name)
Sys.setenv(MODEL_OUTPUT_DIR = output_dir_full)

cat("\nProcessing complete!\n")
cat("Results saved with timestamp:", mod_ver_suffix, "\n")
cat("Output directory:", output_dir_full, "\n")
