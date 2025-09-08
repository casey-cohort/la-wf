#-------------------------------
# LA wildfires project
# author: Arnab Dey and Lara Schwarz, adapted by Lauren Wilner
# date: 2025-09-02
# this code configures, tunes, and fits a Prophet-XGBoost model to the aggregated data 

#-------------------------------
# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script configures, tunes, and fits a Prophet-XGBoost model to the aggregated data
# @date: Dec 16, 2024

# TODO    
# 1. version data stuff 
# 2. add config hyperparams to output csv of metrics for easy vetting
# 3. streamline both scripts and make sure they are doing what we think they are doing. 
  # need to be able to get stable results when we run multiple times.

#-------------------------------
# setup
rm(list = ls())
pacman::p_load(modeltime, tidymodels, tidyverse, timetk, Metrics,
               tictoc, digest, yaml, arrow, future, furrr, progressr)

# set paths 
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/utils.R"))

# determine version number, construct folder name, make folder, set suffix for model version
ver <- gen_ver_number(paste0(path_onedrive, "02_output/"))
folder_name <- paste0("model_run_", Sys.Date(), ".", ver, "/")
dir.create(paste0(path_onedrive, "02_output/", folder_name), showWarnings = FALSE)
mod_ver_suffix <- paste0(Sys.Date(), ".", ver)

# read config 
config <- read_config(paste0(path_repo, "01_code/02_analysis/model_config.yaml"))

# write config
write_config(config, paste0(path_onedrive, "02_output/", folder_name, "model_config_", mod_ver_suffix, ".yaml"))

# ensure consistent numeric precision 
options(digits = 7)
options(scipen = 999)

# set global seed for arg to tuning function
global_seed <- 0112358

#------------------------------
# Set up parallel processing
n_cores <- floor(parallel::detectCores() - 2) # leave 2 cores free
cat("Using", n_cores, "cores out of", parallel::detectCores(), "available\n")
plan(multisession, workers = n_cores)

# Monitor memory usage (this is for macs)
tryCatch({
  mem_info <- system("sysctl hw.memsize", intern = TRUE)
  mem_bytes <- as.numeric(gsub("hw.memsize: ", "", mem_info))
  cat("Available memory:", round(mem_bytes / 1024^3, 1), "GB\n")
}, error = function(e) {
  cat("Memory info not available\n")
})

#------------------------------
# Prepare combinations and estimate runtime
all_combinations <- config$models_to_run_flat %>%
  map_dfr(~data.frame(
    encounter_type = .x$encounter_type,
    exposure_category = .x$exposure_category,
    cause = .x$cause
  ))

cat("Total combinations to process:", nrow(all_combinations), "\n")
cat("Estimated runtime:", round(nrow(all_combinations) * 4 / 60 / n_cores, 1), "hours\n")

#------------------------------
# Process model tuning in batches with progress monitoring
batch_size <- n_cores * 2
n_batches <- ceiling(nrow(all_combinations) / batch_size)
all_combination_results <- list()

progressr::handlers(progressr::handler_progress(
  format = "[:bar] :percent :current/:total ETA: :eta",
  clear = TRUE,
  width = 60
))

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
      result <- run_tuning(combination, config$grid_params, global_seed)
      p()
      result
    }, .options = furrr_options(seed = TRUE))
  })
  
  toc()
  
  all_combination_results <- c(all_combination_results, batch_results)
  
  # Save intermediate results
  save(all_combination_results, 
       file = paste0(path_onedrive, "02_output/", folder_name, "intermediate_results_batch_", batch, "_", mod_ver_suffix, ".RData"))

  cat("Completed", length(all_combination_results), "of", nrow(all_combinations), "combinations\n")
  
  # Memory cleanup every few batches
  if (batch %% 3 == 0) gc()
}

#------------------------------
# Close down parallel processing
plan(sequential)

#------------------------------
# Check for errors and summarize results
errors <- sapply(all_combination_results, function(x) !is.null(x$error) || !isTRUE(x$success))
cat("\nSUMMARY:\n")
cat("Total combinations processed:", length(all_combination_results), "\n")
cat("Successful combinations:", sum(!errors), "\n")
cat("Failed combinations:", sum(errors), "\n")

if (sum(errors) > 0) {
  error_details <- all_combination_results[errors]
  cat("Error details saved for debugging\n")
}

#------------------------------
# Organize results into nested structure
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
# Calculate error metrics for successful models
cat("\n=== Calculating error metrics ===\n")

# Extract only successful results for metrics calculation
successful_results <- all_combination_results[!errors]
metrics_results <- list()

if (length(successful_results) > 0) {
  for (i in seq_along(successful_results)) {
    result <- successful_results[[i]]
    
    # Calculate metrics using the new function
    metrics_result <- calculate_error_metrics(result, global_seed)
    
    if (isTRUE(metrics_result$success)) {
      # Create unique key for this result
      metrics_key <- paste(result$enc_type, result$exposure_category, result$cause, sep = "_")
      metrics_results[[metrics_key]] <- metrics_result$metrics
    } else {
      cat("Failed to calculate metrics for:", result$enc_type, result$exposure_category, result$cause, "\n")
      if (!is.null(metrics_result$error)) {
        cat("Error:", metrics_result$error, "\n")
      }
    }
  }
  
  # Combine all metrics into one dataframe
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
# Save final results
save(all_results, 
     file = paste0(path_onedrive, "02_output/all_results_nested_", mod_ver_suffix, ".RData"))

save(all_combination_results, 
     file = paste0(path_onedrive, "02_output/all_results_with_errors_flat_", mod_ver_suffix, ".RData"))

# Save error metrics if they exist
if (!is.null(all_metrics) && nrow(all_metrics) > 0) {
  write.csv(all_metrics, 
            paste0(path_onedrive, "02_output/performance_metrics_", mod_ver_suffix, ".csv"), 
            row.names = FALSE)
  cat("Performance metrics saved successfully\n")
} else {
  cat("No performance metrics to save\n")
}

cat("\nProcessing complete!\n")
cat("Results saved with timestamp:", mod_ver_suffix, "\n")