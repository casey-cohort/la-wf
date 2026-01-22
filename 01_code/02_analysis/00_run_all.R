#-------------------------------
# LA wildfires project
# Script to run complete Prophet + XGBoost analysis with MBB CIs
# 
# NOTE: Data preparation must be run separately before this pipeline.
#       Run 01_data_prep.R first to prepare training/testing data.
#
# Usage Examples:
#   
#   1. Default usage (uses model_config.yaml):
#      source("01_code/02_analysis/00_run_all.R")
#      run_pipeline()
#   
#   2. Custom config file:
#      source("01_code/02_analysis/00_run_all.R")
#      path <- "/Users/laurenwilner/Library/CloudStorage/OneDrive-SharedLibraries-UW/casey_cohort - Documents/studies/la_wf_pm_evac_its/03_modeling-and-results/02_best-model-selection/model_configs_for_rerun/"
#      file <- "ED_evac_rate_enc_injury_akd_model_run_2026-01-02.v006_x20_sim500_config.yaml"
#      run_pipeline(config_path = paste0(path, file))
#   
#   3. Run bested model (looks up config from bested folder):
#      source("01_code/02_analysis/00_run_all.R")
#      run_pipeline(bested_model = list(encounter_type = "ED", 
#                                        exposure_category = "high_smoke", 
#                                        cause = "rate_enc"))
#-------------------------------

# Load timing library
pacman::p_load(tictoc)

source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_general.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_tuning.R"))

# Helper function for step timing (returns timing info)
run_timed_step <- function(step_num, step_name, script_path, step_times) {
  cat("========================================\n")
  cat("STEP", step_num, ":", step_name, "\n")
  cat("========================================\n")
  
  step_start_time <- Sys.time()
  cat("Step", step_num, "started at:", format(step_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Source the main script
  source(script_path)
  
  step_end_time <- Sys.time()
  step_duration <- step_end_time - step_start_time
  
  # Store timing info
  step_times[[paste0("step", step_num)]] <- list(
    name = step_name,
    start_time = step_start_time,
    end_time = step_end_time,
    duration = step_duration,
    executed = TRUE
  )
  
  cat("\n", step_name, "complete! (", round(as.numeric(step_duration, units = "mins"), 1), "min)\n\n")
  
  return(step_times)
}

#' Run complete Prophet + XGBoost analysis pipeline
#'
#' Runs the complete analysis pipeline including model tuning, MBB confidence intervals,
#' and output generation. Can use a config file directly or look up configs from the bested folder.
#'
#' @param config_path Optional path to a specific config YAML file. If NULL, uses default model_config.yaml
#' @param bested_model Optional model specification list with encounter_type, exposure_category, and cause.
#'                     If provided, looks up config from bested folder.
#' @param user User name (lbw or akd). Determines where outputs are saved.
#'             If NULL, uses value from config file.
#' @param n_sim_mbb Optional number of MBB simulations to override config default
#' @param train_test_date Optional date string (YYYY-MM-DD) to avoid prompting multiple times
#' @param ci_method Optional CI method override. Options: "quantile" or "symmetric_sd".
#'                  If NULL, uses value from config file.
#' @param ensure_nonnegative Optional override for ensure_nonnegative setting.
#'                           If NULL, uses value from config file.
#' @param model_path Optional path to models directory. If NULL, computed from config$user.
#'                   When called from run_batch_bested(), uses the user parameter to compute this.
#'
#' @return Invisibly returns NULL. Results are saved to output directory.
#'
#' @examples
#' # Default usage (uses model_config.yaml)
#' run_pipeline()
#'
#' # Custom config file
#' run_pipeline(config_path = "/path/to/config.yaml")
#'
#' # Run bested model with overrides
#' run_pipeline(bested_model = list(encounter_type = "ED", 
#'                                   exposure_category = "high_smoke", 
#'                                   cause = "rate_enc"),
#'              n_sim_mbb = 500, ci_method = "symmetric_sd", ensure_nonnegative = TRUE)
#'
run_pipeline <- function(config_path = NULL, bested_model = NULL, user = NULL, n_sim_mbb = NULL, train_test_date = NULL, ci_method = NULL, ensure_nonnegative = NULL, model_path = NULL) {
  # Initialize timing variables
  pipeline_start_time <- Sys.time()
  step_times <- list()
  step_names <- c("Model Tuning", "MBB Confidence Intervals", "Generate Outputs")
  
  cat("========================================\n")
  cat("Prophet + XGBoost Analysis Pipeline\n")
  cat("with Moving Block Bootstrap CIs\n")
  cat("========================================\n\n")
  
  cat("Pipeline started at:", format(pipeline_start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Handle config path - can be provided directly or looked up from bested folder
  if (!is.null(bested_model)) {
    # Look up config from bested folder
    if (is.null(user)) {
      # Get user from config
      default_config <- read_config(paste0(getwd(), "/01_code/02_analysis/model_config.yaml"))
      user <- default_config$user
    }
    
    models_dir <- paste0(path_onedrive, "03_modeling-and-results/01_modeling/")
    bested_dir <- paste0(path_onedrive, "03_modeling-and-results/02_best-model-selection/model_configs_for_rerun/")
    
    # Check if bested directory exists
    if (!dir.exists(bested_dir)) {
      stop("Bested directory does not exist: ", bested_dir, 
           "\nPlease run 06_model_compare.R first to generate bested configs.")
    }
    
    # Find matching config file
    # Pattern must match exactly: ED_high_smoke_rate_enc_*_config.yaml
    # Use word boundary or ensure exact match to avoid matching rate_enc_cardio when looking for rate_enc
    # Escape special regex characters in cause name
    cause_escaped <- gsub("([.|()\\^{}+$*?\\[\\\\])", "\\\\\\1", bested_model$cause)
    pattern <- paste0("^", bested_model$encounter_type, "_", 
                      bested_model$exposure_category, "_", 
                      cause_escaped, "_(akd|lbw)_model_run_.*_config\\.yaml$")
    config_files <- list.files(bested_dir, pattern = pattern, full.names = TRUE)
    
    if (length(config_files) == 0) {
      # Provide helpful error message
      all_configs <- list.files(bested_dir, pattern = "_config\\.yaml$", full.names = FALSE)
      stop("No bested config found for: ", bested_model$encounter_type, " - ", 
           bested_model$exposure_category, " - ", bested_model$cause,
           "\nLooking in: ", bested_dir,
           "\nPattern: ", pattern,
           if(length(all_configs) > 0) paste0("\nAvailable configs (first 10): ", paste(head(all_configs, 10), collapse = ", ")) else "\nNo config files found in bested directory.")
    }
    if (length(config_files) > 1) {
      stop("Multiple bested configs found for: ", bested_model$encounter_type, " - ", 
           bested_model$exposure_category, " - ", bested_model$cause,
           "\nFound files:\n  ", paste(basename(config_files), collapse = "\n  "))
    }
    
    config_path <- config_files[1]
    cat("Found bested config:", basename(config_path), "\n")
  }
  
  # Handle config path override and parameter modifications
  if (!is.null(config_path)) {
    # Check if any overrides are requested
    has_overrides <- !is.null(n_sim_mbb) || !is.null(ci_method) || !is.null(ensure_nonnegative)
    
    if (has_overrides) {
      # Read config and apply overrides
      config <- yaml::read_yaml(config_path)
      
      if (!is.null(n_sim_mbb)) {
        config$n_sim_mbb <- n_sim_mbb
        cat("Modified n_sim_mbb to", n_sim_mbb, "\n")
      }
      
      if (!is.null(ci_method)) {
        # Ensure ci_params exists
        if (is.null(config$ci_params)) {
          config$ci_params <- list()
        }
        config$ci_params$method <- ci_method
        cat("Modified ci_method to", ci_method, "\n")
        
        # Ensure ci_level has a default if not present in bested config
        if (is.null(config$ci_params$level)) {
          config$ci_params$level <- 0.95
          cat("Set ci_level to default 0.95\n")
        }
      }
      
      if (!is.null(ensure_nonnegative)) {
        # Ensure ci_params exists
        if (is.null(config$ci_params)) {
          config$ci_params <- list()
        }
        config$ci_params$ensure_nonnegative <- ensure_nonnegative
        cat("Modified ensure_nonnegative to", ensure_nonnegative, "\n")
      }
      
      # Write modified config to temp file
      temp_config <- tempfile(fileext = ".yaml")
      yaml::write_yaml(config, temp_config)
      config_path <- temp_config
      # Clean up temp file on exit
      on.exit(unlink(temp_config), add = TRUE)
    }
    
    # Set TEST_CONFIG_PATH for all scripts to use
    Sys.setenv(TEST_CONFIG_PATH = config_path)
    cat("Using config file:", ifelse(has_overrides, basename(config_path), config_path), "\n\n")
    # Ensure cleanup on exit (even if pipeline fails)
    on.exit(Sys.unsetenv("TEST_CONFIG_PATH"), add = TRUE)
  }
  
  # Ensure parallel resources are cleaned up on exit (even if pipeline fails)
  on.exit(future::plan(future::sequential), add = TRUE)
  
  # Set train_test_date in environment if provided (to avoid prompting multiple times in batch runs)
  if (!is.null(train_test_date)) {
    Sys.setenv(TRAIN_TEST_DATE = train_test_date)
    cat("Using train/test data from:", train_test_date, "\n")
    on.exit(Sys.unsetenv("TRAIN_TEST_DATE"), add = TRUE)
  }
  
  # Set model_path in environment if provided (allows run_batch_bested to override config$user)
  if (!is.null(model_path)) {
    Sys.setenv(MODEL_PATH = model_path)
    cat("Using model path:", model_path, "\n")
    on.exit(Sys.unsetenv("MODEL_PATH"), add = TRUE)
  }
  
  cat("Starting analysis pipeline...\n\n")
  
  # ============================================================================
  # Step 1: Model Tuning
  # ============================================================================
  
  step_times <- run_timed_step(1, step_names[1], paste0(getwd(), "/01_code/02_analysis/02_model_tune_phxgb_parallel.R"), step_times)
  
  # ============================================================================
  # Step 2: Generate MBB Confidence Intervals
  # ============================================================================
  
  step_times <- run_timed_step(2, step_names[2], paste0(getwd(), "/01_code/02_analysis/04_model_mbb_cis.R"), step_times)
  
  # ============================================================================
  # Step 3: Generate Outputs
  # ============================================================================
  
  step_times <- run_timed_step(3, step_names[3], paste0(getwd(), "/01_code/02_analysis/05_model_outputs.R"), step_times)
  
  # ============================================================================
  # Summary
  # ============================================================================
  
  # Calculate overall pipeline timing
  pipeline_end_time <- Sys.time()
  pipeline_total_duration <- pipeline_end_time - pipeline_start_time
  
  cat("\n========================================\n")
  cat("PIPELINE COMPLETE!\n")
  cat("========================================\n")
  cat("Started:", format(pipeline_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Completed:", format(pipeline_end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Calculate step timing summary
  total_executed_time <- 0
  executed_steps <- 0
  
  for (i in 1:3) {
    step_key <- paste0("step", i)
    if (step_key %in% names(step_times)) {
      step_info <- step_times[[step_key]]
      total_executed_time <- total_executed_time + as.numeric(step_info$duration, units = "mins")
      executed_steps <- executed_steps + 1
    }
  }
  
  cat("Timing Summary:\n")
  cat("  Total time:", round(total_executed_time, 1), "minutes (", round(total_executed_time / 60, 2), "hours)\n")
  cat("  Steps completed:", executed_steps, "of 3\n\n")
  
  # Find output directory (using new naming pattern)
  # Read config to get user
  config <- read_config(paste0(path_repo, "01_code/02_analysis/model_config.yaml"))
  models_path <- get_models_path(path_onedrive, user = config$user)
  latest_dir <- find_latest_version(models_path)
  
  cat("Results saved to:", latest_dir, "\n")
  cat("Analysis complete!\n")
}
