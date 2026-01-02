#-------------------------------
# LA wildfires project
# Master script to run complete Prophet + XGBoost analysis with MBB CIs
# 
# NOTE: Data preparation must be run separately before this pipeline.
#       Run 01_data_prep.R first to prepare training/testing data.
#-------------------------------

cat("========================================\n")
cat("Prophet + XGBoost Analysis Pipeline\n")
cat("with Moving Block Bootstrap CIs\n")
cat("========================================\n\n")

# Load timing library
pacman::p_load(tictoc)

# Initialize timing variables
pipeline_start_time <- Sys.time()
step_times <- list()
step_names <- c("Model Tuning", "MBB Confidence Intervals", "Generate Outputs")

cat("Pipeline started at:", format(pipeline_start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")

# Helper function for step timing
run_timed_step <- function(step_num, step_name, script_path, ...) {
  cat("========================================\n")
  cat("STEP", step_num, ":", step_name, "\n")
  cat("========================================\n")
  
  step_start_time <- Sys.time()
  cat("Step", step_num, "started at:", format(step_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Run any additional setup code if provided
  if (length(list(...)) > 0) {
    for (setup_code in list(...)) {
      eval(setup_code)
    }
  }
  
  # Source the main script
  source(script_path)
  
  step_end_time <- Sys.time()
  step_duration <- step_end_time - step_start_time
  
  # Store timing info
  step_times[[paste0("step", step_num)]] <<- list(
    name = step_name,
    start_time = step_start_time,
    end_time = step_end_time,
    duration = step_duration,
    executed = TRUE
  )
  
  cat("\n", step_name, "complete!\n")
  cat("Step", step_num, "duration:", round(as.numeric(step_duration, units = "mins"), 2), "minutes\n\n")
}

# Setup ----
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_general.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_tuning.R"))

# Ensure parallel resources are cleaned up on exit (even if pipeline fails)
on.exit(future::plan(future::sequential), add = TRUE)

cat("Starting analysis pipeline...\n\n")

# ============================================================================
# Step 1: Model Tuning
# ============================================================================

run_timed_step(1, step_names[1], paste0(getwd(), "/01_code/02_analysis/02_model_tune_phxgb_parallel.R"))

# ============================================================================
# Step 2: Generate MBB Confidence Intervals
# ============================================================================

run_timed_step(2, step_names[2], paste0(getwd(), "/01_code/02_analysis/04_model_mbb_cis.R"))

# ============================================================================
# Step 3: Generate Outputs
# ============================================================================

run_timed_step(3, step_names[3], paste0(getwd(), "/01_code/02_analysis/05_model_outputs.R"))

# ============================================================================
# Summary
# ============================================================================

cat("========================================\n")
cat("PIPELINE COMPLETE!\n")
cat("========================================\n\n")

# Calculate overall pipeline timing
pipeline_end_time <- Sys.time()
pipeline_total_duration <- pipeline_end_time - pipeline_start_time

cat("TIMING SUMMARY\n")
cat("========================================\n")
cat("Pipeline started at:", format(pipeline_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Pipeline completed at:", format(pipeline_end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total pipeline duration:", round(as.numeric(pipeline_total_duration, units = "mins"), 2), "minutes\n")
cat("Total pipeline duration:", round(as.numeric(pipeline_total_duration, units = "hours"), 2), "hours\n\n")

cat("Step-by-step timing:\n")
cat("----------------------------------------\n")
total_executed_time <- 0
executed_steps <- 0

for (i in 1:3) {
  step_key <- paste0("step", i)
  if (step_key %in% names(step_times)) {
    step_info <- step_times[[step_key]]
    duration_mins <- round(as.numeric(step_info$duration, units = "mins"), 2)
    duration_hours <- round(as.numeric(step_info$duration, units = "hours"), 2)
    cat(sprintf("Step %d: %s\n", i, step_info$name))
    cat(sprintf("  Duration: %.2f minutes (%.2f hours)\n", duration_mins, duration_hours))
    cat(sprintf("  Started: %s\n", format(step_info$start_time, "%H:%M:%S")))
    cat(sprintf("  Ended: %s\n", format(step_info$end_time, "%H:%M:%S")))
    total_executed_time <- total_executed_time + as.numeric(step_info$duration, units = "mins")
    executed_steps <- executed_steps + 1
  }
}

cat("\nSummary:\n")
cat("----------------------------------------\n")
cat("Steps executed:", executed_steps, "\n")
cat("Total execution time:", round(total_executed_time, 2), "minutes\n")
cat("Total execution time:", round(total_executed_time / 60, 2), "hours\n\n")

# Find output directory (using new naming pattern)
# Read config to get user
config <- read_config(paste0(path_repo, "01_code/02_analysis/model_config.yaml"))
models_path <- get_models_path(path_onedrive, user = config$user)
latest_dir <- find_latest_version(models_path)

cat("Results saved to:\n")
cat("  ", latest_dir, "\n\n")

cat("Generated outputs:\n")
cat("  - Model tuning results: all_results_nested_*.RData\n")
cat("  - MBB confidence intervals: mbb_results_nested_*.rds\n")
cat("  - Performance metrics: tables/performance_metrics_with_mbb_*.csv\n")
cat("  - Model fit plots: figures/model_fit_*.pdf\n")
cat("  - Excess hospitalizations: tables/excess_hospitalizations_*.csv\n")
cat("  - Summary HTML tables: tables/excess_hospitalizations_summary_*.html\n")
cat("\n")

cat("Analysis complete!\n")

