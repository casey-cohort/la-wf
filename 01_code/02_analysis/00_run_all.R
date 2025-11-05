#-------------------------------
# LA wildfires project
# Master script to run complete Prophet + XGBoost analysis with MBB CIs
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
step_names <- c("Data Preparation", "Model Tuning", "MBB Confidence Intervals", "Generate Outputs")

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
source(paste0(getwd(), "/01_code/utils.R"))

# Helper function to get user input in both interactive and terminal modes
get_user_input <- function(prompt_text) {
  cat(prompt_text)
  if (interactive()) {
    # Running in RStudio or R console
    response <- readline()
  } else {
    # Running from terminal (Rscript)
    response <- readLines("stdin", n=1, warn=FALSE)
  }
  return(trimws(response))
}

cat("Starting analysis pipeline...\n\n")

# Ask if user wants to rerun Step 1 (data preparation)
cat("========================================\n")
cat("Data Preparation Step\n")
cat("========================================\n")
cat("Step 1 prepares training/testing data from raw sources.\n")
cat("You may skip if using existing data.\n\n")
response <- get_user_input("Rerun data preparation? (yes/no) [yes]: ")
run_step1 <- !(tolower(response) %in% c("no", "n", "skip"))

if (run_step1) {
  cat("Will run Step 1: Data Preparation\n\n")
} else {
  cat("Skipping Step 1: Will use latest dated data folder\n\n")
}


# ============================================================================
# Step 1: Data Preparation
# ============================================================================

if (run_step1) {
  run_timed_step(1, step_names[1], paste0(getwd(), "/01_code/02_analysis/01_data_prep.R"))
} else {
  cat("========================================\n")
  cat("STEP 1: Data Preparation (SKIPPED)\n")
  cat("========================================\n")
  cat("Using existing data from latest dated folder\n\n")
}

# ============================================================================
# Step 2: Model Tuning
# ============================================================================

run_timed_step(2, step_names[2], paste0(getwd(), "/01_code/02_analysis/02_model_tune_phxgb_parallel.R"))

# ============================================================================
# Step 3: Generate MBB Confidence Intervals
# ============================================================================

run_timed_step(3, step_names[3], paste0(getwd(), "/01_code/02_analysis/04_model_mbb_cis.R"))

# ============================================================================
# Step 4: Generate Outputs
# ============================================================================

run_timed_step(4, step_names[4], paste0(getwd(), "/01_code/02_analysis/05_model_outputs.R"))

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

for (i in 1:4) {
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
source(paste0(getwd(), "/01_code/paths.R"))
find_latest_version <- function(output_path) {
  output_dirs <- list.dirs(output_path, full.names = TRUE, recursive = FALSE)
  # Filter by new pattern: model_run_YYYY-MM-DD.v###_x##_sim###
  pattern <- "^model_run_\\d{4}-\\d{2}-\\d{2}\\.v\\d{3}_x\\d+_sim\\d+$"
  output_dirs <- output_dirs[grepl(pattern, basename(output_dirs))]
  if (length(output_dirs) == 0) {
    return(NULL)
  }
  latest_dir <- output_dirs[order(basename(output_dirs), decreasing = TRUE)][1]
  return(latest_dir)
}

latest_dir <- find_latest_version(paste0(path_onedrive, "02_output/"))

cat("Results saved to:\n")
cat("  ", latest_dir, "\n\n")

cat("Generated outputs:\n")
if (run_step1) {
  cat("  - Dated data folder: 01_data/02_processed/train_test/YYYY-MM-DD/\n")
}
cat("  - Model tuning results: all_results_nested_*.RData\n")
cat("  - MBB confidence intervals: mbb_results_nested_*.rds\n")
cat("  - Performance metrics: tables/performance_metrics_with_mbb_*.csv\n")
cat("  - Model fit plots: figures/model_fit_*.pdf\n")
cat("  - Excess hospitalizations: tables/excess_hospitalizations_*.csv\n")
cat("  - Summary HTML tables: tables/excess_hospitalizations_summary_*.html\n")
cat("\n")

if (!run_step1) {
  cat("Note: Step 1 (Data Preparation) was skipped.\n")
  cat("Analysis used existing data from previous run.\n\n")
}

cat("Analysis complete!\n")

