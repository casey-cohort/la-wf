#-------------------------------
# LA wildfires project
# Master script to run complete Prophet + XGBoost analysis with MBB CIs
#-------------------------------

cat("========================================\n")
cat("Prophet + XGBoost Analysis Pipeline\n")
cat("with Moving Block Bootstrap CIs\n")
cat("========================================\n\n")

# Setup ----

# Check if test mode argument provided
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  test_mode <- tolower(args[1]) %in% c("true", "test", "t", "1")
} else {
  # Interactive prompt
  cat("Run in TEST MODE? (faster, fewer simulations)\n")
  cat("  TEST mode: 2 model combinations, 10 MBB simulations\n")
  cat("  PRODUCTION mode: All combinations from config, 1000 MBB simulations\n")
  response <- readline(prompt = "Enter 'test' for test mode, or 'prod' for production mode [prod]: ")
  test_mode <- tolower(trimws(response)) %in% c("test", "t", "true", "1")
}

# Set global environment variable for test mode
if (test_mode) {
  TEST_MODE <<- TRUE
  cat("\n*** RUNNING IN TEST MODE ***\n")
  cat("  - Limited to 2 model combinations\n")
  cat("  - MBB simulations: 10\n")
  cat("  - Faster execution for testing\n\n")
} else {
  TEST_MODE <<- FALSE
  cat("\nRunning in PRODUCTION MODE\n")
  cat("  - All model combinations from config\n")
  cat("  - MBB simulations: 1000\n")
  cat("  - Full analysis\n\n")
}

# Confirm before proceeding
if (!test_mode) {
  cat("WARNING: Production mode will take significant time to complete.\n")
  proceed <- readline(prompt = "Continue? (yes/no) [yes]: ")
  if (tolower(trimws(proceed)) %in% c("no", "n")) {
    cat("Aborted by user.\n")
    quit(save = "no")
  }
}

cat("\nStarting pipeline...\n\n")

# ============================================================================
# Step 1: Data Preparation
# ============================================================================

cat("========================================\n")
cat("STEP 1: Data Preparation\n")
cat("========================================\n")

# source(paste0(getwd(), "/01_code/02_analysis/01_data_prep.R"))
cat("\nData preparation complete!\n\n")

# ============================================================================
# Step 2: Model Tuning
# ============================================================================

cat("========================================\n")
cat("STEP 2: Model Tuning\n")
cat("========================================\n")

source(paste0(getwd(), "/01_code/02_analysis/02_model_tune_phxgb_parallel.R"))
cat("\nModel tuning complete!\n\n")

# ============================================================================
# Step 3: Generate MBB Confidence Intervals
# ============================================================================

cat("========================================\n")
cat("STEP 3: MBB Confidence Intervals\n")
cat("========================================\n")

source(paste0(getwd(), "/01_code/02_analysis/04_model_mbb_cis.R"))
cat("\nMBB CI generation complete!\n\n")

# ============================================================================
# Step 4: Generate Outputs
# ============================================================================

cat("========================================\n")
cat("STEP 4: Generate Outputs\n")
cat("========================================\n")

source(paste0(getwd(), "/01_code/02_analysis/05_model_outputs.R"))
cat("\nOutput generation complete!\n\n")

# ============================================================================
# Summary
# ============================================================================

cat("========================================\n")
cat("PIPELINE COMPLETE!\n")
cat("========================================\n\n")

# Find output directory
output_dirs <- list.dirs(paste0(path_onedrive, "02_output/"), full.names = TRUE, recursive = FALSE)
output_dirs <- output_dirs[grepl("model_run_", basename(output_dirs))]
latest_dir <- output_dirs[order(basename(output_dirs), decreasing = TRUE)][1]

cat("Results saved to:\n")
cat("  ", latest_dir, "\n\n")

cat("Generated outputs:\n")
cat("  - Model tuning results: all_results_nested_*.RData\n")
cat("  - MBB confidence intervals: mbb_results_nested_*.rds\n")
cat("  - Performance metrics: tables/performance_metrics_with_mbb_*.csv\n")
cat("  - Model fit plots: figures/model_fit_*.pdf\n")
cat("  - Excess hospitalizations: tables/excess_hospitalizations_*.csv\n")
cat("  - Summary HTML tables: tables/excess_hospitalizations_summary_*.html\n\n")

if (exists("TEST_MODE") && TEST_MODE) {
  cat("Note: This was a TEST run with limited data.\n")
  cat("For production analysis, run with TEST_MODE = FALSE\n\n")
}

cat("Analysis complete!\n")

