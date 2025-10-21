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

# Helper function for skipped steps
skip_step <- function(step_num, step_name, reason = "Using results from previous version") {
  cat("========================================\n")
  cat("STEP", step_num, ":", step_name, "(SKIPPED)\n")
  cat("========================================\n")
  cat(reason, "\n\n")
  
  step_times[[paste0("step", step_num)]] <<- list(
    name = step_name,
    executed = FALSE
  )
}

# Setup ----

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

# Check if test mode argument provided
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  test_mode <- tolower(args[1]) %in% c("true", "test", "t", "1")
} else {
  # Interactive prompt
  cat("Run in TEST MODE? (faster, fewer simulations)\n")
  cat("  TEST mode: 2 model combinations, 10 MBB simulations\n")
  cat("  PRODUCTION mode: All combinations from config, 1000 MBB simulations\n")
  response <- get_user_input("Enter 'test' for test mode, or 'prod' for production mode [prod]: ")
  test_mode <- tolower(response) %in% c("test", "t", "true", "1")
}

# Set global environment variable for test mode
if (test_mode) {
  TEST_MODE <<- TRUE
  RUN_MODE <<- "test"
  cat("\n*** RUNNING IN TEST MODE ***\n")
  cat("  - Limited to 2 model combinations\n")
  cat("  - MBB simulations: 10\n")
  cat("  - Faster execution for testing\n")
  cat("  - Using latest data (Step 1 will be skipped)\n")
  cat("  - Version folders will be named: model_run_test_*\n\n")
} else {
  TEST_MODE <<- FALSE
  RUN_MODE <<- "prod"
  cat("\nRunning in PRODUCTION MODE\n")
  cat("  - All model combinations from config\n")
  cat("  - MBB simulations: 1000\n")
  cat("  - Full analysis\n")
  cat("  - Version folders will be named: model_run_prod_*\n\n")
}

# Confirm before proceeding
if (!test_mode) {
  cat("WARNING: Production mode will take significant time to complete.\n")
  proceed <- get_user_input("Continue? (yes/no) [yes]: ")
  if (tolower(proceed) %in% c("no", "n")) {
    cat("Aborted by user.\n")
    quit(save = "no")
  }
}

# ============================================================================
# Determine execution mode: New run or Resume from step
# ============================================================================

# Helper function to find latest version folder
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

# Helper function to copy files from previous version
copy_previous_files <- function(from_dir, to_dir, start_step) {
  cat("\nCopying files from previous version...\n")
  cat("Source:", from_dir, "\n")
  cat("Destination:", to_dir, "\n")
  
  files_to_copy <- c()
  
  # Always copy config file
  config_files <- list.files(from_dir, pattern = "model_config_.*\\.yaml", full.names = TRUE)
  if (length(config_files) > 0) {
    files_to_copy <- c(files_to_copy, config_files[1])
  }
  
  # Copy based on starting step
  if (start_step >= 3) {
    # Copy step 2 outputs
    results_files <- list.files(from_dir, pattern = "all_results_nested_.*\\.RData", full.names = TRUE)
    if (length(results_files) > 0) {
      files_to_copy <- c(files_to_copy, results_files[1])
    }
    
    # Copy performance metrics from step 2
    perf_files <- list.files(from_dir, pattern = "performance_metrics_.*\\.csv", full.names = TRUE)
    if (length(perf_files) > 0) {
      files_to_copy <- c(files_to_copy, perf_files[1])
    }
  }
  
  if (start_step >= 4) {
    # Copy step 3 output
    mbb_files <- list.files(from_dir, pattern = "mbb_results_nested_.*\\.rds", full.names = TRUE)
    if (length(mbb_files) > 0) {
      files_to_copy <- c(files_to_copy, mbb_files[1])
    }
  }
  
  # Copy files
  if (length(files_to_copy) > 0) {
    for (file in files_to_copy) {
      file.copy(file, to_dir, overwrite = TRUE)
      cat("  Copied:", basename(file), "\n")
    }
    cat("File copying complete.\n\n")
  } else {
    cat("No files to copy for Step", start_step, "\n\n")
  }
}

# Source paths to check for previous runs
source(paste0(getwd(), "/01_code/paths.R"))

# Test mode automatically skips Step 1 and uses latest data
if (test_mode) {
  cat("\nTest mode: Automatically using latest data and skipping Step 1.\n")
  cat("Running Steps 2-4 as a new version.\n")
  RESUME_FROM_STEP <<- 2
  PREVIOUS_VERSION <<- NULL
} else {
  # Production mode: Ask user for execution mode
  cat("\nExecution Mode Selection\n")
  cat("========================================\n")
  cat("Options:\n")
  cat("  NEW: Start fresh run from Step 1\n")
  cat("  RESUME: Use latest version outputs and start from a specific step\n")
  response <- get_user_input("Enter 'new' to start fresh, or 'resume' to continue from a step [new]: ")
  resume_mode <- tolower(trimws(response)) %in% c("resume", "r")
  
  if (resume_mode) {
    # Find latest production version (ignoring test versions)
    latest_dir <- find_latest_version(paste0(path_onedrive, "02_output/"), mode = RUN_MODE)
    
    if (is.null(latest_dir)) {
      cat("\nERROR: No previous production model runs found in output directory.\n")
      cat("Cannot resume. Please run in 'new' mode first.\n")
      stop("No previous production runs found", call. = FALSE)
    }
    
    cat("\nFound latest production version:", basename(latest_dir), "\n")
    cat("Full path:", latest_dir, "\n")
    
    # Ask for confirmation
    confirm_response <- get_user_input("\nUse this directory to resume? (yes/no) [yes]: ")
    if (tolower(trimws(confirm_response)) %in% c("no", "n")) {
      cat("\nPlease specify the version to use:\n")
      
      # Ask for date
      date_response <- get_user_input("Enter date (YYYY-MM-DD): ")
      date_response <- trimws(date_response)
      
      # Validate date format
      specified_date <- tryCatch({
        as.Date(date_response)
      }, error = function(e) {
        cat("ERROR: Invalid date format. Please use YYYY-MM-DD format.\n")
        cat("Error details:", e$message, "\n")
        stop("Invalid date format", call. = FALSE)
      })
      
      # Ask for version number
      version_response <- get_user_input("Enter version number (e.g., v001, v002): ")
      version_response <- trimws(version_response)
      
      # Ensure version format
      if (!grepl("^v\\d{3}$", version_response)) {
        cat("ERROR: Invalid version format. Please use format v001, v002, etc.\n")
        stop("Invalid version format", call. = FALSE)
      }
      
      # Construct directory path
      specified_dir <- paste0(path_onedrive, "02_output/model_run_", RUN_MODE, "_", as.character(specified_date), ".", version_response, "/")
      
      # Expand path (handle ~ in path)
      specified_dir_expanded <- path.expand(specified_dir)
      
      # Check if directory exists
      if (!dir.exists(specified_dir_expanded)) {
        cat("\nERROR: Directory does not exist:\n")
        cat("  ", specified_dir, "\n")
        cat("  Expanded path:", specified_dir_expanded, "\n")
        stop("Directory not found", call. = FALSE)
      }
      
      # Display and confirm
      cat("\nSpecified version:", basename(specified_dir), "\n")
      cat("Full path:", specified_dir, "\n")
      
      final_confirm <- get_user_input("\nUse this directory to resume? (yes/no) [yes]: ")
      if (tolower(trimws(final_confirm)) %in% c("no", "n")) {
        cat("Resume cancelled by user.\n")
        stop("Resume cancelled", call. = FALSE)
      }
      
      # Use the specified directory for resume
      latest_dir <- specified_dir
    }
    
    # Ask which step to start from
    cat("\nSelect starting step:\n")
    cat("  Step 1: Data Preparation\n")
    cat("  Step 2: Model Tuning\n")
    cat("  Step 3: MBB Confidence Intervals\n")
    cat("  Step 4: Generate Outputs\n")
    
    step_response <- get_user_input("Enter step number (1-4) to start from [2]: ")
    step_response <- trimws(step_response)
    
    # Default to step 2 if empty
    if (step_response == "") {
      start_step <- 2
    } else {
      start_step <- as.integer(step_response)
    }
    
    # Validate step number
    if (is.na(start_step) || start_step < 1 || start_step > 4) {
      cat("\nERROR: Invalid step number. Must be between 1 and 4.\n")
      stop("Invalid step number", call. = FALSE)
    }
    
    cat("\nResuming from Step", start_step, "\n")
    cat("Previous version files will be copied to new version folder.\n")
    RESUME_FROM_STEP <<- start_step
    PREVIOUS_VERSION <<- latest_dir
  } else {
    cat("\nStarting new run from Step 1.\n")
    RESUME_FROM_STEP <<- 1
    PREVIOUS_VERSION <<- NULL
  }
}

cat("\nStarting pipeline...\n\n")

# ============================================================================
# Step 1: Data Preparation
# ============================================================================

if (RESUME_FROM_STEP <= 1) {
  run_timed_step(1, step_names[1], paste0(getwd(), "/01_code/02_analysis/01_data_prep.R"))
} else {
  skip_step(1, step_names[1], "Using latest dated data folder from previous run")
}

# ============================================================================
# Step 2: Model Tuning
# ============================================================================

if (RESUME_FROM_STEP <= 2) {
  run_timed_step(2, step_names[2], paste0(getwd(), "/01_code/02_analysis/02_model_tune_phxgb_parallel.R"))
  
  # If resuming, copy previous files after new version folder is created
  if (!is.null(PREVIOUS_VERSION) && RESUME_FROM_STEP >= 2) {
    # Find the newly created version folder
    new_version_dir <- find_latest_version(paste0(path_onedrive, "02_output/"), mode = RUN_MODE)
    if (!is.null(new_version_dir) && new_version_dir != PREVIOUS_VERSION) {
      copy_previous_files(PREVIOUS_VERSION, new_version_dir, RESUME_FROM_STEP)
    }
  }
} else {
  skip_step(2, step_names[2])
}

# ============================================================================
# Step 3: Generate MBB Confidence Intervals
# ============================================================================

if (RESUME_FROM_STEP <= 3) {
  # Setup code for Step 3
  setup_code <- if (RESUME_FROM_STEP == 3 && !is.null(PREVIOUS_VERSION)) {
    quote({
      # Step 2 didn't run, so we need to create the version folder and copy files
      source(paste0(getwd(), "/01_code/paths.R"))
      source(paste0(getwd(), "/01_code/utils.R"))
      ver <- gen_ver_number(paste0(path_onedrive, "02_output/"), mode = RUN_MODE)
      folder_name <- paste0("model_run_", RUN_MODE, "_", Sys.Date(), ".", ver, "/")
      dir.create(paste0(path_onedrive, "02_output/", folder_name), showWarnings = FALSE)
      new_version_dir <- paste0(path_onedrive, "02_output/", folder_name)
      copy_previous_files(PREVIOUS_VERSION, new_version_dir, RESUME_FROM_STEP)
    })
  } else NULL
  
  run_timed_step(3, step_names[3], paste0(getwd(), "/01_code/02_analysis/04_model_mbb_cis.R"), setup_code)
} else {
  skip_step(3, step_names[3])
}

# ============================================================================
# Step 4: Generate Outputs
# ============================================================================

if (RESUME_FROM_STEP <= 4) {
  # Setup code for Step 4
  setup_code <- if (RESUME_FROM_STEP == 4 && !is.null(PREVIOUS_VERSION)) {
    quote({
      # Steps 2 and 3 didn't run, so we need to create the version folder and copy files
      source(paste0(getwd(), "/01_code/paths.R"))
      source(paste0(getwd(), "/01_code/utils.R"))
      ver <- gen_ver_number(paste0(path_onedrive, "02_output/"), mode = RUN_MODE)
      folder_name <- paste0("model_run_", RUN_MODE, "_", Sys.Date(), ".", ver, "/")
      dir.create(paste0(path_onedrive, "02_output/", folder_name), showWarnings = FALSE)
      new_version_dir <- paste0(path_onedrive, "02_output/", folder_name)
      copy_previous_files(PREVIOUS_VERSION, new_version_dir, RESUME_FROM_STEP)
    })
  } else NULL
  
  run_timed_step(4, step_names[4], paste0(getwd(), "/01_code/02_analysis/05_model_outputs.R"), setup_code)
}

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
skipped_steps <- 0

for (i in 1:4) {
  step_key <- paste0("step", i)
  if (step_key %in% names(step_times)) {
    step_info <- step_times[[step_key]]
    if (step_info$executed) {
      duration_mins <- round(as.numeric(step_info$duration, units = "mins"), 2)
      duration_hours <- round(as.numeric(step_info$duration, units = "hours"), 2)
      cat(sprintf("Step %d: %s\n", i, step_info$name))
      cat(sprintf("  Duration: %.2f minutes (%.2f hours)\n", duration_mins, duration_hours))
      cat(sprintf("  Started: %s\n", format(step_info$start_time, "%H:%M:%S")))
      cat(sprintf("  Ended: %s\n", format(step_info$end_time, "%H:%M:%S")))
      total_executed_time <- total_executed_time + as.numeric(step_info$duration, units = "mins")
      executed_steps <- executed_steps + 1
    } else {
      cat(sprintf("Step %d: %s (SKIPPED)\n", i, step_info$name))
      skipped_steps <- skipped_steps + 1
    }
  }
}

cat("\nSummary:\n")
cat("----------------------------------------\n")
cat("Steps executed:", executed_steps, "\n")
cat("Steps skipped:", skipped_steps, "\n")
cat("Total execution time:", round(total_executed_time, 2), "minutes\n")
cat("Total execution time:", round(total_executed_time / 60, 2), "hours\n\n")

# Find output directory (using mode-specific pattern)
latest_dir <- find_latest_version(paste0(path_onedrive, "02_output/"), mode = RUN_MODE)

cat("Results saved to:\n")
cat("  ", latest_dir, "\n\n")

# Show execution mode information
if (exists("TEST_MODE") && TEST_MODE) {
  cat("Execution Mode: TEST MODE (NEW RUN)\n")
  cat("  Steps executed:", paste(RESUME_FROM_STEP:4, collapse=", "), "\n")
  cat("  Steps skipped:", if(RESUME_FROM_STEP > 1) paste(1:(RESUME_FROM_STEP-1), collapse=", ") else "None", "\n")
  cat("  Used latest dated data folder\n\n")
} else if (!is.null(PREVIOUS_VERSION) && RESUME_FROM_STEP > 1) {
  cat("Execution Mode: RESUMED\n")
  cat("  Started from: Step", RESUME_FROM_STEP, "\n")
  cat("  Previous version:", basename(PREVIOUS_VERSION), "\n")
  cat("  Steps executed:", paste(RESUME_FROM_STEP:4, collapse=", "), "\n")
  cat("  Steps skipped:", if(RESUME_FROM_STEP > 1) paste(1:(RESUME_FROM_STEP-1), collapse=", ") else "None", "\n\n")
} else {
  cat("Execution Mode: NEW RUN\n")
  cat("  All steps executed (1-4)\n\n")
}

cat("Generated outputs:\n")
if (RESUME_FROM_STEP <= 1) {
  cat("  - Dated data folder: 01_data/02_processed/train_test/YYYY-MM-DD/\n")
}
if (RESUME_FROM_STEP <= 2) {
  cat("  - Model tuning results: all_results_nested_*.RData\n")
}
if (RESUME_FROM_STEP <= 3) {
  cat("  - MBB confidence intervals: mbb_results_nested_*.rds\n")
}
if (RESUME_FROM_STEP <= 4) {
  cat("  - Performance metrics: tables/performance_metrics_with_mbb_*.csv\n")
  cat("  - Model fit plots: figures/model_fit_*.pdf\n")
  cat("  - Excess hospitalizations: tables/excess_hospitalizations_*.csv\n")
  cat("  - Summary HTML tables: tables/excess_hospitalizations_summary_*.html\n")
}
cat("\n")

if (exists("TEST_MODE") && TEST_MODE) {
  cat("Note: This was a TEST run with limited data.\n")
  cat("For production analysis, run with TEST_MODE = FALSE\n\n")
}

cat("Analysis complete!\n")

