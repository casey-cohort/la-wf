#-------------------------------
# LA wildfires project
# General I/O utility functions
#-------------------------------

#-------------------------------
# Seed Management and Validation
#-------------------------------

#' Validate seed reproducibility
#'
#' Tests that the gen_seed() function produces consistent results.
#' Useful for debugging reproducibility issues.
#'
#' @param global_seed Global seed value
#' @param markers Character vector of markers to test
#' @param n_iterations Number of times to test (default: 100)
#'
#' @return TRUE if all iterations produce identical seeds, FALSE otherwise
#'
#' @examples
#' validate_seed_reproducibility(0112358, c("IP", "evac", "num_enc_resp", "test"))
#'
validate_seed_reproducibility <- function(global_seed, markers, n_iterations = 100) {
  
  # Source gen_seed if not available
  if (!exists("gen_seed")) {
    source(paste0(getwd(), "/01_code/00_utils/utils_tuning.R"))
  }
  
  cat("Testing seed reproducibility...\n")
  cat("Global seed:", global_seed, "\n")
  cat("Markers:", paste(markers, collapse = ", "), "\n")
  cat("Iterations:", n_iterations, "\n\n")
  
  # Generate seeds multiple times
  seeds <- sapply(1:n_iterations, function(i) {
    gen_seed(global_seed, markers)
  })
  
  # Check if all are identical
  unique_seeds <- unique(seeds)
  
  if (length(unique_seeds) == 1) {
    cat("✓ SUCCESS: All", n_iterations, "iterations produced identical seed:", unique_seeds[1], "\n")
    return(TRUE)
  } else {
    cat("✗ FAILURE: Found", length(unique_seeds), "different seeds across", n_iterations, "iterations\n")
    cat("Unique seeds:", paste(unique_seeds, collapse = ", "), "\n")
    return(FALSE)
  }
}


#' Log derived seed for debugging
#'
#' Helper function to log seed derivation. Useful for tracing seed flow through pipeline.
#'
#' @param global_seed Global seed value
#' @param markers Character vector of markers
#' @param verbose If TRUE, prints to console (default: TRUE)
#'
#' @return Derived seed (integer)
#'
#' @examples
#' seed <- log_seed_derivation(0112358, c("IP", "evac", "num_enc_resp", "grid_tune"))
#'
log_seed_derivation <- function(global_seed, markers, verbose = TRUE) {
  
  # Source gen_seed if not available
  if (!exists("gen_seed")) {
    source(paste0(getwd(), "/01_code/00_utils/utils_tuning.R"))
  }
  
  derived_seed <- gen_seed(global_seed, markers)
  
  if (verbose) {
    cat("Seed derivation:\n")
    cat("  Global seed:", global_seed, "\n")
    cat("  Markers:", paste(markers, collapse = " → "), "\n")
    cat("  Derived seed:", derived_seed, "\n\n")
  }
  
  return(derived_seed)
}


#' Validate global seed from config
#'
#' Checks that the global seed is a valid integer and logs it.
#'
#' @param config Configuration list containing seed
#'
#' @return TRUE if valid, stops with error if invalid
#'
#' @examples
#' config <- read_config("model_config.yaml")
#' validate_global_seed(config)
#'
validate_global_seed <- function(config) {
  
  if (is.null(config$seed)) {
    stop("ERROR: No seed found in config. Please add 'seed: <integer>' to model_config.yaml")
  }
  
  seed_value <- config$seed
  
  # Check if it's numeric
  if (!is.numeric(seed_value)) {
    stop("ERROR: Seed must be numeric. Found: ", seed_value, " (", class(seed_value), ")")
  }
  
  # Check if it's a valid integer
  if (seed_value != as.integer(seed_value)) {
    warning("Seed is not an integer. Converting ", seed_value, " to ", as.integer(seed_value))
    config$seed <- as.integer(seed_value)
  }
  
  # Check reasonable range (avoid very small seeds that might be unintended)
  if (abs(seed_value) < 1) {
    warning("Seed is very small (", seed_value, "). This may be unintended. Typical seeds are larger integers.")
  }
  
  cat("✓ Global seed validated:", seed_value, "\n\n")
  return(TRUE)
}

#' Generate version number for model output directory
#'
#' Looks for existing model output directories from today's date and increments version.
#' If no directories exist for today, starts at v001.
#' New format: model_run_YYYY-MM-DD.v###_x##_sim###
#'
#' @param path Path to directory containing model output folders
#'
#' @return Version string (e.g., "v001", "v002")
#'
#' @examples
#' models_path <- get_models_path(path_onedrive, user = "lbw")
#' ver <- gen_ver_number(models_path)
#'
gen_ver_number <- function(path) {
  all_dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  
  # get today's date in the format used in folder names
  today <- Sys.Date()
  
  # filter for folders that match the pattern with today's date
  # Pattern matches: model_run_2025-10-22.v001_x20_sim1000
  pattern <- paste0("^model_run_", today, "\\.v\\d{3}_x\\d+_sim\\d+$")
  existing_folders <- all_dirs[grepl(pattern, all_dirs)]
  
  if (length(existing_folders) == 0) {
    return("v001")
  } else {
    # extract version numbers from existing folders
    versions <- sapply(existing_folders, function(x) {
      # extract the version number (the 3 digits after .v and before _x)
      version_match <- regmatches(x, regexpr("\\.v(\\d{3})_", x))
      if (length(version_match) > 0) {
        as.integer(sub("\\.v(\\d{3})_", "\\1", version_match))
      } else {
        0
      }
    })
    
    # remove any NA values and get the next version
    versions <- versions[!is.na(versions)]
    new_version <- max(versions) + 1
    return(paste0("v", sprintf("%03d", new_version)))
  }
}


#-------------------------------
# Config functions
#-------------------------------

#' Read config from YAML file
#'
#' Reads configuration from YAML file with support for TEST_CONFIG_PATH environment
#' variable override (useful for parallel testing). Handles both cartesian product
#' format (models_to_run) and explicit list format (models_to_run_flat).
#'
#' @param file_path Path to YAML config file. If NULL, uses default location.
#'                  Can be overridden by TEST_CONFIG_PATH environment variable.
#'
#' @return List containing configuration parameters
#'
#' @examples
#' config <- read_config(paste0(getwd(), "/01_code/02_analysis/model_config.yaml"))
#'
read_config <- function(file_path = NULL) {
  # Check for environment variable first (for parallel testing)
  test_config <- Sys.getenv("TEST_CONFIG_PATH", unset = "")
  if (test_config != "") {
    file_path <- test_config
    cat("Using test config from environment:", file_path, "\n")
  } else if (is.null(file_path)) {
    # If no env var and no path provided, use default
    file_path <- paste0(getwd(), "/01_code/02_analysis/model_config.yaml")
  }
  # If file_path was provided explicitly and no env var, use the provided path

  # Read in YAML file -------------------------------
  config <- yaml::read_yaml(file_path)
  
  # Handle YAML parser quirk that creates duplicate fields -------------------------------
  # When using multi-line format for models_to_run_flat, the parser creates both 
  # models_to_run and models_to_run_flat as references to the same object
  # CHECK THIS BEFORE MODIFYING models_to_run!
  yaml_parser_created_duplicate <- FALSE
  if(!is.null(config$models_to_run) & !is.null(config$models_to_run_flat)){
    # Check if they're identical (YAML parsing quirk with multi-line format)
    if(identical(config$models_to_run, config$models_to_run_flat)){
      # Mark this as a parser quirk (they're the same object, use models_to_run_flat)
      yaml_parser_created_duplicate <- TRUE
      # Remove the duplicate to avoid confusion
      config$models_to_run <- NULL
    } else {
      # If they're different, user specified both which is an error
      stop("Error: Must provide only one of 'models_to_run' (cartesian product) or 'models_to_run_flat' (explicit list) in the config file.")
    }
  }
  
  # Only modify models_to_run if it exists and is NOT a parser duplicate
  if(!is.null(config$models_to_run) & !yaml_parser_created_duplicate){
    config$models_to_run$cause <- paste0(config$outcome_type, "_", config$models_to_run$cause)
  }

  # Apply prefix to models_to_run_flat if it was provided directly (not generated from models_to_run)
  if(!is.null(config$models_to_run_flat) & (is.null(config$models_to_run) | yaml_parser_created_duplicate)){
    # Check if any cause already has the prefix (to avoid double-prefixing)
    has_prefix <- any(sapply(config$models_to_run_flat, function(x) {
      grepl(paste0("^", config$outcome_type, "_"), x$cause)
    }))
    
    # Only apply prefix if causes don't already have it
    if(!has_prefix){
      config$models_to_run_flat <- lapply(config$models_to_run_flat, function(x) {
        x$cause <- paste0(config$outcome_type, "_", x$cause)
        return(x)
      })
    }
  }

  # Validate that at least one is provided -------------------------------
  if(is.null(config$models_to_run) & is.null(config$models_to_run_flat)){
    stop("Error: Must provide either 'models_to_run' or 'models_to_run_flat' in the config file.")
  }

  # If models_to_run is provided (and not a YAML parser duplicate), expand to cartesian product -------
  if(!is.null(config$models_to_run) & !yaml_parser_created_duplicate){
    # Extract the vectors from the nested structure
    encounter_types <- config$models_to_run$encounter_type
    exposure_categories <- config$models_to_run$exposure_category
    causes <- config$models_to_run$cause
    
    # Create all combinations using expand.grid
    combinations <- expand.grid(
      encounter_type = encounter_types,
      exposure_category = exposure_categories,
      cause = causes,
      stringsAsFactors = FALSE
    )
    
    # Convert to list of lists
    models_to_run_flat <- lapply(1:nrow(combinations), function(i) {
      list(
        encounter_type = as.character(combinations$encounter_type[i]),
        exposure_category = as.character(combinations$exposure_category[i]),
        cause = as.character(combinations$cause[i])
      )
    })
    
    # Store as models_to_run_flat
    config$models_to_run_flat <- models_to_run_flat
    # Keep models_to_run for reference
  }

  # Validate that models_to_run_flat is not empty -------------------------------
  if(length(config$models_to_run_flat) == 0){
    stop("Error: 'models_to_run_flat' cannot be empty.")
  }

  # Make sure everything in models_to_run_flat is unique -------------------------------
  # Convert to data frame for proper duplicate checking
  combinations_df <- do.call(rbind, lapply(config$models_to_run_flat, function(x) {
    data.frame(
      encounter_type = x$encounter_type,
      exposure_category = x$exposure_category,
      cause = x$cause,
      stringsAsFactors = FALSE
    )
  }))
  
  # Check for duplicates
  if(nrow(combinations_df) != nrow(unique(combinations_df))){
    # Find and report duplicates
    duplicates <- combinations_df[duplicated(combinations_df) | duplicated(combinations_df, fromLast = TRUE), ]
    cat("Duplicate combinations found:\n")
    print(duplicates)
    stop("Error: you have specified duplicate model combinations! Please make sure all encounter_type -- exposure_category -- cause combinations are unique.")
  }
  
  return(config)
}


#' Write config to YAML file
#'
#' Writes configuration object to YAML file.
#'
#' @param config Configuration list object
#' @param file_path Path where YAML file should be written
#'
#' @examples
#' write_config(config, paste0(output_dir, "model_config.yaml"))
#'
write_config <- function(config, file_path) {
  yaml::write_yaml(config, file_path)
}


#-------------------------------
# Parallel Processing Setup
#-------------------------------

#' Setup parallel processing based on config
#'
#' Configures future plan for parallel processing, leaving specified number of cores free.
#'
#' @param config Configuration list containing cores_to_leave_out parameter
#'
#' @return Number of cores configured for parallel processing
#'
#' @examples
#' n_cores <- setup_parallel_processing(config)
#'
setup_parallel_processing <- function(config) {
  cores_to_leave_out <- ifelse(is.null(config$cores_to_leave_out), 2, config$cores_to_leave_out)
  n_cores <- max(1, floor(parallel::detectCores() - cores_to_leave_out))
  cat("Using", n_cores, "cores out of", parallel::detectCores(), "available (leaving", cores_to_leave_out, "cores free)\n")
  future::plan(future::multisession, workers = n_cores)
  return(n_cores)
}


#-------------------------------
# Directory and File Management
#-------------------------------

#' Get models directory path with user subfolder
#'
#' Constructs the path to the models directory with user subfolder.
#' Creates the user directory if it doesn't exist.
#'
#' @param path_onedrive Path to OneDrive directory
#' @param user User name from config (defaults to reading from config or "default")
#'
#' @return Full path to models directory with user subfolder
#'
#' @examples
#' models_path <- get_models_path(path_onedrive, user = "lbw")
#'
get_models_path <- function(path_onedrive, user = NULL) {
  # Check if MODEL_PATH environment variable is set (from run_pipeline with model_path parameter)
  env_model_path <- Sys.getenv("MODEL_PATH", unset = "")
  if (env_model_path != "") {
    # Create directory if it doesn't exist
    if (!dir.exists(env_model_path)) {
      dir.create(env_model_path, recursive = TRUE, showWarnings = FALSE)
    }
    return(env_model_path)
  }
  
  if (is.null(user)) {
    # Try to get from config if available
    config_file <- paste0(getwd(), "/01_code/02_analysis/model_config.yaml")
    if (file.exists(config_file)) {
      config <- yaml::read_yaml(config_file)
      user <- config$user
    }
    if (is.null(user) || user == "") {
      user <- "default"
    }
  }
  models_path <- paste0(path_onedrive, "03_modeling-and-results/01_modeling/", user, "/")
  # Create user directory if it doesn't exist
  if (!dir.exists(models_path)) {
    dir.create(models_path, recursive = TRUE, showWarnings = FALSE)
  }
  return(models_path)
}

#' Find latest model output directory
#'
#' Searches for model output directories matching the naming pattern:
#' model_run_YYYY-MM-DD.v###_x##_sim###
#' and returns the most recent one (sorted by directory name)
#'
#' @param output_path Path to directory containing model output folders
#'
#' @return Full path to the latest model output directory, or NULL if none found
#'
#' @examples
#' models_path <- get_models_path(path_onedrive, user = "lbw")
#' latest_dir <- find_latest_version(models_path)
#'
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


#' Get output directory
#'
#' Gets model output directory from MODEL_OUTPUT_DIR environment variable,
#' or falls back to finding the latest version in the user's models subfolder.
#'
#' @param path_onedrive Path to OneDrive directory
#' @param required If TRUE, stops with error if no directory found. If FALSE, returns NULL.
#' @param user User name from config (defaults to reading from config or "default")
#'
#' @return Full path to output directory, or NULL if not found and required=FALSE
#'
#' @examples
#' latest_dir <- get_output_directory(path_onedrive)
#'
get_output_directory <- function(path_onedrive, required = TRUE, user = NULL) {
  output_dir_env <- Sys.getenv("MODEL_OUTPUT_DIR", unset = "")
  cat("MODEL_OUTPUT_DIR environment variable:", ifelse(output_dir_env == "", "(not set)", output_dir_env), "\n")
  
  if (output_dir_env != "" && dir.exists(output_dir_env)) {
    cat("Using output directory from MODEL_OUTPUT_DIR environment variable:", output_dir_env, "\n")
    return(output_dir_env)
  } else {
    cat("MODEL_OUTPUT_DIR not set or directory doesn't exist, falling back to find_latest_version()\n")
    models_path <- get_models_path(path_onedrive, user)
    latest_dir <- find_latest_version(models_path)
    
    if (is.null(latest_dir) && required) {
      stop("No model output directories found. Please run 02_model_tune_phxgb_parallel.R first.")
    }
    
    if (!is.null(latest_dir)) {
      cat("Loading model results from:", latest_dir, "\n")
    }
    return(latest_dir)
  }
}


#' Extract version suffix from directory path
#'
#' Extracts the version suffix (timestamp) from a model output directory name.
#'
#' @param dir_path Full path to model output directory
#'
#' @return Version suffix string (e.g., "2025-11-30.v001_x20_sim1000")
#'
#' @examples
#' mod_ver_suffix <- extract_version_suffix(latest_dir)
#'
extract_version_suffix <- function(dir_path) {
  return(sub("model_run_", "", basename(dir_path)))
}


#' Create output subdirectories
#'
#' Creates and validates output subdirectories within a base directory.
#'
#' @param base_dir Base directory where subdirectories should be created
#' @param subdirs Character vector of subdirectory names to create
#'
#' @return Named list of full paths to created subdirectories
#'
#' @examples
#' output_dirs <- create_output_subdirectories(latest_dir, c("figures", "tables"))
#' figures_dir <- output_dirs$figures
#'
create_output_subdirectories <- function(base_dir, subdirs = c("figures", "tables")) {
  cat("Creating output directories:\n")
  
  result <- list()
  for (subdir in subdirs) {
    full_path <- paste0(base_dir, "/", subdir, "/")
    cat("  ", subdir, ": ", full_path, "\n", sep = "")
    dir.create(full_path, showWarnings = FALSE, recursive = TRUE)
    
    # Verify directory was created
    if (!dir.exists(full_path)) {
      stop("Failed to create ", subdir, " directory: ", full_path)
    }
    
    result[[subdir]] <- full_path
  }
  
  cat("Output directories created successfully\n\n")
  return(result)
}


#-------------------------------
# Data Loading Functions
#-------------------------------

#' Get train/test data path
#'
#' Gets path to train/test data, prompting user for version selection.
#' Works in both interactive R sessions and CLI/Rscript mode.
#'
#' @param path_onedrive Path to OneDrive directory
#' @param prompt_user If TRUE, prompts user to select version (REQUIRED). If FALSE, must provide date parameter.
#' @param date Optional specific date to use (format: "YYYY-MM-DD")
#'
#' @return Full path to train/test data directory
#'
#' @examples
#' train_test_path <- get_train_test_data_path(path_onedrive, prompt_user = TRUE)
#' train_test_path <- get_train_test_data_path(path_onedrive, prompt_user = FALSE, date = "2024-11-29")
#'
get_train_test_data_path <- function(path_onedrive, prompt_user = TRUE, date = NULL) {
  base_path <- paste0(path_onedrive, "01_data/02_processed/train_test/")
  
  # Use specified date if provided
  if (!is.null(date)) {
    data_path <- paste0(base_path, date, "/")
    if (!dir.exists(data_path)) {
      stop("Specified train/test data directory does not exist: ", data_path)
    }
    cat("Using train/test data from:", date, "\n")
    return(data_path)
  }
  
  # If not prompting user, must provide date parameter
  if (!prompt_user) {
    stop("ERROR: When prompt_user = FALSE, you must provide the 'date' parameter.\n",
         "Example: get_train_test_data_path(path_onedrive, prompt_user = FALSE, date = '2024-11-29')")
  }
  
  # Prompt user for version with validation
  # Use scan() which works in both interactive R and Rscript mode
  valid_input <- FALSE
  while (!valid_input) {
    cat("Enter the train/test data version to use (format: YYYY-MM-DD): ")
    
    # Use scan() instead of readline() - works in CLI/Rscript mode
    train_test_date <- tryCatch({
      scan(file = "stdin", what = character(), n = 1, quiet = TRUE)
    }, error = function(e) {
      stop("ERROR: Unable to read user input. Cannot proceed without train/test data version.\n",
           "To run in non-interactive mode, use: get_train_test_data_path(path_onedrive, prompt_user = FALSE, date = 'YYYY-MM-DD')")
    })
    
    # Check if user provided input
    if (length(train_test_date) == 0) {
      stop("ERROR: No input provided. Cannot proceed without train/test data version.\n",
           "To run in non-interactive mode, use: get_train_test_data_path(path_onedrive, prompt_user = FALSE, date = 'YYYY-MM-DD')")
    }
    
    # check if input matches YYYY-MM-DD format
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", train_test_date)) {
      # check if directory exists
      data_path <- paste0(base_path, train_test_date, "/")
      if (dir.exists(data_path)) {
        valid_input <- TRUE
        cat("Using train/test data from:", train_test_date, "\n")
      } else {
        cat("Error: Directory does not exist:", data_path, "\n")
        cat("Available versions:\n")
        available_dirs <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
        cat(paste(available_dirs, collapse = "\n"), "\n")
      }
    } else {
      cat("Error: Invalid format. Please use YYYY-MM-DD format (e.g., 2024-11-29)\n")
    }
  }
  
  return(data_path)
}


#' Load encounter data from parquet file
#'
#' Loads and filters parquet data by encounter type and exposure category.
#'
#' @param train_test_path Path to train/test data directory
#' @param file_name Name of parquet file to load
#' @param exposure Exposure category to filter by
#' @param enc Encounter type to filter by
#'
#' @return Filtered data frame with date column as Date type
#'
#' @examples
#' df <- load_encounter_data(train_test_path, "df-train-test_sf.parquet", "high_smoke", "ED")
#'
load_encounter_data <- function(train_test_path, file_name, exposure, enc) {
  df <- arrow::open_dataset(paste0(train_test_path, file_name)) %>%
    dplyr::filter(
      exposure_category == !!exposure & enc_type == !!enc
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(date = as.Date(date))
  
  return(df)
}


#' Create time series split from config parameters
#'
#' Creates time series split using parameters from config file.
#'
#' @param df_train_test Data frame to split
#' @param config Configuration list containing train_test_params
#'
#' @return rsample split object
#'
#' @examples
#' splits <- create_time_series_split(df_train_test, config)
#'
create_time_series_split <- function(df_train_test, config) {
  splits <- df_train_test %>%
    dplyr::ungroup() %>%  # Ensure data is ungrouped before splitting
    timetk::time_series_split(
      assess = config$train_test_params$assess_split,
      cumulative = TRUE,
      date_var = date
    )
  return(splits)
}


#' Load nested results from output directory
#'
#' Loads nested model results file from output directory with error handling.
#'
#' @param output_dir Path to output directory
#' @param pattern Regular expression pattern to match results file
#'
#' @return Loaded results object (all_results)
#'
#' @examples
#' all_results <- load_nested_results(latest_dir)
#'
load_nested_results <- function(output_dir, pattern = "all_results_nested_.*\\.RData") {
  # Find the nested results file in results subdirectory
  results_subdir <- paste0(output_dir, "/results/")
  results_files <- list.files(results_subdir, pattern = pattern, full.names = TRUE)
  if (length(results_files) == 0) {
    cat("ERROR: No nested results file found in:", results_subdir, "\n")
    cat("Looking for pattern:", pattern, "\n")
    cat("Files in directory:\n")
    print(list.files(results_subdir))
    stop("No nested results file found in ", results_subdir)
  }
  results_file <- results_files[1]
  cat("Loading model results from:", results_file, "\n")
  
  # Load results
  load(results_file)
  cat("Results loaded successfully\n")
  cat("Number of encounter types in all_results:", length(all_results), "\n\n")
  
  return(all_results)
}


#' Load MBB results from output directory
#'
#' Loads MBB results file from output directory with error handling.
#'
#' @param output_dir Path to output directory
#' @param pattern Regular expression pattern to match MBB results file
#'
#' @return Loaded MBB results object
#'
#' @examples
#' mbb_results <- load_mbb_results(latest_dir)
#'
load_mbb_results <- function(output_dir, pattern = "mbb_results_nested_.*\\.rds") {
  # Find the MBB results file in results subdirectory
  results_subdir <- paste0(output_dir, "/results/")
  mbb_files <- list.files(results_subdir, pattern = pattern, full.names = TRUE)
  if (length(mbb_files) == 0) {
    cat("ERROR: No MBB results file found in:", results_subdir, "\n")
    cat("Looking for pattern:", pattern, "\n")
    cat("Files in directory:\n")
    print(list.files(results_subdir))
    stop("No MBB results file found. Please run 04_model_mbb_cis.R first.")
  }
  mbb_file <- mbb_files[1]
  cat("Loading:", mbb_file, "\n\n")
  
  mbb_results <- readRDS(mbb_file)
  return(mbb_results)
}

