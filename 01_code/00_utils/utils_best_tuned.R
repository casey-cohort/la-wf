#-------------------------------
# LA wildfires project
# Utility functions for running batches of best-tuned models
#-------------------------------

#-------------------------------
# Model Combination Helpers
#-------------------------------

#' Generate model combinations from parameter vectors
#'
#' Creates all combinations of encounter_type, exposure_category, and cause
#' using expand.grid and converts to a list of lists format.
#'
#' @param encounter_type Character vector of encounter types
#' @param exposure_category Character vector of exposure categories
#' @param cause Character vector of causes
#'
#' @return List of lists, where each inner list contains encounter_type, exposure_category, and cause
#'
#' @examples
#' Example 1: Run specific models manually
#'   Option A: Using make_models helper (easier when encounter_type is the same)
#'   models_to_run <- make_models("ED", 
#'     list(c("high_smoke", "rate_enc"), 
#'          c("mid_smoke", "rate_enc_resp")))
#'   run_batch_bested(models_to_run, user = "lbw")
#'
#'   Option B: Traditional list format
#'   models_to_run <- list(
#'     list(encounter_type = "ED", exposure_category = "high_smoke", cause = "rate_enc"),
#'     list(encounter_type = "ED", exposure_category = "mid_smoke", cause = "rate_enc_resp")
#'   )
#'   run_batch_bested(models_to_run, user = "lbw")
#'
#' Example 2: Run all ED models with all exposure categories and causes
#'   models_to_run <- generate_model_combinations(
#'     encounter_type = "ED",
#'     exposure_category = c("high_smoke", "mid_smoke", "none"),
#'     cause = c("rate_enc", "rate_enc_resp", "rate_enc_cardio", "rate_enc_injury", "rate_enc_neuro")
#'   )
#'   run_batch_bested(models_to_run, user = "lbw")
#'
#' Example 3: Run all models for a specific exposure category
#'   models_to_run <- generate_model_combinations(
#'     encounter_type = "ED",
#'     exposure_category = "high_smoke",
#'     cause = c("rate_enc", "rate_enc_resp", "rate_enc_cardio", "rate_enc_injury", "rate_enc_neuro")
#'   )
#'   run_batch_bested(models_to_run, user = "lbw")
#'
#' Example 4: Run ALL bested models with diff number of sims
#'   models_to_run <- get_all_bested_models()  # Gets all models from bested folder (user-agnostic)
#'   run_batch_bested(models_to_run, user = "lbw", n_sim_mbb = 500, train_test_date = "2025-12-30")
#'   # user determines where outputs are saved
#'   # train_test_date avoids prompting for data date multiple times
#'
generate_model_combinations <- function(encounter_type, exposure_category, cause) {
  expand.grid(
    encounter_type = encounter_type,
    exposure_category = exposure_category,
    cause = cause,
    stringsAsFactors = FALSE
  ) |>
    purrr::transpose() |>
    purrr::map(~ as.list(.x))
}

#' Create model list from parameters
#'
#' Helper function to create model list more easily. Can be used in two ways:
#' 1. With separate vectors: make_models("ED", c("high_smoke", "mid_smoke"), c("rate_enc", "rate_enc_resp"))
#' 2. With list of pairs: make_models("ED", list(c("high_smoke", "rate_enc"), c("mid_smoke", "rate_enc_resp")))
#'
#' @param encounter_type Character string for encounter type
#' @param exposure_category Either a character vector of exposure categories, or a list of (exposure, cause) pairs
#' @param cause Optional character vector of causes. If NULL, exposure_category must be a list of pairs
#'
#' @return List of lists, where each inner list contains encounter_type, exposure_category, and cause
#'
#' @examples
#' # Option 1: Separate vectors
#' models <- make_models("ED", c("high_smoke", "mid_smoke"), c("rate_enc", "rate_enc_resp"))
#'
#' # Option 2: Using a List
#' models <- make_models("ED", list(c("high_smoke", "rate_enc"), c("mid_smoke", "rate_enc_resp")))
#'
make_models <- function(encounter_type, exposure_category, cause = NULL) {
  # If cause is NULL, assume exposure_category is a list of (exposure, cause) pairs
  if (is.null(cause)) {
    if (!is.list(exposure_category)) {
      stop("If cause is NULL, exposure_category must be a list of (exposure, cause) pairs")
    }
    models <- lapply(exposure_category, function(x) {
      list(encounter_type = encounter_type, exposure_category = x[1], cause = x[2])
    })
    return(models)
  }
  
  # Otherwise use generate_model_combinations
  return(generate_model_combinations(encounter_type, exposure_category, cause))
}

#-------------------------------
# Batch Execution Functions
#-------------------------------

#' Run batch of bested models
#'
#' Runs a batch of best-tuned models using configs from the bested folder.
#' Verifies that configs exist before running, and handles errors gracefully
#' to continue with remaining models.
#'
#' @param models_to_run List of model specifications. Each element should be a list
#'                      with encounter_type, exposure_category, and cause.
#' @param user User name (lbw or akd). Determines where outputs are saved.
#'             If NULL, prompts for user input.
#' @param n_sim_mbb Optional number of MBB simulations to override config default
#' @param train_test_date Optional date string (YYYY-MM-DD) to avoid prompting multiple times
#' @param path_onedrive Path to OneDrive directory (must be set in calling environment)
#' @param bested_dir Directory name within models directory to read configs from.
#'                   Default is "bested_final". Should not include trailing slash.
#' @param ci_method Optional CI method override. Options: "quantile" or "symmetric_sd".
#'                  If NULL, uses value from config file.
#' @param ensure_nonnegative Optional override for ensure_nonnegative setting.
#'                           If NULL, uses value from config file.
#' @param model_path Optional path to models directory. If NULL, computed from user parameter.
#'                   This overrides the config$user setting for output location.
#'
#' @return Invisibly returns a list with:
#'   - valid_models: List of models that were successfully run
#'   - missing_configs: List of models that were skipped (missing configs)
#'   - errors: List of any errors encountered
#'
#' @examples
#' # Run specific models
#' models_to_run <- list(
#'   list(encounter_type = "ED", exposure_category = "high_smoke", cause = "rate_enc"),
#'   list(encounter_type = "ED", exposure_category = "mid_smoke", cause = "rate_enc_resp")
#' )
#' run_batch_bested(models_to_run, user = "lbw")
#'
#' # Run all bested models with custom directory and CI settings
#' models_to_run <- get_all_bested_models()
#' run_batch_bested(models_to_run, user = "lbw", n_sim_mbb = 500, 
#'                  train_test_date = "2025-12-30", bested_dir = "bested",
#'                  ci_method = "symmetric_sd", ensure_nonnegative = TRUE)
#'
run_batch_bested <- function(models_to_run, user = NULL, n_sim_mbb = NULL, 
                             train_test_date = NULL, path_onedrive = NULL, 
                             bested_dir = NULL, ci_method = NULL, 
                             ensure_nonnegative = NULL, model_path = NULL) {
  # Get path_onedrive from environment if not provided
  if (is.null(path_onedrive)) {
    if (!exists("path_onedrive", envir = .GlobalEnv)) {
      stop("path_onedrive must be provided or set in global environment")
    }
    path_onedrive <- get("path_onedrive", envir = .GlobalEnv)
  }
  
  # Set train_test_date in environment if provided (before calling run_pipeline)
  if (!is.null(train_test_date)) {
    Sys.setenv(TRAIN_TEST_DATE = train_test_date)
    cat("Setting TRAIN_TEST_DATE to:", train_test_date, "\n")
    # Clean up TRAIN_TEST_DATE env var on exit if we set it
    on.exit({
      if (Sys.getenv("TRAIN_TEST_DATE", unset = "") != "") {
        Sys.unsetenv("TRAIN_TEST_DATE")
      }
    }, add = TRUE)
  }
  
  # Prompt for user if not provided
  if (is.null(user)) {
    user <- readline(prompt = "Enter user name (lbw or akd): ")
  }
  
  # Compute model_path from user if not provided
  if (is.null(model_path)) {
    model_path <- paste0(path_onedrive, "03_modeling-and-results/01_modeling/", user, "/")
    cat("Using model path (from user):", model_path, "\n")
  } else {
    cat("Using model path (provided):", model_path, "\n")
  }
  
  # Validate models_to_run
  if (length(models_to_run) == 0) {
    stop("models_to_run cannot be empty")
  }

  cat("Running", length(models_to_run), "bested model(s)\n\n")
  
  #-------------------------------
  # Step 2: Verify configs exist in bested folder
  #-------------------------------
  cat("Verifying configs...\n")
  bested_dir_path <- paste0(bested_dir, "/")
  configs_dir_path <- paste0(bested_dir, "/configs/")
  
  # Check if directory exists
  if (!dir.exists(bested_dir_path)) {
    stop("Bested directory does not exist: ", bested_dir_path)
  }
  
  # Check if configs subdirectory exists
  if (!dir.exists(configs_dir_path)) {
    stop("Configs directory does not exist: ", configs_dir_path)
  }

  missing_configs <- list()
  valid_models <- list()
  valid_config_paths <- list()
  
  # Get all config files in directory for debugging
  all_configs <- list.files(configs_dir_path, pattern = "_config\\.yaml$", full.names = FALSE)
  
  for (i in seq_along(models_to_run)) {
    model <- models_to_run[[i]]
    
    # Find matching config file
    # Pattern must match exactly to avoid matching rate_enc_cardio when looking for rate_enc
    cause_escaped <- gsub("([.|()\\^{}+$*?\\[\\\\])", "\\\\\\1", model$cause)
    pattern <- paste0("^", model$encounter_type, "_", 
                      model$exposure_category, "_", 
                      cause_escaped, "_(akd|lbw)_model_run_.*_config\\.yaml$")
    config_files <- list.files(configs_dir_path, pattern = pattern, full.names = TRUE)
    
    if (length(config_files) == 0) {
      missing_configs[[length(missing_configs) + 1]] <- model
      cat("  ✗ Missing:", model$encounter_type, "-", model$exposure_category, "-", model$cause, "\n")
      cat("     Pattern:", pattern, "\n")
    } else if (length(config_files) > 1) {
      cat("  ⚠ Multiple configs found for:", model$encounter_type, "-", model$exposure_category, "-", model$cause, "\n")
      cat("     Using:", basename(config_files[1]), "\n")
      valid_models[[length(valid_models) + 1]] <- model
      valid_config_paths[[length(valid_config_paths) + 1]] <- config_files[1]
    } else {
      cat("  ✓ Found:", basename(config_files[1]), "\n")
      valid_models[[length(valid_models) + 1]] <- model
      valid_config_paths[[length(valid_config_paths) + 1]] <- config_files[1]
    }
  }
  
  if (length(missing_configs) > 0) {
    cat("\n⚠ Warning:", length(missing_configs), "model(s) missing configs in bested folder\n")
    cat("Looking in:", configs_dir_path, "\n")
    if (length(all_configs) > 0) {
      cat("Available config files (first 20):\n")
      for (cfg in head(all_configs, 20)) {
        cat("  -", cfg, "\n")
      }
      if (length(all_configs) > 20) {
        cat("  ... and", length(all_configs) - 20, "more\n")
      }
    } else {
      cat("No config files found in directory.\n")
    }
    cat("These will be skipped.\n\n")
  }
  
  if (length(valid_models) == 0) {
    stop("No valid models found in bested folder: ", configs_dir_path,
         if(length(all_configs) > 0) paste0("\nFound ", length(all_configs), " config file(s) but none matched the requested models.") else "\nNo config files found in directory.")
  }
  
  cat("\nFound", length(valid_models), "valid model(s) to run\n\n")
  
  #-------------------------------
  # Step 3: Run pipeline for each model
  #-------------------------------
  
  # Source 00_run_all.R to get the run_pipeline function
  # Check if run_pipeline exists, if not source it
  if (!exists("run_pipeline")) {
    source(paste0(getwd(), "/01_code/02_analysis/00_run_all.R"))
  }
  
  errors <- list()
  successful_models <- list()
  
  for (i in seq_along(valid_models)) {
    model <- valid_models[[i]]
    config_path <- valid_config_paths[[i]]
    
    cat("\n[", i, "/", length(valid_models), "] ", 
        model$encounter_type, " - ", model$exposure_category, " - ", model$cause, "\n", sep = "")
    
    # Run the pipeline using the config path we already found
    tryCatch({
      run_pipeline(config_path = config_path, user = user, n_sim_mbb = n_sim_mbb, 
                   train_test_date = train_test_date, ci_method = ci_method, 
                   ensure_nonnegative = ensure_nonnegative, model_path = model_path)
      cat("\n✓ Successfully completed model", i, "of", length(valid_models), "\n")
      successful_models[[length(successful_models) + 1]] <- model
    }, error = function(e) {
      cat("\n✗ ERROR in model", i, "of", length(valid_models), ":\n")
      cat("  ", as.character(e), "\n")
      cat("  Continuing to next model...\n")
      errors[[length(errors) + 1]] <- list(model = model, error = as.character(e))
    })
  }
  
  cat("\n=== Batch Complete ===\n")
  cat("Successfully ran:", length(successful_models), "model(s)\n")
  if (length(missing_configs) > 0) {
    cat("Skipped:", length(missing_configs), "model(s) (missing configs)\n")
  }
  if (length(errors) > 0) {
    cat("Errors:", length(errors), "model(s)\n")
  }
  
  # Return results invisibly
  invisible(list(
    valid_models = successful_models,
    missing_configs = missing_configs,
    errors = errors
  ))
}

