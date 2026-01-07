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
#' Option A: Using make_models helper (easier when encounter_type is the same)
#' models_to_run <- make_models("ED", list(c("high_smoke", "rate_enc"), c("mid_smoke", "rate_enc_resp")))
#' run_batch_bested(models_to_run, user = "lbw")
#'
# Example 2: Run all ED models with all exposure categories and causes
#' models_to_run <- generate_model_combinations(encounter_type = "ED", exposure_category = c("high_smoke", "mid_smoke", "none"), cause = c("rate_enc", "rate_enc_resp", "rate_enc_cardio", "rate_enc_injury", "rate_enc_neuro"))
#' run_batch_bested(models_to_run, user = "lbw")
#'
# Example 3: Run all models for a specific exposure category
#' models_to_run <- generate_model_combinations(encounter_type = "ED", exposure_category = "high_smoke", cause = c("rate_enc", "rate_enc_resp", "rate_enc_cardio", "rate_enc_injury", "rate_enc_neuro"))
#' run_batch_bested(models_to_run, user = "lbw")
#'
# Example 4: Run ALL bested models with diff number of sims
#' models_to_run <- get_all_bested_models()  # Gets all models from bested folder (user-agnostic)
#' run_batch_bested(models_to_run, user = "lbw", n_sim_mbb = 500, train_test_date = "2025-12-30")
#' # user determines where outputs are saved
#' # train_test_date avoids prompting for data date multiple times
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
#' # Option 2: List of pairs
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
# Model Discovery Functions
#-------------------------------

#' Get all models from bested folder
#'
#' Scans the bested folder for config files and extracts unique model combinations.
#' User-agnostic - finds models regardless of which user created them.
#'
#' @param path_onedrive Path to OneDrive directory (must be set in calling environment)
#'
#' @return List of lists, where each inner list contains encounter_type, exposure_category, and cause
#'
#' @examples
#' models <- get_all_bested_models()
#' run_batch_bested(models, user = "lbw")
#'
get_all_bested_models <- function(path_onedrive = NULL) {
  # Get path_onedrive from environment if not provided
  if (is.null(path_onedrive)) {
    if (!exists("path_onedrive", envir = .GlobalEnv)) {
      stop("path_onedrive must be provided or set in global environment")
    }
    path_onedrive <- get("path_onedrive", envir = .GlobalEnv)
  }
  
  models_dir <- paste0(path_onedrive, "02_output/models/")
  bested_dir <- paste0(models_dir, "bested/")
  
  if (!dir.exists(bested_dir)) {
    stop("Bested directory does not exist: ", bested_dir)
  }
  
  # Get all config files
  config_files <- list.files(bested_dir, pattern = "_config\\.yaml$", full.names = FALSE)
  
  if (length(config_files) == 0) {
    stop("No config files found in bested directory")
  }
  
  # Extract model combinations from filenames
  # Pattern: ED_high_smoke_rate_enc_akd_model_run_2026-01-02.v006_x20_sim100_config.yaml
  models <- list()
  for (config_file in config_files) {
    # Remove _config.yaml suffix
    base_name <- sub("_config\\.yaml$", "", config_file)
    # Split by underscores
    parts <- strsplit(base_name, "_")[[1]]
    
    # Find where user name appears (akd or lbw)
    user_idx <- which(parts %in% c("akd", "lbw"))
    if (length(user_idx) == 0) next
    
    # Everything before user_idx is: enc_type, exposure_category, cause
    if (user_idx[1] >= 4) {
      encounter_type <- parts[1]
      exposure_category <- parts[2]
      # Cause might be multiple parts (e.g., rate_enc_cardio)
      cause <- paste(parts[3:(user_idx[1]-1)], collapse = "_")
      
      models[[length(models) + 1]] <- list(
        encounter_type = encounter_type,
        exposure_category = exposure_category,
        cause = cause
      )
    }
  }
  
  # Remove duplicates
  models_df <- do.call(rbind, lapply(models, function(x) {
    data.frame(encounter_type = x$encounter_type,
               exposure_category = x$exposure_category,
               cause = x$cause,
               stringsAsFactors = FALSE)
  }))
  models_df <- unique(models_df)
  
  # Convert back to list of lists
  models_list <- lapply(1:nrow(models_df), function(i) {
    list(
      encounter_type = models_df$encounter_type[i],
      exposure_category = models_df$exposure_category[i],
      cause = models_df$cause[i]
    )
  })
  
  return(models_list)
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
#' # Run all bested models
#' models_to_run <- get_all_bested_models()
#' run_batch_bested(models_to_run, user = "lbw", n_sim_mbb = 500, train_test_date = "2025-12-30")
#'
run_batch_bested <- function(models_to_run, user = NULL, n_sim_mbb = NULL, train_test_date = NULL, path_onedrive = NULL) {
  # Get path_onedrive from environment if not provided
  if (is.null(path_onedrive)) {
    if (!exists("path_onedrive", envir = .GlobalEnv)) {
      stop("path_onedrive must be provided or set in global environment")
    }
    path_onedrive <- get("path_onedrive", envir = .GlobalEnv)
  }
  
  # Clean up TRAIN_TEST_DATE env var on exit if we set it
  if (!is.null(train_test_date)) {
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
  
  # Validate models_to_run
  if (length(models_to_run) == 0) {
    stop("models_to_run cannot be empty")
  }

  cat("Running", length(models_to_run), "bested model(s)\n\n")
  
  #-------------------------------
  # Step 2: Verify configs exist in bested folder
  #-------------------------------
  cat("Verifying configs...\n")
  models_dir <- paste0(path_onedrive, "02_output/models/")
  bested_dir <- paste0(models_dir, "bested/")

  missing_configs <- list()
  valid_models <- list()
  
  for (i in seq_along(models_to_run)) {
    model <- models_to_run[[i]]
    
    # Find matching config file
    # Pattern must match exactly to avoid matching rate_enc_cardio when looking for rate_enc
    cause_escaped <- gsub("([.|()\\^{}+$*?\\[\\\\])", "\\\\\\1", model$cause)
    pattern <- paste0("^", model$encounter_type, "_", 
                      model$exposure_category, "_", 
                      cause_escaped, "_(akd|lbw)_model_run_.*_config\\.yaml$")
    config_files <- list.files(bested_dir, pattern = pattern, full.names = TRUE)
    
    if (length(config_files) == 0) {
      missing_configs[[length(missing_configs) + 1]] <- model
      cat("  ✗ Missing:", model$encounter_type, "-", model$exposure_category, "-", model$cause, "\n")
    } else if (length(config_files) > 1) {
      cat("  ⚠ Multiple configs found for:", model$encounter_type, "-", model$exposure_category, "-", model$cause, "\n")
      cat("     Using:", basename(config_files[1]), "\n")
      valid_models[[length(valid_models) + 1]] <- model
    } else {
      cat("  ✓ Found:", basename(config_files[1]), "\n")
      valid_models[[length(valid_models) + 1]] <- model
    }
  }
  
  if (length(missing_configs) > 0) {
    cat("\n⚠ Warning:", length(missing_configs), "model(s) missing configs in bested folder\n")
    cat("These will be skipped.\n\n")
  }
  
  if (length(valid_models) == 0) {
    stop("No valid models found in bested folder")
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
    
    cat("\n[", i, "/", length(valid_models), "] ", 
        model$encounter_type, " - ", model$exposure_category, " - ", model$cause, "\n", sep = "")
    
    # Run the pipeline using bested_model lookup
    tryCatch({
      run_pipeline(bested_model = model, user = user, n_sim_mbb = n_sim_mbb, train_test_date = train_test_date)
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

