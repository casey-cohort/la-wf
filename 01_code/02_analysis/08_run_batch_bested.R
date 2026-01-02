#-------------------------------
# LA wildfires project
# Run a batch of bested models for specified model combinations
# Uses configs from bested folder as-is (no modifications)
#
# USAGE EXAMPLES:
#
# Example 1: Run specific models manually
#   Option A: Using make_models helper (easier when encounter_type is the same)
#   source("01_code/02_analysis/08_run_batch_bested.R")
#   models_to_run <- make_models("ED", 
#     list(c("high_smoke", "rate_enc"), 
#          c("mid_smoke", "rate_enc_resp")))
#   run_batch_bested(models_to_run, user = "lbw")
#
#   Option B: Traditional list format
#   models_to_run <- list(
#     list(encounter_type = "ED", exposure_category = "high_smoke", cause = "rate_enc"),
#     list(encounter_type = "ED", exposure_category = "mid_smoke", cause = "rate_enc_resp")
#   )
#   run_batch_bested(models_to_run, user = "lbw")
#
# Example 2: Run all ED models with all exposure categories and causes
#   source("01_code/02_analysis/08_run_batch_bested.R")
#   models_to_run <- generate_model_combinations(
#     encounter_type = "ED",
#     exposure_category = c("high_smoke", "mid_smoke", "none"),
#     cause = c("rate_enc", "rate_enc_resp", "rate_enc_cardio", "rate_enc_injury", "rate_enc_neuro")
#   )
#   run_batch_bested(models_to_run, user = "lbw")
#
# Example 3: Run all models for a specific exposure category
#   source("01_code/02_analysis/08_run_batch_bested.R")
#   models_to_run <- generate_model_combinations(
#     encounter_type = "ED",
#     exposure_category = "high_smoke",
#     cause = c("rate_enc", "rate_enc_resp", "rate_enc_cardio", "rate_enc_injury", "rate_enc_neuro")
#   )
#   run_batch_bested(models_to_run, user = "lbw")
#
# Example 4: Run ALL bested models with diff number of sims
#   source("01_code/02_analysis/08_run_batch_bested.R")
#   models_to_run <- get_all_bested_models()  # Gets all models from bested folder (user-agnostic)
#   run_batch_bested(models_to_run, user = "lbw", n_sim_mbb = 500, train_test_date = "2025-12-30")
#   # user determines where outputs are saved
#   # train_test_date avoids prompting for data date multiple times
#
#-------------------------------

# Initial Setup ----
pacman::p_load(tidyverse, yaml)

# Set paths
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_general.R"))

# Helper function to generate model combinations
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

# Helper function to create model list more easily
# Usage: make_models("ED", c("high_smoke", "mid_smoke"), c("rate_enc", "rate_enc_resp"))
# Or: make_models("ED", list(c("high_smoke", "rate_enc"), c("mid_smoke", "rate_enc_resp")))
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

# Helper function to get all models from bested folder (user-agnostic)
get_all_bested_models <- function() {
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

# Main function to run batch of bested models
run_batch_bested <- function(models_to_run, user = NULL, n_sim_mbb = NULL, train_test_date = NULL) {
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
  source(paste0(getwd(), "/01_code/02_analysis/00_run_all.R"))
  
  for (i in seq_along(valid_models)) {
    model <- valid_models[[i]]
    
    cat("\n[", i, "/", length(valid_models), "] ", 
        model$encounter_type, " - ", model$exposure_category, " - ", model$cause, "\n", sep = "")
    
    # Run the pipeline using bested_model lookup
    tryCatch({
      run_pipeline(bested_model = model, user = user, n_sim_mbb = n_sim_mbb, train_test_date = train_test_date)
      cat("\n✓ Successfully completed model", i, "of", length(valid_models), "\n")
    }, error = function(e) {
      cat("\n✗ ERROR in model", i, "of", length(valid_models), ":\n")
      cat("  ", as.character(e), "\n")
      cat("  Continuing to next model...\n")
    })
  }
  
  cat("\n=== Batch Complete ===\n")
  cat("Successfully ran:", length(valid_models), "model(s)\n")
  if (length(missing_configs) > 0) {
    cat("Skipped:", length(missing_configs), "model(s) (missing configs)\n")
  }
}

