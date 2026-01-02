#-------------------------------
# LA wildfires project
# Moving Block Bootstrap utility functions
# Adapted for tidymodels prophet_boost workflows
#-------------------------------

#' Generate Confidence Intervals using Moving Block Bootstrap for tidymodels workflows
#'
#' This function uses a base seed that gets propagated to parallel workers via furrr_options(seed = TRUE).
#' Each bootstrap iteration uses a deterministic seed derived as (base_seed + iteration_number) to ensure
#' reproducibility across parallel executions while maintaining independence between iterations.
#'
#' @param wflw_fit Fitted tidymodels workflow object (prophet_boost)
#' @param train_df Training data with columns: date, outcome variable, and predictors
#' @param target_df Target data for predictions (e.g., test or holdout)
#' @param outcome_col Name of the outcome column (e.g., "num_enc_resp")
#' @param n_sim Number of bootstrap simulations (default: 1000)
#' @param L_block Block length for MBB (default: 14)
#' @param seed Base seed for reproducibility (used as base_seed + i for each iteration)
#' @param rec_obj_unfitted Optional unfitted recipe object (to avoid trained recipe issues)
#' @param model_spec Optional model specification (to avoid XGBoost serialization issues)
#'
#' @return List containing:
#'   - pred_matrix: Matrix of predictions (n_rows x n_sim)
#'   - pred_summary: Tibble with date, actual, predicted, conf_lo, conf_hi
#'   - block_length: Block length used
#'
generate_MBB_CIs_tidymodels <- function(wflw_fit,
                                        train_df,
                                        target_df,
                                        outcome_col,
                                        n_sim = 1000,
                                        L_block = 14,
                                        seed = 123,
                                        rec_obj_unfitted = NULL,
                                        model_spec = NULL) {
  
  # NOTE: We do NOT call set.seed() here because:
  # 1. Parallel workers manage their own RNG state via furrr_options(seed = TRUE)
  # 2. Each bootstrap iteration explicitly sets seed as (base_seed + i) for reproducibility
  # 3. This approach ensures both reproducibility and proper parallel execution
  
  # Load required packages
  if (!requireNamespace("boot", quietly = TRUE)) {
    install.packages("boot")
  }
  library(boot)
  
  # Get fitted values on training data
  fitted_values <- suppressWarnings({
    suppressMessages({
      predict(wflw_fit, new_data = train_df)$.pred
    })
  })
  
  # Compute residuals
  residuals_vec <- train_df[[outcome_col]] - fitted_values
  
  # Initialize prediction matrix
  pred_matrix <- matrix(NA, nrow = nrow(target_df), ncol = n_sim)
  
  # Helper function for tsboot
  boot_resid_fn <- function(resid, i) {
    resid[i]
  }
  
  # Extract recipe and model spec for creating fresh workflows
  # This avoids XGBoost serialization issues when refitting
  # Use provided unfitted components if available, otherwise extract from fitted workflow
  if (is.null(rec_obj_unfitted)) {
    recipe_obj <- workflows::extract_recipe(wflw_fit)
  } else {
    recipe_obj <- rec_obj_unfitted
  }
  
  if (is.null(model_spec)) {
    model_spec <- workflows::extract_spec_parsnip(wflw_fit)
  }
  
  # Run bootstrap simulations in parallel
  cat("Running", n_sim, "MBB simulations with block length =", L_block, "in parallel...\n")
  
  # Helper function for a single bootstrap iteration
  # Pass all required variables explicitly to ensure they're available in parallel workers
  run_bootstrap_iteration <- function(i, 
                                      residuals_vec, 
                                      fitted_values, 
                                      train_df, 
                                      target_df, 
                                      outcome_col, 
                                      model_spec, 
                                      recipe_obj, 
                                      L_block, 
                                      boot_resid_fn,
                                      base_seed) {
    # Ensure required packages are loaded in parallel worker
    # Use library() to actually load packages (requireNamespace only checks availability)
    if (!require("boot", quietly = TRUE, character.only = TRUE)) {
      stop("boot package not available in parallel worker")
    }
    if (!require("workflows", quietly = TRUE, character.only = TRUE)) {
      stop("workflows package not available in parallel worker")
    }
    if (!require("tidymodels", quietly = TRUE, character.only = TRUE)) {
      stop("tidymodels package not available in parallel worker")
    }
    # Load modeltime for prophet_boost and prophet_xgboost_fit_impl
    if (!require("modeltime", quietly = TRUE, character.only = TRUE)) {
      stop("modeltime package not available in parallel worker")
    }
    # Also ensure dplyr is loaded (needed for pull())
    if (!require("dplyr", quietly = TRUE, character.only = TRUE)) {
      stop("dplyr package not available in parallel worker")
    }
    # Load lubridate for date functions (month(), year(), etc.) used in recipes
    if (!require("lubridate", quietly = TRUE, character.only = TRUE)) {
      stop("lubridate package not available in parallel worker")
    }
    # Load timetk for time series functions used in recipes
    if (!require("timetk", quietly = TRUE, character.only = TRUE)) {
      stop("timetk package not available in parallel worker")
    }
    
    # CRITICAL: Set seed for this iteration using (base_seed + iteration_number)
    # This ensures each bootstrap iteration is:
    # 1. Reproducible - same iteration always gets same seed
    # 2. Independent - different iterations get different seeds
    # 3. Deterministic across parallel runs - iteration order doesn't matter
    set.seed(base_seed + i)
    
    tryCatch({
      # Generate bootstrapped residual series
      boot_res <- boot::tsboot(
        tseries = residuals_vec,
        statistic = boot_resid_fn,
        R = 1,
        l = L_block,
        sim = "fixed",  # Moving block bootstrap
        endcorr = TRUE
      )
      resid_star <- as.numeric(boot_res$t)
      
      # Form bootstrapped response on training set
      y_star <- fitted_values + resid_star
      train_df_star <- train_df
      # Ensure y_star has the same type as the original outcome variable
      original_type <- class(train_df[[outcome_col]])[1]
      if (original_type == "integer") {
        train_df_star[[outcome_col]] <- as.integer(round(y_star))
      } else {
        train_df_star[[outcome_col]] <- y_star
      }
      
      # Create fresh workflow for each bootstrap iteration to avoid XGBoost issues
      # Build a completely fresh workflow from extracted components
      wflw_fresh <- workflows::workflow() %>%
        workflows::add_model(model_spec) %>%
        workflows::add_recipe(recipe_obj)
      
      # Fit and predict in suppressMessages/suppressWarnings
      wflw_fitted <- suppressWarnings({
        suppressMessages({
          fit(wflw_fresh, train_df_star)
        })
      })
      
      preds <- suppressWarnings({
        suppressMessages({
          pred_df <- predict(wflw_fitted, new_data = target_df)
          if (inherits(pred_df, "data.frame") || inherits(pred_df, "tbl")) {
            preds <- pred_df$.pred
          } else {
            preds <- as.numeric(pred_df)
          }
          preds
        })
      })
      
      # Validate predictions
      if (is.null(preds) || length(preds) != nrow(target_df)) {
        stop("Predictions are NULL or wrong length. Expected ", nrow(target_df), " got ", length(preds))
      }
      
      # Explicitly remove the fitted workflow to free XGBoost resources
      rm(wflw_fitted, wflw_fresh)
      
      # Return predictions
      return(preds)
      
    }, error = function(e) {
      # Return error information along with NA vector for debugging
      error_msg <- paste0("Iteration ", i, ": ", as.character(e))
      na_result <- rep(NA_real_, nrow(target_df))
      attr(na_result, "error") <- error_msg
      return(na_result)
    })
  }
  
  # Create a wrapper function that captures all required variables
  # This ensures all variables are properly available in parallel workers
  run_iteration_wrapper <- function(i) {
    run_bootstrap_iteration(
      i = i,
      residuals_vec = residuals_vec,
      fitted_values = fitted_values,
      train_df = train_df,
      target_df = target_df,
      outcome_col = outcome_col,
      model_spec = model_spec,
      recipe_obj = recipe_obj,
      L_block = L_block,
      boot_resid_fn = boot_resid_fn,
      base_seed = seed
    )
  }
  
  # Run bootstrap simulations in parallel with progress reporting
  # Use furrr_options with seed = TRUE to ensure reproducibility
  # Wrap in tryCatch to fall back to sequential processing if parallel fails
  bootstrap_results <- tryCatch({
    if (!requireNamespace("progressr", quietly = TRUE)) {
      # Fallback if progressr not available
      cat("Note: progressr not available, running without progress bar\n")
      furrr::future_map(
        1:n_sim, 
        run_iteration_wrapper,
        .options = furrr::furrr_options(seed = TRUE)
      )
    } else {
      # Use progressr for progress reporting
      progressr::handlers(progressr::handler_progress(
        format = "[:bar] :percent :current/:total ETA: :eta",
        clear = TRUE,
        width = 60
      ))
      
      with_progress({
        p <- progressr::progressor(steps = n_sim)
        furrr::future_map(
          1:n_sim, 
          function(i) {
            result <- run_iteration_wrapper(i)
            p()
            return(result)
          },
          .options = furrr::furrr_options(seed = TRUE)
        )
      })
    }
  }, error = function(e) {
    # If parallel execution fails, fall back to sequential processing
    warning("\nParallel execution failed with error: ", as.character(e), 
            "\nFalling back to sequential processing...\n", call. = FALSE)
    
    # Run sequentially using lapply
    cat("Running", n_sim, "MBB simulations sequentially...\n")
    lapply(1:n_sim, function(i) {
      if (i %% 10 == 0) cat("  Completed", i, "of", n_sim, "simulations\n")
      run_iteration_wrapper(i)
    })
  })
  
  # Collect results into prediction matrix and gather error messages
  error_messages <- character()
  for (i in 1:n_sim) {
    result <- bootstrap_results[[i]]
    pred_matrix[, i] <- result
    
    # Check for error attribute
    if (!is.null(attr(result, "error"))) {
      error_messages <- c(error_messages, attr(result, "error"))
    }
  }
  
  # Check for successful iterations
  successful_cols <- apply(pred_matrix, 2, function(x) !all(is.na(x)))
  n_successful <- sum(successful_cols)
  n_failed <- n_sim - n_successful
  
  if (n_failed > 0) {
    cat("Warning:", n_failed, "out of", n_sim, "bootstrap iterations failed\n")
    if (length(error_messages) > 0) {
      cat("Sample error messages (showing first 3):\n")
      for (i in 1:min(3, length(error_messages))) {
        cat("  ", error_messages[i], "\n")
      }
      if (length(error_messages) > 3) {
        cat("  ... and", length(error_messages) - 3, "more errors\n")
      }
    }
  }
  
  # Check if we have any valid predictions
  if (all(is.na(pred_matrix))) {
    cat("\nERROR: All bootstrap iterations failed. Error details:\n")
    if (length(error_messages) > 0) {
      cat("All error messages:\n")
      for (msg in error_messages) {
        cat("  ", msg, "\n")
      }
    } else {
      cat("No error messages captured. This suggests a serialization or package loading issue.\n")
      cat("Trying a test iteration to diagnose the problem...\n")
      # Try running one iteration sequentially to see the actual error
      tryCatch({
        test_result <- run_bootstrap_iteration(
          1,
          residuals_vec = residuals_vec,
          fitted_values = fitted_values,
          train_df = train_df,
          target_df = target_df,
          outcome_col = outcome_col,
          model_spec = model_spec,
          recipe_obj = recipe_obj,
          L_block = L_block,
          boot_resid_fn = boot_resid_fn,
          base_seed = seed
        )
        cat("Sequential test iteration succeeded. Issue is likely with parallel execution.\n")
      }, error = function(e) {
        cat("Sequential test iteration also failed:\n")
        cat("  ", as.character(e), "\n")
      })
    }
    stop("All bootstrap iterations failed. No valid predictions generated.")
  }
  
  # Clean up
  rm(bootstrap_results)
  gc(verbose = FALSE)
  
  cat("Bootstrap simulations complete!\n")
  cat("  Successful iterations:", sum(apply(pred_matrix, 2, function(x) !all(is.na(x)))), "out of", n_sim, "\n")
  
  # Get point estimates from original fitted model
  original_pred <- suppressWarnings({
    suppressMessages({
      predict(wflw_fit, new_data = target_df)$.pred
    })
  })
  
  # Validate original predictions
  if (is.null(original_pred) || all(is.na(original_pred)) || all(is.nan(original_pred))) {
    stop("Original model predictions are invalid. Check fitted workflow.")
  }
  
  # Compute summary with confidence intervals
  pred_summary <- tibble::tibble(
    ds = as.Date(target_df$date),
    y_actual = target_df[[outcome_col]],
    yhat = original_pred,
    # CIs using quantile approach; will lead to asymmetric CIs
    # conf_lo = apply(pred_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
    # conf_hi = apply(pred_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
    # CIs using SD of bootstrap distribution; will lead to symmetric CIs
    bootstrap_sd = apply(pred_matrix, 1, sd, na.rm = TRUE),
    conf_lo = original_pred - 1.96 * bootstrap_sd,
    conf_hi = original_pred + 1.96 * bootstrap_sd
  ) |>
  dplyr::select(-bootstrap_sd)  # Remove intermediate column
  
  cat("  Point estimates from original model: ", sum(!is.na(pred_summary$yhat)), "out of", nrow(pred_summary), "\n")
  
  # Return results
  return(list(
    pred_matrix = pred_matrix,
    pred_summary = pred_summary,
    block_length = L_block
  ))
}


#' Generate simple predictions without bootstrap (for training set)
#'
#' @param wflw_fit Fitted tidymodels workflow object
#' @param data_df Data to predict on
#' @param outcome_col Name of the outcome column
#'
#' @return Tibble with date, actual, predicted values
#'
generate_simple_predictions <- function(wflw_fit, data_df, outcome_col) {
  
  preds <- suppressWarnings({
    suppressMessages({
      predict(wflw_fit, new_data = data_df)$.pred
    })
  })
  
  result <- tibble::tibble(
    ds = as.Date(data_df$date),
    y_actual = data_df[[outcome_col]],
    yhat = preds,
    conf_lo = NA_real_,  # No CIs for training fit
    conf_hi = NA_real_
  )
  
  return(result)
}


#' Rebuild and fit workflow from saved components (avoids XGBoost serialization issues)
#'
#' @param result Tuning result object containing recipe and best_params
#' @param train_df Training data
#' @param global_seed Global seed for reproducibility
#' @param enc Encounter type
#' @param exposure Exposure category
#' @param cause Cause (outcome variable)
#'
#' @return Fitted workflow object
#'
rebuild_and_fit_workflow <- function(result, train_df, global_seed, enc, exposure, cause) {
  
  # Load required packages
  if (!requireNamespace("modeltime", quietly = TRUE)) {
    stop("modeltime package required but not installed")
  }
  if (!requireNamespace("tidymodels", quietly = TRUE)) {
    stop("tidymodels package required but not installed")
  }
  
  library(modeltime)
  library(tidymodels)
  
  # Extract components from result
  rec_obj_phxgb <- result$rec_obj_phxgb
  best_params <- result$best_params
  
  # Create fresh model specification with best parameters
  # (avoids XGBoost serialization issues)
  model_seed <- digest::digest(paste(enc, exposure, cause, "model_phxgb_rebuild"), 
                                algo = "xxhash32", seed = global_seed)
  model_seed_int <- as.integer(paste0("0x", substr(model_seed, 1, 6)), 16)
  
  # Build model arguments conditionally - only include parameters that were tuned
  model_args <- list(
    mode = "regression",
    growth = "linear",
    seasonality_yearly = FALSE
  )
  
  # Add parameters only if they exist in best_params (i.e., were tuned)
  if ("mtry" %in% names(best_params)) model_args$mtry <- best_params$mtry
  if ("trees" %in% names(best_params)) model_args$trees <- best_params$trees
  if ("min_n" %in% names(best_params)) model_args$min_n <- best_params$min_n
  if ("tree_depth" %in% names(best_params)) model_args$tree_depth <- best_params$tree_depth
  if ("learn_rate" %in% names(best_params)) model_args$learn_rate <- best_params$learn_rate
  if ("loss_reduction" %in% names(best_params)) model_args$loss_reduction <- best_params$loss_reduction
  if ("stop_iter" %in% names(best_params)) model_args$stop_iter <- best_params$stop_iter
  if ("sample_size" %in% names(best_params)) model_args$sample_size <- best_params$sample_size
  if ("changepoint_num" %in% names(best_params)) model_args$changepoint_num <- best_params$changepoint_num
  if ("changepoint_range" %in% names(best_params)) model_args$changepoint_range <- best_params$changepoint_range
  if ("prior_scale_changepoints" %in% names(best_params)) model_args$prior_scale_changepoints <- best_params$prior_scale_changepoints
  
  model_phxgb_final <- do.call(prophet_boost, model_args) %>%
    set_engine("prophet_xgboost",
               seed = model_seed_int,
               early_stop = TRUE,
               validation = 0.2)
  
  # Create new workflow with fresh components
  wflw_final <- workflow() %>%
    add_model(model_phxgb_final) %>%
    add_recipe(rec_obj_phxgb)
  
  # Fit on training data using withr::with_seed() to manage RNG state
  wflw_fit_seed <- digest::digest(paste(enc, exposure, cause, "wflw_fit_rebuild"), 
                                   algo = "xxhash32", seed = global_seed)
  wflw_fit_seed_int <- as.integer(paste0("0x", substr(wflw_fit_seed, 1, 6)), 16)
  
  wflw_fit <- withr::with_seed(wflw_fit_seed_int, {
    suppressWarnings({
      suppressMessages({
        fit(wflw_final, train_df)
      })
    })
  })
  
  return(wflw_fit)
}

