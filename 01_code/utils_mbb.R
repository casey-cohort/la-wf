#-------------------------------
# LA wildfires project
# Moving Block Bootstrap utility functions
# Adapted for tidymodels prophet_boost workflows
#-------------------------------

#' Generate Confidence Intervals using Moving Block Bootstrap for tidymodels workflows
#'
#' @param wflw_fit Fitted tidymodels workflow object (prophet_boost)
#' @param train_df Training data with columns: date, outcome variable, and predictors
#' @param target_df Target data for predictions (e.g., test or holdout)
#' @param outcome_col Name of the outcome column (e.g., "num_enc_resp")
#' @param n_sim Number of bootstrap simulations (default: 1000)
#' @param L_block Block length for MBB (default: 14)
#' @param seed Random seed for reproducibility
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
  
  set.seed(seed)
  
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
  
  # Run bootstrap simulations
  cat("Running", n_sim, "MBB simulations with block length =", L_block, "...\n")
  
  for (i in 1:n_sim) {
    if (i %% 100 == 0) cat("  Simulation", i, "of", n_sim, "\n")
    
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
    tryCatch({
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
          predict(wflw_fitted, new_data = target_df) %>%
            pull(.pred)
        })
      })
      
      pred_matrix[, i] <- preds
      
      # Explicitly remove the fitted workflow to free XGBoost resources
      rm(wflw_fitted, wflw_fresh)
      
      # Periodic garbage collection to clean up XGBoost objects
      if (i %% 10 == 0) gc(verbose = FALSE)
      
    }, error = function(e) {
      # If error, try to continue with next iteration
      cat("  Warning: Iteration", i, "failed:", as.character(e), "\n")
      # Fill with NA for this iteration
      pred_matrix[, i] <- NA_real_
    })
  }
  
  cat("Bootstrap simulations complete!\n")
  
  # Get central estimate from the bootstrap distribution (mean of bootstrap predictions)
  central_pred <- apply(pred_matrix, 1, mean, na.rm = TRUE)
  
  # Compute summary with confidence intervals
  pred_summary <- tibble::tibble(
    ds = as.Date(target_df$date),
    y_actual = target_df[[outcome_col]],
    yhat = central_pred,
    conf_lo = apply(pred_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
    conf_hi = apply(pred_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
  )
  
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
  
  model_phxgb_final <- prophet_boost(
    mode = "regression",
    growth = "linear",
    seasonality_yearly = FALSE,
    mtry = best_params$mtry,
    min_n = best_params$min_n,
    tree_depth = best_params$tree_depth,
    learn_rate = best_params$learn_rate,
    loss_reduction = best_params$loss_reduction,
    stop_iter = best_params$stop_iter
  ) %>%
    set_engine("prophet_xgboost",
               seed = model_seed_int,
               early_stop = TRUE,
               validation = 0.2)
  
  # Create new workflow with fresh components
  wflw_final <- workflow() %>%
    add_model(model_phxgb_final) %>%
    add_recipe(rec_obj_phxgb)
  
  # Fit on training data
  wflw_fit_seed <- digest::digest(paste(enc, exposure, cause, "wflw_fit_rebuild"), 
                                   algo = "xxhash32", seed = global_seed)
  wflw_fit_seed_int <- as.integer(paste0("0x", substr(wflw_fit_seed, 1, 6)), 16)
  set.seed(wflw_fit_seed_int)
  
  wflw_fit <- suppressWarnings({
    suppressMessages({
      fit(wflw_final, train_df)
    })
  })
  
  return(wflw_fit)
}

