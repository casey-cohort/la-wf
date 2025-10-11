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
                                        seed = 123) {
  
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
    
    # Refit workflow on bootstrapped data
    wflw_fit_star <- suppressWarnings({
      suppressMessages({
        wflw_fit |>
          fit(train_df_star)
      })
    })
    
    # Predict on target data
    preds <- suppressWarnings({
      suppressMessages({
        predict(wflw_fit_star, new_data = target_df)$.pred
      })
    })
    
    pred_matrix[, i] <- preds
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

