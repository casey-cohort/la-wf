# ----------------------------------------------------------------------------
# @description: Utility functions for Moving Block Bootstrap (MBB) to generate
#               confidence intervals for Prophet + XGBoost hybrid models
# ----------------------------------------------------------------------------

#' Generate Confidence Intervals using Moving Block Bootstrap
#'
#' @param model_prophet Fitted Prophet model object
#' @param xgb_model Fitted XGBoost model object
#' @param train_df Training data with columns: ds, y, and predictor variables
#' @param target_df Target data for predictions (e.g., test or holdout)
#' @param predictor_cols Character vector of predictor column names
#' @param n_sim Number of bootstrap simulations (default: 1000)
#' @param L_block Block length for MBB (default: 7)
#' @param seed Random seed for reproducibility
#' @param build_xgb_features_fn Function to build XGBoost feature matrix
#'
#' @return List containing:
#'   - pred_matrix: Matrix of predictions (n_rows x n_sim)
#'   - pred_summary: Tibble with date, actual, predicted, conf_lo, conf_hi
#'   - block_length: Block length used
#'
generate_MBB_CIs <- function(model_prophet,
                              xgb_model,
                              train_df,
                              target_df,
                              predictor_cols,
                              n_sim = 1000,
                              L_block = 7,
                              seed = 123,
                              build_xgb_features_fn) {
  
  set.seed(seed)
  
  # Load required packages
  if (!requireNamespace("boot", quietly = TRUE)) {
    install.packages("boot")
  }
  library(boot)
  
  # Get fitted values on training data
  train_df_prophet <- train_df |> dplyr::select(ds, y)
  pred_train_prophet <- predict(model_prophet, data.frame(ds = train_df_prophet$ds)) |> 
    dplyr::mutate(ds = as.Date(ds))
  
  # Build features for training data
  X_train <- build_xgb_features_fn(pred_train_prophet, train_df)
  resid_hat_train <- predict(xgb_model, xgboost::xgb.DMatrix(X_train))
  
  # Combined fitted values
  fitted_values <- pred_train_prophet$yhat + resid_hat_train
  
  # Compute residuals
  residuals_vec <- train_df$y - fitted_values
  
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
    train_df_star <- train_df |>
      dplyr::mutate(y = y_star)
    
    train_df_prophet_star <- train_df_star |> dplyr::select(ds, y)
    
    # Refit Prophet on bootstrapped data
    m_star <- prophet::prophet(
      yearly.seasonality = model_prophet$yearly.seasonality,
      weekly.seasonality = model_prophet$weekly.seasonality,
      daily.seasonality = model_prophet$daily.seasonality,
      seasonality.mode = model_prophet$seasonality.mode,
      changepoint.prior.scale = model_prophet$changepoint.prior.scale,
      seasonality.prior.scale = model_prophet$seasonality.prior.scale,
      holidays.prior.scale = model_prophet$holidays.prior.scale
    )
    m_star <- prophet::add_country_holidays(m_star, country_name = "US")
    m_star <- suppressMessages(prophet::fit.prophet(m_star, train_df_prophet_star))
    
    # Predict on training data with bootstrapped model
    pred_train_prophet_star <- predict(m_star, data.frame(ds = train_df_prophet_star$ds)) |> 
      dplyr::mutate(ds = as.Date(ds))
    
    # Retrain XGBoost on bootstrapped residuals
    X_train_star <- build_xgb_features_fn(pred_train_prophet_star, train_df_star)
    y_resid_train_star <- train_df_star$y - pred_train_prophet_star$yhat
    
    dtrain_star <- xgboost::xgb.DMatrix(data = X_train_star, label = y_resid_train_star)
    xgb_params <- list(
      objective = "reg:squarederror",
      max_depth = 4,
      eta = 0.1,
      subsample = 0.8,
      colsample_bytree = 0.8,
      nthread = max(1, parallel::detectCores() - 1)
    )
    xgb_model_star <- xgboost::xgb.train(
      params = xgb_params,
      data = dtrain_star,
      nrounds = 300,
      verbose = 0
    )
    
    # Predict on target data
    target_df_prophet <- target_df |> dplyr::select(ds)
    pred_target_prophet_star <- predict(m_star, data.frame(ds = target_df_prophet$ds)) |> 
      dplyr::mutate(ds = as.Date(ds))
    
    X_target_star <- build_xgb_features_fn(pred_target_prophet_star, target_df)
    resid_hat_target_star <- predict(xgb_model_star, xgboost::xgb.DMatrix(X_target_star))
    
    preds <- pred_target_prophet_star$yhat + resid_hat_target_star
    pred_matrix[, i] <- preds
  }
  
  cat("Bootstrap simulations complete!\n")
  
  # Get central estimate from the bootstrap distribution (mean of bootstrap predictions)
  # This ensures the confidence intervals are centered around the predicted values
  central_pred <- apply(pred_matrix, 1, mean, na.rm = TRUE)
  
  # Compute summary with confidence intervals
  pred_summary <- tibble::tibble(
    ds = as.Date(target_df$ds),
    y_actual = target_df$y,
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


#' Select optimal block length for MBB by testing different values
#'
#' @param model_prophet Fitted Prophet model object
#' @param xgb_model Fitted XGBoost model object
#' @param train_df Training data
#' @param target_df Target data for predictions
#' @param predictor_cols Character vector of predictor column names
#' @param L_block_vec Vector of block lengths to test (default: 1:30)
#' @param n_sim Number of simulations per block length (default: 500)
#' @param build_xgb_features_fn Function to build XGBoost feature matrix
#'
#' @return Data frame with block length and corresponding CI widths
#'
select_block_length <- function(model_prophet,
                                 xgb_model,
                                 train_df,
                                 target_df,
                                 predictor_cols,
                                 L_block_vec = 1:30,
                                 n_sim = 500,
                                 build_xgb_features_fn) {
  
  df_L_block <- data.frame(
    L_block = L_block_vec,
    observed = NA,
    expected = NA,
    expected_lo = NA,
    expected_up = NA,
    CI_width = NA
  )
  
  for (L_block in L_block_vec) {
    cat("\nTesting block length:", L_block, "\n")
    
    result_MBB <- generate_MBB_CIs(
      model_prophet = model_prophet,
      xgb_model = xgb_model,
      train_df = train_df,
      target_df = target_df,
      predictor_cols = predictor_cols,
      n_sim = n_sim,
      L_block = L_block,
      build_xgb_features_fn = build_xgb_features_fn
    )
    
    df_L_block$observed[L_block] <- sum(result_MBB$pred_summary$y_actual)
    df_L_block$expected[L_block] <- sum(result_MBB$pred_summary$yhat)
    df_L_block$expected_lo[L_block] <- sum(result_MBB$pred_summary$conf_lo)
    df_L_block$expected_up[L_block] <- sum(result_MBB$pred_summary$conf_hi)
    df_L_block$CI_width[L_block] <- df_L_block$expected_up[L_block] - df_L_block$expected_lo[L_block]
  }
  
  return(df_L_block)
}

