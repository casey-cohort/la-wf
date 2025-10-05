## ----------------------------------------------------------------------------
## @description: Generate Moving Block Bootstrap (MBB) confidence intervals
##               for Prophet + XGBoost predictions
## ----------------------------------------------------------------------------

## Load libraries
pacman::p_load(
  dplyr, ggplot2, prophet, here, tictoc,
  tibble, xgboost, boot
)

## Source paths
source(here("demo/paths.R"))

## Source utility functions
source(here("demo/utils/func_mbb.R"))

## Load fitted models and data
cat("Loading fitted models...\n")
models_output <- readRDS(here(path_outputs, "models", "1.3.1-prophet_xgb_fitted_models.rds"))

# Extract components
m_final_prophet <- models_output$prophet_model
xgb_final <- models_output$xgb_model
train_df <- models_output$train_df
test_df <- models_output$test_df
holdout_df <- models_output$holdout_df
predictor_cols <- models_output$predictor_cols
build_xgb_features <- models_output$build_xgb_features

## Set parameters for MBB
n_sim <- 1000     # Number of bootstrap simulations

# Try to load optimal block length from 1.3.0 script, otherwise use default
recommendations_file <- here(path_outputs, "models", "1.3.0-block_length_recommendations.rds")
if (file.exists(recommendations_file)) {
  cat("Found block length recommendations from 1.3.0 script...\n")
  recommendations <- readRDS(recommendations_file)
  L_block <- recommendations$recommended_L
  cat("  Using recommended block length:", L_block, "days\n")
} else {
  L_block <- 14  # Default block length
  cat("No block length recommendations found. Using default:", L_block, "days\n")
  cat("  (Run 1.3.0-Prophet_XGB_select_block_length.R to compute optimal block length)\n")
}

seed <- 123

cat("\nGenerating MBB confidence intervals...\n")
cat("  Number of simulations:", n_sim, "\n")
cat("  Block length:", L_block, "\n")

## Generate MBB CIs for test set
tic("Test MBB")
result_test_MBB <- generate_MBB_CIs(
  model_prophet = m_final_prophet,
  xgb_model = xgb_final,
  train_df = train_df,
  target_df = test_df,
  predictor_cols = predictor_cols,
  n_sim = n_sim,
  L_block = L_block,
  seed = seed,
  build_xgb_features_fn = build_xgb_features
)
toc()

## Generate MBB CIs for holdout set
tic("Holdout MBB")
result_holdout_MBB <- generate_MBB_CIs(
  model_prophet = m_final_prophet,
  xgb_model = xgb_final,
  train_df = train_df,
  target_df = holdout_df,
  predictor_cols = predictor_cols,
  n_sim = n_sim,
  L_block = L_block,
  seed = seed,
  build_xgb_features_fn = build_xgb_features
)
toc()

## For training set, we'll use simple predictions without bootstrap
## (since it's the fit, not forecast)
train_df_prophet <- train_df |> dplyr::select(ds, y)
pred_train_prophet <- predict(m_final_prophet, data.frame(ds = train_df_prophet$ds)) |> 
  dplyr::mutate(ds = as.Date(ds))
X_train <- build_xgb_features(pred_train_prophet, train_df)
resid_hat_train <- predict(xgb_final, xgboost::xgb.DMatrix(X_train))

result_train_summary <- tibble::tibble(
  ds = as.Date(pred_train_prophet$ds),
  y_actual = train_df$y,
  yhat = pred_train_prophet$yhat + resid_hat_train,
  conf_lo = pred_train_prophet$yhat_lower,  # Use Prophet's built-in intervals for training
  conf_hi = pred_train_prophet$yhat_upper
)

## Save results
cat("\nSaving MBB results...\n")
mbb_results <- list(
  train_summary = result_train_summary,
  test_MBB = result_test_MBB,
  holdout_MBB = result_holdout_MBB,
  n_sim = n_sim,
  L_block = L_block
)

saveRDS(mbb_results, here(path_outputs, "models", "1.3.2-prophet_xgb_MBB_results.rds"))


