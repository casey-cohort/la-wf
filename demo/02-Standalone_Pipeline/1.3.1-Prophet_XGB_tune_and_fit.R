## ----------------------------------------------------------------------------
## @description: Tune and fit Prophet + XGBoost hybrid model with CV
## ----------------------------------------------------------------------------

# Setup ----

## Libraries
pacman::p_load(
  dplyr, ggplot2, prophet, patchwork, here, tictoc,
  purrr, yardstick, tidyr, tibble, rsample,
  xgboost
)

## Constants
source(here("demo/paths.R"))
set.seed(123)

# Data Preparation ----

## Run data preparation script
## will load df_ED, df_train_test, df_holdout, splits, resamples_kfold, holdout_date, predictor_cols)
source(here("demo/02-Standalone_Pipeline/1.2-prep_data_train_test.R"))  

## Process train/test splits
train_df <- training(splits) |> dplyr::select(ds = date, y = count, all_of(predictor_cols)) |> dplyr::mutate(ds = as.Date(ds))
test_df  <- testing(splits)  |> dplyr::select(ds = date, y = count, all_of(predictor_cols)) |> dplyr::mutate(ds = as.Date(ds))
test_h   <- nrow(test_df)
hold_h   <- nrow(df_holdout)

set.seed(123)

# Helper Functions ----

## Build XGBoost feature matrix from Prophet predict() output + additional predictors
## Keeps Prophet component columns as features and drops date/interval columns
## Then adds the additional predictor columns from the original data
build_xgb_features <- function(pred_df_components, data_df) {
  # Extract Prophet components
  prophet_features <- pred_df_components |> 
    dplyr::select(-ds, -yhat, -yhat_lower, -yhat_upper) |> 
    dplyr::select(where(is.numeric))
  
  # Extract additional predictors (excluding ds and y)
  additional_features <- data_df |> 
    dplyr::select(all_of(predictor_cols))
  
  # Combine Prophet components and additional predictors
  feature_df <- dplyr::bind_cols(prophet_features, additional_features)
  as.matrix(feature_df)
}

# Cross-Validation ----

## Hyperparameter grid
grid <- tidyr::crossing(
  changepoint.prior.scale = c(0.05, 0.1, 0.5),
  seasonality.prior.scale = c(5, 10),
  holidays.prior.scale    = c(5, 10),
  seasonality.mode        = c("additive", "multiplicative")
)

## Run CV loop
cat("Starting cross-validation...\n")
tic("CV")
cv_results <- purrr::map_dfr(resamples_kfold$splits, function(s) {
  # Full data with predictors for XGBoost
  trn_full <- training(s) |> dplyr::select(ds = date, y = count, all_of(predictor_cols)) |> dplyr::mutate(ds = as.Date(ds))
  tst_full <- testing(s) |> dplyr::select(ds = date, y = count, all_of(predictor_cols)) |> dplyr::mutate(ds = as.Date(ds))
  
  # Prophet-only data (just ds and y)
  trn_prophet <- trn_full |> dplyr::select(ds, y)
  tst_prophet <- tst_full |> dplyr::select(ds, y)
  horizon <- nrow(tst_full)

  purrr::map_dfr(seq_len(nrow(grid)), function(i) {
    p <- grid[i, ]

    m <- prophet::prophet(
      yearly.seasonality = TRUE,
      weekly.seasonality = TRUE,
      daily.seasonality  = FALSE,
      seasonality.mode   = p$seasonality.mode,
      changepoint.prior.scale = p$changepoint.prior.scale,
      seasonality.prior.scale = p$seasonality.prior.scale,
      holidays.prior.scale    = p$holidays.prior.scale
    )
    m <- prophet::add_country_holidays(m, country_name = "US")
    m <- prophet::fit.prophet(m, trn_prophet)

    # Prophet predictions and components on train and test
    pred_trn_prophet <- predict(m, data.frame(ds = trn_prophet$ds)) |> 
      dplyr::mutate(ds = as.Date(ds))
    pred_tst_prophet <- predict(m, data.frame(ds = tst_prophet$ds)) |> 
      dplyr::mutate(ds = as.Date(ds))

    # Train XGBoost on Prophet residuals using Prophet components as features
    X_trn <- build_xgb_features(pred_trn_prophet, trn_full)
    y_resid_trn <- trn_full$y - pred_trn_prophet$yhat

    dtrain <- xgboost::xgb.DMatrix(data = X_trn, label = y_resid_trn)
    xgb_params <- list(
      objective = "reg:squarederror",
      max_depth = 4,
      eta = 0.1,
      subsample = 0.8,
      colsample_bytree = 0.8,
      nthread = max(1, parallel::detectCores() - 1)
    )
    xgb_nrounds <- 300
    xgb_model <- xgboost::xgb.train(
      params = xgb_params,
      data = dtrain,
      nrounds = xgb_nrounds,
      verbose = 0
    )

    # Predict residuals on test and combine with Prophet baseline
    X_tst <- build_xgb_features(pred_tst_prophet, tst_full)
    resid_hat_tst <- predict(xgb_model, xgboost::xgb.DMatrix(X_tst))

    fc_combined <- tibble::tibble(
      ds = as.Date(pred_tst_prophet$ds),
      yhat = pred_tst_prophet$yhat + resid_hat_tst
    )

    eval_df <- dplyr::left_join(tst_full, fc_combined, by = "ds")

    tibble::tibble(
      rmse = yardstick::rmse_vec(eval_df$y, eval_df$yhat),
      mae  = yardstick::mae_vec(eval_df$y,  eval_df$yhat),
      mape = yardstick::mape_vec(eval_df$y, eval_df$yhat),
      changepoint.prior.scale = p$changepoint.prior.scale,
      seasonality.prior.scale = p$seasonality.prior.scale,
      holidays.prior.scale    = p$holidays.prior.scale,
      seasonality.mode        = p$seasonality.mode
    )
  })
})
toc()

## Select best parameters
best <- cv_results |>
  dplyr::group_by(
    changepoint.prior.scale, seasonality.prior.scale,
    holidays.prior.scale, seasonality.mode
  ) |>
  dplyr::summarise(
    rmse = mean(rmse, na.rm = TRUE),
    mae  = mean(mae,  na.rm = TRUE),
    mape = mean(mape, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(rmse) |>
  dplyr::slice(1) 

cat("\nBest hyperparameters:\n")
print(best)

# Final Model Training ----
cat("\nFitting final Prophet + XGBoost model...\n")

## Fit Prophet model
train_df_prophet <- train_df |> dplyr::select(ds, y)
test_df_prophet <- test_df |> dplyr::select(ds, y)

m_final_prophet <- prophet::prophet(
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  daily.seasonality  = FALSE,
  seasonality.mode   = best$seasonality.mode,
  changepoint.prior.scale = best$changepoint.prior.scale,
  seasonality.prior.scale = best$seasonality.prior.scale,
  holidays.prior.scale    = best$holidays.prior.scale
)
m_final_prophet <- prophet::add_country_holidays(m_final_prophet, country_name = "US")
m_final_prophet <- prophet::fit.prophet(m_final_prophet, train_df_prophet)

## Generate Prophet predictions
pred_train_prophet <- predict(m_final_prophet, data.frame(ds = train_df_prophet$ds)) |> 
  dplyr::mutate(ds = as.Date(ds))

pred_test_prophet <- predict(m_final_prophet, data.frame(ds = test_df_prophet$ds)) |> 
  dplyr::mutate(ds = as.Date(ds))

## Train XGBoost on residuals
X_train <- build_xgb_features(pred_train_prophet, train_df)
y_resid_train <- train_df$y - pred_train_prophet$yhat

dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_resid_train)

xgb_params <- list(
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nthread = max(1, parallel::detectCores() - 1)
)
xgb_nrounds <- 300

xgb_final <- xgboost::xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = xgb_nrounds,
  verbose = 0
)

## Generate XGBoost predictions
pred_train_resid <- predict(xgb_final, xgboost::xgb.DMatrix(build_xgb_features(pred_train_prophet, train_df))) 
pred_test_resid <- predict(xgb_final, xgboost::xgb.DMatrix(build_xgb_features(pred_test_prophet, test_df)))

## Combine predictions
pred_train_combined <- tibble::tibble(
  ds = as.Date(pred_train_prophet$ds),
  yhat = pred_train_prophet$yhat + pred_train_resid
)
pred_test_combined <- tibble::tibble(
  ds = as.Date(pred_test_prophet$ds),
  yhat = pred_test_prophet$yhat + pred_test_resid
)

# Model Evaluation ----

## Training metrics
train_eval <- dplyr::left_join(train_df, pred_train_combined, by = "ds") |> dplyr::filter(!is.na(yhat))
train_metrics <- list(
  train_rmse = yardstick::rmse_vec(train_eval$y, train_eval$yhat),
  train_mae  = yardstick::mae_vec(train_eval$y,  train_eval$yhat),
  train_mape = yardstick::mape_vec(train_eval$y, train_eval$yhat)
)

## Test metrics
test_eval <- dplyr::left_join(test_df, pred_test_combined, by = "ds")
test_metrics <- list(
  test_rmse = yardstick::rmse_vec(test_eval$y, test_eval$yhat),
  test_mae  = yardstick::mae_vec(test_eval$y,  test_eval$yhat),
  test_mape = yardstick::mape_vec(test_eval$y, test_eval$yhat)
)

cat("\nTraining metrics:\n")
print(train_metrics)
cat("\nTest metrics:\n")
print(test_metrics)

# Save Results ----
cat("\nSaving fitted models and data...\n")

models_output <- list(
  prophet_model = m_final_prophet,
  xgb_model = xgb_final,
  best_params = best,
  train_df = train_df,
  test_df = test_df,
  holdout_df = df_holdout |> dplyr::select(ds = date, y = count, period, all_of(predictor_cols)) |> dplyr::mutate(ds = as.Date(ds)),
  predictor_cols = predictor_cols,
  train_metrics = train_metrics,
  test_metrics = test_metrics,
  cv_results = cv_results,
  build_xgb_features = build_xgb_features
)

saveRDS(models_output, here(path_outputs, "models", "1.3.1-prophet_xgb_fitted_models.rds"))


