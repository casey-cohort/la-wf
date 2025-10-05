## ----------------------------------------------------------------------------
## @description: Generate plots, tables, and excess hospitalization calculations
##               for Prophet + XGBoost model with MBB confidence intervals
## ----------------------------------------------------------------------------

## Load libraries
pacman::p_load(
  dplyr, ggplot2, patchwork, here, tibble,
  yardstick, gt
)

## Source paths
source(here("demo/paths.R"))

## Source utility functions
source(here("demo/utils/func_excess_hosp.R"))

## Load fitted models and MBB results
cat("Loading models and MBB results...\n")
models_output <- readRDS(here(path_outputs, "models", "1.3.1-prophet_xgb_fitted_models.rds"))
mbb_results <- readRDS(here(path_outputs, "models", "1.3.2-prophet_xgb_MBB_results.rds"))

# Extract components
train_df <- models_output$train_df
test_df <- models_output$test_df
holdout_df <- models_output$holdout_df
train_metrics <- models_output$train_metrics
test_metrics <- models_output$test_metrics

train_summary <- mbb_results$train_summary
test_summary <- mbb_results$test_MBB$pred_summary
holdout_summary <- mbb_results$holdout_MBB$pred_summary

## ============================================================================
## 1. Calculate Metrics with MBB CIs
## ============================================================================

cat("\n========================================\n")
cat("Model Performance Metrics\n")
cat("========================================\n")

## Test metrics with MBB
test_metrics_mbb <- list(
  test_rmse = yardstick::rmse_vec(test_summary$y_actual, test_summary$yhat),
  test_mae  = yardstick::mae_vec(test_summary$y_actual,  test_summary$yhat),
  test_mape = yardstick::mape_vec(test_summary$y_actual, test_summary$yhat)
)

## Holdout metrics with MBB
holdout_metrics_mbb <- list(
  holdout_rmse = yardstick::rmse_vec(holdout_summary$y_actual, holdout_summary$yhat),
  holdout_mae  = yardstick::mae_vec(holdout_summary$y_actual,  holdout_summary$yhat),
  holdout_mape = yardstick::mape_vec(holdout_summary$y_actual, holdout_summary$yhat)
)

cat("\nTraining metrics:\n")
print(train_metrics)
cat("\nTest metrics:\n")
print(test_metrics_mbb)
cat("\nHoldout metrics:\n")
print(holdout_metrics_mbb)

## Save metrics table to CSV
metrics_df <- tibble::tibble(
  window = c("train", "test", "holdout"),
  rmse = c(train_metrics$train_rmse, test_metrics_mbb$test_rmse, holdout_metrics_mbb$holdout_rmse),
  mae  = c(train_metrics$train_mae,  test_metrics_mbb$test_mae,  holdout_metrics_mbb$holdout_mae),
  mape = c(train_metrics$train_mape, test_metrics_mbb$test_mape, holdout_metrics_mbb$holdout_mape)
)
out_file_metrics <- here(path_tables, "1.3.3-Prophet_XGB_MBB_metrics.csv")
write.csv(metrics_df, out_file_metrics, row.names = FALSE)
cat("\n Metrics saved to:", out_file_metrics, "\n")

## ============================================================================
## 2. Prepare Data for Visualization
## ============================================================================

## Join predictions with original data
df_train_vis <- train_df |> 
  dplyr::left_join(
    train_summary |> dplyr::select(ds, yhat, yhat_lower = conf_lo, yhat_upper = conf_hi), 
    by = "ds"
  ) |>
  dplyr::mutate(date = ds, count = y)

df_test_vis <- test_df |> 
  dplyr::left_join(
    test_summary |> dplyr::rename(date = ds, yhat_lower = conf_lo, yhat_upper = conf_hi) |>
      dplyr::select(date, yhat, yhat_lower, yhat_upper), 
    by = c("ds" = "date")
  ) |>
  dplyr::mutate(date = ds, count = y)

df_holdout_vis <- holdout_df |> 
  dplyr::left_join(
    holdout_summary |> dplyr::rename(date = ds, yhat_lower = conf_lo, yhat_upper = conf_hi) |>
      dplyr::select(date, yhat, yhat_lower, yhat_upper), 
    by = c("ds" = "date")
  ) |>
  dplyr::mutate(date = ds, count = y)

## ============================================================================
## 3. Create Plots
## ============================================================================

cat("\nGenerating plots...\n")

## Plot function with MBB confidence intervals
plot_fit <- function(df, title) {
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = count, color = "Actual"), linewidth = 0.7) +
    geom_line(aes(y = yhat,  color = "Predicted"), linewidth = 0.7) +
    geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), 
                alpha = 0.2, fill = "blue", show.legend = FALSE) +
    scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"), name = "") +
    labs(title = title, y = "Count", x = "Date") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 12)
    )
}

p1 <- plot_fit(df_train_vis, "A) Training (fit)")
p2 <- plot_fit(df_test_vis,  "B) Test (forecast horizon with MBB CIs)")
p3 <- plot_fit(df_holdout_vis,  "C) Holdout (post Jan 7, 2025 with MBB CIs)")

## Arrange vertically
p_prophet_xgb_mbb <- (p1 / p2 / p3) + 
  plot_annotation(
    title = "Prophet + XGBoost: Fit and Forecast with Moving Block Bootstrap CIs",
    subtitle = paste0("Block length = ", mbb_results$L_block, " days, n_sim = ", mbb_results$n_sim)
  )

out_file_plot <- here(path_figures, "1.3.3-Prophet_XGB_MBB.pdf")
ggsave(out_file_plot, p_prophet_xgb_mbb, width = 12, height = 10, dpi = 300)
cat(" Plot saved to:", out_file_plot, "\n")

## ============================================================================
## 4. Calculate Excess Hospitalizations
## ============================================================================

cat("\n========================================\n")
cat("Excess Hospitalization Analysis\n")
cat("========================================\n")

## Identify the intervention/wildfire period in holdout
## Assuming the entire holdout period is the intervention period
## You may need to adjust these dates based on your specific study

## Create data subsets for different periods
### Daily data for holdout period
df_daily_holdout <- holdout_summary |>
  dplyr::mutate(
    period = as.character(ds),
    observed = y_actual,
    respiratory_pred = yhat
  ) |>
  dplyr::rename(conf_lo = conf_lo, conf_hi = conf_hi)

### Total for entire holdout period
df_period_all_holdout <- holdout_summary |>
  dplyr::summarise(
    observed = sum(y_actual),
    expected = sum(yhat),
    expected_low = sum(conf_lo),
    expected_up = sum(conf_hi),
    period = paste0(
      format(min(ds), "%b %d"), " - ", 
      format(max(ds), "%b %d, %Y")
    )
  )

# ### Weekly breakdown (if applicable)
# ## Calculate week numbers
# df_weekly_holdout <- holdout_summary |>
#   dplyr::mutate(week = lubridate::week(ds)) |>
#   dplyr::group_by(week) |>
#   dplyr::summarise(
#     observed = sum(y_actual),
#     expected = sum(yhat),
#     expected_low = sum(conf_lo),
#     expected_up = sum(conf_hi),
#     period = paste0(
#       format(min(ds), "%b %d"), " - ", 
#       format(max(ds), "%b %d, %Y")
#     ),
#     .groups = "drop"
#   )

## Calculate excess hospitalizations
cat("\nCalculating excess hospitalizations...\n")

### Daily excess
result_daily_holdout <- calc_excess_hosp(
  df_daily_holdout,
  observed = "observed",
  expected = "respiratory_pred",
  expected_conf_lo = "conf_lo",
  expected_conf_hi = "conf_hi"
)

### Total period excess
result_period_all <- calc_excess_hosp(
  df_period_all_holdout,
  observed = "observed",
  expected = "expected",
  expected_conf_lo = "expected_low",
  expected_conf_hi = "expected_up"
)

# ### Weekly excess (if multiple weeks)
# result_weekly <- calc_excess_hosp(
#   df_weekly_holdout,
#   observed = "observed",
#   expected = "expected",
#   expected_conf_lo = "expected_low",
#   expected_conf_hi = "expected_up"
# )

## Combine results
result_combined <- dplyr::bind_rows(
  result_period_all,
  # result_weekly,
  result_daily_holdout
)


## ============================================================================
## 5. Create and Save Tables
## ============================================================================

cat("\nCreating summary tables...\n")

## Create gt table for excess hospitalizations
gt_excess_hosp <- result_combined |>
  gt::gt() |>
  gt::tab_header(
    title = "Excess Hospitalizations with Moving Block Bootstrap CIs",
    subtitle = paste0("Block length = ", mbb_results$L_block, " days")
  ) |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(columns = c(period, observed, expected_CI, excess_CI, excess_pct_CI))
  ) |>
  gt::tab_style(
    style = gt::cell_fill(color = "lightblue"),
    locations = gt::cells_body(rows = 1)  # Highlight total period
  )

## Save tables
out_file_excess_html <- here(path_tables, "1.3.3-Prophet_XGB_MBB_excess_hosp.html")
out_file_excess_csv <- here(path_tables, "1.3.3-Prophet_XGB_MBB_excess_hosp.csv")

gt_excess_hosp |> gt::gtsave(out_file_excess_html)
write.csv(result_combined, out_file_excess_csv, row.names = FALSE)


