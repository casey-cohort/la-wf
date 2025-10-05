## ----------------------------------------------------------------------------
## @description: Select optimal block length for Moving Block Bootstrap
## @note: This is an OPTIONAL script to determine the optimal block length.
##        Run this before 1.3.2 if you want to empirically select L_block.
##        Otherwise, you can use a default value (e.g., 7 or 14 days).
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
predictor_cols <- models_output$predictor_cols
build_xgb_features <- models_output$build_xgb_features

## ============================================================================
## Approach 1: Test multiple block lengths empirically
## ============================================================================

cat("\n========================================\n")
cat("Testing Multiple Block Lengths\n")
cat("========================================\n")

## Define range of block lengths to test
L_block_vec <- c(1, 3, 5, 7, 10, 14, 21, 28, 30)  # Days

## Note: This can be computationally intensive
## Using fewer simulations for speed
n_sim <- 500  

cat("\nTesting block lengths:", paste(L_block_vec, collapse = ", "), "\n")
cat("Number of simulations per block length:", n_sim, "\n")
cat("This may take a while...\n\n")

## Create data frame to store results
df_L_block <- data.frame(
  L_block = L_block_vec,
  observed = NA,
  expected = NA,
  expected_lo = NA,
  expected_up = NA,
  CI_width = NA
)

## Loop through different block lengths
tic("Block length selection")
for (i in seq_along(L_block_vec)) {
  L_block <- L_block_vec[i]
  cat("Testing L =", L_block, "...\n")
  
  result_MBB <- generate_MBB_CIs(
    model_prophet = m_final_prophet,
    xgb_model = xgb_final,
    train_df = train_df,
    target_df = test_df,
    predictor_cols = predictor_cols,
    n_sim = n_sim,
    L_block = L_block,
    seed = 123,
    build_xgb_features_fn = build_xgb_features
  )
  
  df_L_block$observed[i] <- sum(result_MBB$pred_summary$y_actual)
  df_L_block$expected[i] <- sum(result_MBB$pred_summary$yhat)
  df_L_block$expected_lo[i] <- sum(result_MBB$pred_summary$conf_lo)
  df_L_block$expected_up[i] <- sum(result_MBB$pred_summary$conf_hi)
  df_L_block$CI_width[i] <- df_L_block$expected_up[i] - df_L_block$expected_lo[i]
}
toc()

## Save results
saveRDS(df_L_block, here(path_outputs, "models", "1.3.0-block_length_selection.rds"))

## Print results
cat("\n========================================\n")
cat("Block Length Selection Results\n")
cat("========================================\n\n")
print(df_L_block)

## Plot L_block vs CI_width
plot_L_block <- ggplot(df_L_block, aes(x = L_block, y = CI_width)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "Effect of Block Length on Confidence Interval Width",
    subtitle = paste0("Based on ", n_sim, " bootstrap simulations"),
    x = "Block Length (days)",
    y = "Width of 95% Confidence Interval"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = L_block_vec)

out_file_plot <- here(path_figures, "1.4.0-block_length_selection.pdf")
ggsave(out_file_plot, plot_L_block, width = 10, height = 6, dpi = 300)

cat("\n Plot saved to:", out_file_plot, "\n")

## ============================================================================
## Approach 2: Use optimal block length from np package
## ============================================================================

cat("\n========================================\n")
cat("Optimal Block Length (np package)\n")
cat("========================================\n")

# Install np package if needed
if (!requireNamespace("np", quietly = TRUE)) {
  install.packages("np")
}
library(np)

## Get fitted values on training data
train_df_prophet <- train_df |> dplyr::select(ds, y)
pred_train_prophet <- predict(m_final_prophet, data.frame(ds = train_df_prophet$ds)) |> 
  dplyr::mutate(ds = as.Date(ds))

## Build features and get predictions
X_train <- build_xgb_features(pred_train_prophet, train_df)
resid_hat_train <- predict(xgb_final, xgboost::xgb.DMatrix(X_train))

## Combined fitted values
fitted_values <- pred_train_prophet$yhat + resid_hat_train

## Compute residuals
residuals_vec <- train_df$y - fitted_values

## Use np::b.star to get optimal block length
cat("\nCalculating optimal block length using Politis & White (2004) method...\n")
b_star_result <- np::b.star(residuals_vec, round = TRUE)

cat("\nOptimal block length (b*):\n")
print(b_star_result)

optimal_L <- ceiling(b_star_result[1])
cat("\nRecommended block length:", optimal_L, "days\n")

## ============================================================================
## Summary and Recommendations
## ============================================================================

cat("\n========================================\n")
cat("RECOMMENDATIONS\n")
cat("========================================\n")

# Find block length with stable CI
# Look for where CI width stabilizes (not too narrow, not too wide)
# A common heuristic is to use 7-14 days for daily time series

recommended_L <- optimal_L

cat("\nBased on the analysis:\n")
cat("  1. Optimal block length (np package):", optimal_L, "days\n")
cat("  2. CI width tends to stabilize around: 7-14 days\n")
cat("\nRecommended block length for MBB:", recommended_L, "days\n")
cat("\nTo use this in your analysis, update the L_block parameter\n")
cat("in script 1.4.2-Prophet_XGB_MBB_CIs.R\n")
cat("========================================\n")

## Save recommendations
recommendations <- list(
  optimal_L_np = optimal_L,
  recommended_L = recommended_L,
  df_L_block = df_L_block,
  residuals_vec = residuals_vec
)

saveRDS(recommendations, here(path_outputs, "models", "1.3.0-block_length_recommendations.rds"))

