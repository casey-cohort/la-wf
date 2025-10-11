#-------------------------------
# LA wildfires project
# Output generation utility functions
#-------------------------------

#' Calculate excess hospitalizations with confidence intervals
#'
#' @param df Data frame with observed and expected values
#' @param observed Column name for observed values
#' @param expected Column name for expected (predicted) values
#' @param expected_conf_lo Column name for lower confidence limit
#' @param expected_conf_hi Column name for upper confidence limit
#'
#' @return Data frame with excess calculations and formatted output
#'
calc_excess_hosp <- function(df,
                              observed = "observed",
                              expected = "expected",
                              expected_conf_lo = "expected_low",
                              expected_conf_hi = "expected_up") {
  
  result <- df |>
    dplyr::mutate(
      # Calculate excess
      excess = !!sym(observed) - !!sym(expected),
      excess_lo = !!sym(observed) - !!sym(expected_conf_hi),
      excess_hi = !!sym(observed) - !!sym(expected_conf_lo),
      
      # Calculate percent excess
      excess_pct = (excess / !!sym(expected)) * 100,
      excess_pct_lo = (excess_lo / !!sym(expected)) * 100,
      excess_pct_hi = (excess_hi / !!sym(expected)) * 100,
      
      # Format as strings with CIs
      expected_CI = sprintf("%.1f (%.1f, %.1f)", 
                           !!sym(expected), 
                           !!sym(expected_conf_lo), 
                           !!sym(expected_conf_hi)),
      
      excess_CI = sprintf("%.1f (%.1f, %.1f)", 
                         excess, 
                         excess_lo, 
                         excess_hi),
      
      excess_pct_CI = sprintf("%.1f%% (%.1f%%, %.1f%%)", 
                             excess_pct, 
                             excess_pct_lo, 
                             excess_pct_hi)
    ) |>
    dplyr::select(period, observed, expected_CI, excess_CI, excess_pct_CI)
  
  return(result)
}


#' Create fit plot with MBB confidence intervals
#'
#' @param df Data frame with columns: date, count, yhat, yhat_lower, yhat_upper
#' @param title Plot title
#'
#' @return ggplot object
#'
create_fit_plot <- function(df, title) {
  ggplot2::ggplot(df, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = count, color = "Actual"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = yhat, color = "Predicted"), linewidth = 0.7) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = yhat_lower, ymax = yhat_upper), 
                alpha = 0.2, fill = "blue", show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"), name = "") +
    ggplot2::labs(title = title, y = "Count", x = "Date") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 12)
    )
}


#' Prepare data for visualization by joining predictions with original data
#'
#' @param original_df Original data frame with date and count columns
#' @param pred_summary Prediction summary from MBB (ds, y_actual, yhat, conf_lo, conf_hi)
#' @param outcome_col Name of the outcome column
#'
#' @return Data frame ready for plotting
#'
prepare_vis_data <- function(original_df, pred_summary, outcome_col) {
  result <- original_df |>
    dplyr::left_join(
      pred_summary |> 
        dplyr::select(ds, yhat, yhat_lower = conf_lo, yhat_upper = conf_hi),
      by = c("date" = "ds")
    ) |>
    dplyr::mutate(count = !!sym(outcome_col))
  
  return(result)
}

