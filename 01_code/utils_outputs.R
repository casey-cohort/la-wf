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
  # Check if df has required columns
  required_cols <- c("date", "count", "yhat", "yhat_lower", "yhat_upper")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Define data gap periods (Feb-Oct for each year)
  gap_periods <- data.frame(
    start = as.Date(c("2023-02-01", "2024-02-01", "2025-02-01")),
    end = as.Date(c("2023-10-31", "2024-10-31", "2025-10-31"))
  )
  
  # Get the date range of the actual data
  date_range <- range(df$date, na.rm = TRUE)
  
  # Filter gap periods to only include those that overlap with the data range
  relevant_gaps <- gap_periods %>%
    dplyr::filter(start <= date_range[2] & end >= date_range[1]) %>%
    dplyr::mutate(
      start = pmax(start, date_range[1]),
      end = pmin(end, date_range[2])
    )
  
  # Filter out data within gap periods
  df_filtered <- df
  if (nrow(relevant_gaps) > 0) {
    for (i in 1:nrow(relevant_gaps)) {
      df_filtered <- df_filtered %>%
        dplyr::filter(date < relevant_gaps$start[i] | date > relevant_gaps$end[i])
    }
  }
  
  # Create index variable for continuous x-axis
  df_filtered <- df_filtered %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      date_idx = 1:n(),
      date_original = date
    )
  
  # Create custom breaks and labels
  # Identify segments (before/after gaps)
  segment_breaks <- c(1)  # Start of first segment
  segment_labels <- c(format(df_filtered$date_original[1], "%b %Y"))
  
  if (nrow(relevant_gaps) > 0) {
    for (i in 1:nrow(relevant_gaps)) {
      # Find last point before gap
      before_gap_idx <- which(df_filtered$date_original < relevant_gaps$start[i])
      if (length(before_gap_idx) > 0) {
        last_before <- max(before_gap_idx)
        segment_breaks <- c(segment_breaks, last_before)
        segment_labels <- c(segment_labels, "//")
      }
      
      # Find first point after gap
      after_gap_idx <- which(df_filtered$date_original > relevant_gaps$end[i])
      if (length(after_gap_idx) > 0) {
        first_after <- min(after_gap_idx)
        segment_breaks <- c(segment_breaks, first_after)
        segment_labels <- c(segment_labels, "//")
      }
    }
  }
  
  # Add end point
  segment_breaks <- c(segment_breaks, nrow(df_filtered))
  segment_labels <- c(segment_labels, format(df_filtered$date_original[nrow(df_filtered)], "%b %Y"))
  
  # Remove duplicates and sort
  unique_idx <- !duplicated(segment_breaks)
  segment_breaks <- segment_breaks[unique_idx]
  segment_labels <- segment_labels[unique_idx]
  
  # Create the plot using date_idx for x-axis
  p <- ggplot2::ggplot(df_filtered, ggplot2::aes(x = date_idx)) +
    # Add data lines
    ggplot2::geom_line(ggplot2::aes(y = count, color = "Actual"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = yhat, color = "Predicted"), linewidth = 0.7) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = yhat_lower, ymax = yhat_upper), 
                alpha = 0.2, fill = "blue", show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"), name = "") +
    ggplot2::scale_x_continuous(
      breaks = segment_breaks,
      labels = segment_labels,
      expand = ggplot2::expansion(mult = 0.01)
    ) +
    ggplot2::labs(title = title, y = "Count", x = "Date") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  return(p)
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

