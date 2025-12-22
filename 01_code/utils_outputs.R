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
  
  # Determine appropriate break interval based on data length
  n_points <- nrow(df_filtered)
  if (n_points <= 30) {
    # For short series, show every point or every few points
    break_interval <- max(1, floor(n_points / 10))
  } else if (n_points <= 100) {
    # For medium series, show roughly every week
    break_interval <- max(1, floor(n_points / 15))
  } else if (n_points <= 200) {
    # For longer series, show roughly every 2 weeks
    break_interval <- max(1, floor(n_points / 20))
  } else {
    # For very long series, show roughly every month
    break_interval <- max(1, floor(n_points / 25))
  }
  
  # Generate breaks at regular intervals
  x_breaks <- seq(1, n_points, by = break_interval)
  # Always include the last point
  if (max(x_breaks) < n_points) {
    x_breaks <- c(x_breaks, n_points)
  }
  
  # Create labels with MM-DD-YY format
  x_labels <- format(df_filtered$date_original[x_breaks], "%m-%d-%y")
  
  # Handle gaps: add gap markers if needed
  if (nrow(relevant_gaps) > 0) {
    gap_breaks <- c()
    gap_labels <- c()
    
    for (i in 1:nrow(relevant_gaps)) {
      # Find last point before gap
      before_gap_idx <- which(df_filtered$date_original < relevant_gaps$start[i])
      if (length(before_gap_idx) > 0) {
        last_before <- max(before_gap_idx)
        # Only add if not already in breaks
        if (!last_before %in% x_breaks) {
          gap_breaks <- c(gap_breaks, last_before)
          gap_labels <- c(gap_labels, "//")
        }
      }
      
      # Find first point after gap
      after_gap_idx <- which(df_filtered$date_original > relevant_gaps$end[i])
      if (length(after_gap_idx) > 0) {
        first_after <- min(after_gap_idx)
        # Only add if not already in breaks
        if (!first_after %in% x_breaks) {
          gap_breaks <- c(gap_breaks, first_after)
          gap_labels <- c(gap_labels, "//")
        }
      }
    }
    
    # Combine regular breaks with gap markers
    all_breaks <- c(x_breaks, gap_breaks)
    all_labels <- c(x_labels, gap_labels)
    
    # Sort by break position
    sort_order <- order(all_breaks)
    x_breaks <- all_breaks[sort_order]
    x_labels <- all_labels[sort_order]
  }
  
  # Create the plot using date_idx for x-axis
  p <- ggplot2::ggplot(df_filtered, ggplot2::aes(x = date_idx)) +
    # Add data lines
    ggplot2::geom_line(ggplot2::aes(y = count, color = "Actual"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = yhat, color = "Predicted"), linewidth = 0.7) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = yhat_lower, ymax = yhat_upper), 
                alpha = 0.2, fill = "blue", show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"), name = "") +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      expand = ggplot2::expansion(mult = 0.01)
    ) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(~ ., name = "Count")
    ) +
    ggplot2::labs(title = title, y = "Count", x = "Date") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y.right = ggplot2::element_text(),
      axis.ticks.y.right = ggplot2::element_line()
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

