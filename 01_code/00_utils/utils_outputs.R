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
#' @param symmetric_ci Logical, if TRUE use symmetric CI approach (default TRUE)
#'
#' @return Data frame with excess calculations and formatted output
#'
calc_excess_hosp <- function(df,
                              observed = "observed",
                              expected = "expected",
                              expected_conf_lo = "expected_low",
                              expected_conf_hi = "expected_up",
                              symmetric_ci = TRUE) {
  
  result <- df |>
    dplyr::mutate(
      # Calculate excess
      excess = !!sym(observed) - !!sym(expected)
    )
  
  if (symmetric_ci) {
    # Symmetric CI approach: compute SE and apply ±1.96*SE
    result <- result |>
      dplyr::mutate(
        expected_se = (!!sym(expected_conf_hi) - !!sym(expected)) / 1.96,
        excess_lo = excess - 1.96 * expected_se,
        excess_hi = excess + 1.96 * expected_se,
        
        # Calculate percent excess
        excess_pct = (excess / !!sym(expected)) * 100,
        excess_pct_lo = (excess_lo / !!sym(expected)) * 100,
        excess_pct_hi = (excess_hi / !!sym(expected)) * 100
      ) |>
      dplyr::select(-expected_se)
  } else {
    # Asymmetric CI approach (original logic)
    result <- result |>
      dplyr::mutate(
        excess_lo = !!sym(observed) - !!sym(expected_conf_hi),
        excess_hi = !!sym(observed) - !!sym(expected_conf_lo),
        
        # Calculate percent excess
        excess_pct = (excess / !!sym(expected)) * 100,
        excess_pct_lo = (excess_lo / !!sym(expected)) * 100,
        excess_pct_hi = (excess_hi / !!sym(expected)) * 100
      )
  }
  
  # Format as strings with CIs (same for both approaches)
  result <- result |>
    dplyr::mutate(
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


#' Calculate excess hospitalizations from MBB results
#'
#' This function processes MBB bootstrap results to calculate excess hospitalizations
#' for daily and period aggregates. It properly handles rates vs counts by using
#' mean() for rates and sum() for counts.
#'
#' @param mbb_result MBB result object containing pred_matrix and pred_summary
#' @param outcome_type Character, either "rate" or "num" (default "num")
#' @param symmetric_ci Logical, if TRUE use symmetric CI approach (default TRUE)
#'
#' @return List containing:
#'   - daily_excess: Data frame with daily excess calculations
#'   - period_excess: Data frame with period aggregate excess calculations
#'   - period_label: Character string with date range
#'
calc_excess_from_mbb <- function(mbb_result,
                                  outcome_type = "num",
                                  symmetric_ci = TRUE) {
  
  # Validate outcome_type
  if (!outcome_type %in% c("rate", "num")) {
    stop("outcome_type must be either 'rate' or 'num'")
  }
  
  # Extract components from MBB result
  pred_summary <- mbb_result$pred_summary
  pred_matrix <- mbb_result$pred_matrix
  
  # Check if we have valid data
  if (is.null(pred_summary) || nrow(pred_summary) == 0) {
    warning("No prediction summary found in MBB result")
    return(NULL)
  }
  
  # ============================================================
  # 1. Daily excess calculations
  # ============================================================
  df_daily <- pred_summary %>%
    dplyr::mutate(
      period = as.character(ds),
      observed = y_actual,
      respiratory_pred = yhat
    ) %>%
    dplyr::rename(conf_lo = conf_lo, conf_hi = conf_hi)
  
  daily_excess <- calc_excess_hosp(
    df_daily,
    observed = "observed",
    expected = "respiratory_pred",
    expected_conf_lo = "conf_lo",
    expected_conf_hi = "conf_hi",
    symmetric_ci = symmetric_ci
  )
  
  # ============================================================
  # 2. Period aggregate excess calculations
  # ============================================================
  
  # Aggregate bootstrap predictions based on outcome type
  if (outcome_type == "rate") {
    # For rates: use average (mean)
    bootstrap_aggregates <- colMeans(pred_matrix, na.rm = TRUE)
    
    df_period <- pred_summary %>%
      dplyr::summarise(
        observed = mean(y_actual, na.rm = TRUE),
        expected = mean(yhat, na.rm = TRUE),
        expected_low = quantile(bootstrap_aggregates, probs = 0.025, na.rm = TRUE),
        expected_up = quantile(bootstrap_aggregates, probs = 0.975, na.rm = TRUE),
        period = paste0(
          format(min(ds), "%b %d"), " - ",
          format(max(ds), "%b %d, %Y")
        )
      )
  } else {
    # For counts: use sum (total)
    bootstrap_aggregates <- colSums(pred_matrix, na.rm = TRUE)
    
    df_period <- pred_summary %>%
      dplyr::summarise(
        observed = sum(y_actual, na.rm = TRUE),
        expected = sum(yhat, na.rm = TRUE),
        expected_low = quantile(bootstrap_aggregates, probs = 0.025, na.rm = TRUE),
        expected_up = quantile(bootstrap_aggregates, probs = 0.975, na.rm = TRUE),
        period = paste0(
          format(min(ds), "%b %d"), " - ",
          format(max(ds), "%b %d, %Y")
        )
      )
  }
  
  period_excess <- calc_excess_hosp(
    df_period,
    observed = "observed",
    expected = "expected",
    expected_conf_lo = "expected_low",
    expected_conf_hi = "expected_up",
    symmetric_ci = symmetric_ci
  )
  
  # Extract period label
  period_label <- df_period$period[1]
  
  # Return results
  return(list(
    daily_excess = daily_excess,
    period_excess = period_excess,
    period_label = period_label
  ))
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


#-------------------------------
# Model Comparison Functions
#-------------------------------

#' Find all model directories across user subdirectories
#'
#' @param models_dir Base models directory path
#' @param subdirs Vector of subdirectory names to search (e.g., c("akd", "lbw"))
#'
#' @return List of model directory info with path and source_dir
#'
find_model_directories <- function(models_dir, subdirs = c("akd", "lbw")) {
  model_dirs <- list()
  for (subdir in subdirs) {
    subdir_path <- paste0(models_dir, "/", subdir, "/")
    if (dir.exists(subdir_path)) {
      all_dirs <- list.dirs(subdir_path, full.names = TRUE, recursive = FALSE)
      for (d in all_dirs) {
        model_dirs[[length(model_dirs) + 1]] <- list(
          path = d,
          source_dir = subdir
        )
      }
    }
  }
  return(model_dirs)
}


#' Extract metrics from a single model directory
#'
#' @param model_dir_info List with path and source_dir
#' @param outcome_type Outcome type to filter by (e.g., "rate", "num")
#'
#' @return Data frame with test metrics, or NULL if not found
#'
extract_metrics_from_dir <- function(model_dir_info, outcome_type) {
  model_dir <- model_dir_info$path
  source_dir <- model_dir_info$source_dir
  version <- basename(model_dir)
  
  # Find performance metrics file in performance_metrics subdirectory
  performance_metrics_dir <- paste0(model_dir, "/performance_metrics/")
  
  if (!dir.exists(performance_metrics_dir)) {
    return(NULL)
  }
  
  # Find performance_metrics_with_mbb_*.csv file
  metrics_files <- list.files(
    performance_metrics_dir, 
    pattern = "^performance_metrics_with_mbb_.*\\.csv$", 
    full.names = TRUE
  )
  
  if (length(metrics_files) == 0) {
    return(NULL)
  }
  
  tryCatch({
    # Read metrics file
    metrics_df <- read.csv(metrics_files[1], stringsAsFactors = FALSE)
    
    # Filter for test window only
    test_metrics <- metrics_df %>%
      filter(window == "test") %>%
      filter(grepl(paste0("^", outcome_type, "_"), cause)) %>%
      mutate(version = version, source_dir = source_dir) %>%
      select(
        source_dir,
        version,
        enc_type,
        exposure_category,
        cause,
        R2 = r2,
        MAPE = mape,
        sMAPE = smape,
        MASE = mase,
        window
      )
    
    return(test_metrics)
    
  }, error = function(e) {
    return(NULL)
  })
}


#' Combine metrics from all directories and create complete grid
#'
#' @param all_metrics_list List of metrics data frames
#' @param outcome_type Outcome type for filtering
#'
#' @return Combined metrics data frame with complete grid
#'
combine_and_expand_metrics <- function(all_metrics_list, outcome_type) {
  if (length(all_metrics_list) == 0) {
    stop("No metrics data could be extracted from any model directories")
  }
  
  combined_metrics <- bind_rows(all_metrics_list)
  
  # Filter out cases where cause does not start with outcome_type
  combined_metrics <- combined_metrics %>%
    filter(grepl(paste0("^", outcome_type, "_"), cause))
  
  # Get all unique (source_dir, version) pairs that actually exist
  unique_dir_version_pairs <- combined_metrics %>%
    distinct(source_dir, version)
  
  # Get all unique combinations of enc_type, exposure_category, cause across all data
  unique_combinations <- combined_metrics %>%
    distinct(enc_type, exposure_category, cause)
  
  # Create complete grid
  complete_grid <- unique_dir_version_pairs %>%
    tidyr::crossing(unique_combinations)
  
  # Left join actual data onto complete grid
  combined_metrics <- complete_grid %>%
    left_join(
      combined_metrics, 
      by = c("source_dir", "version", "enc_type", "exposure_category", "cause")
    ) %>%
    arrange(enc_type, exposure_category, cause, source_dir, desc(version))
  
  return(list(
    metrics = combined_metrics,
    unique_dir_version_pairs = unique_dir_version_pairs,
    unique_combinations = unique_combinations
  ))
}


#' Identify best models based on specified metric
#'
#' @param combined_metrics Combined metrics data frame
#' @param r2_threshold R2 threshold value (default = 0)
#' @param metric Metric to use for selecting best model: "sMAPE" or "MASE" (default = "sMAPE")
#' @param n Number of top models to select with lowest metric value (default = 1)
#'
#' @return Data frame with best model(s) for each combination
#'
identify_best_models <- function(combined_metrics, r2_threshold = 0, metric = "sMAPE", n = 1) {
  # Validate metric parameter
  if (!metric %in% c("sMAPE", "MASE")) {
    stop("metric must be either 'sMAPE' or 'MASE'")
  }
  
  # Get the metric column name
  metric_col <- if (metric == "sMAPE") "sMAPE" else "MASE"
  
  best_models <- combined_metrics %>%
    group_by(enc_type, exposure_category, cause) %>%
    mutate(
      # Flag models with R2 above threshold
      has_r2_above_threshold = !is.na(R2) & R2 > r2_threshold,
      # Flag models with valid metric
      has_valid_metric = !is.na(!!sym(metric_col)),
      # Flag valid models (both R2 above threshold and valid metric)
      is_valid_model = has_r2_above_threshold & has_valid_metric,
      # Check if any valid models exist in this group
      has_any_valid = any(has_r2_above_threshold)
    ) %>%
    # Select best model: if valid models exist, pick lowest metric; otherwise keep first row and set metrics to NA
    group_modify(~ {
      if (.x$has_any_valid[1]) {
        # Filter to valid models and select lowest metric
        .x %>%
          filter(is_valid_model) %>%
          slice_min(.data[[metric_col]], n = n, with_ties = TRUE)
      } else {
        # No valid models - keep first row but set metrics to NA
        .x %>%
          slice(1) %>%
          mutate(R2 = NA_real_, MAPE = NA_real_, sMAPE = NA_real_, MASE = NA_real_)
      }
    }) %>%
    ungroup() %>%
    select(-has_r2_above_threshold, -has_valid_metric, -is_valid_model, -has_any_valid)
  
  return(best_models)
}


#' Extract PDFs and configs for best models
#'
#' @param best_models_df Data frame with best models (must have columns: source_dir, version, enc_type, exposure_category, cause)
#' @param models_dir Base models directory path
#' @param bested_dir Directory to save extracted files (used for both PDFs and configs if separate directories not specified)
#' @param pdf_dir Optional directory to save PDFs (if NULL, uses bested_dir)
#' @param config_dir Optional directory to save configs (if NULL, uses bested_dir)
#'
#' @return NULL (files are written to disk)
#'
extract_best_model_files <- function(best_models_df, models_dir, bested_dir, pdf_dir = NULL, config_dir = NULL) {
  # Use separate directories if provided, otherwise use bested_dir for both
  pdf_dest_dir <- if (!is.null(pdf_dir)) pdf_dir else bested_dir
  config_dest_dir <- if (!is.null(config_dir)) config_dir else bested_dir
  
  # Create directories if they don't exist
  dir.create(pdf_dest_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config_dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Track success/failure counts
  pdf_success <- 0
  pdf_failed <- 0
  config_success <- 0
  config_failed <- 0
  files_replaced <- 0
  
  # Process each best model
  for (i in 1:nrow(best_models_df)) {
    row <- best_models_df[i, ]
    source_dir <- row$source_dir
    version <- row$version
    enc_type <- row$enc_type
    exposure_category <- row$exposure_category
    cause <- row$cause
    
    # Construct paths
    model_version_dir <- paste0(models_dir, source_dir, "/", version, "/")
    
    # Extract mod_ver_suffix from version (remove "model_run_" prefix)
    mod_ver_suffix <- sub("^model_run_", "", version)
    
    # Find PDF
    pdf_source <- paste0(model_version_dir, "figures/model_fit_", enc_type, "_", exposure_category, "_", cause, ".pdf")
    
    # Find config
    config_source <- paste0(model_version_dir, "model_config_", mod_ver_suffix, ".yaml")
    
    # Create common filename prefix so PDF and YAML sort together
    file_prefix <- paste0(enc_type, "_", exposure_category, "_", cause, "_", source_dir, "_", version)
    
    # Pattern to match any existing files for this model combination (regardless of version)
    # This allows us to replace old versions with new best models
    combination_pattern <- paste0("^", enc_type, "_", exposure_category, "_", cause, "_.*")
    
    # Remove existing files for this combination before adding new ones
    # Check both directories if they're different
    existing_pdf_files <- list.files(pdf_dest_dir, pattern = combination_pattern, full.names = TRUE)
    existing_config_files <- list.files(config_dest_dir, pattern = combination_pattern, full.names = TRUE)
    existing_files <- c(existing_pdf_files, existing_config_files)
    if (length(existing_files) > 0) {
      file.remove(existing_files)
      files_replaced <- files_replaced + length(existing_files)
    }
    
    # Copy PDF if it exists
    if (file.exists(pdf_source)) {
      pdf_dest <- paste0(pdf_dest_dir, "/", file_prefix, "_fit.pdf")
      file.copy(pdf_source, pdf_dest, overwrite = TRUE)
      pdf_success <- pdf_success + 1
    } else {
      pdf_failed <- pdf_failed + 1
    }
    
    # Read and modify config if it exists
    if (file.exists(config_source)) {
      tryCatch({
        config <- yaml::read_yaml(config_source)
        
        # Modify models_to_run_flat to only contain this one model
        config$models_to_run_flat <- list(
          list(
            encounter_type = enc_type,
            exposure_category = exposure_category,
            cause = cause
          )
        )
        
        # Remove or null out models_to_run to avoid confusion
        config$models_to_run <- NULL
        
        # Write modified config
        config_dest <- paste0(config_dest_dir, "/", file_prefix, "_config.yaml")
        yaml::write_yaml(config, config_dest)
        config_success <- config_success + 1
        
      }, error = function(e) {
        config_failed <- config_failed + 1
      })
    } else {
      config_failed <- config_failed + 1
    }
  }
  
  # Print summary
  if (files_replaced > 0) {
    cat("  Replaced", files_replaced, "existing file(s) with updated best models\n")
  }
  cat("  Extracted", pdf_success, "PDFs")
  if (pdf_failed > 0) cat(" (", pdf_failed, " failed)", sep = "")
  cat("\n")
  cat("  Extracted", config_success, "configs")
  if (config_failed > 0) cat(" (", config_failed, " failed)", sep = "")
  cat("\n")
}


#' Filter models by R2 threshold
#'
#' @param combined_metrics Combined metrics data frame
#' @param r2_threshold R2 threshold value
#'
#' @return Filtered data frame with models above threshold
#'
filter_models_by_r2 <- function(combined_metrics, r2_threshold = 0.15) {
  r2_above_threshold <- combined_metrics %>%
    group_by(enc_type, exposure_category, cause) %>%
    mutate(
      is_valid_model = !is.na(R2) & R2 > r2_threshold,
    ) %>%
    filter(is_valid_model) %>%
    ungroup() %>%
    select(-is_valid_model)
  
  return(r2_above_threshold)
}


#' Print diagnostic information about models meeting/not meeting threshold
#'
#' @param unique_combinations All unique combinations
#' @param unique_combinations_threshold Combinations meeting threshold
#'
#' @return NULL (prints to console)
#'
print_threshold_diagnostics <- function(unique_combinations, unique_combinations_threshold) {
  # Combinations that don't meet the threshold
  unique_combinations_not_threshold <- unique_combinations %>%
    anti_join(unique_combinations_threshold, 
              by = c("enc_type", "exposure_category", "cause"))
  
  cat("  Above threshold:", nrow(unique_combinations_threshold), 
      "| Below threshold:", nrow(unique_combinations_not_threshold), "\n")
  
  if (nrow(unique_combinations_not_threshold) > 0 && nrow(unique_combinations_not_threshold) <= 10) {
    # Only print if 10 or fewer, otherwise too verbose
    print(unique_combinations_not_threshold)
  }
}

