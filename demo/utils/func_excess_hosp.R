# ----------------------------------------------------------------------------
# @description: Utility function to calculate excess hospitalizations
# ----------------------------------------------------------------------------

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

