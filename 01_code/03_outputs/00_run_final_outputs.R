#-------------------------------
# LA wildfires project
# Wrapper to run final outputs pipeline (03 + 04)
#-------------------------------

# Initial Setup ----
pacman::p_load(tidyverse, here, yaml, gt, writexl, readxl, 
               ggplot2, patchwork, gridExtra, ggtext, lubridate,
               snakecase, purrr, forcats, cowplot)

# Set paths
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_general.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_outputs.R"))

#' Run Final Outputs Pipeline
#'
#' Runs 03_final_outputs.R and 04_plot_outputs.R with the specified analysis mode.
#'
#' @param analysis_mode "all" for main analysis, "evac_analysis" for palisades/eaton
#' @param num_days Number of days to plot (default 7)
#' @param include_no_exposure Include "none" exposure category in plots (default FALSE)
#'
#' @examples
#' run_final_outputs("all")
#' run_final_outputs("evac_analysis", 7, TRUE)
#'
run_final_outputs <- function(analysis_mode = "all", num_days = 7, include_no_exposure = FALSE) {
  
  cat("========================================\n")
  cat("Running Final Outputs Pipeline\n")
  cat("Analysis mode:", analysis_mode, "\n")
  cat("========================================\n\n")
  
  # Set environment variable so sourced scripts can read it
  Sys.setenv(ANALYSIS_MODE = analysis_mode)
  Sys.setenv(PLOT_NUM_DAYS = num_days)
  Sys.setenv(PLOT_INCLUDE_NO_EXPOSURE = include_no_exposure)
  
  # Run 03_final_outputs.R
  cat("=== Running 03_final_outputs.R ===\n\n")
  source(paste0(getwd(), "/01_code/03_outputs/03_final_outputs.R"), local = new.env())
  
  cat("\n")
  
  # Run 04_plot_outputs.R
  cat("=== Running 04_plot_outputs.R ===\n\n")
  source(paste0(getwd(), "/01_code/03_outputs/04_plot_outputs.R"), local = new.env())
  
  cat("\n========================================\n")
  cat("Final Outputs Pipeline Complete!\n")
  cat("Analysis mode:", analysis_mode, "\n")
  cat("========================================\n")
}
