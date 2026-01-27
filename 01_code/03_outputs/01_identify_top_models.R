#-------------------------------
# LA wildfires project
# Compare model performance across all exposure versions
#-------------------------------

# Initial Setup ----
pacman::p_load(tidyverse, here, yaml, writexl)

# Set paths
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_outputs.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_best_tuned.R"))

#' Identify Top Models
#'
#' Searches model directories, extracts metrics, and identifies top performing models.
#'
#' @param analysis_mode "all" for main analysis, "evac_analysis" for palisades/eaton only
#' @param outcome_type Outcome type for metrics (default "rate")
#' @param r2_threshold R2 threshold for filtering (default 0.05)
#' @param metric Metric to rank by (default "MASE")
#' @param n Number of top models to identify per combination (default 6)
#'
#' @examples
#' identify_top_models()                        # Main analysis (all models)
#' identify_top_models("evac_analysis")         # Palisades + Eaton only
#'
identify_top_models <- function(analysis_mode = "all", outcome_type = "rate", 
                                 r2_threshold = 0.05, metric = "MASE", n = 6) {
  
  cat("========================================\n")
  cat("Identify Top Models\n")
  cat("Analysis mode:", analysis_mode, "\n")
  cat("========================================\n\n")
  
  #-------------------------------
  # Step-1: Extract and combine all metrics
  #-------------------------------
  cat("=== Step 1: Extract and Combine Metrics ===\n")
  models_dir <- here(path_onedrive, "03_modeling-and-results/01_modeling/")
  
  # Define subdirectories to search
  subdirs <- c("akd", "lbw")
  
  # Find all model directories
  model_dirs <- find_model_directories(models_dir, subdirs)
  
  cat("Found", length(model_dirs), "total model directories\n")
  
  # Filter by analysis_mode
  if (analysis_mode == "evac_analysis") {
    # Only keep directories with "eaton" or "palisades" in the name
    keep_idx <- sapply(model_dirs, function(x) {
      grepl("eaton|palisades", basename(x$path), ignore.case = TRUE)
    })
    model_dirs <- model_dirs[keep_idx]
    cat("Filtered to", length(model_dirs), "eaton/palisades directories\n")
  } else if (analysis_mode != "all") {
    # Filter by specific analysis_type if not "all"
    keep_idx <- sapply(model_dirs, function(x) {
      grepl(analysis_mode, basename(x$path), ignore.case = TRUE)
    })
    model_dirs <- model_dirs[keep_idx]
    cat("Filtered to", length(model_dirs), analysis_mode, "directories\n")
  }
  
  if (length(model_dirs) == 0) {
    stop("No model directories found in ", models_dir, " subdirectories (akd, lbw)",
         if (analysis_mode != "all") paste0(" matching analysis_mode '", analysis_mode, "'") else "")
  }
  
  # Extract metrics from each directory
  all_metrics_list <- list()
  for (model_dir_info in model_dirs) {
    metrics <- extract_metrics_from_dir(model_dir_info, outcome_type)
    if (!is.null(metrics)) {
      all_metrics_list[[length(all_metrics_list) + 1]] <- metrics
    }
  }
  
  cat("\n")
  
  # Combine and expand metrics
  metrics_result <- combine_and_expand_metrics(all_metrics_list, outcome_type)
  combined_metrics <- metrics_result$metrics
  unique_dir_version_pairs <- metrics_result$unique_dir_version_pairs
  unique_combinations <- metrics_result$unique_combinations
  
  cat("  Found", nrow(unique_dir_version_pairs), "versions,", nrow(unique_combinations), "combinations\n")
  
  # Add evac_type column if evac_analysis (identify_best_models will auto-group by it)
  if (analysis_mode == "evac_analysis" && nrow(combined_metrics) > 0) {
    combined_metrics <- combined_metrics %>%
      mutate(
        evac_type = case_when(
          grepl("eaton", version, ignore.case = TRUE) ~ "eaton",
          grepl("palisades", version, ignore.case = TRUE) ~ "palisades",
          TRUE ~ NA_character_
        )
      ) %>%
      select(evac_type, everything())
    cat("  Added evac_type column\n")
  }
  
  # Create best-model-selection directory if it doesn't exist
  # Use subfolder for non-"all" analysis modes
  model_comparisons_dir <- paste0(path_onedrive, "03_modeling-and-results/02_best-model-selection/")
  if (analysis_mode != "all") {
    model_comparisons_dir <- paste0(model_comparisons_dir, analysis_mode, "/")
  }
  dir.create(model_comparisons_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save output (with analysis_mode suffix for non-"all")
  file_suffix <- if (analysis_mode != "all") paste0("_", analysis_mode) else ""
  output_file <- paste0(model_comparisons_dir, "model_comparison", file_suffix, ".xlsx")
  writexl::write_xlsx(combined_metrics, output_file)
  cat("Saved metrics to:", output_file, "\n")
  
  #-------------------------------
  # Step-2: Identify top n model versions
  #-------------------------------
  cat("\n=== Step 2: Identify Best Models ===\n")
  # First identify top n models based on a metric (e.g. MASE) ----
  top_n_models <- identify_best_models(combined_metrics, r2_threshold = r2_threshold, 
                                        metric = metric, n = n)
  
  # Handle ties: keep only n models per combination
  if (analysis_mode == "evac_analysis" && nrow(top_n_models) > 0) {
    # evac_type is already in data from combined_metrics
    top_n_models <- top_n_models %>%
      group_by(evac_type, enc_type, cause) %>%
      slice_head(n = n) %>%
      ungroup() %>%
      select(evac_type, everything())
    cat("  Final:", nrow(top_n_models), "models (", n, "per combination)\n")
  }
  
  ## save top n models to xlsx
  top_n_models_file <- paste0(model_comparisons_dir, "model_comparison_top_n", file_suffix, ".xlsx")
  writexl::write_xlsx(top_n_models, top_n_models_file)
  cat("Saved top models to:", top_n_models_file, "\n")
  
  cat("\nStep 2 complete!\n\n")
  
  #-------------------------------
  # Step-3: Extract PDFs and configs for top n model versions
  #-------------------------------
  cat("=== Step 3: Extract PDFs and Configs ===\n")
  
  ## Extract PDFs and configs for top n models
  ## Create temp directory to extract the configs corresponding to the best models identified manually
  top_n_models_dir <- paste0(model_comparisons_dir, "model_configs_top_n", file_suffix, "/")
  
  # Clear existing directory to avoid accumulation from previous runs
  if (dir.exists(top_n_models_dir)) {
    unlink(top_n_models_dir, recursive = TRUE)
  }
  
  dir.create(top_n_models_dir, showWarnings = FALSE, recursive = TRUE)
  config_dir <- paste0(top_n_models_dir, "/configs/")
  dir.create(config_dir, showWarnings = FALSE)
  pdf_dir <- paste0(top_n_models_dir, "/plots/")
  dir.create(pdf_dir, showWarnings = FALSE)
  
  extract_best_model_files(top_n_models, models_dir, top_n_models_dir, 
                           pdf_dir = pdf_dir, config_dir = config_dir, 
                           multiple_versions = TRUE)
  
  cat("\nStep 3 complete!\n\n")
  
  cat("========================================\n")
  cat("Output directory:", model_comparisons_dir, "\n")
  cat("========================================\n\n")
  
  cat("NEXT STEPS:\n")
  cat("1. Inspect model_comparison_top_n", file_suffix, ".xlsx\n", sep = "")
  cat("2. Review plots in model_configs_top_n", file_suffix, "/plots/\n", sep = "")
  cat("3. Create model_versions_manual", file_suffix, ".xlsx with selected versions\n", sep = "")
  cat("4. Run run_final_outputs('", analysis_mode, "')\n", sep = "")
  
  # Return paths for reference
  invisible(list(
    model_comparisons_dir = model_comparisons_dir,
    metrics_file = output_file,
    top_n_file = top_n_models_file,
    top_n_dir = top_n_models_dir
  ))
}

# =============================================================================
# Run if executed directly (not just sourced)
# =============================================================================
# Uncomment one of the following to run:
# identify_top_models("all")           # Main analysis
# identify_top_models("evac_analysis") # Palisades + Eaton

########################
# MANUAL STEP: 
# 1. Inspect the model_comparison_top_n*.xlsx file
# 2. Visually inspect the plots corresponding to the top n versions
# 3. Select a version that fits the test period the best
# 4. Create a new excel file called `model_versions_manual_for_final_outputs_evac_analysis.xlsx`
#    with the selected versions for the final outputs
#    - For "all" analysis: model_versions_manual_for_final_outputs.xlsx
#    - For "evac_analysis": model_versions_manual_evac_analysis.xlsx
# 5. Optionally run 02_rerun_final_batches.R with updated config (e.g. higher n_sim) 
#    to generate final models
# 6. Run run_final_outputs() with the appropriate analysis_mode
##########################
