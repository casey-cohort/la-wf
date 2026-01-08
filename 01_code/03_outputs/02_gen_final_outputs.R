#-------------------------------
# LA wildfires project
# Generate final outputs - extract best model PDFs and configs from manual selection
#-------------------------------

# Initial Setup ----
pacman::p_load(tidyverse, here, yaml, gt)

# Set paths
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_general.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_outputs.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_best_tuned.R"))


# Read config to get user and outcome_type
config_file <- paste0(getwd(), "/01_code/02_analysis/model_config.yaml")
config <- yaml::read_yaml(config_file)
outcome_type <- config$outcome_type
ci_method <- config$ci_params$method
ci_level <- config$ci_params$level

# Define directory paths
models_dir <- here(path_onedrive, "02_output/models/")
model_comparisons_dir <- paste0(models_dir, "/model_comparisons/")

#-------------------------------
# Step-1: Extract final PDFs and Configs
#-------------------------------
cat("=== Step 1: Extract Final PDFs and Configs ===\n")

# Read manual best model selection CSV
csv_file <- paste0(model_comparisons_dir, "model_comparison_best_manual.csv")

if (!file.exists(csv_file)) {
  stop("CSV file not found: ", csv_file, "\n",
       "Please create model_comparison_best_manual.csv in ", model_comparisons_dir)
}

cat("Reading best model selections from:", csv_file, "\n")
best_models_manual <- read.csv(csv_file, stringsAsFactors = FALSE)

# Validate required columns
required_cols <- c("enc_type", "exposure_category", "cause", "source_dir", "version")
missing_cols <- setdiff(required_cols, colnames(best_models_manual))

if (length(missing_cols) > 0) {
  stop("CSV file is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
       "Required columns: ", paste(required_cols, collapse = ", "))
}

cat("  Found", nrow(best_models_manual), "model(s) to extract\n")

# Extract PDFs and configs to separate directories
bested_final_dir <- paste0(models_dir, "/bested_final/")
configs_dir <- paste0(bested_final_dir, "/configs/")
plots_dir <- paste0(bested_final_dir, "/plots/")
extract_best_model_files(best_models_manual, models_dir, bested_final_dir, 
                         pdf_dir = plots_dir, config_dir = configs_dir)

cat("\nStep 1 complete!\n\n")

#-------------------------------
# Step-2: Calculate Excess Hospitalizations for Final Models
#-------------------------------
cat("=== Step 2: Calculate Excess Hospitalizations ===\n")

# Create tables subdirectory in bested_final
tables_dir <- paste0(bested_final_dir, "excess_hospitalizations/")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# Create subdirectory for individual files
individual_files_dir <- paste0(tables_dir, "individual_files/")
dir.create(individual_files_dir, recursive = TRUE, showWarnings = FALSE)

# Initialize storage for excess results
all_excess_results <- list()

# Process each best model
for (i in 1:nrow(best_models_manual)) {
  row <- best_models_manual[i, ]
  enc_type <- row$enc_type
  exposure_category <- row$exposure_category
  cause <- row$cause
  source_dir <- row$source_dir
  version <- row$version
  
  cat("Processing:", enc_type, "-", exposure_category, "-", cause, "\n")
  
  # Construct path to MBB results
  model_version_dir <- paste0(models_dir, source_dir, "/", version, "/")
  
  # Extract mod_ver_suffix from version (removes "model_run_" prefix)
  mod_ver_suffix <- extract_version_suffix(version)
  mbb_file <- paste0(model_version_dir, "results/mbb_results_nested_", mod_ver_suffix, ".rds")
  
  if (!file.exists(mbb_file)) {
    cat("  WARNING: MBB results file not found:", mbb_file, "\n")
    cat("  Skipping this model\n\n")
    next
  }
  
  tryCatch({
    # Load MBB results
    mbb_results <- readRDS(mbb_file)
    
    # Extract the specific model's results
    if (!enc_type %in% names(mbb_results) ||
        !exposure_category %in% names(mbb_results[[enc_type]]) ||
        !cause %in% names(mbb_results[[enc_type]][[exposure_category]])) {
      cat("  WARNING: Model combination not found in MBB results\n")
      cat("  Skipping this model\n\n")
      next
    }
    
    result <- mbb_results[[enc_type]][[exposure_category]][[cause]]
    
    # Check if MBB was successful
    if (!isTRUE(result$success)) {
      cat("  WARNING: MBB was not successful for this model\n")
      if (!is.null(result$error)) {
        cat("  Error:", result$error, "\n")
      }
      cat("  Skipping this model\n\n")
      next
    }
    
    # Check for holdout data
    holdout_MBB <- result$holdout_MBB
    if (is.null(holdout_MBB) || is.null(holdout_MBB$pred_summary) || 
        nrow(holdout_MBB$pred_summary) == 0) {
      cat("  WARNING: No holdout data available for this model\n")
      cat("  Skipping this model\n\n")
      next
    }
    
    cat("  Calculating excess hospitalizations...\n")
    
    # Calculate excess using the new utility function
    excess_results <- calc_excess_from_mbb(
      mbb_result = holdout_MBB,
      outcome_type = outcome_type,
      ci_method = ci_method,
      ci_level = ci_level
    )
    
    if (is.null(excess_results)) {
      cat("  WARNING: Excess calculation returned NULL\n")
      cat("  Skipping this model\n\n")
      next
    }
    
    # Combine daily and period results
    result_combined <- bind_rows(
      excess_results$period_excess,
      excess_results$daily_excess
    ) %>%
      mutate(
        enc_type = enc_type,
        exposure_category = exposure_category,
        cause = cause,
        source_dir = source_dir,
        version = version
      )
    
    # Store for aggregation
    combo_key <- paste(enc_type, exposure_category, cause, sep = "_")
    all_excess_results[[combo_key]] <- result_combined
    
    # Save individual excess hospitalization table
    excess_csv <- paste0(individual_files_dir, "excess_hosp_", enc_type, "_", exposure_category, "_", cause, ".csv")
    write.csv(result_combined, excess_csv, row.names = FALSE)
    cat("  Saved to:", excess_csv, "\n\n")
    
  }, error = function(e) {
    cat("  ERROR processing model:", as.character(e), "\n")
    cat("  Skipping this model\n\n")
  })
}

# Save combined excess hospitalizations
if (length(all_excess_results) > 0) {
  cat("\n=== Saving Combined Excess Hospitalizations ===\n")
  
  all_excess <- bind_rows(all_excess_results)
  
  # Save daily results (includes both period and daily rows)
  excess_daily_file <- paste0(individual_files_dir, "combined_excess_hospitalizations_daily.csv")
  write.csv(all_excess, excess_daily_file, row.names = FALSE)
  cat("Daily excess hospitalizations saved to:", excess_daily_file, "\n")
  cat("  Rows:", nrow(all_excess), "\n")
  
  # Extract period summary (first row of each group = period aggregate)
  excess_summary <- all_excess %>%
    group_by(enc_type, exposure_category, cause) %>%
    slice(1) %>%  # First row is the total period
    ungroup()
  
  # Save period results as CSV
  excess_period_file <- paste0(tables_dir, "excess_hospitalizations_period.csv")
  write.csv(excess_summary, excess_period_file, row.names = FALSE)
  cat("Period excess hospitalizations saved to:", excess_period_file, "\n")
  cat("  Rows:", nrow(excess_summary), "\n")
  
  # Create HTML table for summary (total period only)
  gt_excess <- excess_summary %>%
    select(enc_type, exposure_category, cause, period, observed, expected_CI, excess_CI, excess_pct_CI) %>%
    gt() %>%
    tab_header(
      title = "Excess Hospitalizations Summary with MBB CIs - Final Models",
      subtitle = paste0("Holdout Period (post Jan 7, 2025) | Outcome Type: ", outcome_type)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    cols_label(
      enc_type = "Encounter Type",
      exposure_category = "Exposure",
      cause = "Outcome",
      period = "Period",
      observed = "Observed",
      expected_CI = "Expected (95% CI)",
      excess_CI = "Excess (95% CI)",
      excess_pct_CI = "Excess % (95% CI)"
    )
  
  excess_html <- paste0(tables_dir, "excess_hospitalizations_period.html")
  gtsave(gt_excess, excess_html)
  cat("Period excess hospitalizations HTML table saved to:", excess_html, "\n")
} else {
  cat("\nWARNING: No excess hospitalization data was collected.\n")
  cat("  Check that best models have holdout period data.\n")
}

cat("\nStep 2 complete!\n\n")
