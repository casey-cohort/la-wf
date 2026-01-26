#-------------------------------
# LA wildfires project
# Generate final outputs - extract best model PDFs and configs from manual selection
#-------------------------------

# Note: Before running this script, manually update 'best_model_versions_manual.xlsx' with the final models to use.

# Initial Setup ----
pacman::p_load(tidyverse, here, yaml, gt, writexl)

## Set paths
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_general.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_outputs.R"))

## Get analysis_mode from environment (set by run_final_outputs) or default to "all"
analysis_mode <- Sys.getenv("ANALYSIS_MODE", unset = "all")
if (analysis_mode == "") analysis_mode <- "all"
cat("Analysis mode:", analysis_mode, "\n\n")

## Set outcome type
outcome_type <- "rate"

## Set aggregation durations (array of days from Jan 7 for aggregation)
## Each value will produce a separate aggregated output file
agg_duration_array <- c(2, 3, 4, 5, 6, 7, 14, 21, 25)

## Define directory paths (with analysis_mode subfolder for non-"all")
models_dir <- here(path_onedrive, "03_modeling-and-results/01_modeling/")
model_comparisons_dir <- paste0(path_onedrive, "03_modeling-and-results/02_best-model-selection/")
if (analysis_mode != "all") {
  model_comparisons_dir <- paste0(model_comparisons_dir, analysis_mode, "/")
}

## Create outputs directory (with analysis_mode subfolder for non-"all")
outputs_dir <- paste0(path_onedrive, "03_modeling-and-results/03_bested-models/")
if (analysis_mode != "all") {
  outputs_dir <- paste0(outputs_dir, analysis_mode, "/")
}

## Create configs directory
configs_dir <- paste0(outputs_dir, "configs/")
dir.create(configs_dir, showWarnings = FALSE, recursive = TRUE)

## Create plots directory
plots_dir <- paste0(outputs_dir, "plots/")
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

## Create excess hospitalizations base directory (with analysis_mode subfolder for non-"all")
tables_dir <- paste0(path_onedrive, "03_modeling-and-results/04_bested-results/")
if (analysis_mode != "all") {
  tables_dir <- paste0(tables_dir, analysis_mode, "/")
}
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

## Create daily subdirectory (one xlsx per exposure combination with all daily data)
daily_dir <- paste0(tables_dir, "daily/")
dir.create(daily_dir, recursive = TRUE, showWarnings = FALSE)

## Create aggregated subdirectory (one xlsx per duration with all combinations)
aggregated_dir <- paste0(tables_dir, "aggregated/")
dir.create(aggregated_dir, recursive = TRUE, showWarnings = FALSE)


#-------------------------------
# Step-1: Extract final PDFs and Configs
#-------------------------------
cat("=== Step 1: Extract Final PDFs and Configs ===\n")

## Read manual best model selection xlsx (different file for different analysis modes)
if (analysis_mode == "all") {
  xlsx_file <- paste0(model_comparisons_dir, "model_versions_manual_for_final_outputs.xlsx")
} else {
  xlsx_file <- paste0(model_comparisons_dir, "model_versions_manual_for_final_outputs_", analysis_mode, ".xlsx")
}

if (!file.exists(xlsx_file)) {
  stop("XLSX file not found: ", xlsx_file, "\n",
       "Please create best_model_versions_manual.xlsx in ", model_comparisons_dir)
}

cat("Reading best model selections from:", xlsx_file, "\n")
best_models_manual <- readxl::read_xlsx(xlsx_file) |> as.data.frame()

# Validate required columns
required_cols <- c("enc_type", "exposure_category", "cause", "source_dir", "version")
missing_cols <- setdiff(required_cols, colnames(best_models_manual))

if (length(missing_cols) > 0) {
  stop("XLSX file is missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
       "Required columns: ", paste(required_cols, collapse = ", "))
}

cat("  Found", nrow(best_models_manual), "model(s) to extract\n")

# Extract PDFs and configs to separate directories
extract_best_model_files(best_models_manual, models_dir, outputs_dir, 
                         pdf_dir = plots_dir, config_dir = configs_dir)

cat("\nStep 1 complete!\n\n")

#-------------------------------
# Step-2: Calculate Excess Hospitalizations for Final Models
#-------------------------------
cat("=== Step 2: Calculate Excess Hospitalizations ===\n")

# Set CI parameters (matching model_config.yaml)
ci_method <- "quantile"
ci_level <- 0.95

# Initialize storage for daily excess results (one per combination)
all_daily_results <- list()

# Initialize storage for aggregated results (one list per duration)
all_aggregated_results <- list()
for (dur in agg_duration_array) {
  all_aggregated_results[[paste0("days_", dur)]] <- list()
}

# Check if evac_type column exists (for evac_analysis mode)
has_evac_type <- "evac_type" %in% colnames(best_models_manual)

# Process each best model
for (i in 1:nrow(best_models_manual)) {
  row <- best_models_manual[i, ]
  enc_type <- row$enc_type
  exposure_category <- row$exposure_category
  cause <- row$cause
  source_dir <- row$source_dir
  version <- row$version
  evac_type <- if (has_evac_type) row$evac_type else NA_character_
  
  if (has_evac_type && !is.na(evac_type)) {
    cat("Processing:", enc_type, "-", exposure_category, "-", cause, "-", evac_type, "\n")
  } else {
    cat("Processing:", enc_type, "-", exposure_category, "-", cause, "\n")
  }
  
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
    
    # Create combination key for storage (include evac_type if present)
    if (has_evac_type && !is.na(evac_type)) {
      combo_key <- paste(enc_type, exposure_category, cause, evac_type, sep = "_")
    } else {
      combo_key <- paste(enc_type, exposure_category, cause, sep = "_")
    }
    
    # Calculate daily excess (all days, no aggregation)
    daily_results <- calc_excess_from_mbb(
      mbb_result = holdout_MBB,
      outcome_type = outcome_type,
      ci_method = ci_method,
      ci_level = ci_level,
      num_days_agg = NULL  # Get all days
    )
    
    if (is.null(daily_results)) {
      cat("  WARNING: Daily excess calculation returned NULL\n")
      cat("  Skipping this model\n\n")
      next
    }
    
    # Store daily results with metadata
    daily_with_meta <- daily_results$daily_excess %>%
      mutate(
        enc_type = enc_type,
        exposure_category = exposure_category,
        cause = cause,
        source_dir = source_dir,
        version = version
      )
    # Add evac_type column if in evac_analysis mode
    if (has_evac_type && !is.na(evac_type)) {
      daily_with_meta <- daily_with_meta %>%
        mutate(evac_type = evac_type) %>%
        select(evac_type, everything())
    }
    all_daily_results[[combo_key]] <- daily_with_meta
    
    # Calculate aggregated results for each duration
    for (dur in agg_duration_array) {
      agg_results <- calc_excess_from_mbb(
        mbb_result = holdout_MBB,
        outcome_type = outcome_type,
        ci_method = ci_method,
        ci_level = ci_level,
        num_days_agg = dur
      )
      
      if (!is.null(agg_results)) {
        # Store period excess with metadata
        period_with_meta <- agg_results$period_excess %>%
          mutate(
            enc_type = enc_type,
            exposure_category = exposure_category,
            cause = cause,
            source_dir = source_dir,
            version = version
          )
        # Add evac_type column if in evac_analysis mode
        if (has_evac_type && !is.na(evac_type)) {
          period_with_meta <- period_with_meta %>%
            mutate(evac_type = evac_type) %>%
            select(evac_type, everything())
        }
        dur_key <- paste0("days_", dur)
        all_aggregated_results[[dur_key]][[combo_key]] <- period_with_meta
      }
    }
    
    cat("  Processed daily and aggregated results\n\n")
    
  }, error = function(e) {
    cat("  ERROR processing model:", as.character(e), "\n")
    cat("  Skipping this model\n\n")
  })
}

# Save excess hospitalizations to new directory structure
if (length(all_daily_results) > 0) {
  cat("\n=== Saving Excess Hospitalizations ===\n")
  
  # ----- Save Daily Files (one per exposure combination) -----
  cat("\n--- Saving Daily Files ---\n")
  for (combo_key in names(all_daily_results)) {
    daily_xlsx <- paste0(daily_dir, "daily_excess_", combo_key, ".xlsx")
    writexl::write_xlsx(all_daily_results[[combo_key]], daily_xlsx)
  }
  cat("Saved", length(all_daily_results), "daily files to:", daily_dir, "\n")
  
  # ----- Save Aggregated Files (one per duration) -----
  cat("\n--- Saving Aggregated Files ---\n")
  for (dur in agg_duration_array) {
    dur_key <- paste0("days_", dur)
    
    if (length(all_aggregated_results[[dur_key]]) > 0) {
      # Combine all combinations for this duration
      agg_df <- bind_rows(all_aggregated_results[[dur_key]])
      
      # Select standard columns for output (include evac_type if present)
      if ("evac_type" %in% colnames(agg_df)) {
        agg_df <- agg_df %>%
          select(evac_type, enc_type, exposure_category, cause, period, 
                 observed, expected_CI, excess_CI, excess_pct_CI,
                 source_dir, version)
      } else {
        agg_df <- agg_df %>%
          select(enc_type, exposure_category, cause, period, 
                 observed, expected_CI, excess_CI, excess_pct_CI,
                 source_dir, version)
      }
      
      # Save to aggregated directory with suggestive filename
      agg_xlsx <- paste0(aggregated_dir, "excess_hosp_", dur, "days.xlsx")
      writexl::write_xlsx(agg_df, agg_xlsx)
      cat("Saved", nrow(agg_df), "rows to:", agg_xlsx, "\n")
    }
  }
  
  # ----- Create HTML summary table for default duration -----
  # Use the first duration in array for the HTML summary
  default_dur <- agg_duration_array[1]
  default_dur_key <- paste0("days_", default_dur)
  
  if (length(all_aggregated_results[[default_dur_key]]) > 0) {
    excess_summary <- bind_rows(all_aggregated_results[[default_dur_key]])
    
    # Select columns (include evac_type if present)
    if ("evac_type" %in% colnames(excess_summary)) {
      excess_summary <- excess_summary %>%
        select(evac_type, enc_type, exposure_category, cause, period, 
               observed, expected_CI, excess_CI, excess_pct_CI)
    } else {
      excess_summary <- excess_summary %>%
        select(enc_type, exposure_category, cause, period, 
               observed, expected_CI, excess_CI, excess_pct_CI)
    }
    
    gt_excess <- excess_summary %>%
      gt() %>%
      tab_header(
        title = "Excess Hospitalizations Summary with MBB CIs - Final Models",
        subtitle = paste0("Aggregation: ", default_dur, " days | Outcome Type: ", outcome_type)
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
    
    excess_html <- paste0(tables_dir, "excess_hospitalizations_summary.html")
    gtsave(gt_excess, excess_html)
    cat("\nHTML summary table saved to:", excess_html, "\n")
  }
  
} else {
  cat("\nWARNING: No excess hospitalization data was collected.\n")
  cat("  Check that best models have holdout period data.\n")
}

cat("\nStep 2 complete!\n\n")

