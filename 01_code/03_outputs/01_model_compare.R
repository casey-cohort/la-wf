#-------------------------------
# LA wildfires project
# Compare model performance across all exposure versions
#-------------------------------

# Initial Setup ----
pacman::p_load(tidyverse, here, yaml)

# Set paths
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_general.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_outputs.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_best_tuned.R"))


# Read config to get user and outcome_type
config_file <- paste0(getwd(), "/01_code/02_analysis/model_config.yaml")
config <- yaml::read_yaml(config_file)
outcome_type <- config$outcome_type

#-------------------------------
# Step-1: Extract and combine all metrics
#-------------------------------
cat("=== Step 1: Extract and Combine Metrics ===\n")
models_dir <- here(path_onedrive, "02_output/models/")

# Define subdirectories to search
subdirs <- c("akd", "lbw")

# Find all model directories
model_dirs <- find_model_directories(models_dir, subdirs)

cat("Found", length(model_dirs), "model directories\n")

if (length(model_dirs) == 0) {
  stop("No model directories found in ", models_dir, " subdirectories (akd, lbw)")
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

# Create model_comparisons directory if it doesn't exist
model_comparisons_dir <- paste0(models_dir, "/model_comparisons/")
dir.create(model_comparisons_dir, recursive = TRUE, showWarnings = FALSE)

# Save output
output_file <- paste0(model_comparisons_dir, "/", "model_comparison.csv")
write.csv(combined_metrics, output_file, row.names = FALSE)

#-------------------------------
# Step-2: Identify best models
#-------------------------------
cat("=== Step 2: Identify Best Models ===\n")
# First identify top n models based on a metric (e.g. MASE) ----
best_models <- identify_best_models(combined_metrics, r2_threshold = 0.05, metric = "MASE", n = 3)

## save top n models to csv
best_models_file <- paste0(model_comparisons_dir, "/", "model_comparison_best_n.csv")
write.csv(best_models, best_models_file, row.names = FALSE)

# Diagnose range of metrics for top n models ----
metric_range <- best_models |>
  group_by(enc_type, exposure_category, cause) |>
  summarize(
    MASE_min = min(MASE),
    MASE_max = max(MASE),
    sMAPE_min = min(sMAPE),
    sMAPE_max = max(sMAPE)
  ) |>
  ungroup() |>
  mutate(MASE_pct_change = (MASE_max - MASE_min) / MASE_min*100,
         sMAPE_pct_change = (sMAPE_max - sMAPE_min) / sMAPE_min*100) |>
  arrange(desc(MASE_pct_change), desc(sMAPE_pct_change)) |>
  filter(abs(MASE_pct_change) > 10 | abs(sMAPE_pct_change) > 10)
nrow(metric_range) # 16 models with >10% change in MASE or sMAPE among top 3 models

# Select best model per combination ----
## Based on the most recent version (i think this approach needs to be revisited)
best_models <- best_models |>
  group_by(enc_type, exposure_category, cause) |>
  slice_max(version, n = 1) |>
  ungroup()

## Save best models output
best_models_file <- paste0(model_comparisons_dir, "/", "model_comparison_best.csv")
write.csv(best_models, best_models_file, row.names = FALSE)
cat("  Best models:", nrow(best_models), "unique combinations\n\n")

#-------------------------------
# Step-3: Extract PDFs and configs for best models
#-------------------------------
cat("=== Step 3: Extract Best Model PDFs and Configs ===\n")
bested_dir <- paste0(models_dir, "/bested/")
configs_dir <- paste0(bested_dir, "/configs/")
plots_dir <- paste0(bested_dir, "/plots/")
extract_best_model_files(best_models, models_dir, bested_dir, 
                         pdf_dir = plots_dir, config_dir = configs_dir)

#-------------------------------
# Step-4: If needed, re-run batch of bested models
#-------------------------------
cat("=== Step 4: Re-run Batch of Bested Models ===\n")
models_to_run <- list(
  list(encounter_type = "ED", exposure_category = "high_smoke", cause = "rate_enc"),
  list(encounter_type = "ED", exposure_category = "none", cause = "rate_enc"))

# run_batch_bested(models_to_run, user = "akd", n_sim_mbb = 500, train_test_date = "2025-12-30")

