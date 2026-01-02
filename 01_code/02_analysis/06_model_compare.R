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

# Save output
output_file <- paste0(models_dir, "/", "model_comparison.csv")
write.csv(combined_metrics, output_file, row.names = FALSE)

#-------------------------------
# Step-2: Identify best models
#-------------------------------
cat("=== Step 2: Identify Best Models ===\n")
best_models <- identify_best_models(combined_metrics)

# Ensure only one row per combination (safety check)
best_models <- best_models %>%
  group_by(enc_type, exposure_category, cause) %>%
  slice(1) %>%
  ungroup()

# Save best models output
best_models_file <- paste0(models_dir, "/", "model_comparison_best.csv")
write.csv(best_models, best_models_file, row.names = FALSE)
cat("  Best models:", nrow(best_models), "unique combinations\n\n")

#-------------------------------
# Step-3: Extract PDFs and configs for best models
#-------------------------------
cat("=== Step 3: Extract Best Model PDFs and Configs ===\n")
bested_dir <- paste0(models_dir, "/bested/")
extract_best_model_files(best_models, models_dir, bested_dir)

#-------------------------------
# Step-4: Identify bested models with R2 above threshold
#-------------------------------
cat("=== Step 4: Filter Bested Models by R2 Threshold ===\n")
r2_threshold <- 0.15  # Adjust this value as needed

# Filter bested models (best model per combination) with R2 above threshold
# Ensure only one row per combination (safety check)
r2_above_threshold <- best_models %>%
  filter(!is.na(R2) & R2 > r2_threshold) %>%
  group_by(enc_type, exposure_category, cause) %>%
  slice(1) %>%
  ungroup()

# Get unique combinations that meet threshold
unique_combinations_threshold <- r2_above_threshold %>%
  distinct(enc_type, exposure_category, cause)

# Print diagnostics
print_threshold_diagnostics(unique_combinations, unique_combinations_threshold)

# Save bested models with R2 above threshold
r2_above_threshold_file <- paste0(models_dir, "/", "model_comparison_r2_above_threshold.csv")
write.csv(r2_above_threshold, r2_above_threshold_file, row.names = FALSE)

cat("\n=== Model Comparison Complete ===\n")

