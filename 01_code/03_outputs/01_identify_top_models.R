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

# Read config to get user and outcome_type
outcome_type <- "rate"

#-------------------------------
# Step-1: Extract and combine all metrics
#-------------------------------
cat("=== Step 1: Extract and Combine Metrics ===\n")
models_dir <- here(path_onedrive, "03_modeling-and-results/01_modeling/")

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

# Create best-model-selection directory if it doesn't exist
model_comparisons_dir <- paste0(path_onedrive, "03_modeling-and-results/02_best-model-selection/")
dir.create(model_comparisons_dir, recursive = TRUE, showWarnings = FALSE)

# Save output
output_file <- paste0(model_comparisons_dir, "/", "model_comparison.xlsx")
writexl::write_xlsx(combined_metrics, output_file)

#-------------------------------
# Step-2: Identify top n model versions
#-------------------------------
cat("=== Step 2: Identify Best Models ===\n")
# First identify top n models based on a metric (e.g. MASE) ----
top_n_models <- identify_best_models(combined_metrics, r2_threshold = 0.05, metric = "MASE", n = 6)

## save top n models to xlsx
top_n_models_file <- paste0(model_comparisons_dir, "/", "model_comparison_top_n.xlsx")
writexl::write_xlsx(top_n_models, top_n_models_file)

cat("\nStep 2 complete!\n\n")
#-------------------------------
# Step-3: Extract PDFs and configs for top n model versions
#-------------------------------

## Extract PDFs and configs for top n models
## Create temp directory to extract the configs corresponding to the best models identified manually
top_n_models_dir <- paste0(model_comparisons_dir, "/model_configs_top_n/")
dir.create(top_n_models_dir, showWarnings = FALSE, recursive = TRUE)
config_dir <- paste0(top_n_models_dir, "/configs/")
dir.create(config_dir, showWarnings = FALSE)
pdf_dir <- paste0(top_n_models_dir, "/plots/")
dir.create(pdf_dir, showWarnings = FALSE)

extract_best_model_files(top_n_models, models_dir, top_n_models_dir, 
                         pdf_dir = pdf_dir, config_dir = config_dir, 
                         multiple_versions = TRUE)

cat("\nStep 3 complete!\n\n")

########################
# MANUAL STEP: At this stage, inspect the model_comparison_top_n.xlsx file 
## visually inspect the plots corresponding to the top n versions 
## select a version that fits the test period the best
## create a new excel file called `model_versions_manual_for_rerun.xlsx` with the selected versions
## optionally run 03_outputs/03_rerun_final_batches.R with updated config e.g. higher sum_sims to generate final models.
##########################

