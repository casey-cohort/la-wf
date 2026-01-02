# LA wildfires project
# Compare model performance across all exposure versions

# Initial Setup ----
pacman::p_load(tidyverse, here)
## Set paths
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/utils_general.R"))
## Read config to get user
config_file <- paste0(getwd(), "/01_code/02_analysis/model_config.yaml")
config <- yaml::read_yaml(config_file)
outcome_type <- config$outcome_type

# Step-1: Extract and combine all metrics ----
## Go one directory up to get the parent directory
models_dir <- here(path_onedrive, "02_output/models/")
cat("Looking for model directories in:", models_dir, "\n\n")

## Define subdirectories to search (akd and lbw)
subdirs <- c("akd", "lbw")

## Find all model directories across subdirectories
model_dirs <- list()
for (subdir in subdirs) {
  subdir_path <- paste0(models_dir, "/", subdir, "/")
  if (dir.exists(subdir_path)) {
    all_dirs <- list.dirs(subdir_path, full.names = TRUE, recursive = FALSE)
    # Accept all directories (no filtering)
    # Store with subdirectory name for tracking
    for (d in all_dirs) {
      model_dirs[[length(model_dirs) + 1]] <- list(
        path = d,
        source_dir = subdir
      )
    }
  }
}

cat("Found", length(model_dirs), "model directories:\n")
for (d in model_dirs) {
  cat("  -", d$source_dir, "/", basename(d$path), "\n")
}
cat("\n")

if (length(model_dirs) == 0) {
  stop("No model directories found in ", models_dir, " subdirectories (akd, lbw)")
}

## Process each directory
all_metrics_list <- list()
# model_dir_info <- model_dirs[[1]]
for (model_dir_info in model_dirs) {
  model_dir <- model_dir_info$path
  source_dir <- model_dir_info$source_dir
  version <- basename(model_dir)
  cat("Processing:", source_dir, "/", version, "\n")
  
  # Find performance metrics file in tables subdirectory
  tables_dir <- paste0(model_dir, "/tables/")
  
  if (!dir.exists(tables_dir)) {
    cat("  WARNING: No tables directory found, skipping\n")
    next
  }
  
  # Find performance_metrics_with_mbb_*.csv file
  metrics_files <- list.files(
    tables_dir, 
    pattern = "^performance_metrics_with_mbb_.*\\.csv$", 
    full.names = TRUE
  )
  
  if (length(metrics_files) == 0) {
    cat("  WARNING: No performance_metrics_with_mbb_*.csv file found, skipping\n")
    next
  }
  
  # Use the first matching file (there should only be one)
  metrics_file <- metrics_files[1]
  cat("  Loading:", basename(metrics_file), "\n")
  
  tryCatch({
    # Read metrics file
    metrics_df <- read.csv(metrics_file, stringsAsFactors = FALSE)
    # print(metrics_df)
    # Filter for test window only
    test_metrics <- metrics_df %>%
      filter(window == "test") %>%
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

    cat("  Found", nrow(test_metrics), "test window records\n")
    
    # Add to list
    all_metrics_list[[length(all_metrics_list) + 1]] <- test_metrics
    
  }, error = function(e) {
    cat("  ERROR reading file:", as.character(e), "\n")
  })
}

cat("\n")

## Combine all results
if (length(all_metrics_list) == 0) {
  stop("No metrics data could be extracted from any model directories")
}

combined_metrics <- bind_rows(all_metrics_list)

## Filter out cases where cause does not start with outcome_type
combined_metrics <- combined_metrics %>%
  filter(grepl(paste0("^", outcome_type, "_"), cause))

## Get all unique (source_dir, version) pairs that actually exist
unique_dir_version_pairs <- combined_metrics %>%
  distinct(source_dir, version)

## Get all unique combinations of enc_type, exposure_category, cause across all data
unique_combinations <- combined_metrics %>%
  distinct(enc_type, exposure_category, cause)

cat("  Unique directory/version pairs:", nrow(unique_dir_version_pairs), "\n")
cat("  Unique combinations (enc_type × exposure_category × cause):", nrow(unique_combinations), "\n")

## For each directory/version pair, create rows for all combinations
complete_grid <- unique_dir_version_pairs %>%
  crossing(unique_combinations)

cat("  Total rows in complete grid:", nrow(complete_grid), "\n")
cat("  (", nrow(unique_dir_version_pairs), "versions ×", nrow(unique_combinations), "combinations )\n")

## Left join actual data onto complete grid
## This fills missing combinations with NA
combined_metrics <- complete_grid %>%
  left_join(
    combined_metrics, 
    by = c("source_dir", "version", "enc_type", "exposure_category", "cause")
  )

cat("  Combinations with actual data:", sum(!is.na(combined_metrics$R2)), "\n")
cat("  Combinations with missing data:", sum(is.na(combined_metrics$R2)), "\n\n")

## Sort by source directory, version, then by combination details
combined_metrics <- combined_metrics %>%
  arrange(enc_type, exposure_category, cause, source_dir, desc(version))

cat("=== Summary ===\n")
cat("Total versions processed:", length(all_metrics_list), "\n")
cat("Total records in comparison:", nrow(combined_metrics), "\n")
cat("\n")

## Save output
output_file <- paste0(models_dir, "/", "model_comparison.csv")
write.csv(combined_metrics, output_file, row.names = FALSE)

# Step-2: Identify best models ---- 
## Use the combined_metrics dataframe to identify the best models
best_models <- combined_metrics %>%
  group_by(enc_type, exposure_category, cause) %>%
  mutate(
    # Flag models with positive R2
    has_positive_r2 = !is.na(R2) & R2 > 0,
    # Flag models with valid MAPE (not NA and finite, excludes Inf)
    has_valid_smape = !is.na(sMAPE), # removed '& is.finite(MAPE)'
    # Flag valid models (both positive R2 and valid MAPE)
    is_valid_model = has_positive_r2 & has_valid_smape,
    # Check if any valid models exist in this group
    has_any_valid = any(has_positive_r2)
  ) %>%
  # Select best model: if valid models exist, pick lowest sMAPE; otherwise keep first row and set metrics to NA
  group_modify(~ {
    if (.x$has_any_valid[1]) {
      # Filter to valid models and select lowest sMAPE
      .x %>%
        filter(is_valid_model) %>%
        slice_min(sMAPE, n = 1, with_ties = FALSE)
    } else {
      # No valid models - keep first row but set metrics to NA
      .x %>%
        slice(1) %>%
        mutate(R2 = NA_real_, MAPE = NA_real_, sMAPE = NA_real_)
    }
  }) %>%
  ungroup() %>%
  select(-has_positive_r2, -has_valid_smape, -is_valid_model, -has_any_valid)

## Save best models output
best_models_file <- paste0(models_dir, "/", "model_comparison_best.csv")
write.csv(best_models, best_models_file, row.names = FALSE)
cat("Best models saved to:", best_models_file, "\n")

# Step-3: Identify models with R2 above threshold ---- 
## Set R2 threshold (models with R2 above this threshold will be kept)
r2_threshold <- 0.15  # Adjust this value as needed

## Use the combined_metrics dataframe to identify models with R2 above threshold
r2_above_threshold <- combined_metrics %>%
  group_by(enc_type, exposure_category, cause) %>%
  mutate(
    # Flag models with R2 above threshold
    is_valid_model = !is.na(R2) & R2 > r2_threshold,
  ) %>%
  # Filter to keep all models that meet the R2 threshold
  filter(is_valid_model) %>%
  ungroup() %>%
  select(-is_valid_model)

## Unique combinations of enc_type, exposure_category, cause
unique_combinations_threshold <- r2_above_threshold %>%
  distinct(enc_type, exposure_category, cause)

cat("  Unique combinations (enc_type × exposure_category × cause):", nrow(unique_combinations), "\n")

## Combinations that dont meet the threshold
unique_combinations_not_threshold <- unique_combinations %>%
  anti_join(unique_combinations_threshold, by = c("enc_type", "exposure_category", "cause"))

cat("  Combinations that dont meet the threshold:\n", nrow(unique_combinations_not_threshold), "\n")
print(unique_combinations_not_threshold)


## Save models with R2 above threshold
r2_above_threshold_file <- paste0(models_dir, "/", "model_comparison_r2_above_threshold.csv")
write.csv(r2_above_threshold, r2_above_threshold_file, row.names = FALSE)
cat("Models with R2 >", r2_threshold, "saved to:", r2_above_threshold_file, "\n")
