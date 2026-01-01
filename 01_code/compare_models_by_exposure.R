#-------------------------------
# LA wildfires project
# Compare model performance across all exposure versions
#-------------------------------

# Setup ----
pacman::p_load(tidyverse, here)

# Set paths
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/utils_general.R"))

# Define models directory ----
# Read config to get user
config_file <- paste0(getwd(), "/01_code/02_analysis/model_config.yaml")
config <- yaml::read_yaml(config_file)
models_dir <- get_models_path(path_onedrive, user = config$user)
# go one directory up to get the parent directory
models_dir <- dirname(models_dir)
cat("Looking for model directories in:", models_dir, "\n\n")

# Define subdirectories to search (akd and lbw)
subdirs <- c("akd", "lbw")
subdir <- "akd"
# Find all model directories across subdirectories ----
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

# Process each directory ----
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
        # MASE = mase,
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

# Combine all results ----
if (length(all_metrics_list) == 0) {
  stop("No metrics data could be extracted from any model directories")
}

combined_metrics <- bind_rows(all_metrics_list)

# Create complete grid of all possible combinations ----
cat("Creating complete grid of all combinations...\n")

# Get all unique (source_dir, version) pairs that actually exist
unique_dir_version_pairs <- combined_metrics %>%
  distinct(source_dir, version)

# Get all unique combinations of enc_type, exposure_category, cause across all data
unique_combinations <- combined_metrics %>%
  distinct(enc_type, exposure_category, cause)

cat("  Unique directory/version pairs:", nrow(unique_dir_version_pairs), "\n")
cat("  Unique combinations (enc_type × exposure_category × cause):", nrow(unique_combinations), "\n")

# For each directory/version pair, create rows for all combinations
complete_grid <- unique_dir_version_pairs %>%
  crossing(unique_combinations)

cat("  Total rows in complete grid:", nrow(complete_grid), "\n")
cat("  (", nrow(unique_dir_version_pairs), "versions ×", nrow(unique_combinations), "combinations )\n")

# Left join actual data onto complete grid
# This fills missing combinations with NA
combined_metrics <- complete_grid %>%
  left_join(
    combined_metrics, 
    by = c("source_dir", "version", "enc_type", "exposure_category", "cause")
  )

cat("  Combinations with actual data:", sum(!is.na(combined_metrics$R2)), "\n")
cat("  Combinations with missing data:", sum(is.na(combined_metrics$R2)), "\n\n")

# Sort by source directory, version, then by combination details
combined_metrics <- combined_metrics %>%
  arrange(enc_type, exposure_category, cause, source_dir, version)

cat("=== Summary ===\n")
cat("Total versions processed:", length(all_metrics_list), "\n")
cat("Total records in comparison:", nrow(combined_metrics), "\n")
cat("\n")

# Save output ----
output_file <- paste0(models_dir, "/", "model_comparison.csv")
write.csv(combined_metrics, output_file, row.names = FALSE)


# Step-2: Identify best models
# Read the model_comparison.csv file (or use the dataframe already in memory)
best_models <- combined_metrics %>%
  group_by(enc_type, exposure_category, cause) %>%
  mutate(
    # Create a helper column: use R2 if not NA, otherwise use -Inf
    r2_for_sorting = if_else(is.na(R2), -Inf, R2),
    # Check if all R2 values in this group are NA
    all_na = all(is.na(R2))
  ) %>%
  # If all NA, keep first row; otherwise keep row with max R2
  slice(if (first(all_na)) 1 else which.max(r2_for_sorting)) %>%
  ungroup() %>%
  select(-r2_for_sorting, -all_na)

cat("=== Best Models Selection ===\n")
cat("Total unique combinations:", nrow(best_models), "\n")
cat("Combinations with valid R2:", sum(!is.na(best_models$R2)), "\n")
cat("Combinations with NA R2 (first row selected):", sum(is.na(best_models$R2)), "\n\n")

# Save best models output
best_models_file <- paste0(models_dir, "/", "model_comparison_best.csv")
write.csv(best_models, best_models_file, row.names = FALSE)
cat("Best models saved to:", best_models_file, "\n")
