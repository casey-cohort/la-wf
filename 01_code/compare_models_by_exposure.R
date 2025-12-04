#-------------------------------
# LA wildfires project
# Compare model performance across x40 exposure versions
#-------------------------------

# Setup ----
pacman::p_load(tidyverse, here)

# Set paths
source(paste0(getwd(), "/01_code/paths.R"))

# Define models directory ----
models_dir <- paste0(path_onedrive, "02_output/models/")

cat("Looking for model directories in:", models_dir, "\n\n")

# Find all x40 directories ----
all_dirs <- list.dirs(models_dir, full.names = TRUE, recursive = FALSE)

# Filter for directories containing x40 (indicating 40 exposures)
x40_dirs <- all_dirs[grepl("x40", basename(all_dirs))]

cat("Found", length(x40_dirs), "directories with x40 exposures:\n")
for (d in x40_dirs) {
  cat("  -", basename(d), "\n")
}
cat("\n")

if (length(x40_dirs) == 0) {
  stop("No x40 model directories found in ", models_dir)
}

# Process each directory ----
all_metrics_list <- list()
# model_dir <- x40_dirs[1]
for (model_dir in x40_dirs) {
  version <- basename(model_dir)
  cat("Processing:", version, "\n")
  
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
      mutate(version = version) %>%
      select(
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
    all_metrics_list[[version]] <- test_metrics
    
  }, error = function(e) {
    cat("  ERROR reading file:", as.character(e), "\n")
  })
}

cat("\n")

# Combine all results ----
if (length(all_metrics_list) == 0) {
  stop("No metrics data could be extracted from any x40 directories")
}

combined_metrics <- bind_rows(all_metrics_list)

# sort by exposure category
combined_metrics <- combined_metrics %>%
  arrange(enc_type, exposure_category, cause)

cat("=== Summary ===\n")
cat("Total versions processed:", length(all_metrics_list), "\n")
cat("Total records in comparison:", nrow(combined_metrics), "\n")
cat("\n")

# Save output ----
output_file <- paste0(models_dir, "model_comparison.csv")
write.csv(combined_metrics, output_file, row.names = FALSE)
