# Quick diagnostic script to find seed inconsistencies
# Run this to identify where seeds might differ between runs

cat("\n========================================\n")
cat("SEED DIAGNOSTIC SCRIPT\n")
cat("========================================\n\n")

# Source required utilities
source(paste0(getwd(), "/01_code/utils_general.R"))
source(paste0(getwd(), "/01_code/utils_tuning.R"))

# 1. Check config seed
cat("=== 1. Global Seed Check ===\n")
config <- read_config(paste0(getwd(), "/01_code/02_analysis/model_config.yaml"))
cat("Seed value:", config$seed, "\n")
cat("Seed type:", class(config$seed), "\n")
cat("Seed as integer:", as.integer(config$seed), "\n")
if (config$seed != as.integer(config$seed)) {
  cat("⚠ WARNING: Seed is not an integer!\n")
}
cat("\n")

# 2. Test gen_seed reproducibility
cat("=== 2. gen_seed() Reproducibility Test ===\n")
test_markers <- c("IP", "evac", "num_enc_resp", "test")
seeds <- replicate(100, gen_seed(config$seed, test_markers))
unique_seeds <- unique(seeds)
if (length(unique_seeds) == 1) {
  cat("✓ PASS: gen_seed() is reproducible\n")
  cat("  Generated seed:", unique_seeds[1], "\n")
} else {
  cat("✗ FAIL: gen_seed() produced", length(unique_seeds), "different seeds!\n")
  cat("  Unique seeds:", paste(unique_seeds, collapse = ", "), "\n")
  cat("  This is a CRITICAL bug - gen_seed() should always return the same value!\n")
}
cat("\n")

# 3. Check RNG state
cat("=== 3. RNG State Check ===\n")
cat("RNG kind:", paste(RNGkind(), collapse = ", "), "\n")
if (exists(".Random.seed")) {
  cat("Random seed exists, first 5 values:", paste(.Random.seed[1:5], collapse = ", "), "\n")
  cat("Random seed length:", length(.Random.seed), "\n")
} else {
  cat("No .Random.seed set (will be created on first random operation)\n")
}
cat("\n")

# 4. Check parallel setup
cat("=== 4. Parallel Setup Check ===\n")
if (requireNamespace("future", quietly = TRUE)) {
  library(future)
  cat("Number of workers:", nbrOfWorkers(), "\n")
  current_plan <- plan()
  cat("Parallel plan:", class(current_plan)[1], "\n")
  if (inherits(current_plan, "multicore") || inherits(current_plan, "multisession")) {
    cat("Worker details:\n")
    print(current_plan)
  }
} else {
  cat("future package not available\n")
}
cat("\n")

# 5. Check for explicit set.seed calls
cat("=== 5. Checking for set.seed() calls ===\n")
set_seed_files <- c(
  "01_code/utils_mbb.R",
  "01_code/utils_tuning.R",
  "01_code/02_analysis/02_model_tune_phxgb_parallel.R",
  "01_code/02_analysis/04_model_mbb_cis.R"
)

for (file in set_seed_files) {
  full_path <- paste0(getwd(), "/", file)
  if (file.exists(full_path)) {
    content <- readLines(full_path)
    set_seed_lines <- grep("set\\.seed", content, value = FALSE)
    if (length(set_seed_lines) > 0) {
      cat("Found set.seed() in", file, "at lines:", paste(set_seed_lines, collapse = ", "), "\n")
      for (line in set_seed_lines) {
        cat("  Line", line, ":", trimws(content[line]), "\n")
      }
    }
  }
}
cat("\n")

# 6. Test specific seed derivations used in pipeline
cat("=== 6. Testing Pipeline Seed Derivations ===\n")
test_combos <- list(
  c("IP", "evac", "num_enc_resp", "grid_phxgb_tune"),
  c("IP", "evac", "num_enc_resp", "wflw_fit"),
  c("ED", "high_smoke", "num_enc_cardio", "grid_phxgb_tune")
)

for (markers in test_combos) {
  seed_val <- gen_seed(config$seed, markers)
  cat("Markers:", paste(markers, collapse = " + "), "\n")
  cat("  → Seed:", seed_val, "\n")
  
  # Test reproducibility
  test_seeds <- replicate(10, gen_seed(config$seed, markers))
  if (length(unique(test_seeds)) == 1) {
    cat("  ✓ Reproducible\n")
  } else {
    cat("  ✗ NOT reproducible!\n")
  }
  cat("\n")
}

cat("========================================\n")
cat("Diagnostic complete!\n")
cat("========================================\n\n")

cat("NEXT STEPS:\n")
cat("1. Compare this output between two different runs\n")
cat("2. Look for differences in:\n")
cat("   - Global seed value\n")
cat("   - RNG kind\n")
cat("   - Number of parallel workers\n")
cat("   - Random seed state\n")
cat("3. Check if gen_seed() is reproducible (should always be TRUE)\n")
cat("4. Review SEED_DEBUG_GUIDE.md for detailed troubleshooting\n")

