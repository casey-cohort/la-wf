# Seed Debugging Guide

## Where Seeds Are Set in Your Codebase

### 1. **Global Seed Source** (Primary)
- **Location**: `model_config.yaml` line 1
- **Current value**: `seed: 112358`
- **Check**: Is this the same across all runs?

### 2. **Seed Derivation Function**
- **Location**: `utils_tuning.R` - `gen_seed()` function
- **How it works**: Creates deterministic seeds from global seed + markers
- **Potential issue**: If global_seed differs, all derived seeds differ

### 3. **Parallel Processing Seeds**
- **Location 1**: `02_model_tune_phxgb_parallel.R` line 110
  ```r
  .options = furrr_options(seed = TRUE)


  ```
- **Location 2**: `utils_mbb.R` lines 227, 246
  ```r
  .options = furrr::furrr_options(seed = TRUE)
  ```
- **Potential issue**: `furrr_options(seed = TRUE)` uses R's L'Ecuyer RNG, which can behave differently if:
  - Number of parallel workers differs
  - Order of execution differs
  - RNG state was modified before parallel execution

### 4. **Explicit set.seed() Calls**
- **Location**: `utils_mbb.R` line 129
  ```r
  set.seed(base_seed + i)  # Inside bootstrap iteration
  ```
- **Potential issue**: This might conflict with `furrr_options(seed = TRUE)`

## Diagnostic Steps

### Step 1: Verify Global Seed is Consistent
```r
# Add this at the start of your main scripts
config <- read_config("01_code/02_analysis/model_config.yaml")
cat("Global seed from config:", config$seed, "\n")
cat("Seed class:", class(config$seed), "\n")
cat("Seed as integer:", as.integer(config$seed), "\n")
```

### Step 2: Check if gen_seed() is Reproducible
```r
# Test seed derivation
source("01_code/utils_tuning.R")
global_seed <- 112358
markers <- c("IP", "evac", "num_enc_resp", "test")

# Run multiple times - should be identical
seeds <- replicate(10, gen_seed(global_seed, markers))
if (length(unique(seeds)) == 1) {
  cat("✓ gen_seed() is reproducible\n")
} else {
  cat("✗ gen_seed() is NOT reproducible!\n")
  cat("Unique seeds:", unique(seeds), "\n")
}
```

### Step 3: Check Parallel Processing Setup
```r
# Add this before parallel execution
cat("Number of workers:", nbrOfWorkers(), "\n")
cat("Parallel plan:", class(plan()), "\n")
cat("RNG kind:", RNGkind(), "\n")
```

### Step 4: Check for RNG State Pollution
```r
# Add at start of script
cat("Initial RNG state:\n")
print(.Random.seed[1:3])

# Add before parallel execution
cat("RNG state before parallel:\n")
print(.Random.seed[1:3])
```

## Common Issues and Fixes

### Issue 1: Different Number of Cores
**Problem**: Different machines/cores → different parallel execution order
**Fix**: Set explicit number of cores in config and use it consistently

### Issue 2: RNG State Modified Before Parallel
**Problem**: Any `set.seed()` or random operation before `furrr_options(seed = TRUE)` affects results
**Fix**: Ensure no random operations happen between reading config and parallel execution

### Issue 3: set.seed() Inside Parallel Workers
**Problem**: `set.seed(base_seed + i)` in `utils_mbb.R` line 129 might conflict with furrr's seed management
**Fix**: Remove `set.seed()` and rely on `furrr_options(seed = TRUE)` OR use `withr::with_seed()` to isolate

### Issue 4: Different RNG Kind
**Problem**: RNG kind (default, L'Ecuyer, etc.) differs between runs
**Fix**: Explicitly set RNG kind at start:
```r
RNGkind("L'Ecuyer-CMRG")  # What furrr uses
```

## Quick Diagnostic Script

Run this to check all seed-related issues:

```r
# seed_diagnostic.R
source("01_code/utils_general.R")
source("01_code/utils_tuning.R")

# 1. Check config seed
config <- read_config("01_code/02_analysis/model_config.yaml")
cat("=== Global Seed Check ===\n")
cat("Seed value:", config$seed, "\n")
cat("Seed type:", class(config$seed), "\n")
cat("Seed as integer:", as.integer(config$seed), "\n\n")

# 2. Test gen_seed reproducibility
cat("=== gen_seed() Reproducibility Test ===\n")
test_markers <- c("IP", "evac", "num_enc_resp", "test")
seeds <- replicate(100, gen_seed(config$seed, test_markers))
if (length(unique(seeds)) == 1) {
  cat("✓ PASS: gen_seed() is reproducible\n")
  cat("  Generated seed:", unique(seeds)[1], "\n\n")
} else {
  cat("✗ FAIL: gen_seed() produced", length(unique(seeds)), "different seeds\n")
  cat("  Unique seeds:", paste(unique(seeds), collapse = ", "), "\n\n")
}

# 3. Check RNG state
cat("=== RNG State Check ===\n")
cat("RNG kind:", paste(RNGkind(), collapse = ", "), "\n")
if (exists(".Random.seed")) {
  cat("Random seed exists, first 3 values:", .Random.seed[1:3], "\n")
} else {
  cat("No .Random.seed set (will be created on first random operation)\n")
}
cat("\n")

# 4. Check parallel setup
cat("=== Parallel Setup Check ===\n")
if (requireNamespace("future", quietly = TRUE)) {
  cat("Number of workers:", future::nbrOfWorkers(), "\n")
  cat("Parallel plan:", class(future::plan())[1], "\n")
} else {
  cat("future package not available\n")
}
```

## Most Likely Culprits

Based on your code structure, check these in order:

1. **Different global seed values** - Check `model_config.yaml` hasn't changed
2. **RNG state pollution** - Something setting seed before parallel execution
3. **set.seed() conflict** - Line 129 in `utils_mbb.R` might interfere with furrr
4. **Different parallel workers** - Number of cores affects execution order

## Recommended Fix

If `set.seed()` in bootstrap is causing issues, wrap it with `withr::with_seed()`:

```r
# In utils_mbb.R, replace line 129:
# OLD: set.seed(base_seed + i)
# NEW:
withr::with_seed(base_seed + i, {
  # ... bootstrap code ...
})
```

This isolates the seed change to just that operation.




