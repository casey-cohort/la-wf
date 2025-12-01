# la-wf
This repository is for an ITS analysis of the 2025 LA wildfires

## Workflow Overview

The analysis consists of two main phases:

### Phase 1: Exposure Data Preparation (`01_code/01_exposure/`)

This phase compiles and processes exposure data:
- **Evacuation zones**: Compile official evacuation order/warning zones from LA County sources
- **Smoke/PM2.5 exposure**: Process air quality and smoke exposure data
- **Covariates**: Prepare demographic and other covariate data
- **Combined datasets**: Merge exposure sources into unified analysis datasets

Key scripts:
- `01_lacounty_evac.ipynb` - Process evacuation zone data
- `02_compile_evac_exp.ipynb` - Compile evacuation exposure data
- `03_compile_pm_exp.R` - Compile PM2.5 exposure data
- `04_compile_exp.ipynb` - Combine all exposure data
- `covariates/01_cov_prep.R` - Prepare covariate data

### Phase 2: Analysis Pipeline (`01_code/02_analysis/`)

This phase runs the statistical modeling and generates outputs. The pipeline consists of four steps:

1. **Data Preparation** (`01_data_prep.R`): Creates train/test datasets with time splits for cross-validation
2. **Model Tuning** (`02_model_tune_phxgb_parallel.R`): Tunes Prophet + XGBoost models using hyperparameter grid search
3. **MBB Confidence Intervals** (`04_model_mbb_cis.R`): Generates Moving Block Bootstrap confidence intervals for uncertainty quantification
4. **Generate Outputs** (`05_model_outputs.R`): Creates performance metrics, visualizations, and excess hospitalization estimates

**Note**: Script `03_` is reserved for a future script that will add block length optimization for MBB (Moving Block Bootstrap).

### Running the Analysis

The main entry point is `01_code/02_analysis/00_run_all.R`, which:
- Executes all four pipeline steps sequentially in the order: Data Preparation → Model Tuning → MBB Confidence Intervals → Generate Outputs
- Allows skipping data preparation if using existing datasets
- Provides detailed timing information for each step
- Generates outputs in a timestamped directory with naming pattern: `model_run_YYYY-MM-DD.v###_x##_sim###`

Model selection is configured via `01_code/02_analysis/model_config.yaml` (see Model Configuration section below).

## Model Configuration

The analysis pipeline is configured via `01_code/02_analysis/model_config.yaml`. There are two ways to specify which models to run:

### Option 1: `models_to_run` (Cartesian Product)

Use this when you want to run **all combinations** of encounter types, exposure categories, and causes. This is convenient when running many models.

**Example:**
```yaml
models_to_run:
  encounter_type: ["IP", "ED"]
  exposure_category: ["evac", "high_smoke", "mid_smoke", "none"]
  cause: ["num_enc", "num_enc_resp", "num_enc_cardio", "num_enc_injury", "num_enc_neuro"]
```

This would run **2 × 4 × 5 = 40 model combinations**:
- IP + evac + num_enc
- IP + evac + num_enc_resp
- IP + evac + num_enc_cardio
- ... (and 37 more combinations)

### Option 2: `models_to_run_flat` (Explicit List)

Use this when you want to run **specific combinations only**. This gives you precise control over exactly which models to run.

**IMPORTANT:** Use the multi-line format shown below (not inline format).

**Example:**
```yaml
models_to_run_flat:
  - encounter_type: "IP"
    exposure_category: "evac"
    cause: "num_enc_resp"
  - encounter_type: "ED"
    exposure_category: "high_smoke"
    cause: "num_enc_cardio"
  - encounter_type: "ED"
    exposure_category: "mid_smoke"
    cause: "num_enc_injury"
```

This would run **exactly 3 models** (only the combinations listed).

### More Examples

**Example 1: Run all ED models with evac and high_smoke exposures**
```yaml
models_to_run:
  encounter_type: ["ED"]
  exposure_category: ["evac", "high_smoke"]
  cause: ["num_enc", "num_enc_resp", "num_enc_cardio", "num_enc_injury", "num_enc_neuro"]
```
Result: 1 × 2 × 5 = 10 models

**Example 2: Run only respiratory and cardiovascular causes for both encounter types**
```yaml
models_to_run:
  encounter_type: ["IP", "ED"]
  exposure_category: ["evac", "high_smoke", "mid_smoke", "none"]
  cause: ["num_enc_resp", "num_enc_cardio"]
```
Result: 2 × 4 × 2 = 16 models

**Example 3: Run specific high-priority models only**
```yaml
models_to_run_flat:
  - encounter_type: "IP"
    exposure_category: "evac"
    cause: "num_enc_resp"
  - encounter_type: "IP"
    exposure_category: "high_smoke"
    cause: "num_enc_resp"
  - encounter_type: "ED"
    exposure_category: "evac"
    cause: "num_enc_cardio"
  - encounter_type: "ED"
    exposure_category: "high_smoke"
    cause: "num_enc_cardio"
```
Result: Exactly 4 models

### Important Notes

- **Use only ONE option at a time** (`models_to_run` OR `models_to_run_flat`, not both)
- When using `models_to_run_flat`, always use the multi-line format as shown above
- The config file also includes hyperparameter tuning ranges and other settings

## Reproducibility and Seed Management

This codebase implements a comprehensive seed management system to ensure full reproducibility of all analyses. All random operations (model tuning, bootstrap resampling, parallel processing) are controlled by a central seed defined in `model_config.yaml`.

**Key Features:**
- Central global seed configuration
- Deterministic seed derivation for all operations
- Safe parallel processing with reproducible results
- No global RNG state pollution

**For detailed information**, see [SEED_MANAGEMENT.md](01_code/03_testing/SEED_MANAGEMENT.md), which covers:
- How seeds flow through the pipeline
- Testing reproducibility
- Troubleshooting seed issues
- Best practices for maintaining reproducibility

**Quick Test:**
```bash
# Run reproducibility test
Rscript 01_code/02_analysis/test_seed_reproducibility.R

# Run again and compare results
Rscript 01_code/02_analysis/compare_seed_test_results.R
```
