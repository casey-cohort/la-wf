# Model Comparison and Code Organization Improvements

## Summary
This PR introduces significant improvements to model comparison workflows, code organization, and data handling. The main changes include refactoring the model comparison script into a modular structure, implementing user-specific model output directories, standardizing date formats across the codebase, and improving data preprocessing for rate variables.

## Major Changes

### 1. Model Comparison Refactoring (`06_model_compare.R`)
- **Renamed and reorganized**: `compare_models_by_exposure.R` → `01_code/02_analysis/06_model_compare.R` to align with naming conventions
- **Extracted helper functions** to `utils_outputs.R`:
  - `find_model_directories()` - Discovers model directories across user subdirectories
  - `extract_metrics_from_dir()` - Extracts metrics from individual model directories
  - `combine_and_expand_metrics()` - Combines metrics and creates complete grid
  - `identify_best_models()` - Identifies best model per combination based on sMAPE
  - `extract_best_model_files()` - Extracts PDFs and configs for best models
  - `filter_models_by_r2()` - Filters models by R² threshold
  - `print_threshold_diagnostics()` - Prints diagnostic information
- **Reduced verbosity**: Streamlined output to show only essential information (reduced from ~250 lines to ~10-15 lines)
- **Improved file naming**: PDFs and configs now use shared prefixes so they sort together alphabetically (e.g., `ED_high_smoke_rate_enc_injury_akd_model_run_2025-12-31.v014_x20_sim100_fit.pdf` and `ED_high_smoke_rate_enc_injury_akd_model_run_2025-12-31.v014_x20_sim100_config.yaml`)

### 2. User-Specific Model Output Directories
- **New utility function**: `get_models_path()` in `utils_general.R` constructs user-specific model output paths
- **Updated all model write operations** to use `models/{user}/` instead of `models/`:
  - `02_model_tune_phxgb_parallel.R` - All 6 model output write locations updated
  - `00_run_all.R` - Latest version lookup updated
  - `06_model_compare.R` - Model directory search updated
- **Configuration-driven**: User is specified in `model_config.yaml` and automatically used throughout the pipeline
- **Backward compatible**: Latest version searches are scoped to the current user's directory

### 3. Date Format Standardization
- **Standardized all date formats** to `YYYY-MM-DD` with dashes across the codebase:
  - `01_data_prep.R` - Updated file paths and filenames
  - `00_outcome_viz.R` - Updated file paths
- **Consistent naming**: All date references now use `YYYY-MM-DD` format (e.g., `2025-08-08` instead of `08-08-2025` or `08082025`)

### 4. Data Preprocessing Improvements
- **Rate variable handling**: 
  - Rate variables (`rate_enc`, `rate_enc_cardio`, `rate_enc_resp`, `rate_enc_neuro`, `rate_enc_injury`) are now excluded from integer conversion
  - Infinities in rate variables are replaced with 0
- **Exposure category cleaning**: Fixed regex to properly remove text after commas in `exposure_category` variable

### 5. Best Model Extraction Feature
- **New functionality**: Extracts PDF figures and config files for best-performing models
- **Output location**: Files are saved to `models/{user}/bested/` subfolder
- **Config modification**: Extracted configs are modified to contain only the specific model combination in `models_to_run_flat`
- **File organization**: PDFs and configs use shared naming prefixes for easy sorting

### 6. Code Organization
- **Utils folder structure**: All utility files moved to `01_code/00_utils/`:
  - `utils_general.R`
  - `utils_outputs.R`
  - `utils_tuning.R`
  - `utils_mbb.R`
- **Updated all source paths** across the codebase to reference the new `00_utils` location:
  - `06_model_compare.R`
  - `02_model_tune_phxgb_parallel.R`
  - `00_run_all.R`
  - `05_model_outputs.R`
  - `04_model_mbb_cis.R`
  - `utils_general.R` (internal references)

## Technical Details

### Model Comparison Workflow
1. **Step 1**: Extracts and combines metrics from all model directories across user subdirectories
2. **Step 2**: Identifies best model for each `enc_type × exposure_category × cause` combination based on lowest sMAPE with positive R²
3. **Step 3**: Extracts PDF figures and modified config files for best models to `bested/` folder
4. **Step 4**: Filters and reports models meeting R² threshold (default: 0.15)

### File Structure
```
01_code/
├── 00_utils/
│   ├── utils_general.R      # General utilities (paths, versioning)
│   ├── utils_outputs.R      # Output generation and model comparison helpers
│   ├── utils_tuning.R        # Model tuning utilities
│   └── utils_mbb.R           # Moving block bootstrap utilities
└── 02_analysis/
    ├── 00_run_all.R          # Master pipeline script
    ├── 02_model_tune_phxgb_parallel.R
    ├── 04_model_mbb_cis.R
    ├── 05_model_outputs.R
    └── 06_model_compare.R    # Model comparison script (refactored)
```

## Benefits
- **Better organization**: Modular code structure with reusable helper functions
- **User isolation**: Each user's model runs are stored separately, preventing conflicts
- **Cleaner output**: Reduced verbosity makes it easier to identify important information
- **Easier maintenance**: Helper functions can be tested and modified independently
- **Consistent formatting**: Standardized date formats reduce confusion and errors

## Testing
- All scripts tested with existing model directories
- Verified that user-specific paths work correctly
- Confirmed that best model extraction produces correctly formatted files
- Validated that date format changes don't break existing workflows

## Files Changed
- `01_code/02_analysis/06_model_compare.R` (new, refactored from `compare_models_by_exposure.R`)
- `01_code/00_utils/utils_outputs.R` (added model comparison helper functions)
- `01_code/00_utils/utils_general.R` (added `get_models_path()` function)
- `01_code/02_analysis/02_model_tune_phxgb_parallel.R` (updated paths)
- `01_code/02_analysis/00_run_all.R` (updated paths)
- `01_code/02_analysis/05_model_outputs.R` (updated paths)
- `01_code/02_analysis/04_model_mbb_cis.R` (updated paths)
- `01_code/02_analysis/01_data_prep.R` (date format standardization, rate variable handling)
- `01_code/00_outcome_viz.R` (date format standardization)

## Notes
- The old `compare_models_by_exposure.R` file has been removed (replaced by `06_model_compare.R`)
- Users need to ensure their `model_config.yaml` has a `user` field set (e.g., `user: 'lbw'`)
- Date format standardization requires filesystem files to be renamed to match new format (see `FILES_TO_RENAME.md` if it exists)

