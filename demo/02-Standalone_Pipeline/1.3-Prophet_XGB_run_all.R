## ----------------------------------------------------------------------------
## @description: Master script to run the complete Prophet + XGBoost analysis
##               with Moving Block Bootstrap confidence intervals
## ----------------------------------------------------------------------------

## This script orchestrates the complete analysis pipeline:
##   1. Tune and fit Prophet + XGBoost model
##   2. (Optional) Select optimal block length for MBB
##   3. Generate MBB confidence intervals
##   4. Create outputs (plots, tables, excess hospitalizations)

cat("========================================\n")
cat("Prophet + XGBoost with MBB Analysis\n")
cat("========================================\n\n")

## ============================================================================
## Step 1: Tune and Fit Models
## ============================================================================

cat("STEP 1: Tuning and fitting Prophet + XGBoost model...\n")
source(here::here("demo/02-Standalone_Pipeline/1.3.1-Prophet_XGB_tune_and_fit.R"))

## ============================================================================
## Step 2 (Optional): Select Block Length
## ============================================================================

cat("STEP 2 (Optional): Block Length Selection\n")

# Uncomment the line below to run block length selection (run only once)
# source(here::here("demo/02-Standalone_Pipeline/1.3.0-Prophet_XGB_select_block_length.R"))


## ============================================================================
## Step 3: Generate MBB Confidence Intervals
## ============================================================================

cat("STEP 3: Generating MBB confidence intervals...\n")
source(here::here("demo/02-Standalone_Pipeline/1.3.2-Prophet_XGB_MBB_CIs.R"))

## ============================================================================
## Step 4: Generate Outputs
## ============================================================================

cat("STEP 4: Generating plots and excess hospitalization analysis...\n")
source(here::here("demo/02-Standalone_Pipeline/1.3.3-Prophet_XGB_outputs.R"))

