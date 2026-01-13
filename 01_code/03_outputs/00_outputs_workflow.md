When 02_analysis is run multiple times for a particular combination of exposure categories (combination from hereon) and saved under 02_output/mdoels/{akd|lbw}, there might be competing versions of model configurations to choose from. This workflow describes the workflow for selecting the best model for each combination and generating the final outputs. 

## Step-1: Consolidate all model metrics and identify top models
- Run script `03_outputs/01_identify_top_models.R` which will perform the following:
  - Extract the performance metrics from all models stored in 02_outputs/mdoels/{akd|lbw} and combine them into a single worksheet in `02_output/models/model_comparisons/model_comparison.xlsx`
  - Identify top n models versions based on an r2 threshold and ascending MASE values and save them to `02_output/models/model_comparisons/model_comparison_top_n.xlsx`
  - Extract the configs and plots corresponding to the top n models and stores them under `02_output/models/model_comparisons/model_configs_for_rerun'

## Step-2: Re-run models (optional)
- In some cases, you might want to re-run finalized models with the same tuning parameters, but with slightly different config options e.g. higher number of simulations. 
- To do this, first create a new excel file `02_output/models/model_comparisons/model_versions_manual_for_rerun.xlsx` that will contain the version number for each combination that we want to re-run with the same tuning parameters but slightly altered n_sim etc. 
- Once this file is ready, execute `03_outputs/02_rerun_final_batches.R`

## Step-3: Generate final outputs
- Once all models have been finalized, populate the excel sheet `02_output/models/model_comparisons/model_versions_manual_for_final_outputs.xlsx` with the final versions to be used to generate outputs.
- Execute `03_outputs/03_final_outputs.R`
- This will create a directory 02_output/final_outputs/ which will contain the final configs, plots, and excess hospitalization calculations.
- Note: You can edit the number of days of aggreagation after the event start date by altering the num_days_agg variable in the script



