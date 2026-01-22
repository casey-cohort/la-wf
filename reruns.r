pacman::p_load(tidyverse, here, yaml)

# Set paths
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_outputs.R"))
source(paste0(getwd(), "/01_code/00_utils/utils_best_tuned.R"))

## Identify model directories
models_dir <- here(path_onedrive, "03_modeling-and-results/01_modeling/")
model_comparisons_dir <- paste0(path_onedrive, "03_modeling-and-results/02_best-model-selection/")

temp_dir <- paste0(model_comparisons_dir, "/model_configs_for_rerun/")
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
config_dir <- paste0(temp_dir, "/configs/")
dir.create(config_dir, showWarnings = FALSE, recursive = TRUE)
plots_dir <- paste0(temp_dir, "/plots/")
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)


# Step-1: Extract configs and plots corresponding to the best models identified manually
# Read manual best model selection xlsx
xlsx_file <- paste0(model_comparisons_dir, "model_versions_manual_for_rerun.xlsx")
best_models_manual <- readxl::read_xlsx(xlsx_file) |> as.data.frame()

extract_best_model_files(best_models_manual, models_dir, temp_dir, 
                         pdf_dir = plots_dir, config_dir = config_dir)

models_to_run_all_ED <- list(
  list(encounter_type = "ED", exposure_category = "none", cause = "rate_enc_injury"),
  list(encounter_type = "ED", exposure_category = "none", cause = "rate_enc_neuro"),
  list(encounter_type = "ED", exposure_category = "none", cause = "rate_enc_resp"))

run_batch_bested(models_to_run_all_ED,  
                 train_test_date = "2026-01-07",
                 user = "akd",
                 path_onedrive = path_onedrive, 
                 bested_dir = temp_dir,
                 n_sim_mbb = 1000, 
                 ci_method = "quantile",
                 ensure_nonnegative = TRUE)
