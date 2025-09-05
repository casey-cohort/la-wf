#-------------------------------
# LA wildfires project
# author: Arnab Dey and Lara Schwarz, adapted by Lauren Wilner
# date: 2025-09-02
# this code configures, tunes, and fits a Prophet-XGBoost model to the aggregated data 

#-------------------------------
# Code adapted from the following project:

# @project: Two-stage interrupted time series design
# @author: Arnab K. Dey (arnabxdey@gmail.com), Yiqun Ma
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script configures, tunes, and fits a Prophet-XGBoost model to the aggregated data
# @date: Dec 16, 2024

#-------------------------------
# setup
rm(list = ls())
set.seed(0112358)
pacman::p_load(modeltime, tidymodels, tidyverse, timetk, tictoc, 
               parallel, doParallel, foreach, digest)

# ensure consistent numeric precision 
options(digits = 7)
options(scipen = 999)

# set paths 
source(paste0(getwd(), "/02_code/paths.R"))

#-------------------------------
# params
mtry_min <- 3
mtry_max <- 10
min_n_min <- 15L
min_n_max <- 30L
tree_depth_min <- 3
tree_depth_max <- 8
learn_rate_min <- 0.01
learn_rate_max <- 0.1
loss_reduction_min <- -5
loss_reduction_max <- 1
stop_iter_min <- 10L
stop_iter_max <- 50L

#-------------------------------
# load data
df_train_test <- read_csv(paste0(path_repo, 
                    "01_data/02_clean/test_train/df-train-test_sf.csv")) %>%
  mutate(date = as.Date(date))

# create all combinations of encounter types, exposure categories, and causes
enc_types <- unique(df_train_test$enc_type)
exposures <- unique(df_train_test$exposure_category)
causes <- colnames(df_train_test) %>% str_subset("^num_enc")

all_combinations <- expand.grid(
  enc_type = enc_types,
  exposure_category = exposures,
  cause = causes,
  stringsAsFactors = FALSE
)

cat("Total combinations:", nrow(all_combinations), "\n")
cat("Estimated time per combination: 4 minutes\n")
cat("Sequential time estimate:", round(nrow(all_combinations) * 4 / 60, 1), "hours\n")

#------------------------------
# helper function to process a single combination with error handling
run_tuning <- function(i, combinations, df_train_test, 
                      mtry_min, mtry_max, min_n_min, min_n_max,
                      tree_depth_min, tree_depth_max, learn_rate_min,
                      learn_rate_max, loss_reduction_min, loss_reduction_max,
                      stop_iter_min, stop_iter_max) {
  
  enc <- combinations$enc_type[i]
  exposure <- combinations$exposure_category[i]
  cause <- combinations$cause[i]
  
  # generate a deterministic seed based on the combination itself
  # doing this to ensure the same combination **always** gets the same seed
  combination_seed <- digest::digest(paste(enc, exposure, cause), "xxhash32", seed = 0112358)
  combination_seed <- as.integer(paste0("0x", substr(combination_seed, 1, 6)), 16)
  set.seed(combination_seed)
    
  tryCatch({
    cat("Processing combination", i, "of", nrow(combinations), ":", enc, exposure, cause, "\n")
    
    # subset data for this enc_type and exposure_category------------------------------
    df_train_test_encounter <- df_train_test %>%
      filter(enc_type == enc, exposure_category == exposure)
    
    # skip if no data
    if (nrow(df_train_test_encounter) == 0) {
      return(list(error = "No data", combination_index = i))
    }
    
    ## split data into training and test sets------------------------------
    splits <- df_train_test_encounter |>
      time_series_split(
        assess = "75 days",
        skip = "75 days",
        cumulative = TRUE,
        date_var = date
      )

    ## resample data------------------------------
    resamples_kfold <- training(splits) |> 
      time_series_cv(
        assess = "40 days",
        slice_limit = 8,        
        cumulative = TRUE
      )
    
    ## recipe for modeling------------------------------
    formula <- as.formula(paste(cause, "~ ."))
    
    rec_obj_phxgb <- recipe(formula, training(splits)) |>
      # time series features
      step_holiday(date, holidays = timeDate::listHolidays("US")) |>
      # minimal seasonal components
      step_mutate(
        month=factor(month(date)),
        year=factor(year(date)),
        is_weekend = factor(if_else(wday(date, week_start = 1) %in% c(6, 7), "weekend", "weekday")),
        day_of_week = factor(wday(date, label = TRUE)),
        month_day = format(date, "%m-%d"),
        business_closed_holiday = factor(if_else(
            month_day %in% c(
              "01-01",  # New Year's Day
              "01-20",  # Martin Luther King Jr. Day
              "07-04",  # Independence Day
              "12-25"   # Christmas Day
            ), "business_closed", "business_open"
        ))
      ) |>
      step_rm(month_day) |>
      step_novel(all_nominal()) |>
      step_rm(matches("(.iso$)|(.xts$)|(.minute)|(.second)|(.hour)|(.am.pm)")) |>
      step_zv("date_USInaugurationDay", "date_USCPulaskisBirthday", "date_USJuneteenthNationalIndependenceDay",
              "date_USDecorationMemorialDay", "date_USColumbusDay", "date_USGoodFriday",
              "date_USIndependenceDay", "date_USLaborDay", "date_USLincolnsBirthday", "date_USMemorialDay", "date_USPresidentsDay",
              "date_USWashingtonsBirthday") |>
      step_normalize(all_numeric_predictors())
    
    ## specify models------------------------------
    model_phxgb_tune <- prophet_boost(
      mode = "regression",
      growth = "linear",
      seasonality_yearly = FALSE,
      mtry = tune(),
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune(),
      stop_iter = tune()
    ) |>
      set_engine("prophet_xgboost",
                 early_stop = TRUE,
                 validation = 0.2)
    
    # generate grid for tuning------------------------------
    grid_phxgb_tune <- grid_space_filling(
      extract_parameter_set_dials(model_phxgb_tune) |>
        update(
          mtry = mtry(range = c(mtry_min, mtry_max)),
          min_n = min_n(range = c(min_n_min, min_n_max)),
          tree_depth = tree_depth(range = c(tree_depth_min, tree_depth_max)),
          learn_rate = learn_rate(range = c(learn_rate_min, learn_rate_max)),
          loss_reduction = loss_reduction(range = c(loss_reduction_min, loss_reduction_max), trans = log10_trans()),
          stop_iter = stop_iter(range = c(stop_iter_min, stop_iter_max))
        ),
      size = 100
    )
    
    ## workflow for tuning------------------------------
    wflw_phxgb_tune <- workflow() |>
      add_model(model_phxgb_tune) |>
      add_recipe(rec_obj_phxgb)

    ## model tuning------------------------------
    suppressWarnings({
      tune_results_phxgb <- wflw_phxgb_tune |>
        tune_grid(
          resamples = resamples_kfold,
          grid = grid_phxgb_tune,
          control = control_grid(
            verbose = FALSE,
            allow_par = FALSE,
            save_pred = TRUE,
            save_workflow = TRUE,
            event_level = "first",
            pkgs = c("tidymodels", "modeltime", "timetk")
          ),
          metrics = metric_set(rmse, rsq)
        )
    })
        
    # pull best params of model based on RMSE------------------------------
    best_params <- tune_results_phxgb |> select_best(metric = "rmse")
    
    return(list(
      splits = splits,
      tune_results_phxgb = tune_results_phxgb,
      wflw_phxgb_tune = wflw_phxgb_tune,
      best_params = best_params,
      resamples_kfold = resamples_kfold,
      rec_obj_phxgb = rec_obj_phxgb,
      df_train_test_encounter = df_train_test_encounter,
      formula = formula,
      combination_index = i,
      enc_type = enc,
      exposure_category = exposure,
      cause = cause,
      success = TRUE
    ))
    
  }, error = function(e) {
    return(list(
      error = as.character(e),
      combination_index = i,
      enc_type = combinations$enc_type[i],
      exposure_category = combinations$exposure_category[i],
      cause = combinations$cause[i],
      success = FALSE
    ))
  })
}

#------------------------------
# set up parallel processing
n_cores <- min(7, parallel::detectCores() - 3)
cat("Using", n_cores, "cores out of", parallel::detectCores(), "available\n")
cat("Parallel time estimate:", round(nrow(all_combinations) * 4 / 60 / n_cores, 1), "hours\n")

cl <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cl)

# monitor memory usage (mac ver)
tryCatch({
  mem_info <- system("sysctl hw.memsize", intern = TRUE)
  mem_bytes <- as.numeric(gsub("hw.memsize: ", "", mem_info))
  cat("Available memory:", round(mem_bytes / 1024^3, 1), "GB\n")
}, error = function(e) {
  cat("Memory info not available, but you have 64GB which is plenty\n")
})

#------------------------------
# process in batches to monitor progress
batch_size <- n_cores * 2
n_batches <- ceiling(nrow(all_combinations) / batch_size)

all_combination_results <- list()
for (batch in 1:n_batches) {
  cat("\n=== Processing batch", batch, "of", n_batches, "===\n")
  
  start_idx <- (batch - 1) * batch_size + 1
  end_idx <- min(batch * batch_size, nrow(all_combinations))
  batch_indices <- start_idx:end_idx
  
  tic()
  batch_results <- foreach(i = batch_indices, 
                          .packages = c("tidymodels", "modeltime", "timetk", "tidyverse", "digest"),
                          .export = c("all_combinations", "df_train_test", 
                                    "mtry_min", "mtry_max", "min_n_min", "min_n_max",
                                    "tree_depth_min", "tree_depth_max", "learn_rate_min",
                                    "learn_rate_max", "loss_reduction_min", "loss_reduction_max",
                                    "stop_iter_min", "stop_iter_max", "run_tuning")) %dopar% {
    
    # Call the function with all required parameters
    run_tuning(i, all_combinations, df_train_test,
              mtry_min, mtry_max, min_n_min, min_n_max,
              tree_depth_min, tree_depth_max, learn_rate_min,
              learn_rate_max, loss_reduction_min, loss_reduction_max,
              stop_iter_min, stop_iter_max)
  }
  toc()
  
  all_combination_results <- c(all_combination_results, batch_results)
  
  # save intermediate results
  save(all_combination_results, file = paste0(path_repo, "03_output/intermediate_results_batch_", batch, ".RData"))
  
  cat("Completed", length(all_combination_results), "of", nrow(all_combinations), "combinations\n")
}

#------------------------------
# check for errors
errors <- sapply(all_combination_results, function(x) !is.null(x$error) || !isTRUE(x$success))
cat("Combinations with errors:", sum(errors), "\n")

#------------------------------
# compile our results in a nested structure for our future steps
all_results <- list()
for (i in seq_along(all_combination_results)) {
  result <- all_combination_results[[i]]
  if (isTRUE(result$success)) {
    enc <- result$enc_type
    exposure <- result$exposure_category
    cause <- result$cause
    
    if (!enc %in% names(all_results)) all_results[[enc]] <- list()
    if (!exposure %in% names(all_results[[enc]])) all_results[[enc]][[exposure]] <- list()
    
    all_results[[enc]][[exposure]][[cause]] <- result
  }
}

#------------------------------
# clean up and save! 

# clean up after ourselves
stopCluster(cl)

# save nested results
save(all_results, file = paste0(path_repo, "03_output/all_model_tuning_results_parallel.RData"))
save(all_combination_results, file = paste0(path_repo, "03_output/all_combination_results_with_errors.RData"))

cat("\nParallel processing complete!\n")