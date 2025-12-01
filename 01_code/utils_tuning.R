#-------------------------------
# LA wildfires project
# author: Lauren Wilner
# helper functions

#' Dependencies required for run_tuning function in this file:
#' - tidymodels (for modeling workflow)
#' - modeltime (for prophet_boost)
#' - timetk (for time series operations)
#' - tidyverse (for data manipulation)
#' - arrow (for reading parquet files)
#' - timeDate (for holiday functions)
#' - withr (for with_seed() function to manage RNG state)
#' 
#' Note: These should be loaded in the main script before sourcing this file


#-------------------------------
# gen seed
#-------------------------------
# args: global seed, markers (encounter_type string, cause string, exposure_category string, process name string)
# tip: each time a seed is needed for something, use this function to set the seed, then run the function 

gen_seed <- function(global_seed, markers){

  # combine the markers into a single string
  combined_string <- paste(markers, collapse = "_")

  # generate a hash of the combined string with the global seed
  seed_hash <- digest::digest(combined_string, algo = "xxhash32", seed = global_seed)

  # convert the hash to an integer seed
  seed_integer <- as.integer(paste0("0x", substr(seed_hash, 1, 6)), 16)
  
  return(seed_integer)
}

#-------------------------------
# NOTE: Config and version number functions have been moved to utils_general.R
# - gen_ver_number() -> utils_general.R
# - read_config() -> utils_general.R
# - write_config() -> utils_general.R
#-------------------------------


#-------------------------------
# model tuning function
#------------------------------
# run tuning function to process a single combination

run_tuning <- function(combination, grid_params, train_test_params, global_seed, train_test_path, grid_size = 200) {

  enc <- combination$encounter_type
  exposure <- combination$exposure_category
  cause <- combination$cause

  tryCatch({
    cat("Processing combination:", enc, exposure, cause, "\n")
    
    # load and subset data for this enc_type and exposure_category------------------------------
    # load and subset to enc and exposure on load
    df_train_test_encounter <- open_dataset(
        paste0(train_test_path, "df-train-test_sf.parquet")) %>% 
        filter(
          exposure_category == !!exposure & enc_type == !!enc
        ) %>% 
        collect() %>%
        mutate(date = as.Date(date))

    # skip if no data
    if (nrow(df_train_test_encounter) == 0) {
      return(list(error = "No data"))
    }
    
    ## split data into training and test sets------------------------------
    
    splits <- df_train_test_encounter |>
      ungroup() |>
      time_series_split(
        assess = train_test_params$assess_split,
        # skip = "75 days", no skip needed for splits
        cumulative = TRUE,
        date_var = date
      ) # this does not need a seed 

    ## resample data------------------------------
    resamples_kfold <- training(splits) |> 
      time_series_cv(
        assess = train_test_params$assess_cv,
        skip = train_test_params$skip_cv,
        slice_limit = train_test_params$slice_limit_cv,        
        cumulative = TRUE,
        verbose = FALSE
      ) # this does not need a seed 
    
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
      step_normalize(all_numeric_predictors()) # this does not need a seed
    
    ## specify models------------------------------
    
    # Build model arguments conditionally based on tune flag
    model_args <- list(
      mode = "regression",
      growth = "linear",
      seasonality_yearly = FALSE
    )
    
    # Add parameters conditionally - only include if tune: true
    if (isTRUE(grid_params$mtry$tune)) model_args$mtry <- tune()
    if (isTRUE(grid_params$trees$tune)) model_args$trees <- tune()
    if (isTRUE(grid_params$min_n$tune)) model_args$min_n <- tune()
    if (isTRUE(grid_params$tree_depth$tune)) model_args$tree_depth <- tune()
    if (isTRUE(grid_params$learn_rate$tune)) model_args$learn_rate <- tune()
    if (isTRUE(grid_params$loss_reduction$tune)) model_args$loss_reduction <- tune()
    if (isTRUE(grid_params$stop_iter$tune)) model_args$stop_iter <- tune()
    if (isTRUE(grid_params$sample_size$tune)) model_args$sample_size <- tune()
    if (isTRUE(grid_params$changepoint_num$tune)) model_args$changepoint_num <- tune()
    if (isTRUE(grid_params$changepoint_range$tune)) model_args$changepoint_range <- tune()
    if (isTRUE(grid_params$prior_scale_changepoints$tune)) model_args$prior_scale_changepoints <- tune()
    
    # model_phxgb_tune_seed <- gen_seed(global_seed, c(enc, exposure, cause, "model_phxgb_tune"))

    model_phxgb_tune <- do.call(prophet_boost, model_args) |>
      set_engine("prophet_xgboost",
                 # seed = model_phxgb_tune_seed,
                 early_stop = TRUE,
                 validation = train_test_params$validation) # this needs a seed
    
    # generate grid for tuning------------------------------
    # Use withr::with_seed() to set seed only for grid generation without side effects
    grid_phxgb_tune_seed <- gen_seed(global_seed, c(enc, exposure, cause, "grid_phxgb_tune"))
    
    # Build grid update arguments conditionally - only include parameters with tune: true
    grid_update_args <- list()
    if (isTRUE(grid_params$mtry$tune)) grid_update_args$mtry <- mtry(range = grid_params$mtry$range)
    if (isTRUE(grid_params$trees$tune)) grid_update_args$trees <- trees(range = grid_params$trees$range)
    if (isTRUE(grid_params$min_n$tune)) grid_update_args$min_n <- min_n(range = grid_params$min_n$range)
    if (isTRUE(grid_params$tree_depth$tune)) grid_update_args$tree_depth <- tree_depth(range = grid_params$tree_depth$range)
    if (isTRUE(grid_params$learn_rate$tune)) grid_update_args$learn_rate <- learn_rate(range = grid_params$learn_rate$range)
    if (isTRUE(grid_params$loss_reduction$tune)) grid_update_args$loss_reduction <- loss_reduction(range = grid_params$loss_reduction$range, trans = log10_trans())
    if (isTRUE(grid_params$stop_iter$tune)) grid_update_args$stop_iter <- stop_iter(range = grid_params$stop_iter$range)
    if (isTRUE(grid_params$sample_size$tune)) grid_update_args$sample_size <- sample_prop(range = grid_params$sample_size$range)
    if (isTRUE(grid_params$changepoint_num$tune)) grid_update_args$changepoint_num <- changepoint_num(range = grid_params$changepoint_num$range)
    if (isTRUE(grid_params$changepoint_range$tune)) grid_update_args$changepoint_range <- changepoint_range(range = grid_params$changepoint_range$range)
    if (isTRUE(grid_params$prior_scale_changepoints$tune)) grid_update_args$prior_scale_changepoints <- prior_scale_changepoints(range = grid_params$prior_scale_changepoints$range, trans = log10_trans())
    
    param_set <- extract_parameter_set_dials(model_phxgb_tune)
    if (length(grid_update_args) > 0) {
      param_set <- do.call(update, c(list(param_set), grid_update_args))
    }
    # Use withr::with_seed() to manage RNG state locally without global side effects
    grid_phxgb_tune <- withr::with_seed(grid_phxgb_tune_seed, {
      grid_space_filling(param_set, size = grid_size)
    })
    
    ## workflow for tuning------------------------------
    wflw_phxgb_tune <- workflow() |>
      add_model(model_phxgb_tune) |>
      add_recipe(rec_obj_phxgb)

    ## model tuning------------------------------
    # Generate seed for tuning but don't set it globally - tune_grid handles resampling deterministically
    # The seed is used by furrr for parallel processing reproducibility
    tune_results_phxgb_seed <- gen_seed(global_seed, c(enc, exposure, cause, "tune_results_phxgb"))
    
    # Retry logic for tune_grid to handle XGBoost precision issues
    max_retries <- 3
    retry_count <- 0
    tune_results_phxgb <- NULL
    
    while (is.null(tune_results_phxgb) && retry_count < max_retries) {
      tryCatch({
        suppressWarnings({ suppressMessages({
          tune_results_phxgb <- wflw_phxgb_tune |>
            tune_grid( # try: tune_bayes
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
              metrics = metric_set(yardstick::rmse, yardstick::rsq)
            )
        })})
        
        # If we get here, tuning succeeded
        if (!is.null(tune_results_phxgb)) {
          break
        }
        
      }, error = function(e) {
        error_msg <- as.character(e)
        retry_count <<- retry_count + 1
        
        # Check if it's the XGBoost precision error
        if (grepl("Inconsistent.*best_score", error_msg) || grepl("finalizer", error_msg)) {
          cat("  Warning: XGBoost precision error on attempt", retry_count, "- retrying...\n")
          if (retry_count < max_retries) {
            # Force garbage collection and wait a bit
            gc(verbose = FALSE)
            Sys.sleep(2)
          } else {
            cat("  Error: Max retries reached for tune_grid. This combination may fail.\n")
            stop(paste("Failed after", max_retries, "retries:", error_msg))
          }
        } else {
          # Different error - don't retry
          stop(error_msg)
        }
      })
    }
    
    if (is.null(tune_results_phxgb)) {
      stop("Failed to complete tune_grid after all retries")
    }
        
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
      enc_type = enc,
      exposure_category = exposure,
      cause = cause,
      success = TRUE
    ))
    
  }, error = function(e) {
    return(list(
      error = as.character(e),
      enc_type = combination$encounter_type,
      exposure_category = combination$exposure_category,
      cause = combination$cause,
      success = FALSE
    ))
  })
}


#-------------------------------
# Error metrics function
#-------------------------------
# helper function to calculate metrics for a dataset
calc_metrics <- function(data_type, dataset, model_table, enc_type_val, exposure_category_val, cause_val) {
      tryCatch({
        preds <- suppressWarnings({
          suppressMessages({
            model_table %>%
              modeltime_calibrate(new_data = dataset) %>%
              select(.model_desc, .calibration_data) %>%
              unnest(cols = c(.calibration_data)) %>%
              mutate(.model_desc = "PROPHETXGB")
          })
        })
        
        if (nrow(preds) > 0 && !all(is.na(preds$.prediction))) {
          metrics_df <- preds %>%
            group_by(.model_desc) %>%
            summarise(
              mdae = Metrics::mdae(.actual, .prediction),
              mae = Metrics::mae(.actual, .prediction),
              rmse = Metrics::rmse(.actual, .prediction),
              mape = Metrics::mape(.actual, .prediction),
              rse = Metrics::rse(.actual, .prediction),
              smape = Metrics::smape(.actual, .prediction),
              r2 = round(1 - sum((.actual - .prediction)^2) / sum((.actual - mean(.actual))^2), 2),
              .groups = 'drop'
            ) %>%
            mutate(
              enc_type = enc_type_val,
              exposure_category = exposure_category_val,
              cause = cause_val,
              data_type = data_type
            )
          return(metrics_df)
        } else {
          return(NULL)
        }
      }, error = function(e) {
        cat("Error in calc_metrics for", data_type, ":", e$message, "\n")
        return(NULL)
      })
    }

# main function to  calculate error metrics for a given combination
calculate_error_metrics <- function(result, global_seed) {
  
  enc <- result$enc_type
  exposure <- result$exposure_category
  cause <- result$cause
  
  tryCatch({
    cat("Calculating error metrics for:", enc, exposure, cause, "\n")
    
    # extract objects from result
    wflw_phxgb_tune <- result$wflw_phxgb_tune
    tune_results_phxgb <- result$tune_results_phxgb
    splits <- result$splits
    
    # fit the final model with retry logic because this solves the error where the same number isn't seen as the same
    max_retries <- 5
    retry_count <- 0
    success <- FALSE
    wflw_fit <- NULL

    while (!success && retry_count < max_retries) {
      tryCatch({
        # fit the model using withr::with_seed() to manage RNG state
        wflw_fit_seed <- gen_seed(global_seed, c(enc, exposure, cause, "wflw_fit"))
        wflw_fit <- withr::with_seed(wflw_fit_seed, {
          suppressWarnings({
            suppressMessages({
              wflw_phxgb_tune |>
                finalize_workflow(select_best(tune_results_phxgb, metric = "rmse")) |>
                fit(training(splits))
            })
          })
        })
        
        # test if the model actually works by making a small prediction
        test_pred <- suppressWarnings({
          suppressMessages({
            predict(wflw_fit, new_data = head(training(splits), 5))
          })
        })
        
        # if we get here without error and have valid predictions, it worked
        if (!is.null(test_pred) && nrow(test_pred) > 0 && !any(is.na(test_pred$.pred))) {
          success <- TRUE
        } else {
          stop("Model fitted but predictions are invalid")
        }
        
      }, error = function(e) {
        retry_count <<- retry_count + 1
        if (retry_count >= max_retries) {
          stop(paste("Max retries reached for", enc, exposure, cause))
        } else {
          Sys.sleep(1)  # pause between retries
        }
      })
    }

    if (!success || is.null(wflw_fit)) {
      return(list(error = "Could not fit final model", success = FALSE))
    }

    # Generate modeltime table and calculate metrics
    model_tbl <- tryCatch({
      suppressWarnings({
        suppressMessages({
          modeltime_table(wflw_fit)
        })
      })
    }, error = function(e) {
      return(NULL)
    })
    
    # Check if model_tbl was created successfully
    if (is.null(model_tbl) || nrow(model_tbl) == 0) {
      return(list(error = "Could not create modeltime table", success = FALSE))
    }

    # Calculate training and testing metrics - pass model_tbl and combination parameters
    training_metrics <- calc_metrics("training", training(splits), model_tbl, enc, exposure, cause)
    testing_metrics <- calc_metrics("testing", testing(splits), model_tbl, enc, exposure, cause)
    
    # Combine results
    all_metrics <- bind_rows(training_metrics, testing_metrics)
    
    if (!is.null(all_metrics) && nrow(all_metrics) > 0) {
      return(list(
        metrics = all_metrics,
        success = TRUE
      ))
    } else {
      return(list(error = "No valid metrics calculated", success = FALSE))
    }
    
  }, error = function(e) {
    return(list(
      error = as.character(e),
      success = FALSE
    ))
  })
}
