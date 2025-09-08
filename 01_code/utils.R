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
#' 
#' Note: These should be loaded in the main script before sourcing this file


#-------------------------------
# gen ver number 
#-------------------------------
# look for if today's date exists in output folder. if so, look at version number after the date, and increment it by 1. if not, start at v001. always pad with 0's such that the ver number is 3 digits.
gen_ver_number <- function(path) {
  all_dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  
  # get today's date in the format used in folder names
  today <- Sys.Date()
  
  # filter for folders that match the pattern with today's date
  pattern <- paste0("^model_run_", today, "\\.v\\d{3}$")
  existing_folders <- all_dirs[grepl(pattern, all_dirs)]
  
  if (length(existing_folders) == 0) {
    return("v001")
  } else {
    # extract version numbers from existing folders
    versions <- sapply(existing_folders, function(x) {
      # extract the version number (the 3 digits after .v)
      version_match <- regmatches(x, regexpr("\\.v(\\d{3})$", x))
      if (length(version_match) > 0) {
        as.integer(sub("\\.v(\\d{3})$", "\\1", version_match))
      } else {
        0
      }
    })
    
    # remove any NA values and get the next version
    versions <- versions[!is.na(versions)]
    new_version <- max(versions) + 1
    return(paste0("v", sprintf("%03d", new_version)))
  }
}


#-------------------------------
# config functions

# TODO: use dataclass package at some point for validations 
# this will allow us to load via yaml and validate it through dataclass which will confirm all the fields are valid and we return the config obj. 

#-------------------------------
# read config function
#-------------------------------
read_config <- function(file_path) {

  # Read in YAML file -------------------------------
  config <- yaml::read_yaml(file_path)

  # Format models to run list -------------------------------
  ## ensure we have only models_to_run or models_to_run_flat
  if(!is.null(config$models_to_run) & !is.null(config$models_to_run_flat)){
    stop("Error: Must provide only one of models_to_run or models_to_run_flat in the config file.")
  }

  ## ensure at least one of models_to_run or models_to_run_flat is provided
  if(is.null(config$models_to_run) & is.null(config$models_to_run_flat)){
    stop("Error: Must provide one of models_to_run or models_to_run_flat in the config file.")
  }

  ## if models_to_run is provided, convert it to models_to_run_flat and delete models_to_run from config
    # Extract the vectors from the nested structure
    encounter_types <- config$models_to_run$encounter_type
    exposure_categories <- config$models_to_run$exposure_category
    causes <- config$models_to_run$cause
    
    # Create all combinations using expand.grid
    combinations <- expand.grid(
      encounter_type = encounter_types,
      exposure_category = exposure_categories,
      cause = causes,
      stringsAsFactors = FALSE
    )
    
    # Convert to list of lists with clean character values
    models_to_run_flat <- lapply(1:nrow(combinations), function(i) {
      list(
        encounter_type = as.character(combinations$encounter_type[i]),
        exposure_category = as.character(combinations$exposure_category[i]),
        cause = as.character(combinations$cause[i])
      )
    })
    
    # Clean up the original models_to_run if you want
    config$models_to_run <- NULL
    
    # Assign the flattened structure
    config$models_to_run_flat <- models_to_run_flat

  ## Make sure everything in models_to_run_flat is unique
  if(
    length(config$models_to_run_flat) == 
    length(unique(config$models_to_run_flat))
    ){
      return(config)
    } else {
      stop("Error: models_to_run_flat contains duplicate entries.")
    }

}

#-------------------------------
# write config function
#-------------------------------
write_config <- function(config, file_path) {
  yaml::write_yaml(config, file_path)
}


#-------------------------------
# model tuning function
#------------------------------
# run tuning function to process a single combination with error handling

run_tuning <- function(combination, grid_params, global_seed) {

  enc <- combination$encounter_type
  exposure <- combination$exposure_category
  cause <- combination$cause

  tryCatch({
    cat("Processing combination:", enc, exposure, cause, "\n")
    
    # load and subset data for this enc_type and exposure_category------------------------------
    # load and subset to enc and exposure on load
    df_train_test_encounter <- open_dataset(
        paste0(path_onedrive, "01_data/02_clean/test_train/df-train-test_sf.parquet")) %>% 
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
        assess = "75 days",
        skip = "75 days",
        cumulative = TRUE,
        date_var = date
      ) # this does not need a seed 

    ## resample data------------------------------
    resamples_kfold <- training(splits) |> 
      time_series_cv(
        assess = "40 days",
        slice_limit = 8,        
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
    model_phxgb_tune_seed <- gen_seed(global_seed, c(enc, exposure, cause, "model_phxgb_tune"))
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
                 seed = model_phxgb_tune_seed,
                 early_stop = TRUE,
                 validation = 0.2) # this needs a seed
    
    # generate grid for tuning------------------------------
    grid_phxgb_tune_seed <- gen_seed(global_seed, c(enc, exposure, cause, "grid_phxgb_tune"))
    set.seed(grid_phxgb_tune_seed)
    grid_phxgb_tune <- grid_space_filling(
      extract_parameter_set_dials(model_phxgb_tune) |>
        update(
          mtry = mtry(range = grid_params$mtry),
          min_n = min_n(range = grid_params$min_n),
          tree_depth = tree_depth(range = grid_params$tree_depth),
          learn_rate = learn_rate(range = grid_params$learn_rate),
          loss_reduction = loss_reduction(range = grid_params$loss_reduction, trans = log10_trans()),
          stop_iter = stop_iter(range = grid_params$stop_iter)
        ),
      size = 100
    )
    
    ## workflow for tuning------------------------------
    wflw_phxgb_tune <- workflow() |>
      add_model(model_phxgb_tune) |>
      add_recipe(rec_obj_phxgb)

    ## model tuning------------------------------
    tune_results_phxgb_seed <- gen_seed(global_seed, c(enc, exposure, cause, "tune_results_phxgb"))
    set.seed(tune_results_phxgb_seed)
    suppressWarnings({ suppressMessages({
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
          metrics = metric_set(yardstick::rmse, yardstick::rsq)
        )
    })}) # this needs a seed 
        
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
      enc_type = combination$enc_type,
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
calc_metrics <- function(data_type, dataset, model_table) {
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
              enc_type = enc,
              exposure_category = exposure,
              cause = cause,
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
    
    # Extract objects from result
    splits <- result$splits
    wflw_phxgb_tune <- result$wflw_phxgb_tune
    tune_results_phxgb <- result$tune_results_phxgb
    
    # Fit the final model with retry logic
    max_retries <- 5
    retry_count <- 0
    success <- FALSE
    wflw_fit <- NULL

    while (!success && retry_count < max_retries) {
      tryCatch({
        # fit the model 
        wflw_fit_seed <- gen_seed(global_seed, c(enc, exposure, cause, "wflw_fit"))
        set.seed(wflw_fit_seed)
        suppressWarnings({
          suppressMessages({
            wflw_fit <- wflw_phxgb_tune |>
                      finalize_workflow(select_best(tune_results_phxgb, metric = "rmse")) |>
                      fit(training(splits))
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

    # Generate modeltime table and calculate metrics with better error handling
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

    # Calculate training and testing metrics - pass model_tbl as parameter
    training_metrics <- calc_metrics("training", training(splits), model_tbl)
    testing_metrics <- calc_metrics("testing", testing(splits), model_tbl)
    
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

#-------------------------------
# seed generation function
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

