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
# gen ver number 
#-------------------------------
# look for if today's date exists in output folder. if so, look at version number after the date, and increment it by 1. if not, start at v001. always pad with 0's such that the ver number is 3 digits.
# New format: model_run_YYYY-MM-DD.v###_x##_sim###
gen_ver_number <- function(path) {
  all_dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  
  # get today's date in the format used in folder names
  today <- Sys.Date()
  
  # filter for folders that match the pattern with today's date
  # Pattern matches: model_run_2025-10-22.v001_x20_sim1000
  pattern <- paste0("^model_run_", today, "\\.v\\d{3}_x\\d+_sim\\d+$")
  existing_folders <- all_dirs[grepl(pattern, all_dirs)]
  
  if (length(existing_folders) == 0) {
    return("v001")
  } else {
    # extract version numbers from existing folders
    versions <- sapply(existing_folders, function(x) {
      # extract the version number (the 3 digits after .v and before _x)
      version_match <- regmatches(x, regexpr("\\.v(\\d{3})_", x))
      if (length(version_match) > 0) {
        as.integer(sub("\\.v(\\d{3})_", "\\1", version_match))
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
# read config function - reads config from YAML file
# Can override default path using TEST_CONFIG_PATH environment variable
#-------------------------------
read_config <- function(file_path = NULL) {
  # Check for environment variable first (for parallel testing)
  test_config <- Sys.getenv("TEST_CONFIG_PATH", unset = "")
  if (test_config != "") {
    file_path <- test_config
    cat("Using test config from environment:", file_path, "\n")
  } else if (is.null(file_path)) {
    # If no env var and no path provided, use default
    file_path <- paste0(getwd(), "/01_code/02_analysis/model_config.yaml")
  }
  # If file_path was provided explicitly and no env var, use the provided path

  # Read in YAML file -------------------------------
  config <- yaml::read_yaml(file_path)

  # Handle YAML parser quirk that creates duplicate fields -------------------------------
  # When using multi-line format for models_to_run_flat, the parser creates both 
  # models_to_run and models_to_run_flat as references to the same object
  yaml_parser_created_duplicate <- FALSE
  if(!is.null(config$models_to_run) & !is.null(config$models_to_run_flat)){
    # Check if they're identical (YAML parsing quirk with multi-line format)
    if(identical(config$models_to_run, config$models_to_run_flat)){
      # Mark this as a parser quirk (they're the same object, use models_to_run_flat)
      yaml_parser_created_duplicate <- TRUE
    } else {
      # If they're different, user specified both which is an error
      stop("Error: Must provide only one of 'models_to_run' (cartesian product) or 'models_to_run_flat' (explicit list) in the config file.")
    }
  }

  # Validate that at least one is provided -------------------------------
  if(is.null(config$models_to_run) & is.null(config$models_to_run_flat)){
    stop("Error: Must provide either 'models_to_run' or 'models_to_run_flat' in the config file.")
  }

  # If models_to_run is provided (and not a YAML parser duplicate), expand to cartesian product -------
  if(!is.null(config$models_to_run) & !yaml_parser_created_duplicate){
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
    
    # Convert to list of lists
    models_to_run_flat <- lapply(1:nrow(combinations), function(i) {
      list(
        encounter_type = as.character(combinations$encounter_type[i]),
        exposure_category = as.character(combinations$exposure_category[i]),
        cause = as.character(combinations$cause[i])
      )
    })
    
    # Store as models_to_run_flat
    config$models_to_run_flat <- models_to_run_flat
    # Keep models_to_run for reference
  }

  # Validate that models_to_run_flat is not empty -------------------------------
  if(length(config$models_to_run_flat) == 0){
    stop("Error: 'models_to_run_flat' cannot be empty.")
  }

  # Make sure everything in models_to_run_flat is unique -------------------------------
  # Convert to data frame for proper duplicate checking
  combinations_df <- do.call(rbind, lapply(config$models_to_run_flat, function(x) {
    data.frame(
      encounter_type = x$encounter_type,
      exposure_category = x$exposure_category,
      cause = x$cause,
      stringsAsFactors = FALSE
    )
  }))
  
  # Check for duplicates
  if(nrow(combinations_df) != nrow(unique(combinations_df))){
    # Find and report duplicates
    duplicates <- combinations_df[duplicated(combinations_df) | duplicated(combinations_df, fromLast = TRUE), ]
    cat("Duplicate combinations found:\n")
    print(duplicates)
    stop("Error: you have specified duplicate model combinations! Please make sure all encounter_type -- exposure_category -- cause combinations are unique.")
  }
  
  return(config)

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
# run tuning function to process a single combination

run_tuning <- function(combination, grid_params, train_test_params, global_seed, train_test_path) {

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
    } # this needs a seed 
        
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
