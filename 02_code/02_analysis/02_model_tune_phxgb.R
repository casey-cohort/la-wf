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
pacman::p_load(modeltime, tidymodels, tidyverse, timetk, tictoc)

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
    
# Initialize nested list structure
all_results <- list()

# cause vars to loop over
causes <- colnames(df_train_test) %>% str_subset("^num_enc")

#------------------------------
# loop through each enc_type -- exposure_category -- cause combination

for (enc in unique(df_train_test$enc_type)) {
    print(enc)
    if (!enc %in% names(all_results)) {
        all_results[[enc]] <- list()
    }
    
    for (exposure in unique(df_train_test$exposure_category)) {
        print(exposure)
        if (!exposure %in% names(all_results[[enc]])) {
            all_results[[enc]][[exposure]] <- list()
        }
        
        # subset data for this enc_type and exposure_category
        df_train_test_encounter <- df_train_test %>%
            filter(enc_type == enc, exposure_category == exposure)
        
      for (cause in causes) {
          print(cause)
    
      ## split data into training and test sets -------------------------------------
      splits <- df_train_test_encounter |>
        time_series_split(
          assess = "75 days", # LBW comment: what is this and how do we choose it? we took 30% of the total training period, which is the before the outcome time. we have two seasons with 90ish days, one with 60ish days, so we had about 250 days total and so the assess period is 75 days. the assess period is the period you are using to train the model. 
          skip = "75 days",    # same as the assess period to avoid overlapping. with this, it takes 75 days then it skips 75 days then it takes the next 75 days. if you dont have this, then it takes the first 75 days and then starts the next 75 days right after the first 75 days.
          cumulative = TRUE,
          date_var = date
        )

      ## resample data ---------------------------------------------------
      resamples_kfold <- training(splits) |> 
        time_series_cv(
          assess = "40 days", # LBW comment: what is this and how do we choose it? 40 days is X% of what is left. if you do 250-75, you get 175. Then 40 is about 25% of that. so during the 75 days, it resamples 8 times for a 40 day period within each 75 day sub period. essentially training and testing for different sub time periods within the training period.    
          slice_limit = 8,        
          cumulative = TRUE # should we change this to FALSE? how does this interact with skip command?
        )
      
      # NOTE: talk to arnab about assess + cumulative decisions and how it works.
      
      ## recipe for modeling ---------------------------------------------------
      formula <- as.formula(paste(cause, "~ ."))
      print(formula)
      
      rec_obj_phxgb <- recipe(formula, training(splits)) |>
        # Time series features 
        step_holiday(date, holidays = timeDate::listHolidays("US")) |>
        # Minimal seasonal components
        step_mutate(
          month=factor(month(date)),
          year=factor(year(date)),
          is_weekend = factor(if_else(wday(date, week_start = 1) %in% c(6, 7), "weekend", "weekday")),
          day_of_week = factor(wday(date, label = TRUE)),
          month_day = format(date, "%m-%d"),  # Create month_day in "MM-DD" format
          business_closed_holiday = factor(if_else(
            month_day %in% c(
              "01-01",  # New Year's Day
              "01-20",  # Martin Luther King Jr. Day
              "07-04",  # Independence Day
              "12-25"   # Christmas Day
            ), "business_closed", "business_open" # note we dont have thanksgiving specified here since the date changes, but it is in the holiday var.
          ))
        ) |>
        step_rm(month_day) |> # only used this to find kaiser holidays and remove, so dropping this var here
        step_novel(all_nominal()) |> # makes this recipe generalizable to a different dataset (e.g., our post event data)
        # cleaning steps
        step_rm(matches("(.iso$)|(.xts$)|(.minute)|(.second)|(.hour)|(.am.pm)")) |> # removing subdaily measures in our time series that we dont need
        step_zv("date_USInaugurationDay", "date_USCPulaskisBirthday", "date_USJuneteenthNationalIndependenceDay",
                        "date_USDecorationMemorialDay", "date_USColumbusDay", "date_USGoodFriday",
                        "date_USIndependenceDay", "date_USLaborDay", "date_USLincolnsBirthday", "date_USMemorialDay", "date_USPresidentsDay",
                        "date_USWashingtonsBirthday") |> # remove holidays that fall outside our study period
        step_normalize(all_numeric_predictors())
      
      rec_obj_phxgb |> prep() |> juice() |> colnames() # returns all variables in the model
      
      ## specify models ---------------------------------------------------
      model_phxgb_tune <- prophet_boost(
        mode = "regression",
        growth = "linear",
        seasonality_yearly = FALSE,  # Disable yearly seasonality since we have partial year
        # xgboost parameters
        mtry = tune(),
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(),
        stop_iter = tune()
      ) |>
        set_engine("prophet_xgboost",
                  #  set.seed = 0112358, # LBW comment: why do we set the seed again here?? 
                  early_stop = TRUE, # if the model is performing well, it will stop early. do we want this to be true? 
                  validation = 0.2)
      
      # generate grid for tuning---------------------------------------------------
      
      'grid_phxgb_tune' <- grid_space_filling(
        extract_parameter_set_dials(model_phxgb_tune) |>
          update(
            # XGBoost parameters 
          mtry = mtry(range = c(mtry_min, mtry_max)),
          min_n = min_n(range = c(min_n_min, min_n_max)),
          tree_depth = tree_depth(range = c(tree_depth_min, tree_depth_max)),
          learn_rate = learn_rate(range = c(learn_rate_min, learn_rate_max)),
          loss_reduction = loss_reduction(range = c(loss_reduction_min, loss_reduction_max), trans = log10_trans()),
          stop_iter = stop_iter(range = c(stop_iter_min, stop_iter_max))
          ),
        size = 100
        ## LBW question: what is this size? the grid size? 
      )
      
      ## workflow for tuning ---------------------------------------------------
      wflw_phxgb_tune <- workflow() |>
        add_model(model_phxgb_tune) |>
        add_recipe(rec_obj_phxgb)
      
      ## model tuning ---------------------------------------------------
      tic(quite = FALSE)
      suppressWarnings({
        tune_results_phxgb <- wflw_phxgb_tune |>
          tune_grid(
            resamples = resamples_kfold,
            grid = grid_phxgb_tune,
            control = control_grid(
              verbose = TRUE,
              allow_par = TRUE,
              save_pred = TRUE,
              save_workflow = TRUE,
              parallel_over = "resamples",
              event_level = "first",
              pkgs = c("tidymodels", "modeltime", "timetk")
            ),
            metrics = metric_set(rmse, rsq)
          )
      })
      toc() 
      
      # Look at parameters of model
      best_params <- tune_results_phxgb |> select_best(metric = "rmse")
      print(best_params)
      
      all_results[[enc]][[exposure]][[cause]] <- list(
          splits = splits,
          tune_results_phxgb = tune_results_phxgb,
          wflw_phxgb_tune = wflw_phxgb_tune,
          best_params = best_params,
          resamples_kfold = resamples_kfold,
          rec_obj_phxgb = rec_obj_phxgb,
          df_train_test_encounter = df_train_test_encounter,
          formula = formula,
          # store identifiers
          enc_type = enc,
          exposure_category = exposure,
          cause = cause
      )
    }
  }
}

# save nested data
save(all_results, file = paste0(path_repo, "03_output/all_model_tuning_results.RData"))





