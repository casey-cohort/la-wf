#-------------------------------
# LA wildfires project
# Test Runner - Run multiple hyperparameter configurations in parallel
#-------------------------------
# This script allows you to test different hyperparameter configurations
# simultaneously by launching each as a separate R process
#-------------------------------

cat("========================================\n")
cat("Parallel test runner\n")
cat("Run multiple hyperparameter configurations simultaneously\n")
cat("========================================\n\n")

pacman::p_load(yaml, tictoc, parallel)

# Setup paths
source(paste0(getwd(), "/01_code/paths.R"))
source(paste0(getwd(), "/01_code/utils.R"))

#-------------------------------
# Define test configurations
#-------------------------------
# specify the parameters to change - base config values will be kept for others

test_configs <- list(
  # Test 1: shift mtry range up(only specify mtry, everything else stays from base config)
  test1 = list(
    name = "wide_mtry",
    description = "Test: mtry range [16,30] and min_n [1,40] and tree_depth [2,10] and stop_iter [15,60]",
    modifications = list(
      grid_params = list(
        min_n = c(1,40),
        tree_depth = c(2, 10),
        stop_iter = c(15, 60),
        mtry = c(16, 30)
        # min_n, tree_depth, learn_rate, loss_reduction, stop_iter will use base config values
      )
    )
  ),
  
  # Test 2: wider min_n range
  test2 = list(
    name = "wider_min_n",
    description = "Test: mtry range [16,30] and min_n [10,40] and tree_depth [2,10] and stop_iter [15,60]",
    modifications = list(
      grid_params = list(
        min_n = c(10,40),
        tree_depth = c(1, 10),
        stop_iter = c(15, 60),
        mtry = c(16, 30)
      )
    )
  )

  # Test 4: wider/higher stop_iter range 
  # test4 = list(
  #   name = "wider_stop_iter",
  #   description = "Test: Wider stop_iter range [15, 60]",
  #   modifications = list(
  #     grid_params = list(
  #       stop_iter = c(15, 60)
  #     )
  #   )
  # )

  # Test 4: Multiple changes (specify multiple params that change)
  # test4 = list(
  #   name = "multiple_changes",
  #   description = "Testing mtry and tree_depth together",
  #   modifications = list(
  #     grid_params = list(
  #       mtry = c(5, 15),
  #       tree_depth = c(5, 12)
  #       # Other params use base config
  #     )
  #   )
  # )
)

#-------------------------------
# Helper function to apply modifications to base config
#-------------------------------
apply_config_modifications <- function(base_config, modifications) {
  modified_config <- base_config
  
  # apply modifications
  for (key in names(modifications)) {
    if (is.list(modifications[[key]]) && key %in% names(modified_config)) {
      modified_config[[key]] <- modifyList(modified_config[[key]], modifications[[key]])
    } else {
      modified_config[[key]] <- modifications[[key]]
    }
  }
  
  # clean up: if we have models_to_run_flat, remove models_to_run to avoid conflicts when writing YAML
  # (the read_config function converts models_to_run to models_to_run_flat, so both might exist)
  if (!is.null(modified_config$models_to_run_flat)) {
    modified_config$models_to_run <- NULL
  }
  
  return(modified_config)
}

#-------------------------------
# helper function to prepare config file and script for one test
# note: works the same whether you have 1 test or many tests in your list
#-------------------------------
prepare_test_config <- function(test_name, test_config_mods, test_description, base_config_path, n_tests_total = 1) {
  # gen temporary config file for this test
  temp_config_path <- tempfile(pattern = paste0(test_name, "_"), fileext = ".yaml", tmpdir = tempdir())
  
  # read base config
  base_config <- read_config(base_config_path)
  
  # apply modifications for this test
  modified_config <- apply_config_modifications(base_config, test_config_mods)
  
  # Always use test description as model_description (override base config)
  # This ensures each test has its own unique description in the output
  if (!is.null(test_description)) {
    modified_config$model_description <- test_description
  }
  
  # write modified config to temp file
  write_config(modified_config, temp_config_path)
  
  cat("created config for", test_name, ":", test_description, "\n")
  cat("  config file:", temp_config_path, "\n")
  
  # gen R script to run this test
  test_script_path <- tempfile(pattern = paste0("run_", test_name, "_"), fileext = ".R", tmpdir = tempdir())
  
  # write the test runner script
  # use environment variable to point script to our temp config file
  # calculate cores per test when running in parallel (divide available cores by number of tests)
  total_cores <- parallel::detectCores()
  cores_per_test <- max(1, floor(total_cores / n_tests_total))  # at least 1 core per test
  
  test_script_content <- paste0(
    "# test runner for ", test_name, "\n",
    "# ", test_description, "\n",
    "# using config file: ", temp_config_path, "\n",
    "# running ", n_tests_total, " tests in parallel - using ", cores_per_test, " cores per test\n\n",
    "cat('\\n========================================\\n')\n",
    "cat('STARTING TEST: ", test_name, "\\n')\n",
    "cat('========================================\\n\\n')\n\n",
    "# set environment variable so script uses our temp config\n",
    "Sys.setenv(TEST_CONFIG_PATH = '", temp_config_path, "')\n",
    "# set cores per test for parallel execution\n",
    "Sys.setenv(TEST_CORES_PER_TEST = '", cores_per_test, "')\n\n",
    "source(paste0(getwd(), '/01_code/paths.R'))\n",
    "source(paste0(getwd(), '/01_code/utils.R'))\n",
    "\n",
    "# Step 1: Model tuning\n",
    "cat('\\n--- Step 1: Model Tuning ---\\n')\n",
    "source(paste0(getwd(), '/01_code/02_analysis/02_model_tune_phxgb_parallel.R'))\n",
    "cat('Step 1 complete\\n\\n')\n\n",
    "# Step 2: MBB confidence intervals\n",
    "cat('\\n--- Step 2: MBB Confidence Intervals ---\\n')\n",
    "source(paste0(getwd(), '/01_code/02_analysis/04_model_mbb_cis.R'))\n",
    "cat('Step 2 complete\\n\\n')\n\n",
    "# Step 3: Generate outputs (plots, tables)\n",
    "cat('\\n--- Step 3: Generate Outputs (Plots, Tables) ---\\n')\n",
    "source(paste0(getwd(), '/01_code/02_analysis/05_model_outputs.R'))\n",
    "cat('Step 3 complete\\n\\n')\n\n",
    "cat('\\n========================================\\n')\n",
    "cat('TEST COMPLETE: ", test_name, "\\n')\n",
    "cat('========================================\\n')\n"
  )
  
  writeLines(test_script_content, test_script_path)
  
  # return info needed to launch
  return(list(
    name = test_name,
    description = test_description,
    script_path = test_script_path,
    config_path = temp_config_path
  ))
}

#-------------------------------
# Main execution
#-------------------------------
cat("reading base configuration...\n")
base_config_path <- paste0(path_repo, "01_code/02_analysis/model_config.yaml")
base_config <- read_config(base_config_path)
cat("base config loaded.\n\n")

# prepare all test configurations
cat("preparing test configurations...\n")
test_runs <- list()
for (test_key in names(test_configs)) {
  test_def <- test_configs[[test_key]]
  test_run <- prepare_test_config(
    test_def$name,
    test_def$modifications,
    test_def$description,
    base_config_path,
    n_tests_total = length(test_configs)
  )
  test_runs[[test_key]] <- test_run
}
cat("\nprepared", length(test_runs), "test configurations.\n\n")

# Ask user if they want to run in parallel or sequentially
cat("========================================\n")
cat("ready to run tests\n")
cat("========================================\n")
cat("How would you like to run these tests?\n")
cat("  1. Run all tests in parallel (separate R processes) - fast but no visible output\n")
cat("  2. Run tests sequentially (one at a time) - slower but visible output\n")
cat("  3. Exit without running\n\n")
cat("NOTE: Option 2 lets you see everything happening in real-time!\n\n")

# try to get user input with a default
if (interactive()) {
  choice <- readline("Enter choice (1/2/3) [default: 1 for parallel]: ")
  if (choice == "") choice <- "1"  # default to parallel if just hit enter
} else {
  choice <- readLines("stdin", n=1, warn=FALSE)
  if (length(choice) == 0 || trimws(choice) == "") choice <- "1"  # default to parallel
}

choice <- trimws(choice)
if (choice == "") choice <- "1"  # final fallback default

cat("You selected option:", choice, "\n\n")

if (choice == "3") {
  cat("exiting without running tests.\n")
  quit(save = "no", status = 0)
}

if (choice == "1") {
  # Parallel execution
  cat("\n========================================\n")
  cat("PARALLEL EXECUTION MODE\n")
  cat("========================================\n")
  cat("Launching tests in parallel...\n")
  cat("Each test will run in a separate R process.\n")
  cat("Monitor progress by checking output files.\n\n")
  
  temp_files <- list()
  
  for (test_key in names(test_runs)) {
    test_run <- test_runs[[test_key]]
    cat("Launching:", test_run$name, "-", test_run$description, "\n")
    cat("  Script:", test_run$script_path, "\n")
    cat("  Config:", test_run$config_path, "\n")
    
    # launch in background using Rscript
    # need to cd to repo directory first so paths work correctly
    repo_dir <- getwd()
    log_file <- paste0(tempdir(), "/test_", test_run$name, "_", Sys.getpid(), ".log")
    
    # create log file for this test so we can monitor progress
    writeLines(paste0("Test started: ", test_run$name, "\nStarted at: ", Sys.time(), "\n"), log_file)
    
    cat("  Log file:", log_file, "\n")
    cat("  launching command...\n")
    
    # Launch in background using Rscript with output redirection to log file
    # Use system2 for better control over background processes
    # Change to repo directory in the command to avoid affecting current session
    system2("sh",
            args = c("-c", 
                    paste0("cd '", repo_dir, "' && Rscript '", test_run$script_path, "' >> '", log_file, "' 2>&1")),
            wait = FALSE)
    
    # small delay to avoid file conflicts
    Sys.sleep(2)
    
    # track temp files for cleanup later (config is saved in output folder by model script)
    temp_files <- c(temp_files, test_run$config_path, test_run$script_path)
  }
  
  cat("\n========================================\n")
  cat("all tests launched in parallel!\n")
  cat("========================================\n\n")
  
  # check running processes
  cat("checking running processes...\n")
  r_processes <- system("ps aux | grep -i 'Rscript.*test' | grep -v grep", intern = TRUE)
  if (length(r_processes) > 0) {
    cat("found", length(r_processes), "test process(es) running:\n")
    for (proc in r_processes) {
      cat("  ", proc, "\n")
    }
  } else {
    cat("no test processes found running. they may have finished or failed to start.\n")
  }
  
  cat("\n========================================\n")
  cat("how to monitor progress:\n")
  cat("========================================\n")
  cat("1. check log files in:", tempdir(), "\n")
  cat("2. look for output folders in:", paste0(path_onedrive, "02_output/"), "\n")
  cat("3. check running processes: ps aux | grep Rscript\n")
  cat("4. tests will create dated folders like: model_run_YYYY-MM-DD.v###/\n\n")
  
  # NOTE: Do NOT clean up temp files in parallel mode - they're needed by Step 2 and Step 3
  # The temp files will be cleaned up automatically when the R session ends
  # Config files are saved in each model run's output folder for reference
  cat("Note: Temp config files will remain until tests complete.\n")
  cat("They are needed for Steps 2 and 3 (MBB and outputs).\n")
  cat("Config files are also saved in each model run's output folder.\n\n")
  
} else if (choice == "2") {
  # sequential execution 
  cat("\n========================================\n")
  cat("SEQUENTIAL EXECUTION MODE\n")
  cat("you will see all output in this console!\n")
  cat("========================================\n\n")
  cat("Running tests sequentially (one at a time)...\n")
  cat("temp config files will be cleaned up after each test.\n\n")
  
  total_tests <- length(test_runs)
  test_num <- 0
  
  for (test_key in names(test_runs)) {
    test_num <- test_num + 1
    test_run <- test_runs[[test_key]]
    
    cat("\n")
    cat("========================================\n")
    cat("test", test_num, "of", total_tests, ":", test_run$name, "\n")
    cat("========================================\n")
    cat("description:", test_run$description, "\n")
    cat("started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    cat("========================================\n\n")
    
    # source and run the test script - you'll see all output here!
    test_start <- Sys.time()
    source(test_run$script_path)
    test_end <- Sys.time()
    test_duration <- test_end - test_start
    
    cat("\n========================================\n")
    cat("✓ completed:", test_run$name, "\n")
    cat("duration:", round(as.numeric(test_duration, units = "mins"), 2), "minutes\n")
    cat("========================================\n")
    
    # clean up temp files (config is saved in output folder by the model script)
    if (file.exists(test_run$config_path)) {
      unlink(test_run$config_path)
    }
    if (file.exists(test_run$script_path)) {
      unlink(test_run$script_path)
    }
    
    cat("\n")
  }
  
  cat("\n========================================\n")
  cat("all tests completed!\n")
  cat("========================================\n")
  cat("(config files are saved in each model run's output folder)\n")
  cat("check output folders for results.\n\n")
} else {
  cat("invalid choice. exiting.\n")
}

cat("\ntest runner complete!\n")

