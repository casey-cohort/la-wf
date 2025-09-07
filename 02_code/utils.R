#-------------------------------
# LA wildfires project
# author: Lauren Wilner
# helper functions

#-------------------------------
# config functions

# TODO: use dataclass package at some point for validations 
# this will allow us to load via yaml and validate it through dataclass which will confirm all the fields are valid and we return the config obj. 

# read config function
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


# write config function
write_config <- function(config, file_path) {
  yaml::write_yaml(config, file_path)
}


#-------------------------------
# model tuning functions
get_model_combinations <- function(config, df){
  # step 1: make sure all combinations are in df -- throw error if not
    # write which combos were not found in df in the error message
  # step 2: subset df to only the combinations
}

#-------------------------------
# seed generation function
# args: global seed, markers (the stuff that makes the model plus the use case)
# then every time you need a seed for something, use the function to set the seed then run the function 

gen_seed <- function(global_seed, markers){
  
  # combine the markers into a single string
  combined_string <- paste(markers, collapse = "_")

  # generate a hash of the combined string with the global seed
  seed_hash <- digest::digest(combined_string, algo = "xxhash32", seed = global_seed)

  # convert the hash to an integer seed
  seed_integer <- as.integer(paste0("0x", substr(seed_hash, 1, 6)), 16)
  
  return(seed_integer)
}

