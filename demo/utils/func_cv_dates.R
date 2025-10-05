get_cv_dates <- function(resampling_object) {
  if (!inherits(resampling_object, "time_series_cv")) {
    stop("Input must be a time_series_cv object")
  }
  
  fold_info <- resampling_object$splits |>
    map_dfr(function(split) {
      train_data <- analysis(split)
      val_data <- assessment(split)
      
      train_dates <- train_data |>
        summarise(
          train_start = min(date),
          train_end = max(date),
          train_days = n()
        )
      
      val_dates <- val_data |>
        summarise(
          val_start = min(date),
          val_end = max(date),
          val_days = n()
        )
      
      # Create fold ID - use the split's id if available, otherwise create one
      fold_id <- if (!is.null(split$id)) split$id else "Unknown"
      
      bind_cols(
        id = fold_id,
        train_dates,
        val_dates
      )
    })
  
  fold_info |>
    mutate(
      training_period = paste(format(train_start, "%d-%b-%y"), "to", format(train_end, "%d-%b-%y")),
      validation_period = paste(format(val_start, "%d-%b-%y"), "to", format(val_end, "%d-%b-%y"))
    ) |>
    select(id, training_period, validation_period, train_days, val_days)
}
