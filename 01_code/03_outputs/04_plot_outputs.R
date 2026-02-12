## ---------------------------
## LA 2025 Wildfires - Daily Excess Plots
## Adapted from EMB & LBW code
## Goal: Produce plots visualizing excess ED/IP visits
##       (raw, per 1000, and percent) during LA 2025 wildfires
## Breakdown: 
##       2 visit types (ED versus Inpatient (IP))
##       5 outcomes/encounter types (overall, cardio, resp, neuro, injury)
##       stratified by 4 exposure levels (evac_rate, high_smoke, mid_smoke, none)
##       includes cumulative effect plots
## ---------------------------

# setup ---------------------------
if(!requireNamespace('pacman', quietly = TRUE)) install.packages('pacman') 
pacman::p_load(readr, readxl, snakecase, lubridate, purrr,
               dplyr, tidyr, stringr, forcats, cowplot,
               ggplot2, patchwork, gridExtra, ggtext, here, yaml)

# Set paths (centralized in repo)
source(paste0(getwd(), "/01_code/paths.R"))

# Get analysis_mode from environment (set by run_final_outputs) or fall back to config
analysis_mode <- Sys.getenv("ANALYSIS_MODE", unset = "")
if (analysis_mode == "") {
  # Fall back to config file
  config <- yaml::read_yaml(paste0(getwd(), "/01_code/02_analysis/model_config.yaml"))
  analysis_mode <- config$analysis_type
  if (is.null(analysis_mode) || analysis_mode == "") {
    analysis_mode <- "all"
  }
}
cat("Analysis mode:", analysis_mode, "\n")

# Create display label for titles
analysis_label <- switch(analysis_mode,
  "all" = "All Exposures",
  "palisades" = "Palisades Evacuation",
  "eaton" = "Eaton Evacuation",
  "evac_analysis" = "Evacuation Analysis",
  tools::toTitleCase(analysis_mode)
)
cat("Analysis label for plots:", analysis_label, "\n")

# =============================================================================
# PARAMETERS
# =============================================================================
# Read from environment if set (by run_final_outputs), otherwise use defaults
num_days_env <- Sys.getenv("PLOT_NUM_DAYS", unset = "")
num_days <- if (num_days_env != "") as.integer(num_days_env) else 7

include_no_exposure_env <- Sys.getenv("PLOT_INCLUDE_NO_EXPOSURE", unset = "")
include_no_exposure <- if (include_no_exposure_env != "") as.logical(include_no_exposure_env) else FALSE

aggregate_to_weekly <- FALSE   # if TRUE, aggregate daily results into 7-day bins for plotting
weekly_x_labels <- "week"     # "week" (Week 1/2/3...) or "range" (YYYY-MM-DD - YYYY-MM-DD); only used when aggregate_to_weekly = TRUE
cumulative_only <- TRUE       # if TRUE, also generate cumulative-only plots (for main manuscript)

# paths (with analysis_mode subfolder for non-"all")
tables_dir <- paste0(path_onedrive, "03_modeling-and-results/04_bested-results/")
if (analysis_mode != "all") {
  tables_dir <- paste0(tables_dir, analysis_mode, "/")
}
rootdir_daily <- paste0(tables_dir, "daily/")          # folder containing the daily xlsx files
rootdir_aggregated <- paste0(tables_dir, "aggregated/") # folder containing the aggregated xlsx files
output_dir <- paste0(tables_dir, "plots/")             # where to save plots
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("Reading from:", tables_dir, "\n")

# =============================================================================
# READ DATA
# =============================================================================

# read in daily xlsx files and combine ---------------------------
file_list <- list.files(rootdir_daily, pattern = "\\.xlsx$", full.names = TRUE)
results <- map_dfr(file_list, ~read_xlsx(.x))

# read in aggregated/cumulative data for the specified number of days
aggregated_file <- paste0(rootdir_aggregated, "excess_hosp_", num_days, "days.xlsx")
cumulative_results <- read_xlsx(aggregated_file)

# =============================================================================
# CLEAN DATA
# =============================================================================

# helper function to parse combined CI strings like "-12.1 (-43.6, -7.1)" or "-5.1% (-18.6%, -3.0%)"
parse_ci_column <- function(df, ci_col, prefix) {
  df %>%
    mutate(
      # remove % signs first for easier parsing
      .tmp_ci = str_remove_all(!!sym(ci_col), "%"),
      # extract estimate (number before the parenthesis)
      !!paste0(prefix, "_estimate") := as.numeric(str_extract(.tmp_ci, "^[\\-\\d\\.]+")),
      # extract lower CI (first number in parentheses)
      !!paste0(prefix, "_lci") := as.numeric(str_extract(.tmp_ci, "(?<=\\()[\\-\\d\\.]+")),
      # extract upper CI (second number in parentheses)
      !!paste0(prefix, "_uci") := as.numeric(str_extract(.tmp_ci, "(?<=,\\s?)[\\-\\d\\.]+(?=\\))"))
    ) %>%
    select(-.tmp_ci)
}

# helper function to clean data
clean_results <- function(df) {
  
  # check if we have separate columns or combined CI columns
  has_separate_cols <- "excess" %in% names(df)
  has_combined_ci <- "excess_CI" %in% names(df)
  
  # first do the parsing/renaming based on format
  if (has_separate_cols) {
    # daily format - rename columns
    df <- df %>%
      rename(
        excess_estimate = excess,
        excess_lci = excess_lo,
        excess_uci = excess_hi,
        excess_pct_estimate = excess_pct,
        excess_pct_lci = excess_pct_lo,
        excess_pct_uci = excess_pct_hi
      )
  } else if (has_combined_ci) {
    # aggregated format - parse CI strings
    df <- df %>%
      parse_ci_column("excess_CI", "excess") %>%
      parse_ci_column("excess_pct_CI", "excess_pct")
  }
  
  # then do the common transformations
  df <- df %>%
    mutate(
      # extract visit type from enc_type column
      visit_type = factor(enc_type, levels = c("ED", "IP")),
      # use exposure_category directly
      exposure = factor(exposure_category, levels = c("none", "mid_smoke", "high_smoke", "evac")),
      # extract encounter type: get everything after the last underscore
      encounter_raw = str_extract(cause, "[^_]+$"),
      # recode to human-readable labels
      encounter_type = case_when(
        encounter_raw == "enc" ~ "Total",
        encounter_raw == "cardio" ~ "Cardiovascular",
        encounter_raw == "resp" ~ "Respiratory",
        encounter_raw == "injury" ~ "Injury",
        encounter_raw == "neuro" ~ "Neuropsychiatric",
        TRUE ~ encounter_raw
      ),
      encounter_type = factor(encounter_type, 
                              levels = c("Total", "Cardiovascular", "Injury", "Neuropsychiatric", "Respiratory"))
    )
  
  # add date column if period exists and looks like a single date (not a range)
  if ("period" %in% names(df)) {
    # check if period looks like a single date (contains "-" but not " - " which indicates a range)
    if (any(grepl("^\\d{4}-\\d{2}-\\d{2}$", df$period))) {
      df <- df %>% mutate(date = as.Date(period))
    }
  }
  
  return(df)
}

# clean both datasets
results <- clean_results(results)
cumulative_results <- clean_results(cumulative_results)

# filter to specified number of days for daily results
results <- results %>%
  arrange(date) %>%
  filter(date <= min(date) + days(num_days - 1))

# warn if requested num_days exceeds what's available upstream
available_days <- n_distinct(results$date)
if (available_days < num_days) {
  warning(
    "Requested num_days = ", num_days, " but only ", available_days,
    " day(s) are available in the daily xlsx outputs (",
    as.character(min(results$date, na.rm = TRUE)), " to ",
    as.character(max(results$date, na.rm = TRUE)), ").\n",
    "This is upstream of plotting (i.e., the daily excess outputs only contain that many days)."
  )
}

# filter out "none" exposure if not including it
if (!include_no_exposure) {
  results <- results %>% filter(exposure != "none")
  cumulative_results <- cumulative_results %>% filter(exposure != "none")
  
  # update factor levels
  results$exposure <- droplevels(results$exposure)
  cumulative_results$exposure <- droplevels(cumulative_results$exposure)
}

# =============================================================================
# OPTIONAL: AGGREGATE DAILY RESULTS TO WEEKLY BINS (FOR PLOTTING)
# =============================================================================

if (aggregate_to_weekly) {
  week_len_days <- 7L
  start_date <- min(results$date, na.rm = TRUE)
  end_date <- max(results$date, na.rm = TRUE)
  has_observed <- "observed" %in% names(results)
  has_expected <- "expected" %in% names(results)
  
  # Assign each day to a week bin starting at start_date
  results <- results %>%
    mutate(
      week_index = as.integer(floor(as.numeric(difftime(date, start_date, units = "days")) / week_len_days)) + 1L,
      week_start = start_date + days((week_index - 1L) * week_len_days),
      week_end = pmin(week_start + days(week_len_days - 1L), end_date),
      week_range = paste0(format(week_start, "%Y-%m-%d"), " - ", format(week_end, "%Y-%m-%d")),
      week_label = paste0("Week ", week_index),
      # pick weekly x-axis label style ONCE (weekly_x_labels is a scalar parameter)
      period_week = if (identical(weekly_x_labels, "range")) week_range else week_label
    ) %>%
    group_by(enc_type, exposure_category, cause, week_index, week_start, week_end, week_range, week_label) %>%
    summarise(
      # Keep a representative date for plotting (use week_start)
      date = week_start,
      period = first(period_week),
      
      # Aggregate counts over the week (approximate CI aggregation by summing bounds)
      observed = if (has_observed) sum(observed, na.rm = TRUE) else NA_real_,
      expected = if (has_expected) sum(expected, na.rm = TRUE) else NA_real_,
      
      excess_estimate = sum(excess_estimate, na.rm = TRUE),
      excess_lci = sum(excess_lci, na.rm = TRUE),
      excess_uci = sum(excess_uci, na.rm = TRUE),
      
      # Recompute percent excess from aggregated excess/expected when available
      excess_pct_estimate = dplyr::if_else(!is.na(expected) && expected != 0,
                                          (excess_estimate / expected) * 100,
                                          mean(excess_pct_estimate, na.rm = TRUE)),
      excess_pct_lci = dplyr::if_else(!is.na(expected) && expected != 0,
                                     (excess_lci / expected) * 100,
                                     mean(excess_pct_lci, na.rm = TRUE)),
      excess_pct_uci = dplyr::if_else(!is.na(expected) && expected != 0,
                                     (excess_uci / expected) * 100,
                                     mean(excess_pct_uci, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(date) %>%
    mutate(period = factor(period, levels = unique(period))) %>%
    # Re-run the standard cleaning to restore factor variables / labels
    clean_results()
  
  # Keep a simple mapping for reference when using "Week 1/2/3..." labels
  week_map <- results %>%
    distinct(week_index, week_label, week_range) %>%
    arrange(week_index)
}

# =============================================================================
# COLOR PALETTE AND LABELS
# =============================================================================

# For evac_analysis mode, use evac_type (eaton/palisades) as the exposure grouping
if (analysis_mode == "evac_analysis") {
  # Check if evac_type column exists
  if ("evac_type" %in% names(results)) {
    # Replace exposure with evac_type for evac rows, keep "none" as is
    # Filter out any rows where evac_type is NA (except for "none" exposure)
    results <- results %>%
      filter(exposure == "none" | !is.na(evac_type)) %>%
      mutate(exposure = ifelse(!is.na(evac_type) & exposure == "evac", evac_type, exposure))
    cumulative_results <- cumulative_results %>%
      filter(exposure == "none" | !is.na(evac_type)) %>%
      mutate(exposure = ifelse(!is.na(evac_type) & exposure == "evac", evac_type, exposure))
  }
  
  # Remove any remaining NA exposures
  results <- results %>% filter(!is.na(exposure))
  cumulative_results <- cumulative_results %>% filter(!is.na(exposure))
  
  # Define colors for evac_analysis (none + eaton + palisades)
  if (include_no_exposure) {
    exposure_colors <- c(
      "none" = "#a6cee3",       # light blue (same as other plots)
      "eaton" = "#7f0000",      # dark red (evac color)
      "palisades" = "#d98888"   # lighter red/salmon
    )
    exposure_labels <- c(
      "none" = "None",
      "eaton" = "Eaton",
      "palisades" = "Palisades"
    )
  } else {
    exposure_colors <- c(
      "eaton" = "#7f0000",      # dark red (evac color)
      "palisades" = "#d98888"   # lighter red/salmon
    )
    exposure_labels <- c(
      "eaton" = "Eaton",
      "palisades" = "Palisades"
    )
  }
  
  # Update factor levels for proper ordering
  exposure_order <- if (include_no_exposure) c("none", "eaton", "palisades") else c("eaton", "palisades")
  results$exposure <- factor(results$exposure, levels = exposure_order)
  cumulative_results$exposure <- factor(cumulative_results$exposure, levels = exposure_order)
  
} else {
  # Standard colors for "all" analysis mode
  if (include_no_exposure) {
    exposure_colors <- c(
      "none" = "#a6cee3",      # light blue
      "mid_smoke" = "#f2c88f", # light orange  
      "high_smoke" = "#d37750", # darker orange
      "evac" = "#7f0000"       # dark red
    )
    exposure_labels <- c(
      "none" = "None",
      "mid_smoke" = "Moderate smoke",
      "high_smoke" = "High smoke",
      "evac" = "Evacuation zone"
    )
  } else {
    exposure_colors <- c(
      "mid_smoke" = "#f2c88f", # light orange  
      "high_smoke" = "#d37750", # darker orange
      "evac" = "#7f0000"       # dark red
    )
    exposure_labels <- c(
      "mid_smoke" = "Moderate smoke",
      "high_smoke" = "High smoke",
      "evac" = "Evacuation zone"
    )
  }
}

# =============================================================================
# PLOTTING FUNCTIONS
# =============================================================================

# daily plotting function ---------------------------
plot_estimates <- function(data, visit, encounter, prefix) {
  
  # plot title based on visit type and encounter
  if(encounter == "Total" & visit == "ED") {
    plot_title <- "A                              Emergency Department"
  } else if (encounter == "Total" & visit == "IP") {
    plot_title <- "B                                   Inpatient"
  } else {
    plot_title <- ""
  }
  
  # y-axis label - use encounter name directly since it's already human-readable
  y_axis_label <- case_when(
    grepl("per", prefix) ~ paste0(encounter, "\n \nExcess visits\n per 1000"),
    grepl("pct", prefix) ~ paste0(encounter, "\n \nPercent excess\n visits"),
    TRUE ~ paste0(encounter, "\n \nExcess visits")
  )
  
  # remove y axis label for IP plots (right column)
  if (visit == "IP") {
    y_axis_label <- ""
  }
  
  # filter data
  plot_data <- data %>% 
    filter(visit_type == visit, encounter_type == encounter)
  
  # create the plot
  week_plot <- plot_data %>% 
    ggplot(aes(x = if (aggregate_to_weekly) period else date, 
               y = !!sym(paste0(prefix, "_estimate")), 
               color = exposure)) +
    geom_point(position = position_dodge(width = 0.6), size = 2.75) +
    geom_errorbar(aes(ymin = !!sym(paste0(prefix, "_lci")), 
                      ymax = !!sym(paste0(prefix, "_uci"))), 
                  position = position_dodge(width = 0.6), width = .2, linewidth = 1.25) +
    scale_color_manual(values = exposure_colors, 
                       labels = exposure_labels,
                       name = "Exposure group") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    theme_minimal(base_size = 22) +
    theme(plot.title = element_text(hjust = 0, size = 26, face = "bold"),
          axis.text.x = element_text(
            angle = if (aggregate_to_weekly) 0 else 90,
            vjust = if (aggregate_to_weekly) 0.5 else 0.5,
            hjust = if (aggregate_to_weekly) 0.5 else 1,
            margin = margin(t = 0)
          ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "darkgrey", linewidth = 0.5),
          legend.position = "none") +
    labs(x = NULL, y = y_axis_label, title = plot_title)
  
  if (aggregate_to_weekly) {
    week_plot <- week_plot + scale_x_discrete(drop = FALSE)
  } else {
    week_plot <- week_plot + scale_x_date(
      date_breaks = "1 day",
      date_labels = "%Y-%m-%d",
      expand = expansion(add = 0.5)
    )
  }
  
  return(week_plot)
}

# cumulative plotting function ---------------------------
plot_cumulative <- function(data, visit, encounter, prefix, for_cumulative_only = FALSE) {
  
  # plot title - for cumulative-only plots, show visit type; for full plots, show "Cumulative"
  if (for_cumulative_only) {
    # For cumulative-only layout: show ED/IP label for Total row only
    if (encounter == "Total") {
      plot_title <- if (visit == "ED") "Emergency Department" else "Inpatient"
    } else {
      plot_title <- ""
    }
  } else {
    # For full layout: just show "Cumulative" for Total row
    if (encounter == "Total") {
      plot_title <- "Cumulative"
    } else {
      plot_title <- ""
    }
  }
  
  # y-axis label - show encounter type on left side
  y_axis_label <- if (for_cumulative_only) encounter else ""
  
  # Styling adjustments for cumulative-only plots
  base_font_size <- if (for_cumulative_only) 18 else 22
  title_size <- if (for_cumulative_only) 20 else 26
  
  # filter data
  plot_data <- data %>% 
    filter(visit_type == visit, encounter_type == encounter)
  
  # create the plot
  cum_plot <- plot_data %>% 
    ggplot(aes(x = exposure, 
               y = !!sym(paste0(prefix, "_estimate")), 
               color = exposure)) +
    geom_point(size = 2.75) +
    geom_errorbar(aes(ymin = !!sym(paste0(prefix, "_lci")), 
                      ymax = !!sym(paste0(prefix, "_uci"))), 
                  width = .2, linewidth = 1.25) +
    scale_color_manual(values = exposure_colors, 
                       labels = exposure_labels,
                       name = "Exposure group") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    theme_minimal(base_size = base_font_size) +
    theme(plot.title = element_text(hjust = 0.5, size = title_size, face = "bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "darkgrey", linewidth = 0.5),
          legend.position = "none") +
    scale_x_discrete(labels = exposure_labels) +
    labs(x = NULL, y = y_axis_label, title = plot_title)
  
  return(cum_plot)
}

# =============================================================================
# CREATE ALL PLOTS
# =============================================================================

# excess raw - daily
ed_enc_plot <- plot_estimates(results, "ED", "Total", "excess")
ed_cardio_plot <- plot_estimates(results, "ED", "Cardiovascular", "excess")
ed_injury_plot <- plot_estimates(results, "ED", "Injury", "excess")
ed_resp_plot <- plot_estimates(results, "ED", "Respiratory", "excess")
ed_neuro_plot <- plot_estimates(results, "ED", "Neuropsychiatric", "excess")

ip_enc_plot <- plot_estimates(results, "IP", "Total", "excess")
ip_cardio_plot <- plot_estimates(results, "IP", "Cardiovascular", "excess")
ip_injury_plot <- plot_estimates(results, "IP", "Injury", "excess")
ip_resp_plot <- plot_estimates(results, "IP", "Respiratory", "excess")
ip_neuro_plot <- plot_estimates(results, "IP", "Neuropsychiatric", "excess")

# excess raw - cumulative
ed_enc_cum <- plot_cumulative(cumulative_results, "ED", "Total", "excess")
ed_cardio_cum <- plot_cumulative(cumulative_results, "ED", "Cardiovascular", "excess")
ed_injury_cum <- plot_cumulative(cumulative_results, "ED", "Injury", "excess")
ed_resp_cum <- plot_cumulative(cumulative_results, "ED", "Respiratory", "excess")
ed_neuro_cum <- plot_cumulative(cumulative_results, "ED", "Neuropsychiatric", "excess")

ip_enc_cum <- plot_cumulative(cumulative_results, "IP", "Total", "excess")
ip_cardio_cum <- plot_cumulative(cumulative_results, "IP", "Cardiovascular", "excess")
ip_injury_cum <- plot_cumulative(cumulative_results, "IP", "Injury", "excess")
ip_resp_cum <- plot_cumulative(cumulative_results, "IP", "Respiratory", "excess")
ip_neuro_cum <- plot_cumulative(cumulative_results, "IP", "Neuropsychiatric", "excess")

# excess percent - daily
pct_ed_enc_plot <- plot_estimates(results, "ED", "Total", "excess_pct")
pct_ed_cardio_plot <- plot_estimates(results, "ED", "Cardiovascular", "excess_pct")
pct_ed_injury_plot <- plot_estimates(results, "ED", "Injury", "excess_pct")
pct_ed_resp_plot <- plot_estimates(results, "ED", "Respiratory", "excess_pct")
pct_ed_neuro_plot <- plot_estimates(results, "ED", "Neuropsychiatric", "excess_pct")

pct_ip_enc_plot <- plot_estimates(results, "IP", "Total", "excess_pct")
pct_ip_cardio_plot <- plot_estimates(results, "IP", "Cardiovascular", "excess_pct")
pct_ip_injury_plot <- plot_estimates(results, "IP", "Injury", "excess_pct")
pct_ip_resp_plot <- plot_estimates(results, "IP", "Respiratory", "excess_pct")
pct_ip_neuro_plot <- plot_estimates(results, "IP", "Neuropsychiatric", "excess_pct")

# excess percent - cumulative
pct_ed_enc_cum <- plot_cumulative(cumulative_results, "ED", "Total", "excess_pct")
pct_ed_cardio_cum <- plot_cumulative(cumulative_results, "ED", "Cardiovascular", "excess_pct")
pct_ed_injury_cum <- plot_cumulative(cumulative_results, "ED", "Injury", "excess_pct")
pct_ed_resp_cum <- plot_cumulative(cumulative_results, "ED", "Respiratory", "excess_pct")
pct_ed_neuro_cum <- plot_cumulative(cumulative_results, "ED", "Neuropsychiatric", "excess_pct")

pct_ip_enc_cum <- plot_cumulative(cumulative_results, "IP", "Total", "excess_pct")
pct_ip_cardio_cum <- plot_cumulative(cumulative_results, "IP", "Cardiovascular", "excess_pct")
pct_ip_injury_cum <- plot_cumulative(cumulative_results, "IP", "Injury", "excess_pct")
pct_ip_resp_cum <- plot_cumulative(cumulative_results, "IP", "Respiratory", "excess_pct")
pct_ip_neuro_cum <- plot_cumulative(cumulative_results, "IP", "Neuropsychiatric", "excess_pct")

# =============================================================================
# COMBINE WITH PATCHWORK
# =============================================================================

# calculate appropriate width based on number of days (plus extra for cumulative column)
n_days <- n_distinct(results$date)
plot_width <- max(24, n_days * 1.5 + 8)  # extra width for cumulative plots

# create filename suffix based on analysis type and whether no exposure is included
exposure_suffix <- if (include_no_exposure) "_with_none" else "_no_none"
analysis_suffix <- paste0("_", analysis_mode)

# build week-definitions caption for plots (only used in weekly mode with "week" labels)
week_caption <- ""
if (aggregate_to_weekly && identical(weekly_x_labels, "week") && exists("week_map")) {
  week_caption <- paste0(
    apply(week_map, 1, function(r) paste0(r[["week_label"]], " = ", r[["week_range"]])),
    collapse = "    "
  )
}

## excess raw ---------------------------
ed_excess <- (ed_enc_plot / ed_cardio_plot / ed_injury_plot / ed_neuro_plot / ed_resp_plot)
ed_cum <- (ed_enc_cum / ed_cardio_cum / ed_injury_cum / ed_neuro_cum / ed_resp_cum)
ip_excess <- (ip_enc_plot / ip_cardio_plot / ip_injury_plot / ip_neuro_plot / ip_resp_plot)
ip_cum <- (ip_enc_cum / ip_cardio_cum / ip_injury_cum / ip_neuro_cum / ip_resp_cum)

full_excess <- (ed_excess | ed_cum | ip_excess | ip_cum) +
  plot_layout(guides = "collect", widths = c(4, 1, 4, 1)) +
  plot_annotation(caption = week_caption) &
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5, size = 14))

pdf(paste0(output_dir, "full_excess_", num_days, "days", analysis_suffix, exposure_suffix, ".pdf"), 
    width = plot_width, height = 22)
print(full_excess)
dev.off()

## excess percent ---------------------------
ed_pct_excess <- (pct_ed_enc_plot / pct_ed_cardio_plot / pct_ed_injury_plot / pct_ed_neuro_plot / pct_ed_resp_plot)
ed_pct_cum <- (pct_ed_enc_cum / pct_ed_cardio_cum / pct_ed_injury_cum / pct_ed_neuro_cum / pct_ed_resp_cum)
ip_pct_excess <- (pct_ip_enc_plot / pct_ip_cardio_plot / pct_ip_injury_plot / pct_ip_neuro_plot / pct_ip_resp_plot)
ip_pct_cum <- (pct_ip_enc_cum / pct_ip_cardio_cum / pct_ip_injury_cum / pct_ip_neuro_cum / pct_ip_resp_cum)

full_pct_excess <- (ed_pct_excess | ed_pct_cum | ip_pct_excess | ip_pct_cum) +
  plot_layout(guides = "collect", widths = c(4, 1, 4, 1)) +
  plot_annotation(caption = week_caption) &
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5, size = 14))

pdf(paste0(output_dir, "full_pct_excess_", num_days, "days", analysis_suffix, exposure_suffix, ".pdf"), 
    width = plot_width, height = 22)
print(full_pct_excess)
dev.off()

message("Plots saved to: ", output_dir)
message("  - full_excess_", num_days, "days", analysis_suffix, exposure_suffix, ".pdf")
message("  - full_pct_excess_", num_days, "days", analysis_suffix, exposure_suffix, ".pdf")

# =============================================================================
# CUMULATIVE-ONLY PLOTS (for main manuscript)
# =============================================================================

if (cumulative_only) {
  
  # Create cumulative plots specifically for standalone layout (with ED/IP titles and outcome labels)
  # excess raw - cumulative only
  ed_enc_cum_solo <- plot_cumulative(cumulative_results, "ED", "Total", "excess", for_cumulative_only = TRUE)
  ed_cardio_cum_solo <- plot_cumulative(cumulative_results, "ED", "Cardiovascular", "excess", for_cumulative_only = TRUE)
  ed_injury_cum_solo <- plot_cumulative(cumulative_results, "ED", "Injury", "excess", for_cumulative_only = TRUE)
  ed_resp_cum_solo <- plot_cumulative(cumulative_results, "ED", "Respiratory", "excess", for_cumulative_only = TRUE)
  ed_neuro_cum_solo <- plot_cumulative(cumulative_results, "ED", "Neuropsychiatric", "excess", for_cumulative_only = TRUE)
  
  ip_enc_cum_solo <- plot_cumulative(cumulative_results, "IP", "Total", "excess", for_cumulative_only = TRUE)
  ip_cardio_cum_solo <- plot_cumulative(cumulative_results, "IP", "Cardiovascular", "excess", for_cumulative_only = TRUE)
  ip_injury_cum_solo <- plot_cumulative(cumulative_results, "IP", "Injury", "excess", for_cumulative_only = TRUE)
  ip_resp_cum_solo <- plot_cumulative(cumulative_results, "IP", "Respiratory", "excess", for_cumulative_only = TRUE)
  ip_neuro_cum_solo <- plot_cumulative(cumulative_results, "IP", "Neuropsychiatric", "excess", for_cumulative_only = TRUE)
  
  # excess percent - cumulative only
  pct_ed_enc_cum_solo <- plot_cumulative(cumulative_results, "ED", "Total", "excess_pct", for_cumulative_only = TRUE)
  pct_ed_cardio_cum_solo <- plot_cumulative(cumulative_results, "ED", "Cardiovascular", "excess_pct", for_cumulative_only = TRUE)
  pct_ed_injury_cum_solo <- plot_cumulative(cumulative_results, "ED", "Injury", "excess_pct", for_cumulative_only = TRUE)
  pct_ed_resp_cum_solo <- plot_cumulative(cumulative_results, "ED", "Respiratory", "excess_pct", for_cumulative_only = TRUE)
  pct_ed_neuro_cum_solo <- plot_cumulative(cumulative_results, "ED", "Neuropsychiatric", "excess_pct", for_cumulative_only = TRUE)
  
  pct_ip_enc_cum_solo <- plot_cumulative(cumulative_results, "IP", "Total", "excess_pct", for_cumulative_only = TRUE)
  pct_ip_cardio_cum_solo <- plot_cumulative(cumulative_results, "IP", "Cardiovascular", "excess_pct", for_cumulative_only = TRUE)
  pct_ip_injury_cum_solo <- plot_cumulative(cumulative_results, "IP", "Injury", "excess_pct", for_cumulative_only = TRUE)
  pct_ip_resp_cum_solo <- plot_cumulative(cumulative_results, "IP", "Respiratory", "excess_pct", for_cumulative_only = TRUE)
  pct_ip_neuro_cum_solo <- plot_cumulative(cumulative_results, "IP", "Neuropsychiatric", "excess_pct", for_cumulative_only = TRUE)
  
  ## excess raw - cumulative only assembly ---------------------------
  ed_cum_solo <- (ed_enc_cum_solo / ed_cardio_cum_solo / ed_injury_cum_solo / ed_neuro_cum_solo / ed_resp_cum_solo)
  ip_cum_solo <- (ip_enc_cum_solo / ip_cardio_cum_solo / ip_injury_cum_solo / ip_neuro_cum_solo / ip_resp_cum_solo)
  
  cumulative_excess <- (ed_cum_solo | ip_cum_solo) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5))
  
  pdf(paste0(output_dir, "cumulative_excess_", num_days, "days", analysis_suffix, exposure_suffix, ".pdf"), 
      width = 9, height = 16)
  print(cumulative_excess)
  dev.off()
  
  ## excess percent - cumulative only assembly ---------------------------
  ed_pct_cum_solo <- (pct_ed_enc_cum_solo / pct_ed_cardio_cum_solo / pct_ed_injury_cum_solo / pct_ed_neuro_cum_solo / pct_ed_resp_cum_solo)
  ip_pct_cum_solo <- (pct_ip_enc_cum_solo / pct_ip_cardio_cum_solo / pct_ip_injury_cum_solo / pct_ip_neuro_cum_solo / pct_ip_resp_cum_solo)
  
  cumulative_pct_excess <- (ed_pct_cum_solo | ip_pct_cum_solo) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5))
  
  pdf(paste0(output_dir, "cumulative_pct_excess_", num_days, "days", analysis_suffix, exposure_suffix, ".pdf"), 
      width = 9, height = 16)
  print(cumulative_pct_excess)
  dev.off()
  
  message("  - cumulative_excess_", num_days, "days", analysis_suffix, exposure_suffix, ".pdf")
  message("  - cumulative_pct_excess_", num_days, "days", analysis_suffix, exposure_suffix, ".pdf")
}

if (aggregate_to_weekly && identical(weekly_x_labels, "week") && exists("week_map")) {
  message("Weekly bins used (for reference):")
  apply(week_map, 1, function(r) message("  - ", r[["week_label"]], ": ", r[["week_range"]]))
}