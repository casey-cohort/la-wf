 ``
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
               ggplot2, patchwork, gridExtra, ggtext)

# =============================================================================
# PARAMETERS
# =============================================================================
num_days <- 7                # number of days to plot (must match available aggregated file)
include_no_exposure <- FALSE  # TRUE for supplement, FALSE for main manuscript

# paths
rootdir_daily <- "/Users/laurenwilner/Library/CloudStorage/OneDrive-SharedLibraries-UW/casey_cohort - Documents/studies/la_wf_pm_evac_its/02_output/final_outputs/excess_hospitalizations/daily/" # folder containing the 40 xlsx files

rootdir_aggregated <- "/Users/laurenwilner/Library/CloudStorage/OneDrive-SharedLibraries-UW/casey_cohort - Documents/studies/la_wf_pm_evac_its/02_output/final_outputs/excess_hospitalizations/aggregated/" # folder containing the aggregated xlsx files

output_dir <- "/Users/laurenwilner/Library/CloudStorage/OneDrive-SharedLibraries-UW/casey_cohort - Documents/studies/la_wf_pm_evac_its/02_output/final_outputs/excess_hospitalizations/plots/" # where to save plots

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

# filter out "none" exposure if not including it
if (!include_no_exposure) {
  results <- results %>% filter(exposure != "none")
  cumulative_results <- cumulative_results %>% filter(exposure != "none")
  
  # update factor levels
  results$exposure <- droplevels(results$exposure)
  cumulative_results$exposure <- droplevels(cumulative_results$exposure)
}

# =============================================================================
# COLOR PALETTE AND LABELS
# =============================================================================

# define colors and labels based on whether we include no exposure
if (include_no_exposure) {
  exposure_colors <- c(
    "none" = "#a6cee3",      # light blue
    "mid_smoke" = "#f2c88f", # light orange  
    "high_smoke" = "#d37750", # darker orange
    "evac" = "#7f0000"       # dark red
  )
  exposure_labels <- c(
    "none" = "No exposure",
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
    ggplot(aes(x = date, 
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
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "darkgrey", linewidth = 0.5),
          legend.position = "none") +
    scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d",
                 expand = expansion(add = 0.5)) +
    labs(x = NULL, y = y_axis_label, title = plot_title)
  
  return(week_plot)
}

# cumulative plotting function ---------------------------
plot_cumulative <- function(data, visit, encounter, prefix) {
  
  # plot title - only for Total encounter
  if(encounter == "Total") {
    plot_title <- "Cumulative"
  } else {
    plot_title <- ""
  }
  
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
    theme_minimal(base_size = 22) +
    theme(plot.title = element_text(hjust = 0.5, size = 26, face = "bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "darkgrey", linewidth = 0.5),
          legend.position = "none") +
    scale_x_discrete(labels = exposure_labels) +
    labs(x = NULL, y = NULL, title = plot_title)
  
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

# create filename suffix based on whether no exposure is included
exposure_suffix <- if (include_no_exposure) "_with_none" else "_no_none"

## excess raw ---------------------------
ed_excess <- (ed_enc_plot / ed_cardio_plot / ed_injury_plot / ed_neuro_plot / ed_resp_plot)
ed_cum <- (ed_enc_cum / ed_cardio_cum / ed_injury_cum / ed_neuro_cum / ed_resp_cum)
ip_excess <- (ip_enc_plot / ip_cardio_plot / ip_injury_plot / ip_neuro_plot / ip_resp_plot)
ip_cum <- (ip_enc_cum / ip_cardio_cum / ip_injury_cum / ip_neuro_cum / ip_resp_cum)

full_excess <- (ed_excess | ed_cum | ip_excess | ip_cum) +
  plot_layout(guides = "collect", widths = c(4, 1, 4, 1)) & 
  theme(legend.position = "bottom")

png(paste0(output_dir, "full_excess_", num_days, "days", exposure_suffix, ".png"), 
    width = plot_width, height = 22, units = "in", res = 300)
print(full_excess)
dev.off()

## excess percent ---------------------------
ed_pct_excess <- (pct_ed_enc_plot / pct_ed_cardio_plot / pct_ed_injury_plot / pct_ed_neuro_plot / pct_ed_resp_plot)
ed_pct_cum <- (pct_ed_enc_cum / pct_ed_cardio_cum / pct_ed_injury_cum / pct_ed_neuro_cum / pct_ed_resp_cum)
ip_pct_excess <- (pct_ip_enc_plot / pct_ip_cardio_plot / pct_ip_injury_plot / pct_ip_neuro_plot / pct_ip_resp_plot)
ip_pct_cum <- (pct_ip_enc_cum / pct_ip_cardio_cum / pct_ip_injury_cum / pct_ip_neuro_cum / pct_ip_resp_cum)

full_pct_excess <- (ed_pct_excess | ed_pct_cum | ip_pct_excess | ip_pct_cum) +
  plot_layout(guides = "collect", widths = c(4, 1, 4, 1)) & 
  theme(legend.position = "bottom")

png(paste0(output_dir, "full_pct_excess_", num_days, "days", exposure_suffix, ".png"), 
    width = plot_width, height = 22, units = "in", res = 300)
print(full_pct_excess)
dev.off()

message("Plots saved to: ", output_dir)
message("  - full_excess_", num_days, "days", exposure_suffix, ".png")
message("  - full_pct_excess_", num_days, "days", exposure_suffix, ".png")