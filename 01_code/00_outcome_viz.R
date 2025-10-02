#-------------------------------
# LA ITS prep
# author: Lara Schwarz, adapted by Lauren Wilner
# date: 2025-09-02
# this code preps all data for the interrupted time series analysis

#-------------------------------
# setup
rm(list=ls())
if (!requireNamespace('pacman', quietly = TRUE)) {install.packages('pacman')}
pacman::p_load(tidyverse, readr, tidyr, purrr, lubridate, MMWRweek, here, arrow, gridExtra)

# set paths
source(paste0(getwd(), "/01_code/paths.R"))

#-------------------------------
# load data
df_temp <- read_csv(paste0(path_onedrive, "01_data/01_raw/ed_ipt_dat/2025-08-08/ENC_EXP_DAILY_08082025.csv")) %>% 
   # clean names so there are no spaces
   mutate(exposure_category = str_replace_all(exposure_category, ",.*", ""),
         exposure_category = str_replace_all(exposure_category, " ", "_"),
         exposure_category = ifelse(exposure_category == "no_smoke", "none", exposure_category),
         encounter_dt = as.Date(encounter_dt, format = "%m/%d/%Y"),
         month_year = floor_date(encounter_dt, "month"),
         month = month(encounter_dt, label = TRUE),
         year = year(encounter_dt),
         season = case_when(
            year == 2022 & month %in% c("Nov", "Dec") ~ "2022-2023",
            year == 2023 & month == "Jan" ~ "2022-2023",
            year == 2023 & month %in% c("Nov", "Dec") ~ "2023-2024", 
            year == 2024 & month == "Jan" ~ "2023-2024",
            year == 2024 & month %in% c("Nov", "Dec") ~ "2024-2025",
            year == 2025 & month == "Jan" ~ "2024-2025"
        )
  ) %>% filter(!is.na(season) & month %in% c("Nov", "Dec", "Jan")) %>%
  mutate(month = droplevels(month)) # filter to only Nov-Jan seasons

# reshape to long on the causes 
plot_data <- df_temp %>% 
    pivot_longer(cols = c("num_enc", "num_enc_cardio", "num_enc_resp", "num_enc_neuro", "num_enc_injury"), 
                 names_to = "cause", 
                 values_to = "num_encounters") %>%
      filter(encounter_dt >= as.Date("2022-11-01") & encounter_dt <= as.Date("2025-01-31") & 
         month %in% c("Nov", "Dec", "Jan"))


# plots
plot_list_all <- list()
plot_list_evac <- list()

# jan 7 dates for reference line
jan7_dates <- data.frame(
  season = c("2022-2023", "2023-2024", "2024-2025"),
  jan7_date = as.Date(c("2023-01-07", "2024-01-07", "2025-01-07"))
)

for(e in unique(plot_data$enc_type)){
# Plot without evac
    p_all <- ggplot(plot_data %>% filter(enc_type == e), 
                        aes(x = encounter_dt, y = num_encounters, color = exposure_category)) +
      geom_line(size = 0.8) +
      geom_vline(data = jan7_dates, aes(xintercept = jan7_date), 
            linetype = "dotted", color = "black", size = 0.8) +
    #   geom_text(data = jan7_dates, aes(x = jan7_date, y = Inf, label = "Jan 7"), 
    #         vjust = 1.2, hjust = -0.1, size = 3, color = "black", inherit.aes = FALSE) +
      facet_grid(cause ~ season, scales = "free") +
      scale_color_manual(
        name = "Exposure Category",
        values = c("evac" = "#0ea5e9",
                   "high_smoke" = "#D95F02",
                   "mid_smoke" = "#1B9E77", 
                   "none" = "#7570B3"),
        labels = c("high_smoke" = "high smoke, no evac", 
                   "mid_smoke" = "mid smoke, no evac",
                   "none" = "no smoke, no evac")
      ) +
      labs(
        title = paste("KPSC time series -", e),
        x = "Date",
        y = "Number of Encounters"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
    
    # Plot only evac
    p_evac <- ggplot(plot_data %>% filter(enc_type == e, exposure_category == "evac"), 
                     aes(x = encounter_dt, y = num_encounters)) +
      geom_line(size = 0.8, color = "#0ea5e9") +
      geom_vline(data = jan7_dates, aes(xintercept = jan7_date), 
            linetype = "dotted", color = "black", size = 0.8) +
      geom_text(data = jan7_dates, aes(x = jan7_date, y = Inf, label = "Jan 7"), 
            vjust = 1.2, hjust = -0.1, size = 3, color = "black", inherit.aes = FALSE) +
      facet_grid(cause ~ season, scales = "free") +
      labs(
        title = paste("KPSC time series -", e, "- evac only"),
        x = "Date",
        y = "Number of Encounters"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
    
    plot_list_all <- append(plot_list_all, list(p_all))
    plot_list_evac <- append(plot_list_evac, list(p_evac))
    
}

ggsave(paste0(path_onedrive, "outcome_diagnostic.pdf"),
       marrangeGrob(plot_list_all, nrow=1, ncol=1, top=NULL),
       width = 25, height = 10, limitsize = FALSE)

ggsave(paste0(path_onedrive, "outcome_diagnostic_evac_only.pdf"),
       marrangeGrob(plot_list_evac, nrow=1, ncol=1, top=NULL),
       width = 25, height = 10, limitsize = FALSE)