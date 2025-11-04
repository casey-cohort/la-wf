# scatter of each chosen paramter vs rsq 

library(pacman)
p_load(tidyverse, gridExtra)

# read in the performance metrics csv
source(paste0(getwd(), "/01_code/paths.R"))
model_run <- "2025-11-03.v009_x40_sim50"

metrics <- read.csv(paste0(path_onedrive, "02_output/model_run_", model_run, "/performance_metrics_", model_run, ".csv"))
# subset out major outliers 
metrics <- metrics %>%
    filter(r2 > -5)

# plot each chosen parameter vs rsq
# add a caption with the range that was given for that parameter
params <- c("mtry", "min_n", "tree_depth", "learn_rate", "loss_reduction", "stop_iter")
plot_list <- list()
for (param in params) {
    param_range_var <- paste0(param, "_range")
    print(paste0(param, ", ", param_range_var))
    p <- ggplot(metrics, aes(x = !!sym(param), y = r2)) +
        geom_point() + geom_smooth(method = "lm") +
        labs(x = param, 
        y = "R-squared", 
        caption = paste0("Range: ", unique(metrics[[param_range_var]]))) +
        theme_minimal()
    plot_list[[param]] <- p
}  
plot_list

# print the plots in a grid of 2x3
grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], plot_list[[6]], ncol = 2, nrow = 3)
