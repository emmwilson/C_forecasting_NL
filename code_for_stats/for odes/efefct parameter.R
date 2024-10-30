library(ggplot2)
library(tidyverse)
library(grid)

rPy_combos_rec <- readRDS(file = "/Users/emmersonEmmerson/Downloads/rPy_combos_rec.RDS")

rPy_combos_recdf <- as.data.frame(rPy_combos_rec) %>%
  group_by(parameter)

rPy_combos_recmean <- rPy_combos_recdf %>%
  group_by(parameter) %>%
  summarise(mean = mean(recover))

rPy_plot <- ggplot() +
  geom_violin(data = rPy_combos_recdf, aes(factor(parameter), recover)) +
  geom_point(data = rPy_combos_recmean, aes(factor(parameter), mean, colour = mean)) +
  theme_classic() +
  scale_x_discrete(expand = c(0.1,0), breaks = levels(factor(rPy_combos_recmean$parameter))[c(T, rep(F, 1))])+
  labs(x = 'rPy (units)', y = 'Percent simulations recover') +
  # rectangle with gradient
  theme(legend.position = "none") +
  scale_colour_gradientn(colours = c("#4D8F26", "#f3f59e", "#f29414"), values = c(1, 0.5, 0), limits = c(0, 1)) +
  coord_cartesian(clip = "off") +
  annotation_custom(grid::rectGrob(x = unit(0.008, "npc"), y = unit(0.5, "npc"),
                                   width = unit(2, "mm"), height = unit(79, "mm"), gp = gpar(col = NA, fill = grid::linearGradient(colour = c("#f29414", "#f3f59e", "#4D8F26"), stops = c(0, 0.5, 1)), lwd = unit(0.1, "pt")))) 

rPy_plot

#saveRDS(rPy_plot, file = "~/C_forecasting_NL/code_for_stats/for odes/outputs/rPy_plot.RDS") 

jpeg("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/rPy_plot.jpg", height = 10, width = 12, units = "cm", res = 600)
rPy_plot
dev.off()

library(scales)
# into function
plot_param_rec <- function(param, units) {
  p_combos_rec <- readRDS(file = paste0("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/", param, "_combos_rec.RDS"))
  
  p_combos_recdf <- as.data.frame(p_combos_rec) %>%
    mutate(parameter = round(parameter, 3)) %>% 
    group_by(parameter)
  
  p_combos_recmean <- as.data.frame(p_combos_rec) %>%
    mutate(parameter = round(parameter, 3)) %>% 
    group_by(parameter) %>%
    summarise(mean = mean(recover))

  
  p_plot <- ggplot() +
    geom_violin(data = p_combos_recdf, aes(factor(parameter), recover), size = 0.5) +
    geom_point(data = p_combos_recmean, aes(factor(parameter), mean, colour = mean)) +
    theme_classic() +
    scale_x_discrete(expand = c(0.1,0), breaks = levels(factor(p_combos_recmean$parameter))[c(T, rep(F, 1))])+
    ylim(c(0,1)) +
    labs(x = paste0(param, units), y = NULL) +
    # rectangle with gradient
    theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_colour_gradientn(colours = c("#4D8F26", "#f3f59e", "#f29414"), values = c(1, 0.5, 0), limits = c(0, 1)) +
    coord_cartesian(clip = "off") +
    annotation_custom(grid::rectGrob(x = unit(0.008, "npc"), y = unit(0.5, "npc"),
                                     width = unit(1, "mm"), height = unit(25, "mm"), gp = gpar(col = NA, fill = grid::linearGradient(colour = c("#f29414", "#f3f59e", "#4D8F26"), stops = c(0, 0.5, 1)), lwd = unit(0.1, "pt"))))
  
  jpeg(paste0("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/", param, "_plot.jpg"), height = 4.5, width = 5, units = "cm", res = 600)
  ifelse(p_combos_recmean$mean[[1]] != p_combos_recmean$mean[[11]], print(p_plot), print("NA"))
  dev.off()
  
  ifelse(p_combos_recmean$mean[[1]] != p_combos_recmean$mean[[11]], return(p_plot), return("NA")) # could do change must be certain magnitude or just if theres change at all (sometimes none) - could print all with any amount of change and only show the ones with the most visible

}

plot_param_rec_noy <- function(param, units) {
  p_combos_rec <- readRDS(file = paste0("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/", param, "_combos_rec.RDS"))
  
  p_combos_recdf <- as.data.frame(p_combos_rec) %>%
    mutate(parameter = round(parameter, 3)) %>% 
    group_by(parameter)
  
  p_combos_recmean <- as.data.frame(p_combos_rec) %>%
    mutate(parameter = round(parameter, 3)) %>% 
    group_by(parameter) %>%
    summarise(mean = mean(recover))
  
  p_plot <- ggplot() +
    geom_violin(data = p_combos_recdf, aes(factor(parameter), recover), size = 0.5) +
    geom_point(data = p_combos_recmean, aes(factor(parameter), mean, colour = mean)) +
    theme_classic() +
    scale_x_discrete(expand = c(0.1,0), breaks = levels(factor(p_combos_recmean$parameter))[c(T, rep(F, 1))])+
    ylim(c(0,1)) +
    labs(x = paste0(param, units), y = NULL) +
    # rectangle with gradient
    theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.text.y = element_blank()) +
     scale_colour_gradientn(colours = c("#4D8F26", "#f3f59e", "#f29414"), values = c(1, 0.5, 0), limits = c(0, 1)) +
    # coord_cartesian(clip = "off") +
    # annotation_custom(grid::rectGrob(x = unit(0.008, "npc"), y = unit(0.5, "npc"),
    #                                  width = unit(1, "mm"), height = unit(25, "mm"), gp = gpar(col = NA, fill = grid::linearGradient(colour = c("#f29414", "#f3f59e", "#4D8F26"), stops = c(0, 0.5, 1)), lwd = unit(0.1, "pt"))))
   
  jpeg(paste0("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/", param, "_plot.jpg"), height = 4.5, width = 4.25, units = "cm", res = 600)
  ifelse(p_combos_recmean$mean[[1]] != p_combos_recmean$mean[[11]], print(p_plot), print("NA"))
  dev.off()
  
  ifelse(p_combos_recmean$mean[[1]] != p_combos_recmean$mean[[11]], return(p_plot), return("NA")) # could do change must be certain magnitude or just if theres change at all (sometimes none) - could print all with any amount of change and only show the ones with the most visible
  
}

plot_param_rec('rPy', 'units')
plot_param_rec('e', 'units')
plot_param_rec_noy('rU', 'units')
plot_param_rec_noy('lPm', 'units') # something went wrong with lPm, seems it is missing some simulations (only has 1034 vs 1265)
plot_param_rec_noy('lU', 'units')
plot_param_rec_noy('lM', 'units') # also missing simulations
plot_param_rec_noy('kU', 'units') # also missing simulations



lPm_combos_rec <- readRDS(file = paste0("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/", 'lPm', "_combos_rec.RDS"))

lM_combos_rec <- readRDS(file = paste0("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/", 'lM', "_combos_rec.RDS"))

kU_combos_rec <- readRDS(file = paste0("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/", 'kU', "_combos_rec.RDS"))

lU_combos_rec <- readRDS(file = paste0("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/", 'lU', "_combos_rec.RDS"))

rU_combos_rec <- readRDS(file = paste0("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/", 'rU', "_combos_rec.RDS"))
e_combos_rec <- readRDS(file = paste0("~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/", 'e', "_combos_rec.RDS"))
