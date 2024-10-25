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



