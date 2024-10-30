n_fs_ratio <- readRDS(file = "~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/n_fs_ratio.RDS") 

n_fs_ratiof2 <- n_fs_ratio %>% 
  filter(percent_recover < 100 & percent_recover > 0)

lm(percent_recover ~ Moose + By, n_fs_ratiof2)
lm(percent_recover ~ Moose + By, n_fs_ratio)

library(car)

n_fs_ratiof2_lm <- n_fs_ratiof2 %>% group_by(percent_recover) %>% do(model = lm(By ~ Moose, data = .))

avg_coef_M_By <- mean(do.call(rbind, lapply(n_fs_ratiof2_lm$model, function(x) x$coefficients[2])), na.rm = T)

# do similar for time to recover
avg_yr_50_f_ratio <- readRDS(file = "~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/avg_yr_50_f_ratio.RDS") 

avg_yr_ratiof2_lm <- avg_yr_50_f_ratio %>% group_by(ratio) %>% do(model = lm(By ~ Moose, data = .))
avg_yr_coef_M_By <- mean(do.call(rbind, lapply(avg_yr_ratiof2_lm$model, function(x) x$coefficients[2])), na.rm = T)

lm(mean_yr ~ Moose + By, subset(avg_yr_50_f_ratio, percent_recover == 100))


avg_yr_ratiof100_lm <- subset(avg_yr_50_f_ratio, percent_recover == 100) %>% group_by(ratio) %>% do(model = lm(By ~ Moose, data = .))
avg_yr_coef_M100_By <- mean(do.call(rbind, lapply(avg_yr_ratiof100_lm$model, function(x) x$coefficients[2])), na.rm = T)
1/avg_yr_coef_M100_By
