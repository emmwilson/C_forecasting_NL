# recovery time based on parameters
rep_n <- 10

param_listat <- list(rep(list(),rep_n))

value_listat <- as.list(signif(seq(from = param_dist['e','min'], to = param_dist['e','max'], by = (param_dist['e','max'] - param_dist['e','min'])/rep_n),4))

for(i in 1:length(value_listat)){
  param_listat[[i]] <- sampled_param_list_final[seq(1, length(sampled_param_list_final), 5)]
  for(j in 1:length(sampled_param_list_final[seq(1, length(sampled_param_list_final), 5)])){
    param_listat[[i]][[j]][['e']] <- value_listat[[i]]
  }
}

param_time_rec <- function(param_list_error) {
  out_param2 <- lapply(y_0dz2, ode_reps, parameters = param_list_error)
  
  names(out_param2) <- seq(1, length(y_0dz2), by = 1)
  out_param_less2 <- purrr::discard(out_param2, ~nrow(.) < 3)
  
  out_param_less_last2 <- lapply(out_param_less2,tail, 1, SIMPLIFY = T) 
  
  names(out_param_less2) <- paste(names(out_param_less_last2), ifelse(map(out_param_less_last2, pluck, 4) > 0, "forest", "grassland"))
  
  args_recover <- out_param_less2[ ! names(out_param_less2) %>% str_detect("grassland") ]
  
  args_recover_yr <- lapply(args_recover, return_year_stable)
  
  args_recover_yravg <- mean(as.numeric(args_recover_yr))
  
  return(args_recover_yravg)
}

gf_param_yr <- lapply(param_listat, sapply, param_time_rec) 
names(gf_param_yr) <- value_listat


gf_param_yrdf <- as.data.frame(do.call(rbind, gf_param_yr)) %>% 
  mutate(parameter = rownames(.)) %>% 
  pivot_longer(cols = !c("parameter"), names_to = "rep", values_to = "yr_recover")

param_plot <- ggplot(gf_param_yrdf, aes(x = parameter, y = yr_recover)) +
  geom_jitter(width = 0.05, height = 0.005)+
  geom_line() +
  theme_bw()

library(ggridges)
theme_set(theme_minimal())

library(plyr)

gf_param_yrdfr <- gf_param_yrdf %>% 
  mutate(yr_recr = round_any(yr_recover,1)) %>% 
  mutate(par = parameter)

ggplot(gf_param_yrdfr, aes(x = yr_recr, y = par, fill = stat(x))) + 
  geom_density_ridges_gradient(rel_min_height = 0.01, scale = 0.7,jittered_points = TRUE,
                               position = position_points_jitter(width = 0.0, height = 0.07),
                               point_size = 2, point_alpha = 0.5, alpha = 0.7) +
  scale_fill_viridis(direction = -1)

ggplot(gf_param_yrdfr, aes(x = yr_recr, y = par)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5) 

# atm best graph
ggplot(gf_param_yrdfr, aes(x = par, y = yr_recr, colour = yr_recr)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0) +
  scale_color_viridis(direction = -1)

#what if dont round?
gf_param_yrdf <- gf_param_yrdf %>% 
  group_by(parameter)

# add mean in larger point size, increase transparency of other points
ggplot(gf_param_yrdf, aes(x = parameter, y = yr_recover, colour = yr_recover)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0, size = 1) +
  scale_color_viridis(direction = -1) +
  geom_point(stat = 'summary', fun = 'mean', size = 2) # interesting that for some parameter combos the time to recover increases with e but for others it decrease - speaks to the complexity of interactions between parameters - overall as e increase it converges towards a point

# think below is too complicated
# instead of averaging return all three numbers
param_time_rec2 <- function(param_list_error, y_d) {
  out_param2 <- mapply(param_list_error, y_d ode_reps)
  
  names(out_param2) <- seq(1, length(y_0dz2), by = 1)
  out_param_less2 <- purrr::discard(out_param2, ~nrow(.) < 3)
  
  out_param_less_last2 <- lapply(out_param_less2,tail, 1, SIMPLIFY = T) 
  
  names(out_param_less2) <- paste(names(out_param_less_last2), ifelse(map(out_param_less_last2, pluck, 4) > 0, "forest", "grassland"))
  
  args_recover <- out_param_less2[ ! names(out_param_less2) %>% str_detect("grassland") ]
  
  args_recover_yr <- lapply(args_recover, return_year_stable)
  args_recover_yr_df <- as.data.frame(do.call(rbind, args_recover_yr))
  
  
  return(args_recover_yr_df)
}

y_0dz2 <- initial_conditions[seq(1, length(initial_conditions), 5000)]

gf_param_yr2 <- lapply(param_listat, sapply, param_time_rec) 
names(gf_param_yr2) <- value_listat

gf_param_df <- lapply(gf_param_yr2, function(x) do.call(cbind, x))

lapply(gf_param_df, function (x) data.frame(mean = apply(x, mean), max = apply(x, max), min = apply(x, min)))
# mean will be what the density is made from? the instead of points it was a line from min to max



gf_param_yrdf2 <- as.data.frame(do.call(rbind, gf_param_yr2)) %>% 
  mutate(parameter = rownames(.)) %>% 
  pivot_longer(cols = !c("parameter"), names_to = "rep", values_to = "yr_recover")



param_plot2 <- ggplot(gf_param_yrdf2, aes(x = parameter, y = yr_recover)) +
  geom_jitter(width = 0.05, height = 0.005)+
  geom_line() +
  theme_bw()

library(ggridges)
theme_set(theme_minimal())

library(plyr)

gf_param_yrdfr2 <- gf_param_yrdf2 %>% 
  mutate(yr_recr = round_any(yr_recover,1)) %>% 
  mutate(par = parameter)

ggplot(gf_param_yrdfr2, aes(x = yr_recr, y = par, fill = stat(x))) + 
  geom_density_ridges_gradient(rel_min_height = 0.01, scale = 0.7,jittered_points = TRUE,
                               position = position_points_jitter(width = 0.0, height = 0.07),
                               point_size = 2, point_alpha = 0.5, alpha = 0.7) +
  scale_fill_viridis(direction = -1)


ggplot(gf_param_yrdf2, aes(x = yr_recover, y = parameter)) + 
  geom_violin()





# whole thing into function

return_year_stable_param <- function(param) {
  param_listat <- list(rep(list(),11))
  
  value_listat <- as.list(signif(seq(from = param_dist[param,'min'], to = param_dist[param,'max'], by = (param_dist[param,'max'] - param_dist[param,'min'])/10),4))
  
  for(i in 1:length(value_listat)){
    param_listat[[i]] <- sampled_param_list_final[seq(1, length(sampled_param_list_final), 20)]
    for(j in 1:length(sampled_param_list_final[seq(1, length(sampled_param_list_final), 20)])){
      param_listat[[i]][[j]][['a']] <- value_listat[[i]]
    }
  }
  
  gf_param_yr <- lapply(param_listat, sapply, param_time_rec) 
  names(gf_param_yr) <- value_listat
  
  
  gf_param_yrdf <- as.data.frame(do.call(rbind, gf_param_yr)) %>% 
    mutate(parameter = rownames(.)) %>% 
    pivot_longer(cols = !c("parameter"), names_to = "rep", values_to = "yr_recover")
  
  param_plot <- ggplot(gf_param_yrdf, aes(x = parameter, y = yr_recover)) +
    geom_jitter(width = 0.05, height = 0.005)+
    geom_line() +
    theme_bw()
}


