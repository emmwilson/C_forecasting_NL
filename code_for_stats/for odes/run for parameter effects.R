## ------------------------------------------------------------------------------------------------------------------------------------------
pacman::p_load(
  tidyverse,
  deSolve,
  parallel,
  pbapply,
  future.apply,
  progressr)


## ------------------------------------------------------------------------------------------------------------------------------------------
lv <- function(times, state, parms) {
  with(as.list(c(state, parms)), {
# moose
    Consumption    <- ((a * Pyoung)/(1 + a * h * Pyoung)) * Moose
    Death <- lM * (Moose^2)
    
    # palatable 
    GrowthPy <- rPy * Pyoung * (1 - (Pyoung + Pmature + alphaUPy * Unpal)/kPy)
    NewSeed <- s * Pmature
    DeathPy <- lPy * Pyoung
    GrowthPm <- rPm * Pmature * (1 - (Pyoung + Pmature + alphaUPm * Unpal)/kPm)
    Ageing <- g*(g^(3*(Consumption)/(Pyoung+0.01))) * Pyoung #need to add small amount to Pyoung in denominator
    DeathPm <- lPm * Pmature
    
    # unpalatable
    GrowthU <- rU * Unpal * (1 - (alphaPyU * Pyoung + alphaPmU * Pmature + Unpal)/kU)
    DeathU <- lU * Unpal

    # odes
    dMoose  <- Consumption*e - Death 
    dPyoung <- GrowthPy + NewSeed - Consumption - Ageing - DeathPy
    dPmature <- GrowthPm + Ageing - DeathPm
    dUnpal <- GrowthU - DeathU
    

    list(c(dMoose, dPyoung, dPmature, dUnpal))
  })
}


## ------------------------------------------------------------------------------------------------------------------------------------------
parms  <- c(a = 0.9985, 
            h = 0.0223,
            e = 0.02,
            lM = 0.3234,
            rPy = 0.2,
            rPm = 0.14,
            s = 0.011,
            alphaUPy = 0.001,
            alphaUPm = 0.0001, 
            kPy = 1000,
            kPm = 68000, 
            g = 0.133,
            lPm = 0.01,
            lPy = 0.2,
            rU = 0.9,
            alphaPyU = 0.002, 
            alphaPmU = 0.006, 
            kU = 900,
            lU = 0.4)

sampled_param_list_final <- readRDS(file = "~/C_forecasting_NL/code_for_stats/for odes/outputs/sampled_param_list_final.RDS")



## ------------------------------------------------------------------------------------------------------------------------------------------

y_0 <- c(Moose =2, Pyoung = 50, Pmature = 0, Unpal = 100)


## ------------------------------------------------------------------------------------------------------------------------------------------
eps <- 1e-2
## event triggered if state variable <= eps
eventfun <- function(times, y, parms) {
  y[which(y<eps | y<0)] <- 0 
  return(y)
}


## ------------------------------------------------------------------------------------------------------------------------------------------
time_vec1 <- seq(from = 1, to = 1000, length.out = 1000)


## ----message=FALSE, include=FALSE----------------------------------------------------------------------------------------------------------
out2 <- ode(y = y_0, times = time_vec1, func = lv, parms = parms, method = "lsode", events=list(func = eventfun, time = time_vec1), atol = 1e-21)



## ------------------------------------------------------------------------------------------------------------------------------------------
ode_reps <- function(parameters, initial_conditions) {
  out <- ode(y = initial_conditions, times = time_vec1, func = lv, parms = parameters, events=list(func = eventfun, time = time_vec1), method = "lsode", atol = 1e-21)
}


## ------------------------------------------------------------------------------------------------------------------------------------------
# to repeat ode over initial conditions and parameter values with one parameter set
ode_fvsg <- function(param_list_error, init_cond) {

  out_param2 <- lapply(init_cond, ode_reps, parameters = param_list_error)

  return(out_param2)
}


## ------------------------------------------------------------------------------------------------------------------------------------------
# to calculate percent recovered
percent_recover_param <- function(x) {
  df_list <- list()
  n_rec_list <- list()
  for(i in 1:ncol(x)){
  df_list[[i]] <- as.data.frame(do.call(cbind, x[,i]))
  n_rec_list[[i]] <- nrow(filter(df_list[[i]], g_or_f == " forest"))/nrow(df_list[[i]])
  }
  return(n_rec_list)
}


## ------------------------------------------------------------------------------------------------------------------------------------------
# make list of parameters

param_list_combos <- function(parameter){

  value_list <- as.list(signif(seq(from = param_dist[parameter,'min'], to = param_dist[parameter,'max'], by = (param_dist[parameter,'max'] - param_dist[parameter,'min'])/10),4))

  param_list <- rep(list(list()), length(value_list))
  
  for(i in 1:length(value_list)){
   param_list[[i]] <- sampled_param_list_final
    for(j in 1:length(sampled_param_list_final)){
     param_list[[i]][[j]][[parameter]] <- value_list[[i]]
    }
  }
  names(param_list) <- value_list
  return(param_list)
}

#a_param_list_combos <- param_list_combos('a')

# run over params and inti cond
param_init_runode <- function(parameter_list){
  
  out_param2 <- rep(list(list()), length(parameter_list))
  
  for(i in 1:length(parameter_list)) {
  plan(multisession, workers = 20)
  set.seed(123)
  
  #out_param2[[i]] <- future_mapply(ode_reps, parameters = param_list[[i]], initial_conditions = y_0d_par, future.seed = TRUE)
  out_param2[[i]] <- future_lapply(parameter_list[[i]], FUN = ode_fvsg, init_cond = y_0d_par, future.seed = TRUE)
  }
  names(out_param2) <- names(parameter_list)
  return(out_param2)
}

param_init_get_end <- function(out_param2, parameter_list){ 
  
  out_param_less2 <- rep(list(list()), length(parameter_list))
  g_vs_f_param2 <- rep(list(list()), length(parameter_list))
  
  for(i in 1:length(parameter_list)) {
    for(j in 1:length(out_param2[[i]])){
  names(out_param2[[i]][[j]]) <- seq(1, length(c(y_0d_par)), by = 1)
  out_param_less2[[i]][[j]] <- lapply(purrr::discard(out_param2[[i]][[j]], ~nrow(.) < 3),tail, 1, SIMPLIFY = T) 
  
  names(out_param_less2[[i]][[j]]) <- paste(names(out_param_less2[[i]][[j]]), ifelse(map(out_param_less2[[i]][[j]], pluck, 4) > 0, "forest", "grassland"))
  
  
  g_vs_f_param2[[i]][[j]] <- data.frame(id = parse_number(names(out_param_less2[[i]][[j]])), g_or_f = str_remove_all(names(out_param_less2[[i]][[j]]), "[:digit:]")) %>% 
    left_join(y_0d_par_id) %>% 
    mutate(Moose = unlist(map(y_0d_par, pluck, 1)), By = unlist(map(y_0d_par, pluck, 2))) %>% 
    select(!c(1,3:5))
}
}
  return(g_vs_f_param2)
}

#a_param_list_combos12 <- lapply(list(a_param_list_combos[[1]], a_param_list_combos[[2]]), function(x) x[c(3,4)])


#a_param_ode <- param_init_runode(a_param_list_combos12)



# get percent recovered
get_percent_param_rec <- function(param_ode_out, parameter_list) {
  
  percent_rec <- rep(list(list()), length(parameter_list))

 for(i in 1:length(parameter_list)) {
  
  percent_rec[[i]] <- future_lapply(param_ode_out[[i]], FUN = function(x) nrow(filter(x, g_or_f == " forest"))/nrow(x) , future.seed = TRUE)
  names(percent_rec) <- names(parameter_list)
  plan(sequential)
 }
  names(percent_rec) <- names(parameter_list)
  
  recover_df <- as.data.frame(do.call(rbind, percent_rec)) %>% 
   mutate(parameter = as.numeric(rownames(.))) %>% 
    mutate(across(contains("V"), ~ unlist(.))) %>% 
   pivot_longer(cols = !c("parameter"), names_to = "rep", values_to = "recover") %>% 
    mutate(recover = as.numeric(recover))
   
  return(recover_df)

}



## ------------------------------------------------------------------------------------------------------------------------------------------
plot_effect_param <- function(data) {
 ggplot(data, aes(x = parameter, y = recover, colour = recover)) +
  geom_violin() +
  geom_jitter(width = 0.05, height = 0, size = 1) +
  scale_fill_gradient2(high = "#4D8F26", low = "#f29414", mid = "#f3f59e", midpoint = 50) +
  geom_point(stat = 'summary', fun = 'mean', size = 2) 
}


## ------------------------------------------------------------------------------------------------------------------------------------------
return_year_stable_delta <- function(odes) {

  args_recover_delta <- rep(list(list(list())), length(odes))
  args_recover_deltayr <- rep(list(list(list())), length(odes))
  out_param_less2 <- rep(list(list(list())), length(odes))
  args_recover <- rep(list(list(list())), length(odes))
  odes2 <- odes
  
  for(i in 1:length(odes)){
    for(j in 1:length(c(sampled_param_list_final))){
  
      #need to filter out those that don't recover
      
  out_param_less2[[i]][[j]] <- lapply(purrr::discard(odes[[i]][[j]], ~nrow(.) < 3),tail, 1, SIMPLIFY = T) 
  
  names(out_param_less2[[i]][[j]]) <- paste(names(out_param_less2[[i]][[j]]), ifelse(map(out_param_less2[[i]][[j]], pluck, 4) > 0, "forest", "grassland"))
  
  args_recover[[i]][[j]] <- out_param_less2[[i]][[j]][ ! names(out_param_less2[[i]][[j]]) %>% str_detect("grassland") ]
  
  names(args_recover[[i]][[j]]) <- parse_number(names(args_recover[[i]][[j]]))
  
  odes2[[i]][[j]] <- odes[[i]][[j]][names(odes[[i]][[j]]) %in% names(args_recover[[i]][[j]])]
      
  args_recover_delta[[i]][[j]] <- lapply(odes2[[i]][[j]], function(x) as.data.frame(x) %>% 
    mutate(dMoose = Moose - lag(Moose), dPyoung = Pyoung - lag(Pyoung), dPmature = Pmature - lag(Pmature), dUnpal = Unpal - lag(Unpal)) %>% 
    filter(if_all(c(6:9), ~. <= 1e-8 | is.nan(.))))
  
  args_recover_deltayr[[i]][[j]]<- lapply(args_recover_delta[[i]][[j]],  function(x) x[1,1])
     }
    }
  return(args_recover_deltayr)
}




#using cv
return_year_recovered <- function(odes, p_combos_yr, param_combos) {
  # same prep as above
  
  args_recover_delta <- rep(list(list(list())), length(odes))
  args_recover_deltayr <- rep(list(list(list())), length(odes))
  out_param_less2 <- rep(list(list(list())), length(odes))
  args_recover <- rep(list(list(list())), length(odes))

  odes2 <- odes
  odes_stable_yrs <- rep(list(list(list())), length(odes2))
  odes_avg_mat <- rep(list(list(list())), length(odes2))
  odes_yr_mat<- rep(list(list(list())), length(odes2))
  
  for(i in 1:length(odes)){
    for(j in 1:length(sampled_param_list_final)){
  
      #need to filter out those that don't recover
      
  out_param_less2[[i]][[j]] <- lapply(purrr::discard(odes[[i]][[j]], ~nrow(.) < 3),tail, 1, SIMPLIFY = T) 
  
  names(out_param_less2[[i]][[j]]) <- paste(names(out_param_less2[[i]][[j]]), ifelse(map(out_param_less2[[i]][[j]], pluck, 4) > 0, "forest", "grassland"))
  
  args_recover[[i]][[j]] <- out_param_less2[[i]][[j]][ ! names(out_param_less2[[i]][[j]]) %>% str_detect("grassland") ]
  
  names(args_recover[[i]][[j]]) <- parse_number(names(args_recover[[i]][[j]]))
  
  odes2[[i]][[j]] <- odes[[i]][[j]][names(odes[[i]][[j]]) %in% names(args_recover[[i]][[j]])]
  names(odes2[[i]][[j]]) <- names(args_recover[[i]][[j]])
  
  # new filter
  
  
  odes_stable_yrs[[i]][[j]] <- lapply(names(odes2[[i]][[j]]), function(x) as.data.frame(odes2[[i]][[j]][[x]]) %>% 
    filter(time >= p_combos_yr[[i]][[j]][[x]]))
  
  
  names(odes_stable_yrs[[i]][[j]]) <- names(odes2[[i]][[j]])
  
  odes_avg_mat[[i]][[j]] <- lapply(names(odes2[[i]][[j]]), function(x) as.numeric(odes_stable_yrs[[i]][[j]][[x]][1,4]))
  
  names(odes_avg_mat[[i]][[j]]) <- names(odes2[[i]][[j]])
  
  # return year simulation initially hit that mature value
  
  
  odes_yr_mat[[i]][[j]] <- lapply(names(odes2[[i]][[j]]), function(x) as.data.frame(odes2[[i]][[j]][[x]]) %>% 
    filter(Pmature >= as.numeric(odes_avg_mat[[i]][[j]][[x]]) - as.numeric(odes_avg_mat[[i]][[j]][[x]])/1000))
  
  odes_yr_mat[[i]][[j]] <- lapply(odes_yr_mat[[i]][[j]], function(x) as.numeric(x[1,1]))
  
  names(odes_yr_mat[[i]][[j]]) <- names(odes2[[i]][[j]])
    }
  }
  names(odes_yr_mat) <- names(param_combos)
  return(odes_yr_mat)
}




# function to get 
# param_time_rec <- function(param_list_error) {
#   out_param2 <- lapply(y_0d_par, ode_reps, parameters = param_list_error)
#   
#   names(out_param2) <- seq(1, length(y_0d_par), by = 1)
#   out_param_less2 <- purrr::discard(out_param2, ~nrow(.) < 3)
#   
#   out_param_less_last2 <- lapply(out_param_less2,tail, 1, SIMPLIFY = T) 
#   
#   names(out_param_less2) <- paste(names(out_param_less_last2), ifelse(map(out_param_less_last2, pluck, 4) > 0, "forest", "grassland"))
#   
#   args_recover <- out_param_less2[ ! names(out_param_less2) %>% str_detect("grassland") ]
#   
#   args_recover_yr <- lapply(args_recover, return_year_stable_delta)
#   
#   args_recover_yravg <- mean(as.numeric(args_recover_yr))
#   
#   return(args_recover_yravg)
# }


## ------------------------------------------------------------------------------------------------------------------------------------------
# return_year_stable_param <- function(param) {
#   param_listat <- list(rep(list(),11))
#   
#   value_listat <- as.list(signif(seq(from = param_dist[param,'min'], to = param_dist[param,'max'], by = (param_dist[param,'max'] - param_dist[param,'min'])/10),4))
#   
#   for(i in 1:length(value_listat)){
#     param_listat[[i]] <- sampled_param_list_final[seq(1, length(sampled_param_list_final), 10)] # will not be seq for full simulation
#     for(j in 1:length(sampled_param_list_final[seq(1, length(sampled_param_list_final), 10)])){
#       param_listat[[i]][[j]][['a']] <- value_listat[[i]]
#     }
#   }
#   
#   gf_param_yr <- lapply(param_listat, sapply, param_time_rec) 
#   names(gf_param_yr) <- value_listat
#   
#   
#   gf_param_yrdf <- as.data.frame(do.call(rbind, gf_param_yr)) %>% 
#     mutate(parameter = rownames(.)) %>% 
#     pivot_longer(cols = !c("parameter"), names_to = "rep", values_to = "yr_recover")
# 
# }


## ------------------------------------------------------------------------------------------------------------------------------------------
plot_effect_param_t <- function(data) {
 ggplot(data, aes(x = parameter, y = yr_recover, colour = yr_recover)) +
  geom_violin() +
  geom_jitter(width = 0.05, height = 0, size = 1) +
  scale_fill_gradient(low = "#421650", high = "white", na.value = "#C4C4C4") +
  geom_point(stat = 'summary', fun = 'mean', size = 2) 
}



## ------------------------------------------------------------------------------------------------------------------------------------------
# make datafram with distribution of parameters

LVlow <-  c(0.996, #a
            0.0207933, # h
            0.01, #e
            0.2647, # lM
            0.15, #rPy
            0.1, #rPm
            0.0088, #s
            0.0005, #alphaUPy
            0.000, #alphaUPm
            400, #kPy
            20000, # kPm
            0.09, # g
            0.005, #lPm
            0.1, #lPy
            0.6,#rU
            0.001, # alphaPyU
            0.004, # alphaPmU
            300, #kU
            0.168) #lU

LVhigh <-  c(1.005, #a
            0.038735, # h
            0.03, #e
            0.3765, # lM
            0.325, #rPy
            0.2, #rPm
            0.053, #s
            0.002, #alphaUPy
            0.0002, #alphaUPm
            2000, #kPy
            80000, # kPm
            0.2, # g
            0.06, #lPm
            0.3, #lPy
            1.3 ,#rU
            0.003, # alphaPyU
            0.008, # alphaPmU
            1200, #kU
            1) #lU


param_dist <- data.frame(min = LVlow, max = LVhigh, mean = parms)

library(truncnorm)


## ------------------------------------------------------------------------------------------------------------------------------------------
#save to use for all repetitions
sampled_param_list_final <- readRDS(file = "~/C_forecasting_NL/code_for_stats/for odes/outputs/sampled_param_list_final.RDS")


## ------------------------------------------------------------------------------------------------------------------------------------------
y_0d_par <- readRDS(file = "~/C_forecasting_NL/code_for_stats/for odes/outputs/y_0d_par.RDS") 

y_0d_par_id <- as.data.frame(t(as.data.frame(y_0d_par))) %>% 
  mutate(id = seq(1, length(y_0d_par), by = 1))


## ------------------------------------------------------------------------------------------------------------------------------------------
# split into parts
param_combos <- lapply(names(parms), param_list_combos)
names(param_combos) <- names(parms)

for(i in 1:length(param_combos)){
  nam <- paste(names(param_combos)[i], "combos", sep = "_")
  assign(nam,param_combos[[i]])
}

