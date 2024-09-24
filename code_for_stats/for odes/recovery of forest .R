# add variation around points based on random selection of other parameters

param_list_rpy2 <- list(rep(list(),11))

for(i in 1:length(rPy_list)){
  param_list_rpy2[[i]] <- sampled_param_list_final[seq(1, length(sampled_param_list_final), 10)]
  for(j in 1:length(sampled_param_list_final[seq(1, length(sampled_param_list_final), 10)])){
  param_list_rpy2[[i]][[j]][['rPy']] <- rPy_list[[i]]
  }
}



y_0dz2 <- initial_conditions[seq(1, length(initial_conditions), 2000)]

run_ode_get_fvsg_error <- function(param_list_error) {
  out_param2 <- lapply(y_0dz2, ode_reps, parameters = param_list_error)
  
  names(out_param2) <- seq(1, length(y_0dz2), by = 1)
  out_param_less2 <- purrr::discard(out_param2, ~nrow(.) < 3)
  out_param_less_last2 <- lapply(out_param_less2,tail, 1, SIMPLIFY = T) 
  
  names(out_param_less_last2) <- paste(names(out_param_less_last2), ifelse(map(out_param_less_last2, pluck, 4) > 0, "forest", "grassland"))
  
  
  g_vs_f_param2 <- data.frame(id = parse_number(names(out_param_less_last2)), g_or_f = str_remove_all(names(out_param_less_last2), "[:digit:]")) %>% 
    left_join(y_0d_id) %>% 
    mutate(Moose = unlist(map(y_0dz2, pluck, 1)), By = unlist(map(y_0dz2, pluck, 2))) %>% 
    select(!c(1,4,5,6))
  
  
  return(g_vs_f_param2)
}

g_vs_f_param_rPy1_err <- lapply(param_list_rpy2, sapply, run_ode_get_fvsg_error)

percent_recover_param <- function(x) {
  df_list <- list()
  n_rec_list <- list()
  for(i in 1:ncol(x)){
  df_list[[i]] <- as.data.frame(do.call(cbind, x[,i]))
  n_rec_list[[i]] <- nrow(filter(df_list[[i]], g_or_f == " forest"))/nrow(df_list[[i]])
  }
  return(n_rec_list)
}


g_vs_f_param_rPy_recover <- lapply(g_vs_f_param_rPy1_err, percent_recover_param)
names(g_vs_f_param_rPy_recover) <- rPy_list

options(digits = 15)

rPy_recover_df <- as.data.frame(do.call(rbind, g_vs_f_param_rPy_recover)) %>% 
  mutate(parameter = rownames(.)) %>% 
  mutate(across(contains("V"), ~ unlist(.))) %>% 
  pivot_longer(cols = !c("parameter"), names_to = "rep", values_to = "recover")


ggplot(rPy_recover_df, aes(x = parameter, y = recover)) +
  geom_jitter(width = 0.05, height = 0.005)+
  geom_line() +
  theme_bw()


# make whole thing into function?
param_recovery <- function(parameter){

  param_list <- list(rep(list(),11))

  value_list <- as.list(signif(seq(from = param_dist[parameter,'min'], to = param_dist[parameter,'max'], by = (param_dist[parameter,'max'] - param_dist[parameter,'min'])/10),4))

  for(i in 1:length(value_list)){
   param_list[[i]] <- sampled_param_list_final[seq(1, length(sampled_param_list_final), 10)]
    for(j in 1:length(sampled_param_list_final[seq(1, length(sampled_param_list_final), 10)])){
     param_list[[i]][[j]][[parameter]] <- value_list[[i]]
    }
  }

  g_vs_f <- lapply(param_list, sapply, ode_fvsg)

  g_vs_f_recover <- lapply(g_vs_f, percent_recover_param)
  names(g_vs_f_recover) <- value_list

  recover_df <- as.data.frame(do.call(rbind, g_vs_f_recover)) %>% 
   mutate(parameter = rownames(.)) %>% 
    mutate(across(contains("V"), ~ unlist(.))) %>% 
   pivot_longer(cols = !c("parameter"), names_to = "rep", values_to = "recover")

  param_plot <- ggplot(recover_df, aes(x = parameter, y = recover)) +
   geom_jitter(width = 0.05, height = 0.005)+
   geom_line() +
   theme_bw()

  return(param_plot)
}

param_recovery('a')



# try subset heatmap to test for negatives
out_end2 <- list()                     
out2 <- list()        

# make all combinations availabel in environment
n_i2 <- 13

M_init2 <- seq(2, 6, length.out = n_i2)
Py_init2 <- seq(0, 200, length.out = n_i2) 
Pm_init2 <- rep(0, n_i2)
U_init2 <- rep(100, n_i2)

init_seq2 <- data.frame(Moose=rep(M_init2, each = n_i2), Pyoung = rep(Py_init2, n_i2), Pmature = rep(Pm_init2,n_i2), Unpal = rep(U_init2,n_i2))

y_0d2 <- split(as.matrix(init_seq2), row(init_seq2)) %>% 
  lapply(setNames, nm = c("Moose", "Pyoung", "Pmature", "Unpal"))

y_0d_id2 <- as.data.frame(t(as.data.frame(y_0d2))) %>% 
  mutate(id2 = seq(1, length(y_0d2), by = 1))

args2 <- expand.grid(
  parameters2=sampled_param_list_final[seq(1, length(sampled_param_list_final), 2)],
  initial_conditions2=y_0d2) 
args_id2 <- args2 %>% mutate(id2 = seq(1, nrow(args2), by = 1))
list2env(args2, envir=.GlobalEnv)

# run over params and init conditions
args2$output <- mapply(ode_reps, parameters2, initial_conditions2, SIMPLIFY = F)

# give simulations names based params and init conditions
names(args2$output) <- seq(1, nrow(args_id2), by = 1)

# remove any simulations that didnt run properly
args_less2 <- purrr::discard(args2$output, ~nrow(.) < 3)
arg_removed2 <- purrr::keep(args2$output, ~nrow(.) < 3)


arg_less_last2 <- lapply(args_less2,tail, 1, SIMPLIFY = T)
names(arg_less_last2) <- paste(names(arg_less_last2), ifelse(map(arg_less_last2, pluck, 4) > 0, "forest", "grassland"))

# add info on initial conditions
g_vs_f2 <- data.frame(id2 = parse_number(names(arg_less_last2)), g_or_f = str_remove_all(names(arg_less_last2), "[:digit:]")) %>% 
  left_join(args_id2) %>% 
  mutate(Moose = unlist(map(initial_conditions2, pluck, 1)), By = unlist(map(initial_conditions2, pluck, 2))) %>% 
  select(!c(1,3,4))

# count number of recoveries
n_g_vs_f2 <- g_vs_f2 %>% 
  group_by(as.factor(Moose), as.factor(By), as.factor(g_or_f), .drop=FALSE) %>% 
  count(.drop=FALSE) %>% 
  ungroup() %>% 
  mutate(Moose = as.numeric(as.character(`as.factor(Moose)`))) %>% 
  mutate(By = as.numeric(as.character(`as.factor(By)`))) %>% 
  rename(g_or_f = `as.factor(g_or_f)`) %>% 
  select(c(3:6))

# into %
n_f2 <- subset(n_g_vs_f2, g_or_f == " forest") %>% 
  mutate(percent_recover = (n/length(sampled_param_list_final[seq(1, length(sampled_param_list_final), 2)]))*100)

# mark 50/50

library(viridis)

n_f22 <- n_f2 %>% 
  filter(percent_recover >= 50) %>% 
  group_by(Moose,  .drop=FALSE) %>% 
  filter(By == min(By)) %>%
  mutate(Moose = Moose - 0.2068966/2, By = By - 17.24138/2	) %>%
  arrange(Moose)

n_f22 <- n_f22 %>% bind_rows(n_f22[nrow(n_f22), ] %>% mutate(Moose = Moose))
n_f22

# make heatmap
heatmap_moose_By2 <- ggplot() +
  geom_tile(data = n_f2, aes(x = Moose, y = By, fill = percent_recover)) +
  geom_step(data = n_f22, aes(x = Moose, y = By), size = 0.5) +
  scale_fill_viridis_c() +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Percent\nrecovered") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))

heatmap_moose_By2

# recovery time
args_less22 <- args_less2
names(args_less22) <- names(arg_less_last2)


# remove any that go to grassland
args_recover2 <- args_less[ ! names(args_less) %>% str_detect("grassland") ]

# get years to reach stable
library(zoo)

return_year_stable_delta2 <- function(odes) {
  
  args_recover_delta <- as.data.frame(args_recover2) %>% 
    mutate(dMoose = Moose - lag(Moose), dPyoung = Pyoung - lag(Pyoung), dPmature = Pmature - lag(Pmature), dUnpal = Unpal - lag(Unpal)) %>% 
    filter(if_all(c(6:9), ~. <= 1e-8 | is.nan(.)))
  
  return(args_recover_delta)
}

args_recover_delta <- as.data.frame(args_recover2[[40]]) %>% 
  mutate(dMoose = Moose - lag(Moose), dPyoung = Pyoung - lag(Pyoung), dPmature = Pmature - lag(Pmature), dUnpal = Unpal - lag(Unpal))

ggplot(subset(args_recover_delta, time >250 & time < 270), aes(time, dMoose)) +
  geom_line()
ggplot(subset(args_recover_delta, time >240 & time < 350), aes(time, dPyoung)) +
  geom_line()
ggplot(subset(args_recover_delta, time >80 & time < 270), aes(time, dPmature)) +
  geom_line()
ggplot(subset(args_recover_delta, time >230 & time < 270), aes(time, dUnpal)) +
  geom_line()


year_stable2 <- lapply(args_recover2, return_year_stable_delta2)

names(args_id2) <- names(arg_less_last2)

# add to simulation data
f_yr2 <- data.frame(id2 = parse_number(names(year_stable2)), g_or_f = str_remove_all(names(year_stable2), "[:digit:]"), year = unlist(year_stable2)) %>% 
  left_join(args_id2) %>% 
  mutate(Moose = unlist(map(initial_conditions2, pluck, 1)), By = unlist(map(initial_conditions2, pluck, 2))) %>% 
  na.omit() %>% 
  select(!c(1,4,5))
# seems to depend mostly on parameter

avg_yr_f2 <- f_yr2 %>% 
  group_by(as.factor(Moose), as.factor(By), .drop = F) %>% 
  summarise(mean_yr = mean(as.numeric(year))) %>% 
  ungroup() %>% 
  mutate(Moose = as.numeric(as.character(`as.factor(Moose)`))) %>% 
  mutate(By = as.numeric(as.character(`as.factor(By)`))) %>% 
  select(c(3:5))

# take out ones that go to grassland over 50% of time
just_f <- avg_yr_f2 %>% 
  right_join(filter(n_f2, percent_recover >= 50))
  
n_f22100 <- n_f2 %>% 
  filter(percent_recover >= 100) %>% 
  group_by(Moose,  .drop=FALSE) %>% 
  filter(By == min(By)) %>%
  mutate(Moose = Moose - 0.2068966/2, By = By - 17.24138/2	) %>%
  arrange(Moose)

n_f22100 <- n_f22100 %>% bind_rows(n_f22100[nrow(n_f22100), ] %>% mutate(Moose = Moose))
n_f22100

heatmap_yr_f2 <- ggplot() +
  geom_tile(data = avg_yr_f2, aes(x = Moose, y = By, fill = mean_yr)) +
  geom_step(data = n_f22, aes(x = Moose, y = By), size = 0.5) +
  geom_step(data = n_f22100, aes(x = Moose, y = By), size = 0.5, color = "red") +
  scale_fill_viridis_c(direction = -1) +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Years to\nrecover") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylim(c(0,200))

heatmap_yr_f2 # seems that below the 50% mark the few simulations that do recover recover fast. skewed because the ones that take a long time to recover above the 50% line are pulled downward because the ones that recover here just dont recover uner 50%


cv <- function(x)  sd(x)/mean(x)
# 
# select only one column of
arg_last_25M_l2 <- lapply(lapply(args_less2,tail, 25, SIMPLIFY = T), "[", , 'Moose')
arg_last_25Py_l2 <- lapply(lapply(args_less2,tail, 25, SIMPLIFY = T), "[", , 'Pyoung')
arg_last_25Pm_l2 <- lapply(lapply(args_less2,tail, 25, SIMPLIFY = T), "[", , 'Pmature')
arg_last_25U_l2 <- lapply(lapply(args_less2,tail, 25, SIMPLIFY = T), "[", , 'Unpal')

arg_last_25_l2 <- list(arg_last_25M_l2, arg_last_25Py_l2, arg_last_25Pm_l2, arg_last_25U_l2)

arg_cv_l2 <- lapply(arg_last_25_l2, lapply, sd)

options(digits=22)

as.data.frame(arg_last_25M_l2[["24"]])

sprintf("%.30f",arg_last_25M_l2[["24"]])

# even using stiff solver and as much precision as I am allowed some still result in tiny variations
# but the deSolve vignette considered steady state if the difference between time steps is less than 1e-10 (which mine are well below) so can consider steady state
# and rootSolve considers steady state when the average change in stock is less than 1e-8
