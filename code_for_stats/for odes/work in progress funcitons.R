# make small subset for testing
  parameters2 <- parameters[seq(1, length(parameters), 200)]
  initial_conditions2 <- initial_conditions[seq(1, length(initial_conditions), 500)]

  args2_l <- expand.grid(
    parameters2=parameters2,
    initial_conditions2=initial_conditions2) 

  args_id2 <- args2_l %>% mutate(id = seq(1, nrow(args2_l), by = 1))
  list2env(args2_l, envir=.GlobalEnv)

  time_vec1 <- seq(from = 1, to = 600, length.out = 600)
  
  args2_l$output <- mapply(ode_reps, parameters =parameters2, initial_conditions = initial_conditions2, SIMPLIFY = F)

  names(args2_l$output) <- seq(1, nrow(args_id2), by = 1)

  args_less2_l <- purrr::discard(args2_l$output, ~nrow(.) < 3)
  arg_less_last2_l <- lapply(args_less2_l,tail, 1, SIMPLIFY = T)
  names(arg_less_last2_l) <- paste(names(arg_less_last2_l), ifelse(map(arg_less_last2_l, pluck, 4) > 0, "forest", "grassland"))

# get at year 350 and give same name
arg_150_l <- lapply(lapply(args_less2_l,tail, 25, SIMPLIFY = T), head, 1)
names(arg_150_l) <- paste(names(arg_150_l), ifelse(map(arg_less_last2_l, pluck, 4) > 0, "forest", "grassland"))

# calculate difference
arg_350_400_M_l <- mapply('-', map(arg_less_last2_l, pluck, 2), map(arg_150_l, pluck, 2), SIMPLIFY = FALSE)
arg_350_400_Py_l <- mapply('-', map(arg_less_last2_l, pluck, 3), map(arg_150_l, pluck, 3), SIMPLIFY = FALSE)
arg_350_400_Pm_l <- mapply('-', map(arg_less_last2_l, pluck, 4), map(arg_150_l, pluck, 4), SIMPLIFY = FALSE)
arg_350_400_U_l <- mapply('-', map(arg_less_last2_l, pluck, 5), map(arg_150_l, pluck, 5), SIMPLIFY = FALSE)
# show any that aren't the same
library(rlist)
arg_350_400_M_less_l <- list.filter(arg_350_400_M_l, . > 0.00001)
arg_350_400_Py_less_l <- list.filter(arg_350_400_Py_l, . > 0.00001)
arg_350_400_Pm_less_l <- list.filter(arg_350_400_Pm_l, . > 0.00001)
arg_350_400_U_less_l <- list.filter(arg_350_400_U_l, . > 0.00001)

# some larger valeus in unpalatable 

# or look at coefficient of variation for last 25 years
cv <- function(x)  sd(x)/mean(x)

# select only one column of 
arg_last_25M_l <- lapply(lapply(args_less2_l,tail, 25, SIMPLIFY = T), "[", , 'Moose')
arg_last_25Py_l <- lapply(lapply(args_less2_l,tail, 25, SIMPLIFY = T), "[", , 'Pyoung')
arg_last_25Pm_l <- lapply(lapply(args_less2_l,tail, 25, SIMPLIFY = T), "[", , 'Pmature')
arg_last_25U_l <- lapply(lapply(args_less2_l,tail, 25, SIMPLIFY = T), "[", , 'Unpal')

arg_last_25_l <- list(arg_last_25M_l, arg_last_25Py_l, arg_last_25Pm_l, arg_last_25U_l)

arg_cv_l <- lapply(arg_last_25_l, lapply, cv)

library(rlist)
# return any that aren't 0 
arg_cv_less_l <- lapply(arg_cv_l, list.filter, . != 0)

max(unlist(arg_cv_less_l[[1]]))
max(unlist(arg_cv_less_l[[2]]))
max(unlist(arg_cv_less_l[[3]]))
max(unlist(arg_cv_less_l[[4]]))


arg_cv_less_l1 <- as.data.frame(do.call(rbind, arg_cv_less_l[[1]])) %>% mutate(id = as.numeric(names(arg_cv_less_l[[1]])))
arg_cv_less_l1_add <- left_join(arg_cv_less_l1, args_id)

arg_cv_less_l2 <- as.data.frame(do.call(rbind, arg_cv_less_l[[2]])) %>% mutate(id = as.numeric(names(arg_cv_less_l[[2]])))
arg_cv_less_l2_add <- left_join(arg_cv_less_l2, args_id)

arg_cv_less_l3 <- as.data.frame(do.call(rbind, arg_cv_less_l[[3]])) %>% mutate(id = as.numeric(names(arg_cv_less_l[[3]])))
arg_cv_less_l3_add <- left_join(arg_cv_less_l3, args_id)

arg_cv_less_l4 <- as.data.frame(do.call(rbind, arg_cv_less_l[[4]])) %>% mutate(id = as.numeric(names(arg_cv_less_l[[4]])))
arg_cv_less_l4_add <- left_join(arg_cv_less_l4, args_id)


plot(subset(as.data.frame(args2_l$output[[5326]]), time %in% 200:1500)$time, subset(as.data.frame(args2_l$output[[5326]]), time %in% 200:1500)$Moose) +
  lines(subset(as.data.frame(args2_l$output[[5326]]), time %in% 200:1500)$time, subset(as.data.frame(args2_l$output[[5326]]), time %in% 200:1500)$Moose)

# compare to earlier on in simulation

arg_last_25M_le <- lapply(lapply(lapply(args_less2_l, head, 500), tail, 25, SIMPLIFY = T), "[", , 'Moose')
arg_last_25Py_le <- lapply(lapply(lapply(args_less2_l, head, 500), tail, 25, SIMPLIFY = T), "[", , 'Pyoung')
arg_last_25Pm_le <- lapply(lapply(lapply(args_less2_l, head, 500), tail, 25, SIMPLIFY = T), "[", , 'Pmature')
arg_last_25U_le <- lapply(lapply(lapply(args_less2_l, head, 500), tail, 25, SIMPLIFY = T), "[", , 'Unpal')

arg_last_25_le <- list(arg_last_25M_le, arg_last_25Py_le, arg_last_25Pm_le, arg_last_25U_le)

arg_cv_le <- lapply(arg_last_25_le, lapply, cv)

library(rlist)
# return any that aren't 0 
arg_cv_less_le <- lapply(arg_cv_le, list.filter, . != 0)

max(unlist(arg_cv_less_le[[1]]))
max(unlist(arg_cv_less_le[[2]]))
max(unlist(arg_cv_less_le[[3]]))
max(unlist(arg_cv_less_le[[4]]))


arg_cv_less_le1 <- as.data.frame(do.call(rbind, arg_cv_less_le[[1]])) %>% mutate(id = as.numeric(names(arg_cv_less_le[[1]])))
arg_cv_less_le1_add <- left_join(arg_cv_less_le1, args_id)

arg_cv_less_le2 <- as.data.frame(do.call(rbind, arg_cv_less_le[[2]])) %>% mutate(id = as.numeric(names(arg_cv_less_le[[2]])))
arg_cv_less_le2_add <- left_join(arg_cv_less_l2, args_id)

arg_cv_less_le3 <- as.data.frame(do.call(rbind, arg_cv_less_le[[3]])) %>% mutate(id = as.numeric(names(arg_cv_less_le[[3]])))
arg_cv_less_le3_add <- left_join(arg_cv_less_l3, args_id)

arg_cv_less_le4 <- as.data.frame(do.call(rbind, arg_cv_less_le[[4]])) %>% mutate(id = as.numeric(names(arg_cv_less_le[[4]])))
arg_cv_less_le4_add <- left_join(arg_cv_less_le4, args_id)







## for n samples of random parameter combos vs n recover
# will take a long time so do on computer at school

n_p_list <- as.list(rgamma(100, 4, 0.0175), 1000)

rep_over_n_param_samples <- function (n_param) {
  
# make n random combinations of paramaters
  n_p <- n_param
  for(i in rownames(param_dist)){
    sampled_param[[i]] <- signif(rtruncnorm(n=n_p, a=param_dist[[i,1]], b=param_dist[[i,2]], mean=param_dist[[i,3]]), 4)
  }
  
  sampled_param_list <-  split(as.matrix(as.data.frame(sampled_param)), row(as.data.frame(sampled_param))) %>% 
    lapply(setNames, nm = names(parms))
  
  sampled_param_list_final <- list()
  sampled_param_list_final[[1]] <- parms
  sampled_param_list_final[[2]] <- parms
  sampled_param_list_final[[1]] <- sampled_param_list[[1]]
  
  
  
  # remove combinations that don't meet assumptions (around competative effects, growth rates, carrying capacities, and death rates)
  
  # relative growth rate
  sampled_param_list2 <-  sampled_param_list[sapply(sampled_param_list, `[`, 'rP') < sapply(sampled_param_list, `[`, 'rU')]
  
  # competative effects
  sampled_param_list3 <-  sampled_param_list2[sapply(sampled_param_list2, `[`, 'alphaUPm') < sapply(sampled_param_list2, `[`, 'alphaUPy') & sapply(sampled_param_list2, `[`, 'alphaUPy') < sapply(sampled_param_list2, `[`, 'alphaPyU') & sapply(sampled_param_list2, `[`, 'alphaPyU') < sapply(sampled_param_list2, `[`, 'alphaPmU')]
  
  # carrying capacities
  sampled_param_list4 <-  sampled_param_list3[sapply(sampled_param_list3, `[`, 'kU') < sapply(sampled_param_list3, `[`, 'kPy') & sapply(sampled_param_list3, `[`, 'kPy') < sapply(sampled_param_list3, `[`, 'kPm')]
  
  # loss
  sampled_param_list_final <-  sampled_param_list4[sapply(sampled_param_list4, `[`, 'lPm') < sapply(sampled_param_list4, `[`, 'lU')]

# combine with initial values (constant) 
  args <- expand.grid(
    parameters=sampled_param_list_final,
    initial_conditions=y_0d) 
  args_id <- args %>% mutate(id = seq(1, nrow(args), by = 1))
  list2env(args, envir=.GlobalEnv)
  
# run ode
  args$output <- mapply(ode_reps, parameters, initial_conditions, SIMPLIFY = F)
  
# get total number of recovered
  names(args$output) <- seq(1, nrow(args_id), by = 1)
  
  args_less <- purrr::discard(args$output, ~nrow(.) < 3)
  arg_less_last <- lapply(args_less,tail, 1, SIMPLIFY = T) 
  names(arg_less_last) <- paste(names(arg_less_last), ifelse(map(arg_less_last, pluck, 4) > 0, "forest", "grassland"))
  
  g_vs_f <- data.frame(id = parse_number(names(arg_less_last)), g_or_f = str_remove_all(names(arg_less_last), "[:digit:]")) %>% 
    left_join(args_id) %>% 
    mutate(Moose = unlist(map(initial_conditions, pluck, 1)), By = unlist(map(initial_conditions, pluck, 2))) %>% 
    select(!c(1,3,4))
  
  n_g_vs_f <- g_vs_f %>% 
    group_by(as.factor(Moose), as.factor(By), as.factor(g_or_f), .drop=FALSE) %>% 
    count(.drop=FALSE) %>% 
    ungroup() %>% 
    mutate(Moose = as.numeric(as.character(`as.factor(Moose)`))) %>% 
    mutate(By = as.numeric(as.character(`as.factor(By)`))) %>% 
    rename(g_or_f = `as.factor(g_or_f)`) %>% 
    select(c(3:6))
  
  total_rec <- sum(filter(n_g_vs_f, g_or_f == " forest")$n)
  
  return(total_rec)
}

n_p_recoveries <- lapply(n_p_list, rep_over_n_param_samples)

plot(n_p_list, n_p_recoveries)
