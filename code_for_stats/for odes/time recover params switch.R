#for params switch init and random 
# don;t actually wnat to switch direction of combinations because keeping each point its own random combination of params (averaged over starting conditions) allows us to focus on effect of parameter of interest

ode_fvsg2_params <- function(param_list_error) {
  
  args_param <- expand.grid(
    parametersp=param_list_error,
    initial_conditionsp=y_0dz2)
  
  list2env(args_param, envir=.GlobalEnv)
  
  args_param_id <- args_param %>% mutate(id = seq(1, nrow(args_param), by = 1))
  
  args_param$output <- mapply(ode_reps, parametersp, initial_conditionsp, SIMPLIFY = F)
  
  names(args_param$output) <- seq(1, nrow(args_param_id), by = 1)
  
  out_param_less2 <- purrr::discard(args_param$output, ~nrow(.) < 3)
  out_param_less_last2 <- lapply(out_param_less2,tail, 1, SIMPLIFY = T) 
  
  names(out_param_less_last2) <- paste(names(out_param_less_last2), ifelse(map(out_param_less_last2, pluck, 4) > 0, "forest", "grassland"))
  
  g_vs_f_param2 <- data.frame(id = parse_number(names(out_param_less_last2)), g_or_f = str_remove_all(names(out_param_less_last2), "[:digit:]")) %>% 
    left_join(args_param_id) %>% 
    mutate(Moose = unlist(map(initial_conditionsp, pluck, 1)), By = unlist(map(initial_conditionsp, pluck, 2))) %>% 
    select(!c(1, 3,4))
  
  return(g_vs_f_param2)
}

y_0dz2 <- initial_conditions[seq(1, length(initial_conditions), 3000)]

y_0dz2_id <- as.data.frame(t(as.data.frame(y_0dz2))) %>% 
  mutate(id = seq(1, length(y_0dz2), by = 1))

gf_param_yr3 <- lapply(param_listat, ode_fvsg2_params) 


# for each dataframe:
gf_param_yr3df <- lapply(gf_param_yr3, function(x) x %>% 
         group_by(as.factor(Moose), as.factor(By), as.factor(g_or_f), .drop=FALSE) %>% 
         dplyr::count(.drop = F) %>% 
         ungroup() %>% 
         mutate(Moose = as.numeric(as.character(`as.factor(Moose)`))) %>% 
         mutate(Pyoung = as.numeric(as.character(`as.factor(By)`))) %>% 
         mutate(g_or_f = `as.factor(g_or_f)`) %>%
         select(c(4:7)) %>% 
         filter(interaction(Moose, Pyoung) %in%
                      interaction(y_0dz2_id$Moose, y_0dz2_id$Pyoung)) %>% 
         subset(g_or_f == " forest") %>% 
         mutate(percent_recover = (n/length(param_listat[[1]]))*100) 
)


gf_param_yr3df_df <- as.data.frame(do.call(rbind, gf_param_yr3df)) %>% 
  mutate(param = rep(unlist(value_listat), each = length(y_0dz2))) 

ggplot(gf_param_yr3df_df, aes(x = as.factor(param), y = percent_recover)) +
  geom_violin() +
  geom_jitter(aes(color = interaction(Moose, Pyoung)))+
  scale_color_viridis(discrete = T)
