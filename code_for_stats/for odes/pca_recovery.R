# multiple coordination plot based on parameters in simulations for initial conmditions

library(ggfortify)


g_vs_fparm <- data.frame(id = parse_number(names(arg_less_last)), g_or_f = str_remove_all(names(arg_less_last), "[:digit:]")) %>% 
  left_join(args_id)

g_vs_fparm2 <- list()

for(i in 1:length(parms)) {
  nam <- names(parms)[i]
  g_vs_fparm2[[i]] <- assign(nam,unlist(map(g_vs_fparm$parameters, pluck, i)))

}

names(g_vs_fparm2) <- names(parms)

g_vs_fparm2df <- as.data.frame(g_vs_fparm2) %>% 
  cbind(g_vs_fparm)

saveRDS(g_vs_fparm2df, file = "~/C_forecasting_NL/code_for_stats/for odes/outputs/g_vs_fparm2df.RDS")

n_g_vs_fparm <- g_vs_fparm2df %>% 
  group_by_at(vars(names(parms), g_or_f), .drop=FALSE) %>% 
  count(.drop=FALSE) %>% 
  ungroup() 

# into %
n_fsparm <- n_g_vs_fparm %>% 
  subset( g_or_f == " forest") %>% 
  mutate(percent_recover = (n/nrow(g_vs_fparm2df)*100) )



