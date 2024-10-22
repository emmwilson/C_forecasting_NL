# multiple coordination plot based on parameters in simulations for initial conmditions

library(ggfortify)


g_vs_fparm <- data.frame(id = parse_number(names(arg_less_last)), g_or_f = str_remove_all(names(arg_less_last), "[:digit:]")) %>% 
  left_join(args_id)

g_vs_fparm2 <- list()

for(i in 1:length(parms)) {
  nam <- names(parms)[i]
  g_vs_fparm2[[i]] <- assign(nam,unlist(map(g_vs_fparm$parameters, pluck, i)))
  names(g_vs_fparm2) <- names(parms)
}

g_vs_fparm2df <- as.data.frame(g_vs_fparm2) %>% 
  cbind(g_vs_fparm)

n_g_vs_f <- g_vs_f %>% 
  group_by(as.factor(Moose), as.factor(By), as.factor(g_or_f), .drop=FALSE) %>% 
  count(.drop=FALSE) %>% 
  ungroup() %>% 
  mutate(Moose = as.numeric(as.character(`as.factor(Moose)`))) %>% 
  mutate(By = as.numeric(as.character(`as.factor(By)`))) %>% 
  rename(g_or_f = `as.factor(g_or_f)`) 


pca_res <- prcomp(df, scale. = TRUE)

autoplot(pca_res)