# multiple coordination plot based on parameters in simulations for initial conmditions

library(ggfortify)


# for all init conditions

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


g_vs_fparm2df <- readRDS(file = "~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/g_vs_fparm2df.RDS")


n_g_vs_fparm <- g_vs_fparm2df %>% 
  group_by_at(vars(names(parms), g_or_f), .drop=FALSE) %>% 
  count(.drop=FALSE) %>% 
  ungroup() 

# into %
n_fsparm <- n_g_vs_fparm %>% 
  subset( g_or_f == " forest") %>% 
  mutate(percent_recover = (n/nrow(g_vs_fparm2df)*100) )

# one model with all
model <- lm(formula(paste0("percent_recover ~ ", paste0(names(parms), collapse="+"))), data = n_fsparm)
summary(model)

# mdoel for each param
for(i in 1:length(names(parms))) {
  nam_lm <- paste0(names(parms)[i], "_model")
  assign(nam_lm, summary(lm(formula(paste0("percent_recover ~ ", paste0(names(parms)[i], collapse="+"))), data = n_fsparm)))
}

a_model
h_model
e_model
lM_model
rPy_model
rPm_model
s_model
alphaUPy_model
alphaUPm_model
kPy_model
kPm_model
g_model
lPm_model
lPy_model
rU_model
alphaPyU_model
alphaPmU_model
kU_model
lU_model

# for ones within gradient
y_0d_par <- readRDS(file = "~/Documents/Master's/C_forecasting_NL/code_for_stats/for odes/outputs/y_0d_par.RDS") 

# same as above but with left_join(y_0d_par)
g_vs_fparm_gr <- data.frame(id = parse_number(names(arg_less_last)), g_or_f = str_remove_all(names(arg_less_last), "[:digit:]")) %>% 
  left_join(args_id_par)

g_vs_fparm2_gr <- list()

for(i in 1:length(parms)) {
  nam <- names(parms)[i]
  g_vs_fparm2_gr[[i]] <- assign(nam,unlist(map(g_vs_fparm_gr$parameters, pluck, i)))
  
}

names(g_vs_fparm2_gr) <- names(parms)

g_vs_fparm2df_gr <- as.data.frame(g_vs_fparm2_gr) %>% 
  cbind(g_vs_fparm_gr)

n_g_vs_fparm_gr <- g_vs_fparm2df_gr %>% 
  group_by_at(vars(names(parms), g_or_f), .drop=FALSE) %>% 
  count(.drop=FALSE) %>% 
  ungroup() 

# into %
n_fsparm_gr <- n_g_vs_fparm_gr %>% 
  subset( g_or_f == " forest") %>% 
  mutate(percent_recover = (n/nrow(g_vs_fparm2df_gr)*100) )

# one model with all
model_gr <- lm(formula(paste0("percent_recover ~ ", paste0(names(parms), collapse="+"))), data = n_fsparm_gr)
summary(model_gr)

# mdoel for each param
for(i in 1:length(names(parms))) {
  nam_lm_gr <- paste0(names(parms)[i], "_model")
  assign(nam_lm_gr, summary(lm(formula(paste0("percent_recover ~ ", paste0(names(parms)[i], collapse="+"))), data = n_fsparm_gr)))
}