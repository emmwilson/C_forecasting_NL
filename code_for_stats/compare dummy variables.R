c_env_sub_scale_GM3 <- c_env_sub_scale_GM
c_env_sub_scale_GM3$Moose_H <- ifelse(c_env_sub_scale_GM3$Moose_new == "bH", 1, 0)
c_env_sub_scale_GM3$Moose_X <- ifelse(c_env_sub_scale_GM3$Moose_new == "cX", 1, 0)

dummy_var <- list("FHT", "EVIamp", "EVImed", "ELE", "SLO", "ASP", "LGS", "Moose_H", "Moose_X")

GM_lm_full_subfe3 <- lm_glm(c_env_sub_scale_GM3, "c_m2_subplot", dummy_var)
res3 <- model.matrix(GM_lm_full_subfe3) # gives model matrix
head(res3[, -1])

GM_lm_full_subfe <- lm_glm(c_env_sub_scale_GM, "c_m2_subplot", full_var)
res <- model.matrix(GM_lm_full_subfe) # gives model matrix
head(res[, -1])
