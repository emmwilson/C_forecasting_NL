# sensitivity of model to year - by fit on one year validate on the other, randomly subsample between years, is addition of year as effect significant
# put together and shuffle year randomly then run model many mamy times and get distributions
# are top model different when reshuffled

# deal with year after:

# refine hypotheses

# i. log(C) ~ ε
# 
# ii. log(C) ~ (1|site) + ε
# 
# iii. log(C) ~ all env. + ε
# 
# iv. log(C) ~ all env. + (1|site) + ε
# 
# v. log(C) ~ reduced env. + ε
# 
# vi. log(C) ~ reduced env. + (1|site) + ε
# 
# vii. log(C) ~ univariate + ε
# 
# viii. log(C) ~ univariate + (1|site) + ε


lm_null_site <- function(df, c) {
  lm_null_site <- lmer(formula(paste0("log(",quo_name(c),") ~ (1|site_id)")), data = df) #idk if the cross vaidatio will work, maybe use package "cv"
}
  
lm_full <- function(df, c, variables) {
  lm_full <- glm(formula(paste0("log(",quo_name(c),") ~", paste0(variables, collapse="+"))), data = df)

}

lm_full_site <- function(df, c, variables) {
  lm_full_site <- lmer(formula(paste0("log(",quo_name(c),") ~", paste0(variables, collapse="+"), "+ (1|site_id)")), data = df)
}

lm_uni <- function(df, c, variables) {
  lm_uni <- glm(formula(paste0("log(",quo_name(c),") ~", quo_name(variables))), data = df)
}

lm_uni_site <- function(df, c, variables) {
  lm_uni_site <- lmer(formula(paste0("log(",quo_name(c),") ~", quo_name(variables), "+ (1|site_id)")), data = df)
}

lm_red <- function(df, c, variables) {
  lm_red <- glm(formula(paste0("log(",quo_name(c),") ~", paste0(variables, collapse="+"))), data = df)
}

lm_red_site <- function(df, c, variables) {
  lm_red <- lmer(formula(paste0("log(",quo_name(c),") ~", paste0(variables, collapse="+"), "+ (1|site_id)")), data = df)
}
