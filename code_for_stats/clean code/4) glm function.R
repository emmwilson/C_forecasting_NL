# this function can run glm for any combination of data, column name for carbon, and variable combination
# especially useful for running univariate models

lm_glm <- function(df, c, variables) {
  lm_glm <- glm(formula(paste0(quo_name(c)," ~ ", paste0(variables, collapse="+"))), data = df, family = Gamma(link = "log"))
  eval <- eval(lm_glm$call$formula)
  formula1<- eval(eval)
  lm_glm$call$formula <- formula1
  lm_glm
}

bi_glm <- function(df, c, variables) {
  lm_glm <- glm(formula(paste0(quo_name(c)," ~ ", paste0(variables, collapse="+"))), data = df, family = binomial(link = "logit"))
  eval <- eval(lm_glm$call$formula)
  formula1<- eval(eval)
  lm_glm$call$formula <- formula1
  lm_glm
}