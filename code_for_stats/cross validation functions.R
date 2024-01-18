

# using cv::cv  =========================================================

# need to try code using cv not cv.glm
#cv_try2 <- cv.glm_works(c_env_sub_scale_GM, "c_m2_subplot", lm_full_site, "@")  this code won't work because trying to specify a random effec when the cross validation can only hangle glm

##  glm  =========================================================

# works for full model, reduced model, and can use lapply to apply over all univariate models (lapply(full_var, cv.cv_glm_var, df = df, c = "c", model = model))

cv.cv_glm <- function(df, c, model, variables) {
  
  C_LM_Output_Sort <- NULL
  C_LM_Output <- NULL  # create empty vectors
  
  #Do it 100 times
  for(x in seq(1,100,1)){
    
    df2<-df[complete.cases(df),] #remove NAs
    
    model_C <- model(df2, c, variables) #run function for model of interest on nonshuffled data
    
    eval <- eval(model_C$call$formula)
    formula1<- eval(eval) #extract the readable formula
    
    #shuffle data
    C_Shuf <- df2
    #We are shuffling the 4th column as this is the C column
    C_Shuf[,4] <-sample(C_Shuf[,4])
    
    #Step 2
    #Run the model on shuffled data
    
    lm_C_Rand <- glm(formula = formula1, data = C_Shuf) # use readable formula and shuffled data
    lm_C_Rand$call$formula <- formula1 # make sure glm formula is written readable
    
    #get cross validation
    cv <-cv::cv(data = C_Shuf, model=lm_C_Rand, k=10)
    
    
    #Step 4
    #Collect the cv values for each model. For each iteration x save the cv.
    C_LM_Output <- rbind(C_LM_Output, data.frame(Iteration = x, CV = cv$`adj CV crit`))
    
    print (x)
  }
  
  #Step 5
  #Sort random set to calculate 90% CI for cv$delta[2]
  C_LM_Output_Sort <- C_LM_Output[order(C_LM_Output$CV),]
}


# for lmer 
# I think the cvsm cross-validation needs you to fold you data beforehand?
# following code for cv::cv but it gives error

## lmer  =========================================================

# gives extremely weird numbers for adjusted cv (eg; 7025536557) 
# might be because of warning "fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients"
# will try with reduced models
# tried with reduced models (see cv_try3_red), didn't get any warnings but still got very large numbers


cv.cv_lmer <- function(df, c, model, variables) {
  
  C_LM_Output_Sort <- NULL
  C_LM_Output <- NULL  # create empty vectors
  
  #Do it 100 times
  for(x in seq(1,100,1)){
    
    df2<-df[complete.cases(df),] #remove NAs
    
    model_C <- model(df2, c, variables) #run function for model of interest on nonshuffled data
    
    eval <- eval(model_C@call$formula) # @ instead of $ because s4
    formula1<- eval(eval) #extract the clean formula
    
    #shuffle data
    C_Shuf <- df2
    #We are shuffling the 4th column as this is the C column
    C_Shuf[,4] <-sample(C_Shuf[,4])
    
    #Step 2
    #Run the model on shuffled data
    
    lm_C_Rand <- lmer(formula = formula1, data = C_Shuf) # use clean formula and shuffled data
    lm_C_Rand@call$formula <- formula1 # make sure glm formula is written cleanly
    
    #get cross validation
    cv <-cv::cv(data = C_Shuf, model=lm_C_Rand, k=10)
    
    
    #Step 4
    #Collect the cv values for each model. For each iteration x save the cv.
    C_LM_Output <- rbind(C_LM_Output, data.frame(Iteration = x, CV = cv$`adj CV crit`))
    
    print (x)
  }
  
  #Step 5
  #Sort random set to calculate 90% CI for cv$delta[2]
  C_LM_Output_Sort <- C_LM_Output[order(C_LM_Output$CV),]
}
# 
# cv_try3 <- cv.cv_lmer(c_env_sub_scale_GM, "c_m2_subplot", lm_full_site, full_var)
# 
# red_var <- list("FHT")
# cv_try3_red <- cv.cv_lmer(c_env_sub_scale_GM, "c_m2_subplot", lm_full_site, red_var)
# 
# df_real <- c_env_sub_scale_GM[complete.cases(c_env_sub_scale_GM),]
# 
# cv_real_full_try <- cv(GM_lm_full_site_sub, data = df_real, k = 10)
# cv_real_red_try <- cv(GM_lm_red1_site_sub, data = df_real, k = 10)
# 
# cv_real_full_try
# cv_real_red_try
# 
# library(nlme)
# 
# plot(GM_lm_full_site_sub)
# hist(resid(GM_lm_full_site_sub), main="", xlab="Residuals") 
# qqnorm(resid(GM_lm_full_site_sub))
# qqline(resid(GM_lm_full_site_sub))


# its got to be something about the random effects?
#internet said maybe colinearity but I tried with only one predictor variable and still get huge numbers, also don;t think it's a regularization problem bceause it works when I dont include random effects


# using boot::cv.glm  =========================================================
# works only for models built with glm

cv.glm_works <- function(df, c, model, variables) {
  
  C_LM_Output_Sort <- NULL
  C_LM_Output <- NULL  # create empty vectors
  
  #Do it 100 times
  for(x in seq(1,100,1)){
    
    df2<-df[complete.cases(df),] #remove NAs
    
    model_C <- model(df2, c, variables) #run function for model of interest on nonshuffled data
    
    eval <- eval(model_C$call$formula)
    formula1<- eval(eval) #extract the clean formula
    
    #shuffle data
    C_Shuf <- df2
    #We are shuffling the 4th column as this is the C column
    C_Shuf[,4] <-sample(C_Shuf[,4])
    
    #Step 2
    #Run the model on shuffled data
    
    lm_C_Rand <- glm(formula = formula1, data = C_Shuf) # use clean formula and shuffled data
    
    #get cross validation
    cv <-cv.glm(data=C_Shuf, glmfit=lm_C_Rand, K=10)
    
    
    #Step 4
    #Collect the cv values for each model. For each iteration x save the cv.
    C_LM_Output <- rbind(C_LM_Output, data.frame(Iteration = x, CV = cv$delta[2]))
    
    print (x)
  }
  
  #Step 5
  #Sort random set to calculate 90% CI for cv$delta[2]
  C_LM_Output_Sort <- C_LM_Output[order(C_LM_Output$CV),]
}

# cv_try1 <- cv.glm_works(c_env_sites_scale_GM, "c_m2_site", lm_full, full_var)



# using cvsm::cross.validate  =========================================================

# don't need anymore

#i'm not certain this works as a k-fold cross validation
# if it does I think you have to fold the data before putting it through cross.validate()
# I think I may be able to get it to work (unclear if it will accept log transformed dependant variable) but either way I don't think it gives the outputs we want

# followed code from package

# library(groupdata2)
# 
# df2<-c_env_sub_scale_GM[complete.cases(c_env_sub_scale_GM),]
# df2
# 
# lm_full_site_test <- lm_full_site(df2, "c_m2_subplot")
# formula_test <- deparse(lm_full_site_test@call$formula)
# # Set seed for reproducibility
# set.seed(7)
# 
# # Fold data
# data <- fold(
#   df2,
#   k = 10
# ) %>%
#   arrange(.folds)
# 
# cvsm_cross <- cross_validate(
#   data,
#   formulas = formula_test,
#   family = "gaussian",
#   REML = F
# )
# 
