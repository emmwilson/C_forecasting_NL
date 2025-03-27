cv_cv.glm2 <- function(data, vars,  k) {
  c_Output <- NULL
  
    seed.list <- 1334:1433 #set seed separately for each draw 
  
  #CV it 100 times
  for(x in seq(1,100,1)){
    set.seed(seed.list[[x]])
    #Step 1 - Re-shuffle the response
    #Reshuffle stoich data and place in new file
    c_Shuf <- transform(data, c_m2_site = sample(c_m2_site))
    
    #Step 2
    #Run the model
    #Need to do glm because boot works on glm. Normal distribution so same as lm.
    c_Rand <- lm_glm(vars, df = c_Shuf, c = "c_m2_site")
    
    #Step 3
    #Run cross validation
    cv <-cv.glm(data=c_Shuf, glmfit=c_Rand, K=k)
    
    
    #Step 4
    #Collect the cv values for each model. For each iteration x save the cv.
    c_Output <- rbind(c_Output, data.frame(Iteration = x, CV = cv$delta[2]))
    
    print (x)
  }
  
  #Step 5
  #Sort random set to calculate 90% CI for cv$delta[2]
  c_Output_Sort <- c_Output[order(c_Output$CV),]
  
  t.test(c_Output_Sort$CV)
}

## brglm cv function
gap_cv.brgml <- function(data, vars,  k) {
  gap_Output <- NULL
  
  seed.list <- 1334:1433
  
  #Do it 100 times
  for(x in seq(1,100,1)){
    set.seed(seed.list[[x]])
    #Step 1 - Re-shuffle the response
    #Reshuffle stoich data and place in new file
    gap_Shuf <- transform(data, gap = sample(gap))
    
    #Step 2
    #Run the model
    #Need to do glm because boot works on glm. Normal distribution so same as lm.
    gap_Rand <- firths_glm(vars, df = gap_Shuf, c = "gap")
    
    #Step 3
    #Run cross validation
    cv.gap <-cv.glm(data=gap_Shuf, glmfit=gap_Rand, K=k)
    
    
    #Step 4
    #Collect the cv values for each model. For each iteration x save the cv.
    gap_Output <- rbind(gap_Output, data.frame(Iteration = x, CV = cv.gap$delta[2]))
    
    print (x)
  }
  
  #Step 5
  #Sort random set to calculate 90% CI for cv$delta[2]
  gap_Output_Sort <- gap_Output[order(gap_Output$CV),]
  
  t.test(gap_Output_Sort$CV)
}