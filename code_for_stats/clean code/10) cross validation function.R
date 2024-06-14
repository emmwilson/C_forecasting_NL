GM_c_Output <- NULL

#Do it 100 times
for(x in seq(1,100,1)){
  
  #Step 1 - Re-shuffle the response
  #Reshuffle stoich data and place in new file
  GM_c_Shuf <- GM_c_env
  #We are shuffling the seventh column as this is the Percent C column
  GM_c_Shuf[,7] <-sample(GM_c_Shuf[,7])
  
  #Step 2
  #Run the model
  #Need to do glm because boot works on glm. Normal distribution so same as lm.
  GM_C_Rand <- lm_glm(GM_var, df = GM_c_Shuf, c = "c_m2_site")
  
  #Step 3
  #Run cross validation
  cv <-cv.glm(data=GM_c_Shuf, glmfit=GM_C_Rand, K=10)
  
  
  #Step 4
  #Collect the cv values for each model. For each iteration x save the cv.
  GM_c_Output <- rbind(GM_c_Output, data.frame(Iteration = x, CV = cv$delta[2]))
  
  print (x)
}

#Step 5
#Sort random set to calculate 90% CI for cv$delta[2]
GM_c_Output_Sort <- GM_c_Output[order(GM_c_Output$CV),]
