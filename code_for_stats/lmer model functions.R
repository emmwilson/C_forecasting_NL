# sensitivity of model to year - by fit on one year validate on the other, randomly subsample between years, is addition of year as effect significant
# put together and shuffle year randomly then run model many mamy times and get distributions
# are top model different when reshuffled

# deal with year after:

# refine hypotheses

# i. log(C) ~ ε
# 
# ii. log(C) ~ (1|year) + ε
# 
# ii. log(C) ~ (1|site) + ε
# 
# iv. log(C) ~ all env. + ε
# 
# v. log(C) ~ all env. + (1|year) + ε
# 
# vi. log(C) ~ all env. + (1|site) + ε
# 
# vii. log(C) ~ reduced env. + ε
# 
# viii. log(C) ~ reduced env. + (1|year) + ε
# 
# ix. log(C) ~ reduced env. + (1|site) + ε

# xi. univariate model

null_year <- function(df) {
  null <- glm(log(c_m2_subplot) ~ (1|year), data = df)
  print(summary(null))
}

null_site <- function(df) {
  null_site <- glm(log(c_m2_subplot) ~ (1|site_id), data = df)
  print(summary(null_site))
}

null_yearsite <- function(df) {
  null_site <- glm(log(c_m2_subplot) ~ (1|year/site_id), data = df)
  print(summary(null_site))
}
  
full_FHT <- function(df) {
  full_FHT <- glm(log(c_m2_subplot) ~ FHT + EVIamp + EVImed + ELE + SLO + ASP + CEC_LCT, data = df)
  print(summary(full_FHT))
}

full_CC <- function(df) {
  full_CC <- glm(log(c_m2_subplot) ~ CC + EVIamp + EVImed + ELE + SLO + ASP + CEC_LCT, data = df)
  print(summary(full_CC))
}

full_FHT_year <- function(df) {
  full_FHT_year <- glm(log(c_m2_subplot) ~ FHT + EVIamp + EVImed + ELE + SLO + ASP + CEC_LCT + (1|year), data = df)
  print(summary(full_FHT_year))
}

full_FHT_site <- function(df) {
  full_FHT_site <- glm(log(c_m2_subplot) ~ FHT + EVIamp + EVImed + ELE + SLO + ASP + CEC_LCT + (1|site), data = df)
  print(summary(full_FHT_site))
}

full_FHT_yearsite <- function(df) {
  full_FHT_site <- glm(log(c_m2_subplot) ~ FHT + EVIamp + EVImed + ELE + SLO + ASP + CEC_LCT + (1|year/site), data = df)
  print(summary(full_FHT_site))
}

full_CC_year <- function(df) {
  full_CC_year <- glm(log(c_m2_subplot) ~ CC + EVIamp + EVImed + ELE + SLO + ASP + CEC_LCT + (1|year), data = df)
  print(summary(full_CC_year))
}

full_CC_site <- function(df) {
  full_CC_site <- glm(log(c_m2_subplot) ~ CC + EVIamp + EVImed + ELE + SLO + ASP + CEC_LCT + (1|site), data = df)
  print(summary(full_CC_site))
}

full_CC_yearsite <- function(df) {
  full_CC_site <- glm(log(c_m2_subplot) ~ CC + EVIamp + EVImed + ELE + SLO + ASP + CEC_LCT + (1|year/site), data = df)
  print(summary(full_CC_site))
}