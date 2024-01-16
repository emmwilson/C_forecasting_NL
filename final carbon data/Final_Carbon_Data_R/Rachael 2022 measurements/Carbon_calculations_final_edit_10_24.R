#R code to calculate the carbon content of trees, deadwood, woody shrubs,
#herbaceous vegetation, ground vegetation, litter, soil, and roots.

#load packages
library(readxl)
library(openxlsx)
library(tidyverse)
library(dtplyr)

#set working directory
setwd("C:/Users/rmora/OneDrive/Documents/MUN/Thesis/Data Files")

##Tree carbon##

#Steps taken to calculate the biomass and carbon content of trees over 3 metres 
#in height. The equations and parameters used to do this are from table 4 in
#Lambert et al., (2005).

#For Lambert et al., 2005 paper see here:https://cdnsciencepub.com/doi/10.1139/x05-112

#load data
Tree_Biomass <- read_excel("Tree_Biomass/Tree_Biomass.xlsx")

#Create custom function to calculate the biomass of wood in living trees.
wood_biomass <- function(x1,x2,x3,x4,x5){x1 * (x2^x3) * (x4^x5)}

#Create custom function to calculate the biomass of bark in living trees.
bark_biomass <- function(x1,x2,x3,x4,x5){x1 * (x2^x3) * (x4^x5)}

#Create custom function to calculate the biomass of branches in living trees.
branch_biomass <- function(x1,x2,x3,x4,x5){x1 * (x2^x3) * (x4^x5)}

#Create custom function to calculate the biomass of foliage in living trees.
foliage_biomass <- function(x1,x2,x3,x4,x5){x1 * (x2^x3) * (x4^x5)}

#Set columns as numeric.
Tree_Biomass$Height <- as.numeric(Tree_Biomass$Height)
Tree_Biomass$DBH <-as.numeric(Tree_Biomass$DBH)

#Create a subset of trees identified as mountain_ash & alder.
ash_alder_subset <- subset(Tree_Biomass, Species=="mountain_ash" 
                           | Species =="alder")

#Fill in the parameter columns for mountain_ash & alder data. Note that the 
#parameters used below are listed under "General Hardwood" in 
#Lambert et al., (2005).

ash_alder_subset$Bwood1 = 0.0359
ash_alder_subset$Bwood2 = 2.0263
ash_alder_subset$Bwood3 = 0.6987
ash_alder_subset$Bbark1 = 0.0094
ash_alder_subset$Bbark2 = 1.8677
ash_alder_subset$Bbark3 = 0.6985
ash_alder_subset$Bbranches1 = 0.0433
ash_alder_subset$Bbranches2 = 2.6817
ash_alder_subset$Bbranches3 = -0.5731
ash_alder_subset$Bfoliage1 = 0.0859
ash_alder_subset$Bfoliage2 = 1.8485
ash_alder_subset$Bfoliage3 = -0.5383

#Create a subset of trees identified as balsam fir.
balsam_fir_subset <- subset(Tree_Biomass, Species=="balsam_fir")

#Note that there is a missing DBH value within the balsam fir data.
#To solve this issue, I will create a scatter plot of all other balsam fir
#height and DBH data. I will then use the slope of this data to calculate the
#missing DBH value.

#Create a scatter plot of balsam fir DBH & height values and run a linear
#regression model of this data.Note that we forced the intercept through zero.
plot(balsam_fir_subset$Height,balsam_fir_subset$DBH, xlab='Height (m)
     ',ylab='DBH (cm)')
bF_lm<- lm(DBH~0+Height,data=balsam_fir_subset)

#Retrieve summary of linear regression model.
summary(bF_lm)

#Use slope of linear regression to calculate missing DBH value 
#(i.e. multiplying it by the recorded height value). Ensure that location of 
#missing value is still in row 112.
balsam_fir_subset[112, "DBH"] <-1.73132*3.9

#Fill in the parameter columns for balsam_fir data.
balsam_fir_subset$Bwood1 = 0.0294
balsam_fir_subset$Bwood2 = 1.8357
balsam_fir_subset$Bwood3 = 0.8640
balsam_fir_subset$Bbark1 = 0.0053
balsam_fir_subset$Bbark2 = 2.0876
balsam_fir_subset$Bbark3 = 0.5842
balsam_fir_subset$Bbranches1 = 0.0117
balsam_fir_subset$Bbranches2 = 3.5097
balsam_fir_subset$Bbranches3 = -1.3006
balsam_fir_subset$Bfoliage1 = 0.1245
balsam_fir_subset$Bfoliage2 = 2.5230
balsam_fir_subset$Bfoliage3 = -1.1230

#Create a subset of trees identified as yellow_birch.
yellow_birch_subset <- subset(Tree_Biomass, Species=="yellow_birch")

#Fill in the parameter columns for yellow_birch data.
yellow_birch_subset$Bwood1 = 0.0259
yellow_birch_subset$Bwood2 = 1.9044
yellow_birch_subset$Bwood3 = 0.9715
yellow_birch_subset$Bbark1 = 0.0069
yellow_birch_subset$Bbark2 = 2.0834
yellow_birch_subset$Bbark3 = 0.5371
yellow_birch_subset$Bbranches1 = 0.0325
yellow_birch_subset$Bbranches2 = 2.3851
yellow_birch_subset$Bbranches3 = 0
yellow_birch_subset$Bfoliage1 = 0.1683
yellow_birch_subset$Bfoliage2 = 1.2764
yellow_birch_subset$Bfoliage3 = 0

#Create a subset of trees identified as white_birch.
white_birch_subset <- subset(Tree_Biomass, Species=="white_birch")

#Fill in the parameter columns for white_birch data.
white_birch_subset$Bwood1 = 0.0338
white_birch_subset$Bwood2 = 2.0702
white_birch_subset$Bwood3 = 0.6876
white_birch_subset$Bbark1 = 0.0080
white_birch_subset$Bbark2 = 1.9754
white_birch_subset$Bbark3 = 0.6659
white_birch_subset$Bbranches1 = 0.0257
white_birch_subset$Bbranches2 = 3.1754
white_birch_subset$Bbranches3 = -0.9417
white_birch_subset$Bfoliage1 = 0.1415
white_birch_subset$Bfoliage2 = 2.3074
white_birch_subset$Bfoliage3 = -1.1189

#Create a subset of trees identified as black_spruce.
black_spruce_subset <- subset(Tree_Biomass, Species=="black_spruce")

#Fill in the parameter columns for black_spruce data.
black_spruce_subset$Bwood1 = 0.0309
black_spruce_subset$Bwood2 = 1.7527
black_spruce_subset$Bwood3 = 1.0014
black_spruce_subset$Bbark1 = 0.0115
black_spruce_subset$Bbark2 = 1.7405
black_spruce_subset$Bbark3 = 0.6589
black_spruce_subset$Bbranches1 = 0.0380
black_spruce_subset$Bbranches2 = 3.2558
black_spruce_subset$Bbranches3 = -1.4218
black_spruce_subset$Bfoliage1 = 0.2048
black_spruce_subset$Bfoliage2 = 2.5754
black_spruce_subset$Bfoliage3 = -1.3704

#Create a subset of trees identified as red_maple, mountain_maple, or
#only identified to the maple genus. Note that the parameters used below are 
#listed under "red maple" in Lambert et al., (2005).
maple_subset <- subset(Tree_Biomass, Species=="red_maple"|
                         Species=="mountain_maple"|Species =="maple")

#Fill in the parameter columns for maple data.
maple_subset$Bwood1 = 0.0315
maple_subset$Bwood2 = 2.0342
maple_subset$Bwood3 = 0.7485
maple_subset$Bbark1 = 0.0283
maple_subset$Bbark2 = 2.0907
maple_subset$Bbark3 = 0
maple_subset$Bbranches1 = 0.0225
maple_subset$Bbranches2 = 2.4106
maple_subset$Bbranches3 = 0
maple_subset$Bfoliage1 = 0.0571
maple_subset$Bfoliage2 = 1.4898
maple_subset$Bfoliage3 = 0

#Create a subset of trees identified as trembling_aspen.
aspen_subset <- subset(Tree_Biomass, Species=="trembling_aspen")

#Fill in the parameter columns for aspen data.
aspen_subset$Bwood1 = 0.0142
aspen_subset$Bwood2 = 1.9389
aspen_subset$Bwood3 = 1.0572
aspen_subset$Bbark1 = 0.0063
aspen_subset$Bbark2 = 2.0819
aspen_subset$Bbark3 = 0.6617
aspen_subset$Bbranches1 = 0.0137
aspen_subset$Bbranches2 = 2.9270
aspen_subset$Bbranches3 = -0.6221
aspen_subset$Bfoliage1 = 0.0270
aspen_subset$Bfoliage2 = 1.6183
aspen_subset$Bfoliage3 = 0

#Create a subset of trees identified as pin_cherry.Note that the parameters used 
#below are listed under "black cherry" in Lambert et al., (2005).
cherry_subset <- subset(Tree_Biomass, Species=="pin_cherry")

#Fill in the parameter columns for pin_cherry data.
cherry_subset$Bwood1 = 0.0181
cherry_subset$Bwood2 = 1.7013
cherry_subset$Bwood3 = 1.3057
cherry_subset$Bbark1 = 0.0101
cherry_subset$Bbark2 = 1.5956
cherry_subset$Bbark3 = 0.9190
cherry_subset$Bbranches1 = 0.0005
cherry_subset$Bbranches2 = 2.8004
cherry_subset$Bbranches3 = 0.8603
cherry_subset$Bfoliage1 = 0.1976
cherry_subset$Bfoliage2 = 1.4421
cherry_subset$Bfoliage3 = -0.5264

#Create a subset of data where no trees were present in the subplots.
NA_subset <- subset(Tree_Biomass, Species=="NA")

#Fill in the parameter columns for NA data.
NA_subset$Bwood1 = 0
NA_subset$Bwood2 = 0
NA_subset$Bwood3 = 0
NA_subset$Bbark1 = 0
NA_subset$Bbark2 = 0
NA_subset$Bbark3 = 0
NA_subset$Bbranches1 = 0
NA_subset$Bbranches2 = 0
NA_subset$Bbranches3 = 0
NA_subset$Bfoliage1 = 0
NA_subset$Bfoliage2 = 0
NA_subset$Bfoliage3 = 0
NA_subset$Height = 0
NA_subset$DBH = 0

#Join all data subsets back together.
all_trees <- rbind(ash_alder_subset, aspen_subset, balsam_fir_subset,
                       black_spruce_subset, cherry_subset, maple_subset, NA_subset, 
                       white_birch_subset, yellow_birch_subset) 

#Use wood biomass function to fill the wood column for all data.
all_trees$wood <- wood_biomass(all_trees$Bwood1, all_trees$DBH,
                               all_trees$Bwood2, all_trees$Height, all_trees$Bwood3)

#Use bark biomass function to fill the bark column for all data.
all_trees$bark <- bark_biomass(all_trees$Bbark1, all_trees$DBH,
                               all_trees$Bbark2, all_trees$Height, all_trees$Bbark3)

#Use branch biomass function to fill the branches column for all data.
all_trees$branches <- branch_biomass(all_trees$Bbranches1, all_trees$DBH,
                                     all_trees$Bbranches2, all_trees$Height, all_trees$Bbranches3)

#Use foliage biomass function to fill the foliage column for all data.
all_trees$foliage <- foliage_biomass(all_trees$Bfoliage1, all_trees$DBH,
                                         all_trees$Bfoliage2, all_trees$Height, all_trees$Bfoliage3)

#Calculate total biomass per tree by summing the biomass within each 
#compartment (i.e. wood, bark, branches, foliage).
all_trees$tree_biomass <- (all_trees$wood+all_trees$bark+
                              all_trees$branches+all_trees$foliage)

#Multiply total biomass by 0.5 to calculate total carbon content per tree.
all_trees$carbon <- (all_trees$tree_biomass*0.5)

#Combine columns describing Site ID & Subplot ID. Note that although this 
#carbon pool was measured in 5 m^2 transects, we are sizing up all carbon pools
#to the 9 m^2 subplot.
all_trees$Site_Subplot_ID <- paste(all_trees$Park_ID,
                                   all_trees$Site_ID,all_trees$Excl_Ctrl,
                                   all_trees$Subplot_ID, sep="_")

#Sum the carbon of trees within each transect.
carbon_trees <-aggregate(all_trees$carbon, list
                         (all_trees$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(carbon_trees) <- c("Site_Subplot_ID", "carbon_5m2")

#Size up carbon from 5^m2 transect to 9m^2 subplot.
carbon_trees$carbon_9m2<-carbon_trees$carbon_5m2*(9/5)

#Export final dataset to Excel file
write.xlsx(total_dataset, file="Tree_Biomass_Final.xlsx")
write.xlsx(carbon_trees, file="Carbon_Trees.xlsx")

##################################################################################
##Deadwood Carbon

#The equations used for calculating the biomass in deadwood are from Richardson et al., 2009
#(see equations 1 & 6). The values used for converting volume to biomass are from 
#Tables 4 & 5 in Harmon et al., 2011. Note that if deadwood was identified, 
#I used the hardwood or softwood values in Table 5; otherwise, I used the values 
#for all species found in Table 4.

#See here for Harmon et al.,2011 paper: https://permanent.fdlp.gov/gpo14619/rp-nrs15.pdf
#See here for Richardson et al., 2009 paper: https://www.academia.edu/770677/Deadwood_in_New_Zealands_indigenous_forests

#load Deadwood_Field_Data spreadsheet
Deadwood_Field_Data <- read_excel("Deadwood/Deadwood_Field_Data.xlsx")

#set columns as numeric
Deadwood_Field_Data$Length <- as.numeric(Deadwood_Field_Data$Length)
Deadwood_Field_Data$Height <- as.numeric(Deadwood_Field_Data$Height)
Deadwood_Field_Data$Diameter_A <-as.numeric(Deadwood_Field_Data$Diameter_A)
Deadwood_Field_Data$Diameter_B <-as.numeric(Deadwood_Field_Data$Diameter_B)
Deadwood_Field_Data$Diameter_C <-as.numeric(Deadwood_Field_Data$Diameter_C)
Deadwood_Field_Data$Diameter_D <-as.numeric(Deadwood_Field_Data$Diameter_D)

#create custom function to calculate the volume of standing deadwood
standing_volume <- function(dbh,h){0.0000598 * (dbh^2 * h)^0.946*(1-0.0019*dbh)}

#create custom function to calculate the volume of stumps
stump_volume <- function(len,x1,x2) {(pi*len/32)*((x1+x2)^2+
                                                    (x1+x2)^2)}

#create custom function to calculate the volume of fallen logs
log_volume <- function(len,x1,x2,x3,x4) {(pi*len/32)*((x1+x2)^2+
                                                        (x3+x4)^2)}

#Create a subset of stump data
stump_subset <- subset(Deadwood_Field_Data, Status=="stump")

#Use stump volume function to fill the volume column for stump subset. 
#Note that for stump entries,the height column was filled with the recorded length.
#Diameter measurements were recorded in cm and converted to m.
stump_subset$Volume <- stump_volume(stump_subset$Height,
                                    stump_subset$Diameter_A/100,stump_subset$Diameter_B/100)

#Create a subset of fallen log data with all diameter measurements
log_subset1 <- subset(Deadwood_Field_Data, Status=="F" & !is.na(Diameter_C))

#Use log volume function to fill the volume column for log subset.
#Diameter measurements were recorded in cm and converted to m.
log_subset1$Volume <- log_volume(log_subset1$Length,
                                 log_subset1$Diameter_A/100,log_subset1$Diameter_B/100, log_subset1$Diameter_C/100,
                                 log_subset1$Diameter_D/100)

#Create a subset of fallen log data with just 2 diameter measurements.
log_subset2 <- subset(Deadwood_Field_Data, Status=="F" & is.na(Diameter_C))

#Use stump volume function to fill the volume column for logs with missing 
#diameter measurements.
#Diameter measurements were recorded in cm and converted to m.
log_subset2$Volume <- stump_volume(log_subset2$Length,
      log_subset2$Diameter_A/100,log_subset2$Diameter_B/100)

#Create a subset of standing deadwood data
standing_subset <- subset(Deadwood_Field_Data, Status=="S")

#use standing volume function to fill the volume column for standing
#deadwood
standing_subset$Volume <- standing_volume(standing_subset$Diameter_A,
                                          standing_subset$Height)

#create a subset of data with no deadwood present in subplots
NA_subset <- subset(Deadwood_Field_Data, Status=="NA")

#Fill in volume column for NA subset.
NA_subset$Volume <- 0

#Join all data subsets back together.
all_deadwood <- rbind(log_subset1, log_subset2, NA_subset, standing_subset,
                       stump_subset) 

#Convert all volume values from cubic metres to cubic centimetres.
all_deadwood$volume_cm3 <- (all_deadwood$Volume * 1000000)

#Create a subset of the data that was identified as downed balsam_fir or spruce.
#Note that all spruce entries are downed deadwood.
softwood_subset1 <-subset(all_deadwood, Species=="balsam_fir" & Status == "F"|
                            Species=="spruce")

#Fill in the biomass column for deadwood that was identified as downed softwood.
#Note that none of these entries were recorded as decay class 3.
softwood_subset1$biomass = ifelse(softwood_subset1$Decay_Class %in% c("1"),
    softwood_subset1$volume_cm3*0.38,ifelse(softwood_subset1$Decay_Class %in% c("2"),
    softwood_subset1$volume_cm3*0.34,ifelse(softwood_subset1$Decay_Class %in% c("4"),
    softwood_subset1$volume_cm3*0.15, softwood_subset1$volume_cm3<-0)))

#Create subset of the data that was identified as standing softwood.
softwood_subset2 <- subset(all_deadwood, Species == "balsam_fir" & Status == "S")

#Fill in the biomass column for deadwood that was identified as standing softwood. Note that
#there is a data entry for a standing balsam fir within class 5, but in Harmon et al., 2011
#no standing species within decay class 5 were sampled; I therefore use the values
#from decay class 4 for this data entry.
softwood_subset2$biomass <- softwood_subset2$volume_cm3*0.26

#Create a subset of the data that was identified as downed birch.
hardwood_subset1 <- subset(all_deadwood, Species == "birch" & Status =="F")

#Fill in the biomass column for deadwood that was identified as downed birch.
hardwood_subset1$biomass = ifelse(hardwood_subset1$Decay_Class %in% c("1"),
    hardwood_subset1$volume_cm3*0.43,ifelse(hardwood_subset1$Decay_Class %in% c("2"),
    hardwood_subset1$volume_cm3*0.33,ifelse(hardwood_subset1$Decay_Class %in% c("3"),
    hardwood_subset1$volume_cm3*0.23,ifelse(hardwood_subset1$Decay_Class %in% c("4"),
    hardwood_subset1$volume_cm3*0.13,ifelse(hardwood_subset1$Decay_Class %in% c("5"),
    hardwood_subset1$volume_cm3*0.11, hardwood_subset1$volume_cm3<-0)))))

#Create a subset of the data that was identified as standing birch.
hardwood_subset2 <- subset(all_deadwood, Species == "birch" & Status == "S")

#Fill in the biomass column for deadwood that was identified as standing birch.
#Note that only one entry of decay class 5 exists (I used value from decay class 4).
hardwood_subset2$biomass <- hardwood_subset2$volume_cm3*0.2  

#Create a subset of the data that was not identified and was downed deadwood.
downed_subset <- subset(all_deadwood, Species="NA" & Status =="F")

#Fill in the biomass column for downed deadwood that was not identified.
downed_subset$biomass = ifelse(downed_subset$Decay_Class %in% c("1"),
   downed_subset$volume_cm3*0.4,ifelse(downed_subset$Decay_Class %in% c("2"),
   downed_subset$volume_cm3*0.33,ifelse(downed_subset$Decay_Class %in% c("3"),
   downed_subset$volume_cm3*0.26,ifelse(downed_subset$Decay_Class %in% c("4"),
   downed_subset$volume_cm3*0.15,ifelse(downed_subset$Decay_Class %in% c("5"),                          
   downed_subset$volume_cm3*0.11, downed_subset$volume_cm3<-0)))))

#Create a subset of data that was not identified and was standing deadwood.
standing_subset <- subset (all_deadwood,Species="NA" & Status =="S")

#Fill in the biomass column for standing deadwood that was not identified.
standing_subset$biomass = ifelse(standing_subset$Decay_Class %in% c("1"),
 standing_subset$volume_cm3*0.4,ifelse(standing_subset$Decay_Class %in% c("2"),
 standing_subset$volume_cm3*0.38,ifelse(standing_subset$Decay_Class %in% c("3"),
 standing_subset$volume_cm3*0.35,ifelse(standing_subset$Decay_Class %in% c("4","5"),
 standing_subset$volume_cm3*0.25, downed_subset$volume_cm3<-0))))

#Create a subset of stump data that was not identified. Note that no stumps in 
#dataset were identified.
stump_subset <- subset(all_deadwood, Species="NA" & Status =="stump")

#Fill in the biomass column for stump deadwood that was not identified.
#Average density values for standing deadwood were used below.
stump_subset$biomass = ifelse(stump_subset$Decay_Class %in% c("1"),
  stump_subset$volume_cm3*0.4,ifelse(stump_subset$Decay_Class %in% c("2"),
  stump_subset$volume_cm3*0.38,ifelse(stump_subset$Decay_Class %in% c("3"),
  stump_subset$volume_cm3*0.35,ifelse(stump_subset$Decay_Class %in% c("4","5"),
  stump_subset$volume_cm3*0.25, stump_subset$volume_cm3<-0))))

#Create a subset of data with no deadwood present in the subplots.
NA_subset <- subset(all_deadwood, Status=="NA")

#Fill in the biomass column for NA data.
NA_subset$biomass <- 0

#Join all data subsets back together.
deadwood_final <- rbind(softwood_subset1, softwood_subset2, hardwood_subset1,
    hardwood_subset2, downed_subset, standing_subset, stump_subset, NA_subset)

#multiply biomass by 0.5 to convert to carbon content
deadwood_final$carbon <- deadwood_final$biomass *0.5

#Combine columns describing Site ID & Subplot ID.
deadwood_final$Site_Subplot_ID <- paste(deadwood_final$Park_ID,
  deadwood_final$Site_ID,deadwood_final$Excl_Ctrl,
  deadwood_final$Subplot_ID, sep="_")

#Sum the carbon content of all deadwood within each transect.
carbon_deadwood <-aggregate(deadwood_final$carbon, list
                            (deadwood_final$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(carbon_deadwood) <- c("Site_Subplot_ID", "carbon_9m2")

write.xlsx(carbon_deadwood, file="carbon_deadwood.xlsx")
write.xlsx(total_dataset_final, file="deadwood_biomass.xlsx")

################################################################################
##Woody Shrubs, 30 cm - 3 m in height##

#Shrubs and saplings were separated according to their growth forms. Any species
#that typically is considered a tree, was classified as a sapling; all other species 
#were considered shrubs and were separated based on if they are woody or non-woody.
#Equations and parameters used varied depending on the species type and measurements
#that were collected. 

#Steps taken to calculate the biomass and carbon of woody shrubs between 30 cm 
#and 3 metres in height.

#load data
Saplings_Shrubs <- read_excel("Shrub & Sapling Biomass/Saplings & Shrubs.xlsx")

#Set columns as numeric.
Saplings_Shrubs$Height <- as.numeric(Saplings_Shrubs$Height)
Saplings_Shrubs$Diameter_A <-as.numeric(Saplings_Shrubs$Diameter_A)
Saplings_Shrubs$Diameter_B <-as.numeric(Saplings_Shrubs$Diameter_B)
Saplings_Shrubs$Percent_Cover <-as.numeric(Saplings_Shrubs$Percent_Cover)
Saplings_Shrubs$Basal_Diameter <-as.numeric(Saplings_Shrubs$Basal_Diameter)
Saplings_Shrubs$`# of individuals` <-as.numeric(Saplings_Shrubs$`# of individuals`)

#Combine columns describing Site & Subplot ID. Note that although shrubs, saplings
#and herbaceous vegetation were measured within 5m^2 transects, all data will be 
#extrapolated to and summed at the 9m^2 subplot level.
Saplings_Shrubs$Site_Subplot_ID <- paste(Saplings_Shrubs$Park_ID,
                  Saplings_Shrubs$Site_ID,Saplings_Shrubs$Excl_Ctrl,
                  Saplings_Shrubs$Subplot_ID, sep="_")

#Create a subset of blueberry and black huckleberry data.
hb_subset <- subset(Saplings_Shrubs,Species =="blueberry"|
                      Species =="black_huckleberry")

#Calculate area of cover of these species. Note that area of cover is divided by
#5 in future steps to convert to an area that would be covered in a 1 square meter 
#subplot, rather than a 5 square meter subplot, following the 
#CH method in Chen et al., 2008.
hb_subset$cover_area<-((hb_subset$Diameter_A/100)*
                         (hb_subset$Diameter_B/100)*hb_subset$`# of individuals`)

#Sum the area of cover of these species within each transect.
hb_sum <-aggregate(hb_subset$cover_area, list(hb_subset$Site_Subplot_ID), FUN=sum)

#Average the height of these species within each transect.
hb_mean <-aggregate(hb_subset$Height, list(hb_subset$Site_Subplot_ID), FUN=mean)

#Change column names.
colnames(hb_sum) <- c("Site_Subplot_ID", "cover_area") 
colnames(hb_mean) <- c("Site_Subplot_ID", "Height")

#Divide cover area by 5.
hb_sum$cover_area <- hb_sum$cover_area/5

#Merge dataframes.
hb_data <-merge(x=hb_sum, y=hb_mean,
                by="Site_Subplot_ID")

#Fill in biomass column using the CH method (see Table 2) in Chen et al., 2008.
#Note that biomass is calculated in g/m2.
hb_data$biomass<- (10543 *hb_data$cover_area*hb_data$Height)*5

#Create a subset of canada yew data.
canada_yew <- subset(Saplings_Shrubs,Species =="canada_yew")

#Calculate area of cover of canada yew data.
canada_yew$cover_area<-((canada_yew$Diameter_A/100)*(canada_yew$Diameter_B/100)*
                          canada_yew$`# of individuals`)

#Sum the area of cover within each transect.
cy_sum <-aggregate(canada_yew$cover_area, list(canada_yew$Site_Subplot_ID), FUN=sum)

#Average the height within each transect.
cy_mean <-aggregate(canada_yew$Height, list(canada_yew$Site_Subplot_ID), FUN=mean)

#Change column names.
colnames(cy_sum) <- c("Site_Subplot_ID", "cover_area") 
colnames(cy_mean) <- c("Site_Subplot_ID", "Height")

#Divide cover area by 5.
cy_sum$cover_area <- cy_sum$cover_area/5

#Merge dataframes.
cy_data <-merge(x=cy_sum, y=cy_mean,
                by="Site_Subplot_ID")

#Fill in biomass column using the CH method and parameters for crowberry (see Table 2) 
#in Chen et al., 2008.
cy_data$biomass<- (2699.5 *cy_data$cover_area*canada_yew$Height)*5

#Create a subset of labrador tea data.
labrador_tea <- subset(Saplings_Shrubs,Species =="labrador_tea")

#Calculate area of cover.
labrador_tea$cover_area<-((labrador_tea$Diameter_A/100)*(labrador_tea$Diameter_B/100)*
                            labrador_tea$`# of individuals`)

#Sum the area of cover within each transect.
lt_sum <-aggregate(labrador_tea$cover_area, list(labrador_tea$Site_Subplot_ID), FUN=sum)

#Average the height within each transect.
lt_mean <-aggregate(labrador_tea$Height, list(labrador_tea$Site_Subplot_ID), FUN=mean)

#Change column names.
colnames(lt_sum) <- c("Site_Subplot_ID", "cover_area") 
colnames(lt_mean) <- c("Site_Subplot_ID", "Height")

#Divide cover area by 5.
lt_sum$cover_area <- lt_sum$cover_area/5

#Merge dataframes.
lt_data <-merge(x=lt_sum, y=lt_mean,
                by="Site_Subplot_ID")

#Fill in biomass column using the CH method and parameters for labrador tea (see Table 2) 
#in Chen et al., 2008.
lt_data$biomass<- (1093 *lt_data$cover_area*lt_data$Height)*5

#Create a subset of deciduous shrub data (i.e. currant, wild raisin, and rhodora).
crw_subset <- subset(Saplings_Shrubs,Species =="currant"| Species =="wild_raisin"|
                       Species =="rhodora")

#Calculate area of cover.
crw_subset$cover_area<-((crw_subset$Diameter_A/100)*(crw_subset$Diameter_B/100)*
                          crw_subset$`# of individuals`)

#Sum the area of cover within each transect.
crw_sum <-aggregate(crw_subset$cover_area, list(crw_subset$Site_Subplot_ID), FUN=sum)

#Average the height within each transect.
crw_mean <-aggregate(crw_subset$Height, list(crw_subset$Site_Subplot_ID), FUN=mean)

#Change column names.
colnames(crw_sum) <- c("Site_Subplot_ID", "cover_area") 
colnames(crw_mean) <- c("Site_Subplot_ID", "Height")

#Divide cover area by 5.
crw_sum$cover_area <- crw_sum$cover_area/5

#Merge dataframes.
crw_data <-merge(x=crw_sum, y=crw_mean,
                 by="Site_Subplot_ID")

#Fill in biomass column using the CH method and parameters for deciduous shrubs (see Table 2) 
#in Chen et al., 2008.
crw_data$biomass<- (1197.1 *crw_data$cover_area*crw_data$Height)*5

#Create a subset of laurel data.
laurel <- subset(Saplings_Shrubs,Species =="laurel")

#Calculate area of cover.
laurel$cover_area<-((laurel$Diameter_A/100)*(laurel$Diameter_B/100)*
                      laurel$`# of individuals`)

#Sum the area of cover within each transect.
laurel_sum <-aggregate(laurel$cover_area, list(laurel$Site_Subplot_ID), FUN=sum)

#Average the height within each transect.
laurel_mean <-aggregate(laurel$Height, list(laurel$Site_Subplot_ID), FUN=mean)

#Change column names.
colnames(laurel_sum) <- c("Site_Subplot_ID", "cover_area") 
colnames(laurel_mean) <- c("Site_Subplot_ID", "Height")

#Divide cover area by 5.
laurel_sum$cover_area <- laurel_sum$cover_area/5

#Merge dataframes.
laurel_data <-merge(x=laurel_sum, y=laurel_mean,
                    by="Site_Subplot_ID")

#Fill in biomass column using the CH method and parameters for evergreen shrubs (see Table 2) 
#in Chen et al., 2008.
laurel_data$biomass<- (1216.4 *laurel_data$cover_area*laurel_data$Height)*5

#The biomass of the following species were calculated using equation 7 from
#Flade et al., 2020

#Create a subset of alder data.
alder_subset<- subset(Saplings_Shrubs,Species =="alder")

#Calculate the volume of alder data. Note that diameters (measured in cm) were
#converted to metres so that volume was calculated in cubic metres.
alder_subset$volume <-(alder_subset$Diameter_A/100)*(alder_subset$Diameter_B/100)*
  alder_subset$Height

#Fill in biomass column using equation 7 from Flade et al., 2020
alder_subset$biomass <- 185.0650 * (alder_subset$volume^0.9760)*
  alder_subset$`# of individuals`

alder_sum <-aggregate(alder_subset$biomass, list(alder_subset$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(alder_sum) <- c("Site_Subplot_ID", "biomass") 

#Create a subset of red elderberry data. Note that for three entries, cross-
#sectional diameters and height were measured, but for four others basal diameter
#and height were measured. I therefore used the volume equation from Flade et al., 2020 
#for the entries with cross-sectional diameters and used the equation and parameters
#for soapberry from Brown (1976) for the other entries.
elderberry_1 <- subset(Saplings_Shrubs,Species =="red_elderberry" & is.na(Basal_Diameter))
elderberry_2 <- subset(Saplings_Shrubs,Species =="red_elderberry" & is.na(Diameter_A))

#Calculate the volume of elderberry data with cross-sectional diameters. 
#Note that diameters (measured in cm) are converted to metres so that volume 
#is calculated in cubic metres.
elderberry_1$volume <-(elderberry_1$Diameter_A/100)*(elderberry_1$Diameter_B/100)*
  elderberry_1$Height

#Fill in biomass column using equation 7 and Sheperdia canadensis parameters
#from Flade et al., 2020.
elderberry_1$biomass <- 232.2120 * (elderberry_1$volume^0.6290)*elderberry_1$`# of individuals`

#Fill in biomass column using Sheperdia canadensis equation and parameters
#from Brown (1976).
elderberry_2$biomass <- 33.016 *(elderberry_2$Basal_Diameter^2.407)*elderberry_2$`# of individuals`

#Sum the biomass within each transect.
el_sum1 <-aggregate(elderberry_1$biomass, list(elderberry_1$Site_Subplot_ID), FUN=sum)
el_sum2 <-aggregate(elderberry_2$biomass, list(elderberry_2$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(el_sum1) <- c("Site_Subplot_ID", "biomass") 
colnames(el_sum2) <- c("Site_Subplot_ID", "biomass") 

#Create a subset of soapberry, mountain holly, pin cherry & serviceberry data.
#Note that because not all species had allometric equations that I could use with
#the measurements collected, I used equations for species with similar growth forms.
soapberry_subset <- subset(Saplings_Shrubs,Species =="soapberry" |Species =="mountain_holly"|
                             Species =="pin_cherry" | Species =="serviceberry")

#Calculate the volume of soapberry data. Note that diameters (measured in cm) were
#converted to metres so that volume was calculated in cubic metres.
soapberry_subset$volume <-(soapberry_subset$Diameter_A/100)*(soapberry_subset$Diameter_B/100)*
  soapberry_subset$Height

#Fill in biomass column using equation 7 from Flade et al., 2020
soapberry_subset$biomass <- 232.2120 * (soapberry_subset$volume^0.6290)*soapberry_subset$`# of individuals`

#Sum the biomass within each transect.
sb_sum <-aggregate(soapberry_subset$biomass, list(soapberry_subset$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(sb_sum) <- c("Site_Subplot_ID", "biomass") 

#Create a subset of willow data.
willow_subset <- subset(Saplings_Shrubs,Species =="willow")

#Calculate the volume of willow data. Note that diameters (measured in cm) were
#converted to metres so that volume was calculated in cubic metres.
willow_subset$volume <-(willow_subset$Diameter_A/100)*(willow_subset$Diameter_B/100)*
  willow_subset$Height

#Fill in biomass column using equation 7 from Flade et al., 2020
willow_subset$biomass <- 262.4690 * (willow_subset$volume^0.7850)*willow_subset$`# of individuals`

#Sum the biomass within each transect.
willow_sum <-aggregate(willow_subset$biomass, list(willow_subset$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(willow_sum) <- c("Site_Subplot_ID", "biomass") 

#Create a subset of creeping juniper data.Note that only one individual was
#recorded.
creeping_juniper<- subset(Saplings_Shrubs,Species =="creeping_juniper")

#Fill in biomass column using equation from Brown (1976).
creeping_juniper$biomass <-59.205*(creeping_juniper$Basal_Diameter^2.202)

#Sum the biomass within each transect.
cj_sum <-aggregate(creeping_juniper$biomass, list(creeping_juniper$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(cj_sum) <- c("Site_Subplot_ID", "biomass") 

#Create a subset of raspberries.
raspberry<- subset(Saplings_Shrubs,Species =="raspberry")

#Calculate percent cover of each recorded raspberry.
raspberry$Percent_Cover<- ((raspberry$Diameter_A*raspberry$Diameter_B)/50000)*100

#Fill in biomass column using equation from Guevara et al., 2021
raspberry$biomass <-0.005966*(raspberry$Percent_Cover^1.432098)

#Convert raspberry biomass from Mg/hectare to g/square meter
raspberry$biomass<- raspberry$biomass*100

#Sum the biomass of raspberries within each transect.
raspberry_sum <-aggregate(raspberry$biomass, list(raspberry$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(raspberry_sum) <- c("Site_Subplot_ID", "biomass")

#Join all data subsets back together. Note that datasets that were calculated
#differently have a different number of columns. I therefore joined datasets
#with the same number of columns first, dropped the extra columns, then joined 
#the rest together.
df1 <- rbind(crw_data, cy_data,hb_data, laurel_data, lt_data)
df2<-subset(df1, select = -c(cover_area, Height))
df3<-rbind(alder_sum, cj_sum, el_sum1, el_sum2, raspberry_sum, sb_sum, willow_sum, df2)

#Sum all calculated biomass within a transect.
woody_shrubs<- aggregate(df3$biomass, list(df3$Site_Subplot_ID), FUN=sum)

#Change column names
colnames(woody_shrubs) <- c("Site_Subplot_ID", "biomass_5m2")

#Size biomass to the subplot level (9 square meters in size), then calculate
#carbon content.
woody_shrubs$biomass_9m2<- woody_shrubs$biomass_5m2*9/5
woody_shrubs$carbon_9m2<-woody_shrubs$biomass_9m2*0.5

#Export data to excel.
write.xlsx(woody_shrubs, file="woody_shrubs_carbon.xlsx")

################################################################################
##Sapling carbon

#Steps taken to calculate the biomass and carbon of saplings between 30 cm 
#and 3 metres in height.

#Note that for several sapling records, cross-sectional diameters were recorded 
#rather than basal diameter. To fill in this data, I will create scatter plots 
#of the height and basal diameter data for these species. I will then use the 
#slope of these plots to calculate missing basal diameter values.

#Create a subset of saplings identified as ash with basal diameter measurements present.
ash_subset <- subset(Saplings_Shrubs, Species=="ash" & !is.na(Basal_Diameter))

#Create a scatter plot of ash basal diameter & height values and run a linear
#regression model of this data.Note that we forced the intercept through zero.
plot(ash_subset$Height,ash_subset$Basal_Diameter, xlab='Height (m)
     ',ylab='Basal Diameter (cm)')
ash_lm<- lm(Basal_Diameter~0+Height,data=ash_subset)

#Retrieve summary of linear regression model.
summary(ash_lm)

#Create a subset of ash data with missing basal diameter values.
ash_NA_subset <- subset(Saplings_Shrubs, Species=="mountain_ash" |Species== "ash" 
                        & is.na(Basal_Diameter))

#Use slope of linear regression to calculate missing ash basal diameter values 
#(i.e. multiplying it by the recorded height values)
ash_NA_subset$Basal_Diameter<- ash_NA_subset$Height*2.8159

#Create a subset of saplings identified as aspen with basal diameter measurements present.
aspen_subset <- subset(Saplings_Shrubs, Species=="aspen" & !is.na(Basal_Diameter)
                       |Species=="trembling_aspen" & !is.na(Basal_Diameter))

#Create a scatter plot of aspen basal diameter & height values and run a linear
#regression model of this data.Note that we forced the intercept through zero.
plot(aspen_subset$Height,aspen_subset$Basal_Diameter, xlab='Height (m)
     ',ylab='Basal Diameter (cm)')
aspen_lm<- lm(Basal_Diameter~0+Height,data=aspen_subset)

#Retrieve summary of linear regression model.
summary(aspen_lm)

#Create a subset of aspen data with missing basal diameter values.
aspen_NA_subset <- subset(Saplings_Shrubs, Species== "aspen" 
                          & is.na(Basal_Diameter))

#Use slope of linear regression to calculate missing aspen basal diameter values 
#(i.e. multiplying it by the recorded height values)
aspen_NA_subset$Basal_Diameter<- aspen_NA_subset$Height*0.9116

#Create a subset of saplings identified as fir with basal diameter measurements present.
bF_subset <- subset(Saplings_Shrubs, Species=="balsam_fir" & !is.na(Basal_Diameter))

#Create a scatter plot of fir basal diameter & height values and run a linear
#regression model of this data.Note that we forced the intercept through zero.
plot(bF_subset$Height,bF_subset$Basal_Diameter, xlab='Height (m)
     ',ylab='Basal Diameter (cm)')
bF_lm<- lm(Basal_Diameter~0+Height,data=bF_subset)

#Retrieve summary of linear regression model.
summary(bF_lm)

#Create a subset of fir data with missing basal diameter values.
bF_NA_subset <- subset(Saplings_Shrubs, Species== "balsam_fir" 
                       & is.na(Basal_Diameter))

#Use slope of linear regression to calculate missing fir basal diameter values 
#(i.e. multiplying it by the recorded height values)
bF_NA_subset$Basal_Diameter<- bF_NA_subset$Height*1.88522

#Create a subset of saplings identified as birch with basal diameter measurements present.
birch_subset <- subset(Saplings_Shrubs, Species=="birch" & !is.na(Basal_Diameter)
                       | Species=="heart_leaved_birch" & !is.na(Basal_Diameter)| Species=="white_birch" 
                       & !is.na(Basal_Diameter)| Species=="yellow_birch" & !is.na(Basal_Diameter))

#Create a scatter plot of birch basal diameter & height values and run a linear
#regression model of this data.Note that we forced the intercept through zero.
plot(birch_subset$Height,birch_subset$Basal_Diameter, xlab='Height (m)
     ',ylab='Basal Diameter (cm)')
birch_lm<- lm(Basal_Diameter~0+Height,data=birch_subset)

#Retrieve summary of linear regression model.
summary(birch_lm)

#Create a subset of birch data with missing basal diameter values.
birch_NA_subset <- subset(Saplings_Shrubs, Species== "birch" & is.na(Basal_Diameter)
                          | Species== "heart_leaved_birch" & is.na(Basal_Diameter))

#Use slope of linear regression to calculate missing birch basal diameter values 
#(i.e. multiplying it by the recorded height values)
birch_NA_subset$Basal_Diameter<- birch_NA_subset$Height*1.4294

#Create a subset of saplings identified as spruce with basal diameter measurements present.
spruce_subset <- subset(Saplings_Shrubs, Species=="black_spruce" & !is.na(Basal_Diameter))

#Create a scatter plot of spruce basal diameter & height values and run a linear
#regression model of this data.Note that we forced the intercept through zero.
plot(spruce_subset$Height,spruce_subset$Basal_Diameter, xlab='Height (m)
     ',ylab='Basal Diameter (cm)')
spruce_lm<- lm(Basal_Diameter~0+Height,data=spruce_subset)

#Retrieve summary of linear regression model.
summary(spruce_lm)

#Create a subset of spruce data with missing basal diameter values.
spruce_NA_subset <- subset(Saplings_Shrubs, Species== "black_spruce" & is.na(Basal_Diameter))

#Use slope of linear regression to calculate missing spruce basal diameter values 
#(i.e. multiplying it by the recorded height values)
spruce_NA_subset$Basal_Diameter<- spruce_NA_subset$Height*1.7053

#Create a subset of saplings identified as maple with basal diameter measurements present.
maple_subset <- subset(Saplings_Shrubs, Species=="maple" & !is.na(Basal_Diameter)
                       | Species=="mountain_maple" & !is.na(Basal_Diameter)| Species=="red_maple" 
                       & !is.na(Basal_Diameter))

#Create a scatter plot of maple basal diameter & height values and run a linear
#regression model of this data.Note that we forced the intercept through zero.
plot(maple_subset$Height,maple_subset$Basal_Diameter, xlab='Height (m)
     ',ylab='Basal Diameter (cm)')
maple_lm<- lm(Basal_Diameter~0+Height,data=maple_subset)

#Retrieve summary of linear regression model.
summary(maple_lm)

#Create a subset of maple data with missing basal diameter values.
maple_NA_subset <-subset(Saplings_Shrubs, Species=="maple" & is.na(Basal_Diameter))

#Use slope of linear regression to calculate missing maple basal diameter values 
#(i.e. multiplying it by the recorded height values)
maple_NA_subset$Basal_Diameter<- maple_NA_subset$Height*1.19944

#Create subsets of all other sapling data entries.
tamarack_subset <-subset(Saplings_Shrubs, Species=="tamarack")
pine_subset <-subset(Saplings_Shrubs, Species=="white_pine")

#Join all data subsets back together.
saplings_dataset <- rbind(ash_subset, ash_NA_subset, aspen_subset, aspen_NA_subset,
                          bF_subset,bF_NA_subset, birch_subset, birch_NA_subset, maple_subset, 
                          maple_NA_subset, spruce_subset, spruce_NA_subset, tamarack_subset,
                          pine_subset) 

#Calculate biomass in kg using equation from Conti et al., 2019.
#See Model 3 of Table 1 in reference.
saplings_dataset$ind_biomass<- exp(-2.869 + (2.584* log(saplings_dataset$Basal_Diameter)))
saplings_dataset$tot_biomass<- saplings_dataset$ind_biomass*saplings_dataset$`# of individuals`

#Change to g
saplings_dataset$biomass_g<- saplings_dataset$tot_biomass*1000

#Combine columns describing Site & Subplot ID. Note that although shrubs, saplings
#and herbaceous vegetation were measured within transects, all data will be summed
#at the subplot level (9m^2).
saplings_dataset$Site_Subplot_ID <- paste(saplings_dataset$Park_ID,
                                          saplings_dataset$Site_ID,saplings_dataset$Excl_Ctrl,
                                          saplings_dataset$Subplot_ID, sep="_")

#Sum biomass within each transect.
sapling_biomass <- saplings_dataset %>% group_by(Site_Subplot_ID, Park_ID, Site_ID, Excl_Ctrl,
                         Subplot_ID) %>% summarise(across(c(biomass_g),sum),
                         .groups = 'drop') %>%  as.data.frame()

#Size biomass to the subplot level (9 square meters in size), then calculate
#carbon content.
sapling_biomass$biomass_9m2<- sapling_biomass$biomass_g*9/5
sapling_biomass$carbon_9m2<-sapling_biomass$biomass_9m2*0.5

#Export data to excel.
write.xlsx(sapling_biomass, file="Sapling_carbon_new.xlsx")

################################################################################
##Herbaceous Vegetation, 30 cm to 3 m in height

#Steps taken to calculate the biomass and carbon of herbaceous vegetation
#between 30 cm and 3 m in height.

#The equations and parameters used for calculating the biomass of forbs, grasses, 
#and ferns are from the pooled data set (Table 3) in Guevara et al., 2021.

#Note that one goldenrod entry is missing cross-sectional diameter values.
#To fill in this missing data, I will calculate area of cover and create a
#scatter plot of this and height data for all other goldenrod entries. I will then 
#use the slope of these plots to calculate the missing value.

#Create a subset of goldenrod data.
goldenrod <- subset(Saplings_Shrubs, Species=="goldenrod")

#Multiply cross-sectional values to calculate the area of cover.
goldenrod$Area_of_Cover <- goldenrod$Diameter_A*goldenrod$Diameter_B

#Create a subset of goldenrod data entries with no missing data & goldenrod data 
#entries with missing value.
goldenrod_subset <- subset(goldenrod,!is.na(Area_of_Cover))
goldenrod_NA_subset <- subset(goldenrod, is.na(Area_of_Cover))

#Create a scatter plot of area of cover and height values and run a linear
#regression model of this data.Note that we forced the intercept through zero.
plot(goldenrod_subset$Height,goldenrod_subset$Area_of_Cover, xlab='Height (m)
     ',ylab='Area of cover (cm2)')
goldenrod_lm<- lm(Area_of_Cover~0+Height,data=goldenrod_subset)

#Retrieve summary of linear regression model.
summary(goldenrod_lm)

#Use slope of linear regression to calculate missing goldenrod area of cover value.
goldenrod_NA_subset$Area_of_Cover<- goldenrod_NA_subset$Height*459.53

#Join goldenrod data back together.
goldenrod<-rbind(goldenrod_NA_subset, goldenrod_subset)

#Subset non-woody vegetation from original dataset. Separate them into Guevara et al., 2021
#categories.#Note that although goldenrod is a forb, I am keeping it separate, due to the 
#previously missing cross-sectional diameter measurements.

ferns <- subset(Saplings_Shrubs, Species=="fern")
graminoids <- subset(Saplings_Shrubs, Species=="grass")
forbs <- subset(Saplings_Shrubs, Species=="fireweed" |Species=="strawberry" |
                  Species=="cow_parsnip" |Species=="wild_sarsaparilla")

#Subset data based on if percent cover was recorded in the field or needs to 
#be calculated.

ferns_PC <- subset(ferns, !is.na(Percent_Cover))
forbs_PC <- subset(forbs, !is.na(Percent_Cover))
ferns_NA <- subset(ferns, is.na(Percent_Cover))
forbs_NA <- subset(forbs, is.na(Percent_Cover))

#Calculate percent cover based on cross-sectional diameters.Note that only goldenrods 
#had more than one individual noted per entry, in the cases where percent cover needed to be 
#calculated. 
ferns_NA$Percent_Cover <- ((ferns_NA$Diameter_A*ferns_NA$Diameter_B)/50000) *100
forbs_NA$Percent_Cover <- ((forbs_NA$Diameter_A*forbs_NA$Diameter_B)/50000) *100 
goldenrod$Percent_Cover<-((goldenrod$Area_of_Cover/50000) *(goldenrod$`# of individuals`)) *100 


#Join datasets back together.Note that I removed the area of cover column from
#the goldenrod subset to do this.
goldenrod1<-subset(goldenrod, select = -c(Area_of_Cover))
ferns <- rbind(ferns_NA, ferns_PC) 
forbs <- rbind(forbs_NA, forbs_PC, goldenrod1) 

#Create custom function to calculate the biomass of forbs, graminoids, 
#and ferns. Note that x1 represents percent cover.
Forb_Biomass <-function(x1){0.014729*(x1^1.266369)}
Graminoid_Biomass <-function(x1){0.005639*(x1^1.502171)}
Fern_Biomass <-function(x1){0.023319*(x1^1.440794)}

#Calculate biomass using custom functions. 
forbs$biomass <- Forb_Biomass(forbs$Percent_Cover)
graminoids$biomass <- Graminoid_Biomass(graminoids$Percent_Cover)
ferns$biomass <- Fern_Biomass(ferns$Percent_Cover)

#Note that the equations calculate biomass in Mg/ hectare. As I will eventually 
#be summing all biomass at the subplot level (measured in square 
#meters), I will convert the calculated biomass of forbs, grasses, and ferns to 
#g/ square meter by mutliplying by 100.

forbs$biomass <-forbs$biomass*100
graminoids$biomass <-graminoids$biomass*100
ferns$biomass <- ferns$biomass*100

#Join all non-woody vegetation datasets back together.
Non_woody_veg <- rbind(forbs, graminoids, ferns) 

#Combine columns describing Site & Subplot ID. Note that although shrubs, saplings
#and herbaceous vegetation was measured within transect, all data will be summed
#at the subplot level (9m^2).
Non_woody_veg$Site_Subplot_ID <- paste(Non_woody_veg$Park_ID,
                                       Non_woody_veg$Site_ID,Non_woody_veg$Excl_Ctrl,
                                       Non_woody_veg$Subplot_ID, sep="_")

#Sum biomass within each transect.
Herbaceous_biomass <-aggregate(Non_woody_veg$biomass, list
                               (Non_woody_veg$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(Herbaceous_biomass) <- c("Site_Subplot_ID", "biomass_5m2")

#Size biomass to the subplot level (9 square meters in size), then calculate
#carbon content.
Herbaceous_biomass$biomass_9m2<- Herbaceous_biomass$biomass_5m2*9/5
Herbaceous_biomass$carbon_9m2<-Herbaceous_biomass$biomass_9m2*0.5

#Export data to excel.
write.xlsx(Herbaceous_biomass, file="Herbaceous_carbon.xlsx")
write.xlsx(Non_woody_veg, file="Herbaceous_data_final.xlsx")

################################################################################
##Woody shrubs under 30 cm in height

#load data
Woody_Vegetation_Data <- read_excel("Ground Vegetation Biomass/Woody_Vegetation_Data.xlsx")

#Create a column describing Site ID, Subplot ID, Quadrat ID 
Woody_Vegetation_Data$Site_Quadrat_ID <- paste(Woody_Vegetation_Data$Park_ID,
                                               Woody_Vegetation_Data$Site_ID,Woody_Vegetation_Data$Excl_Ctrl,
                                               Woody_Vegetation_Data$Subplot_ID, Woody_Vegetation_Data$Quadrat_ID, sep="_")
Woody_Vegetation_Data$Site_Subplot_ID <- paste(Woody_Vegetation_Data$Park_ID,
                                               Woody_Vegetation_Data$Site_ID,Woody_Vegetation_Data$Excl_Ctrl,
                                               Woody_Vegetation_Data$Subplot_ID, sep="_")

#Set columns as numeric.
Woody_Vegetation_Data$Height <- as.numeric(Woody_Vegetation_Data$Height)
Woody_Vegetation_Data$Diameter_A <-as.numeric(Woody_Vegetation_Data$Diameter_A)
Woody_Vegetation_Data$Diameter_B <-as.numeric(Woody_Vegetation_Data$Diameter_B)
Woody_Vegetation_Data$Number_of_Individuals<-as.numeric(Woody_Vegetation_Data$Number_of_Individuals)

#subset data based on equation that will be used
evergreen_shrubs<- subset(Woody_Vegetation_Data, Species== "balsam_fir" |
                            Species== "spruce"| Species =="kalmia")
deciduous_shrubs<- subset(Woody_Vegetation_Data, Species== "mountain_maple"|
                            Species=="dogwood"|Species=="birch"|Species== "wild_raisin"|
                            Species=="ash"|Species=="red_maple"|Species=="mountain_holly"|Species=="bush_honeysuckle"|
                            Species=="serviceberry"|Species=="alder" |Species=="maple" |Species=="unknown")
blueberry<- subset(Woody_Vegetation_Data, Species=="blueberry")
labrador_tea<- subset(Woody_Vegetation_Data, Species=="labrador_tea")

#subset NA data & herbaceous species
NA_subset<- subset(Woody_Vegetation_Data, Species =="NA"|Species=="sage"|
                     Species=="horsetail"|Species=="wild_sarsaparilla")

#Fill in NA data with zeroes
NA_subset$biomass = 0

#Calculate area of cover of evergreen shrubs.
evergreen_shrubs$cover_area<-((evergreen_shrubs$Diameter_A/100)*(evergreen_shrubs$Diameter_B/100)*
                                evergreen_shrubs$Number_of_Individuals)

#Sum the area of cover within each quadrat.
evergreen_sum <-aggregate(evergreen_shrubs$cover_area, list(evergreen_shrubs$Site_Quadrat_ID), FUN=sum)

#Average the height within each quadrat.
evergreen_mean <-aggregate(evergreen_shrubs$Height, list(evergreen_shrubs$Site_Quadrat_ID), FUN=mean)

#Change column names.
colnames(evergreen_sum) <- c("Site_Quadrat_ID", "cover_area") 
colnames(evergreen_mean) <- c("Site_Quadrat_ID", "Height")

#Change height of mean height to metres.
evergreen_mean$Height<-evergreen_mean$Height/100

#Multiply cover area by 4 to extrapolate data to the level of 1m^2.
evergreen_sum$cover_area <- evergreen_sum$cover_area*4

#Merge dataframes.
evergreen_data <-merge(x=evergreen_sum, y=evergreen_mean,
                       by="Site_Quadrat_ID")

#Fill in biomass column using the CH method and parameters for evergreen shrubs (see Table 2) 
#in Chen et al., 2009.
evergreen_data$biomass<- (1216.4 *evergreen_data$cover_area*evergreen_data$Height)

#Calculate area of cover of deciduous shrubs.
deciduous_shrubs$cover_area<-((deciduous_shrubs$Diameter_A/100)*(deciduous_shrubs$Diameter_B/100)*
                                deciduous_shrubs$Number_of_Individuals)

#Sum the area of cover within each quadrat.
deciduous_sum <-aggregate(deciduous_shrubs$cover_area, list(deciduous_shrubs$Site_Quadrat_ID), FUN=sum)

#Average the height within each quadrat.
deciduous_mean <-aggregate(deciduous_shrubs$Height, list(deciduous_shrubs$Site_Quadrat_ID), FUN=mean)

#Change column names.
colnames(deciduous_sum)<- c("Site_Quadrat_ID", "cover_area")
colnames(deciduous_mean) <- c("Site_Quadrat_ID", "Height")

#Change height of mean height to metres.
deciduous_mean$Height<-deciduous_mean$Height/100

#Multiply cover area by 4.
deciduous_sum$cover_area<- deciduous_sum$cover_area*4 

#Merge dataframes.
deciduous_data <-merge(x=deciduous_sum, y=deciduous_mean,
                       by="Site_Quadrat_ID")

#Fill in biomass column using the CH method and parameters for deciduous shrubs (see Table 2) 
#in Chen et al., 2009.
deciduous_data$biomass<- (1197.1 * deciduous_data$cover_area*deciduous_data$Height)

#Calculate area of cover of blueberry plants.
blueberry$cover_area<-((blueberry$Diameter_A/100)*(blueberry$Diameter_B/100)*
                         blueberry$Number_of_Individuals)

#Sum the area of cover within each quadrat.
blueberry_sum <-aggregate(blueberry$cover_area, list(blueberry$Site_Quadrat_ID), FUN=sum)

#Average the height within each quadrat.
blueberry_mean <-aggregate(blueberry$Height, list(blueberry$Site_Quadrat_ID), FUN=mean)

#Change column names.
colnames(blueberry_sum) <- c("Site_Quadrat_ID", "cover_area") 
colnames(blueberry_mean) <- c("Site_Quadrat_ID", "Height")

#Change height of mean height to metres.
blueberry_mean$Height<-blueberry_mean$Height/100

#Multiply cover area by 4.
blueberry_sum$cover_area <- blueberry_sum$cover_area*4

#Merge dataframes.
blueberry_data <-merge(x=blueberry_sum, y=blueberry_mean,
                       by="Site_Quadrat_ID")

#Fill in biomass column using the CH method and parameters for blueberries (see Table 2) 
#in Chen et al., 2009.
blueberry_data$biomass<- (10543 *blueberry_data$cover_area*blueberry_data$Height)

#Calculate area of cover of labrador tea.
labrador_tea$cover_area<-((labrador_tea$Diameter_A/100)*(labrador_tea$Diameter_B/100)*
                            labrador_tea$Number_of_Individuals)

#Sum the area of cover within each transect.
lt_sum <-aggregate(labrador_tea$cover_area, list(labrador_tea$Site_Quadrat_ID), FUN=sum)

#Average the height within each transect.
lt_mean <-aggregate(labrador_tea$Height, list(labrador_tea$Site_Quadrat_ID), FUN=mean)

#Change column names.
colnames(lt_sum) <- c("Site_Quadrat_ID", "cover_area") 
colnames(lt_mean) <- c("Site_Quadrat_ID", "Height")

#Change height of mean height to metres.
lt_mean$Height<-lt_mean$Height/100

#Multiply cover area by 4.
lt_sum$cover_area <- lt_sum$cover_area*4

#Merge dataframes.
lt_data <-merge(x=lt_sum, y=lt_mean,
                by="Site_Quadrat_ID")

#Fill in biomass column using the CH method and parameters for labrador tea (see Table 2) 
#in Chen et al., 2009.
lt_data$biomass<- (1093 *lt_data$cover_area*lt_data$Height)

#Merge datasets together.
df1 <- rbind(blueberry_data, deciduous_data,evergreen_data,lt_data)
df1 <-subset(df1, select = -c(cover_area, Height))

#Sum biomass within each quadrat.
df1_sum <-aggregate(df1$biomass, list(df1$Site_Quadrat_ID), FUN=sum)
colnames(df1_sum) <- c("Site_Quadrat_ID", "biomass")

#Merge rows of biomass data with rows of NA data.
df2<-subset(NA_subset, select = c(Site_Quadrat_ID, biomass))
df3<- rbind(df1_sum, df2)
df3_sum <-aggregate(df3$biomass, list(df3$Site_Quadrat_ID), FUN=sum)
colnames(df3_sum) <- c("Site_Quadrat_ID", "biomass")

#Pull columns from original dataset describing site and subplot information.
subplot_ID <- subset(Woody_Vegetation_Data, select = c(Site_Quadrat_ID, Site_Subplot_ID))

#Merge site and subplot information with biomass data.
woody_veg <-merge(x=df3_sum, y=subplot_ID,
                   by="Site_Quadrat_ID")

#Sum the biomass of the four quadrats within each subplot.
woody_veg_biomass<-aggregate(woody_veg$biomass, list(woody_veg$Site_Subplot_ID), FUN=sum)
colnames(woody_veg_biomass) <- c("Site_Subplot_ID", "biomass_4m2")

#Multiply by 9/4 to get biomass at the 3 x 3 m^2 subplot level.Note that biomass
#was calculated in g/m^2.
woody_veg_biomass$biomass_9m2<- woody_veg_biomass$biomass_4m2*9/4

#Multiply by 0.5 to calculate carbon content within each subplot.
woody_veg_biomass$carbon_9m2<- woody_veg_biomass$biomass_9m2*0.5

#Export data to excel.
write.xlsx(woody_veg_biomass, file="under_30_biomass.xlsx")

###############################################################################
##Ground Vegetation (less than 30 cm in height)

#The equations and parameters used for calculating the biomass of forbs, grasses, 
#brambles and ferns are from the pooled data set (Table 3) in Guevara et al., 2021.
#The equation used for calculating the biomass of lichens and mosses
#is from Figure 2 in MacDonald et al., 2012. Note that, this equation was
#developed to calculate biomass of bryoids (lichens & mosses).I will therefore
#first sum the percent cover of lichens and mosses to calculate the total 
#percent cover of bryoids, then calculate their biomass.

#load data
Ground_Cover_Data <- read_excel("Ground Vegetation Biomass/Ground_Cover_Data.xlsx")

#Set columns as numeric.
Ground_Cover_Data$Forb <- as.numeric(Ground_Cover_Data$Forb)
Ground_Cover_Data$Grass<-as.numeric(Ground_Cover_Data$Grass)
Ground_Cover_Data$Fern<-as.numeric(Ground_Cover_Data$Fern)
Ground_Cover_Data$Bramble<-as.numeric(Ground_Cover_Data$Bramble)
Ground_Cover_Data$Lichen<-as.numeric(Ground_Cover_Data$Lichen)
Ground_Cover_Data$Moss<-as.numeric(Ground_Cover_Data$Moss)

#Fill in blank bryoid column by summing the percent cover of lichens and mosses.
Ground_Cover_Data$Bryoids <- (Ground_Cover_Data$Lichen+Ground_Cover_Data$Moss)

#Create custom function to calculate the biomass of bryoids, forbs, grasses, 
#brambles and ferns.Note that for each function x1 = the recorded percent cover of 
#that category.
Forb_Biomass <-function(x1){0.014729*(x1^1.266369)}
Grass_Biomass <-function(x1){0.005639*(x1^1.502171)}
Bramble_Biomass <-function(x1){0.005966*(x1^1.432098)}
Fern_Biomass <-function(x1){0.023319*(x1^1.440794)}
Bryoid_Biomass <-function(x1){5.534*x1}

#Calculate biomass using custom functions. 
Ground_Cover_Data$Forb_Biomass <- Forb_Biomass(Ground_Cover_Data$Forb)
Ground_Cover_Data$Grass_Biomass <- Grass_Biomass(Ground_Cover_Data$Grass)
Ground_Cover_Data$Bramble_Biomass <- Bramble_Biomass(Ground_Cover_Data$Bramble)
Ground_Cover_Data$Fern_Biomass <- Fern_Biomass(Ground_Cover_Data$Fern)
Ground_Cover_Data$Bryoid_Biomass <-Bryoid_Biomass(Ground_Cover_Data$Bryoids)

#Note that the bryoid equation provides biomass in g/ square meter, while all
#other equations provide biomass in Mg/ hectare. As I will be summing all biomass
#at the subplot level (measured in square meters), I will convert the biomass of 
#forbs, grasses, brambles and ferns to g/ square meter; to do this I used a 
#conversion factor of 1000000 grams/Mg *#1 hectare/ 10000m^2 (i.e. 100).
Ground_Cover_Data$Forb_Biomass <-Ground_Cover_Data$Forb_Biomass*100
Ground_Cover_Data$Grass_Biomass <-Ground_Cover_Data$Grass_Biomass*100
Ground_Cover_Data$Bramble_Biomass <- Ground_Cover_Data$Bramble_Biomass*100
Ground_Cover_Data$Fern_Biomass <- Ground_Cover_Data$Fern_Biomass*100

#Sum the biomass of all ground vegetation forms per quadrat.
Ground_Cover_Data$Total_Veg_Biomass <- (Ground_Cover_Data$Forb_Biomass+
                                          Ground_Cover_Data$Grass_Biomass+Ground_Cover_Data$Bramble_Biomass+
                                          Ground_Cover_Data$Fern_Biomass+Ground_Cover_Data$Bryoid_Biomass)

#Combine columns describing Site & Subplot ID
Ground_Cover_Data$Site_Subplot_ID <- paste(Ground_Cover_Data$Park_ID,
                                           Ground_Cover_Data$Site_ID,Ground_Cover_Data$Excl_Ctrl,
                                           Ground_Cover_Data$Subplot_ID, sep="_")

#Sum biomass of quadrats according to their subplot.
groundveg_biomass <-aggregate(Ground_Cover_Data$Total_Veg_Biomass, list
                        (Ground_Cover_Data$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(groundveg_biomass) <- c("Site_Subplot_ID", "biomass") 

#Extrapolate the summed biomass to the size of the 3 x 3 m subplot, 
#then calculate the corresponding carbon content. Note that I am extrapolating 
#data from a 4m^2 area (summed from four biomass values in g/m^2) to a 9m^2 area.
groundveg_biomass$Total_Biomass_9m2 <- groundveg_biomass$biomass*(9/4)
groundveg_biomass$Total_Carbon_9m2<-(groundveg_biomass$Total_Biomass_9m2*0.5)

write.xlsx(Sum_biomass, file="Ground_Cover_carbon.xlsx")

################################################################################
##Litter Data

#The following script is used to calculate the total amount of carbon in the 
#litter pool within each 3 x 3 m subplot. Within each subplot, litter was collected
#in two 0.5 x 0.5 m quadrats.

#load data
Litter_Samples <- read_excel("Litter_Data/Litter_Samples.xlsx")

#Set column as numeric.
Litter_Samples$Total_Carbon <-as.numeric(Litter_Samples$Total_Carbon)

#Calculate the amount (grams) of carbon within each total sample, by converting
#percent carbon of the sub-sample that was analyzed to a decimal, then multiplying 
#it by the dry weight of the total sample collected.
Litter_Samples$Total_Carbon_Weight <- Litter_Samples$Dry_Wt*
  (Litter_Samples$Total_Carbon/100)

#Extrapolate the amount of carbon collected within the two 0.5 x 0.5 m quadrats
#(representing a total area of 0.5 m^2) to the size of the 3 x 3 m subplot.
Litter_Samples$Carbon_9m2 <- Litter_Samples$Total_Carbon_Weight*18

#Combine columns containing site and subplot information
Litter_Samples$Site_Subplot_ID <- paste(Litter_Samples$Park_ID,
                                        Litter_Samples$Site_ID,Litter_Samples$Ex_Ctrl,
                                        Litter_Samples$Subplot_ID, sep="_")
#Drop columns
Litter_data<- subset(Litter_Samples, select = -c(1:16) )

#Export final dataset.
write.xlsx(Litter_data, file="Litter_Carbon.xlsx")

################################################################################
##Soil Data

#The following script is used to calculate the total amount of carbon in the 
#soil pool within each 3 x 3 m subplot. Within each subplot, soil cores were collected
#using a brass cylinder in the Northwest and Southwest 0.5 x 0.5 m quadrats. Note that
#there is one subplot, where data from one quadrat will be deleted, as we did not process
#that sample due to its composition (GM-04-C2).

#load data
Soil_Samples_MUN <- read_excel("Soil_Samples_MUN.xlsx")
Soil_Samples <- read_excel("Soil_Samples.xlsx")

#Set columns as numeric.
Soil_Samples$Core_Length <-as.numeric(Soil_Samples$Core_Length)
Soil_Samples$`Wt_of_Org_&_Soil` <-as.numeric(Soil_Samples$`Wt_of_Org_&_Soil`)

#Combine columns with site ID information
Soil_Samples$Site_Subplot_ID <- paste(Soil_Samples$Park_ID,
                                      Soil_Samples$Site_ID,Soil_Samples$Ex_Ctrl,
                                      Soil_Samples$`Subplot ID`, sep="_")
Soil_Samples_MUN$Site_Subplot_ID <- paste(Soil_Samples_MUN$Park_ID,
                                     Soil_Samples_MUN$Site_ID,Soil_Samples_MUN$Ex_Ctrl,
                                    Soil_Samples_MUN$`Subplot_ID`, sep="_")

#Drop row containing data for GM-04-C2 (sample was discarded).
soil<-Soil_Samples[-c(32),]

#Sum core weights based on the subplot they were collected within. Note that organic
#material (ex. fine roots) was kept in the sample for carbon analysis. Therefore
#the weight used below, is the weight of the soil and organic material.
sum_core_weights <-aggregate(soil$`Wt_of_Org_&_Soil`, list
                             (soil$Site_Subplot_ID), FUN=sum)

#Note that 2 values where the core length seemed to have been recorded inaccurately
#were changed to be the same as the length of the core that was collected in the 
#same subplot (TN-32-C2 & TN-32-C9).
soil[331,8]<-7.5
soil[334,8]<-4.7

#Change column names.
colnames(sum_core_weights) <- c("Site_Subplot_ID", "Weight")

#Join dataframes together
Soil_data <-merge(x=Soil_Samples_MUN, y=sum_core_weights,
                  by="Site_Subplot_ID")

#Set column as numeric
Soil_data$'% Carbon' <-as.numeric(Soil_data$'% Carbon')

#Calculate the amount (grams) of carbon within each total sample, by converting
#percent carbon of the sub-sample to a decimal, then multiplying it by the total
#dry weight of the sample collected. Note that this is the summed dry weights of 
#two soil cores collected within the same subplot.
Soil_data$Total_Carbon_Weight <- Soil_data$Weight*
  (Soil_data$'% Carbon'/100)

#Calculate the amount of carbon (grams) within each 3 x 3 m subplot, by dividing
#the amount of carbon within the total weight of the soil samples by the area
#of two soil cores (in m^2; soil core had a radius of 0.75 inches) and multiplying 
#it by the size of the subplot (9 m^2).
#Subset GM-04-B1, as this subplot only had one soil core analyzed and the
#sizing up process will therefore be different.
one_core<- subset(Soil_data, Site_Subplot_ID == "GM_4_NA_B1")
two_cores<- subset(Soil_data, Site_Subplot_ID != "GM_4_NA_B1")
one_core$Carbon_9m2<- (one_core$Total_Carbon_Weight*9)/((0.01905^2)*pi)
two_cores$Carbon_9m2<- (two_cores$Total_Carbon_Weight*9)/(2*((0.01905^2)*pi))

#Join dataframes together
soil_final<-rbind(x=one_core, y=two_cores)

#Export data to excel.
write.xlsx(soil_final, file="Soil_carbon2.xlsx")

################################################################################
##Tree and shrub roots

#The following script will be used to calculate the root biomass/ carbon content
#of all trees that were recorded. These calculations will be based on their 
#calculated aboveground biomass. The equations used below are from Li et al., 2003.

#Create subsets of hardwood and softwood trees. 
hardwoods <- subset(all_trees, Species=="mountain_ash"| Species =="alder"
                    | Species =="trembling_aspen" |Species =="pin_cherry" | Species =="mountain_maple"
                    | Species =="red_maple" | Species =="maple" | Species =="white_birch"
                    | Species =="yellow_birch")
softwoods <- subset(all_trees, Species=="balsam_fir" 
                    | Species =="black_spruce")
NA_subset<- subset(all_trees, Species=="NA")

#Sum the softwood and hardwood biomass within each transect, then divide
#by 5 to calculate the biomass per square meter.
softwood_biomass <-aggregate(softwoods$tree_biomass, list
                             (softwoods$Site_Subplot_ID), FUN=sum)
colnames(softwood_biomass) <- c("Site_Subplot_ID", "biomass_5m2")
softwood_biomass$biomass_m2<-softwood_biomass$biomass_5m2/5
hardwood_biomass <-aggregate(hardwoods$tree_biomass, list
                             (hardwoods$Site_Subplot_ID), FUN=sum)
colnames(hardwood_biomass) <- c("Site_Subplot_ID", "biomass_5m2")
hardwood_biomass$biomass_m2<-hardwood_biomass$biomass_5m2/5

#Note that tree biomass is in Kilograms/m2, but the following equations
#require it to first be converted to megagrams/hectare. 
#Calculate the root biomass and carbon content of softwoods.
softwood_biomass$biomass_MG_H<- softwood_biomass$biomass_m2*10
softwood_biomass$root_biomass <- 0.222*softwood_biomass$biomass_MG_H

#Convert root biomass back to kilograms/m2.
softwood_biomass$root_biomass<- softwood_biomass$root_biomass/10

#Calculate the root biomass and carbon content of hardwoods.
hardwood_biomass$biomass_MG_H<- hardwood_biomass$biomass_m2*10
hardwood_biomass$root_biomass <- 1.576*(hardwood_biomass$biomass_MG_H^0.615)

#Convert root biomass back to kilograms/m2.
hardwood_biomass$root_biomass<- hardwood_biomass$root_biomass/10

#Join datasets back together.
tree_roots_data <- rbind(softwood_biomass,hardwood_biomass) 

#Sum the root biomass within each transect.
tree_roots <-aggregate(tree_roots_data$root_biomass, list
                       (tree_roots_data$Site_Subplot_ID), FUN=sum)

#Change column names.
colnames(tree_roots) <- c("Site_Subplot_ID", "biomass_m2")

#Size up root biomass from Kg/m2 to the 9m2 subplot. Then calculate
#carbon content.
tree_roots$biomass_9m2<-tree_roots$biomass_m2*(9)
tree_roots$carbon_9m2<- tree_roots$biomass_9m2*0.5

#Export final dataset to Excel file.
write.xlsx(tree_roots, file="tree_root_carbon.xlsx")

#Drop columns and rename columns from herbaceous vegetation, saplings, 
#woody shrubs, and woody shrubs under 30 cm tall datasets.
Herbaceous <-subset(Herbaceous_biomass, select = -c(biomass_5m2, carbon_9m2))
colnames(Herbaceous) <- c("Site_Subplot_ID", "herb_biomass_9m2")
Woody <-subset(woody_shrubs, select = -c(biomass_5m2, carbon_9m2))
colnames(Woody) <- c("Site_Subplot_ID", "woody_biomass_9m2")
Under_30 <-subset(woody_veg_biomass, select = -c(biomass_4m2, carbon_9m2))
colnames(Under_30) <- c("Site_Subplot_ID", "under_30_biomass_9m2")
Saplings <-subset(sapling_biomass, select = c(Site_Subplot_ID, biomass_9m2))
colnames(Saplings) <- c("Site_Subplot_ID", "sapling_biomass_9m2")

#Join all datasets together.
df1<- merge(x = Herbaceous, y = Woody, all = TRUE)
df2<- merge(x = df1, y = Saplings, all = TRUE)
all_shrubs<- merge(x = df2, y = Under_30, all = TRUE)

#Set NA values to zero.
all_shrubs[is.na(all_shrubs)] = 0

#Sum the biomass of all shrubs within each subplot.
all_shrubs_sum  <-  rowSums(all_shrubs[ , 2 : 5])
final_shrubs  <-  cbind(all_shrubs_sum, all_shrubs)

#Calculate the biomass, then carbon content, of shrub roots. Following 
#Coomes et al., (2002), I will calculate the biomass of shrub roots as 
#25% of the calculated aboveground shrub biomass; this will then be converted
#to carbon content assuming 50% carbon content.
final_shrubs$root_biomass<- final_shrubs$all_shrubs_sum *0.25
final_shrubs$root_carbon<- final_shrubs$root_biomass*0.5

#Drop extra columns, to keep just root biomass, root carbon content, and 
#site information.
shrub_roots <-subset(final_shrubs, select = c(Site_Subplot_ID, root_biomass, root_carbon))

#Export final dataset to Excel file.
write.xlsx(shrub_roots, file="shrub_root_carbon.xlsx")

###############################################################################
##Merge all dataframes together

#Drop columns
carbon_trees<- subset(carbon_trees, select = -c(2) )
groundcover_carbon<- subset(groundveg_biomass, select = -c(2,3) )
herbaceous_carbon<- subset(Herbaceous_biomass, select = -c(2,3) )
sapling_carbon<- subset(sapling_biomass, select = -c(2:7) )
shrub_roots_carbon<- subset(shrub_roots, select = -c(2) )
soil_carbon<- subset(soil_final, select = -c(2:12) )
tree_roots_carbon<- subset(tree_roots, select = -c(2,3) )
under_30_carbon<- subset(woody_veg_biomass, select = -c(2,3) )
woody_shrubs_carbon<- subset(woody_shrubs, select = -c(2,3) )

#Change column names
colnames(carbon_deadwood) <- c("Site_Subplot_ID", "Deadwood_carbon")
colnames(carbon_trees) <- c("Site_Subplot_ID", "Tree_carbon")
colnames(groundcover_carbon) <- c("Site_Subplot_ID", "Ground_Cover_carbon")
colnames(herbaceous_carbon) <- c("Site_Subplot_ID", "Herbaceous_carbon")
colnames(Litter_data) <- c("Litter_carbon", "Site_Subplot_ID")
colnames(sapling_carbon) <- c("Site_Subplot_ID", "Sapling_carbon")
colnames(shrub_roots_carbon) <- c("Site_Subplot_ID", "Shrub_Root_carbon")
colnames(soil_carbon) <- c("Site_Subplot_ID", "Soil_carbon")
colnames(tree_roots_carbon) <- c("Site_Subplot_ID", "Tree_root_carbon")
colnames(under_30_carbon) <- c("Site_Subplot_ID", "Under_30_carbon")
colnames(woody_shrubs_carbon) <- c("Site_Subplot_ID", "Woody_shrubs_carbon")

#Note that all carbon measurements are in grams, except for trees and tree roots.
#I will convert these measurements to grams.
carbon_trees$Tree_carbon<- carbon_trees$Tree_carbon*1000
tree_roots_carbon$Tree_root_carbon<- tree_roots_carbon$Tree_root_carbon*1000


