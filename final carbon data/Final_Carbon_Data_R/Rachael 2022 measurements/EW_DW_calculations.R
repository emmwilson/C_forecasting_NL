##Deadwood Carbon

##The equations used for calculating the biomass in deadwood are from Richardson et al., 2009
##(see equations 1 & 6). The values used for converting volume to biomass are from 
##Tables 4 & 5 in Harmon et al., 2011. Note that if deadwood was identified, 
##I used the hardwood or softwood values in Table 5; otherwise, I used the values 
##for all species found in Table 4.

##See here for Harmon et al.,2011 paper: https://permanent.fdlp.gov/gpo14619/rp-nrs15.pdf
##See here for Richardson et al., 2009 paper: https://www.academia.edu/770677/Deadwood_in_New_Zealands_indigenous_forests

##load Deadwood_Field_Data spreadsheet
Deadwood_Field_Data <- read_excel("Final_Carbon_Data_R/Rachael 2022 measurements/Deadwood_Field_Data.xlsx")

##set columns as numeric
Deadwood_Field_Data$Length <- as.numeric(Deadwood_Field_Data$Length)
Deadwood_Field_Data$Height <- as.numeric(Deadwood_Field_Data$Height)
Deadwood_Field_Data$Diameter_A <-as.numeric(Deadwood_Field_Data$Diameter_A)
Deadwood_Field_Data$Diameter_B <-as.numeric(Deadwood_Field_Data$Diameter_B)
Deadwood_Field_Data$Diameter_C <-as.numeric(Deadwood_Field_Data$Diameter_C)
Deadwood_Field_Data$Diameter_D <-as.numeric(Deadwood_Field_Data$Diameter_D)

##create custom function to calculate the volume of standing deadwood
standing_volume <- function(dbh,h){0.0000598 * (dbh^2 * h)^0.946*(1-0.0019*dbh)}

##create custom function to calculate the volume of stumps
stump_volume <- function(len,x1,x2) {(pi*len/32)*((x1+x2)^2+
                                                    (x1+x2)^2)}

##create custom function to calculate the volume of fallen logs
log_volume <- function(len,x1,x2,x3,x4) {(pi*len/32)*((x1+x2)^2+
                                                        (x3+x4)^2)}

##Create a subset of stump data
stump_subset <- subset(Deadwood_Field_Data, Status=="stump")

##Use stump volume function to fill the volume column for stump subset. 
##Note that for stump entries,the height column was filled with the recorded length.
##Diameter measurements were recorded in cm and converted to m.
stump_subset$Volume <- stump_volume(stump_subset$Height,
                                    stump_subset$Diameter_A/100,stump_subset$Diameter_B/100)

##Create a subset of fallen log data with all diameter measurements
log_subset1 <- subset(Deadwood_Field_Data, Status=="F" & !is.na(Diameter_C))

##Use log volume function to fill the volume column for log subset.
##Diameter measurements were recorded in cm and converted to m.
log_subset1$Volume <- log_volume(log_subset1$Length,
                                 log_subset1$Diameter_A/100,log_subset1$Diameter_B/100, log_subset1$Diameter_C/100,
                                 log_subset1$Diameter_D/100)

##Create a subset of fallen log data with just 2 diameter measurements.
log_subset2 <- subset(Deadwood_Field_Data, Status=="F" & is.na(Diameter_C))

##Use stump volume function to fill the volume column for logs with missing 
##diameter measurements.
##Diameter measurements were recorded in cm and converted to m.
log_subset2$Volume <- stump_volume(log_subset2$Length,
                                   log_subset2$Diameter_A/100,log_subset2$Diameter_B/100)

##Create a subset of standing deadwood data
standing_subset <- subset(Deadwood_Field_Data, Status=="S")

##use standing volume function to fill the volume column for standing
##deadwood
standing_subset$Volume <- standing_volume(standing_subset$Diameter_A,
                                          standing_subset$Height)

##create a subset of data with no deadwood present in subplots
NA_subset <- subset(Deadwood_Field_Data, Status=="NA")

##Fill in volume column for NA subset.
NA_subset$Volume <- 0

##Join all data subsets back together.
all_deadwood <- rbind(log_subset1, log_subset2, NA_subset, standing_subset,
                      stump_subset) 

##Convert all volume values from cubic metres to cubic centimetres.
all_deadwood$volume_cm3 <- (all_deadwood$Volume * 1000000)

##Create a subset of the data that was identified as downed balsam_fir or spruce.
##Note that all spruce entries are downed deadwood.
softwood_subset1 <-subset(all_deadwood, Species=="balsam_fir" & Status == "F"|
                            Species=="spruce")

##Fill in the biomass column for deadwood that was identified as downed softwood.
##Note that none of these entries were recorded as decay class 3.
softwood_subset1$biomass = ifelse(softwood_subset1$Decay_Class %in% c("1"),
                                  softwood_subset1$volume_cm3*0.38,ifelse(softwood_subset1$Decay_Class %in% c("2"),
                                                                          softwood_subset1$volume_cm3*0.34,ifelse(softwood_subset1$Decay_Class %in% c("4"),
                                                                                                                  softwood_subset1$volume_cm3*0.15, softwood_subset1$volume_cm3<-0)))

##Create subset of the data that was identified as standing softwood.
softwood_subset2 <- subset(all_deadwood, Species == "balsam_fir" & Status == "S")

##Fill in the biomass column for deadwood that was identified as standing softwood. Note that
##there is a data entry for a standing balsam fir within class 5, but in Harmon et al., 2011
##no standing species within decay class 5 were sampled; I therefore use the values
##from decay class 4 for this data entry.
softwood_subset2$biomass <- softwood_subset2$volume_cm3*0.26

##Create a subset of the data that was identified as downed birch.
hardwood_subset1 <- subset(all_deadwood, Species == "birch" & Status =="F")

##Fill in the biomass column for deadwood that was identified as downed birch.
hardwood_subset1$biomass = ifelse(hardwood_subset1$Decay_Class %in% c("1"),
                                  hardwood_subset1$volume_cm3*0.43,ifelse(hardwood_subset1$Decay_Class %in% c("2"),
                                                                          hardwood_subset1$volume_cm3*0.33,ifelse(hardwood_subset1$Decay_Class %in% c("3"),
                                                                                                                  hardwood_subset1$volume_cm3*0.23,ifelse(hardwood_subset1$Decay_Class %in% c("4"),
                                                                                                                                                          hardwood_subset1$volume_cm3*0.13,ifelse(hardwood_subset1$Decay_Class %in% c("5"),
                                                                                                                                                                                                  hardwood_subset1$volume_cm3*0.11, hardwood_subset1$volume_cm3<-0)))))

##Create a subset of the data that was identified as standing birch.
hardwood_subset2 <- subset(all_deadwood, Species == "birch" & Status == "S")

##Fill in the biomass column for deadwood that was identified as standing birch.
##Note that only one entry of decay class 5 exists (I used value from decay class 4).
hardwood_subset2$biomass <- hardwood_subset2$volume_cm3*0.2  


# same until here ---------------------------------------------------------


##Create a subset of the data that was not identified and was downed deadwood.
downed_subset_EW <- subset(all_deadwood, Species=="NA" & Status =="F")

##Fill in the biomass column for downed deadwood that was not identified.
downed_subset_EW$biomass = ifelse(downed_subset_EW$Decay_Class %in% c("1"),
                               downed_subset_EW$volume_cm3*0.4,ifelse(downed_subset_EW$Decay_Class %in% c("2"),
                                                                   downed_subset_EW$volume_cm3*0.33,ifelse(downed_subset_EW$Decay_Class %in% c("3"),
                                                                                                        downed_subset_EW$volume_cm3*0.26,ifelse(downed_subset_EW$Decay_Class %in% c("4"),
                                                                                                                                             downed_subset_EW$volume_cm3*0.15,ifelse(downed_subset_EW$Decay_Class %in% c("5"),                          
                                                                                                                                                                                  downed_subset_EW$volume_cm3*0.11, downed_subset_EW$volume_cm3<-0)))))

##Create a subset of data that was not identified and was standing deadwood.
standing_subset_EW <- subset (all_deadwood,Species=="NA" & Status =="S")

##Fill in the biomass column for standing deadwood that was not identified.
standing_subset_EW$biomass = ifelse(standing_subset_EW$Decay_Class %in% c("1"),
                                 standing_subset_EW$volume_cm3*0.4,ifelse(standing_subset_EW$Decay_Class %in% c("2"),
                                                                       standing_subset_EW$volume_cm3*0.38,ifelse(standing_subset_EW$Decay_Class %in% c("3"),
                                                                                                              standing_subset_EW$volume_cm3*0.35,ifelse(standing_subset_EW$Decay_Class %in% c("4","5"),
                                                                                                                                                     standing_subset_EW$volume_cm3*0.25, downed_subset_EW$volume_cm3<-0))))

##Create a subset of stump data that was not identified. Note that no stumps in 
##dataset were identified.
stump_subset_EW <- subset(all_deadwood, Species=="NA" & Status =="stump")

##Fill in the biomass column for stump deadwood that was not identified.
##Average density values for standing deadwood were used below.
stump_subset_EW$biomass = ifelse(stump_subset_EW$Decay_Class %in% c("1"),
                              stump_subset_EW$volume_cm3*0.4,ifelse(stump_subset_EW$Decay_Class %in% c("2"),
                                                                 stump_subset_EW$volume_cm3*0.38,ifelse(stump_subset_EW$Decay_Class %in% c("3"),
                                                                                                     stump_subset_EW$volume_cm3*0.35,ifelse(stump_subset_EW$Decay_Class %in% c("4","5"),
                                                                                                                                         stump_subset_EW$volume_cm3*0.25, stump_subset_EW$volume_cm3<-0))))

##Create a subset of data with no deadwood present in the subplots.
NA_subset <- subset(all_deadwood, Status=="NA")

##Fill in the biomass column for NA data.
NA_subset$biomass <- 0

##Join all data subsets back together.
deadwood_final_EW <- rbind(softwood_subset1, softwood_subset2, hardwood_subset1,
                        hardwood_subset2, downed_subset_EW, standing_subset_EW, stump_subset_EW, NA_subset)

##multiply biomass by 0.5 to convert to carbon content
deadwood_final_EW$carbon <- deadwood_final_EW$biomass *0.5

##Combine columns describing Site ID & Subplot ID.
deadwood_final_EW$Site_Subplot_ID <- paste(deadwood_final_EW$Park_ID,
                                        deadwood_final_EW$Site_ID,deadwood_final_EW$Excl_Ctrl,
                                        deadwood_final_EW$Subplot_ID, sep="_")

##Sum the carbon content of all deadwood within each transect.
carbon_deadwood_EW <-aggregate(deadwood_final_EW$carbon, list
                            (deadwood_final_EW$Site_Subplot_ID), FUN=sum)

##Change column names.
colnames(carbon_deadwood_EW) <- c("Site_Subplot_ID", "carbon_9m2")