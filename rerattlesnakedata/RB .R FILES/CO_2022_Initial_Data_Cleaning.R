# Monday September 26 2022

#Meeting with Savannah
#Data uploading and initial cleaning

setwd("/Users/robinbedard/Desktop/Thesis/csv files")

#loading data from all captured snakes
all_viridis <- read.csv("EXP 1 Measurements (All Captured Snakes) - Sheet1.csv", skip = 1)

#loading data from all pregnant females and repeated measurements
mother_viridis <- read.csv("Mothers Data Collection Sheet (DA water masses removed, edited for R) - Sheet1 (1).csv", skip = 1)

#loading data from all neonate snakes day after birth
neonate_viridis_DAB <- read.csv("Neonates Day After Birth Msmnts. - Sheet1.csv", skip = 1)

#loading data from all neonate snakes post-shed
neonate_viridis_PS <- read.csv("Neonates Post Shed Msmnts. - Sheet1.csv", skip = 1)

library(dplyr)
library(tidyverse)

## ALL VIRIDIS

#remove trailing white space or any weird characters, then properly class each column of the all_viridis data, renamed the new data frame to all_viridis_clean

all_viridis_clean <- all_viridis %>%
  mutate(IndivID = as.factor(str_trim(IndivID)),
         Sex = as.factor(str_trim(Sex)),
         RookeryCaptureID = as.factor(str_trim(RookeryCaptureID)),
         LifeStage.. = as.factor(str_trim(LifeStage..)),
         Pregnant = as.factor(str_trim(Pregnant)),
         Mass.g. = as.numeric(str_trim(Mass.g.)),
         SVL.cm. = as.numeric(str_trim(SVL.cm.)),
         ShedStage = as.factor(str_trim(ShedStage)),
         TbCEWL.C. = as.numeric(str_trim(TbCEWL.C.)),
         DateofCapture = as.Date(str_trim(DateofCapture),
                                 format = "%m-%d-%y"),
         CEWL.BloodCollectDate = as.Date(str_trim(CEWL.BloodCollectDate),
                                         format = "%m-%d-%y"),
         DateBloodAnalyzed = as.Date(str_trim(DateBloodAnalyzed),
                                     format = "%m-%d-%y")
         ) %>%
  
#rename all column names 
  
  dplyr::select(Indiv_ID = IndivID, Rookery_Capture_ID = RookeryCaptureID, Date_of_Capture = DateofCapture, SVL_cm = SVL.cm., Life_Stage = LifeStage.., Mass_g = Mass.g., Shed_Stage = ShedStage, Tb_at_CEWL = TbCEWL.C.,.CEWL_Blood_Collect_Date = CEWL.BloodCollectDate, Plasma_Osmol_Rep1 = PlasmaOsmolalityRep1.mmol.kg., Plasma_Osmol_Rep2 = PlasmaOsmolalityRep2.mmol.kg., Plasma_Osmol_Rep3 = PlasmaOsmolalityRep3.mmol.kg., Plasma_Osmol_Rep4 = PlasmaOsmolalityRep4.mmol.kg.)

## NEONATES DAY AFTER BIRTH 

#remove trailing white space or any weird characters, then properly class for the neonate after birth viridis data frame, renaming the data frame to be neonate_viridis_DAB_clean

neonate_viridis_DAB_clean <- neonate_viridis_DAB %>%
  mutate(IndivID = as.factor(str_trim(IndivID)),
         DateBorn = as.Date(str_trim(DateBorn),
                            format = "%m-%d-%y"),
         TailLength.cm. = as.numeric(str_trim(TailLength.cm.)),
         SVL.cm. = as.numeric(str_trim(SVL.cm.)),
         TailLength.SVLRatio = as.numeric(str_trim(TailLength.SVLRatio)),
         Sex = as.factor(str_trim(Sex)),
         MotherID = as.factor(str_trim(MotherID)),
         Treatment = as.factor(str_trim(Treatment)),
         CEWL.BloodCollectDate = as.Date(str_trim(CEWL.BloodCollectDate),
                              format = "%m-%d-%y"),
         TbCEWL.C. = as.numeric(str_trim(TbCEWL.C.)),
         Mass.g. = as.numeric(str_trim(Mass.g.)),
         DateBloodAnalyzed = as.Date(str_trim(DateBloodAnalyzed),
                              format = "%m-%d-%y"),
         Pee = as.factor(str_trim(Pee))
         )
neonate_viridis_DAB_clean$Treatment[26] <- "W"
summary(neonate_viridis_DAB_clean)

#Rename all of the column names 
neonate_viridis_DAB_clean <- neonate_viridis_DAB_clean %>%
  dplyr::select(Indiv_ID = IndivID, Date_Born = DateBorn, Tail_Length_cm = TailLength.cm., SVL_cm = SVL.cm., Tail_SVL_Ratio = TailLength.SVLRatio, Mass_g = Mass.g., Sex = Sex, Mother_ID = MotherID, Treatment = Treatment, CEWL_Blood_Collect_Date = CEWL.BloodCollectDate, Tb_CEWL_c = TbCEWL.C., Date_Blood_Analyzed = DateBloodAnalyzed, Pee = Pee, Plasma_Osmol_Rep1 = PlasmaOsmolalityRep1.mmol.kg., Plasma_Osmol_Rep2 = PlasmaOsmolalityRep2.mmol.kg., Plasma_Osmol_Rep3 = PlasmaOsmolalityRep3.mmol.kg., Plasma_Osmol_Rep4 = PlasmaOsmolalityRep4.mmol.kg.)

## NEONATES POST SHED 

neonate_viridis_PS_clean <- neonate_viridis_PS %>%
  mutate(IndivID = as.factor(str_trim(IndivID)),
         DateBorn = as.Date(str_trim(DateBorn),
                            format = "%m-%d-%y"),
         DateShed = as.Date(str_trim(DateShed),
                            format = "%m-%d-%y"),
         TailLength. = as.numeric(str_trim(TailLength.)),
         SVL = as.numeric(str_trim(SVL)),
         TailLength.SVLRatio = as.numeric(str_trim(TailLength.SVLRatio)),
         Sex = as.factor(str_trim(Sex)),
         MotherID = as.factor(str_trim(MotherID)),
         Treatment = as.factor(str_trim(Treatment)),
         CEWL.BloodCollectDate = as.Date(str_trim(CEWL.BloodCollectDate),
                                         format = "%m-%d-%y"),
         TbCEWL.C. = as.numeric(str_trim(TbCEWL.C.)),
         Mass.g. = as.numeric(str_trim(Mass.g.)),
         DateBloodAnalyzed = as.Date(str_trim(DateBloodAnalyzed),
                                     format = "%m-%d-%y"),
         Pee = as.factor(str_trim(Pee))
  )

summary(neonate_viridis_PS_clean)

#Rename all of the column names 
neonate_viridis_PS_clean <- neonate_viridis_PS_clean %>%
  dplyr::select(Indiv_ID = IndivID, Date_Born = DateBorn, Date_Shed = DateShed, Tail_Length_cm = TailLength., SVL_cm = SVL, Tail_SVL_Ratio = TailLength.SVLRatio, Mass_g = Mass.g., Sex = Sex, Mother_ID = MotherID, Treatment = Treatment, CEWL_Blood_Collect_Date = CEWL.BloodCollectDate, Tb_CEWL_c = TbCEWL.C., Date_Blood_Analyzed = DateBloodAnalyzed, Pee = Pee, Plasma_Osmol_Rep1 = PlasmaOsmolalityRep1.mmol.kg., Plasma_Osmol_Rep2 = PlasmaOsmolalityRep2.mmol.kg., Plasma_Osmol_Rep3 = PlasmaOsmolalityRep3.mmol.kg., Plasma_Osmol_Rep4 = PlasmaOsmolalityRep1.mmol.kg..1)

## MOTHERS 

#remove trailing white space or any weird characters, then properly class for the mother viridis data frame, renaming the data frame to be mother_viridis_clean
mother_viridis_clean <- mother_viridis %>%
  mutate(IndivID = as.factor(str_trim(IndivID)),
         RookeryCaptureID = as.factor(str_trim(RookeryCaptureID)),
         HydrationTreatment = as.factor(str_trim(HydrationTreatment)),
         Mass.g. = as.numeric(str_trim(Mass.g.)),
         SVL.cm. = as.numeric(str_trim(SVL.cm.)),
         ShedStage = as.factor(str_trim(ShedStage)),
         TbCEWL.C. = as.numeric(str_trim(TbCEWL.C.)),
         DateofParturition = as.Date(str_trim(DateOfParturition),
                                 format = "%m-%d-%y"),
         TimeOfParturition = as.POSIXct(str_trim(TimeOfParturition),
                                 format = "%H:%M"),
         CEWL.BloodCollectDate = as.Date(str_trim(CEWL.BloodCollectDate),
                                         format = "%m-%d-%y"),
         DateBloodAnalyzed = as.Date(str_trim(DateBloodAnalyzed),
                                     format = "%m-%d-%y"), 
         LiveOffspring = as.numeric(str_trim(LiveOffspring)),
         DeadOffspring = as.numeric(str_trim(DeadOffspring)),
         Slugs = as.numeric(str_trim(Slugs)), 
         StageOfPregnancy = as.factor(str_trim(StageOfPregnancy))
         
  ) %>%
  
#rename all columns 
  
  dplyr::select(Indiv_ID = IndivID, Rookery_Capture_ID = RookeryCaptureID, Hydration_Trmt = HydrationTreatment, Mass_g = Mass.g., SVL_cm = SVL.cm., Shed_Stage = ShedStage, Tb_at_CEWL = TbCEWL.C., Date_Parturition = DateofParturition, Time_Parturition = TimeOfParturition, Preg_Stage = StageOfPregnancy, CEWL_Blood_Collect_Date = CEWL.BloodCollectDate, Date_Blood_Analyzed = DateBloodAnalyzed, Plasma_Osmol_Rep1 = PlasmaOsmolalityRep1.mmol.kg., Plasma_Osmol_Rep2 = PlasmaOsmolalityRep2.mmol.kg., Plasma_Osmol_Rep3 = PlasmaOsmolalityRep3.mmol.kg., Plasma_Osmol_Rep4 = PlasmaOsmolalityRep4.mmol.kg.) %>%
  
#fill in all of the empty spaces below the individual ID with the repeated ID
  fill(Indiv_ID)

#Create new data frame called temp selecting out all of the measurements that are not repeated. These measurements were only taken once (ID, Rookery capture, treatment, SVL, dates).
temp <- mother_viridis_clean %>% dplyr::select(Indiv_ID, Rookery_Capture_ID, Hydration_Trmt, SVL_cm, Date_Parturition, Time_Parturition) %>%
  dplyr::filter(complete.cases(SVL_cm))

#Create a new data frame called final_mother_viridis selecting out all of the measurements that are repeated. Did this by using the -c to deselect all of the ones above, automatically keeping the other ones that are repeated. Also took out the plasma osmolarity measurements because we don't want them right now. They will be analyzed separately in a different data frame and on a separate RMD. 
final_mother_viridis <- mother_viridis_clean %>% dplyr::select(-c(Rookery_Capture_ID, Hydration_Trmt, SVL_cm, Date_Parturition, Time_Parturition, Date_Blood_Analyzed, Plasma_Osmol_Rep1, Plasma_Osmol_Rep2, Plasma_Osmol_Rep3, Plasma_Osmol_Rep4)) %>%
#join the temp data frame to the final_mother_viridis data frame. This will fill in all of the NA spaces where the merged cells caused issues. This is the final mother data frame that can be used for analysis. It does not include osmolarity measurements. That will be separate. 
  left_join(temp, by = "Indiv_ID")

#Creating a data frame called osmolality1 that will incluDe all of the osmolarity data from the mother viridis, the indiv ID, blood collect date, and blood analyzed date. 
osmolality1 <- mother_viridis_clean %>% dplyr::select(Indiv_ID, CEWL_Blood_Collect_Date, Date_Blood_Analyzed, Preg_Stage, Hydration_Trmt, Plasma_Osmol_Rep1, Plasma_Osmol_Rep2, Plasma_Osmol_Rep3, Plasma_Osmol_Rep4)

#save the osmolarity data frame to a RDS file. This needs to be save separately for some reason that I don't really understand but it has to do with it automatically uploading when you open a file. Save it and upload the RDS file to the RMD that Savannah sent for analyzing the osmolarity data. 
saveRDS(osmolality1, "/Users/robinbedard/Desktop/Thesis/MotherOsmolality.RDS")

saveRDS(final_mother_viridis, "/Users/robinbedard/Desktop/Thesis/FinalMotherViridis.RDS")


#NEONATE DAY AFTER BIRTH 
#Basically doing the same this as above that I did with the mothers but now with the neonate data the day after birth. I need to create a df with only the relevant osmolarity data information and a df with all other columns, save them both as RDS, then create a new R script where I do the neonate osmolarity data wrangling. 

final_neonate_dab <- neonate_viridis_DAB_clean %>%
  dplyr::select(Indiv_ID, Date_Born, Tail_Length_cm, SVL_cm, Tail_SVL_Ratio, Mass_g, Sex, Mother_ID, Treatment, Tb_CEWL_c, Pee)

neonate_osmol_temp <- neonate_viridis_DAB_clean %>%
  dplyr::select(-c(Date_Born, Tail_Length_cm, SVL_cm, Tail_SVL_Ratio, Mass_g, Sex, Mother_ID, Treatment, Tb_CEWL_c, Pee))

saveRDS(final_neonate_dab, "/Users/robinbedard/Desktop/Thesis/FinalNeonateDAB.RDS")

saveRDS(neonate_osmol_temp, "/Users/robinbedard/Desktop/Thesis/NeonateOsmolarityDAB.RDS")




