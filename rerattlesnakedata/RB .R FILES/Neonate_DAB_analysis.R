
#Analysis for neonates the day after birth

setwd("/Users/robinbedard/Desktop/Thesis/RDS_files")
#reading in the file of osmolarities for all neonates
neonate_dab_osmol <- readRDS("clean_Neonate_DAB_Osmolality.RDS")
#reading in all other body measurements of neonates
neonate_dab_all <- readRDS("FinalNeonateDAB.RDS")
#joinging them together to create 1 df
neonate_dab_all_final <- left_join(neonate_dab_all, neonate_dab_osmol, by = c("CEWL_Blood_Collect_Date", "Indiv_ID"))
#reading in CEWL data
all_data_CEWL <- readRDS("CEWL_data_all_clean.RDS")
#merging all together
neonate_dab_all_final <- left_join(neonate_dab_all_final, all_data_CEWL, by = c("Indiv_ID" = "individual_ID", "CEWL_Blood_Collect_Date" = "date"))

#Thermocouple Calibration


setwd("/Users/robinbedard/Desktop/Thesis/csv files")
thermo_c <- read.csv("thermoc_calibration.csv")
thermo_coupl <- thermo_c %>%
  dplyr::select(Time, Value) %>% 
  mutate(Time = as.POSIXct(substr(Time, 1,5), format = "%H:%M"))

thermo_ref <- read.csv("thermalcouple_c_reference.csv") %>% 
  mutate(Time = as.POSIXct(Time, format = "%H:%M"))

thermo_c_final <- left_join(thermo_coupl, thermo_ref, by = "Time")

#Calibrate Thermocouple Data
#First, calculate and save regression curves:
# calculate regression curves
regression_a <- lm(data = thermo_c_final,
                   Value ~ Temp)

# create df to store calibration equations
TC_calibration <- data.frame(TC_A = (coef(regression_a))) %>%
  `rownames<-`(c("intercept", "slope"))

#Next, use calibration curves to correct the data we measured:
# this applies respective calibration equation
# to all cloacal thermocouple data points of that proper thermocouple
# 2=slope & 1=intercept

neonate_dab_all_final <- neonate_dab_all_final %>%
  mutate(calibrated_cloacal_temp = Tb_CEWL_c*TC_calibration$TC_A[2] +
           TC_calibration$TC_A[1])

summary(neonate_dab_all_final)

#Now I have a new column with the calibrated cloacal temperature at the time the measurement was taken. 


library("ggpubr")

ggplot(data = neonate_dab_all_final) +
  geom_boxplot(aes(x = Treatment, y = Plasma_Osmol_Rep_mean), fill = "lightblue", color = "black", outlier.fill = "lightgreen", outlier.colour = "black", outlier.shape = 21, outlier.size = 2) +
  labs(title = "Plasma Osmolality of Neonates Day After Birth",
       subtitle = "by Treatment Group", 
       x = "Hydration Treatment",
       y = "Mean Osmolality (mmol/kg)") +
  stat_summary(aes(x = Treatment, y = Plasma_Osmol_Rep_mean), fun = "mean", color= "red", shape = 16) +
  scale_y_continuous(limits = c(250, 380),
                     breaks = seq(from = 250, to = 3650, by = 30)) +
  stat_compare_means(method = "t.test", aes(x = Treatment, y = Plasma_Osmol_Rep_mean))




neonate_dab_osmol_lm <- lm(Plasma_Osmol_Rep_mean ~ Treatment, data = neonate_dab_all_final)
summary(neonate_dab_osmol_lm)



ggplot(data = neonate_dab_all_final) +
  geom_boxplot(aes(x = Treatment, y = CEWL_g_m2h), fill = "lightblue", color = "black", outlier.fill = "lightgreen", outlier.colour = "black", outlier.shape = 21, outlier.size = 2) +
  labs(title = "CEWL of Neonates Day After Birth",
       subtitle = "by Treatment Group", 
       x = "Hydration Treatment",
       y = "CEWL") +
  stat_summary(aes(x = Treatment, y = CEWL_g_m2h), fun = "mean", color= "red", shape = 16) +
  #scale_y_continuous(limits = c(250, 380),
                    # breaks = seq(from = 250, to = 3650, by = 30)) +
  stat_compare_means(method = "t.test", aes(x = Treatment, y = CEWL_g_m2h))


neonate_dab_cewl_lm <- lm(CEWL_g_m2h ~ Treatment, data = neonate_dab_all_final)
summary(neonate_dab_cewl_lm)

#Creating a new column with the averages of osmolarities in a clutch so the neonates are replicates. Then will add this to the mother analysis data frame. 

neonate_dab_all_final2 <- neonate_dab_all_final %>%                                  
  group_by(Mother_ID) %>%
  mutate(clutch_osmol_avg = mean(Plasma_Osmol_Rep_mean, na.rm = TRUE))

#Creating a new column with the averages of CEWL in a clutch so the neonates are replicates. Then will add this to the mother analysis data frame

neonate_dab_all_final3 <- neonate_dab_all_final2 %>%                                  
  group_by(Mother_ID) %>%
  mutate(clutch_CEWL_avg = mean(CEWL_g_m2h, na.rm = TRUE))

neonate_dab_all_final4 <- neonate_dab_all_final3 %>%
  mutate(Pee = as.factor(str_trim(Pee)))
  

neonate_dab_all_final5 <- neonate_dab_all_final4 %>%                                  
  group_by(Mother_ID) %>%
  mutate(Prop_Pee = sum(Pee == "Y")/sum(Pee == "Y" | Pee == "N"))


#Redoing figures above because those did not have the neonates as replicates which does not meet the assumption of individuality for the statistical test. ... Realized that I don't think I can do this because it assumes the samples size is much larger than it actually is. I think I need to add the data to  the mothers data frame and then make these figures below. 

#osmolarity
ggplot(data = neonate_dab_all_final3) +
  geom_boxplot(aes(x = Treatment, y = clutch_osmol_avg), fill = "lightblue", color = "black", outlier.fill = "lightgreen", outlier.colour = "black", outlier.shape = 21, outlier.size = 2) +
  labs(title = "Plasma Osmolality of Neonates Day After Birth",
       subtitle = "by Treatment Group with Neonates as Replicates", 
       x = "Hydration Treatment",
       y = "Osmolality (mmol/kg)") +
  stat_summary(aes(x = Treatment, y = clutch_osmol_avg), fun = "mean", color= "red", shape = 16) +
  scale_y_continuous(limits = c(250, 380),
                     breaks = seq(from = 250, to = 3650, by = 30)) +
  stat_compare_means(method = "t.test", aes(x = Treatment, y = clutch_osmol_avg))

#cewl
ggplot(data = neonate_dab_all_final3) +
  geom_boxplot(aes(x = Treatment, y = clutch_CEWL_avg), fill = "lightblue", color = "black", outlier.fill = "lightgreen", outlier.colour = "black", outlier.shape = 21, outlier.size = 2) +
  labs(title = "CEWL of Neonates Day After Birth",
       subtitle = "by Treatment Group with Neonates as Replicates", 
       x = "Hydration Treatment",
       y = "CEWL") +
  stat_summary(aes(x = Treatment, y = clutch_CEWL_avg), fun = "mean", color= "red", shape = 16) +
  #scale_y_continuous(limits = c(250, 380),
  # breaks = seq(from = 250, to = 3650, by = 30)) +
  stat_compare_means(method = "t.test", aes(x = Treatment, y = clutch_CEWL_avg))

#Making a dataframe that only has mother id, clutch cewl, and clutch osmolarity. Now there is only one value for each mother. This is what we want. 
summary(neonate_dab_all_final5)

clutch_averages <- unique(neonate_dab_all_final5[ , c(8, 9, 11, 18, 19, 20)])
summary(clutch_averages)

saveRDS(clutch_averages, "/Users/robinbedard/Desktop/Thesis/RDS_files/clutch_averages_RDS")

#Now I will redo the analysis with the neonates as replicates for the mothers and only one value per mother. 
 
library(ggplot2)


#osmolarity
ggplot(data = clutch_averages) +
  geom_boxplot(aes(x = Treatment, y = clutch_osmol_avg), fill = "lightblue", color = "black", outlier.fill = "lightgreen", outlier.colour = "black", outlier.shape = 21, outlier.size = 2) +
  labs(title = "Plasma Osmolality of Neonates Day After Birth",
       subtitle = "by Treatment Group with Neonates as Replicates", 
       x = "Hydration Treatment",
       y = "Osmolality (mmol/kg)") +
  stat_summary(aes(x = Treatment, y = clutch_osmol_avg), fun = "mean", color= "red", shape = 16) +
  scale_y_continuous(limits = c(260, 380),
                     breaks = seq(from = 260, to = 3650, by = 30)) +
  annotate("text", x=1.5, y=370, label= "p = 0.03726")


neonate_dab_osmol_lm <- lm(clutch_osmol_avg ~ Treatment, data = clutch_averages)
summary(neonate_dab_osmol_lm)

#p = 0.03726 ; significant difference 

#cewl
ggplot(data = clutch_averages) +
  geom_boxplot(aes(x = Treatment, y = clutch_CEWL_avg), fill = "lightblue", color = "black", outlier.fill = "lightgreen", outlier.colour = "black", outlier.shape = 21, outlier.size = 2) +
  labs(title = "CEWL of Neonates Day After Birth",
       subtitle = "by Treatment Group with Neonates as Replicates", 
       x = "Hydration Treatment",
       y = "CEWL") +
  stat_summary(aes(x = Treatment, y = clutch_CEWL_avg), fun = "mean", color= "red", shape = 16) +
  scale_y_continuous(limits = c(4, 18),
                     breaks = seq(from = 4, to = 18, by = 3)) +
  annotate("text", x=1.5, y=17.5, label= "p = 0.2192")

neonate_cewl_osmol_lm <- lm(clutch_CEWL_avg ~ Treatment, data = clutch_averages)
summary(neonate_cewl_osmol_lm)

#p = 0.2192 ; not significantly different 


#Starting to redo the pee analysis. I need to get the proportion of neonates that pee'ed in the clutch for each mother so that they are replicates.

pee_proportions <- data.frame(prop.table(table(neonate_dab_all_final$Mother_ID, neonate_dab_all_final$Pee))) %>%
dplyr::select(Indiv_ID = Var1, Pee = Var2, Freq = Freq)
  
trmts <- neonate_dab_all_final[ , c(8, 9)]

pee_proportions <- unique(left_join(pee_proportions, trmts, by = c("Indiv_ID" = "Mother_ID")))

library(ggplot2)
library(ggpubr)


ggplot(data = pee_proportions) +
  geom_boxplot(aes(x = Pee, y = Freq, fill = Treatment)) +
  facet_wrap(~Treatment) +
  stat_compare_means(method = "pair", aes(x = Pee, y = Freq)) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 0.065))

ggplot(data = pee_proportions) +
  geom_boxplot(aes(x = Treatment, y = Freq)) +
  facet_wrap(~Pee) +
  stat_compare_means(method = "t.test", aes(x = Treatment, y = Freq))


table(clutch_averages$Treatment, clutch_averages$Prop_Pee)

ggplot(clutch_averages) +
  aes(x =  Prop_Pee, fill = Treatment) +
  geom_bar()

ggplot(clutch_averages) +
  aes(x = Prop_Pee, fill = Treatment) +
  geom_bar(position = "fill")



ggplot(clutch_averages)+ 
  aes(x = Mother_ID, y = Prop_Pee, fill = Treatment) +
  geom_point()

ggplot(clutch_averages, aes(x = Treatment, y = Prop_Pee, fill = Treatment)) +
  geom_boxplot() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = "Treatment",
       y = "Proportion of neonates in a clutch that did 'pee'") +
  stat_summary(fun = "mean")
  
  

summary(clutch_averages)
TukeyHSD(aov(data = clutch_averages, Prop_Pee ~ Treatment))
