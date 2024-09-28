#All snake initial measurement analysis (all males, juv, preg and nonpreg females)

#uploading the osmolarity data 
setwd("/Users/robinbedard/Desktop/Thesis/RDS_files")
all_snake_osmol_temp <- readRDS("/Users/robinbedard/Desktop/Thesis/RDS_Files/clean_all_viridis_initial_outliersremoved.RDS")

all_snake_initial <- readRDS("/Users/robinbedard/Desktop/Thesis/RDS_Files/final_all_viridis_initial.RDS")

all_snake_initial_final <- left_join(all_snake_osmol_temp, all_snake_initial, by = c("CEWL_Blood_Collect_Date", "Indiv_ID"))
#reading in CEWL data
all_data_CEWL <- readRDS("/Users/robinbedard/Desktop/Thesis/RDS_files/CEWL_data_all_clean.RDS")
#merging all together
all_snakes_final <- left_join(all_snake_initial_final, all_data_CEWL, by = c("Indiv_ID" = "individual_ID", "CEWL_Blood_Collect_Date" = "date"))

all_snakes_final_2 <- all_snakes_final %>%
# compute SMI
  mutate(SMI = case_when(Pregnant == "Y" ~ Mass_g * ((77.55152/SVL_cm) ^ 3.098426), 
                         Pregnant != "Y" ~ Mass_g * ((78.34545/SVL_cm) ^ 3.030647),
                         is.na(Pregnant) == TRUE ~ Mass_g * ((78.34545/SVL_cm) ^ 3.030647)))

saveRDS(all_snakes_final_2, "/Users/robinbedard/Desktop/Thesis/RMD_Files/all_snakes_final.RDS")

#body temperature data

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

all_snakes_final_3 <- all_snakes_final_2 %>%
  mutate(calibrated_cloacal_temp = Tb_at_CEWL*TC_calibration$TC_A[2] +
      TC_calibration$TC_A[1])
  
summary(all_snakes_final_3)

# Get levels and add "Male"
levels <- levels(all_snakes_final_3$Pregnant)
levels[length(levels) + 1] <- "Male"

# refactor Preg to include "Male" as a factor level
# and replace NA with "None"
all_snakes_final_3$Pregnant <- factor(all_snakes_final_3$Pregnant, levels = c("Y", "N", "Male"))
all_snakes_final_3$Pregnant[is.na(all_snakes_final_3$Pregnant)] <- "Male"


?aov
corr_df <- all_snakes_final_3 %>%
  mutate(Indiv_ID = as.numeric(Indiv_ID)) %>%
  ungroup() %>%
  dplyr::select(Indiv_ID, Plasma_Osmol_Rep_mean, SVL_cm, Mass_g, calibrated_cloacal_temp, CEWL_g_m2h, msmt_temp_C, msmt_RH_percent, SMI)

library("PerformanceAnalytics")

chart.Correlation(corr_df, histogram=TRUE, pch=19)

library("PerformanceAnalytics")
library("lme4")
library("car")


#seeing which nonnumerical variables are correlated with CEWL

summary(lm(CEWL_g_m2h ~ CEWL_Blood_Collect_Date, data = all_snakes_final_3))
TukeyHSD(aov(CEWL_g_m2h ~ as.factor(CEWL_Blood_Collect_Date), all_snakes_final_3))
#collect date is not correlated with cewl
TukeyHSD(aov(CEWL_g_m2h ~ Rookery_Capture_ID, all_snakes_final_3))
#no rookeries are similar, don't have to include in model
summary(lm(CEWL_g_m2h ~ Date_of_Capture, data = all_snakes_final_3))
TukeyHSD(aov(CEWL_g_m2h ~ as.factor(Date_of_Capture), all_snakes_final_3))
#date of capture is correlated with cewl, will include in model
summary(lm(CEWL_g_m2h ~ Sex, data = all_snakes_final_3))
#no difference betweek sexes, wont include
summary(lm(CEWL_g_m2h ~ Pregnant, data = all_snakes_final_3))
TukeyHSD(aov(CEWL_g_m2h ~ Pregnant, all_snakes_final_3))
#no difference between pregnant and non pregnant snakes 
TukeyHSD(aov(CEWL_g_m2h ~ Life_Stage, all_snakes_final_3))
#no difference between life stages, wont include
TukeyHSD(aov(CEWL_g_m2h ~ Shed_Stage, all_snakes_final_3))
#no difference between shed stages



#seeing which nonnumerical variables are correlated with osmolarity

summary(lm(Plasma_Osmol_Rep_mean ~ CEWL_Blood_Collect_Date, data = all_snakes_final_3))
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ as.factor(CEWL_Blood_Collect_Date), all_snakes_final_3))
#collect date is not correlated with osmolarity
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ Rookery_Capture_ID, all_snakes_final_3))
#no rookeries are similar, don't have to include in model
summary(lm(Plasma_Osmol_Rep_mean ~ Date_of_Capture, data = all_snakes_final_3))
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ as.factor(Date_of_Capture), all_snakes_final_3))
#date of capture is not correlated with osmolarity
summary(lm(Plasma_Osmol_Rep_mean ~ Sex, data = all_snakes_final_3))
#no difference between sexes, wont include
summary(lm(Plasma_Osmol_Rep_mean ~ Pregnant, data = all_snakes_final_3))
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ Pregnant, all_snakes_final_3))
#no difference between pregnant and non pregnant snakes 
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ Life_Stage, all_snakes_final_3))
#no difference between life stages, wont include
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ Shed_Stage, all_snakes_final_3))
#no difference between shed stages

ggplot(data = all_snakes_final_3,
       aes(y = Plasma_Osmol_Rep_mean)) +
  geom_boxplot()

?boxplot
#inital model for cewl

CEWL_LMM_01 <- lm(data = all_snakes_final_3,
                  CEWL_g_m2h ~ 
                    msmt_temp_C)

CEWL_LMM_02 <- lm(data = all_snakes_final_3,
                          CEWL_g_m2h ~ 
                            msmt_temp_C + 
                           SVL_cm + 
                            SMI)

CEWL_LMM_03 <- lm(data = all_snakes_final_3,
                  CEWL_g_m2h/msmt_temp_C ~ 
                    SVL_cm + 
                    SMI)

CEWL_LMM_04 <- lm(data = all_snakes_final_3,
                  CEWL_g_m2h ~ Plasma_Osmol_Rep_mean)

CEWL_Tukey_Preg <- TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ Pregnant, all_snakes_final_3))

CEWL_LM_05 <- lm(data = all_snakes_final_3,
                  CEWL_g_m2h ~ SMI)

summary(CEWL_LM_05)
summary(CEWL_LMM_04)

plot(CEWL_LMM_04)
summary(CEWL_LMM_03)
summary(CEWL_LMM_01)
summary(CEWL_LMM_02)
drop1(CEWL_LMM_01)
drop1(CEWL_LMM_02)
vif(CEWL_LMM_01)

library("broom")

#saving separately 
write.csv(broom::tidy(CEWL_LMM_01), "/Users/robinbedard/Desktop/Thesis/csv files/initial_measurements_model_1.csv")
write.csv(broom::tidy(CEWL_LMM_02), "/Users/robinbedard/Desktop/Thesis/csv files/initial_measurements_model_2.csv")
write.csv(broom::tidy(CEWL_LMM_04), "/Users/robinbedard/Desktop/Thesis/csv files/initial_measurements_cewl_osmol_regression.csv")
write.csv(broom::tidy(CEWL_Tukey_Preg), "/Users/robinbedard/Desktop/Thesis/csv files/Preg_Male_NonPreg_Pairwise.csv")
write.csv(broom::tidy(CEWL_LM_05), "/Users/robinbedard/Desktop/Thesis/csv files/SMI_CEWL_Regression.csv")


#saving together
both_models <- data.frame(broom::tidy(CEWL_LMM_01)) %>%
rbind(data.frame(broom::tidy(CEWL_LMM_02))) %>%
  mutate(model = c(rep("CEWL; best mod", 2), rep("CEWL; 2nd best mod", 4)))

write.csv(both_models, "/Users/robinbedard/Desktop/Thesis/csv files/initial_measurements_both models.csv")

ggplot(data = all_snakes_final_3,
       aes(x = Plasma_Osmol_Rep_mean,
           y = CEWL_g_m2h)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = "Osmolarity (mmol/kg)",
       y = "CEWL (g/m^2h)", 
       title = "Regression of All Snakes' Initial Osmolarity vs. CEWL",
       subtitle = "p = 0.0469, t = 2.041, estimate = 0.1043, R^2 = 0.08144")

ggplot(data = all_snakes_final_3,
       aes(x = msmt_temp_C,
           y = CEWL_g_m2h)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = "Ambient Temperature (C)",
       y = "CEWL (g/m^2h)", 
       title = "Ambient Temperature (C) vs. CEWL")

ggplot(data = all_snakes_final_3,
       aes(x = SMI,
           y = CEWL_g_m2h)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = "SMI (Body Condition)",
       y = "CEWL (g/m^2h)")

ggplot(data = all_snakes_final_3,
       aes(x = SVL_cm,
           y = CEWL_g_m2h)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = "SVL (mm)",
       y = "CEWL (g/m^2h)")

ggplot(data = all_snakes_final_3,
       aes(x = Pregnant,
           y = CEWL_g_m2h,
           fill = Pregnant)) +
  geom_boxplot() +
  labs(x = "Pregnant (Y or N) or Male",
       y = "CEWL (g/m^2h)") +
  theme_classic() +
  stat_summary(fun = "mean") +
  theme(legend.position = "none")
  
#initial model for osmolarity 

OSMOL_LM_01 <- lm(data = all_snakes_final_3,
                  Plasma_Osmol_Rep_mean ~ 
                    msmt_temp_C +
                    calibrated_cloacal_temp +
                    SMI)

drop1(OSMOL_LM_01)
#Ambient temp and smi can be taken out without difference in AIC
summary(OSMOL_LM_01)

OSMOL_LM_02 <- lm(data = all_snakes_final_3,
                  Plasma_Osmol_Rep_mean ~ 
                    calibrated_cloacal_temp)
summary(OSMOL_LM_02)

#saving the model tables together
both_models_cewl <- data.frame(broom::tidy(OSMOL_LM_02)) %>%
  rbind(data.frame(broom::tidy(OSMOL_LM_01))) %>%
  mutate(model = c(rep("Osmolarity; best model", 4), rep("Osmolarity; 2nd best model", 2)))

write.csv(both_models_cewl, "/Users/robinbedard/Desktop/Thesis/csv files/initial_measurements_both models_osmoalrity.csv")

ggplot(data = all_snakes_final_3,
       aes(x = msmt_temp_C,
           y = Plasma_Osmol_Rep_mean)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = "Ambient Temperature (C)",
       y = "Osmolarity (mmol/kg)")

ggplot(data = all_snakes_final_3,
       aes(x = calibrated_cloacal_temp,
           y = Plasma_Osmol_Rep_mean)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = "Body Temperature at Time of Measurement (C)",
       y = "Osmolarity (mmol/kg)")

ggplot(data = all_snakes_final_3,
       aes(x = SMI,
           y = Plasma_Osmol_Rep_mean)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = "SMI (Body Condition)",
       y = "Osmolarity (mmol/kg)")

saveRDS(all_snakes_final_3, "/Users/robinbedard/Desktop/Thesis/RDS_Files/viridis_2022.RDS")


