#September 29 2022
#Adding the osmolarity means to the clean mothers data set to begin initial analysis

setwd("/Users/robinbedard/Desktop/Thesis/RDS_files")
library("lme4")
library(dplyr)
library(tidyverse)
#Loading in the data from the RDS files 

#Body measurement and measurements that are not replicated

mother_data_final <- read_rds("FinalMotherViridis.RDS") 

##Osmolarity data after outliers were removed and replicates were averaged 

mother_temp_osm <- read_rds("clean_MotherOsmolality.RDS")

##merging the two data frames so osmolarity data is with all other data
?left_join

mother_data_all_final <- left_join(mother_data_final, mother_temp_osm, by =c("Indiv_ID", "CEWL_Blood_Collect_Date"))

##Factoring the pregnancy stages so that they are plotted in the proper order. 
mother_data_all_final$Preg_Stage <- factor(mother_data_all_final$Preg_Stage, levels = c("DAC", "1WAC", "2WAC", "DAB", "1WAB")) 

saveRDS(mother_data_all_final, "/Users/robinbedard/Desktop/Thesis/mother_data_all_final.RDS")

##attempting to make a simple plot of osmolarity data.

ggplot(data = mother_data_all_final) +
  geom_boxplot(aes(x = Hydration_Trmt, y = Plasma_Osmol_Rep_mean), fill = "lightblue", color = "black", outlier.fill = "lightgreen", outlier.colour = "black", outlier.shape = 21, outlier.size = 2) +
  facet_wrap(~Preg_Stage) +
  labs(title = "Mean Plasma Osmolality Across Pregnancy Stages",
       subtitle = "by Treatment Group", 
       x = "Hydration Treatment",
       y = "Mean Osmolality (mmol/kg)") +
  stat_summary(aes(x = Hydration_Trmt, y = Plasma_Osmol_Rep_mean), fun = "mean", color= "red", shape = 16) +
  scale_y_continuous(limits = c(250, 380),
                     breaks = seq(from = 250, to = 380, by = 30))

##Trying to get the overall means by treatment group at each stage of pregnancy using summarise and group by

osmol_means_by_tmt_pregstage <- mother_data_all_final %>%
  group_by(Preg_Stage, Hydration_Trmt) %>%
  summarise(mean = mean(Plasma_Osmol_Rep_mean))
  
  
?group_by
##Making a figure that will have all stages of pregnancy on the x axis, osmolarity on the y axis, then lines for each individual throughout time, lines color coded by treatment group, one thick line representing the means for each treatment group at that stage of pregnancy. 

##Using Savannah's code to try and replicate

ggplot() +
  geom_line(data = mother_data_all_final,
            aes(x = Preg_Stage,
                y = Plasma_Osmol_Rep_mean,
                color = Hydration_Trmt,
                group = Indiv_ID),
                alpha = 0.2) +
  geom_line(data = osmol_means_by_tmt_pregstage,
            aes(x = Preg_Stage,
                y = mean,
                color = Hydration_Trmt,
                group = Hydration_Trmt),
            size = 1
            ) +


                #group = Hydration_Trmt,
                #alpha = 1) +
  #stat_smooth(data = mother_data_all_final,
   #           aes(x = Preg_Stage,
    #              y = Plasma_Osmol_Rep_mean,
     #             color = Hydration_Trmt,
      #            group = Hydration_Trmt),
       #       formula = y ~ x,
        #      method = "loess",
           #   se = F,
            #  size = 1.2,
             # alpha = 1 ) +
  theme_classic() +
  labs(title = "Plasma Osmolality Across Pregnancy Stages",
       subtitle = "By Treatment Group",
       x = "Pregnancy Stage",
       y = "Plasma Osmolality (mmol/kg)",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))
 
#Below is extra code from Savannah's figure if I end up wanting it
  
  #geom_point(data = mother_data_all_final,
   #          aes(x = Preg_Stage,
    #             y = Plasma_Osmol_Rep_mean,
     #            color = Hydration_Trmt,
      ##      size = 6,
        #     alpha = 1) +
  
  #scale_shape_manual(values = c(15:18), name = "") +
  #scale_color_brewer(palette = "Set2", name = "") +
  #xlab("") +
  #ylab("Plasma Osmolality (mmol/kg)") +
  #guides(shape = guide_legend(nrow = 2, byrow = TRUE)) +
  #theme(text = element_text(color = "black",
   #                         family = "sans",
    #                        size = 22),
     #   axis.text = element_text(color = "black",
      #                           family = "sans",
       #                          size = 16),
      #  legend.text = element_text(color = "black",
       #                            family = "sans",
        #                           size = 22),
       # legend.text.align = 0,
      #  legend.position = "none",
       # plot.margin = unit(c(0.6, #top
        #                     0.1, #right
         #                    0.1, #bottom
          #                   0.1 #left
      #  ), "cm")
#  ) -> osml_fig



#linear model for day after capture osmolarities. Is there a difference in osmolarity between the treatment group and the control group the day after capture?
DAC_osmol <- mother_data_all_final %>%
  dplyr::filter(Preg_Stage == "DAC")
DAC_osmol_lm <- lm(Plasma_Osmol_Rep_mean ~ Hydration_Trmt, data = DAC_osmol)
summary(DAC_osmol_lm)
#Yes, there is a significant difference between the control and hydration treatment groups the day after capture (p = 8.36e-05). On average, the hydration treatment osmolarity was 25.025 mmol/kg higher than the control group osmolarity. 

#linear model for the osmolarities one week after capture, compared between hydration groups. 
oneWAC_osmol <- mother_data_all_final %>%
  dplyr::filter(Preg_Stage == "1WAC")
oneWAC_osmol_lm <- lm(Plasma_Osmol_Rep_mean ~ Hydration_Trmt, data = oneWAC_osmol)
summary(oneWAC_osmol_lm)
#There is a significant difference between the control and hydration treatment groups the day after capture (p = 0.0141). On average, the hydration treatment group had an osmolarity of 15.317 mmol/kg lower than the control group. 

#linear model for the osmolarities 2 weeks after capture, compared between hydration groups. 
twoWAC_osmol <- mother_data_all_final %>%
  dplyr::filter(Preg_Stage == "2WAC")
twoWAC_osmol_lm <- lm(Plasma_Osmol_Rep_mean ~ Hydration_Trmt, data = twoWAC_osmol)
summary(twoWAC_osmol_lm)
#There is a significant difference between the control and hydration treatment groups the day after capture (p = 0.01706). On average, the hydration treatment group had an osmolarity of 32.517 mmol/kg lower than the control group. 

#linear model for the osmolarities the day after birth, compared between hydration groups.
DAB_osmol <- mother_data_all_final %>%
  dplyr::filter(Preg_Stage == "DAB")
DAB_osmol_lm <- lm(Plasma_Osmol_Rep_mean ~ Hydration_Trmt, data = DAB_osmol)
summary(DAB_osmol_lm)
#There is no significant difference between the control and hydration groups the day after birth (p = 0.197). 

#Linear model for the osmolarities 1 week after birth, compared between hydration groups.
oneWAB_osmol <- mother_data_all_final %>%
  dplyr::filter(Preg_Stage == "1WAB")
oneWAB_osmol_lm <- lm(Plasma_Osmol_Rep_mean ~ Hydration_Trmt, data = oneWAB_osmol)
summary(oneWAB_osmol_lm)
#says it is significant, but since I only have one individual in the hydration group one week after birth, these reaults don't really mean anything. 

#Running a linear model with all of the data to look at the pairwise comparisions between all stages of pregnancy and treatment groups.
all_osmol_lm <- lm(Plasma_Osmol_Rep_mean ~ Hydration_Trmt * Preg_Stage, data = mother_data_all_final)
all_osmol_av <- aov(all_osmol_lm)
TukeyHSD(all_osmol_av)

#uploading the RDS file that has ALL of the data to do CEWL analysis. This has all body measurements, all CEWL measurements, and all osmolarity measurements for the mothers. 

mother_data_all_final_2 <- readRDS("/Users/robinbedard/Desktop/Thesis/RDS_files/mother_data_all_final_2.RDS")

#Simple boxplots by treatment group and pregnancy stage

ggplot(data = mother_data_all_final_2) +
  geom_boxplot(aes(x = Hydration_Trmt, y = CEWL_g_m2h), fill = "lightblue", color = "black", outlier.fill = "lightgreen", outlier.colour = "black", outlier.shape = 21, outlier.size = 2) +
  facet_wrap(~Preg_Stage) +
  labs(title = "CEWL Across Pregnancy Stages",
       subtitle = "by Treatment Group", 
       x = "Hydration Treatment",
       y = "CEWL") +
  stat_summary(aes(x = Hydration_Trmt, y = CEWL_g_m2h), fun = "mean", color= "red", shape = 16) 

#making the same plot as above but with CEWL data across treatment groups. Need to create the means per pregnancy stage before I can do this. 

CEWL_means_by_tmt_pregstage <- mother_data_all_final_2 %>%
  group_by(Preg_Stage, Hydration_Trmt) %>%
  summarise(mean = mean(CEWL_g_m2h))

ggplot() +
  geom_line(data = mother_data_all_final_2,
            aes(x = Preg_Stage,
                y = CEWL_g_m2h,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 0.2) +
  geom_line(data = CEWL_means_by_tmt_pregstage,
            aes(x = Preg_Stage,
                y = mean,
                color = Hydration_Trmt,
                group = Hydration_Trmt),
            size = 1) +
  theme_classic() +
  labs(title = "CEWL Across Pregnancy Stages",
       subtitle = "By Treatment Group",
       x = "Pregnancy Stage",
       y = "CEWL",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))




ggplot() +
  geom_line(data = mother_data_all_final_2,
            aes(x = Preg_Stage,
                y = CEWL_g_m2h,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 0.2) +
  stat_smooth(data = mother_data_all_final_2,
              aes(x = Preg_Stage,
              y = CEWL_g_m2h,
              color = Hydration_Trmt,
              group = Hydration_Trmt),
              formula = y ~ x,
              method = "loess") +
  theme_classic() +
  labs(title = "CEWL Across Pregnancy Stages",
       subtitle = "By Treatment Group",
       x = "Pregnancy Stage",
       y = "CEWL",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))


#uploading the csv file that has the column with the day relative to the day of birth

setwd("/Users/robinbedard/Desktop/Thesis/csv files") 
day_scale <- read.csv("DAY RELATIVE TO DAY OF BIRTH - Sheet1.csv")


day_scale_clean <- day_scale %>%
  mutate(IndivID = as.factor(str_trim(IndivID)),
         CEWL.BloodCollectDate = as.Date(str_trim(CEWL.BloodCollectDate),
                                         format = "%m-%d-%y"),
         DayRelativeToZero = as.numeric(DayRelativeToZero)
  ) %>%
  
  #rename all column names 
  
  dplyr::select(Indiv_ID = IndivID, CEWL_Blood_Collect_Date = CEWL.BloodCollectDate, Day_Relative = DayRelativeToZero)  %>%

fill(Indiv_ID)

#joining the final table with the day relative to birth column to create a new final dataset 

mother_data_all_final_3 <- left_join(mother_data_all_final_2, day_scale_clean, by = c("Indiv_ID", "CEWL_Blood_Collect_Date"))

saveRDS(mother_data_all_final_3, "/Users/robinbedard/Desktop/Thesis/RDS_files/mother_data_all_final_3.RDS")

# averaging the osmolarities by day relative by treatment group

osmol_means_by_tmt_dayrelative <- mother_data_all_final_3 %>%
  group_by(Day_Relative, Hydration_Trmt) %>%
  summarise(mean = mean(Plasma_Osmol_Rep_mean))

# making plots

# making a plot to standardize the x axis of day relative to the day the snakes gave birth. This only includes the individuals who did give birth. This is because it is relative to the day they gave birth so the mothers that did not give birth have NAs for the values. I see a problem with this graph because all snakes were captured on different days, therefore the start of water treatment varies for the individuals so even the hydration group is messed up by some snakes not yet having water. 

ggplot() +
  geom_line(data = mother_data_all_final_3,
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 0.2) +
  geom_line(data = osmol_means_by_tmt_dayrelative,
            aes(x = Day_Relative,
                y = mean,
                color = Hydration_Trmt,
                group = Hydration_Trmt),
            size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton",
       subtitle = "Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))
  

#trying to look at each individual. There is def a better way to do this. 

ggplot() +
  geom_line(data = mother_data_all_final_3,
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 0.2) +
  facet_wrap(~Indiv_ID) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton",
       subtitle = "Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))


#same original figure as above. Mother osmolarities by treatment group. This time removing the snakes that did not give birth. This will only indclude snakes that DID give birth. 

#create df with only complete cases (individuals with no NAs and therefore did give birth) 

data_complete <- mother_data_all_final_3[complete.cases(mother_data_all_final_3), ] 

#making new means by preg stage by treatment group
osmol_means_by_tmt_pregstage_birthonly <- data_complete %>%
  group_by(Preg_Stage, Hydration_Trmt) %>%
  summarise(mean = mean(Plasma_Osmol_Rep_mean))

#make plot

ggplot() +
  geom_line(data = data_complete,
            aes(x = Preg_Stage,
                y = Plasma_Osmol_Rep_mean,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 0.2) +
  geom_line(data = osmol_means_by_tmt_pregstage_birthonly,
            aes(x = Preg_Stage,
                y = mean,
                color = Hydration_Trmt,
                group = Hydration_Trmt),
            size = 1) +
  theme_classic() +
  labs(title = "Plasma Osmolality Across Pregnancy Stages",
       subtitle = "By Treatment Group",
       x = "Pregnancy Stage",
       y = "Plasma Osmolality (mmol/kg)",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))

#making plots for each individual that gave birth

#101
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "101", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "101", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -11.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -12, to = 0, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 101; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#103
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "103", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "103", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -14, to = 0, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 103; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#104
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "104", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "104", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -13.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -14, to = 0, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 104; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#105
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "105", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "105", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -14, to = 0, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 105; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#112
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "112", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "112", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -12.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -13, to = 0, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 112; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#114
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "114", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "114", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -13, to = 0, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 114; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#115
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "115", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "115", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -14, to = 0, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 115; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#119
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "119", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "119", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -11.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -12, to = 0, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 119; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#124
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "124", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "124", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -3.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -15, to = 0, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 124; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#125
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "125", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "125", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -3, to = 10, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 125; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#126
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "126", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "126", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -5.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -16, to = 1, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 126; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#127
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "127", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "127", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -7.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -8, to = 1, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 127; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#128
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "128", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "128", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -1.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -2, to = 8, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 128; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#129
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "129", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "129", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -2, to = 7, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 129; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#131
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "131", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "131", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -3.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -4, to = 1, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 131; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#133
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "133", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "133", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -4, to = 1, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 133; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#134
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "134", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "134", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -3.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -4, to = 1, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 134; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")

#135
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "135", ],
            aes(x = Day_Relative,
                y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "135", ],
             aes(x = Day_Relative,
                 y = Plasma_Osmol_Rep_mean), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -1, to = 8, by = 1)) +
  labs(title = "Plasma Osmolarity Relative to Day of Parturiton (Indiv. 135; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "Osmolarity")


#Boxplot to show no difference between osmolarities day after birth 

library("ggpubr")

ggplot(data = mother_data_all_final_3[mother_data_all_final_3$Preg_Stage == "DAB", ]) +
  geom_boxplot(data = mother_data_all_final_3[mother_data_all_final_3$Preg_Stage == "DAB", ], 
               aes(x = Hydration_Trmt, y = Plasma_Osmol_Rep_mean, fill = Hydration_Trmt)) +
  labs(title = "Plasma Osmolarity Day After Birth",
       subtitle = "by Treatment Group", 
       x = "Hydration Treatment",
       y = "Plasma Osmolality (mmol/kg)") +
  scale_y_continuous(limits = c(280, 360),
                     breaks = seq(from = 280, to = 360, by = 30)) +
stat_compare_means(method = "t.test", aes(x = Hydration_Trmt, y = Plasma_Osmol_Rep_mean)) +
  stat_summary(aes(x = Hydration_Trmt, y = Plasma_Osmol_Rep_mean), fun = "mean", color= "red", shape = 16) +
  theme_classic() +
  theme(legend.position = "none")
  

#Repeating everything above but for CEWL. 

CEWL_means_by_tmt_dayrelative <- mother_data_all_final_3 %>%
  group_by(Day_Relative, Hydration_Trmt) %>%
  summarise(mean = mean(CEWL_g_m2h))


ggplot() +
  geom_line(data = mother_data_all_final_3,
            aes(x = Day_Relative,
                y = CEWL_g_m2h,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 0.2) +
  geom_line(data = CEWL_means_by_tmt_dayrelative,
            aes(x = Day_Relative,
                y = mean,
                color = Hydration_Trmt,
                group = Hydration_Trmt),
            size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  labs(title = "CEWL Relative to Day of Parturiton",
       subtitle = "Parturiton = Day 0",
       x = "Day",
       y = "CEWL",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))
 
#uninformative, same reason as above with osmolarity. 

#Will now separate it out by individual. 

#101
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "101", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "101", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -11.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -12, to = 0, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 101; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")


#103
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "103", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "103", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -14, to = 0, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 103; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#104
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "104", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "104", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -13.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -14, to = 0, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 104; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#105
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "105", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "105", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -14, to = 0, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 105; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#112
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "112", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "112", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -12.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -13, to = 0, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 112; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#114
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "114", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "114", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -13, to = 0, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 114; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#115
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "115", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "115", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -14, to = 0, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 115; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#119
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "119", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "119", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -11.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -12, to = 0, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 119; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#124
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "124", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "124", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -3.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -15, to = 0, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 124; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#125
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "125", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "125", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -3, to = 10, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 125; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#126
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "126", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "126", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -5.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -16, to = 1, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 126; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#127
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "127", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "127", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -7.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -8, to = 1, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 127; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#128
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "128", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "128", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -1.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -2, to = 8, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 128; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#129
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "129", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "129", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -2, to = 7, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 129; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#131
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "131", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "131", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -3.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -4, to = 1, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 131; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#133
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "133", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "133", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -4, to = 1, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 133; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#134
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "134", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "cyan3") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "134", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "cyan3") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = -3.5, linetype = "dotted", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -4, to = 1, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 134; Hydration)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#135
ggplot() +
  geom_line(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "135", ],
            aes(x = Day_Relative,
                y = CEWL_g_m2h), color = "coral2") +
  geom_point(data = mother_data_all_final_3[mother_data_all_final_3$Indiv_ID == "135", ],
             aes(x = Day_Relative,
                 y = CEWL_g_m2h), color = "coral2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -1, to = 8, by = 1)) +
  labs(title = "CEWL Relative to Day of Parturiton (Indiv. 135; Control)",
       subtitle = "Day of Parturiton = Day 0",
       x = "Day",
       y = "CEWL")

#Boxplot for all mothers who gave birth to compare CEWL DAB

ggplot(data = mother_data_all_final_3[mother_data_all_final_3$Preg_Stage == "DAB", ]) +
  geom_boxplot(data = mother_data_all_final_3[mother_data_all_final_3$Preg_Stage == "DAB", ], 
               aes(x = Hydration_Trmt, y = CEWL_g_m2h, fill = Hydration_Trmt)) +
  labs(title = "CEWL Day After Birth",
       subtitle = "by Treatment Group", 
       x = "Hydration Treatment",
       y = "CEWL") +
  #scale_y_continuous(limits = c(280, 360),
                    # breaks = seq(from = 280, to = 360, by = 30)) +
  stat_compare_means(method = "t.test", aes(x = Hydration_Trmt, y = CEWL_g_m2h)) +
  stat_summary(aes(x = Hydration_Trmt, y = CEWL_g_m2h), fun = "mean", color= "red", shape = 16) +
  theme_classic() +
  theme(legend.position = "none")

#Doing analysis to look at the osmolarities relative to the number of days in treatment. 

#first, uploading the CSV with the new column of days relative to number of days in treatment
setwd("/Users/robinbedard/Desktop/Thesis/csv files") 
days_in_tmt <- read.csv("DAYS IN TREATMENT - Sheet1.csv")

library(stringr)
library(tidyr)

days_in_tmt_clean <- days_in_tmt %>%
  mutate(IndivID = as.factor(str_trim(IndivID)),
         CEWL.BloodCollectDate = as.Date(str_trim(CEWL.BloodCollectDate),
                                         format = "%m-%d-%y"),
         DaysInTrmt = as.numeric(DaysInTrmt),
         GaveBirth = as.factor(GaveBirth)
  ) %>%
  
  #rename all column names 
  
  dplyr::select(Indiv_ID = IndivID, CEWL_Blood_Collect_Date = CEWL.BloodCollectDate, Days_in_Treatment = DaysInTrmt, Gave_Birth = GaveBirth)  %>%
  
  fill(Indiv_ID)

#joining the final table with the days in treatment column to create a new final data set 

mother_data_all_final_4 <- left_join(mother_data_all_final_3, days_in_tmt_clean, by = c("Indiv_ID", "CEWL_Blood_Collect_Date"))

saveRDS(mother_data_all_final_4, "/Users/robinbedard/Desktop/Thesis/RDS_files/mother_data_all_final_3.RDS")

# averaging the osmolarities by days in treatments group

osmol_means_by_days_in_tmt <- mother_data_all_final_4 %>%
  group_by(Days_in_Treatment, Hydration_Trmt, Gave_Birth) %>%
  summarise(mean = mean(Plasma_Osmol_Rep_mean))

# making plots

# making a plot to standardize the x axis of number of days in treatment. This includes all individuals. 

# Make a modified copy of the original data to rename the values for the hydration treatment and if they gave brith so the labels are clear in the facet wrap.

mother_data_all_final_4_mod <- mother_data_all_final_4 %>%
  # Rename
  mutate(Hydration_Trmt = recode(Hydration_Trmt, "C" = "Control", "W" = "Hydration")) %>%
  mutate(Gave_Birth = recode(Gave_Birth, "Y" = "Gave Birth", "N" = "Did Not Birth"))

#plot for osmolarity.

ggplot() +
  geom_line(data = mother_data_all_final_4_mod,
            aes(x = Days_in_Treatment,
                y = Plasma_Osmol_Rep_mean,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 1) +
  facet_wrap(~Gave_Birth + Hydration_Trmt) +
  theme_classic() +
  labs(title = "Plasma Osmolarity Relative to Number of Days in Treatment",
       subtitle = "By treatment group and if snake gave birth",
       x = "Days in Treatment",
       y = "Osmolarity",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))


#plot for cewl.

ggplot() +
  geom_line(data = mother_data_all_final_4_mod,
            aes(x = Days_in_Treatment,
                y = CEWL_g_m2h,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 1) +
  facet_wrap(~Gave_Birth + Hydration_Trmt) +
  theme_classic() +
  labs(title = "CEWL Relative to Number of Days in Treatment",
       subtitle = "By treatment group and if snake gave birth",
       x = "Days in Treatment",
       y = "CEWL",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))

#below here I am trying to get the change in osmolarity to compare the changes over time. 

install.packages("xlsx")
library(xlsx)
write.csv(mother_data_all_final_4, "/Users/robinbedard/Desktop/Thesis/csv files/All.mother.data.csv")

#downloaded the sheet to calculate the change is osmolarity for each mother. Reuploading

relative_osmolarities <- read.csv("/Users/robinbedard/Desktop/Thesis/csv files/osmol_relative_change.csv")

relative_osmolarities <- relative_osmolarities[ , c(1, 2, 4)]

relative_osmolarities <- relative_osmolarities %>%
  mutate(Indiv_ID = as.factor(str_trim(Indiv_ID)),
         CEWL_Blood_Collect_Date = as.Date(str_trim(CEWL_Blood_Collect_Date),
                                         format = "%m/%d/%y"))

#joining it to the final df

mother_data_all_final_5 <- full_join(mother_data_all_final_4, relative_osmolarities, by = c("Indiv_ID", "CEWL_Blood_Collect_Date"))


relative_cewl <- read.csv("/Users/robinbedard/Desktop/Thesis/csv files/mother_changecewl.csv")
  
relative_cewl <- relative_cewl[ , c(1, 2, 4, 5)]
  
relative_cewl <- relative_cewl %>%
    mutate(Indiv_ID = as.factor(str_trim(Indiv_ID)),
           CEWL_Blood_Collect_Date = as.Date(str_trim(CEWL_Blood_Collect_Date),
                                             format = "%m/%d/%y"))
  
mother_data_all_final_6 <- full_join(mother_data_all_final_5, relative_cewl, by = c("Indiv_ID", "CEWL_Blood_Collect_Date"))

relative_osmol_means <- mother_data_all_final_6 %>%
  group_by(Day_Relative, Hydration_Trmt) %>%
  summarise(mean = mean(Change_in_osmol))

#figure showing the change is osmolarity for all mothers that gave birth relative to the day they gave birth

ggplot() +
  geom_line(data = mother_data_all_final_6,
            aes(x = Day_Relative,
                y = Change_in_osmol,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 1) +
  #geom_line(data = relative_osmol_means,
         #   aes(x = Day_Relative,
            #    y = mean,
             #   color = Hydration_Trmt,
              #  group = Hydration_Trmt),
           # size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_classic() +
  labs(title = "Change in Plasma Osmolarity Relative to Day of Parturiton",
       subtitle = "Parturiton = Day 0",
       x = "Day",
       y = "Change in Osmolarity",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))

#Figure showing change in osmolarity

ggplot() +
  geom_line(data = mother_data_all_final_6,
            aes(x = Days_in_Treatment,
                y = Change_in_osmol,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 1) +
  labs(title = "Change in Plasma Osmolarity Relative to Number of Days in Tmt",
       x = "Days in Treatment",
       y = "Change in Osmolarity",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))

#figure showing the change in osmolarity relative to the number of days in treatment. Separated out by mothers who gave birth and hydration treatment 

ggplot() +
  geom_line(data = mother_data_all_final_6,
            aes(x = Days_in_Treatment,
                y = Change_in_osmol,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 1) +
  facet_wrap(~Gave_Birth + Hydration_Trmt) +
theme_classic() +
  labs(title = "Change in Plasma Osmolarity Relative to Number of Days in Treatment",
       subtitle = "By treatment group and if snake gave birth",
       x = "Days in Treatment",
       y = "Change in Osmolarity",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))

#figure showing the change in osmolarity relative to the number of days in treatment separated out by snakes that did and did not give birth. 

ggplot() +
  geom_line(data = mother_data_all_final_6,
            aes(x = Days_in_Treatment,
                y = Change_in_osmol,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 1) +
  facet_wrap(~Gave_Birth) +
  theme_classic() +
  labs(title = "Change in Plasma Osmolarity Relative to Number of Days in Treatment",
       subtitle = "By treatment group and if snake gave birth",
       x = "Days in Treatment",
       y = "Change in Osmolarity",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))
 
#Figure showing the change of CEWL relative to the number of days in treatment for the two treatment groups. 

ggplot() +
  geom_line(data = mother_data_all_final_6,
            aes(x = Days_in_Treatment,
                y = Change_in_CEWL,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 1) +
  labs(title = "Change in CEWL Relative to Number of Days in Treatment",
       x = "Days in Treatment",
       y = "Change in CEWL",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))

#Figure showing the change in CEWL relative to the number of days in treatment separated out by if the snake gave birth and treatment. 

ggplot() +
  geom_line(data = mother_data_all_final_6,
            aes(x = Days_in_Treatment,
                y = Change_in_CEWL,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 1) +
  facet_wrap(~Gave_Birth + Hydration_Trmt)+
theme_classic() +
  labs(title = "Change in CEWL Relative to Number of Days in Treatment",
       subtitle = "By treatment group and if snake gave birth",
       x = "Days in Treatment",
       y = "Change in CEWL",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))

#figure showing change in cewl relative to days in treatment separated out by if snake gave birth

ggplot() +
  geom_line(data = mother_data_all_final_6,
            aes(x = Days_in_Treatment,
                y = Change_in_CEWL,
                color = Hydration_Trmt,
                group = Indiv_ID),
            alpha = 1) +
  facet_wrap(~Gave_Birth)+
  theme_classic() +
  labs(title = "Change in CEWL Relative to Number of Days in Treatment",
       subtitle = "By if snake gave birth",
       x = "Days in Treatment",
       y = "Change in CEWL",
       color = "Treatment") +
  scale_color_discrete(name = "Treatment", labels = c("Control", "Hydration"))



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

mother_data_all_final_7 <- mother_data_all_final_6 %>%
  mutate(calibrated_cloacal_temp = Tb_at_CEWL*TC_calibration$TC_A[2] +
           TC_calibration$TC_A[1])

summary(mother_data_all_final_7)

#Now I have a new column with the calibrated cloacal temperature at the time the measurement was taken. 

#SMI/Body condition for initial measurements of all pregnant snakes. I can use this equation that was done for all snakes (males, np females, etc.) that was created for the initial measurements of the pregnant snakes. 

#Need to create SMI equations for the different replicates measurements. So I will need another equation for 1WAC, 2WAC, DAB, 1WAB. Going to be created on separate RMD files for all different repeated measurements. 

saveRDS(mother_data_all_final_8, "/Users/robinbedard/Desktop/Thesis/RDS_files/mother_data_all_final_8.RDS")

#Body Condition equation for 1WAC
#case_when(Preg_Stage == "1WAC" ~ Mass_g * ((78.685/SVL_cm) ^ 2.539133)))
#case_when(Preg_Stage == "2WAC" ~ Mass_g * ((75.72/SVL_cm) ^ 2.183441))
#case_when(Preg_Stage == "DAB" ~ Mass_g * ((81.31667/SVL_cm) ^ 3.694591))
#case_when(Preg_Stage == "1WAB" ~ Mass_g * ((77.375/SVL_cm) ^ 1.905052)))

mother_data_all_final_8 <- mother_data_all_final_7 %>%
  mutate(SMI = Mass_g * ((77.55152/SVL_cm) ^ 3.098426))

ggplot(data = mother_data_all_final_8,
       aes(y = calibrated_cloacal_temp)) +
  geom_boxplot()
#random temperature that was above 80? remove it
mother_data_all_final_9 <- mother_data_all_final_8 %>%
  dplyr::filter(calibrated_cloacal_temp < 40)

summary(mother_data_all_final_9)

correl_df <- mother_data_all_final_9 %>%
  mutate(Indiv_ID = as.numeric(Indiv_ID)) %>%
  ungroup() %>%
  dplyr::select(Indiv_ID, Plasma_Osmol_Rep_mean, SVL_cm, Mass_g, calibrated_cloacal_temp, CEWL_g_m2h, msmt_temp_C, msmt_RH_percent, SMI, Day_Relative, Days_in_Treatment)

library("PerformanceAnalytics")

#Correlation plot to see which variables are related to one another
chart.Correlation(correl_df, histogram=TRUE, pch=19)

#initial models 

#model for osmolarity... first I have to find which categonical variables are related to osmolarity. 


summary(mother_data_all_final_9)

summary(lm(Plasma_Osmol_Rep_mean ~ Shed_Stage, data = mother_data_all_final_9))
#not correlated, no need to include in model
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ Preg_Stage, data = mother_data_all_final_9))
#No correlation
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ as.factor(CEWL_Blood_Collect_Date), mother_data_all_final_9))
#no correlation
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ Rookery_Capture_ID, data = mother_data_all_final_9))
#no correlation
summary(lm(Plasma_Osmol_Rep_mean ~ Hydration_Trmt, data = mother_data_all_final_9))
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ Hydration_Trmt, data = mother_data_all_final_9))
#says it doesnt but it should.... I will put it in the final model anyways and see what happens
TukeyHSD(aov(Plasma_Osmol_Rep_mean ~ as.factor(Date_Parturition), mother_data_all_final_9))
#no correlation
summary(lm(Plasma_Osmol_Rep_mean ~ Gave_Birth, data = mother_data_all_final_9))
#no correlation

OSMOL_LMM_01 <- lme4::lmer(data = mother_data_all_final_9,
                           Plasma_Osmol_Rep_mean ~
                             SVL_cm +
                             Mass_g +
                             calibrated_cloacal_temp +
                             Day_Relative +
                             #Hydration_Trmt +
                             (1|Indiv_ID))

drop1(OSMOL_LMM_01)

library(lmerTest)

summary(mother_data_all_final_9)

OSMOL_LMM_02 <- lme4::lmer(data = mother_data_all_final_9,
                           Plasma_Osmol_Rep_mean ~
                             SVL_cm +
                             Mass_g +
                             calibrated_cloacal_temp +
                             Day_Relative *
                             Hydration_Trmt +
                             (1|Indiv_ID))

summary(OSMOL_LMM_02)
drop1(OSMOL_LMM_02)
#Lower AIC is better. So the row "AIC" shows what the AIC would be if that variable was dropped from the model. So, if a variable is outside of the +/- 2 range and the value would be lower if dropped, then it should be taken out of the model. When looking at the AIC, you want to drop the lowest AIC's that are outside of the +/- 2 range from the "none" dropped AIC. Then, rerun the model and do it again to see if any others should be dropped after dropping that one. Continue process until no more can be dropped or until no more are outside of the +/- 2 range. Once none are outside of that range, you create two models that are equivalent in their predicitive power. One is just simpler than the other one. 

coef(summary(OSMOL_LMM_01))
anova(OSMOL_LMM_01)










#linear regression of CEWL and Osmolarity 

osm_cewl_lm <- lm(Plasma_Osmol_Rep_mean ~ CEWL_g_m2h, data = mother_data_all_final_8)
summary(osm_cewl_lm)

plot(osm_cewl_lm)
  
ggplot(mother_data_all_final_8, 
       aes(Plasma_Osmol_Rep_mean, CEWL_g_m2h)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = "Osmolarity (mmol/kg)",
       y = "CEWL (g/m^2h)",
       title = "Osmolarity vs. CEWL for all Preg. F. Measuremnts",
       subtitle = "p = 0.583, R^2 = 0.00383")

ggplot(data = mother_data_all_final_8,
       aes(x = msmt_temp_C, y = calibrated_cloacal_temp)) +
  geom_point()
