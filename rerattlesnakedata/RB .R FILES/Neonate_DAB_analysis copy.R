

neonate_dab_osmol <- readRDS("clean_Neonate_DAB_Osmolality.RDS")

neonate_dab_all <- readRDS("FinalNeonateDAB.RDS")

neonate_dab_all_final <- left_join(neonate_dab_all_final, neonate_dab_osmol, by = c("CEWL_Blood_Collect_Date", "Indiv_ID"))

all_data_CEWL <- readRDS("./CEWL data/CEWL_data_all_clean.RDS")

neonate_dab_all_final <- left_join(neonate_dab_all_final, all_data_CEWL, by = c("Indiv_ID" = "individual_ID", "CEWL_Blood_Collect_Date" = "date"))


install.packages("ggpubr")
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




ggplot(data = neonate_dab_all_final) +
  geom_bar(aes(x = Pee, fill = Treatment)) +
  facet_wrap(~Treatment) +
  labs(title = "'Pee' from Neonates Day After Birth",
       subtitle = "by Treatment Group", 
       x = "'Pee'?",
       y = "# of Individuals") +
  stat_compare_means(method = "t.test", aes(x = Treatment, y = Pee))

