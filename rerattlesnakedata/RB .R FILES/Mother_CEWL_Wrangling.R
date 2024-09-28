#Uploading the RSD file that has all of the CEWL data with outliers removed and filtering out only the mother measurements/joining them so that the data set is only of the mother data. 

mother_data_all_final <- readRDS("mother_data_all_final.RDS")

all_data_CEWL <- readRDS("./CEWL data/CEWL_data_all_clean.RDS")

mother_data_all_final_2 <- left_join(mother_data_all_final, all_data_CEWL, by = c("Indiv_ID" = "individual_ID", "CEWL_Blood_Collect_Date" = "date"))

saveRDS(mother_data_all_final_2, "/Users/robinbedard/Desktop/Thesis/mother_data_all_final_2")


