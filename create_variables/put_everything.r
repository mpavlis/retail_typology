library(data.table)

options(max.print = 3600)

composition <- fread("~/Dropbox/liverpool/retail_typology/data/composition/composition_input.csv")
diversity <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/national_diversity.csv")
local_diversity <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/local_diversity.csv")
setnames(local_diversity, "indie_pct", "local_indie_pct")
retail_function <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/retail_function.csv")
morphology <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/morphology.csv")[,.(cluster_id, roek)]
size <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/size.csv")
pop <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/pop_catchments_weighted.csv")
income <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/mean_income.csv")
unemployment <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/mean_weighted_jsa.csv")
vacancy <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/vacancy.csv")[,.(cluster_id, vac_pct_16, vac_change_pos)]
working_pop <- fread("~/Dropbox/liverpool/retail_typology/data/wz/working_pop_weighted.csv")
competitors_count <- fread("~/Dropbox/liverpool/retail_typology/data/competitors_count.csv")
retail_change <- fread("~/Dropbox/liverpool/retail_typology/data/retail_change.csv")[,.(cluster_id, service_change_pos)]
crime <- fread("~/Dropbox/liverpool/retail_typology/data/crime/count_crime.csv")[,.(id, crime_density)]
setnames(crime, "id", "cluster_id")

all_data <- composition[diversity, on = "cluster_id"]
all_data <- size[all_data, on = "cluster_id", nomatch = 0] # we should produce an output with as many entries as cluster catchments (i.e. 3110)
all_data <- local_diversity[all_data, on = "cluster_id"]
all_data <- retail_function[all_data, on = "cluster_id"]
all_data <- morphology[all_data, on = "cluster_id"]
all_data <- pop[all_data, on = "cluster_id"]
all_data <- income[all_data, on = "cluster_id"]
all_data <- unemployment[all_data, on = "cluster_id"]
all_data <- vacancy[all_data, on = "cluster_id"]
all_data <- working_pop[all_data, on = "cluster_id"]
all_data <- competitors_count[all_data, on = "cluster_id"]
all_data <- crime[all_data, on = "cluster_id"]
all_data <- retail_change[all_data, on = "cluster_id"]

####################################################### Deal with NAs ###################################################################

##### 1. service_change_pos #############################################################################################################
# some clusters did not have a service unit neither in 2014 nor in 2016, so there is no change between years, replace NA with 0
all_data[is.na(service_change_pos), service_change_pos := 0]

##### 2. crime_density ##################################################################################################################
# some clusters did not crime occurrence, replace NA with 0
all_data[is.na(crime_density), crime_density := 0]

##### 3. vac_pct_16 #####################################################################################################################
# some clusters did not have vacant units, replace NA with 0
all_data[is.na(vac_pct_16), vac_pct_16 := 0]

##### 4. vac_change_pos #################################################################################################################
# some clusters did not have vacant units, replace NA with 0
all_data[is.na(vac_change_pos), vac_change_pos := 0]

##### 5. local_services_diversity_pct ###################################################################################################
# some clusters did not have neighbours, replace NA values with 100 if the cluster has an occurrence, 0 if it doesn't
all_data[is.na(local_services_diversity_pct), local_services_diversity_pct := ifelse(services_diversity_pct != 0, 100, 0)]

##### 6. local_retail_diversity_pct #####################################################################################################
# some clusters did not have neighbours, replace NA values with 100 if the cluster has an occurrence, 0 if it doesn't
all_data[is.na(local_retail_diversity_pct), local_retail_diversity_pct := ifelse(retail_diversity_pct != 0, 100, 0)]

##### 7. local_small_multi_pct ##########################################################################################################
# some clusters did not have neighbours, replace NA values with 100 if the cluster has an occurrence, 0 if it doesn't
all_data[is.na(local_small_multi_pct), local_small_multi_pct := ifelse(small_multi_pct != 0, 100, 0)]

##### 8. local_large_multi_pct ##########################################################################################################
# some clusters did not have neighbours, replace NA values with 100 if the cluster has an occurrence, 0 if it doesn't
all_data[is.na(local_large_multi_pct), local_large_multi_pct := ifelse(large_multi_pct != 0, 100, 0)]

##### 9. local_indie_pct ################################################################################################################
# some clusters did not have neighbours, replace NA values with 100 if the cluster has an occurrence, 0 if it doesn't
all_data[is.na(local_indie_pct), local_indie_pct := ifelse(indie_pct != 0, 100, 0)]

##### 10. local_leisure_pct #############################################################################################################
# some clusters did not have neighbours, replace NA values with 100 if the cluster has an occurrence, 0 if it doesn't
all_data[is.na(local_leisure_pct), local_leisure_pct := ifelse(attractive_leisure_pct != 0, 100, 0)]

##### 11. local_convenience_pct #########################################################################################################
# some clusters did not have neighbours, replace NA values with 100 if the cluster has an occurrence, 0 if it doesn't
all_data[is.na(local_convenience_pct), local_convenience_pct := ifelse(attractive_convenience_pct != 0, 100, 0)]

##### 11. local_consumer_pct ############################################################################################################
# some clusters did not have neighbours, replace NA values with 100 if the cluster has an occurrence, 0 if it doesn't
all_data[is.na(local_consumer_pct), local_consumer_pct := ifelse(attractive_consumer_pct != 0, 100, 0)]

##### 12. local_departm_stores_pct ######################################################################################################
# some clusters did not have neighbours, replace NA values with 100 if the cluster has an occurrence, 0 if it doesn't
all_data[is.na(local_departm_stores_pct), local_departm_stores_pct := ifelse(department_stores_pct != 0, 100, 0)]

##### 13. local_comparison_pct ##########################################################################################################
# some clusters did not have neighbours, replace NA values with 100 if the cluster has an occurrence, 0 if it doesn't
all_data[is.na(local_comparison_pct), local_comparison_pct := ifelse(attractive_comparison_pct != 0, 100, 0)]


fwrite(all_data, "~/Dropbox/liverpool/retail_typology/analysis/indicators/all_data.csv")

# all_data_sele <- all_data[, .(cluster_id, crime_count, competitors, working_pop, vac_pct_15, vac_change_pos, mean_jsa, mean_income, resident_pop, catch_area, retail_area, roek, stores_nr, ethnic_pct,
#                          charity_pct, betting_pct, value_brand_pct, mass_brand_pct, anchor_pct, premium_brand_pct, local_services_diversity_pct, local_retail_diversity_pct,
#                          local_small_multi_pct, local_large_multi_pct, local_indie_pct, local_leisure_pct, local_convenience_pct, local_consumer_pct, local_departm_stores_pct,
#                          local_comparison_pct, clothing, house_goods, electrical, hobbies, food_retailers, ctn, off_license, chemists, restaurants, bars, caffes, entertainment,
#                          fitness, health_and_beauty, consumer_services, household_services, financial_services, recruitment_services, services_diversity_pct, retail_diversity_pct,
#                          small_multi_pct, large_multi_pct, indie_pct, attractive_leisure_pct, attractive_convenience_pct, attractive_consumer_pct, department_stores_pct, attractive_comparison_pct)]
# 
# all_data_sele[, working_pop_density := working_pop / catch_area]
# all_data_sele[, resident_pop_density := resident_pop / catch_area]
# all_data_sele[, crime_density := crime_count / retail_area]
# 
# cor(all_data_sele[,2:61])



