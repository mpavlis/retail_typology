################################################################################################################################################
#                                                Script to create vars for the composition domain                                              #
# Vars: % of                                                                                                                                   #
# 1. clothing and footwear,                                                                                                                    #
# 2. DIY and Household goods                                                                                                                   #
# 3. Electrical                                                                                                                                #
# 4. Recreational (toys, books and sport)                                                                                                      #
# 5. Other goods                                                                                                                               #
################################################################################################################################################

library(sf)
library(stringr)
library(data.table)
library(clusterCrit)
library(parallel)

ru <- st_read("~/Dropbox/liverpool/catchments/catchments2/retail_units/retail_units_all_data_area_id_names_fin.shp", stringsAsFactors = F)
ru2 <- as.data.frame(ru)
setDT(ru2)

################################################################################################################################################
#                                                           Comparison subdomain                                                               #
################################################################################################################################################

##### 1. Clothing and footwear #################################################################################################################
# footwear in category field as 'Footwear', clothing in category as 'Fashion & General Clothing'
clothing <- c('Footwear', 'Fashion & General Clothing')
sum(ru2$category %in% clothing) # 27684
clothing_prop <- ru2[, 100 * sum(category %in% clothing)/.N, keyby = "cluster_id"]

##### 2. DIY and Household #####################################################################################################################
# DIY in subcat as D.I.Y, Household in subcat as Household Stores
ru2[category == "DIY, Hardware, Builder's Merchants & Household Goods", unique(subcat)]
house_goods <- ru2[, 100 * sum(subcat %in% c("D.I.Y.", "Household Stores")) / .N, keyby = "cluster_id"]
  
##### 3. Electrical goods ######################################################################################################################
ru2[category == "Electrical Goods & Home Entertainment", unique(subcat)]
electrical <- ru2[, 100 * sum(category == "Electrical Goods & Home Entertainment") / .N, keyby = "cluster_id"]

##### 4. Recreational ##########################################################################################################################
# For books, either 'Books, Arts & Crafts, Stationery, Printers' from cat or 'Booksellers' from subcat
ru2[category %in% c('Sports, Toys, Cycle Shops & Hobbies', "Gifts, China & Leather Goods"), unique(subcat)]
hobbies <- ru2[, 100 * sum(category %in% c('Sports, Toys, Cycle Shops & Hobbies', "Gifts, China & Leather Goods")) / .N, keyby = "cluster_id"]

################################################################################################################################################
#                                                           Convenience subdomain                                                              #
################################################################################################################################################

##### 1. Food retailers ########################################################################################################################
groceries_cat <- sapply(gsub(",", "", unique(ru2$category)), function(x) any(str_to_lower(unlist(strsplit(x, " "))) == "groceries"))
groceries_cat[groceries_cat]

butchers_cat <- sapply(gsub(",", "", unique(ru2$category)), function(x) any(str_to_lower(unlist(strsplit(x, " "))) == "butchers"))
butchers_cat[butchers_cat]

bakers_cat <- sapply(gsub(",", "", unique(ru2$category)), function(x) any(str_to_lower(unlist(strsplit(x, " "))) == "bakers"))
bakers_cat[bakers_cat]

food_retailers_cat <- c(names(groceries_cat[groceries_cat]), names(butchers_cat[butchers_cat]), names(bakers_cat[bakers_cat]))

food_retailers <- ru2[, 100 * sum(category %in% food_retailers_cat) / .N, keyby = "cluster_id"]

##### 2. CTNs (Confectionery, Tobacco, Newsagents) #############################################################################################
ctn <- ru2[, 100 * sum(category == 'Confectionery, Tobacco, Newsagents') / .N, keyby = "cluster_id"]

##### 3. Off license ###########################################################################################################################
off_licence_log <- sapply(gsub(",","", unique(ru2$category)), function(x) any(c("off", "license") %in% str_to_lower(unlist(strsplit(x, " ")))))
off_licence_cat <- names(off_licence_log[off_licence_log])
off_license <- ru2[, 100 * sum(category == off_licence_cat) / .N, keyby = "cluster_id"]

##### 4. Chemists ##############################################################################################################################

chemists_log <- sapply(gsub(",","", unique(ru2$subcat)), function(x) any(c("chemist", "chemists") %in% str_to_lower(unlist(strsplit(x, " ")))))
chemists_cat <- unique(ru2$subcat)[which(chemists_log)]

chemists <- ru2[, 100 * sum(subcat %in% chemists_cat) / .N, keyby = "cluster_id"]

################################################################################################################################################
#                                                           Hospitality subdomain                                                              #
################################################################################################################################################

##### 1. Restaurants ###########################################################################################################################
restaurant_log <- sapply(gsub(",","", unique(ru2$category)), function(x) "restaurants" %in% str_to_lower(unlist(strsplit(x, " "))))
restaurant_cat <- names(restaurant_log[restaurant_log])
restaurants <- ru2[, 100 * sum(category == restaurant_cat) / .N, keyby = "cluster_id"]

##### 2. Bars and pubs #########################################################################################################################
bars_log <- sapply(gsub(",","", unique(ru2$category)), function(x) any(c("bars", "pubs") %in% str_to_lower(unlist(strsplit(x, " ")))))
bars_cat <- unique(ru2$category)[which(bars_log)]
bars <- ru2[, 100 * sum(category == bars_cat) / .N, keyby = "cluster_id"]

##### 3. Cafes and fast food ###################################################################################################################
caffes_ff_log <- sapply(gsub(",","", unique(ru2$category)),
                        function(x) any(c("cafes","cafe", "coffee", "fast") %in% str_to_lower(unlist(strsplit(x, " ")))))
caffes_ff_cat <- unique(ru2$category)[which(caffes_ff_log)]
caffes_ff <- ru2[, 100 * sum(category == caffes_ff_cat) / .N, keyby = "cluster_id"]

##### 4. Entertainment #########################################################################################################################
entertainment_log <- sapply(gsub(",","", unique(ru2$category)), function(x) "entertainment" %in% str_to_lower(unlist(strsplit(x, " "))))
entertainment_cat <- unique(ru2$category)[which(entertainment_log)][2]
entertainment <- ru2[, 100 * sum(category == entertainment_cat) / .N, keyby = "cluster_id"]

##### 5. Fitness centres #######################################################################################################################
finess_subcats <- c("Health Clubs", "Sports Clubs", "Leisure Centres & Swimming Baths")
fitness <- ru2[, 100 * sum(subcat %in% finess_subcats) / .N, keyby = "cluster_id"]

##### 6. Health and beauty services ############################################################################################################
health_and_beauty_log <- sapply(gsub(",","", unique(ru2$subcat)),
                                function(x) all(c("health", "beauty") %in% str_to_lower(unlist(strsplit(x, " ")))))
health_and_beauty_cat <- unique(ru2$subcat)[which(health_and_beauty_log)]
health_and_beauty <- ru2[, 100 * sum(subcat %in% health_and_beauty_cat) / .N, keyby = "cluster_id"]

################################################################################################################################################
#                                              Other Consumer Services subdomain                                                               #
################################################################################################################################################

##### 1. Consumer services #####################################################################################################################
consumer_cats <- c("Banks, Financial Services & Building Societies", "Travel Agents & Tour Operators", "Estate Agents & Auctioneers")
consumer_services <- ru2[, 100 * sum(category %in% consumer_cats) / .N, keyby = "cluster_id"]

##### 2. Household services ####################################################################################################################
household_cats <- c("Launderettes, Dry Cleaners & Other", "Car & Motorbike Showrooms", "Household & Home", "Locksmiths, Clothing Alterations & Shoe Repairs")
household_services <- ru2[, 100 * sum(category %in% household_cats) / .N, keyby = "cluster_id"]

##### 3. Business services #####################################################################################################################
# check_wholesale <- sapply(str_to_lower(gsub(",","",ru$store_name)), function(x) "wholesale" %in% unlist(strsplit(x, " "))) # only 27
# 
# check_legal <- sapply(str_to_lower(gsub(",","",ru$store_name)), function(x) "legal" %in% unlist(strsplit(x, " "))) 
# # perhaps "Estate Agents & Auctioneers" from category, "Financial Advisors", "Insurance Agents", "Mortgage Companies & Advisors",  from subcat 
financial_services <- ru2[, 100 * sum(category %in% "Estate Agents & Auctioneers" | subcat %in% c("Financial Advisors", "Insurance Agents", "Mortgage Companies & Advisors")) / .N, keyby = "cluster_id"]
# 
# recruitment_subcat <- "Recruitment Agencies"

recruitment_services <- ru2[, 100 * sum(subcat %in% "Recruitment Agencies") / .N, keyby = "cluster_id"]

out <- data.table(cluster_id = clothing_prop$cluster_id, 
                  clothing = clothing_prop$V1,
                  house_goods = house_goods$V1,
                  electrical = electrical$V1,
                  hobbies = hobbies$V1,
                  food_retailers = food_retailers$V1,
                  ctn = ctn$V1,
                  off_license = off_license$V1,
                  chemists = chemists$V1,
                  restaurants = restaurants$V1,
                  bars = bars$V1,
                  caffes = caffes_ff$V1,
                  entertainment = entertainment$V1,
                  fitness = fitness$V1,
                  health_and_beauty = health_and_beauty$V1,
                  consumer_services = consumer_services$V1,
                  household_services = household_services$V1,
                  financial_services = financial_services$V1,
                  recruitment_services = recruitment_services$V1)

fwrite(out, "~/Dropbox/liverpool/retail_typology/data/composition/composition_input.csv")

########################################################################################################################
# get_cl_kmeans <- function(k, data_mat, criterion = "S_Dbw"){
#   
#   crit_vals <- vector("numeric", length = 1000)
#   seed_vals <- 234:1233
#   for (i in 1:1000){
#     set.seed <- seed_vals[i]
#     temp <- kmeans(data_mat, centers=k, iter.max=10000)
#     # if (temp$tot.withinss < fit){
#     crit_vals[i] <- as.numeric(intCriteria(data_mat, as.integer(temp$cluster), criterion))
#   }
#   
#   idx_rm <- is.na(crit_vals)
#   if (any(idx_rm)){
#     crit_vals <- crit_vals[!idx_rm]
#     seed_vals <- seed_vals[!idx_rm]
#   }
#   idx_best <- bestCriterion(crit_vals, criterion)
#   return(c(crit_vals[idx_best], seed_vals[idx_best]))
# }
# 
# composition_dt <- fread("/home/michalis/Dropbox/liverpool/retail_typology/data/composition/composition_input.csv")
# comp_std <- composition_dt[, lapply(.SD, scale), .SDcols = 2:19]
# 
# # Determine number of clusters
# # wss <- composition_dt[, (.N - 1) * sum(unlist(lapply(.SD, var))), .SDcols = 2:17] #(nrow(composition_dt)-1) * sum(apply(composition_dt,2,var))
# # cal_har_crit <- vector("numeric", 14)
# # for (i in 2:15){
# #   clustering <- kmeans(composition_dt[,2:17], centers=i)
# #   wss[i] <- sum(clustering$withinss)
# #   cal_har_crit[i-1] <- intCriteria(as.matrix(composition_dt[,2:17]), clustering$cluster, "Calinski_Harabasz")
# # }
# # plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
# 
# 
# test_clusters <- do.call(rbind, mclapply(2:18, get_cl_kmeans, as.matrix(comp_std), "S_Dbw", mc.cores = 10))
# # test_clusters
# #          [,1] [,2]
# # [1,] 1.446936  240
# # [2,] 1.590649  589
# # [3,] 1.796437  927
# # [4,] 1.914620  291
# # [5,] 1.120690  950
# # [6,] 1.476037  247
# # [7,] 1.456968  430
# # [8,] 1.373320 1115
# # [9,] 1.335035  978
# # [10,] 1.299188  417
# # [11,] 1.247035  556
# # [12,] 1.205545  484
# # [13,] 1.262809  659
# # [14,] 1.448658  833
# # [15,] 1.443864  521
# # [16,] 1.337600  956
# # [17,] 1.314078  555
# idx_best <- bestCriterion(test_clusters[,1], "S_Dbw") # 5 -> 6 clusters seems more appropriate
# test_clusters[idx_best,] # 1.12069 950 -> 950 seed
# 
# set.seed(950)
# clustering <- kmeans(comp_std, centers = 6, iter.max = 10000)
# table(clustering$cluster)
# # 1    2    3    4    5    6 
# # 410 1593  232  729  175  190 
# 
# composition_dt[, clustering := clustering$cluster]
# 
# fwrite(composition_dt, "~/Dropbox/liverpool/retail_typology/data/composition/composition_clustering_6.csv")
# 
# ru[composition_dt, clustering := i.clustering, on = "cluster_id"]
# 
# ru <- st_as_sf(as.data.frame(ru), crs = 27700)
# 
# st_write(ru, "/home/michalis/Dropbox/liverpool/retail_typology/data/composition/retail_units_clustering_6.shp", driver = "ESRI Shapefile")
# 
# comp_dt_long <- data.table::melt(composition_dt, 20, 2:19)
# 
# means <- aggregate(comp_dt_long$value, by = list(comp_dt_long$clustering, comp_dt_long$variable), mean)
# names(means) <- c("cluster", "category", "mean_pct")
# means$cluster <- as.factor(means$cluster)
# 
# ggplot(means, aes(x = category, fill = cluster, y = mean_pct)) + geom_dotplot(binaxis = "y", stackdir = "center") +
#   ylab("Mean of Variable (%)") + scale_fill_discrete(name = "Cluster ID") 
#   # geom_segment(aes(x=1,y=0.2523363,xend=1,yend=0.2959657 ),colour="black",size=1) +
#   # geom_segment(aes(x=2,y=0.2415900,xend=2,yend=0.3934252 ),colour="black",size=1) +
#   # geom_segment(aes(x=3,y=0.2418190,xend=3,yend=0.4512376 ),colour="black",size=1) +
#   # geom_segment(aes(x=4,y=0.3988005,xend=4,yend=0.2448053 ),colour="black",size=1) +
#   # geom_hline(yintercept=0.2566891)