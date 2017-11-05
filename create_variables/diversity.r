################################################################################################################################################
#                                                Script to create vars for the diversity domain                                                #                                                                                                                                 #
# I) National Diversity                                                                                                                        #
# Vars: % of                                                                                                                                   #
# 1. multiples (independent, small multiples [<10], large multiples [>10]),                                                                    #
# 2. retail diversity (comparison + convenience)                                                                                               #
# 3. services diversity (leisure + services)                                                                                                   #
# 4. total number of stores                                                                                                                    #
# 5. most popular comparison, convenience and leisure chains                                                                                   #                                                                                                                                             
################################################################################################################################################

library(sf)
library(stringr)
library(data.table)
library(clusterCrit)
library(parallel)

ru <- st_read("~/Dropbox/liverpool/catchments/catchments2/retail_units/retail_units_all_data_area_id_names_fin.shp", stringsAsFactors = F)
ru2 <- as.data.frame(ru)
setDT(ru2)

setkey(ru2, cluster_id)
ru2[, store_name2 := str_to_lower(str_trim(store_name))]
################################################################################################################################################
#                                                           National Diversity                                                                 #
################################################################################################################################################
comp_conv <- ru2[classifica %in% c("Comparison", "Convenience") & !category %in% c("Chemists, Toiletries & Health", "Discount & Surplus Stores"), ]
count_stores <- table(comp_conv$store_name)

##### 1. Independent multiples (1 unique store name for comparison and convenience) ############################################################

independent_stores <- names(which(count_stores == 1))
indie <- comp_conv[, 100 * sum(store_name %in% independent_stores) / .N, by = cluster_id]
setnames(indie, "V1", "indie_pct")

##### 2. Small multiples ( > 1 & <= 10 stores for comparison and convenience) ##################################################################

small_multi_names <- names(which(count_stores > 1 & count_stores <= 10))
small_multi <- comp_conv[, 100 * sum(store_name %in% small_multi_names) / .N, by = cluster_id]
setnames(small_multi, "V1", "small_multi_pct")

##### 3. Large multiples ( > 10 stores for comparison and convenience) #########################################################################

large_multi_names <- names(which(count_stores > 10))
large_multi <- comp_conv[, 100 * sum(store_name %in% large_multi_names) / .N, by = cluster_id]
setnames(large_multi, "V1", "large_multi_pct")

##### 4. Retail diversity (number of unique subcategory / total number of subcategory for comparison and convenience) ##########################

total_subcat <- comp_conv[, length(unique(subcat))]
retail_diversity <- comp_conv[, 100 * length(unique(subcat)) / total_subcat, by = cluster_id]
setnames(retail_diversity, "V1", "retail_diversity_pct")

##### 5. Services diversity (number of unique subcategory / total number of subcategory for leisure and services) ##############################

total_subcat_services <- ru2[classifica %in% c("Leisure", "Services"), length(unique(subcat))]
services_diversity <- ru2[classifica %in% c("Leisure", "Services"), 100 * length(unique(subcat)) / total_subcat_services, by = cluster_id]
setnames(services_diversity, "V1", "services_diversity_pct")

##### 6. Department stores (total number of stores) ############################################################################################

department_stores <- ru2[, 100 * sum(subcat == "Department Stores") / .N, by = cluster_id]
setnames(department_stores, "V1", "department_stores_pct")

##### 7. Attractive comparison (number of unique attractive stores / total number of unique attractive stores) #################################

attractive_comparison_list <- list(# john_lewis = c("john lewis at home", "john lewis", "john lewis of hungerford"),
                               # debenhams = c("debenhams", "desire by debenhams"),
                               boots = "boots the chemist",
                               # primark = "primark",
                               next_ = c("next", "next clearance", "next home", "next furniture"),
                               h_m = c("h&m", "h&m kids", "h&m discounts"),
                               # fraser = c("house of fraser", "house of fraser outlet", "house of fraser binns", "house of fraser beatties",
                               #            "highland house of fraser", "house of fraser outlet store"),
                               # harvey_nichols = "harvey nichols",
                               m_s = c("marks & spencer", "marks & spencer outlet", "marks & spencer home"),
                               # zara = "zara",
                               # b_q = "b&q",
                               # currys = c("currys", "currys megastore", "currys pc world", "currys pc world megastore"),
                               argos = c("argos", "argos extra"),
                               tkmaxx = "tk maxx",
                               # ikea = "ikea",
                               # b_m = c("b&m bargains", "b&m home store"),
                               home_bargains = c("home bargains"),
                               wilko = c("wilko"),
                               whsmith = c("whsmith", "whsmith books", "whsmith bookshop", "whsmith stationery"),
                               holland_barrett = "holland & barrett",
                               superdrug = "superdrug",
                               carphone = "the carphone warehouse",
                               ee = "ee",
                               poundland = "poundland",
                               new_look = "new look",
                               vodafone = "vodafone",
                               clarks = c("clarks", "clarks factory shop", "clarks outlet"),
                               shoe_zone = "shoe zone",
                               o2 = "o2",
                               h.samuel = "h.samuel",
                               jd_sports = "jd sports",
                               sports_direct = "sports direct",
                               peacocks = "peacocks",
                               carpetright = "carpetright",
                               dorothy_perkins = "dorothy perkins",
                               three_store = "3 store",
                               game = "game",
                               poundstretcher = "poundstretcher",
                               brighthouse = "brighthouse",
                               body_shop = c("the body shop", "the body shop depot", "the body shop outlet"),
                               pets_home = "pets at home",
                               river_island = "river island",
                               ryman_stationer = "ryman the stationer", 
                               bonmarche = "bonmarche",
                               poundworld = c("poundworld", "poundworld express", "poundworld extra", "poundworld plus"),
                               stores_99p = c("99p stores", "99p stores plus"),
                               mco = c("m&co", "m&co kids"),
                               waterstones = "waterstones",
                               the_works = c("the works", "the works outlet")
)

comparison_stores_lookup <- as.data.frame(do.call(rbind, lapply(names(attractive_comparison_list),
                                                                function(store_name) cbind(store_name, attractive_comparison_list[[store_name]]))),
                                          stringsAsFactors = F)
names(comparison_stores_lookup) <- c("retailer_name", "retailer_store_names")
setDT(comparison_stores_lookup)

ru2[comparison_stores_lookup, attractive_comp := i.retailer_name, on = c(store_name2 = "retailer_store_names")]

attractive_comparison <- ru2[, 100 * (length(unique(attractive_comp)) - 1) / length(attractive_comparison_list), by = "cluster_id"] # -1 to account for NA being among the unique values
setnames(attractive_comparison, "V1", "attractive_comparison_pct")

##### 8. Attractive convenience (number of unique attractive stores / total number of unique attractive stores) #################################

# occur <- sapply(unique(ru$store_name2), function(x) "clintons" %in% unlist(strsplit(x, " ")))
# ru[store_name2 %in% names(occur[occur]), sort(table(store_name2), decreasing = T)]

convenience_list <- list(asda = c("asda", "asda supercentre"),
                         morrisons = "morrisons",
                         sainsburys = c("sainsbury's", "sainsbury's market"),
                         tesco = c("tesco metro", "tesco", "tesco extra"),
                         waitrose = "waitrose",
                         aldi = "aldi",
                         lidl = "lidl",
                         coop = c("the co-operative food", "the midcounties co-operative food", "lincolnshire co-operative foodstores", "the southern co-operative food"),
                         ms =  "marks & spencer simply food",
                         iceland = "iceland",
                         greggs = c("greggs", "greggs bakery outlet", "greggs cafe", "greggs the bakery"),
                         card_factory = "card factory",
                         clintons = c("clintons", "simply clintons"),
                         lloyds_pharmacy = "lloydspharmacy",
                         thorntons = c("cafe thorntons", "thorntons", "thorntons chocolate cafe", "thorntons icecream parlour"),
                         spar = "spar",
                         farmfoods = "farmfoods",
                         heron_foods = "heron foods",
                         hallmark = "hallmark",
                         savers = "savers health & beauty")


convenience_lookup_table <- as.data.frame(do.call(rbind, lapply(names(convenience_list),
                                                         function(store_name) cbind(store_name, convenience_list[[store_name]]))),
                                   stringsAsFactors = F)
names(convenience_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(convenience_lookup_table)

ru2[convenience_lookup_table, attractive_conv := i.retailer_name, on = c(store_name2 = "retailer_store_names")]
attractive_convenience <- ru2[, 100 * (length(unique(attractive_conv)) - 1) / length(convenience_list), by = "cluster_id"]
setnames(attractive_convenience, "V1", "attractive_convenience_pct")

##### 9. Attractive leisure (number of unique attractive stores / total number of unique attractive stores) #################################

leisure_list <- list(starbucks = "starbucks coffee",
                     costa = "costa",
                     caffe_nero = "caffe nero",
                     mcdonalds = "mcdonald's",
                     kfc = "kfc",
                     subway = "subway",
                     dominos = "domino's pizza",
                     pizza_express = "pizzaexpress",
                     nandos = "nando's",
                     pizza_hut = c("pizza hut", "pizza hut delivery", "pizza hut express"),
                     prezzo = "prezzo",
                     frankie_bennys = "frankie & benny's",
                     zizzi = "zizzi",
                     wagamama = "wagamama",
                     ask_italian = "ask italian",
                     william_hill = "william hill",
                     ladbrokes = "ladbrokes",
                     coral = "coral",
                     betfred = "betfred",
                     paddy_power = "paddy power",
                     pret_a_manger = "pret a manger",
                     burger_king = "burger king",
                     papa_johns = "papa john's",
                     eat = "eat.",
                     cashino_gaming = "cashino gaming"
                     )

leisure_lookup_table <- as.data.frame(do.call(rbind, lapply(names(leisure_list),
                                                                function(store_name) cbind(store_name, leisure_list[[store_name]]))),
                                          stringsAsFactors = F)
names(leisure_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(leisure_lookup_table)

ru2[leisure_lookup_table, attractive_leisu := i.retailer_name, on = c(store_name2 = "retailer_store_names")]
attractive_leisure <- ru2[, 100 * (length(unique(attractive_leisu)) - 1) / length(leisure_list), by = "cluster_id"]
setnames(attractive_leisure, "V1", "attractive_leisure_pct")

##### 10. Attractive consumer services (number of unique attractive stores / total number of unique attractive stores) ########################

consumer_list <- list(barclays = "barclays bank plc",
                     natwest = "natwest",
                     lloyds = "lloyds bank",
                     hsbc = "hsbc",
                     santander = "santander",
                     nationwide_building_society = "nationwide building society",
                     halifax = "halifax plc",
                     tsb = "tsb bank",
                     post_office = "post office",
                     thomas_cook = "thomas cook",
                     thomson = "thomson",
                     specsavers = c("specksavers opticians", "specsavers hearcare", "specsavers hearing centre"),
                     sue_ryder_care = "sue ryder care",
                     timpson = "timpson",
                     halfords = c("halfords", "halfords autocentre", "halfords cycle republic")
                     )

consumer_lookup_table <- as.data.frame(do.call(rbind, lapply(names(consumer_list),
                                                            function(store_name) cbind(store_name, consumer_list[[store_name]]))),
                                      stringsAsFactors = F)
names(consumer_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(consumer_lookup_table)

ru2[consumer_lookup_table, attractive_cons := i.retailer_name, on = c(store_name2 = "retailer_store_names")]
attractive_consumer <- ru2[, 100 * (length(unique(attractive_cons)) - 1) / length(consumer_list), by = "cluster_id"]
setnames(attractive_consumer, "V1", "attractive_consumer_pct")

##### 11. Put everything together ####################################################################################################

out <- department_stores[attractive_comparison, on = "cluster_id"]
out <- attractive_consumer[out, on = "cluster_id"]
out <- attractive_convenience[out, on = "cluster_id"]
out <- attractive_leisure[out, on = "cluster_id"]
out <- indie[out, on = "cluster_id"]
out <- large_multi[out, on = "cluster_id"]
out <- small_multi[out, on = "cluster_id"]
out <- retail_diversity[out, on = "cluster_id"]
out <- services_diversity[out, on = "cluster_id"]
out[is.na(services_diversity_pct), services_diversity_pct := 0 ]
out[is.na(retail_diversity_pct), c("retail_diversity_pct", "small_multi_pct", "large_multi_pct", "indie_pct") := list(0, 0, 0, 0)]

fwrite(out, "~/Dropbox/liverpool/retail_typology/analysis/indicators/national_diversity.csv")
