################################################################################################################################################
#                                                Script to create vars for the function domain                                                 #
# Vars:                                                                                                                                        #
# 1. Anchor stores                                                                                                                             #
# 2. Premium retailers (designers & top department stores)                                                                                     #
# 3. Mass (big 20 + mid-range restaurant chains)                                                                                               #
# 4. Value (Aldi, Lidl, discount stores, takeaways, charity shops, pawnbrokers)                                                                #
# 5. Ethnic retail                                                                                                                             #
# 6. Kiosks and  (is_concession field)                                                                                                         #
# 7. Bars                                                                                                                                      #
################################################################################################################################################

library(sf)
library(stringr)
library(data.table)
library(parallel)

ru <- st_read("~/Dropbox/liverpool/catchments/catchments2/retail_units/retail_units_all_data_area_id_names_fin.shp", stringsAsFactors = F)
ru2 <- as.data.frame(ru)
setDT(ru2)
ru2[, store_name2 := str_to_lower(str_trim(store_name))]
setkey(ru2, cluster_id)

oas <- st_read("~/Dropbox/liverpool/GIS_Data/OAs_2011/oas11.shp", stringsAsFactors = F)
oas$area <- as.numeric(st_area(oas)) / 1000^2
oas_area <- aggregate(oas$area, by = list(oas$code), sum)
setDT(oas_area)
setnames(oas_area, names(oas_area), c("oa11", "catchment_area"))
setkey(oas_area, oa11)

catch_lookup <- fread("~/Dropbox/liverpool/retail_typology/data/catchments_oa_lookup.csv")
setkey(catch_lookup, oa11)

##### 1. Catchment area ########################################################################################################################

catch_lookup[oas_area, catchment_area := i.catchment_area, on = "oa11"]
catch_area <- catch_lookup[, sum(catchment_area), keyby = "cluster_id"]
setnames(catch_area, "V1", "catchment_area")

##### 2. Size (nr of stores) ###################################################################################################################

cluster_size <- ru2[, .N, by = "cluster_id"]
setnames(cluster_size, "N", "stores_nr")

##### 3. Anchor stores #########################################################################################################################
### a) For convenience the big 5 ###
big5_convenience <- list(asda = c("asda", "asda supercentre"),
                  morrisons = "morrisons",
                  sainsburys = c("sainsbury's", "sainsbury's market"),
                  tesco = c("tesco metro", "tesco", "tesco extra"),
                  waitrose = "waitrose")

big5_lookup_table <- as.data.frame(do.call(rbind, lapply(names(big5_convenience), function(store_name) cbind(store_name, big5_convenience[[store_name]]))), stringsAsFactors = F)
names(big5_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(big5_lookup_table)

### b) For comparison ###
comparison_stores <- list(john_lewis = c("john lewis at home", "john lewis", "john lewis of hungerford"),
                          debenhams = c("debenhams", "desire by debenhams"),
                          boots = "boots the chemist",
                          primark = "primark",
                          next_ = c("next", "next clearance", "next home", "next furniture"),
                          h_m = c("h&m", "h&m kids", "h&m discounts"),
                          harvey_nichols = "harvey nichols",
                          m_s = c("marks & spencer", "marks & spencer outlet", "marks & spencer home"),
                          zara = "zara",
                          b_q = "b&q",
                          ikea = "ikea",
                          homebase = c("homebase", "homebase design centre")
                          )

comparison_lookup_table <- as.data.frame(do.call(rbind, lapply(names(comparison_stores), function(store_name) cbind(store_name, comparison_stores[[store_name]]))), stringsAsFactors = F)
names(comparison_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(comparison_lookup_table)

### c)  for leisure ###
leisure_stores <- list(odeon = "odeon cinema",
                       vue_cinema = c("vue", "vue box office"),
                       cineworld = "cineworld"
                       )
leisure_lookup_table <- as.data.frame(do.call(rbind, lapply(names(leisure_stores), function(store_name) cbind(store_name, leisure_stores[[store_name]]))), stringsAsFactors = F)
names(leisure_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(leisure_lookup_table)

### d) Put everything together ###
anchor_dt <- rbindlist(list(big5_lookup_table, comparison_lookup_table, leisure_lookup_table))
# sanity check
all(anchor_dt$retailer_store_names %in% unique(ru2$store_name2)) # T
ru2[anchor_dt, anchor_stores := i.retailer_name, on = c(store_name2 = "retailer_store_names")]

anchor_pct <- ru2[, 100 * length(unique(anchor_stores)) / length(unique(anchor_dt$retailer_name)), by = "cluster_id"]

##### 2. Premium brands ########################################################################################################################
occur <- sapply(unique(ru2$store_name2), function(x) all(c("fat", "face") %in% unlist(strsplit(gsub(",", "", x), " "))))
ru2[store_name2 %in% names(occur[occur]), sort(table(store_name2), decreasing = T)]

premium_brands_list <- list(waitrose = "waitrose",
                            john_lewis = c("john lewis at home", "john lewis", "john lewis of hungerford"),
                            harvey_nichols = "harvey nichols",
                            laura_ashley = c("laura ashley", "laura ashley home"),
                            ted_baker = c("ted baker", "ted baker factory outlet", "ted baker & friends", "ted baker & moore", "ted baker pashion"),
                            tommy_hilfiger = c("tommy hilfiger", "tommy hilfiger kids"),
                            fat_face = "fat face",
                            superdry = c("superdry", "superdry ladieswear", "superdry menswear"),
                            seasalt = "seasalt",
                            jack_wills = "jack wills",
                            white_stuff = "white stuff",
                            crew_clothing = "crew clothing co.",
                            hugo_boss = "hugo boss",
                            cath_kidston = "cath kidston",
                            joules = "joules",
                            swarovski = "swarovski",
                            lacoste = "lacoste",
                            diesel = c("diesel", "diesel female", "diesel male"),
                            apple_store = "apple store",
                            bose = "bose"
                            )

premium_lookup_table <- as.data.frame(do.call(rbind, lapply(names(premium_brands_list), function(store_name) cbind(store_name, premium_brands_list[[store_name]]))), stringsAsFactors = F)
names(premium_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(premium_lookup_table)

all(premium_lookup_table$retailer_store_names %in% unique(ru2$store_name2)) # T
ru2[premium_lookup_table, premium_brands := i.retailer_name, on = c(store_name2 = "retailer_store_names")]

premium_brand_pct <- ru2[, 100 * length(unique(premium_brands)) / length(unique(premium_lookup_table$retailer_name)), by = "cluster_id"]

##### 3. Mass brands ###########################################################################################################################

mass_list <- list(asda = c("asda", "asda supercentre"),
                  sainsburys = c("sainsbury's", "sainsbury's market"),
                  tesco = c("tesco metro", "tesco", "tesco extra"),
                  currys = c("currys", "currys megastore", "currys pc world", "currys pc world megastore"),
                  argos = c("argos", "argos extra"),
                  boots = "boots the chemist",
                  next_ = c("next", "next clearance", "next home", "next furniture"),
                  tkmaxx = "tk maxx",
                  whsmith = c("whsmith", "whsmith books", "whsmith bookshop", "whsmith stationery"),
                  zara = "zara",
                  h_m = c("h&m", "h&m kids", "h&m discounts"),
                  jd_sports = "jd sports",
                  river_island = "river island",
                  costa = "costa",
                  caffe_nero = "caffe nero",
                  dominos = "domino's pizza",
                  pizza_express = "pizzaexpress",
                  mcdonalds = "mcdonald's",
                  starbucks = "starbucks coffee",
                  subway = "subway",
                  greggs = c("greggs", "greggs cafe", "greggs bakery outlet", "greggs the bakery"),
                  superdrug = "superdrug",
                  holland_barrett = "holland & barrett",
                  h.samuel = "h.samuel",
                  claires = c("claire's", "claire's outlet"),
                  topshop = "topshop",
                  game = "game",
                  perfume_shop = "the perfume shop",
                  kfc = "kfc",
                  fragrance_shop = "the fragrance shop"
                  )

mass_lookup_table <- as.data.frame(do.call(rbind, lapply(names(mass_list), function(store_name) cbind(store_name, mass_list[[store_name]]))), stringsAsFactors = F)
names(mass_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(mass_lookup_table)

all(mass_lookup_table$retailer_store_names %in% unique(ru2$store_name2)) # T
ru2[mass_lookup_table, mass_brands := i.retailer_name, on = c(store_name2 = "retailer_store_names")]

mass_brand_pct <- ru2[, 100 * length(unique(mass_brands)) / length(unique(mass_lookup_table$retailer_name)), by = "cluster_id"]

##### 4. Value stores ###########################################################################################################################

value_list <- list(aldi = "aldi",
                   lidl = "lidl",
                   iceland = "iceland",
                   primark = "primark",
                   poundland = "poundland",
                   poundworld = c("poundworld", "poundworld express", "poundworld extra", "poundworld plus"),
                   poundstretcher = "poundstretcher",
                   farmfoods = "farmfoods",
                   p99 = "99p stores",
                   pound_bakery = "pound bakery",
                   savers = "savers health & beauty",
                   b_m = c("b&m bargains", "b&m home store"),
                   home_bargains = "home bargains"
                   )

value_lookup_table <- as.data.frame(do.call(rbind, lapply(names(value_list), function(store_name) cbind(store_name, value_list[[store_name]]))), stringsAsFactors = F)
names(value_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(value_lookup_table)

all(value_lookup_table$retailer_store_names %in% unique(ru2$store_name2)) # T
ru2[value_lookup_table, value_brands := i.retailer_name, on = c(store_name2 = "retailer_store_names")]

value_brand_pct <- ru2[, 100 * length(unique(value_brands)) / length(unique(value_lookup_table$retailer_name)), by = "cluster_id"]

##### 5. Charity shops, Discount stores #########################################################################################################

charity <- ru2[,  100 * sum(category %in% c("Charity & Secondhand Shops")) / .N, by = "cluster_id"]
setnames(charity, "V1", "charity_pct")

##### 6. Betting + Pawnbrokers ##################################################################################################################

betting <- ru2[,  100 * sum(subcat %in% c("Bookmakers", "Pawnbrokers")) / .N, by = "cluster_id"]
setnames(betting, "V1", "betting_pct")

##### 7. Ethnic retail ##########################################################################################################################
rm_rest <- c("Restaurant - British", "Restaurant - Vegan", "Restaurant - Welsh", "Restaurant - Pizzeria", "Restaurant - Other",
             "Restaurant - Seafood", "Restaurant - Brasserie", "Restaurant - Vegetarian", "Restaurant - American", "Restaurant - English",
             "Restaurant - Creperie", "Restaurant - Scottish")
ethnic_restaurants <- ru2[, setdiff(grep("Restaurant - ", unique(subcat), value = T), rm_rest)]
ethnic_pct <- ru2[, 100 * sum(subcat %in% ethnic_restaurants) / .N, by = "cluster_id"]
setnames(ethnic_pct, "V1", "ethnic_pct")


# anchor_pct, premium_brand_pct, mass_brand_pct, value_brand_pct, betting, charity, ethnic_pct
setnames(anchor_pct, "V1", "anchor_pct")
setnames(premium_brand_pct, "V1", "premium_brand_pct")
setnames(mass_brand_pct, "V1", "mass_brand_pct")
setnames(value_brand_pct, "V1", "value_brand_pct")

out <- anchor_pct[premium_brand_pct, on = "cluster_id"]
out <- mass_brand_pct[out, on = "cluster_id"]
out <- value_brand_pct[out, on = "cluster_id"]
out <- betting[out, on = "cluster_id"]
out <- charity[out, on = "cluster_id"]
out <- ethnic_pct[out, on = "cluster_id"]
out <- cluster_size[out, on = "cluster_id"]
out <- catch_area[out, on = "cluster_id"]
# remove clusters without a catchment
out <- out[!is.na(out$area), ]

fwrite(out, "~/Dropbox/liverpool/retail_typology/analysis/indicators/retail_function.csv")
