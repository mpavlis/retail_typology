################################################################################################################################################
#                                                Script to create vars for the diversity domain                                                #                                                                                                                                 #
# II) Local Diversity (Intersection of catchments)                                                                                             #
# Vars: % of                                                                                                                                   #
# 1. multiples (independent, small multiples [<10], large multiples [>10]),                                                                    #
# 2. retail diversity (comparison + convenience)                                                                                               #
# 3. services diversity (leisure + services)                                                                                                   #
# 4. total number of stores                                                                                                                    #
# 5. most popular comparison, convenience and leisure chains                                                                                   #
#                                                                                                                                              #
# Calculate total number of retail units per Area and TC and then divide each TC's by total in the area                                        #
# Highest prob per TC based on comp and conv to calc population stats                                                                          #
################################################################################################################################################

library(data.table)
library(sf)
library(stringr)

source("~/Dropbox/git/retail_typology/create_variables/attractive_list.r")

##### 1. Functions ############################################################################################################################

cluster_neighs <- function(lookup, cluster_id){
  if (! all(c("cluster_id.x", "cluster_id.y") %in% names(lookup))){
    stop("column names in lookup should be cluster_id.x and cluster_id.y")
  }
  # with the following every neighbouring retail centre is a competitor
  unique(c(cluster_id, lookup[cluster_id.x == cluster_id,]$cluster_id.y, lookup[cluster_id.y == cluster_id, ]$cluster_id.x))
  
  # with the following small retail centres are not competitors of large ones, but large ones are competitors
  # unique(c(cluster_id, lookup[cluster_id.x == cluster_id,]$cluster_id.y))
}

produce_pcts <- function(Dt, lookup, expr_numerator, expr_denominator, query){
  out <- data.table(id = unique(Dt$cluster_id), pct = 100)
  setkey(out, pct)
  for(i in as.integer(1:nrow(out))){
    cl <- out$id[i]
    clusters <- cluster_neighs(lookup, cl)
    if (length(clusters) > 1){
      set(out,
          i,
          "pct",
          100 * eval(parse(text = expr_numerator)) / eval(parse(text = expr_denominator))
      )
    }
  }
  out[is.na(pct), pct := 0]
  out
}

multiples_pct <- function(Dt, lookup, store_names){
  produce_pcts(Dt = Dt, lookup = lookup,
               expr_numerator = "Dt[cluster_id == cl, sum(store_name %in% query)]",
               expr_denominator = "Dt[cluster_id %in% clusters, sum(store_name %in% query)]",
               query = store_names)
}

diversity_pct <- function(Dt, lookup){
  produce_pcts(Dt = Dt, lookup = lookup,
               expr_numerator = "Dt[cluster_id == cl, length(unique(subcat))]",
               expr_denominator = "Dt[cluster_id %in% clusters, length(unique(subcat))]",
               query = NULL)
  
}

department_stores_pct <- function(Dt, lookup){
  produce_pcts(Dt = Dt, lookup = lookup,
               expr_numerator = "Dt[cluster_id == cl, sum(subcat == query)]",
               expr_denominator = "Dt[cluster_id %in% clusters, sum(subcat == query)]",
               query = "Department Stores")
}

attractive_pct <- function(Dt, lookup){
  produce_pcts(Dt = Dt,lookup = lookup,
               expr_numerator = "Dt[cluster_id == cl, length(na.omit(unique(attractive_retailers)))]",
               expr_denominator = "Dt[cluster_id %in% clusters, length(na.omit(unique(attractive_retailers)))]",
               query = NULL)
}

##### 2. Input data ###########################################################################################################################

lookup <- fread("~/Dropbox/liverpool/retail_typology/data/lookup_competitors.csv")
setnames(lookup, names(lookup), c("cluster_id.x", "cluster_id.y"))

all(unique(lookup$cluster_id.y) %in% unique(lookup$cluster_id.x)) # F
# The reason for the above result is that some big centres have only small neighbours, the latter are not competitors for the former

ru <- st_read("~/Dropbox/liverpool/catchments/catchments2/retail_units/retail_units_all_data_area_id_names_fin.shp", stringsAsFactors = F)
ru2 <- as.data.frame(ru)
setDT(ru2)
ru2 <- ru2[cluster_id %in% unique(c(lookup$cluster_id.x, lookup$cluster_id.y)),]
setkey(ru2, cluster_id)
ru2[, store_name2 := str_to_lower(str_trim(store_name))]

comp_conv <- ru2[classifica %in% c("Comparison", "Convenience") & ! category %in% c("Chemists, Toiletries & Health", "Discount & Surplus Stores") & ! store_name2 %in% c("halfords cycle republic", "sue ryder care"), ]
leis_serv <- ru2[classifica %in% c("Leisure", "Service") | store_name2 %in% c("halfords cycle republic", "sue ryder care"), ]

count_store_names <- table(comp_conv$store_name)

##### 3. Independent multiples (1 unique store name for comparison and convenience) ############################################################

independent_stores <- names(which(count_store_names == 1))
indie <- multiples_pct(Dt = comp_conv, lookup = lookup, store_names = independent_stores)
setnames(indie, 1:2, c("cluster_id", "indie_pct"))

##### 3. Small multiples ( > 1 & <= 10 stores for comparison and convenience) ##################################################################

small_multi_names <- names(which(count_store_names > 1 & count_store_names <= 10))
small_multi <- multiples_pct(Dt = comp_conv, lookup = lookup, store_names = small_multi_names)
setnames(small_multi, 1:2, c("cluster_id", "local_small_multi_pct"))

##### 4. Large multiples ( > 10 stores for comparison and convenience) #########################################################################

large_multi_names <- names(which(count_store_names > 10))
large_multi <- multiples_pct(Dt = comp_conv, lookup = lookup, store_names = large_multi_names)
setnames(large_multi, 1:2, c("cluster_id", "local_large_multi_pct"))

##### 5. Retail diversity (number of unique subcategory / total number of subcategory for comparison and convenience) ##########################

retail_diversity <- diversity_pct(Dt = comp_conv, lookup = lookup)
setnames(retail_diversity, 1:2, c("cluster_id", "local_retail_diversity_pct"))

##### 6. Services diversity (number of unique subcategory / total number of subcategory for leisure and services) ##############################

services_diversity <- diversity_pct(Dt = leis_serv, lookup = lookup)
setnames(services_diversity, 1:2, c("cluster_id", "local_services_diversity_pct"))

##### 7. Department stores (total number of stores) ############################################################################################

department_stores <- department_stores_pct(Dt = comp_conv, lookup = lookup)
setnames(department_stores, 1:2, c("cluster_id", "local_departm_stores_pct"))

##### 8. Attractive comparison (number of unique attractive stores / total number of unique attractive stores) #################################

comparison_stores_lookup <- as.data.frame(do.call(rbind, lapply(names(attractive_comparison_list),
                                                                function(store_name) cbind(store_name, attractive_comparison_list[[store_name]]))),
                                          stringsAsFactors = F)
names(comparison_stores_lookup) <- c("retailer_name", "retailer_store_names")
setDT(comparison_stores_lookup)

if ("attractive_retailers" %in% names(comp_conv)) comp_conv[, attractive_retailers := NULL]
all(comparison_stores_lookup$retailer_store_names %in% unique(comp_conv$store_name2)) # False, we removed chemists etc see above
comp_conv[comparison_stores_lookup, attractive_retailers := i.retailer_name, on = c(store_name2 = "retailer_store_names")]

attractive_comparison <- attractive_pct(Dt = comp_conv, lookup = lookup)
setnames(attractive_comparison, c("id", "pct"), c("cluster_id", "local_comparison_pct"))

##### 9. Attractive leisure (number of unique attractive stores / total number of unique attractive stores) ###################################

leisure_lookup_table <- as.data.frame(do.call(rbind, lapply(names(leisure_list),
                                                            function(store_name) cbind(store_name, leisure_list[[store_name]]))),
                                      stringsAsFactors = F)
names(leisure_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(leisure_lookup_table)

if ("attractive_retailers" %in% names(leis_serv)) leis_serv[, attractive_retailers := NULL]
all(leisure_lookup_table$retailer_store_names %in% unique(leis_serv$store_name2)) # True
leis_serv[leisure_lookup_table, attractive_retailers := i.retailer_name, on = c(store_name2 = "retailer_store_names")]
attractive_leisure <- attractive_pct(Dt = leis_serv, lookup = lookup)
setnames(attractive_leisure, c("id", "pct"), c("cluster_id", "local_leisure_pct"))

##### 10. Attractive consumer services (number of unique attractive stores / total number of unique attractive stores) ########################

consumer_lookup_table <- as.data.frame(do.call(rbind, lapply(names(consumer_list),
                                                             function(store_name) cbind(store_name, consumer_list[[store_name]]))),
                                       stringsAsFactors = F)
names(consumer_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(consumer_lookup_table)

if ("attractive_retailers" %in% names(leis_serv)) leis_serv[, attractive_retailers := NULL]
all(consumer_lookup_table$retailer_store_names %in% unique(leis_serv$store_name2)) # True
leis_serv[consumer_lookup_table, attractive_retailers := i.retailer_name, on = c(store_name2 = "retailer_store_names")]
attractive_consumer <- attractive_pct(Dt = leis_serv, lookup = lookup)
setnames(attractive_consumer, c("id", "pct"), c("cluster_id", "local_consumer_pct"))

##### 11. Attractive convenience (number of unique attractive stores / total number of unique attractive stores) ##############################

convenience_lookup_table <- as.data.frame(do.call(rbind, lapply(names(convenience_list),
                                                                function(store_name) cbind(store_name, convenience_list[[store_name]]))),
                                          stringsAsFactors = F)
names(convenience_lookup_table) <- c("retailer_name", "retailer_store_names")
setDT(convenience_lookup_table)

if ("attractive_retailers" %in% names(comp_conv)) comp_conv[, attractive_retailers := NULL]
all(convenience_lookup_table$retailer_store_names %in% unique(comp_conv$store_name2)) # F
# length_conv <- length(unique(convenience_lookup_table[retailer_store_names %in% unique(comp_conv$store_name2), ]$retailer_name))
comp_conv[convenience_lookup_table, attractive_retailers := i.retailer_name, on = c(store_name2 = "retailer_store_names")]
attractive_convenience <- attractive_pct(Dt = comp_conv, lookup = lookup)
setnames(attractive_convenience, c("id", "pct"), c("cluster_id", "local_convenience_pct"))

##### 11. Put everything together #############################################################################################################

out <- department_stores[attractive_comparison, on = "cluster_id"]
out <- attractive_consumer[out, on = "cluster_id"]
out <- attractive_convenience[out, on = "cluster_id"]
out <- attractive_leisure[out, on = "cluster_id"]
out <- indie[out, on = "cluster_id"]
out <- large_multi[out, on = "cluster_id"]
out <- small_multi[out, on = "cluster_id"]
out <- retail_diversity[out, on = "cluster_id"]
out <- services_diversity[out, on = "cluster_id"]
out[is.na(local_services_diversity_pct), c("local_services_diversity_pct", "local_leisure_pct", "local_consumer_pct") := list(0, 0, 0)]


fwrite(out, "~/Dropbox/liverpool/retail_typology/analysis/indicators/local_diversity.csv")
