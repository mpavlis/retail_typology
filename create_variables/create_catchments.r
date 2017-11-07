library(sf)
# library(parallel)
library(data.table)
# library(dplyr)
# library(igraph)
# source("~/Dropbox/git/postgis/postgis_functions.r")

##### 1. Lookup table for competitors based on the comparison catchments #################################################################################
comparison_catch1 <- fread("~/Dropbox/liverpool/catchments/catchments2/comparison/results/final/huff_probs.csv")[,.(cluster_id, oa11)]
comparison_catch2 <- fread("~/Dropbox/liverpool/catchments/catchments2/comparison/results/final/iso_catch_lookup_oas.csv")
comparison_catch <- unique(rbindlist(list(comparison_catch1, comparison_catch2)))
lookup_comp <- comparison_catch[comparison_catch, on = "oa11", allow.cartesian = T]
lookup_comp_a <- lookup_comp[cluster_id != i.cluster_id, .(cluster_id,i.cluster_id)]
lookup_comp_b <- lookup_comp_a[, .(i.cluster_id, cluster_id)]
setnames(lookup_comp_b, names(lookup_comp_b), names(lookup_comp_b)[2:1])
lookup_comp2 <- unique(rbindlist(list(lookup_comp_a, lookup_comp_b)))
fwrite(lookup_comp2, "~/Dropbox/liverpool/catchments/catchments2/comparison/results/final/competitors_lookup.csv")

##### 2. Lookup table for competitors based on the convenience catchments ################################################################################

convenience_catch <- fread("~/Dropbox/liverpool/catchments/catchments2/convenience/results/final/huff_probs.csv")[,.(cluster_id, oa11)]
convenience_catch <- unique(rbindlist(list(convenience_catch, comparison_catch)))
lookup_conv <- convenience_catch[convenience_catch, on = "oa11", allow.cartesian = T]
lookup_conv_a <- lookup_conv[cluster_id != i.cluster_id, .(cluster_id,i.cluster_id)]
lookup_conv_b <- lookup_conv_a[, .(i.cluster_id, cluster_id)]
setnames(lookup_conv_b, names(lookup_conv_b), names(lookup_conv_b)[2:1])
lookup_conv2 <- unique(rbindlist(list(lookup_conv_a, lookup_conv_b)))
lookup_conv2 <- lookup_conv2[! cluster_id %in% unique(comparison_catch$cluster_id), ]
fwrite(lookup_conv2, "~/Dropbox/liverpool/catchments/catchments2/convenience/results/final/competitors_lookup.csv")

all_competitors <- unique(rbindlist(list(lookup_comp2, lookup_conv2)))

all_competitors_count <- table(all_competitors$cluster_id)
all_competitors_count <- data.table(cluster_id = names(all_competitors_count), competitors = as.integer(all_competitors_count))

boundaries <- st_read("~/Dropbox/liverpool/boundaries/new_boundaries/all_boundaries_fin.gpkg", stringsAsFactors = F)
all(boundaries$id %in% all_competitors_count$cluster_id) # F
sum(!boundaries$id %in% all_competitors_count$cluster_id) # 213

absent <- data.table(cluster_id = boundaries[!boundaries$id %in% all_competitors_count$cluster_id, ]$id, competitors = 0)

all_competitors_count <- rbindlist(list(all_competitors_count, absent))
fwrite(all_competitors_count, "~/Dropbox/liverpool/retail_typology/data/competitors_count.csv")
fwrite(all_competitors, "~/Dropbox/liverpool/retail_typology/data/lookup_competitors.csv")

##### 3. Agreggate probs per LSOA, MSOA ##################################################################################################################

# lsoa <- st_read("~/Dropbox/liverpool/GIS_Data/LSOAs_2011/LSOA_2011_EW_BFC_V2.shp", stringsAsFactors = F)
# msoa <- st_read("~/Dropbox/liverpool/GIS_Data/MSOAs_2011/MSOA_2011_EW_BFC_V2.shp", stringsAsFactors = F)

lookup_scotland <- fread("~/Dropbox/liverpool/GIS_Data/lookup/oa_dz_iz_council_scotland.csv")[,.(OutputArea, DataZone, InterZone)]
setnames(lookup_scotland, names(lookup_scotland), c("oa11", "lsoa11", "msoa11"))
lookup_ew <- fread("~/Dropbox/liverpool/GIS_Data/lookup/lookup.csv")[,.(OA11CD,LSOA11CD,MSOA11CD)]
setnames(lookup_ew, names(lookup_ew), c("oa11", "lsoa11", "msoa11"))

all_lookup <- rbindlist(list(lookup_ew, lookup_scotland))

conv_probs <- fread("~/Dropbox/liverpool/catchments/catchments2/convenience/results/final/huff_probs.csv")[,.(cluster_id,oa11,huff_prob)]
comp_probs <- fread("~/Dropbox/liverpool/catchments/catchments2/comparison/results/final/huff_probs.csv")[,.(cluster_id,oa11,huff_prob)]

all_probs <- rbindlist(list(conv_probs, comp_probs))
all_probs <- all_probs[, max(huff_prob), keyby = c("cluster_id", "oa11")]
all_probs[, retail_class := ifelse(cluster_id %in% comp_probs$cluster_id, "comparison", "convenience")]
setnames(all_probs, "V1", "max_huff_prob")

fwrite(all_probs, "~/Dropbox/liverpool/retail_typology/data/catchments_oa_lookup.csv")

# sanity check
unique_oas <- unique(all_probs$oa11)
# all(unique_oas[substr(unique_oas, 1, 1) == "S"] %in% lookup_scotland$OutputArea) # T
# all(unique_oas[substr(unique_oas, 1, 1) != "S"] %in% lookup_ew$OA11CD) # T
all(unique_oas %in% all_lookup$oa11) # T

all_probs <- all_lookup[all_probs, on = "oa11"]

lsoa_probs <- all_probs[, mean(max_huff_prob), keyby = c("cluster_id", "lsoa11")]
setnames(lsoa_probs, "V1", "mean_huff_prob")
msoa_probs <- all_probs[, mean(max_huff_prob), keyby = c("cluster_id", "msoa11")]
setnames(msoa_probs, "V1", "mean_huff_prob")

fwrite(lsoa_probs, "~/Dropbox/liverpool/retail_typology/data/lsoa_probs.csv")
fwrite(msoa_probs, "~/Dropbox/liverpool/retail_typology/data/msoa_probs.csv")

##### 4. Build graph from lookups ########################################################################################################################
# lookup2 <- unique(rbind(lookup, lookup_comp))
# G <- graph.edgelist(lookup2)
# G2 <- simplify(G)
# graph_list <- decompose(G2)

##### 3. Build graph from lookups ########################################################################################################################

# comp_catch <- st_read("~/Dropbox/liverpool/catchments/catchments2/comparison/results/huff2b.shp", stringsAsFactors = F)
# 
# length(unique(comp_catch$cluster_id)) # 869
# conv_catch <- st_read("~/Dropbox/liverpool/catchments/catchments2/convenience/results/huff1.shp", stringsAsFactors = F)
# length(unique(conv_catch$cluster_id)) # 3477
# 
# common_clusters <- unique(comp_catch$cluster_id)[unique(comp_catch$cluster_id) %in% unique(conv_catch$cluster_id)]
# 
# london_clusters <- c("TC2549", "TC2691")
# "TC2691" %in% unique(comp_catch$cluster_id) # F
# "TC2691" %in% unique(conv_catch$cluster_id) # T
# edinburgh_clusters <- c("TC0131", "TC0136")
# "TC0136" %in% unique(comp_catch$cluster_id) # F
# "TC0136" %in% unique(conv_catch$cluster_id) # T
# 
# # Get all the OAs that are associated with a cluster as lookup table
# clusters_oas_lookup <- unique(rbind(unique(as.data.frame(comp_catch)[,c("cluster_id", "oa11")]),
#                                     unique(as.data.frame(conv_catch)[,c("cluster_id", "oa11")]),
#                                     data.frame(cluster_id = "TC2691", oa11 = comp_catch[comp_catch$cluster_id == "TC2549", ]$oa11, stringsAsFactors = F),
#                                     data.frame(cluster_id = "TC0136", oa11 = comp_catch[comp_catch$cluster_id == "TC0131", ]$oa11, stringsAsFactors = F)))
# 

 
# area_df <- data.frame(cluster_id = common_clusters,
#                       comp_area = unlist(mclapply(common_clusters, function(x) sum(st_area(comp_catch[comp_catch$cluster_id == x, ])) / 10^6, mc.cores = 8)),
#                       conv_area = unlist(mclapply(common_clusters, function(x) sum(st_area(conv_catch[conv_catch$cluster_id == x, ])) / 10^6, mc.cores = 8)),
#                       stringsAsFactors = F,
#                       row.names = NULL)

# comp_or_conv <- ifelse(area_df$comp_area > area_df$conv_area, "comparison", "convenience")
# table(comp_or_conv)
# # comparison convenience 
# #        611         211 
# 
# catchment_codes <- rbind.data.frame(as.data.frame(comp_catch)[! comp_catch$cluster_id %in% common_clusters[comp_or_conv == "convenience"], c("cluster_id", "oa11")],
#                                     as.data.frame(conv_catch)[! conv_catch$cluster_id %in% c(common_clusters[comp_or_conv == "comparison"], london_clusters, edinburgh_clusters) , c("cluster_id", "oa11")],
#                                     data.frame(cluster_id = "TC2691", oa11 = comp_catch[comp_catch$cluster_id == "TC2549", ]$oa11, stringsAsFactors = F),
#                                     data.frame(cluster_id = "TC0136", oa11 = comp_catch[comp_catch$cluster_id == "TC0131", ]$oa11, stringsAsFactors = F))

# write.csv(clusters_oas_lookup, "~/Dropbox/liverpool/retail_typology/data/catchments_oa_lookup.csv", row.names = F)

##### Calculate population density ###################################################################################
# pop_dens_eng <- fread("~/My Passport/backup/Census_11/pop/density_2011.csv")
# wales_pop <- fread("~/Dropbox/liverpool/retail_typology/data/wales_population.csv", skip = 1, select = c(2,5))
# scotland_pop <- fread("~/Dropbox/liverpool/retail_typology/data/scotland_population.csv", skip = 1, select = c(1,2))
# 
# sum(wales_pop$V2 %in% unique(c(unique(conv_catch$oa11), unique(comp_catch$oa11)))) # 7725

