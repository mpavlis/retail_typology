library(sf)
library(data.table)
library(dplyr)

pop_wz_sc <- read.csv("~/Dropbox/liverpool/retail_typology/data/wz/scots_pop.csv", stringsAsFactors = F)
setDT(pop_wz_sc)
pop_wz_ew <- read.csv("~/Dropbox/liverpool/retail_typology/data/wz/pop_ew.csv", stringsAsFactors = F, skip = 8, header = F, col.names = c("oa11", "pop"))
setDT(pop_wz_ew)

probs <- fread("~/Dropbox/liverpool/retail_typology/data/catchments_oa_lookup.csv")

all(unique(probs[substr(oa11, 1, 1) %in% c("E", "W"), ]$oa11 %in% pop_wz_ew$oa11)) # TRUE
all(unique(probs[substr(oa11, 1, 1) == "S", ]$oa11 %in% pop_wz_sc$oa11)) # F
sum(!unique(probs[substr(oa11, 1, 1) == "S", ]$oa11 %in% pop_wz_sc$oa11)) # 1

pop_wz <- rbindlist(list(pop_wz_ew, pop_wz_sc))

pop_wz_probs <- pop_wz[probs, on = "oa11", nomatch = 0]
# pop_wz_stats <- catchments_pop_wz[, {total_pop = sum(pop); list(total_pop, total_pop / sum(area))}, keyby = "cluster_id"]
# setnames(pop_wz_stats, 2:3, c("pop_total_wz", "pop_density_wz"))

pop_total <- pop_wz_probs[, sum(pop * max_huff_prob), keyby = "cluster_id"]
setnames(pop_total, "V1", "working_pop")
fwrite(pop_total, "~/Dropbox/liverpool/retail_typology/data/wz/working_pop_weighted.csv")
