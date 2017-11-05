library(data.table)
setwd("~/Dropbox/liverpool/retail_typology/data/jsa")

# jsa <- fread("jsa_feb17_feb16.csv")
jsa <- fread("jsa_15_17.csv")
# there are NAs where number of applicants where <2 and were suppressed, replace with 1
jsa[, (2:21) := lapply(.SD, function(x) ifelse(is.na(x), 1, x)), .SDcols = 2:21]
# calculate mean unemployed for the period 2015 to 2017 per lsoa
mean_jsa <- jsa[, .(mean_jsa = rowMeans(.SD)), by = "V1"]
setnames(mean_jsa, "V1", "lsoa11")

pop_probs_sc <- fread("~/Dropbox/liverpool/retail_typology/data/pop_est/pop_dz15_probs.csv")
pop_probs_sc <- pop_probs_sc[total_pop != 0, ]
pop_probs_ew <- fread("~/Dropbox/liverpool/retail_typology/data/pop_est/pop_lsoa15_probs.csv")

pop_probs <- rbindlist(list(pop_probs_sc, pop_probs_ew))

# sanity check
all(pop_probs$lsoa11 %in% mean_jsa$lsoa11) # T
pop_probs[mean_jsa, mean_jsa := i.mean_jsa, on = "lsoa11"]
# pop_probs[, weight := weighted_pop / total_pop]
mean_weighted_jsa <- pop_probs[, .(mean_jsa = weighted.mean(mean_jsa, mean_huff_prob)), by = "cluster_id"]
fwrite(mean_weighted_jsa, "~/Dropbox/liverpool/retail_typology/analysis/indicators/mean_weighted_jsa.csv")
