library(data.table)

setwd("~/Dropbox/liverpool/retail_typology/data/income")

pop_probs_sc <- fread("~/Dropbox/liverpool/retail_typology/data/pop_est/pop_dz15_probs.csv")
pop_probs_ew <- fread("~/Dropbox/liverpool/retail_typology/data/pop_est/pop_msoa15_probs.csv")

income_sc <- fread("income_estimates_scotland_2014.csv")[,.(dz11, income_pw_mean)]
setnames(income_sc, names(income_sc), c("lsoa11", "income_mean"))

income_ew <- fread("income_estimates_ew_2014.csv")[,.(msoa11cd, total_income_pw)]
setnames(income_ew, names(income_ew), c("msoa11", "income_mean"))

# lsoa_probs <- fread("~/Dropbox/liverpool/retail_typology/data/lsoa_probs.csv")
# msoa_probs <- fread("~/Dropbox/liverpool/retail_typology/data/msoa_probs.csv")

pop_probs_sc_income <- pop_probs_sc[income_sc, on = "lsoa11", nomatch = 0]
pop_probs_sc_income[,lsoa11 := NULL]
pop_probs_ew_income <- pop_probs_ew[income_ew, on = "msoa11", nomatch = 0]
pop_probs_ew_income[,msoa11 := NULL]

income_probs <- rbindlist(list(pop_probs_sc_income, pop_probs_ew_income))
setkey(income_probs, cluster_id)

pop_total <- income_probs[, sum(weighted_pop), by = "cluster_id"]
setnames(pop_total, "V1", "resident_pop")
fwrite(pop_total, "~/Dropbox/liverpool/retail_typology/analysis/indicators/pop_catchments_weighted.csv")

income_probs[, weight := weighted_pop / total_pop]
mean_weighted_income <- income_probs[, weighted.mean(income_mean, weight), by = "cluster_id"]
setnames(mean_weighted_income, "V1", "mean_income")
fwrite(mean_weighted_income, "~/Dropbox/liverpool/retail_typology/analysis/indicators/mean_income.csv")


