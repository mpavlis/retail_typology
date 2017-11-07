#################################################################################################################################
#                                            Aportion pop to catchments using huff probs                                        #
#################################################################################################################################

library(data.table)

pop_dz15 <- fread("~/Dropbox/liverpool/retail_typology/data/pop_est/pop_dz/dz_pop_15.csv")
setnames(pop_dz15, "dz11", "lsoa11")
pop_lsoa15 <- fread("~/Dropbox/liverpool/retail_typology/data/pop_est/pop_lsoa/pop_lsoa_15.csv")
pop_msoa15 <- fread("~/Dropbox/liverpool/retail_typology/data/pop_est/pop_msoa/msoa_pop_15.csv")

lsoa_probs <- fread("~/Dropbox/liverpool/retail_typology/data/lsoa_probs.csv")
msoa_probs <- fread("~/Dropbox/liverpool/retail_typology/data/msoa_probs.csv")

pop_lsoa15_probs <- pop_lsoa15[lsoa_probs, on = "lsoa11", nomatch = 0]
pop_lsoa15_probs[, weighted_pop := total_pop * mean_huff_prob]

pop_msoa15_probs <- pop_msoa15[msoa_probs, on = "msoa11", nomatch = 0]
pop_msoa15_probs[, weighted_pop := total_pop * mean_huff_prob]

pop_dz15_probs <- pop_dz15[lsoa_probs, on = "lsoa11", nomatch = 0]
pop_dz15_probs[, weighted_pop := total_pop * mean_huff_prob]

fwrite(pop_dz15_probs, "~/Dropbox/liverpool/retail_typology/data/pop_est/pop_dz15_probs.csv")
fwrite(pop_lsoa15_probs, "~/Dropbox/liverpool/retail_typology/data/pop_est/pop_lsoa15_probs.csv")
fwrite(pop_msoa15_probs, "~/Dropbox/liverpool/retail_typology/data/pop_est/pop_msoa15_probs.csv")
