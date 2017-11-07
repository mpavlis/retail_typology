library(sf)
library(data.table)

setwd("~/Dropbox/liverpool/retail_typology/data")

cl_14 <- st_read("clusters_14.gpkg", stringsAsFactors = F)
cl_16 <- st_read("clusters_16.gpkg", stringsAsFactors = F)

setDT(cl_14)
setDT(cl_16)

sort(unique(cl_14$businesstype1)[which(!unique(cl_14$businesstype1) %in% cl_16$category)])
# [1] "1106"                        "Medical"                     "Royal Mail Delivery Offices" "Shopping Centres & Markets"  "Transport"  

cl_14 <- cl_14[! businesstype1 %in% c('Transport', 'Shopping Centres & Markets', 'NULL')]
cl_16 <- cl_16[category != 'NULL', ]
cl_14[, businesstype1b := businesstype1]
cl_14[businesstype1 == 'Royal Mail Delivery Offices', businesstype1b := 'Employment & Post Offices']
cl_14[businesstype1 == '1106', businesstype1b := 'Employment & Post Offices']
cl_14[businesstype1 == 'Medical', businesstype1b := 'Employment & Post Offices']

all(unique(cl_14$businesstype1b) %in% unique(cl_16$category)) # T
all(unique(cl_16$category) %in% unique(cl_14$businesstype1b)) # T

cl_14[unique(cl_16[,.(category, classifica)]), classifica := i.classifica, on = c(businesstype1b = "category")]

services_14 <- cl_14[classifica == 'Service', .N, keyby = "cluster_id"]
setnames(services_14, "N", "service_14")
services_16 <- cl_16[classifica == 'Service', .N, keyby = "cluster_id"]
setnames(services_16, "N", "service_16")

services_14[services_16, service_16 := i.service_16, on = "cluster_id"]
services_14 <- services_14[!is.na(service_16)]
services_14[, service_change := service_14 - service_16]
services_14[, service_change_pos := service_change + abs(min(service_change))]

fwrite(services_14, "retail_change.csv")
