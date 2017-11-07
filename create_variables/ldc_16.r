library(sf)
library(data.table)

source("~/Dropbox/git/postgis/postgis_functions.r")

##### 1. 2016 #################################################################################################################
retail <- fread("~/Dropbox/liverpool/GIS_Data/retail/ldc_2016_raw.csv")
nspl <- fread("~/Dropbox/liverpool/GIS_Data/lookup/nspl_16.csv")

retail[, lat := as.numeric(Latitude)]
retail[, lon := as.numeric(Longitude)]
sum(is.na(retail$lat)) # 95336
sum(unique(retail$PostCode) %in% nspl$`Postcode 1`)
sum(gsub(" ", "", unique(retail$PostCode)) %in% gsub(" ", "", nspl$`Postcode 1`))

retail[, PostCode := gsub(" ", "", PostCode)]
nspl[, PostCode := gsub(" ", "", `Postcode 1`)]
retail[nspl, c("lat_nspl", "lon_nspl") := list(i.Latitude, i.Longitude), on = "PostCode"]

retail[, lat2 := ifelse(is.na(lat), lat_nspl, lat)]
retail[, lon2 := ifelse(is.na(lon), lon_nspl, lon)]
retail2 <- retail[!is.na(lat2), ]
retail2 <- retail2[lat2 < 60.40, ]
retail2[, lat_nspl := NULL]
retail2[, lon_nspl := NULL]
retail2[, lat := NULL]
retail2[, lon := NULL]
setnames(retail2, c("lat2", "lon2"), c("lat", "lon"))

fwrite(retail2, "~/Dropbox/liverpool/retail_typology/data/ldc_2016_add_lat_lon.csv")

# retail_sf <- st_as_sf(retail2, coords = c("lon", "lat"), crs = 4326, agr = "constant")
# st_write(retail_sf, "~/Dropbox/liverpool/retail_typology/data/ldc_2016_wgs84.shp", driver = "ESRI Shapefile")
# 
con <- dbConnect(PostgreSQL(), dbname = "retail_typology", user = "user", password = "pwd")
import_or_append(con, working_dirs = "~/Dropbox/liverpool/retail_typology/data", shp_names = "ldc16.shp", srid = 27700, table_name = "ldc16")
import_or_append(con, working_dirs = "~/Dropbox/liverpool/boundaries/new_boundaries", shp_names = "all_boundaries_fin.shp", srid = 27700, table_name = "boundaries")

dbExecute(con, "CREATE TABLE ldc16_bound_id AS SELECT t1.*, t2.id cluster_id FROM ldc16 t1, boundaries t2 WHERE ST_DWithin(t1.geom, t2.geom, 15)")

retail <- st_read_db(con, "ldc16_bound_id")
st_write(retail, "clusters_16.gpkg")

##### 2. 2015 #################################################################################################################
# retail <- fread("~/Dropbox/liverpool/cities/raw_data/data/LDC2015_all.csv")
# retail2 <- data.frame(premise = retail$Premise,
#                       occupier = ifelse(is.na(retail$Occupier.x), retail$Occupier.y, retail$Occupier.x),
#                       name = ifelse(is.na(retail$Name.x), retail$Name.y, retail$Name.x),
#                       classifica = ifelse(is.na(retail$Classifica), retail$Classification, retail$Classifica),
#                       category = ifelse(is.na(retail$Category.x), retail$Category.y, retail$Category.x),
#                       subcat = ifelse(is.na(retail$SubCategor), retail$SubCategory, retail$SubCategor),
#                       care_of = ifelse(is.na(retail$CareOf.x), retail$CareOf.y, retail$CareOf.x),
#                       care_ofid = ifelse(is.na(retail$CareOfId.x), retail$CareOfId.y, retail$CareOfId.x),
#                       is_concess = ifelse(is.na(retail$IsConcessi), retail$IsConcession, retail$IsConcessi),
#                       street_no = ifelse(is.na(retail$StreetNo.x), retail$StreetNo.y, retail$StreetNo.x),
#                       street = ifelse(is.na(retail$Street.x), retail$Street.y, retail$StreetNo.x),
#                       town = ifelse(is.na(retail$Town.x), retail$Town.y, retail$Town.x),
#                       postcode = ifelse(is.na(retail$PostCode.x), retail$PostCode.y, retail$PostCode.x),
#                       latitude = ifelse(is.na(retail$Latitude.x), retail$Latitude.y, retail$Latitude.x),
#                       longitude = ifelse(is.na(retail$Longitude.x), retail$Longitude.y, retail$Longitude.x),
#                       voa = ifelse(is.na(retail$Voa.x), retail$Voa.y, retail$Voa.x),
#                       voa_rate = ifelse(is.na(retail$Voa_Busine), retail$Voa_BusinessRate, retail$Voa_Busine),
#                       field_resea = ifelse(is.na(retail$FieldResea), retail$FieldResearched, retail$FieldResea),
#                       clgid = ifelse(is.na(retail$ClgId.x), retail$ClgId.y, retail$ClgId.x),
#                       clgname = ifelse(is.na(retail$CLGName.x), retail$CLGName.y, retail$CLGName.x),
#                       shop_id = ifelse(is.na(retail$ShoppingAr), retail$ShoppingAreaID, retail$ShoppingAr),
#                       shop_name = ifelse(is.na(retail$Shopping_1), retail$ShoppingAreaName, retail$Shopping_1),
#                       stringsAsFactors = F
#                       )
# setDT(retail2)
# nspl <- fread("~/Dropbox/liverpool/GIS_Data/lookup/nspl_16.csv")
# 
# retail2[, lat := as.numeric(latitude)]
# retail2[, lon := as.numeric(longitude)]
# sum(is.na(retail2$lat)) # 90905
# sum(unique(retail2$postcode) %in% nspl$`Postcode 1`) # 77761
# sum(gsub(" ", "", unique(retail2$postcode)) %in% gsub(" ", "", nspl$`Postcode 1`)) # 150953
# 
# retail2[, postcode := gsub(" ", "", postcode)]
# nspl[, postcode := gsub(" ", "", `Postcode 1`)]
# retail2[nspl, c("lat_nspl", "lon_nspl") := list(i.Latitude, i.Longitude), on = "postcode"]
# 
# retail2[, lat2 := ifelse(is.na(lat), lat_nspl, lat)]
# retail2[, lon2 := ifelse(is.na(lon), lon_nspl, lon)]
# retail2 <- retail2[!is.na(lat2), ]
# retail2[, lat_nspl := NULL]
# retail2[, lon_nspl := NULL]
# retail2[, lat := NULL]
# retail2[, lon := NULL]
# setnames(retail2, c("lat2", "lon2"), c("lat", "lon"))
# 
# fwrite(retail2, "~/Dropbox/liverpool/retail_typology/data/ldc_2015_add_lat_lon.csv")
# 
# # retail_sf <- st_as_sf(retail2, coords = c("lon", "lat"), crs = 4326, agr = "constant")
# # st_write(retail_sf, "~/Dropbox/liverpool/retail_typology/data/ldc_2016_wgs84.shp", driver = "ESRI Shapefile")
# # 
# con <- dbConnect(PostgreSQL(), dbname = "retail_typology", user = "user", password = "pwd")
# import_or_append(con, working_dirs = "~/Dropbox/liverpool/retail_typology/data", shp_names = "ldc15.shp", srid = 27700, table_name = "ldc15")
# 
# dbExecute(con, "CREATE TABLE ldc15_bound_id AS SELECT t1.*, t2.cl_id cluster_id FROM ldc15 t1, boundaries t2 WHERE ST_Within(t1.geom, t2.geom)")
# 
# retail <- st_read_db(con, "ldc15_bound_id")
# st_write(retail, "~/Dropbox/liverpool/retail_typology/data/clusters_15.shp", driver = "ESRI Shapefile")

##### 2. 2014 ###############################################################################################################
# retail <- fread("~/Dropbox/liverpool/retail_typology/data/ldc_2016_raw.csv")
# nspl <- fread("~/Dropbox/liverpool/GIS_Data/lookup/nspl_16.csv")
# 
# retail[, lat := as.numeric(Latitude)]
# retail[, lon := as.numeric(Longitude)]
# sum(is.na(retail$lat)) # 95336
# sum(unique(retail$PostCode) %in% nspl$`Postcode 1`)
# sum(gsub(" ", "", unique(retail$PostCode)) %in% gsub(" ", "", nspl$`Postcode 1`))
# 
# retail[, PostCode := gsub(" ", "", PostCode)]
# nspl[, PostCode := gsub(" ", "", `Postcode 1`)]
# retail[nspl, c("lat_nspl", "lon_nspl") := list(i.Latitude, i.Longitude), on = "PostCode"]
# 
# retail[is.na(lat), lat := lat_nspl]
# retail[is.na(lat), lon := lon_nspl]
# retail2 <- retail[!is.na(lat), ]
# retail2[, lat_nspl := NULL]
# retail2[, lon_nspl := NULL]
# 
# fwrite(retail2, "~/Dropbox/liverpool/retail_typology/data/ldc_2016_add_lat_lon.csv")

# retail_sf <- st_as_sf(retail2, coords = c("lon", "lat"), crs = 4326, agr = "constant")
# st_write(retail_sf, "~/Dropbox/liverpool/retail_typology/data/ldc_2016_wgs84.shp", driver = "ESRI Shapefile")
