library(sf)
library(data.table)

# source("~/Dropbox/git/postgis/postgis_functions.r")

# con <- dbConnect(PostgreSQL(), dbname = "retail_typology", user = "user", password = "pwd")

# import_or_append(con, working_dirs = "~/Dropbox/liverpool/boundaries/new_boundaries", shp_names = "all_boundaries_fin.shp", srid = 27700, table_name = "boundaries")

setwd("~/Dropbox/liverpool/retail_typology/data")

# ldc15 <- fread("ldc_2015_add_lat_lon.csv")
# ldc15[, latitude := NULL]
# ldc15[, longitude := NULL]
# ldc16 <- fread("ldc_2016_add_lat_lon.csv")
# ldc16[, Latitude := NULL]
# ldc16[, Longitude := NULL]

# ldc15_sp <- st_as_sf(x = ldc15, agr = 'constant', coords = c("lon", "lat"), crs = 4326)
# ldc15_sp <- st_transform(ldc15_sp, 27700)
# ldc16_sp <- st_as_sf(x = ldc16, agr = 'constant', coords = c("lon", "lat"), crs = 4326)
# ldc16_sp <- st_transform(ldc16_sp, 27700)
# 
# st_write(ldc15_sp, "ldc15.gpkg")
# st_write(ldc16_sp, "ldc16.gpkg")

# cl_15 <- dbGetQuery(con, "SELECT t1.id cl_id, t2.name, t2.classifica FROM boundaries t1 LEFT JOIN ldc15 t2 ON ST_DWithin(t1.geom, t2.geom, 10)")
# cl_16 <- dbGetQuery(con, "SELECT t1.id cl_id, t2.name, t2.classification classifica FROM boundaries t1 LEFT JOIN ldc16 t2 ON ST_DWithin(t1.geom, t2.geom, 10)")

cl_14 <- st_read("clusters_14.gpkg", stringsAsFactors = F)
cl_16 <- st_read("clusters_16.gpkg", stringsAsFactors = F)

setDT(cl_14)
setDT(cl_16)

vac_14 <- cl_14[, .(vac_pct_14 = 100 * sum(shopname == "Vacant Property") / .N), keyby = "cluster_id"]
vac_16 <- cl_16[, .(vac_pct_16 = 100 * sum(name == "Vacant Property") / .N), keyby = "cluster_id"]
vac_14[cluster_id == 'TC2027', vac_pct_14 := 0]

vac_all <- vac_16[vac_14, on = "cluster_id", nomatch=0]
vac_all[, vac_change := vac_pct_16 - vac_pct_14]
vac_all[cluster_id == 'TC0460', vac_change := 0]
vac_all[cluster_id == 'TC1568', vac_change := 0]
vac_all[cluster_id == 'TC1868', vac_change := 0]

vac_all[, vac_change_pos := vac_change + abs(min(vac_change))]

fwrite(vac_all, "~/Dropbox/liverpool/retail_typology/analysis/indicators/vacancy.csv")
