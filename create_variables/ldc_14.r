library(sf)
library(data.table)

source("~/Dropbox/git/postgis/postgis_functions.r")

##### 1. 2014 #################################################################################################################
retail <- fread("~/Dropbox/liverpool/GIS_Data/retail/LDC14_PostCodes.csv")

retail_sf <- st_as_sf(retail, coords = c("E", "N"), crs = 27700, agr = "constant")
st_write(retail_sf, "ldc_2014.gpkg")

setwd("~/Dropbox/liverpool/retail_typology/data")

con <- dbConnect(PostgreSQL(), dbname = "retail_typology", user = "user", password = "pwd")
import_or_append(con, working_dirs = getwd(), shp_names = "ldc14.shp", srid = 27700, table_name = "ldc14")
# import_or_append(con, working_dirs = "~/Dropbox/liverpool/boundaries/new_boundaries", shp_names = "all_boundaries_fin.shp", srid = 27700, table_name = "boundaries")

dbExecute(con, "CREATE TABLE ldc14_bound_id AS SELECT t1.*, t2.id cluster_id FROM ldc14 t1, boundaries t2 WHERE ST_DWithin(t1.geom, t2.geom, 15)")

retail <- st_read_db(con, "ldc14_bound_id")
st_write(retail, "clusters_14.gpkg")

