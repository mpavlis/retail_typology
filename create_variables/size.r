library(sf)
library(RPostgreSQL)
library(data.table)

con <- dbConnect(PostgreSQL(), dbname = "retail_typology", user = "user", password = "pwd")

boundaries <- st_read("~/Dropbox/liverpool/boundaries/new_boundaries/all_boundaries_fin.gpkg", stringsAsFactors = F)
boundaries_area <- data.table(cluster_id = boundaries$id, retail_area = as.numeric(st_area(boundaries)) / 10000)

oas <- st_read("~/Dropbox/liverpool/GIS_Data/OAs_2011/oas11.shp", stringsAsFactors = F)
catchments_oa <- fread("~/Dropbox/liverpool/retail_typology/data/catchments_oa_lookup.csv", stringsAsFactors = F)
oas_area <- data.table(oa11 = oas$code, area = as.numeric(st_area(oas)) / 10000)
oas_area <- oas_area[, sum(area), by = "oa11"]
setnames(oas_area, "V1", "area")

catchments_oa[oas_area, area := i.area, on = "oa11"]
catchments_area <- catchments_oa[, sum(area), keyby = "cluster_id"]
setnames(catchments_area, "V1", "catch_area")

out <- catchments_area[boundaries_area, on = "cluster_id", nomatch = 0]
fwrite(out, "~/Dropbox/liverpool/retail_typology/analysis/indicators/size.csv")

##### 2. Morphology ####################################################################################################
# roeck_test = 1 - concave_area / circle_area 
morph_ind <- dbGetQuery(con, "SELECT id cluster_id, ST_Area(ST_MinimumBoundingCircle(geom)) circle_area, ST_Perimeter(geom) locoh_perimeter FROM boundaries")
setDT(morph_ind)
morph_ind[out, retail_area := i.retail_area, on = "cluster_id"]
morph_ind <- morph_ind[!is.na(retail_area), ]
morph_ind[, circle_area := circle_area / 10^4]
morph_ind[, locoh_perimeter := locoh_perimeter / 1000]
morph_ind[, roek := 1 - retail_area / circle_area]

fwrite(morph_ind, "~/Dropbox/liverpool/retail_typology/analysis/indicators/morphology.csv")
