####################################################################################################################################
#                                                        Crime data                                                                #
####################################################################################################################################

library(data.table)
library(sf)
library(RCurl)
library(RPostgreSQL)

source("~/Dropbox/git/postgis/postgis_functions.r")

####### Attention! In the end I didn't use crime rate per catchment but per cluster, so the following was not used #################

##### 1. Download data #############################################################################################################
data_url <- "https://data.police.uk/data/archive/"
# down_dir <- "~/Dropbox/liverpool/retail_typology/data/crime"
# dir.create(down_dir)
year <- 2016
for (m in 1:12){
  m <- ifelse(m < 10, paste0("0", m), as.character(m))
  full_data_url <- paste0(data_url, year, "-", m, ".zip")
  temp <- tempfile()
  download.file(full_data_url, temp)
  csv_names <- unzip(temp, list = T)
  unzip(temp, junkpaths = T, exdir = "~/temp", overwrite = T)
  unlink(temp)
}

csvs <- list.files("~/temp")
con <- dbConnect(PostgreSQL(), dbname = "crime", user = "postgres", password = "pwd")
dbExecute(con, "CREATE TABLE crime(crime_id TEXT,	month TEXT,	reported_by TEXT,	falls_within TEXT, longitude NUMERIC,
                latitude NUMERIC,	location TEXT, lsoa_code TEXT,	lsoa_name TEXT,	crime_type TEXT, last_outcome_category TEXT, context TEXT)")
for (csv in csvs){
  system(paste0("psql -U postgres -c \"copy crime FROM '/home/michalis/temp/", csv, "' CSV HEADER\" crime"))
}

dbExecute(con, "ALTER TABLE crime ADD COLUMN geog geography(POINT,4326)")
dbExecute(con, "UPDATE crime SET geog = ST_GeogFromText('SRID=4326;POINT(' || longitude || ' ' || latitude || ')')")
dbExecute(con, "CREATE INDEX ON crime USING gist(geog)")
dbExecute(con, "CREATE INDEX ON crime(crime_type)")
# import_or_append(con = con, working_dirs = "/home/shishou/Dropbox/liverpool/GIS_Data/LADs_2011", shp_names = "Local_UnitaryAuthority.shp", srid = 27700, table_name = "lads")
import_or_append(con = con, working_dirs = "/home/michalis/Dropbox/liverpool/GIS_Data/OAs_2011", shp_names = "oas11.shp", srid = 27700, table_name = "oas")
dbExecute(con, "create index on oas(code)")

dbExecute(con, "CREATE TABLE oas_84 (gid serial PRIMARY KEY, oa11 TEXT, geog geography(MultiPolygon, 4326))")
dbExecute(con, "INSERT INTO oas_84(oa11, geog) SELECT code, ST_Transform(geom, 4326) FROM oas")
dbExecute(con, "CREATE INDEX ON oas_84(code)")
dbExecute(con, "CREATE INDEX ON oas_84 USING gist(geog)")

import_or_append(con = con, working_dirs = "/home/michalis/Dropbox/liverpool/boundaries/new_boundaries", shp_names = "all_boundaries_fin.shp", srid = 27700, table_name = "boundaries")
dbExecute(con, "CREATE INDEX ON boundaries(id)")

dbExecute(con, "CREATE TABLE boundaries84_buf100(id TEXT, geog geography(Polygon, 4326))")
dbExecute(con, "INSERT INTO boundaries84_buf100(id, geog) SELECT id, ST_Transform(ST_Buffer(geom, 100), 4326) FROM boundaries")
dbExecute(con, "CREATE INDEX ON boundaries84_buf100(id)")
dbExecute(con, "CREATE INDEX ON boundaries84_buf100 USING gist(geog)")

dbExecute(con, paste("CREATE TABLE count_crimes AS",
                     "SELECT t1.id, ST_Area(t1.geog), count(t2.*) FROM boundaries84_buf100 t1 LEFT JOIN crime t2 ON ST_Covers(t1.geog, t2.geog)", 
                     "WHERE t2.crime_type NOT IN ('Burglary', 'Other theft') GROUP BY t1.id, t1.geog"))

# dbExecute(con, paste("CREATE TABLE count_crimes AS",
#                      "SELECT t1.id, count(t2.*) FROM boundaries84 t1 LEFT JOIN crime t2 ON ST_DWithin(t1.geog, t2.geog, 100, false)", 
#                      "WHERE t2.crime_type NOT IN ('Burglary', 'Other theft') GROUP BY id"))
count_crimes <- dbGetQuery(con, "select * from count_crimes")
names(count_crimes)[2:3] <- c("buffer_retail_area", "count_crime")
count_crimes$buffer_retail_area <- count_crimes$buffer_retail_area / 10^4
count_crimes$crime_density <- count_crimes$count_crime / count_crimes$buffer_retail_area
write.csv(count_crimes, "~/Dropbox/liverpool/retail_typology/data/crime/count_crime.csv")

#### 2. Crime/pop per cluster id #########################################################################################################
crimes <- fread("~/Dropbox/liverpool/retail_typology/data/crime/count_crime.csv")[,2:3]
all_probs <- fread("~/Dropbox/liverpool/retail_typology/data/catchments_oa_lookup.csv")

# sanity check
sum(unique(all_probs$oa11) %in% crimes$code) # 172636
sum(!unique(all_probs$oa11) %in% crimes$code) # 46710

table(substr(unique(all_probs[oa11 %in% crimes$code, ]$oa11), 1, 1))
#      E      S      W 
# 163489    283   8864 
table(substr(unique(all_probs[!oa11 %in% crimes$code, ]$oa11), 1, 1))
#    E     S     W 
# 4890 41676   144 

probs_crime <- crimes[all_probs, on = c(code = "oa11")]
probs_crime[is.na(count), count := 0]

crime_rate <- catchments_crime[, list(sum(count) / sum(pop), sum(count)), keyby = "cluster_id"]
setnames(crime_rate, c("V1", "V2"), c("crime_per_pop", "total_crime"))

fwrite(crime_rate, "~/Dropbox/liverpool/retail_typology/analysis/indicators/crime_rate.csv")
