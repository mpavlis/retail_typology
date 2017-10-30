library(data.table)
# library(clusterCrit)
# library(parallel)
library(caret)
library(corrplot)
library(sf)
library(dplyr)
library(mclust)
library(ggsci)
library(car)

options(max.print = 3600)

###################################################################################################################
#                                               Functions                                                         #
###################################################################################################################

##### 1. Standardize ##############################################################################################

fun_std <- function(x){
  (x - min(x)) / (max(x) - min(x))
} 

##### 2. Plot the mean value of each variable conditional on cluster level ########################################

mean_per_level <- function(Df, id_vars = "clustering", measure_vars = names(Df)[-which(names(Df) %in% id_vars)], fun_central = mean, means_csv_name = NULL, ...){

  Df_long <- data.table::melt(Df, id.vars = id_vars, measure.vars = measure_vars)
  
  means <- aggregate(Df_long$value, by = list(Df_long[[id_vars]], Df_long$variable), fun_central)
  names(means) <- c("cluster", "category", "mean_pct")
  means$cluster <- as.factor(means$cluster)
  # write.csv(means, "mean_per_cluster.csv", row.names = F)
  
  if (!is.null(means_csv_name)) fwrite(means, means_csv_name)
  
  return(ggplot(means, aes(x = category, fill = cluster, y = mean_pct)) + geom_dotplot(binaxis = "y", stackdir = "center") + 
           theme(axis.text.x = element_text(size = 15, angle=90,vjust = 0.5,hjust = 1)) +
           ylab("Mean of Variable (%)") + scale_fill_discrete(name = "Cluster ID") + scale_fill_d3())
  
}

##### 3. Create a clustergram #####################################################################################

# Can be used for K-means and PAM, performs z-standardization by default

# based on Hadley Wicham's available at:
# https://gist.githubusercontent.com/hadley/439761/raw/a7027786f34e4819dc411496b61a34c02f7c1586/clustergram-had.r

get_clustergram <- function(Df, z_std = T, which_many = c("many_kmeans", "many_pam"), k_vec = 2:20, ...){
  Dt <- as.data.table(Df)
  if (z_std){
    n <- ncol(Dt)
    Dt[, (1:n) := lapply(.SD, fun_std), .SDcols = 1:n]
  }
  if (which_many == "many_kmeans"){
    cl_many <- many_kmeans(Dt, k_vec, ...)
  } else if (which_many == "many_pam"){
    cl_many <- many_pam(Dt, k_vec, ...)
  }
  pr_c <- princomp(Dt)
  pr1_c <- predict(pr_c)[, 1]
  pr2_c <- predict(pr_c)[, 2]
 return(list(Dt, clustergram(cl_many, pr1_c)))
}

###############################################################################################################
#                                  Cluster Analysis of retail centres in GB                                   #
###############################################################################################################

##### 1. Import data ##########################################################################################

all_data <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/all_data.csv")
all_data <- all_data[cluster_id != 'TC0419', ] # the boundary is not representative, remove it

##### 2. Subset data to variables of interest #################################################################

all_data_sele <- all_data[, .(cluster_id, crime_count, competitors, working_pop, vac_pct_15, vac_change_pos, mean_jsa, mean_income, resident_pop, catch_area, retail_area, roek, stores_nr, ethnic_pct,
                              charity_pct, betting_pct, value_brand_pct, mass_brand_pct, anchor_pct, premium_brand_pct, local_services_diversity_pct, local_retail_diversity_pct,
                              local_small_multi_pct, local_large_multi_pct, local_indie_pct, local_leisure_pct, local_convenience_pct, local_consumer_pct, local_departm_stores_pct,
                              local_comparison_pct, clothing, house_goods, electrical, hobbies, food_retailers, ctn, off_license, chemists, restaurants, bars, caffes, entertainment,
                              fitness, health_and_beauty, consumer_services, household_services, financial_services, recruitment_services, services_diversity_pct, retail_diversity_pct,
                              small_multi_pct, large_multi_pct, indie_pct, attractive_leisure_pct, attractive_convenience_pct, attractive_consumer_pct, department_stores_pct, attractive_comparison_pct)]

all_data_sele[, crime_density := crime_count / retail_area]
all_data_sele[, retail_density := stores_nr / retail_area]
all_data_sele[, working_pop_density := working_pop / catch_area]
all_data_sele[, resident_pop_density := resident_pop / catch_area]
all_data_sele[, crime_count := NULL]
all_data_sele[, working_pop := NULL]
all_data_sele[, resident_pop := NULL]
all_data_sele[, retail_area := NULL]
rm_vars <- c("working_pop_density", "large_multi_pct", "ethnic_pct", "attractive_leisure_pct", "local_services_diversity_pct", "attractive_convenience_pct",
             "attractive_consumer_pct", "attractive_comparison_pct", "local_small_multi_pct", "department_stores_pct", "financial_services", "cluster_id", "premium_brand_pct")


all_data_sele2 <- all_data_sele[,-which(names(all_data_sele) %in% rm_vars),with=F]

##### 3. Box-Cox transformation, pick lambda using the caret library ######################################

all_data_trans <- do.call(cbind, lapply(1:ncol(all_data_sele2), function(x) unlist(ifelse(min(all_data_sele2[, x, with = F]) < 1, all_data_sele2[,x, with = F] + 1, all_data_sele2[,x, with=F]))))
colnames(all_data_trans) <- names(all_data_sele2)
trans <- preProcess(all_data_trans, method = "BoxCox")
all_data_trans <- as.data.frame(predict(trans, all_data_trans))

# all_data_trans2 <- all_data_sele2
# trans2 <- preProcess(all_data_trans2, method = "invHyperbolicSine")
# all_data_trans2 <- as.data.frame(predict(trans2, all_data_trans2))

##### 4. Exploratory Analysis #############################################################################

# correlation, correlogram
corrplot(cor(all_data_trans), order = "hclust")

# normality, QQ-plots
working_dir <- "~/Dropbox/liverpool/retail_typology/analysis/clustering"
dir.create(working_dir)
setwd(working_dir)
for (n in names(all_data_sele2)){
  tiff(paste0("plots/qq_plots/",n,".tiff"), width = 1200, height = 900, compression = "none")
  par(mfrow=c(1,2))
  qqnorm(all_data_sele2[[n]], main = n)
  qqline(all_data_sele2[[n]])
  qqnorm(all_data_trans[[n]], main = paste(n, "lambda =", trans$bc[[n]]$lambda))
  qqline(all_data_trans[[n]])
  dev.off()
}

# decide on the number of clusters
clustergram_list <- get_clustergram(Df = all_data_trans, which_many = "many_pam")
plot(clustergram_list[[2]])

# clustergram_kmeans <- get_clustergram(Df = all_data_trans, which_many = "many_kmeans", iter.max = 20, nstart=10000)

# Due to the presence of outliers the PAM method was selected from k-means and mclust

##### 5. PAM #####################################################################################################

# 8 clusters were selected based on the clustergram
set.seed(1234)
m_pam <- pam(x = clustergram_list[[1]], k = 8, diss = F, metric = "euclidean", stand = F)
plot(m_pam)

##### 6. mclust ##################################################################################################

# m1 <- Mclust(all_data_trans_std)
# summary(m1)
# Mclust EEV (ellipsoidal, equal volume and shape) model with 4 components:
#   
#   log.likelihood    n   df      BIC      ICL
#         89340.49 2485 4373 144492.7 144476.3
# 
# Clustering table:
#   1    2    3    4 
# 265  433  770 1017 

# summary(m1, parameters = T)
# plot(m1)

# m1_b <- Mclust(all_data_trans_std, prior = priorControl())
# summary(m2)
# 
#   Mclust VVV (ellipsoidal, varying volume, shape, and orientation) model with 4 components:
#   
#   Prior: defaultPrior()
# 
# log.likelihood    n   df      BIC      ICL
#       94374.14 2485 4511 153481.2 153471.8
# 
# Clustering table:
#   1    2    3    4 
# 340  568  517 1060 
# plot(m2)
# 
# m2 <- Mclust(all_data_trans_std2)
# m2_b <- Mclust(all_data_trans_std2, prior = priorControl())
# 
# m3 <- Mclust(all_data_trans_std3)
# m3_b <- Mclust(all_data_trans_std3, prior = priorControl())

##### 7. Put everything together #################################################################################

clusters <- data.frame(id = all_data$cluster_id, pam8 = m_pam$clustering, stringsAsFactors = F)

boundaries <- st_read("~/Dropbox/liverpool/boundaries/new_boundaries/all_boundaries_fin.shp", stringsAsFactors = F)
boundaries <- inner_join(boundaries, clusters, by = "id")
clustergram_list[[1]][, id := all_data$cluster_id]
boundaries <- inner_join(boundaries, clustergram_list[[1]])
st_write(boundaries, "boundaries_pam8_clusters.gpkg")

##### 8. Evaluate clustering solution ############################################################################

# plot mean of variables per cluster
ggp <- mean_per_level(Df = as.data.frame(boundaries)[,-which(names(boundaries) %in% c("id","name", "geometry"))], id_vars = "pam8", means_csv_name = paste0("means_pam8.csv"))
ggsave("plots/means_pam8.tiff", ggp, width = 297, height = 210, units = "mm")

# index scores, 
# sum raw variable values conditional on cluster id and divide by total values
setDT(clusters)
all_data_sele2[, id := all_data$cluster_id]
clusters <- clusters[all_data_sele2, on="id"]

sum_per_cluster <- clusters[, lapply(.SD, sum), keyby = "pam8", .SDcols = 3:47]
sum_total <- clusters[, lapply(.SD, sum), .SDcols = 3:47]

index_scores <- data.table(pam8 = 1:8)
for (n in names(sum_per_cluster)[2:46]){
  set(index_scores, j = n, value = round(100*sum_per_cluster[[n]] / sum_total[[n]]))
}

fwrite(index_scores, "index_score_pam8.csv")
