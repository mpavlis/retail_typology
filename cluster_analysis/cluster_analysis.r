library(data.table)
library(clusterCrit)
library(parallel)
library(caret)
library(corrplot)
library(sf)
library(mclust)
library(ggsci)
library(car)

source("~/Dropbox/git/retail_typology/clustergram.r")

options(max.print = 3600)

################################################################ Functions ####################################################################################

##### 1. z-standardise ########################################################################################################################################

fun_std <- function(x){
  ifelse(max(x) - min(x) != 0, (x - min(x)) / (max(x) - min(x)), 0)
} 

##### 2. Create a plot of the centrality measure of each explanatory variable conditional on cluster level ####################################################

centrality_per_level <- function(Df, id_vars = "clustering", measure_vars = names(Df)[-which(names(Df) %in% id_vars)], fun_central = mean, out_csv_name = NULL, ...){

  Df_long <- data.table::melt(Df, id.vars = id_vars, measure.vars = measure_vars)
  
  centrality <- aggregate(Df_long$value, by = list(Df_long[[id_vars]], Df_long$variable), fun_central)
  names(centrality) <- c("cluster", "category", "mean_pct")
  centrality$cluster <- as.factor(centrality$cluster)
  # write.csv(centrality, "mean_per_cluster.csv", row.names = F)
  
  if (!is.null(out_csv_name)) fwrite(centrality, out_csv_name)
  
  return(ggplot(centrality, aes(x = category, fill = cluster, y = mean_pct)) + geom_dotplot(binaxis = "y", stackdir = "center") + 
           theme(axis.text.x = element_text(size = 15, angle=90,vjust = 0.5,hjust = 1)) +
           ylab("Mean of Variable (%)") + scale_fill_discrete(name = "Cluster ID") + scale_fill_d3())
  
}

##### 3. Produce clustergram to decide on the number of clusters ##############################################################################################

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

##### 4. Cluster analysis with stats ##########################################################################################################################

get_ca <- function(k, vars_df, id, cl_boundaries, out_data_dir, out_plots_dir){
  
  set.seed(1234)
  m_pam <- pam(x = vars_df, k = k, diss = F, metric = "euclidean", stand = F)
  tryCatch({
    tiff(paste0(out_plots_dir, "/cluster_analysis_", k, ".tiff"), width = 1600, height = 1600, compression = "none")
    plot(m_pam, which.plots = 1)
    dev.off()
  }, error = function(err){
    error_file <- file(paste0(out_plots_dir, "/error_received_k_", k, ".txt"), "w")
    cat("The following error was received:", err$message, file = error_file, sep = "\n")
    close(error_file)
    }
  )
  
  vars_df$pam <- m_pam$clustering
  
  ggp <- centrality_per_level(Df = vars_df, id_vars = "pam",fun_central = median, out_csv_name = paste0(out_data_dir, "/medians_pam", k, ".csv"))
  ggsave(paste0(out_plots_dir, "/medians_pam", k, ".tiff"), ggp, width = 297, height = 210, units = "mm")
  
  vars_df$id <- id
  
  cl_boundaries <- inner_join(cl_boundaries, vars_df, by = "id")
  st_write(cl_boundaries, paste0(out_data_dir, "/boundaries_pam", k, ".gpkg"))
  
  vars_df <- as.data.table(vars_df)
  
  sum_per_cluster <- vars_df[, lapply(.SD, sum), keyby = "pam", .SDcols = 1:(ncol(vars_df)-2)]
  sum_total <- vars_df[, lapply(.SD, sum), .SDcols = 1:(ncol(vars_df)-2)]
  
  index_scores <- data.table(pam = 1:k)
  for (n in names(sum_per_cluster)[2:ncol(sum_per_cluster)]){
    set(index_scores, j = n, value = round(100*sum_per_cluster[[n]] / sum_total[[n]]))
  }
  
  fwrite(index_scores, paste0(out_data_dir, "/index_score_pam", k, ".csv"))
}

############################################################# Statistical Analysis #####################################################################

##### 1. Import data ###################################################################################################################################

working_dir <- "~/Dropbox/liverpool/retail_typology/analysis/clustering"
# dir.create(working_dir)
setwd(working_dir)
# dir.create("plots")
# dir.create("plots/qq_plots")
all_data <- fread("~/Dropbox/liverpool/retail_typology/analysis/indicators/all_data.csv")
boundaries <- st_read("~/Dropbox/liverpool/boundaries/new_boundaries/all_boundaries_fin.gpkg", stringsAsFactors = F)

##### 2. Select variables ##############################################################################################################################

all_data[,catchment_area := NULL] # catchment area and catch_area are the same, remove the first one
all_data[, retail_density := stores_nr / retail_area]
all_data[, working_pop_density := working_pop / catch_area]
all_data[, resident_pop_density := resident_pop / catch_area]
all_data[, working_pop := NULL]
all_data[, resident_pop := NULL]

data_sele <- as.data.table(all_data)
data_sele[, cluster_id := NULL]

tiff("plots/correlogram_all_untrans.tiff", width = 1600, height = 1600, compression = "none")
corrplot(cor(data_sele, method = "spearman"), order = "hclust")
dev.off()

rm_vars <- c("working_pop_density", "large_multi_pct", "ethnic_pct", "attractive_leisure_pct", "local_services_diversity_pct", "attractive_convenience_pct",
             "attractive_consumer_pct", "attractive_comparison_pct", "local_small_multi_pct", "department_stores_pct", "financial_services")

data_sele <- data_sele[,-which(names(data_sele) %in% rm_vars),with=F]

tiff("plots/correlogram_sele_untrans.tiff", width = 1600, height = 1600, compression = "none")
corrplot(cor(data_sele, method = "spearman"), order = "hclust")
dev.off()

##### 3. Transform variables (Box-Cox) #################################################################################################################

data_sele_trans <- do.call(cbind, lapply(1:ncol(data_sele), function(x) unlist(ifelse(min(data_sele[, x, with = F]) < 1, data_sele[,x, with = F] + 1, data_sele[,x, with=F]))))
colnames(data_sele_trans) <- names(data_sele)
trans <- preProcess(data_sele_trans, method = "BoxCox")
data_sele_trans <- as.data.frame(predict(trans, data_sele_trans))

tiff("plots/correlogram_sele_trans.tiff", width = 1600, height = 1600, compression = "none")
corrplot(cor(data_sele_trans, method = "spearman"), order = "hclust")
dev.off()

# data_trans2 <- data_sele
# trans2 <- preProcess(data_trans2, method = "invHyperbolicSine")
# data_trans2 <- as.data.frame(predict(trans2, data_trans2))

##### 4. Compare data distributions ####################################################################################################################

for (n in names(data_sele)){
  tiff(paste0("plots/qq_plots/",n,".tiff"), width = 1200, height = 900, compression = "none")
  par(mfrow=c(1,2))
  qqnorm(data_sele[[n]], main = n)
  qqline(data_sele[[n]])
  qqnorm(data_sele_trans[[n]], main = paste(n, "lambda =", trans$bc[[n]]$lambda))
  qqline(data_sele_trans[[n]])
  dev.off()

}

##### 5. Create clustergram, standaridisation of the variables is performed by the get_clustergram function ############################################

clustergram_list <- get_clustergram(Df = data_sele_trans, which_many = "many_pam")
ggsave("plots/clustergram.tiff", plot(clustergram_list[[2]]), width = 297, height = 210, units = "mm")

# clustergram1 <- get_clustergram(Df = data_sele_trans, which_many = "many_kmeans", iter.max = 20, nstart=10000)

##### 6. PAM cluster analysis ##########################################################################################################################

# set.seed(1234)
for (k in 4:9) get_ca(k, vars_df = clustergram_list[[1]], id = all_data$cluster_id, cl_boundaries = boundaries, out_data_dir = getwd(), out_plots_dir = "plots")

# m_pam2 <- pam(x = data_sele_trans_std2, k = 6, diss = F, metric = "euclidean", stand = F)

##### 7. Second tier ###################################################################################################################################

# We selected 5 clusters, create the second tier

first_tier <- st_read("boundaries_pam5.gpkg", stringsAsFactors = F)
first_tier[first_tier$id %in% c("RC0167", "RC0312", "RC0379", "RC0026", "RC0456"),]$pam <- 2

dir.create("second_tier")
setwd("second_tier")
for (cl in 1:5){
  out_dir <- paste0("tier_of_cluster_", cl)
  out_plots_dir <- paste0(out_dir, "/plots")
  dir.create(out_dir)
  dir.create(out_plots_dir)
  subset_typology <- first_tier[first_tier$pam == cl, ]
  vars_only <- as.data.frame(subset_typology)[,4:51]
  tryCatch(
    {clustergram_list <- get_clustergram(Df = vars_only, z_std = F, which_many = "many_pam", k_vec = 2:5)
      ggsave(paste0(out_plots_dir, "/clustergram_",cl, ".tiff"), plot(clustergram_list[[2]]), width = 297, height = 210, units = "mm")
      for (k in 2:5){
        get_ca(k = k, vars_df = vars_only, id = subset_typology$id, cl_boundaries = subset_typology[, c("id", "name", "geom")], out_data_dir = out_dir, out_plots_dir = out_plots_dir)
      }
    }, error = function(err){
      error_file <- file(paste0(out_dir, "/error_received.txt"), "w")
      cat("The following error was received:", err$message, file = error_file, sep = "\n")
      close(error_file)
      }
    )
}

##### 7. mclust ########################################################################################################################################

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

# m2 <- mclustBIC(all_data_trans_std)
# summary(m2, all_data_trans_std)
# Best BIC values:
#   EEV,4     VEV,4      VEV,6
# BIC      144492.7 142726.72 142113.294
# BIC diff      0.0  -1766.03  -2379.453
# 
# Classification table for model (EEV,4):
#   1    2    3    4 
# 265  433  770 1017 

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


