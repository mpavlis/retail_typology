library(plyr)
library(dplyr)
library(ggplot2)
library(cluster)

many_pam <- function(x, ks = ks.default(nrow(x)), ...) {
  ldply(seq_along(ks), function(i) {
    cl <- pam(x, k = ks[i], ...)
    data.frame(obs = seq_len(nrow(x)), i = i, k = ks[i], cluster = cl$clustering)
  })
}

ks.default <- function(rows) seq(2, max(3, rows %/% 4))

many_kmeans <- function(x, ks = ks.default(nrow(x)), ...) {
  ldply(seq_along(ks), function(i) {
    cl <- kmeans(x, centers = ks[i], ...)
    data.frame(obs = seq_len(nrow(x)), i = i, k = ks[i], cluster = cl$cluster)
  })
}

all_hclust <- function(x, ks = ks.default(nrow(x)), point.dist = "euclidean", cluster.dist = "ward") {
  d <- dist(x, method = point.dist)
  cl <- hclust(d, method = cluster.dist)
  
  ldply(seq_along(ks), function(i) {
    data.frame(
      obs = seq_len(nrow(x)), i = i, k = ks[i], 
      cluster = cutree(cl, ks[i])
    )
  })  
}

clustergram <- function(clusters, y, line.width = NULL) {
  clusters$y <- y[clusters$obs]
  clusters$center <- ave(clusters$y, clusters$i, clusters$cluster)  
  
  if (is.null(line.width)) {
    line.width <- 0.5 * diff(range(clusters$center, na.rm = TRUE)) / 
      length(unique(clusters$obs))
  }
  clusters$line.width <- line.width
  
  # Adjust center positions so that they don't overlap  
  clusters <- clusters[with(clusters, order(i, cluster, y)), ]
  clusters <- ddply(clusters, c("i", "cluster"), transform, 
                    adj = center + (line.width * seq_along(y))
  )
  
  structure(clusters, 
            class = c("clustergram", class(clusters)),
            line.width = line.width)
}

plot.clustergram <- function(x) {
  i_pos <- !duplicated(x$i)
  
  means <- ddply(x, c("cluster", "i"), summarise, 
                 min = min(adj), max = max(adj))
  
  ggplot(x, aes(i)) +
    geom_ribbon(aes(y = adj, group = obs, fill = y, ymin = adj - line.width/2, ymax = adj + line.width/2, colour = y)) + 
    geom_errorbar(aes(ymin = min, ymax = max), data = means, width = 0.1) + 
    scale_x_continuous("cluster", breaks = x$i[i_pos], labels = x$k[i_pos]) +
    labs(y = "Cluster average", colour = "Obs\nvalue", fill = "Obs\nvalue")
  
}

# Example:
# all_data_trans_std <- all_data_trans
# setDT(all_data_trans_std)
# n <- ncol(all_data_trans_std)
# all_data_trans_std[, (1:n) := lapply(.SD, fun_std), .SDcols = 1:n]
# k_2_20 <- many_kmeans(all_data_trans_std, 2:20, nstart = 1000, iter.max = 20)
# pam_2_20 <- many_pam(all_data_trans_std, 2:20)
# pr <- princomp(all_data_trans_std)
# pr1 <- predict(pr)[, 1]
# pr2 <- predict(pr)[, 2]
# plot(clustergram(k_2_20, pr1))