
# Ge the silhouette information
# cluster: the cluster assignement of observation
# diss the dissimilarity matrix
.get_silinfo <- function(cluster, diss){
  
  k <- length(unique(cluster))
  silinfo <- list()
  if(k > 1) {
    sil.obj <- cluster::silhouette(cluster, diss)
    widths <- as.data.frame(sil.obj[, 1:3])
    rownames(widths) <- colnames(as.matrix(diss))
    widths <- widths[order(widths$cluster, -widths$sil_width), ]
    widths$cluster <- as.factor(widths$cluster)
    
    # summary
    silinfo$widths <- widths
    silinfo$clus.avg.widths <- as.vector(tapply(widths$sil_width, widths$cluster, mean))
    silinfo$avg.width <- mean(widths$sil_width)
  }
  
  silinfo
}