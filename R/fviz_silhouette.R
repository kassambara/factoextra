#' @include eigenvalue.R get_pca.R hcut.R
 NULL
#'Visualize Silhouette Information from Clustering
#'@description Silhouette (Si) analysis is a cluster validation approach that
#'  measures how well an observation is clustered and it estimates the average
#'  distance between clusters. fviz_silhouette() provides ggplot2-based elegant
#'  visualization of silhouette information from i) the result of
#'  \code{\link[cluster]{silhouette}}(), \code{\link[cluster]{pam}}(),
#'  \code{\link[cluster]{clara}}() and \code{\link[cluster]{fanny}}() [in
#'  cluster package]; ii) \code{\link{eclust}}() and \code{\link{hcut}}() [in
#'  factoextra].
#'  
#'  Read more: 
#'  \href{http://www.sthda.com/english/wiki/clustering-validation-statistics-4-vital-things-everyone-should-know-unsupervised-machine-learning}{Clustering
#'   Validation Statistics}.
#'@details - Observations with a large silhouhette Si (almost 1) are very well 
#'  clustered.
#'  
#'  - A small Si (around 0) means that the observation lies between two
#'  clusters.
#'  
#'  - Observations with a negative Si are probably placed in the wrong cluster.
#'  
#'@param sil.obj an object of class silhouette: pam, clara, fanny [in cluster 
#'  package]; eclust and hcut [in factoextra].
#'@param label logical value. If true, x axis tick labels are shown
#'@param print.summary logical value. If true a summary of cluster silhouettes 
#'  are printed in fviz_silhouette().
#'  
#'@return return a ggplot
#'@author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'@seealso \code{\link{fviz_cluster}}, \code{\link{hcut}}, 
#'  \code{\link{hkmeans}},  \code{\link{eclust}}, \code{\link{fviz_dend}}
#' @examples 
#' set.seed(123)
#' 
#' # Data preparation
#' # +++++++++++++++
#' data("iris")
#' head(iris)
#' # Remove species column (5) and scale the data
#' iris.scaled <- scale(iris[, -5])
#' 
#' # K-means clustering
#' # +++++++++++++++++++++
#' km.res <- kmeans(iris.scaled, 3, nstart = 25)
#' 
#' # Visualize kmeans clustering
#' fviz_cluster(km.res, iris[, -5], frame.type = "norm")+
#' theme_minimal()
#' 
#' # Visualize silhouhette information
#' require("cluster")
#' sil <- silhouette(km.res$cluster, dist(iris.scaled))
#' fviz_silhouette(sil)
#' 
#' # Identify observation with negative silhouette
#' neg_sil_index <- which(sil[, "sil_width"] < 0)
#' sil[neg_sil_index, , drop = FALSE]
#' 
#' # PAM clustering
#' # ++++++++++++++++++++
#' require(cluster)
#' pam.res <- pam(iris.scaled, 3)
#' # Visualize pam clustering
#' fviz_cluster(pam.res, frame.type = "norm")+
#' theme_minimal()
#' # Visualize silhouhette information
#' fviz_silhouette(pam.res)
#' 
#' # Hierarchical clustering
#' # ++++++++++++++++++++++++
#' # Use hcut() which compute hclust and cut the tree
#' hc.cut <- hcut(iris.scaled, k = 3, hc_method = "complete")
#' # Visualize dendrogram
#' fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
#' # Visualize silhouhette information
#' fviz_silhouette(hc.cut)
#' 
#'@export
fviz_silhouette <- function(sil.obj, label = FALSE, print.summary = TRUE){
  
  if(inherits(sil.obj, c("eclust", "hcut", "pam", "clara", "fanny"))){
    df <- as.data.frame(sil.obj$silinfo$widths)
  }
  else if(inherits(sil.obj, "silhouette"))
    df <- as.data.frame(sil.obj[, 1:3])
  else stop("Don't support an oject of class ", class(sil.obj))
  
  # order by cluster and by sil_width
  df <- df[order(df$cluster, -df$sil_width), ]
  if(!is.null(rownames(df))) df$name <- factor(rownames(df), levels = rownames(df))
  else df$name <- as.factor(1:nrow(df))
  df$cluster <- as.factor(df$cluster)
  mapping <- aes_string(x = "name", y = "sil_width", 
                        color = "cluster", fill = "cluster")
  p <- ggplot(df, mapping) +
    geom_bar(stat = "identity") +
    labs(y = "Silhouette width Si", x = "",
         title = paste0("Clusters silhouette plot ",
                        "\n Average silhouette width: ", 
                        round(mean(df$sil_width), 2)))+
    ggplot2::ylim(c(NA, 1))
  # Labels
  if(!label) p <- p + theme(axis.text.x = element_blank(), 
                            axis.ticks.x = element_blank())
  else if(label)
    p <- p + theme(axis.text.x = element_text(angle=45))
  
  # Print summary
  ave <- tapply(df$sil_width, df$cluster, mean)
  n <- tapply(df$cluster, df$cluster, length)
  sil.sum <- data.frame(cluster = names(ave), size = n,
                      ave.sil.width = round(ave,2))
  if(print.summary) print(sil.sum)
  
  p
}


