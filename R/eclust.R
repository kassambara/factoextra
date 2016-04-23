#' @include utilities.R cluster_utilities.R dist.R fviz_cluster.R fviz_dend.R
NULL
#' Visual enhancement of clustering analysis
#' 
#' @description 
#' Visual enhancement of clustering analysis.
#' @param x numeric vector, data matrix or data frame
#' @param FUNcluster a clustering function including "kmeans", "pam", "clara", "fanny", "hclust", "agnes" and "diana". 
#' Abbreviation is allowed.
#' @param k the number of clusters to be generated. If NULL, the gap statistic is used to estimate the appropriate 
#' number of clusters. In the case of kmeans, k can be either the number of clusters, 
#' or a set of initial (distinct) cluster centers.
#' @param k.max the maximum number of clusters to consider, must be at least two.
#' @param stand logical value; default is FALSE. If TRUE, then the data will be standardized using the function scale(). 
#' Measurements are standardized for each variable (column), by subtracting the variable's mean value and 
#' dividing by the variable's standard deviation.
#' @param graph logical value. If TRUE, cluster plot is displayed.
#' @param hc_metric character string specifying the metric to be used for calculating 
#' dissimilarities between observations. Allowed values are those accepted by the function dist() [including "euclidean", "manhattan", "maximum",
#'  "canberra", "binary", "minkowski"] and correlation based distance measures ["pearson", "spearman" or "kendall"]. 
#'  Used only when FUNcluster is a hierarchical 
#'  clustering function such as one of "hclust", "agnes" or "diana".
#' @param hc_method the agglomeration method to be used (?hclust): 
#' "ward.D", "ward.D2", "single", "complete", "average", ...
#' @param gap_maxSE a list containing the parameters (method and SE.factor) for determining the location of 
#' the maximum of the gap statistic (Read the documentation ?cluster::maxSE).
#' @param nboot integer, number of Monte Carlo ("bootstrap") samples. Used only for determining the number of clusters 
#' using gap statistic.
#' @param verbose logical value. If TRUE, the result of progress is printed.
#' @param seed integer used for seeding the random number generator.
#' @param ... other arguments to be passed to FUNcluster.
#' @return Returns an object of class "eclust" containing the result 
#' of the standard function used (e.g., kmeans, pam, hclust, agnes, diana, etc.). 
#'      
#' It includes also:
#' \itemize{
#' \item cluster: the cluster assignement of observations after cutting the tree
#' \item nbclust: the number of clusters
#' \item silinfo: the silhouette information of observations,  
#' including $widths (silhouette width values of each observation), 
#' $clus.avg.widths (average silhouette width of each cluster) and 
#' $avg.width (average width of all clusters)
#' \item size: the size of clusters
#' \item data: a matrix containing the original or the standardized data (if stand = TRUE)
#' }
#' The "eclust" class has method for fviz_silhouette(), fviz_dend(), fviz_cluster().
#' @seealso \code{\link{fviz_silhouette}}, \code{\link{fviz_dend}}, \code{\link{fviz_cluster}}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' 
#' @examples 
#' # Load and scale data
#' data("USArrests")
#' df <- scale(USArrests)
#' 
#' # Enhanced k-means clustering
#' res.km <- eclust(df, "kmeans")
#' # Silhouette plot
#' fviz_silhouette(res.km)
#' # Optimal number of clusters using gap statistics
#' res.km$nbclust
#' # Print result
#'  res.km
#'  
#'  # Enhanced hierarchical clustering
#'  res.hc <- eclust(df, "hclust") # compute hclust
#'   fviz_dend(res.hc) # dendrogam
#'   fviz_silhouette(res.hc) # silhouette plot
#'  
#' @name eclust
#' @rdname eclust
#' @export
eclust <- function(x, FUNcluster = c("kmeans", "pam", "clara", "fanny", "hclust", "agnes", "diana"),
                   k = NULL, k.max = 10, stand = FALSE,
                   graph = TRUE,
                   hc_metric = "euclidean", hc_method = "ward.D2",
                   gap_maxSE = list(method = "firstmax", SE.factor = 1), 
                   nboot = 100, verbose = interactive(),
                   seed = 123,  ...)
  {
  set.seed(seed)
  data <- x
  if(stand) x <- scale(x)
  # Define the type of clustering
  FUNcluster <- match.arg(FUNcluster)
  fun_clust <- switch(FUNcluster,
                 kmeans = stats::kmeans,
                 pam = cluster::pam,
                 clara = cluster::clara,
                 fanny = cluster::fanny,
                 diana = hcut,
                 agnes = hcut,
                 hclust = hcut
                 )
  
  if(!inherits(data, c("matrix", "data.frame")) ) graph = FALSE
  else if(ncol(data)< 2) graph = FALSE
      
  gap_stat <- NULL
  # Partitioning clustering
  # ++++++++++++++++++++++++++++++
  clust <- list()
  if(FUNcluster %in% c("kmeans", "pam", "clara", "fanny")){
    # Number of cluster
    if(is.null(k)) {
      gap <- .gap_stat(x, fun_clust, k.max = k.max, nboot = nboot,
                       gap_maxSE = gap_maxSE, verbose = verbose, ...)
      k <- gap$k
      gap_stat <- gap$stat
    }
    clust <- fun_clust(x, k, ...)
    
    if(inherits(k, c("matrix", "data.frame"))) k <- nrow(k) # cluster centers are provided as k
    
    # Plot
    if(graph) {
      clust$clust_plot <- fviz_cluster(clust, x)
      print(clust$clust_plot + labs(title = paste0(toupper(FUNcluster), " Clustering")))
    }
    if(k > 1) clust$silinfo <-.get_silinfo(clust$cluster, stats::dist(x))
  }
  
  
  # Hierarchical clustering
  # ++++++++++++++++++++++++++++++++
  else if(FUNcluster %in% c("hclust", "agnes", "diana")){
    
    res.dist <- get_dist(x, method = hc_metric)
    # Number of cluster
    if(is.null(k)) {
      gap <- .gap_stat(x, fun_clust, k.max = k.max, nboot = nboot,
                       gap_maxSE = gap_maxSE, verbose = verbose, diss = res.dist)
      k <- gap$k
      gap_stat <- gap$stat
    }
    res.hc <- hcut(res.dist, k, hc_func = FUNcluster, hc_method = hc_method )
    clust <- res.hc
    if(graph) fviz_dend(clust, k)
  }
  
  clust$nbclust <- k
  clust$data <- x
  clust$gap_stat <- gap_stat
  class(clust) <- c(class(clust), "eclust")
  clust
}


# Compute gap stat and get k
.gap_stat <- function(x, fun_clust, k.max = 10, nboot = 100,
                   gap_maxSE = list(method = "firstmax", SE.factor = 1),
                   verbose = interactive(), ...)
  {
  gap_stat <- cluster::clusGap(x, fun_clust, K.max = k.max,  B = nboot, 
                               verbose = verbose, ...)
  gap <- gap_stat$Tab[, "gap"]
  se <- gap_stat$Tab[, "SE.sim"]
  k <- .maxSE(gap, se, method = gap_maxSE$method, SE.factor = gap_maxSE$SE.factor)
  list(stat = gap_stat, k = k)
}



