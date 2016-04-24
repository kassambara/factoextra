#' @include cluster_utilities.R dist.R
NULL
#' Computes Hierarchical Clustering and Cut the Tree
#' 
#' @description 
#' Computes hierarchical clustering (hclust, agnes, diana) and cut the tree into k clusters. It also accepts 
#' correlation based distance measure methods such as "pearson", "spearman" and "kendall".
#' @param x a numeric matrix, numeric data frame or a dissimilarity matrix.
#' @param k the number of clusters to be generated.
#' @param hc_method the agglomeration method to be used (?hclust) for hclust() and agnes(): 
#' "ward.D", "ward.D2", "single", "complete", "average", ...
#' @param hc_metric character string specifying the metric to be used for calculating 
#' dissimilarities between observations. Allowed values are those accepted by the function dist() [including "euclidean", "manhattan", "maximum",
#'  "canberra", "binary", "minkowski"] and correlation based distance measures ["pearson", "spearman" or "kendall"].
#' @param stand logical value; default is FALSE. If TRUE, then the data will be standardized using the function scale(). 
#' Measurements are standardized for each variable (column), by subtracting the variable's mean value and 
#' dividing by the variable's standard deviation.
#' @param isdiss logical value specifying wether x is a dissimilarity matrix.
#' @param hc_func the hierarchical clustering function to be used. Default value is "hclust". Possible values 
#' is one of "hclust", "agnes", "diana". Abbreviation is allowed.
#' @param graph logical value. If TRUE, the dendrogram is displayed.
#' @param ... not used.
#' @return an object of class "hcut" containing the result 
#' of the standard function used (read the documentation of hclust, agnes, diana). 
#'       
#' It includes also:
#' \itemize{
#' \item cluster: the cluster assignement of observations after cutting the tree
#' \item nbclust: the number of clusters
#' \item silinfo: the silhouette information of observations (if k > 1)
#' \item size: the size of clusters
#' \item data: a matrix containing the original or  the standardized data (if stand = TRUE)
#' }
#' 
#' @seealso \code{\link{fviz_dend}}, \code{\link{hkmeans}}, \code{\link{eclust}}
#' @examples 
#' \donttest{
#' data(USArrests)
#' 
#' # Compute hierarchical clustering and cut into 4 clusters
#' res <- hcut(USArrests, k = 4, stand = TRUE)
#' 
#' # Cluster assignements of observations
#' res$cluster
#' # Size of clusters
#' res$size
#' 
#' # Visualize the dendrogram
#' fviz_dend(res, rect = TRUE)
#' 
#' # Visualize the silhouette
#' fviz_silhouette(res)
#' 
#' # Visualize clusters as scatter plots
#' fviz_cluster(res)
#' }
#' 
#' @export
hcut <- function(x, k = 2, isdiss = inherits(x, "dist"), 
                 hc_func = c("hclust", "agnes", "diana"),
                 hc_method = "ward.D2", hc_metric = "euclidean",
                 stand = FALSE, graph = FALSE, ...){
  
  if(!inherits(x, c("matrix", "data.frame", "dist")))
    stop("The data must be of class matrix, data.frame, or dist")
  if(stand) x <- scale(x)
  data <- x
  
  hc_func <- match.arg(hc_func)
  hc_func <- hc_func[1]
  
  if(!isdiss) x <- get_dist(x, method = hc_metric)
  
  
  if(hc_func == "hclust") hc <- stats::hclust(x, method = hc_method)
  else if(hc_func == "agnes") {
    if(hc_method %in%c("ward.D", "ward.D2")) hc_method = "ward"
    hc <- cluster::agnes(x, method = hc_method)
  }
  else if(hc_func == "diana") hc <- cluster::diana(x)
  else stop("Don't support the function ", hc_func)
  hc.cut <- stats::cutree(hc, k = k)
  hc$cluster = hc.cut
  hc$nbclust <- k
  if(k > 1)  hc$silinfo <- .get_silinfo(hc.cut, x)
  hc$size <- as.vector(table(hc.cut))
  hc$data <- data
  class(hc) <- c(class(hc), "hcut")
  if(graph) fviz_dend(hc)
  hc
}
