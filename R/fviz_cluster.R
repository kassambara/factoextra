#' @include eigenvalue.R get_pca.R hcut.R
 NULL
#'Visualize Clustering Results
#'@description Provides ggplot2-based elegant visualization of partitioning
#'methods including kmeans [stats package]; pam, clara and fanny [cluster
#'package]; dbscan [fpc package]; Mclust [mclust package]; HCPC [FactoMineR];
#'hkmeans [factoextra]. Observations are represented by points in the plot,
#'using principal components if ncol(data) > 2. An ellipse is drawn around each
#'cluster.
#'@param object an object of class "partition" created by the functions pam(),
#'  clara() or fanny() in cluster package; "kmeans" [in stats package]; "dbscan"
#'  [in fpc package]; "Mclust" [in mclust]; "hkmeans", "eclust" [in factoextra].
#'  Possible value are also any list object with data and cluster components 
#'  (e.g.: object = list(data = mydata, cluster = myclust)).
#'@param data the data that has been used for clustering. Required only when
#'  object is a class of kmeans or dbscan.
#'@param stand logical value; if TRUE, data is standardized before principal
#'  component analysis
#'@param geom a text specifying the geometry to be used for the graph. Allowed
#'  values are the combination of c("point", "text"). Use "point" (to show only
#'  points);  "text" to show only labels; c("point", "text") to show both types.
#' @param repel a boolean, whether to use ggrepel to avoid overplotting text 
#'   labels or not.
#'@param show.clust.cent logical; if TRUE, shows cluster centers
#'@param frame logical value; if TRUE, draws outline around points of each
#'  cluster
#'@param frame.type Character specifying frame type. Possible values are
#'  'convex' or types supporeted by \code{ggplot2::stat_ellipse} including one
#'  of c("t", "norm", "euclid").
#'@param frame.level Passed for \code{ggplot2::stat_ellipse} 's level. Ignored
#'  in 'convex'. Default value is 0.95.
#'@param frame.alpha Alpha for frame specifying the transparency level of fill
#'  color.
#'@param labelsize font size for the labels
#'@param pointsize the size of points
#'@param title the title of the graph
#'@param jitter a parameter used to jitter the points in order to reduce
#'  overplotting. It's a list containing the objects what, width and height (i.e
#'  jitter = list(what, width, height)). \itemize{ \item what: the element to be
#'  jittered. Possible values are "point" or "p"; "label" or "l"; "both" or "b".
#'  \item width: degree of jitter in x direction \item height: degree of jitter
#'  in y direction }
#'@param outlier.color,outlier.shape the color and the shape of outliers. 
#'  Outliers can be detected only in DBSCAN clustering.
#'  
#'@return return a ggpplot.
#'@author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'@seealso \code{\link{fviz_silhouette}}, \code{\link{hcut}},
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
#' # use repel = TRUE to avoid overplotting
#' fviz_cluster(km.res, iris[, -5], frame.type = "norm")
#' 
#' 
#'# Change the color and theme
#'fviz_cluster(km.res, iris[, -5]) + 
#'  scale_color_brewer(palette = "Set2")+
#'  scale_fill_brewer(palette = "Set2") +
#'  theme_minimal()
#'  
#'  \dontrun{
#' # Show points only
#' fviz_cluster(km.res, iris[, -5], geom = "point")
#' # Show text only
#' fviz_cluster(km.res, iris[, -5], geom = "text")
#'  
#' # PAM clustering
#' # ++++++++++++++++++++
#' require(cluster)
#' pam.res <- pam(iris.scaled, 3)
#'  # Visualize pam clustering
#' fviz_cluster(pam.res, geom = "point", frame.type = "norm")
#' }
#' 
#' # Hierarchical clustering
#' # ++++++++++++++++++++++++
#' # Use hcut() which compute hclust and cut the tree
#' hc.cut <- hcut(iris.scaled, k = 3, hc_method = "complete")
#' # Visualize dendrogram
#' fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
#' # Visualize cluster
#' fviz_cluster(hc.cut, frame.type = "convex")
#' 
#'
#' 
#' @name fviz_cluster
#' @rdname fviz_cluster
#' @export
fviz_cluster <- function(object, data = NULL, stand = TRUE, 
                         geom = c("point", "text"), repel = FALSE,
                         show.clust.cent = TRUE,
                         frame = TRUE, frame.type = "convex", frame.level = 0.95,
                         frame.alpha = 0.2,
                         pointsize = 2, labelsize = 4, title = "Cluster plot",
                         jitter = list(what = "label", width = NULL, height = NULL),
                         outlier.color = "black", outlier.shape = 19){
  
  # object from cluster package
  if(inherits(object, c("partition", "hkmeans", "eclust"))) data <- object$data
  # Object from kmeans (stats package)
  else if((inherits(object, "kmeans") & !inherits(object, "eclust"))| inherits(object, "dbscan")){
    if(is.null(data)) stop("data is required for plotting kmeans/dbscan clusters")
  } 
  # Object from mclust package
  else if(inherits(object, "Mclust")) {
    object$cluster <- object$classification
    data <- object$data
  }
  # HCPC in FactoMineR
  else if(inherits(object, "HCPC")) {
    data <- object$data.clust[, -ncol(object$data.clust), drop = FALSE]
    object$cluster <- as.vector(object$data.clust$clust)
  }
  else if(inherits(object, "hcut")){
    if(inherits(object$data, "dist")){
      if(is.null(data)) stop("The option 'data' is required for an object of class hcut." )
    }
    else data <- object$data
  }
  # Any obects containing data and cluster elements
  else if(!is.null(object$data) & !is.null(object$cluster)){
    data <- object$data
    cluster <- object$cluster
  }
  else stop("Can't handle an object of class ", class(object))
  if(stand) data <- scale(data)
  cluster <- as.factor(object$cluster)
  
  pca_performed <- FALSE
  
  # Prepare the data for plotting
  # ++++++++++++++++++++++++
  # PCA is performed depending on the number of variables
  if(inherits(data, c("matrix", "data.frame"))){
    # ncol(data) > 2 --> PCA
    if(ncol(data)>2){
    pca <- stats::prcomp(data, scale = FALSE, center = FALSE)
    ind  <- facto_summarize(pca, element = "ind", result = "coord", axes = 1:2)
    pca_performed = TRUE
    }
    # PCA is not performed
    else if(ncol(data) == 2){
      ind <- as.data.frame(data)
      ind <- cbind.data.frame(name = rownames(ind), ind)
    }
    else{
      stop("The dimension of the data < 2! No plot.")
    }
    colnames(ind)[2:3] <-  c("x", "y")
    label_coord <- ind
  }
  else stop("A data of class ", class(data), " is not supported.")
  
  # Plot data and labels
  # ++++++++++++++++++++++++
  label = FALSE
  if("text" %in% geom) label = TRUE
  if(!("point" %in% geom)) pointsize = 0
  if(is.null(jitter$what)) jitter$what <- "label"
 
  # Jittering
  if(jitter$what %in% c("both", "b")) label_coord <- ind <- .jitter(ind, jitter)
  else if(jitter$what %in% c("point", "p")) ind <- .jitter(ind, jitter)
  else if(jitter$what %in% c("label", "l")) label_coord <- .jitter(label_coord, jitter)
  
  plot.data <- cbind.data.frame(ind, cluster = cluster)
  label_coord <- cbind.data.frame(label_coord, cluster = cluster)
  
  # IF DBSCAN: cluster 0 is outliers. We don't want to make ellipse around
  # these observations. Let's remove them. They will be added to the plot later
  is_outliers = FALSE
  if(inherits(object, "dbscan")){
    outliers <- which(cluster == 0)
    if(length(outliers) > 0){
      is_outliers = TRUE
      outliers_data <- plot.data[outliers, , drop = FALSE]
      outliers_labs <- label_coord[outliers, , drop = FALSE]
      
      ind <- ind[-outliers, , drop = FALSE]
      cluster <- cluster[-outliers]
      plot.data <- plot.data[-outliers, , drop = FALSE]
      label_coord <- label_coord[-outliers, , drop = FALSE]
    }
  }
  
  # Plot
  # ++++++++++++++++++++++++
  ngroups <- length(levels(plot.data$cluster))
  p <- ggplot()
  if("point" %in% geom) 
  {
    if(ngroups <= 6){
    p <- p+geom_point(data = plot.data , 
                      aes_string('x', 'y', color="cluster", shape = "cluster"),
                      size = pointsize)
    }
    else 
      p <- p+geom_point(data = plot.data , 
                        aes_string('x', 'y', color="cluster", shape = "cluster"),
                        size = pointsize) +
        ggplot2::scale_shape_manual(values=1:ngroups, labels = levels(plot.data$cluster)) 
        
  }
  
  if("text" %in% geom){
    if(repel)
      p <- p +ggrepel::geom_text_repel(data = label_coord, 
                                       aes_string('x', 'y', label = "name", color="cluster"),
                                       size = labelsize)
    else
      p <- p + geom_text(data = label_coord, 
                         aes_string('x', 'y', label = 'name', color="cluster"),  
                         size = labelsize, vjust = -0.7)
  }
  
  
    
  
  # Add cluster center
  clustcent <- stats::aggregate(ind[, 2:3], by=list(cluster=cluster), mean)
  colnames(clustcent) <- c("cluster", "x", "y")
  if(show.clust.cent){
    if("point" %in% geom) 
      p <- p + geom_point(data=clustcent,
                          aes_string('x', 'y', color="cluster", shape="cluster"),
                          size=pointsize*2)    
    if("text" %in% geom){
      if(repel)
        p <- p +ggrepel::geom_text_repel(data = clustcent, 
                                         aes_string('x', 'y', color="cluster"),
                                         label=clustcent$cluster, size=labelsize*1.2)
      else
        p <- p + geom_text(data=clustcent, 
                           aes_string('x', 'y', color="cluster"),
                           label=clustcent$cluster, size=labelsize*1.2, vjust=-1)
    }
    
  }
  
  # Add frame
  if(frame){
    if (frame.type == 'convex'){
      frame.data <- .cluster_chull(ind[, c("x", "y")], cluster)
      mapping = aes_string(x = "x", y = "y", colour ="cluster", fill = "cluster")
      p <- p + ggplot2::geom_polygon(data = frame.data,  mapping = mapping, alpha = frame.alpha)
    }
    else if (frame.type %in% c('t', 'norm', 'euclid')) {
        mapping = aes_string(x = "x", y = "y", colour = "cluster", fill = "cluster")
        p <- p + ggplot2::stat_ellipse(mapping = mapping, data = plot.data,
                                       level = frame.level, type = frame.type,
                                       geom = 'polygon', alpha = frame.alpha)
    }
  }
  
  # Add outliers (can exist only in dbscan)
  if(is_outliers)
    p <- .add_outliers(p, outliers_data, outliers_labs, outlier.color, outlier.shape,
                  pointsize, labelsize, geom, repel = repel)
  
  
  # Plot titles
  # ++++++++++++++++++++++++
  if(pca_performed){
    eig <- get_eigenvalue(pca)[,2]
    xlab = paste0("Dim", 1, " (", round(eig[1],1), "%)") 
    ylab = paste0("Dim", 2, " (", round(eig[2], 1),"%)")
  }else{
    xlab <- colnames(data)[1]
    ylab <- colnames(data)[2]
  }
  title2 <- title
  p <- p + labs(title = title2, x = xlab, y = ylab)

  p
}


# Compute convex hull for each cluster
# ++++++++++++++++++++++++++++++++
# x,y: numeric vector corresponding to the coordinates of points
# cluster: groups of observations
.cluster_chull <- function(x, cluster){
  cluster <- as.factor(cluster)
  levs <- levels(cluster)
  res = NULL
  for(lev in levs){
    dd <- x[which(cluster == lev), , drop = FALSE]
    cc <- chull(dd)
    res <- rbind(res, cbind(dd[cc, , drop = FALSE], cluster = rep(lev, length(cc))))
  }
  as.data.frame(res)
}

# Add outliers to cluster plot (for dbscan only)
.add_outliers <-function(p, outliers_data, outliers_labs, 
                         outlier.color = "black", outlier.shape = 19,
                         pointsize = 2, labelsize = 4, geom = c("point", "text"), repel = FALSE)
  {
  
  if("point" %in% geom) 
    p <-  p + geom_point(data = outliers_data, 
                      aes_string('x', 'y'),
                      size = pointsize, color = outlier.color, shape = outlier.shape)
  if("text" %in% geom){
    if(repel)
      p <- p +ggrepel::geom_text_repel(data = outliers_labs, 
                                       aes_string('x', 'y', label = 'name'),
                                       size = labelsize, color = outlier.color)
    else
      p <- p + geom_text(data = outliers_labs, 
                         aes_string('x', 'y', label = 'name'),  
                         size = labelsize, vjust = -0.7, color = outlier.color)
  }
    
  return(p)
}

