#' @include eigenvalue.R get_pca.R
 NULL
#' Visualize clustering results
#' @description 
#' Draws a 2-dimensional cluster plot for visualizing the results of partitioning methods, 
#' including kmeans, pam, clara and fanny. 
#' Observations are represented by points in the plot, using principal components. 
#'An ellipse is drawn around each cluster.
#' @param object an object of class "partition" created by the functions pam(), clara() or fanny() 
#' in cluster package. It can be also an output of kmeans() function in stats package. In this case 
#' the argument data is required.
#' @param data the data that has been used for clustering. Required only when object is a class of kmeans.
#' @param stand logical value; if TRUE, data is standardized before principal component analysis
#' @param geom a text specifying the geometry to be used for the graph. 
#' Allowed values are the combination of c("point", "text"). 
#' Use "point" (to show only points);  "text" to show only labels; c("point", "text") to show both types.
#' @param show.clust.cent logical; if TRUE, shows cluster centers
#' @param frame logical value; if TRUE, draws outline around points of each cluster 
#' @param frame.type Character specifying frame type. 
#' Possible values are 'convex' or types supporeted by \code{ggplot2::stat_ellipse} 
#' including one of c("t", "norm", "euclid").
#' @param frame.level Passed for \code{ggplot2::stat_ellipse} 's level. Ignored in 'convex'. 
#' Default value is 0.95.
#' @param frame.alpha Alpha for frame specifying the transparency level of fill color. 
#' @param labelsize font size for the labels
#' @param pointsize the size of points
#' @param jitter a parameter used to jitter the points in order to reduce overplotting. 
#' It's a list containing the objects what, width and height (i.e jitter = list(what, width, height)). 
#' \itemize{
#' \item what: the element to be jittered. Possible values are "point" or "p"; "label" or "l"; "both" or "b".
#' \item width: degree of jitter in x direction
#' \item height: degree of jitter in y direction
#' }
#' @param ... others arguments to be passed to the function ggplot2::autoplot()
#' 
#' @examples 
#' \donttest{
#' set.seed(123)
#' # K-means clustering
#' data(USArrests)
#' km.res <- kmeans(scale(USArrests), 4, nstart = 25)
#' 
#' # Visualize kmeans clustering
#' fviz_cluster(km.res, USArrests)
#' 
#' # Visualize pam clustering
#' data("iris")
#' pam.res <- pam(scale(iris[, -5]), 3)
#' fviz_cluster(pam.res)
#' 
#' # Change frame type
#' fviz_cluster(pam.res, frame.type = "t")
#' 
#' # Remove ellipse fill color
#' # Change frame level
#' fviz_cluster(pam.res, frame.type = "t",
#'             frame.alpha = 0, frame.level = 0.7)
#'             
#'# Show point only
#'fviz_cluster(pam.res, geom = "point")
#'
#'# Show text only
#'fviz_cluster(pam.res, geom = "text")
#'
#'# Change the color and theme
#'fviz_cluster(pam.res) + 
#'  scale_color_brewer(palette = "Set2")+
#'  scale_fill_brewer(palette = "Set2") +
#'  theme_minimal()
#' }
#' @export
fviz_cluster <- function(object, data = NULL, stand = TRUE, 
                         geom = c("point", "text"), 
                         show.clust.cent = TRUE,
                         frame = TRUE, frame.type = "convex", frame.level = 0.95,
                         frame.alpha = 0.2,
                         pointsize = 2, labelsize = 4, 
                         jitter = list(what = "label", width = NULL, height = NULL),
                         ...){
  
  if(inherits(object, "partition")) data <- object$data
  else if(inherits(object, "kmeans")){
    if(is.null(data)) stop("data is required for plotting kmeans clusters")
  } 
  else stop("Can't handle an object of class ", class(object))
  if(stand) data <- scale(data)
  cluster <- as.factor(object$cluster)
  
  # Data frame to be used for plotting
  # If ncol(data) > 2 --> PCA
  pca <- stats::prcomp(data, scale = FALSE, center = FALSE)
  ind  <- facto_summarize(pca, element = "ind", result = "coord", axes = 1:2)
  colnames(ind)[2:3] <-  c("x", "y")
  label_coord <- ind
  
  # plot
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
  
  p <- ggplot()
  if("point" %in% geom) 
    p <- p+geom_point(data = plot.data , 
                      aes_string('x', 'y', color="cluster", shape = "cluster"),
                      size = pointsize)
  if("text" %in% geom)
    p <- p + geom_text(data = label_coord, 
                       aes_string('x', 'y', label = 'name', color="cluster"),  
                       size = labelsize, vjust = -0.7)
  
  # Add cluster center
  clustcent <- stats::aggregate(ind[, 2:3], by=list(cluster=cluster), mean)
  colnames(clustcent) <- c("cluster", "x", "y")
  if(show.clust.cent){
    if("point" %in% geom) 
      p <- p + geom_point(data=clustcent,
                          aes_string('x', 'y', color="cluster", shape="cluster"),
                          size=pointsize*2)    
    if("text" %in% geom)
      p <- p + geom_text(data=clustcent, 
                         aes_string('x', 'y', color="cluster"),
                         label=clustcent$cluster, size=labelsize*1.2, vjust=-1)
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
  
  # Plot titles
  eig <- get_eigenvalue(pca)[,2]
  xlab = paste0("Dim", 1, " (", round(eig[1],1), "%)") 
  ylab = paste0("Dim", 2, " (", round(eig[2], 1),"%)")
  p <- p + labs(title = "Cluster plot", x = xlab, y = ylab)

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



