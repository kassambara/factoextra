#' @include eigenvalue.R get_pca.R hcut.R
 NULL
#'Visualize Clustering Results
#'@description Provides ggplot2-based elegant visualization of partitioning
#'  methods including kmeans [stats package]; pam, clara and fanny [cluster
#'  package]; dbscan [fpc package]; Mclust [mclust package]; HCPC [FactoMineR];
#'  hkmeans [factoextra]. Observations are represented by points in the plot,
#'  using principal components if ncol(data) > 2. An ellipse is drawn around
#'  each cluster.
#'@param object an object of class "partition" created by the functions pam(),
#'  clara() or fanny() in cluster package; "kmeans" [in stats package]; "dbscan"
#'  [in fpc package]; "Mclust" [in mclust]; "hkmeans", "eclust" [in factoextra].
#'  Possible value are also any list object with data and cluster components
#'  (e.g.: object = list(data = mydata, cluster = myclust)).
#'@param data the data that has been used for clustering. Required only when
#'  object is a class of kmeans or dbscan.
#'@param choose.vars a character vector containing variables to be considered
#'  for plotting.
#'@param stand logical value; if TRUE, data is standardized before principal
#'  component analysis
#'@param axes a numeric vector of length 2 specifying the dimensions to be
#'  plotted.
#'@param geom a text specifying the geometry to be used for the graph. Allowed
#'  values are the combination of c("point", "text"). Use "point" (to show only
#'  points);  "text" to show only labels; c("point", "text") to show both types.
#'@param repel a boolean, whether to use ggrepel to avoid overplotting text
#'  labels or not.
#'@param show.clust.cent logical; if TRUE, shows cluster centers
#'@param ellipse logical value; if TRUE, draws outline around points of each
#'  cluster
#'@param ellipse.type Character specifying frame type. Possible values are
#'  'convex', 'confidence' or types supported by
#'  \code{\link[ggplot2]{stat_ellipse}} including one of c("t", "norm",
#'  "euclid").
#'@param ellipse.level the size of the concentration ellipse in normal
#'  probability. Passed for \code{ggplot2::stat_ellipse} 's level. Ignored in
#'  'convex'. Default value is 0.95.
#'@param ellipse.alpha Alpha for frame specifying the transparency level of fill
#'  color. Use alpha = 0 for no fill color.
#'@param labelsize font size for the labels
#'@param shape the shape of points.
#'@param pointsize the size of points
#'@param outlier.pointsize,outlier.color,outlier.shape,outlier.labelsize
#'  arguments for customizing outliers, which can be detected only in DBSCAN
#'  clustering.
#'@param main plot main title.
#'@param xlab,ylab character vector specifying x and y axis labels,
#'  respectively. Use xlab = FALSE and ylab = FALSE to hide xlab and ylab,
#'  respectively.
#'@inheritParams ggpubr::ggpar
#'@param ... other arguments to be passed to the functions
#'  \code{\link[ggpubr]{ggscatter}} and \code{\link[ggpubr]{ggpar}}.
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
#' km.res <- kmeans(iris.scaled, 3, nstart = 10)
#'
#' # Visualize kmeans clustering
#' # use repel = TRUE to avoid overplotting
#' fviz_cluster(km.res, iris[, -5], ellipse.type = "norm")
#'
#'
#'# Change the color palette and theme
#'fviz_cluster(km.res, iris[, -5],
#'    palette = "Set2", ggtheme = theme_minimal())
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
#' fviz_cluster(pam.res, geom = "point", ellipse.type = "norm")
#'
#' # Hierarchical clustering
#' # ++++++++++++++++++++++++
#' # Use hcut() which compute hclust and cut the tree
#' hc.cut <- hcut(iris.scaled, k = 3, hc_method = "complete")
#' # Visualize dendrogram
#' fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
#' # Visualize cluster
#' fviz_cluster(hc.cut, ellipse.type = "convex")
#'
#' }
#'
#'
#'
#'@name fviz_cluster
#'@rdname fviz_cluster
#'@export
fviz_cluster <- function(object, data = NULL, choose.vars = NULL, stand = TRUE, 
                         axes = c(1, 2),
                         geom = c("point", "text"), repel = FALSE,
                         show.clust.cent = TRUE,
                         ellipse = TRUE, ellipse.type = "convex", ellipse.level = 0.95,
                         ellipse.alpha = 0.2,
                         shape = NULL, pointsize = 1.5, labelsize = 12,
                         main = "Cluster plot",  xlab = NULL, ylab = NULL,
                         outlier.color = "black", outlier.shape = 19,
                         outlier.pointsize = pointsize, outlier.labelsize = labelsize,
                         ggtheme = theme_grey(), ...){
  
  # Deprecated arguments
  extra_args <- list(...)
  .check_axes(axes, .length = 2)
  
  if (!is.null(extra_args$jitter)) {
    warning("argument jitter is deprecated; please use repel = TRUE instead, to avoid overlapping of labels.", 
            call. = FALSE)
    if(!is.null(extra_args$jitter$width) | !is.null(extra_args$jitter$height) ) repel = TRUE
  }
  
  if(!is.null(extra_args$frame)) ellipse <- .facto_dep("frame", "ellipse", ellipse)
  if(!is.null(extra_args$frame.type)) ellipse.type <- .facto_dep("frame.type", "ellipse.type", extra_args$frame.type)
  if(!is.null(extra_args$frame.level)) ellipse.level <- .facto_dep("frame.level", "ellipse.level", extra_args$frame.level)
  if(!is.null(extra_args$frame.alpha)) ellipse.alpha <- .facto_dep("frame.alpha", "ellipse.alpha", extra_args$frame.alpha)
  if(!is.null(extra_args$title)) main <- .facto_dep("title", "main", extra_args$title)
  
  
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
    object$cluster <- object$call$X$clust
    data <- res.hcpc <- object
    stand <- FALSE # to avoid trying to standardize HCPC results
#     data <- object$data.clust[, -ncol(object$data.clust), drop = FALSE]
#     object$cluster <- as.vector(object$data.clust$clust)
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
  
  # Choose variables
  if(!is.null(choose.vars))
    data <- data[, choose.vars, drop = FALSE]
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
    ind <- facto_summarize(pca, element = "ind", result = "coord", axes = axes)
    eig <- get_eigenvalue(pca)[axes,2]
    if(is.null(xlab)) xlab = paste0("Dim", axes[1], " (", round(eig[1],1), "%)") 
    if(is.null(ylab)) ylab = paste0("Dim", axes[2], " (", round(eig[2], 1),"%)")
    }
    # PCA is not performed
    else if(ncol(data) == 2){
      ind <- as.data.frame(data, stringsAsFactors = TRUE)
      ind <- cbind.data.frame(name = rownames(ind), ind, stringsAsFactors = TRUE)
      if(is.null(xlab)) xlab <- colnames(data)[1]
      if(is.null(ylab)) ylab <- colnames(data)[2]
      
      if(xlab=="x") xlab <- "x value"
      if(ylab == "y") ylab <- "y value"
    }
    else{
      stop("The dimension of the data < 2! No plot.")
    }
    colnames(ind)[2:3] <-  c("x", "y")
    label_coord <- ind
  }
  else if(inherits(data, "HCPC")){
    ind <- res.hcpc$call$X[, c(axes, ncol(res.hcpc$call$X))]
    colnames(ind) <- c("Dim.1", "Dim.2", "clust")
    ind <- cbind.data.frame(name = rownames(ind), ind, stringsAsFactors = TRUE)
    colnames(ind)[2:3] <-  c("x", "y")
    label_coord <- ind
    eig <- get_eigenvalue(res.hcpc$call$t$res)[axes,2]
    if(is.null(xlab)) xlab = paste0("Dim", axes[1], " (", round(eig[1], 1), "%)") 
    if(is.null(ylab)) ylab = paste0("Dim", axes[2], " (", round(eig[2], 1),"%)")
  }
  else stop("A data of class ", class(data), " is not supported.")
  
  # Plot data and labels
  # ++++++++++++++++++++++++
  label = FALSE
  if("text" %in% geom) label = TRUE
  if(!("point" %in% geom)) pointsize = 0
  
  plot.data <- cbind.data.frame(ind, cluster = cluster, stringsAsFactors = TRUE)
  label_coord <- cbind.data.frame(label_coord, cluster = cluster, stringsAsFactors = TRUE)
  # Augment data
  if(inherits(object, "Mclust")){
    plot.data$uncertainty <- object$uncertainty
    label_coord$uncertainty <- object$uncertainty
  }
  
  # IF DBSCAN: cluster 0 is outliers. We don't want to make ellipse around
  # these observations. Let's remove them. They will be added to the plot later
  is_outliers = FALSE
  if(inherits(object, c("dbscan", "Mclust"))){
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
  lab <- NULL
  if("text" %in% geom) lab <- "name"
  if(is.null(shape)) shape <- "cluster"
  
  if(inherits(object, "partition") & missing(show.clust.cent))
    show.clust.cent <- FALSE # hide mean point for PAM, CLARA
  
  p <- ggpubr::ggscatter(plot.data, "x", "y",
                         color="cluster", shape = shape, size = pointsize,
                         point = "point" %in% geom, 
                         label = lab,
                         font.label = labelsize, repel = repel,
                         mean.point = show.clust.cent, 
                         ellipse = ellipse, ellipse.type = ellipse.type,
                         ellipse.alpha = ellipse.alpha, ellipse.level = ellipse.level,
                         main = main, xlab = xlab, ylab = ylab,
                         ggtheme = ggtheme, ...
                         )
  
  # Add outliers (can exist only in dbscan)
  if(is_outliers)
    p <- .add_outliers(p, outliers_data, outliers_labs, outlier.color, outlier.shape,
                  outlier.pointsize, outlier.labelsize/3, geom, repel = repel)

  p
}



# Add outliers to cluster plot (for dbscan only)
.add_outliers <-function(p, outliers_data, outliers_labs, 
                         outlier.color = "black", outlier.shape = 19,
                         pointsize = 2, labelsize = 4, geom = c("point", "text"), repel = FALSE)
  {
  
  # FIX: ggplot2 3.0.0+ deprecation - aes_string() replaced with aes() + .data pronoun
  # See: https://github.com/kassambara/factoextra/issues/190
  if("point" %in% geom)
    p <-  p + geom_point(data = outliers_data,
                      aes(x = .data[["x"]], y = .data[["y"]]),
                      size = pointsize, color = outlier.color, shape = outlier.shape)
  if("text" %in% geom){
    if(repel)
      p <- p +ggrepel::geom_text_repel(data = outliers_labs,
                                       aes(x = .data[["x"]], y = .data[["y"]], label = .data[["name"]]),
                                       size = labelsize, color = outlier.color)
    else
      p <- p + geom_text(data = outliers_labs,
                         aes(x = .data[["x"]], y = .data[["y"]], label = .data[["name"]]),
                         size = labelsize, vjust = -0.7, color = outlier.color)
  }
    
  return(p)
}

