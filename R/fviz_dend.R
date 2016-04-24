#' Enhanced Visualization of Dendrogram
#' 
#' @description 
#' Enhanced visualization of dendrogram.
#' @param x an object of class dendrogram, hclust, agnes, diana, hcut or hkmeans.
#' @param k the number of groups for cutting the tree.
#' @param k_colors a vector containing colors to be used for the groups. 
#' It should contains k number of colors. 
#' @param show_labels a logical value. If TRUE, leaf labels are shown. Default value is TRUE.
#' @param color_labels_by_k logical value. If TRUE, labels are colored automatically by group when k != NULL.
#' @param label_cols a vector containing the colors for labels. 
#' @param type type of plot. Allowed values are one of "rectangle" or "triangle"
#' @param rect logical value specifying whether to add a rectangle around groups. Used only when k != NULL.
#' @param rect_border,rect_lty,rect_lwd border color, line type and line width for rectangles
#' @param cex size of labels
#' @param main,xlab,ylab main and axis titles
#' @param ... other arguments to be passed to the function plot.dendrogram()
#' @examples 
#' \donttest{
#' # Load and scale the data
#' data(USArrests)
#' df <- scale(USArrests)
#' 
#' # Hierarchical clustering
#' res.hc <- hclust(dist(df))
#' 
#' # Default plot
#' fviz_dend(res.hc)
#' 
#' # Cut the tree
#' fviz_dend(res.hc, cex = 0.5, k = 4, color_labels_by_k = TRUE)
#' 
#' # Don't color labels, add rectangles
#' fviz_dend(res.hc, cex = 0.5, k = 4, 
#'  color_labels_by_k = FALSE, rect = TRUE)
#'  
#' # Triangle
#' fviz_dend(res.hc, cex = 0.5, k = 4, type = "triangle")
#' 
#' # Change the color of tree using black color for all groups
#' # Change rectangle border colors
#' fviz_dend(res.hc, rect = TRUE, k_colors ="black",
#' rect_border = 2:5, rect_lty = 1)
#' 
#' # Customized color for groups
#' fviz_dend(res.hc, k = 4, 
#'  k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"))
#'  
#'  
#'  # Color labels using k-means clusters
#'  km.clust <- kmeans(df, 4)$cluster
#'  fviz_dend(res.hc, k = 4, 
#'    k_colors = c("blue", "green3", "red", "black"),
#'    label_cols =  km.clust[res.hc$order], cex = 0.6)
#' 
#' }
#' @export
fviz_dend <- function(x, k = NULL, k_colors = NULL, show_labels = TRUE, color_labels_by_k = FALSE,
                      label_cols = NULL,
                      type = c("rectangle", "triangle"),
                      rect = FALSE, rect_border = "gray", rect_lty = 2, rect_lwd = 1.5, 
                      cex = 0.8, main = "Cluster Dendrogram", xlab = "", ylab = "Height", ...)
{
  
  if(inherits(x, "hcut")){
    k <- x$nbclust
    dend <- as.dendrogram(x)
  }
  else if(inherits(x, "hkmeans")){
    k <- length(unique(x$cluster))
    dend <- as.dendrogram(x$hclust)
  }
  else if(inherits(x, c("hclust", "agnes", "diana"))) dend <- as.dendrogram(x)
  else if(inherits(x, "dendrogram")) dend <- x
  else stop("Can't handle an object of class ", paste(class(x), collapse =", ") )
  
  dend <- dendextend::set(dend, "labels_cex", cex) 
  if(!is.null(k)) {
    if(is.null(k_colors)) k_colors <- grDevices::rainbow(k)
    dend <- dendextend::set(dend, what = "branches_k_color", k = k, value = k_colors)
    if(color_labels_by_k) dend <- dendextend::set(dend, "labels_col",  k = k, value = k_colors)
  }
  if(!is.null(label_cols)){
    dend <- dendextend::set(dend, "labels_col", label_cols) 
  }
  
  leaflab <- ifelse(show_labels, "perpendicular", "none")
  
  plot(dend,  type = type[1], xlab = xlab, ylab = ylab, main = main,
       leaflab = leaflab,...)
  if(rect & !is.null(k))
    dendextend::rect.dendrogram(dend, k=k, border = rect_border, 
                                lty = rect_lty, lwd = rect_lwd)
  
}
