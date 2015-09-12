#' Enhanced visualization of dendrogram
#' 
#' @description 
#' Enhanced visualization of dendrogram.
#' @param x an object of class dendrogram, hclust, agnes, diana or hcut.
#' @param k the number of groups for cutting the tree.
#' @param k_colors a vector containing colors to be used for the groups. 
#' It should contains k number of colors. 
#' @param color_labels_by_k logical value. If TRUE, labels are colored automatically by group when k != NULL.
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
#' fviz_dend(res.hc, rect = T, k_colors ="black",
#' rect_border = 2:5, rect_lty = 1)
#' 
#' # Customized color for groups
#' fviz_dend(res.hc, k = 4, 
#'  k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"))
#' 
#' }
#' @export
fviz_dend <- function(x, k = NULL, k_colors = NULL, color_labels_by_k = FALSE,
                      type = c("rectangle", "triangle"),
                      rect = FALSE, rect_border = "gray", rect_lty = 2, rect_lwd = 1.5, 
                      cex = 0.8, main = "Cluster Dendrogram", xlab = "", ylab = "Height", ...)
{
  
  if(inherits(x, "hcut")){
    k <- x$nbclust
    dend <- as.dendrogram(x)
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
  
  plot(dend,  type = type[1], xlab = xlab, ylab = ylab, main = main, ...)
  if(rect & !is.null(k))
    dendextend::rect.dendrogram(dend, k=k, border = rect_border, 
                                lty = rect_lty, lwd = rect_lwd)
}
