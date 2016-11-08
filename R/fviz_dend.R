#' Enhanced Visualization of Dendrogram
#' 
#' @description Enhanced visualization of dendrogram.
#' @param x an object of class dendrogram, hclust, agnes, diana, hcut or 
#'   hkmeans.
#' @param k the number of groups for cutting the tree.
#' @param k_colors a vector containing colors to be used for the groups. It 
#'   should contains k number of colors.
#' @param show_labels a logical value. If TRUE, leaf labels are shown. Default 
#'   value is TRUE.
#' @param color_labels_by_k logical value. If TRUE, labels are colored 
#'   automatically by group when k != NULL.
#' @param label_cols a vector containing the colors for labels.
#' @param lwd a numeric value specifying branches line width.
#' @param type type of plot. Allowed values are one of "rectangle" or "triangle"
#' @param rect logical value specifying whether to add a rectangle around 
#'   groups. Used only when k != NULL.
#' @param rect_border,rect_lty,rect_lwd border color, line type and line width 
#'   for rectangles
#' @param as.ggplot a logical value. If TRUE, the dendrogram is plotted using 
#'   ggplot2.
#' @param horiz a logical value. If TRUE, an horizontal dendrogram is drawn.
#' @param circular a logical value. If TRUE, draws a circular dendogram.
#' @param cex size of labels
#' @param main,xlab,ylab main and axis titles
#' @param sub Plot subtitle. If NULL, the method used hierarchical clustering is
#'   shown. To remove the subtitle use sub = "".
#' @param ggtheme function, ggplot2 theme name. Ignored when as.ggplot = FALSE.
#'   Default value is theme_ggdend(). Allowed values include ggplot2 official
#'   themes: theme_gray(), theme_bw(), theme_minimal(), theme_classic(),
#'   theme_void(), ....
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
                      label_cols = NULL, lwd = 1,
                      type = c("rectangle", "triangle"),
                      rect = FALSE, rect_border = "gray", rect_lty = 2, rect_lwd = 1.5, 
                      as.ggplot = FALSE, ggtheme = theme_ggdend(), horiz = FALSE, circular = FALSE,
                      cex = 0.8, main = "Cluster Dendrogram", xlab = "", ylab = "Height", 
                      sub = NULL, ...)
{
  
  if(inherits(x, "hcut")){
    k <- x$nbclust
    dend <- as.dendrogram(x)
    method <- x$method
  }
  else if(inherits(x, "hkmeans")){
    k <- length(unique(x$cluster))
    dend <- as.dendrogram(x$hclust)
    method <- x$hclust$method
  }
  else if(inherits(x, c("hclust", "agnes", "diana"))) {
    dend <- as.dendrogram(x)
    method <- x$method
  }
  else if(inherits(x, "dendrogram")) {
    dend <- x
    method <- ""
  }
  else stop("Can't handle an object of class ", paste(class(x), collapse =", ") )
  
  if(is.null(sub) & method!="") sub = paste0("Method: ", method)
  
  dend <- dendextend::set(dend, "labels_cex", cex) 
  dend <- dendextend::set(dend, "branches_lwd", lwd) 
  
  if(!is.null(k)) {
    if(is.null(k_colors)) k_colors <- grDevices::rainbow(k)
    dend <- dendextend::set(dend, what = "branches_k_color", k = k, value = k_colors)
    if(color_labels_by_k) dend <- dendextend::set(dend, "labels_col",  k = k, value = k_colors)
  }
  if(!is.null(label_cols)){
    dend <- dendextend::set(dend, "labels_col", label_cols) 
  }
  
  leaflab <- ifelse(show_labels, "perpendicular", "none")
  
  if(as.ggplot & !circular){
    p <- .ggplot_dend(dend, type = "rectangle", offset_labels = -0.1, ggtheme = ggtheme, horiz = horiz)+
      expand_limits(y=-1)+
      labs(title = main, x = xlab, y = ylab)
    if(horiz) p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    else p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    return(p)
  }
  else if(circular){
    p <- .ggplot_dend(dend, type = "rectangle", offset_labels = -0.1,
                      circular = circular, ggtheme = ggtheme)
    return(p)
  }
  else{
    plot(dend,  type = type[1], xlab = xlab, ylab = ylab, main = main,
         leaflab = leaflab, sub = sub, horiz = horiz,...)
    if(rect & !is.null(k))
      dendextend::rect.dendrogram(dend, k=k, border = rect_border, 
                                  lty = rect_lty, lwd = rect_lwd)
  }
}

#' @export
#' @describeIn fviz_dend default theme used for dendrogram plotted with ggplot2
theme_ggdend <- function() {
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.title.x = element_text(colour = NA), 
        axis.title.y = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.line = element_blank(), 
        axis.ticks = element_blank())
}


# Helper functions
#%%%%%%%%%%%%%%%%%%%%
# data: a ggdend class object.
.ggplot_dend <- function (dend, segments = TRUE, labels = TRUE, nodes = TRUE, 
                        horiz = FALSE, ggtheme = theme_ggdend(), 
                        offset_labels = 0, circular = FALSE, type = "rectangle",
                        ...) {
  
  gdend <- dendextend::as.ggdend(dend, type = type)
  #angle <- ifelse(horiz, 0, 90)
  #hjust <- ifelse(horiz, 0, 1)
  gdend$labels$angle <- ifelse(horiz, 0, 90)
  gdend$labels$hjust <- ifelse(horiz, 0, 1)
  gdend$labels$vjust <- 0.5
  if(circular){
    # If circular, change the angle and hjust so that the labels rotate
    if(circular) {
      pms <- .get_label_params(gdend$labels)
      gdend$labels$angle <- pms$angle
      gdend$labels$hjust <- pms$hjust
    }
  }
  
  data <- dendextend::prepare.ggdend(gdend)

  
  p <- ggplot()
  if (segments) {
    p <- p + geom_segment(data = data$segments,
      aes_string(x = "x", y = "y", xend = "xend", yend = "yend", 
                 colour = "col", linetype = "lty", size = "lwd"), lineend = "square") +
      guides(linetype = FALSE, col = FALSE) + scale_colour_identity() + 
      scale_size_identity() + scale_linetype_identity()
  }
  if (nodes) {
    p <- p + geom_point(data = data$nodes, 
      aes_string(x = "x", y = "y", colour = "col", shape = "pch", size = "cex")) + 
      guides(shape = FALSE, col = FALSE, size = FALSE) + 
      scale_shape_identity()
  }
  if (labels) {
    data$labels$cex <- 5 * data$labels$cex
    data$labels$y <- data$labels$y + offset_labels
      p <- p + geom_text(data = data$labels, 
                         aes_string(x = "x", y = "y", label = "label", colour = "col",
                                    size = "cex", angle = "angle", hjust = "hjust", vjust = "vjust"))#edited
  }
  if (horiz) {
    p <- p + coord_flip() + scale_y_reverse(expand = c(0.2, 0))
  }
  p <- p + ggtheme
  
  if(circular){
    p <- p + theme(plot.margin = margin(0, 0, 0, 0),
                  axis.title.x = element_blank(), axis.title.y = element_blank(),
                  axis.text= element_blank(), 
                  axis.line = element_blank(), axis.ticks = element_blank())+
                  ylim(max(dendextend::get_branches_heights(dend)), -1)+
                  coord_polar(theta = 'x', direction = 1)
    
  }
  
  p
}

# Create the angle and hjust vectors so that the labels 
# rotation switches from 6 o'clock to 12 o'clock to improve readability. 
.get_label_params <- function(labeldf) {        
  nn <- length(labeldf$y)
  halfn <- floor(nn/2)
  firsthalf <- rev(90 + seq(0,360, length.out = nn))
  secondhalf <- rev(-90 + seq(0,360, length.out = nn))
  angle <- numeric(nn)
  angle[1:halfn] <- firsthalf[1:halfn]
  angle[(halfn+1):nn] <- secondhalf[(halfn+1):nn]
  
  hjust <- numeric(nn)
  hjust[1:halfn] <- 0
  hjust[(halfn+1):nn] <- 1
  
  return(list(angle = angle, hjust = hjust))
}



