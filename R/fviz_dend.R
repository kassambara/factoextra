#' Enhanced Visualization of Dendrogram
#' 
#' @description Draws easily beautiful dendrogram using either R base plot or 
#'   ggplot2. Provides also an option for drawing a circular dendrogram.
#' @param x an object of class dendrogram, hclust, agnes, diana, hcut or 
#'   hkmeans.
#' @param k the number of groups for cutting the tree.
#' @param k_colors a vector containing colors to be used for the groups. It 
#'   should contains k number of colors. If as.ggplot = TRUE, Allowed values 
#'   include also "grey" for grey color palettes; brewer palettes e.g. "RdBu", 
#'   "Blues", ...;  and scientific journal palettes from ggsci R package, e.g.: 
#'   "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and 
#'   "rickandmorty".
#' @param show_labels a logical value. If TRUE, leaf labels are shown. Default 
#'   value is TRUE.
#' @param color_labels_by_k logical value. If TRUE, labels are colored 
#'   automatically by group when k != NULL.
#' @param label_cols a vector containing the colors for labels.
#' @param labels_track_height a numeric value for adjusting the room for the
#'   labels. Ignored when as.ggplot = FALSE.
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
#'   Default value is theme_classic(). Allowed values include ggplot2 official 
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
fviz_dend <- function(x, k = NULL, k_colors = NULL,  show_labels = TRUE, color_labels_by_k = TRUE,
                      label_cols = NULL,  labels_track_height = 1, lwd = 1,
                      type = c("rectangle", "triangle"),
                      rect = FALSE, rect_border = "gray", rect_lty = 2, rect_lwd = 1.5, 
                      as.ggplot = FALSE, ggtheme = theme_classic(), horiz = FALSE, circular = FALSE,
                      cex = 0.8, main = "Cluster Dendrogram", xlab = "", ylab = "Height", 
                      sub = NULL, ...)
{
  
  if(.is_col_palette(k_colors)) palette <- k_colors
  else palette <- NULL
  if(!color_labels_by_k & is.null(label_cols)) label_cols <- "black"
  
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
  if(as.ggplot |circular) lwd <- lwd/1.5
  
  dend <- dendextend::set(dend, "labels_cex", cex) 
  dend <- dendextend::set(dend, "branches_lwd", lwd) 
  
  if(!is.null(k)) {
    if(is.null(k_colors) | .is_col_palette(k_colors)) k_colors <- grDevices::rainbow(k)
    dend <- dendextend::set(dend, what = "branches_k_color", k = k, value = k_colors)
    if(color_labels_by_k) dend <- dendextend::set(dend, "labels_col",  k = k, value = k_colors)
  }
  
  if(!is.null(label_cols)){
    dend <- dendextend::set(dend, "labels_col", label_cols) 
  }
  
  leaflab <- ifelse(show_labels, "perpendicular", "none")
  
  if(as.ggplot){
    if(xlab =="") xlab <- NULL
    if(ylab=="") ylab <- NULL
  }
  
  if(as.ggplot | circular){
    p <- .ggplot_dend(dend, type = "rectangle", offset_labels = -0.1, nodes = FALSE,
                      ggtheme = ggtheme, horiz = horiz, circular = circular, palette = palette,
                      label_cols = label_cols, labels_track_height = labels_track_height, ...)
    if(!circular) p <- p + labs(title = main, x = xlab, y = ylab)
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


# Helper functions
#%%%%%%%%%%%%%%%%%%%%
# .ggplot_dend derrived from dendextend::ggplot.ggdend
# data: a ggdend class object.
.ggplot_dend <- function (dend, segments = TRUE, labels = TRUE, nodes = TRUE, 
                        horiz = FALSE, ggtheme = theme_ggdend(), 
                        offset_labels = 0, circular = FALSE, type = "rectangle",
                        palette = NULL, label_cols = NULL, labels_track_height = 1,
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
  # To avoid overlaping of labels at coord_polar start
  if(circular) {
    n_rows <- nrow(data$labels)
    data$labels$x[1] <- 0.7
    data$labels$vjust[1] <- 1.7
  }

  
  p <- ggplot()
  if (segments) {
    p <- p + geom_segment(data = data$segments,
      aes_string(x = "x", y = "y", xend = "xend", yend = "yend", 
                 colour = "col", linetype = "lty", size = "lwd"), lineend = "square") +
      guides(linetype = FALSE, col = FALSE) + #scale_colour_identity() + 
      scale_size_identity() + scale_linetype_identity()
    if(is.null(palette)) p <- p + scale_colour_identity()
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
    if(is.null(label_cols)) label_cols <- "col"
    p <- p + ggpubr::geom_exec(geom_text, data = data$labels,
                               x = "x", y = "y", label = "label", color = label_cols, size = "cex",
                                angle = "angle", hjust = "hjust", vjust = "vjust")
  }
  p <- ggpubr::ggpar(p, ggtheme = ggtheme, palette = palette, ...) + theme(axis.line = element_blank())
  if (horiz & !circular) {
    p <- p + coord_flip() + scale_y_reverse()+
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.x = element_text())
  }
  else p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  if(circular){
    p <- p + theme(plot.margin = margin(0, 0, 0, 0),
                  axis.title.x = element_blank(), axis.title.y = element_blank(),
                  axis.text= element_blank(), 
                  axis.line = element_blank(), axis.ticks = element_blank())+
                  ylim(max(dendextend::get_branches_heights(dend)), -1)+
                  coord_polar(theta = 'x', direction = 1)
    
  }
  else{
    p <- p + expand_limits(y=-labels_track_height)
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


.is_col_palette <- function(palette){
  brewerpal <- c(
    # sequential
    'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
    'YlGn', 'YlGnBu YlOrBr', 'YlOrRd',
    #Divergent
    'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral',
    # Qualitative
    'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3'
  )
  # Scientific Journal and Sci-Fi Themed Color Palettes for ggplot2
  # ggsci package: https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html
  ggscipal <- c("npg", "aaas", "lancet", "jco",
                "ucscgb", "uchicago", "simpsons", "rickandmorty")
  
  if(is.null(palette)) return(FALSE)
  else return(length(palette)==1 & palette[1] %in% c(brewerpal, ggscipal))
}


