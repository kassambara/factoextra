#' Enhanced Visualization of Dendrogram
#' 
#' @description Draws easily beautiful dendrograms using either R base plot or 
#'   ggplot2. Provides also an option for drawing a circular dendrogram and 
#'   phylogenic trees.
#' @param x an object of class dendrogram, hclust, agnes, diana, hcut, 
#'   hkmeans or HCPC (FactoMineR).
#' @param k the number of groups for cutting the tree.
#' @param h a numeric value. Cut the dendrogram by cutting at height h. (k 
#'   overrides h)
#' @param k_colors,palette a vector containing colors to be used for the groups.
#'   It should contains k number of colors. Allowed values include also "grey" 
#'   for grey color palettes; brewer palettes e.g. "RdBu", "Blues", ...;  and 
#'   scientific journal palettes from ggsci R package, e.g.: "npg", "aaas", 
#'   "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty".
#' @param show_labels a logical value. If TRUE, leaf labels are shown. Default 
#'   value is TRUE.
#' @param color_labels_by_k logical value. If TRUE, labels are colored 
#'   automatically by group when k != NULL.
#' @param label_cols a vector containing the colors for labels.
#' @param labels_track_height a positive numeric value for adjusting the room for the 
#'   labels. Used only when type = "rectangle".
#' @param repel logical value. Use repel = TRUE to avoid label overplotting when
#'   type = "phylogenic".
#' @param lwd a numeric value specifying branches and rectangle line width.
#' @param type type of plot. Allowed values are one of "rectangle", "triangle", 
#'   "circular", "phylogenic".
#' @param phylo_layout the layout to be used for phylogenic trees. Default value
#'   is "layout.auto". Allowed values include: 
#'   \code{\link[igraph]{layout.auto}}, \code{\link[igraph]{layout_with_drl}}, 
#'   \code{\link[igraph]{layout_as_tree}}, \code{\link[igraph]{layout.gem}},
#'   \code{\link[igraph]{layout.mds}} and \code{\link[igraph]{layout_with_lgl}}.
#' @param rect logical value specifying whether to add a rectangle around 
#'   groups. Used only when k != NULL.
#' @param rect_border,rect_lty border color and line type for rectangles.
#' @param rect_fill a logical value. If TRUE, fill the rectangle.
#' @param lower_rect a value of how low should the lower part of the rectangle 
#'   around clusters. Ignored when rect = FALSE.
#' @param horiz a logical value. If TRUE, an horizontal dendrogram is drawn.
#' @param cex size of labels
#' @param main,xlab,ylab main and axis titles
#' @param sub Plot subtitle. If NULL, the method used hierarchical clustering is
#'   shown. To remove the subtitle use sub = "".
#' @param ggtheme function, ggplot2 theme name. Default value is 
#'   theme_classic(). Allowed values include ggplot2 official themes: 
#'   theme_gray(), theme_bw(), theme_minimal(), theme_classic(), theme_void(), 
#'   ....
#' @param ... other arguments to be passed to the function plot.dendrogram()
#' @return an object of class fviz_dend which is a ggplot with the attributes 
#'   "dendrogram" accessible using attr(x, "dendrogram"), where x is the result 
#'   of fviz_dend().
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
fviz_dend <- function(x, k = NULL, h = NULL, k_colors = NULL, palette = NULL,  show_labels = TRUE, color_labels_by_k = TRUE,
                      label_cols = NULL,  labels_track_height = NULL, repel = FALSE, lwd = 0.7,
                      type = c("rectangle",  "circular", "phylogenic"),
                      phylo_layout = "layout.auto",
                      rect = FALSE, rect_border = "gray", rect_lty = 2, rect_fill = FALSE, lower_rect,
                      horiz = FALSE, cex = 0.8, main = "Cluster Dendrogram", xlab = "", ylab = "Height", 
                      sub = NULL, ggtheme = theme_classic(),  ...)
{
  
#  if(.is_col_palette(k_colors)) palette <- k_colors
 # else palette <- NULL
  if(missing(k_colors) & !is.null(palette)) {
    k_colors <- palette
    palette <- NULL
  }
  if(!color_labels_by_k & is.null(label_cols)) label_cols <- "black"
  type <- match.arg(type)
  circular <- type == "circular"
  phylogenic <- type == "phylogenic"
  rectangle <- type == "rectangle"
  
  if(inherits(x, "HCPC")){
    k <- length(unique(x$data.clust$clust))
    #k <- x$call$t$nb.clust
    x <- x$call$t$tree #hclust
  }
    
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
  if(is.null(method)) method <- ""
  else if(is.na(method)) method <- ""
  if(is.null(sub) & method!="") sub = paste0("Method: ", method)
  
  if(!is.null(dendextend::labels_cex(dend))) cex <- dendextend::labels_cex(dend)
  dend <- dendextend::set(dend, "labels_cex", cex) 
  dend <- dendextend::set(dend, "branches_lwd", lwd) 
  
  k <- .get_k(dend, k, h)
  if(!is.null(k)) {
    if(.is_color_palette(k_colors)) k_colors <- ggpubr::get_palette(k_colors, k = k)
    else if(is.null(k_colors)) k_colors <- ggpubr::get_palette("default", k = k)
    dend <- dendextend::set(dend, what = "branches_k_color", k = k, value = k_colors)
    if(color_labels_by_k) dend <- dendextend::set(dend, "labels_col",  k = k, value = k_colors)
  }
  
  if(!is.null(label_cols)){
    dend <- dendextend::set(dend, "labels_col", label_cols) 
  }
  
  leaflab <- ifelse(show_labels, "perpendicular", "none")
  
  if(xlab =="") xlab <- NULL
  if(ylab=="") ylab <- NULL
  
  max_height <- max(dendextend::get_branches_heights(dend))
  if(missing(labels_track_height))
    labels_track_height <- max_height/8
  if(max_height < 1) offset_labels <- -max_height/100
  else offset_labels <- -0.1
  
  if(rectangle | circular){
    p <- .ggplot_dend(dend, type = "rectangle", offset_labels = offset_labels, nodes = FALSE,
                      ggtheme = ggtheme, horiz = horiz, circular = circular, palette = palette,
                      labels = show_labels, label_cols = label_cols, 
                      labels_track_height = labels_track_height, ...)
    if(!circular) p <- p + labs(title = main, x = xlab, y = ylab)
  }
  else if(phylogenic){
    p <- .phylogenic_tree(dend, labels = show_labels, label_cols = label_cols,
                                 palette = palette, repel = repel,
                                 ggtheme = ggtheme, phylo_layout = phylo_layout, ...)
  }
#   base plot
#   else{
#     plot(dend,  type = type[1], xlab = xlab, ylab = ylab, main = main,
#          leaflab = leaflab, sub = sub, horiz = horiz,...)
#     if(rect & !is.null(k))
#       dendextend::rect.dendrogram(dend, k=k, border = rect_border, 
#                                   lty = rect_lty, lwd = lwd)
#   }
  
  # Add rectangle around clusters
  if(circular | phylogenic | is.null(k)) rect <- FALSE
  if(rect_fill & missing(rect_lty)) rect_lty = "blank"
  if(missing(lower_rect)) lower_rect = -(labels_track_height+0.5)
  if(rect){
    p <- p + .rect_dendrogram(dend, k = k, palette = rect_border, rect_fill = rect_fill,
                              rect_lty = rect_lty, linewidth = lwd, 
                              lower_rect = lower_rect)
  }
  
  attr(p, "dendrogram") <- dend
  structure(p, class = c(class(p), "fviz_dend"))
  return(p)
}


# require igraph
.phylogenic_tree <- function(dend, labels = TRUE, label_cols = NULL,
                              palette = NULL, repel = FALSE,
                              ggtheme = theme_classic(),
                             phylo_layout = "layout.auto", ...){
  
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("igraph package needed for phylogenic tree. Please install it using install.packages('igraph').")
  }
  
  
  allowed_layouts <- c("layout.auto", "layout_with_drl", "layout_as_tree", 
                      "layout.gem", "layout.mds", "layout_with_lgl")
  
  if(!(phylo_layout %in% allowed_layouts)) stop( phylo_layout, " is not supported as layout. ", "Allowed phylogenic layout are: ",
                                                paste( allowed_layouts, collapse = ", "))
  
  layout_func <- switch(phylo_layout,
                        layout.auto = igraph::layout.auto,
                        layout_with_drl = igraph::layout_with_drl,
                        layout_as_tree = igraph::layout_as_tree,
                        layout.gem = igraph::layout.gem,
                        layout.mds = igraph::layout.mds,
                        layout_with_lgl = igraph::layout_with_lgl
                        )
  
  # Convert to 'phylo' object
  hc <- stats::as.hclust(dend)
  phylo_tree <- .as.phylo(hc)
  graph_edges <- phylo_tree$edge
  
  # get graph from edge list
  graph_net <- igraph::graph.edgelist(graph_edges)
  
  # extract layout (x-y coords)
  set.seed(123)
  graph_layout = layout_func(graph_net)
  # number of observations
  nobs <- length(hc$labels)
  
  # draw tree branches
  data.segments <- data.frame(
    x = graph_layout[graph_edges[,1],1], 
    y = graph_layout[graph_edges[,1],2],
    xend = graph_layout[graph_edges[,2],1],
    yend = graph_layout[graph_edges[,2],2],
    stringsAsFactors = TRUE
  )
  
  data.labels <- data.frame(
    x = graph_layout[1:nobs,1], 
    y = graph_layout[1:nobs,2],
    label = phylo_tree$tip.label,
    stringsAsFactors = TRUE
  )
  data.labels <- data.labels[order(as.vector(data.labels$label)), ]
  
  # Support for dendextend
  gdend <- dendextend::as.ggdend(dend)
  gdat <- dendextend::prepare.ggdend(gdend)
  gdat$labels <- gdat$labels[order(as.vector(gdat$labels$label)), ]
  data.labels <- cbind(data.labels, gdat$labels[, c("col", "cex")])
  if(!is.null(dendextend::labels_cex(dend))) font.label <- round(dendextend::labels_cex(dend)[1]*12)
  else font.label <- 12
  
  # FIX: ggplot2 3.0.0+ deprecation - aes_string() replaced with aes() + .data pronoun
  # See: https://github.com/kassambara/factoextra/issues/190
  p <- ggplot() + geom_segment(data = data.segments,
                    aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]]),
                    lineend = "square")
  if(is.null(label_cols)) label_cols <- "col"
  if(!labels) labels <- NULL
  else labels <- "label"
  
  p <- ggpubr::ggscatter(data.labels, "x", "y", label = labels,
                         color = label_cols,
                         ggp = p, repel = repel, font.label = font.label, ...)
  
  if(is.null(palette)) p <- p + scale_colour_identity()
  
  p <- ggpubr::ggpar(p, ggtheme = ggtheme, palette = palette, ...) 
  
  p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                 axis.text= element_blank(), 
                 axis.line = element_blank(), axis.ticks = element_blank(),
                 legend.position = "none")
  p
}


# Helper functions
#%%%%%%%%%%%%%%%%%%%%

# Plot dendrogram using ggplot
# .ggplot_dend derrived from dendextend::ggplot.ggdend
# data: a ggdend class object.
.ggplot_dend <- function (dend, segments = TRUE, labels = TRUE, nodes = TRUE, 
                        horiz = FALSE, ggtheme = theme_classic(), 
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
    # FIX: ggplot2 3.0.0+ deprecation - aes_string() replaced with aes() + .data pronoun
    # FIX: ggplot2 3.4.0+ deprecation - size replaced with linewidth for line geoms
    # See: https://github.com/kassambara/factoextra/issues/190, #191
    p <- p + geom_segment(data = data$segments,
      aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]],
                 colour = .data[["col"]], linetype = .data[["lty"]], linewidth = .data[["lwd"]]), lineend = "square") +
      # FIX: ggplot2 3.3.4+ deprecation - use "none" instead of FALSE for guides()
      # See: https://github.com/kassambara/factoextra/issues/179
      guides(linetype = "none", col = "none") + #scale_colour_identity() +
      scale_size_identity() + scale_linetype_identity()
    if(is.null(palette)) p <- p + scale_colour_identity()
  }
  if (nodes) {
    # FIX: ggplot2 3.0.0+ deprecation - aes_string() replaced with aes() + .data pronoun
    # See: https://github.com/kassambara/factoextra/issues/190
    p <- p + geom_point(data = data$nodes,
      aes(x = .data[["x"]], y = .data[["y"]], colour = .data[["col"]], shape = .data[["pch"]], size = .data[["cex"]])) +
      # FIX: ggplot2 3.3.4+ deprecation - use "none" instead of FALSE for guides()
      # See: https://github.com/kassambara/factoextra/issues/179
      guides(shape = "none", col = "none", size = "none") +
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

# Function used for circular dendrogram
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


# Convert 'hclust' to 'phylo' object
# used for phylogenic tree
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# from ape::as.phylo.hclust
# x hclust
.as.phylo <- function (x, ...) 
{
  N <- dim(x$merge)[1]
  edge <- matrix(0L, 2 * N, 2)
  edge.length <- numeric(2 * N)
  node <- integer(N)
  node[N] <- N + 2L
  cur.nod <- N + 3L
  j <- 1L
  for (i in N:1) {
    edge[j:(j + 1), 1] <- node[i]
    for (l in 1:2) {
      k <- j + l - 1L
      y <- x$merge[i, l]
      if (y > 0) {
        edge[k, 2] <- node[y] <- cur.nod
        cur.nod <- cur.nod + 1L
        edge.length[k] <- x$height[i] - x$height[y]
      }
      else {
        edge[k, 2] <- -y
        edge.length[k] <- x$height[i]
      }
    }
    j <- j + 2L
  }
  if (is.null(x$labels)) 
    x$labels <- as.character(1:(N + 1))
  obj <- list(edge = edge, edge.length = edge.length/2, tip.label = x$labels, 
              Nnode = N)
  class(obj) <- "phylo"
  obj
  #ape::reorder.phylo(obj)
}

# Get k value if h specified
# Make also some checking
# dend a dendrogram object
# h: tree height
.get_k <- function(dend, k = NULL, h = NULL){
  
  if (!dendextend::is.dendrogram(dend)) stop("x is not a dendrogram object.")
  if (length(h) > 1L | length(k) > 1L) 
    stop("'k' and 'h' must be a scalar(i.e.: of length 1)")
  tree_heights <- dendextend::heights_per_k.dendrogram(dend)[-1]
  tree_order <- stats::order.dendrogram(dend)
  if (!is.null(h)) {
    if (!is.null(k)) 
      stop("specify exactly one of 'k' and 'h'")
    ss_ks <- tree_heights < h
    k <- min(as.numeric(names(ss_ks))[ss_ks])
    k <- max(k, 2)
  }
  k
}


# Add rectangle to a dendrogram
# lower_rect: a (scalar) value of how low should the lower part of the rect be.
.rect_dendrogram <- function (dend, k = NULL,  h = NULL, 
                             k_colors = NULL, palette = NULL, rect_fill = FALSE, rect_lty = 2, 
                             lower_rect=-1.5, 
          ...) 
{
  
  if(missing(k_colors) & !is.null(palette)) k_colors <- palette
  # value (should be between 0 to 1): proportion of the height 
  # our rect will be between the height needed for k and k+1 clustering.
  prop_k_height <-  0.5
  
  if (!dendextend::is.dendrogram(dend)) 
    stop("x is not a dendrogram object.")
  
  k <- .get_k(dend, k, h)
  tree_heights <- dendextend::heights_per_k.dendrogram(dend)[-1]
  tree_order <- stats::order.dendrogram(dend)
  
  if (is.null(k)) stop("specify k")
  if (k < 2) {
      stop(gettextf("k must be between 2 and %d", length(tree_heights)), 
              domain = NA)
  }

  cluster <- dendextend::cutree(dend, k = k)
  clustab <- table(cluster)[unique(cluster[tree_order])]
  m <- c(0, cumsum(clustab))
  which <- 1L:k
  
  xleft <- ybottom <- xright <- ytop <- list()
  for (n in seq_along(which)) {
    next_k_height <- tree_heights[names(tree_heights) == k + 1]
    if (length(next_k_height) == 0) {
      next_k_height <- 0
      prop_k_height <- 1
    }
    
    xleft[[n]] = m[which[n]] + 0.66
    ybottom[[n]] = lower_rect
    xright[[n]] = m[which[n] + 1] + 0.33
    ytop[[n]] <- tree_heights[names(tree_heights) == k] * 
      prop_k_height + next_k_height * (1 - prop_k_height)
  }
  
  df <- data.frame(xmin = unlist(xleft), ymin = unlist(ybottom), xmax = unlist(xright), ymax = unlist(ytop), stringsAsFactors = TRUE)
  
  color <- k_colors
  # FIX: R 4.2.0+ deprecation - condition has length > 1 warning
  # Use length() == 1 check before scalar comparison
  # See: https://github.com/kassambara/factoextra/issues/163, #180
  if(length(color) == 1 && color == "cluster") color <- "default"
  if(.is_color_palette(color)) color <- ggpubr::get_palette(color, k = k)
  else if(length(color) > 1 && length(color) < k){
    color <- rep(color, k)[1:k]
  }
  if(rect_fill){
    fill <- color
    alpha <- 0.2
  }
  else {
    fill <- "transparent"
    alpha <- 0
  }
  df$color <- color
  
  df$cluster <- as.factor(paste0("c", 1:k))
  ggpubr::geom_exec(geom_rect, data = df, 
                    xmin = "xmin", ymin = "ymin", xmax = "xmax", ymax = "ymax",
                    fill = fill, color = color, linetype = rect_lty, alpha = alpha,  ...)
}
