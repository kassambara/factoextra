#' Add supplementary data to a plot
#' 
#' @description
#' Add supplementary data to a plot
#'  
#' @param ggp a ggplot2 plot.
#' @param df a data frame containing the x and y coordinates
#' @param axes a numeric vector of length 2 specifying the components to be plotted.
#' @param geom a character specifying the geometry to be used for the graph
#'  Allowed values are "point" or "arrow" or "text"
#' @param color the color to be used
#' @param addlabel a logical value. If TRUE, labels are added
#' @param labelsize the size of labels. Default value is 4
#' @param pointsize the size of points
#' @param shape point shape when geom ="point"
#' @param linetype the linetype to be used when geom ="arrow"
#' @param repel a boolean, whether to use ggrepel to avoid overplotting text 
#'   labels or not.
#' @param font.family character vector specifying font family.
#' @param ... Additional arguments, not used
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' # Principal component analysis
#' data(decathlon2)
#' decathlon2.active <- decathlon2[1:23, 1:10]
#' res.pca <- prcomp(decathlon2.active,  scale = TRUE)
#' 
#' # Visualize variables
#' p <- fviz_pca_var(res.pca)
#' print(p)
#' 
#' # Add supplementary variables
#' coord <- data.frame(PC1 = c(-0.7, 0.9), PC2 = c(0.25, -0.07))
#' rownames(coord) <- c("Rank", "Points")
#' print(coord)
#' fviz_add(p, coord, color ="blue", geom="arrow") 
#'  }
#'  
#' @export 
fviz_add <- function(ggp, df, axes = c(1,2), geom=c("point", "arrow"), color ="blue", 
                     addlabel = TRUE, labelsize = 4, pointsize = 2, shape=19, linetype ="dashed",
                     repel = FALSE, font.family = "", ...)
{
  # Deprecated arguments
  extra_args <- list(...)
  if (!is.null(extra_args$jitter)) {
    warning("argument jitter is deprecated; please use repel = TRUE instead, to avoid overlapping of labels.", 
            call. = FALSE)
    if(!is.null(extra_args$jitter$width) | !is.null(extra_args$jitter$height) ) repel = TRUE
  }
  
  if(!inherits(df, c("data.frame", "matrix")))
     stop("df should be a data frame or a matrix")
  if(!inherits(df, "data.frame")) df <- as.data.frame(df, stringsAsFactors = TRUE)
     
  if(ncol(df) < 2)
    stop("df should have at least two columns (x and y coordinates)")
  
  if(length(intersect(geom, c("point", "arrow", "text"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  
  if(is.null(df$name)) df$name <- rownames(df)
  if(is.null(df$x)) df$x <- df[,axes[1]]
  if(is.null(df$y)) df$y <- df[,axes[2]]
  
  # Plot
  #%%%%%%%%%%%%%%%%%%%%%%
  hjust <- vjust <- 0.5
  if("point" %in% geom) {
    p <- ggp + ggpubr::geom_exec(geom_point, data = df, x = "x", y = "y",
                                 color = color, shape = shape, size = pointsize)
    vjust <- -0.7
  }
  else if("arrow" %in% geom){
    # FIX: ggplot2 3.0.0+ deprecation - aes_string() replaced with aes() + .data pronoun
    # See: https://github.com/kassambara/factoextra/issues/190
    p <- ggp + geom_segment(data = df,
                      aes(x = 0, y = 0, xend = .data[["x"]], yend = .data[["y"]]),
                      arrow = grid::arrow(length = grid::unit(0.2, 'cm')),
                      color=color, linetype=linetype)
    hjust <- 0.8
    vjust <- 0
  }
  else if("text" %in% geom) {
    vjust <- -0.7
    p <- ggp
  }
  
  if(addlabel | "text" %in% geom){
    if(repel){
      p <- p + ggpubr::geom_exec(ggrepel::geom_text_repel, data = df, x = "x", y = "y", 
                                 label = "name", color = color, size = labelsize,
                                 family = font.family)
    }
    else{
      p <- p + ggpubr::geom_exec(geom_text, data = df, x = "x", y = "y", 
                                 label = "name", color = color, size = labelsize,
                                 vjust=vjust, hjust = hjust, family = font.family)
      
#       p <- p + geom_text(data = df, aes_string("x", "y"), color = color,
#                          label = df$name, size = labelsize, vjust=vjust, hjust = hjust)
      
    } 
  }
  
  
  return(p)
}