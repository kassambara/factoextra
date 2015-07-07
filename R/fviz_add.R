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
#' @param jitter a parameter used to jitter the points in order to reduce overplotting. 
#' It's a list containing the objects what, width and height (i.e jitter = list(what, width, height)). 
#' \itemize{
#' \item what: the element to be jittered. Possible values are "point" or "p"; "label" or "l"; "both" or "b".
#' \item width: degree of jitter in x direction
#' \item height: degree of jitter in y direction
#' }
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
                     jitter = list(what = "label", width = NULL, height = NULL))
{
  if(!inherits(df, c("data.frame", "matrix")))
     stop("df should be a data frame or a matrix")
     
  if(ncol(df) < 2)
    stop("df should have at least two columns (x and y coordinates)")
  
  if(length(intersect(geom, c("point", "arrow", "text"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  
  df <- data.frame(name = rownames(df), x = df[,axes[1]], y = df[,axes[2]]) 
  label_coord <- df
  
  # jittering
  if(jitter$what %in% c("both", "b")){
    label_coord <- df <- .jitter(data, jitter)
  }
  else if(jitter$what %in% c("point", "p")){
    df<- .jitter(df, jitter)
  }
  else if(jitter$what %in% c("label", "l")){
    label_coord <- .jitter(label_coord, jitter)
  }
  
  
  if("point" %in% geom) {
    p <-  ggp + geom_point(data = df, aes_string("x", "y"), 
                           color = color, shape = shape, size = pointsize)
    if(addlabel) 
      p <- p + geom_text(data = label_coord, aes_string("x", "y"), color = color,
                         label = df$name, size = labelsize, vjust=-0.7)
  }
  else if("arrow" %in% geom){
    p <- ggp + geom_segment(data = df,
                      aes_string(x = 0, y = 0, xend = 'x', yend = 'y'),
                      arrow = grid::arrow(length = grid::unit(0.2, 'cm')), 
                      color=color, linetype=linetype)
    if(addlabel)
      p <- p + geom_text(data = label_coord, aes_string("x", "y"),
                         label = df$name, color=color, 
                         size = labelsize, hjust=0.8, vjust=0) 
  }
  else if("text" %in% geom)
    p <- ggp + geom_text(data = label_coord, aes_string("x", "y"), color = color,
                       label = df$name, size = labelsize, vjust=-0.7)
  
  return(p)
}