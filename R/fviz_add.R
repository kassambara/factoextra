#' Add supplementary data to a plot
#' 
#' @description
#' Add supplementary data to a plot
#'  
#' @param ggp a ggplot2 plot.
#' @param df a data frame containing the x and y coordinates
#' @param axes a numeric vector of length 2 specifying the components to be plotted.
#' @param geom a character specifying the geometry to be used for the graph
#'  Allowed values are "point" or "arrow"
#' @param color the color to be used
#' @param addlabel a logical value. If TRUE, labels are added
#' @param labelsize the size of labels. Default value is 4
#' @param shape point shape when geom ="point"
#' @param linetype the linetype to be used when geom ="arrow"
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#'  }
#'  
#' @export 
fviz_add <- function(ggp, df, axes = c(1,2), geom=c("point", "arrow"), color ="blue", 
                     addlabel = TRUE, labelsize = 4, shape=19, linetype ="dashed")
{
  if(!inherits(df, c("data.frame", "matrix")))
     stop("df should be a data frame or a matrix")
     
  if(ncol(df) < 2)
    stop("df should have at least two columns (x and y coordinates)")
  
  if(length(intersect(geom, c("point", "arrow"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  
  df <- data.frame(name =rownames(df), x = df[,axes[1]], y = df[,axes[2]]) 
  
  if(geom[1]=="point"){
    p <-  ggp + geom_point(data = df, aes(x, y), color = color, shape = shape)
    if(addlabel) 
      p <- p + geom_text(data = df, aes(x, y), color = color,
                         label = df$name, size = labelsize, vjust=-0.7)
  }
  else if(geom[1]=="arrow"){
    p <- ggp + geom_segment(data = df,
                      aes(x = 0, y = 0, xend = x, yend = y),
                      arrow = arrow(length = unit(0.2, 'cm')), 
                      color=color, linetype=linetype)
    if(addlabel)
      p <- p + geom_text(data = df, aes(x, y),
                         label = df$name, color=color, 
                         size = labelsize, hjust=0.8, vjust=0) 
  }
  
  return(p)
}