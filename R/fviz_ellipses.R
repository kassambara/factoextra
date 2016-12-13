#' @include utilities.R
NULL
#' Draw confidence ellipses around the categories
#' @description Draw confidence ellipses around the categories
#' @inheritParams fviz
#' @inheritParams ggpubr::ggpar
#' @param X an object of class MCA, PCA or MFA.
#' @param habillage a numeric vector of indexes of variables or a
#'   character vector of names of variables. Can be also a data frame containing grouping variables.
#' @param geom a text specifying the geometry to be used for the graph.  Allowed
#'   values are the combination of c("point", "text"). Use "point" (to show only
#'   points); "text" to show only labels; c("point", "text") to show both types.
#' @param ... Arguments to be passed to the functions ggpubr::ggscatter()  &
#'   ggpubr::ggpar().
#'   
#' @return a ggplot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'   
#' @rdname fviz_ellipses
#' @examples
#' \donttest{
#' 
#' # Multiple Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mca <- MCA(poison, quanti.sup = 1:2, 
#'               quali.sup = 3:4, graph=FALSE)
#'               
#' fviz_ellipses(res.mca, 1:4, geom = "point",
#' palette = "jco") 
#' 
#'  }
#' @export
fviz_ellipses <- function(X, habillage, axes = c(1,2), 
                          addEllipses = TRUE, ellipse.type = "confidence",
                          palette = NULL, pointsize = 1, geom = c("point", "text"),
                          ggtheme = theme_bw(),...){
  
  df <- facto_summarize(X, element = "ind", axes = axes, result = c("coord"))
  colnames(df)[2:3] <-  c("x", "y")
  # augment data, if qualitative variable is used to color points by groups
  dd <- .add_ind_groups(X, df, habillage)
  df <- dd$ind
  color <- dd$name.quali
  
  if(length(habillage)==1 | is.factor(habillage)) legend. <- "right" else legend. = "none"
  
  label <- NULL
  if("text" %in% geom) label <- "name"
  p <- ggpubr::ggscatter(df, x = "x", y = "y", color = color, palette = palette,
                    ellipse = addEllipses, ellipse.type = ellipse.type,
                    legend = legend., ggtheme = ggtheme, mean.point = TRUE,
                    label = label, size = pointsize, ...)
  
  # Faceting when multiple variables are used to color individuals
  # in this case there is a column "facet_vars" in df
  if("facet_vars" %in% colnames(df)){
    groups <- c("facet_vars", "Groups")
    xx <- ggpubr::desc_statby(df, measure.var = "x", grps = groups)[, c(groups, "mean")]
    colnames(xx)[ncol(xx)] <- "x"
    yy <- ggpubr::desc_statby(df, measure.var = "y", grps = groups)[, c(groups, "mean")]
    xx$y <- yy$mean
    grp_coord <- xx
    p <- ggpubr::ggtext(grp_coord, x = "x", y = "y", color = color,
                             palette = palette,
                           legend = legend., ggtheme = ggtheme,
                           label = "Groups",  ggp = p, ...) 
    p <- p + facet_wrap(~facet_vars) 
  }
  
  p <- .fviz_finish(p, X, axes,  ...) 
  p
}
