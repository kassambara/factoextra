#' @include get_pca_var.R
#' @include get_eigenvalue.R
#' @include get_pca_ind.R
#' @include fviz_pca_var.R
#' @include fviz_pca_ind.R
#' 
#' Visualizing Principal Component Analysis Variables using ggplot2
#' 
#' @description
#' This function can be used to visualize the output several PCA functions
#'  including PCA() from FactoMineR package; prcomp() and princomp() from stats package;
#'  dudi.pca() from ade4 package.
#' 
#' @param res.pca an object of class PCA (FactoMineR); prcomp (stats); princomp (stats);
#'  dudi and pca (ade4).
#' @param quanti.sup a numeric data.frame or matrix; rows are individuals and
#'  columns are supplementary quantitative variables
#' @param axes a numeric vector of length 2 specifying the component to be plotted.
#' @param label a character vector specifying the elements to be labelled.
#'  Default value is "all". Allowed values are "none" or the combination of c("var", "quanti.sup").
#'  "var" can be used to label only active variables.
#'  "quanti.sup" can be used to label only quantitative supplementary variables.
#' @param invisible a character value specifying the elements to be hidden on the plot.
#'  Default value is "none". Allowed values are the combination of c("var", "quanti.sup").
#' @param labelsize font size for the labels
#' @param col.var color for variables. The default value is "black".
#'  Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the colors for variables are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2+y2, "coord"), x values("x") or y values("y")
#' @param alpha.var controls the transparency of colors.
#' The value can variate from 0 (total transparency) to 1 (no transparency).
#' Default value is 1. Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the transparency for variable colors are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2+y2, "coord"), x values("x") or y values("y").
#' @param col.quanti.sup a color for the quantitative supplementary variables.
#' @param col.circle a color for the correlation circle.
#' @param ... optional arguments to be passed to the function get_pca_ind().
#'  This can includes the argument data (the original data used for pca) 
#'  which is required when res.pca is not from FactoMineR or adea4 packages.
#'  
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#'  library("FactoMineR")
#'  data(decathlon)
#'  res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13, graph = FALSE)
#'  fviz_pca_var(res.pca, col.var="contrib")
#'  
#'  # Change the color
#'  p <- fviz_pca_var(res.pca, col.var="contrib")
#'  p + scale_color_gradient2(low ="blue", mid="white", high="red", midpoint=40)
#'  
#'  # Change the theme
#'  p + theme_minimal()
#'  
#'  p + theme_classic()
#'  }
#'  
#'  @export fviz_pca_predict
#'  
     
fviz_pca_predict <- function(res.pca, quanti.sup = NULL, 
                         axes=c(1,2), label="all",  invisible ="none",
                         labelsize=4, col.var="black", alpha.var=1, 
                         col.quanti.sup="blue", col.circle ="grey70", ...)
{

  
  eig.df <- get_eigenvalue(res.pca)
  pca.var <- get_pca_var(res.pca)
  
  lab.var <- lab.quanti <- FALSE
  if(label[1]=="all" | "var" %in% label) lab.var =TRUE
  if(label[1]=="all" | "quanti.sup" %in% label) lab.quanti =TRUE
  hide.var <- hide.quanti <- FALSE
  if("var" %in% invisible) hide.var =TRUE
  if("quanti.sup" %in% invisible) hide.quanti =TRUE
  
  p <- fviz_pca_var(X = res.pca, axes = axes, label = label,  invisible = invisible,
       labelsize = labelsize, col.var = col.var, alpha.var=alpha.var, 
       col.quanti.sup = col.quanti.sup, col.circle = col.circle) 
  
  result = list()
  # Add quantitative supplementary variables
  if(!is.null(quanti.sup) & !hide.quanti){
    quanti.sup <- .predict_pca_quanti(res.pca, quanti.sup, ...)
    result$quanti.sup <- quanti.sup
    
    if(!is.null(quanti.sup$coord)){
      quanti_coord <- quanti.sup$coord
        quanti_coord <- as.data.frame(quanti_coord[, axes, drop=FALSE])
        colnames(quanti_coord) <- c("x", "y")
        p <- p + geom_segment(data = quanti_coord,
              aes(x = 0, y = 0, xend = x, yend = y),
              arrow = arrow(length = unit(0.2, 'cm')), color=col.quanti.sup, linetype=2)
        if(lab.quanti)
          p <- p + geom_text(data = quanti_coord, aes(x,y),
                   label = rownames(quanti_coord), color=col.quanti.sup, 
                   size = labelsize, hjust=0.8, vjust=0) 
      
      result$plot <- p
     }  
   }
  
  result
}


   
# Supplementary quantitative variables
# Calculate the correlations between supplementary variables
# and the principal components
.predict_pca_quanti <- function(res.pca, quanti.sup, ...)
{
  if(!inherits(quanti.sup, c("matrix", "data.frame")))
    stop("quanti.sup should be an object of class matrix or data.frame")
  
  # quanti coord
  quanti.sup <- as.data.frame(quanti.sup)
  ind.coord <- get_pca_ind(res.pca, ...)$coord
  quanti.coord <- cor(quanti.sup, ind.coord)
  quanti = list(coord = quanti.coord, cor = quanti.coord, cos2 = quanti.coord^2 )
  quanti
}



