#' @include get_pca_var.R get_eigenvalue.R
NULL
#' Graph of variables - Principal Component Analysis
#' 
#' @description
#' This function can be used to visualize variable factor maps from the output of several PCA functions : 
#' PCA() from FactoMineR package; prcomp() and princomp() from stats package;
#'  dudi.pca() from ade4 package.
#' 
#' @param X an object of class PCA (FactoMineR); prcomp (stats); princomp (stats);
#'  dudi and pca (ade4).
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
#'  
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' data(iris)
#' # Principal component analysis
#' res.pca <- princomp(iris[, -5],  cor = TRUE)
#' 
#' # Default plot
#' fviz_pca_var(res.pca)
#' 
#' # Change color and theme
#' fviz_pca_var(res.pca, col.var="steelblue")+
#'  theme_minimal()
#'  
#' # Control variable colors using their contribution
#' fviz_pca_var(res.pca, col.var="contrib")
#' 
#' # Change the gradient color
#' fviz_pca_var(res.pca, col.var="contrib")+
#'  scale_color_gradient2(low="blue", mid="white", 
#'            high="red", midpoint=55)+theme_bw()
#'            
#' # Control the transparency of variables using their contribution
#' fviz_pca_var(res.pca, alpha.var="contrib")+
#'    theme_minimal()
#'  }
#'  @export 
fviz_pca_var <- function(X, axes=c(1,2), label="all",  invisible ="none",
                         labelsize=4, col.var="black", alpha.var=1, 
                         col.quanti.sup="blue", col.circle ="grey70")
{
  
  eig.df <- get_eigenvalue(X)
  pca.var <- get_pca_var(X)
  scale.unit <- .get_scale_unit(X)
  
  eig <- eig.df[,2]
  var <- data.frame(pca.var$coord[, axes, drop=FALSE])
  colnames(var)<- c("x", "y")
  
  title <- "Variables factor map - PCA"
  xlab = paste0("PC", axes[1], " (", round(eig[axes[1]],1), "%)") 
  ylab = paste0("PC", axes[2], " (", round(eig[axes[2]], 1),"%)")
  
  cos2 <- apply(pca.var$cos2[, axes], 1, sum)
  coord <- apply(pca.var$coord[, axes]^2, 1, sum) # same as cos2
  eig <- eig.df[,axes]
  contrib <- apply(pca.var$contrib[, axes], 1, sum)
  
  # Label positions
  textpos <-var[, c("x", "y")]
  
  var <- cbind.data.frame(name = rownames(var), 
                          textpos_x = textpos$x, textpos_y = textpos$y,
                          var, cos2 = cos2, contrib = contrib, coord=coord)
  
  
  lab.var <- lab.quanti <- FALSE
  if(label[1]=="all" | "var" %in% label) lab.var =TRUE
  if(label[1]=="all" | "quanti.sup" %in% label) lab.quanti =TRUE
  hide.var <- hide.quanti <- FALSE
  if("var" %in% invisible) hide.var =TRUE
  if("quanti.sup" %in% invisible) hide.quanti =TRUE
  
  # Draw correlation circle
  if(scale.unit){
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- data.frame(xcircle = cos(theta), ycircle = sin(theta))
    p <- ggplot(data = circle, aes(xcircle, ycircle)) + geom_path(color=col.circle)+
      geom_hline(yintercept = 0, linetype="dashed")+
      geom_vline(xintercept = 0, linetype="dashed")    
  }
  else p <- ggplot()
  
  if(!hide.var){
    
    # The color and the transparency of variables are automatically controlled by
    # their cos2, contrib, coord, "x" or "y" coordinates
    if(col.var %in% c("cos2","contrib", "coord", "x", "y") &
         alpha.var %in% c("cos2","contrib", "coord", "x", "y"))
    {
      p <- p + geom_segment(data = var,
                aes_string(x = 0, y = 0, xend = 'x', yend = 'y', 
                           color=col.var, alpha=alpha.var),
                arrow = arrow(length = unit(0.2, 'cm')))
      
      if(lab.var) p <- p + geom_text(data = var, 
                           aes_string('textpos_x','textpos_y', label = 'name', 
                              color=col.var, alpha=alpha.var), size = labelsize)
    }
    # Only the color is controlled automatically
    else if(col.var %in% c("cos2","contrib", "coord", "x", "y")){
      p <- p + geom_segment(data = var,
              aes_string(x = 0, y = 0, xend = 'x', yend = 'y', color=col.var),
              arrow = arrow(length = unit(0.2, 'cm')), alpha = alpha.var)
      
      if(lab.var) p <- p + geom_text(data = var, aes_string('textpos_x','textpos_y', color=col.var),
                                     label = var$name,  size = labelsize, alpha=alpha.var)
    }
    # Only the transparency is controlled automatically
    else if(alpha.var %in% c("cos2","contrib", "coord", "x", "y")){
      p <- p + geom_segment(data = var,
                aes_string(x = 0, y = 0, xend = 'x', yend = 'y', alpha=alpha.var),
                arrow = arrow(length = unit(0.2, 'cm')), color=col.var)
      
      if(lab.var)p <- p + geom_text(data = var, aes_string('textpos_x','textpos_y',
                          alpha=alpha.var, label ='name'),
                          size = labelsize, color=col.var)
    }
    
    else{
      p <- p + geom_segment(data = var,
                aes(x = 0, y = 0, xend = x, yend = y),
                arrow = arrow(length = unit(0.2, 'cm')), color=col.var)
      if(lab.var) p <- p + geom_text(data = var, aes(textpos_x,textpos_y),
                           label = var$name, color=col.var, 
                           size = labelsize, hjust=0.8, vjust=0) 
    }
  }
  
  # Add supplementary quantitative variables
  # Available only in FactoMineR
  if(inherits(X, 'PCA')){
    quanti_coord <- X$quanti.sup$coord
    if(!is.null(quanti_coord) & !hide.quanti){
      quanti_coord <- as.data.frame(quanti_coord[, axes, drop=FALSE])
      colnames(quanti_coord) <- c("x", "y")
      p <- p + geom_segment(data = quanti_coord,
                aes(x = 0, y = 0, xend = x, yend = y),
                arrow = arrow(length = unit(0.2, 'cm')), color=col.quanti.sup, linetype=2)
      
      if(lab.quanti)
        p <- p + geom_text(data = quanti_coord, aes(x,y),
                 label = rownames(quanti_coord), color=col.quanti.sup, 
                 size = labelsize, hjust=0.8, vjust=0) 
    }
  }
  
  p <- p+ labs(title = title, x = xlab, y = ylab)
  p 
}


# X : an object of class PCA, princomp, prcomp, dudi
# Return TRUE if the data are scaled to unit variance
.get_scale_unit <-function(X){
  scale_unit <- FALSE
  if(inherits(X, 'PCA')) scale_unit <- X$call$scale.unit
  else if(inherits(X, "prcomp" )) {
    scale_unit <- X$scale
    if(is.numeric(scale_unit)) scale_unit = TRUE
  }
  else if(inherits(X, "princomp")){
    scale_unit <- X$scale
    if(length(unique(scale_unit))>1) scale_unit <- TRUE
    else scale_unit = FALSE
  }
  else if(inherits(X, 'pca') & inherits(X, 'dudi')){
    scale_unit <- X$norm
    if(length(unique(scale_unit))>1) scale_unit <- TRUE
    else scale_unit = FALSE
  }
  else stop("Error in .get_scale_unit function : can't handle an object of class ",
            class(X))
 
  scale_unit
}

