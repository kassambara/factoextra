#' @include fviz_pca_ind.R get_pca_var.R
 NULL
#' Biplot of individuals and variables - Principal Component Analysis
#' 
#' @description
#' This function can be used to visualize a biplot of individuals and variables from the output of several PCA functions : 
#' PCA() from FactoMineR package; prcomp() and princomp() from stats package;
#'  dudi.pca() from ade4 package.
#'  
#' @param X an object of class PCA (FactoMineR); prcomp (stats); princomp (stats);
#'  dudi and pca (ade4).
#' @param axes a numeric vector of length 2 specifying the components to be plotted.
#' @param geom a character specifying the geometry to be used for the graph.
#'  Allowed values are "point" (to show only points),
#'  "text" to show only labels or c("point", "text") to show both types.
#' @param label a character vector specifying the elements to be labelled.
#'  Default value is "all".
#'  Allowed values are "none" or the combination of c("ind", "ind.sup", "quali", "var", "quanti.sup").
#'  "ind" can be used to label only active individuals.
#'  "ind.sup" is for supplementary individuals.
#'  "quali" is for supplementary qualitative variables. "var" is for active variables.
#'  "quanti.sup" is for quantitative supplementary variables.
#' @param invisible a character value specifying the elements to be hidden on the plot.
#'  Default value is "none".
#'  Allowed values are the combination of c("ind", "ind.sup", "quali", "var", "quanti.sup").
#' @param labelsize font size for the labels
#' @param habillage an optional factor variable for coloring
#'  the observations by groups. Default value is "none".
#'  If X is an PCA object from FactoMineR package, habillage can also specify
#'  the supplementary qualitative variable (by its index or name) to be used for
#'  coloring individuals by groups (see ?PCA in FactoMineR). 
#' @param addEllipses logical value.
#'  If TRUE, draws ellipses around the individuals when habillage != "none".
#' @param ellipse.level the size of the concentration ellipse in normal probability
#' @param col.ind,col.var color for individuals and variables, respectively.
#'  Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the colors for individuals/variables are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2+y2, "coord"), x values("x") or y values("y").
#'  To use automatic coloring (by cos2, contrib, ....), make sure that habillage ="none".
#' @param col.ind.sup color for supplementary individuals
#' @param alpha.ind,alpha.var controls the transparency of
#'  individual and variable colors, respectively.
#' The value can variate from 0 (total transparency) to 1 (no transparency).
#' Default value is 1. Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the transparency for individual/variable colors are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2+y2, "coord"), x values("x") or y values("y").
#'  To use this, make sure that habillage ="none".
#' @param col.quanti.sup a color for the quantitative supplementary variables.
#' @param col.circle a color for the correlation circle.
#' @param data the original data used for the PCA. 
#' This argument is required only when X is not from FactoMineR or ade4 packages.
#' It's used to calculate the cos2 of the individuals.
#' @param ... optional arguments to be passed to the function get_pca_ind().
#' This can include the argument data (the original data used for pca) 
#' which is required when X is not from FactoMineR or ade4 packages.
#'  
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' data(iris)
#'
#' # Principal component analysis
#' res.pca <- princomp(iris[, -5],  cor = TRUE)
#' # biplot
#' fviz_pca_biplot(res.pca, data = iris[, -5])
#' # Keep only the labels for variables
#' fviz_pca_biplot(res.pca, data = iris[, -5], label ="var")
#'# Control automatically the color of individuals using the cos2
#' # cos2 = the quality of the individuals on the factor map
#' fviz_pca_biplot(res.pca, data = iris[, -5], label ="var",
#'               col.ind="cos2") 
#' # change the color by groups, ellipses
#' fviz_pca_biplot(res.pca, data = iris[, -5], label="var",
#' habillage=iris$Species, addEllipses=TRUE, ellipse.level=0.95)
#' 
#'  }
#' @export
fviz_pca_biplot <- function(X,  axes = c(1,2), geom=c("point", "text"),
                  label = "all", invisible="none", labelsize=4,
                  habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                  col.ind = "black", col.ind.sup = "blue", alpha.ind =1,
                  col.var="steelblue", alpha.var=1, col.quanti.sup="blue",
                  col.circle ="grey70", data = NULL, ...)
{
 
  # The original data is required to compute the cos2
  # of individuals
  if(inherits(X, 'princomp')){     
    if(is.null(data)) { 
      data.name <- readline(
        paste0("The original data used during the PCA analysis are required. ",
               "What's the name of the object data ? : ")
      )
      data <- eval(parse(text=data.name))
      if(is.null(data)) stop("The argument data is NULL. ",
                             "The original data used for the pca analysis are required.")
    }
  }
  
  eig.df <- get_eigenvalue(X)
  pca.ind <- get_pca_ind(X, data = data)
  pca.var <- get_pca_var(X)
  scale.unit <- .get_scale_unit(X)
  
  ind <- data.frame(pca.ind$coord[, axes, drop=FALSE])
  colnames(ind)<- c("x", "y")
  
  eig <- eig.df[,2]
  var <- data.frame(pca.var$coord[, axes, drop=FALSE])
  colnames(var)<- c("x", "y")
  
  title <- "Biplot"
  xlab = paste0("PC", axes[1], " (", round(eig[axes[1]],1), "%)") 
  ylab = paste0("PC", axes[2], " (", round(eig[axes[2]], 1),"%)")
  
  cos2 <- apply(pca.var$cos2[, axes], 1, sum)
  coord <- apply(pca.var$coord[, axes]^2, 1, sum)
  contrib <- pca.var$contrib[, axes]
  eig <- eig.df[,axes]
  contrib <- contrib[,1]*eig[1,1] +  contrib[,2]*eig[2,1] 
  
  # rescale variable coordinates
  r <- min(
    (max(ind[,"x"])-min(ind[,"x"])/(max(var[,"x"])-min(var[,"x"]))),
    (max(ind[,"y"])-min(ind[,"y"])/(max(var[,"y"])-min(var[,"y"])))
  )
  var <- var*r*0.7
  
  
  # Find the best position for labels to avoid overlap
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
  
  # Individuals
  p <- fviz_pca_ind(X,  axes = axes, geom = geom, label = label, invisible=invisible,
          labelsize=labelsize, 
          col.ind = col.ind, col.ind.sup = col.ind.sup, alpha.ind=alpha.ind,
          habillage=habillage, addEllipses=addEllipses, ellipse.level=ellipse.level,
          data = data)
  
  
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
      if(lab.var)p <- p + geom_text(data = var, aes_string('textpos_x','textpos_y', alpha=alpha.var, label ='name'),
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
  
  # # Add supplementary quantitative variables
  # Available only in FactoMineR
  if(inherits(X, 'PCA')){
    quanti_coord <- X$quanti.sup$coord
    if(!is.null(quanti_coord) & !hide.quanti){
      quanti_coord <- as.data.frame(quanti_coord[, axes, drop=FALSE])*r*0.7
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
  
  p+labs(title=title)
}

