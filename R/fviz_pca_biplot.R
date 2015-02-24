#' @include fviz_pca_ind.R
 NULL

#' Biplot of Principal Component Analysis Individuals and Variables using ggplot2
#' 
#' @param X an object of class PCA (from FactoMineR package).
#' @param axes a numeric vector of length 2 specifying the component to be plotted.
#' @param label a character vector specifying the elements to be labelled.
#'  Default value is "all".
#'  Allowed values are "none" or the combination of c("ind", "ind.sup", "quali", "var", "quanti.sup").
#'  "ind" can be used to label only active individuals.
#'  "ind.sup" if for supplementary individuals.
#'  "quali" is for supplementary qualitative variables. "var" is for active variables.
#'  "quanti.sup" is for quantitative supplementary variables.
#' @param invisible a character value specifying the elements to be hidden on the plot.
#'  Default value is "none".
#'  Allowed values are the combination of c("ind", "ind.sup", "quali", "var", "quanti.sup").
#' @param labelsize font size for the labels
#' @param habillage specify the qualitative variable (by its index or name) to be used for
#'  coloring individuals by groups. Default value is "none".
#' @param addEllipses logical value.
#'  If TRUE, draw ellipses around the individuals when habillage != "none".
#' @param col.ind, col.var color for individuals and variables, respectively.
#'  Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the colors for individuals/variables are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2+y2, "coord"), x values("x") or y values("y").
#'  To use automatic coloring (by cos2, contrib, ....), make sure that habillage ="none".
#' @param col.ind.sup color for supplementary individuals
#' @param alpha.ind, alpha.var controls the transparency of
#'  individual and variable colors, respectively.
#' The value can variate from 0 (total transparency) to 1 (no transparency).
#' Default value is 1. Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the transparency for individuals/variables colors are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2+y2, "coord"), x values("x") or y values("y").
#'  To use this, make sure that habillage ="none".
#' @param col.quanti.sup a color for the quantitative supplementary variables.
#' @param col.circle a color for the correlation circle.
#' @param autolab logical value. If TRUE, the best positions for labels are calculated to avoid overlapping.
#'  This can be time-consuming if there are many variables.
#'  
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#'  library("FactoMineR")
#'  data(decathlon)
#'  res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13, graph = FALSE)
#'  fviz_pca_biplot(res.pca, autolab=TRUE)
#'  
#'  # Use habillage
#'  p <- fviz_pca_biplot(res.pca, autolab=TRUE, habillage=13, label=c("var", "quanti.sup"))
#'  p
#'  
#'  # Change the theme
#'  p + theme_minimal()
#'  
#'  p + theme_classic()
#'  }
#'  
fviz_pca_biplot <- function(X,  axes = c(1,2), label = "all", invisible="none", labelsize=4,
                  habillage="none", addEllipses=FALSE, 
                  col.ind = "black", col.ind.sup = "blue", alpha.ind =1,
                  col.var="steelblue", alpha.var=1, col.quanti.sup="blue",
                  col.circle ="grey70", autolab=FALSE)
{
  
  library("ggplot2")
  library("grid")
  library("FactoMineR")
  
  ind <- data.frame(X$ind$coord[, axes, drop=FALSE])
  colnames(ind)<- c("x", "y")
  
  eig <- X$eig[,2]
  var <- data.frame(X$var$coord[, axes, drop=FALSE])
  colnames(var)<- c("x", "y")
  
  title <- "Biplot"
  xlab = paste0("Dim.", axes[1], " (", round(eig[axes[1]],1), "%)") 
  ylab = paste0("Dim.", axes[2], " (", round(eig[axes[2]], 1),"%)")
  scale.unit <- X$call$scale.unit
  
  cos2 <- apply(X$var$cos2[, axes], 1, sum)
  coord <- apply(X$var$coord[, axes]^2, 1, sum)
  contrib <- X$var$contrib[, axes]
  eig <- X$eig[,axes]
  contrib <- contrib[,1]*eig[1,1] +  contrib[,2]*eig[2,1] 
  
  # rescale variable coordinates
  r <- min(
    (max(ind[,"x"])-min(ind[,"x"])/(max(var[,"x"])-min(var[,"x"]))),
    (max(ind[,"y"])-min(ind[,"y"])/(max(var[,"y"])-min(var[,"y"])))
  )
  var <- var*r*0.7
  
  
  # Find the best position for labels to avoid overlap
  textpos <-var[, c("x", "y")]
  if(autolab){
    autopos <- autoLab(var$x, var$y, labels =rownames(var))
    textpos$x <- autopos$x
    textpos$y <- autopos$y
  }
  
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
  p <- fviz_pca_ind(X,  axes = axes, label = label, invisible=invisible,
                    labelsize=labelsize, 
                    col.ind = col.ind, col.ind.sup = col.ind.sup, alpha.ind=alpha.ind,
                    habillage=habillage, addEllipses=addEllipses, autolab=autolab)
  
  
  if(!hide.var){
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
    else if(col.var %in% c("cos2","contrib", "coord", "x", "y")){
      p <- p + geom_segment(data = var,
                            aes_string(x = 0, y = 0, xend = 'x', yend = 'y', color=col.var),
                            arrow = arrow(length = unit(0.2, 'cm')), alpha = alpha.var)
      if(lab.var) p <- p + geom_text(data = var, aes_string('textpos_x','textpos_y', color=col.var),
                                     label = var$name,  size = labelsize, alpha=alpha.var)
    }
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
  
  # Add supplementary quantitative variables
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
  p+labs(title=title)+coord_equal()
}

