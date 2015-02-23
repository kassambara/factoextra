#' Visualizing Principal Component Analysis Individuals using ggplot2
#' 
#' @param X an object of class PCA (from FactoMineR package).
#' @param axes a numeric vector of length 2 specifying the component to be plotted.
#' @param label a character vector specifying the elements to be labelled.
#'  Default value is "all".
#'  Allowed values are "none" or the combination of c("ind", "ind.sup", "quali").
#'  "ind" can be used to label only active individuals.
#'  "ind.sup" if for supplementary individuals.
#'  "quali" is for supplementary qualitative variables.
#' @param invisible a character value specifying the elements to be hidden on the plot.
#'  Default value is "none". Allowed values are the combination of c("ind", "ind.sup", "quali").
#' @param labelsize font size for the labels
#' @param habillage specify the qualitative variable (by its index or name) to be used for
#'  coloring individuals by groups. Default value is "none".
#' @param addEllipses logical value.
#'  If TRUE, draw ellipses around the individuals when habillage != "none".
#' @param col.ind color for individuals. The default value is "black".
#'  Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the colors for individuals are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2+y2, "coord"), x values("x") or y values("y").
#'  To use automatic coloring (by cos2, contrib, ....), make sure that habillage ="none".
#' @param col.ind.sup color for supplementary individuals
#' @param alpha.ind controls the transparency of colors.
#' The value can variate from 0 (total transparency) to 1 (no transparency).
#' Default value is 1. Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the transparency for individuals colors are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2+y2, "coord"), x values("x") or y values("y").
#'  To use this, make sure that habillage ="none".
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
#'  fviz_pca_ind(res.pca, autolab=TRUE)
#'  
#'  # Use habillage
#'  fviz_pca_ind(res.pca, autolab=TRUE, habillage=13)
#'  
#'  # Change the color by contrib
#'  p <- fviz_pca_ind(res.pca, col.ind ="contrib")
#'  p
#'  # Change the color
#'  p + scale_color_gradient2(low ="blue", mid="white", high="red", midpoint=40)
#'  
#'  # Change the theme
#'  p + theme_minimal()
#'  
#'  p + theme_classic()
#'  }
#'  

fviz_pca_ind <- function(X,  axes = c(1,2), 
                 label = "all", invisible="none", labelsize=4, 
                 habillage="none", addEllipses=FALSE, 
                 col.ind = "black", col.ind.sup = "blue", alpha.ind =1, autolab=FALSE)
{
  library("ggplot2")
  library("grid")
  library("FactoMineR")
  
  eig <- X$eig[,2]
  ind <- data.frame(X$ind$coord[, axes, drop=FALSE])
  colnames(ind)<- c("x", "y")
  
  title <- "Individuals graph"
  xlab = paste0("Dim.", axes[1], " (", round(eig[axes[1]],1), "%)") 
  ylab = paste0("Dim.", axes[2], " (", round(eig[axes[2]], 1),"%)")
  
  cos2 <- apply(X$ind$cos2[, axes], 1, sum, na.rm=TRUE)
  coord <- apply(X$ind$coord[, axes]^2, 1, sum, na.rm=TRUE)
  contrib <- X$ind$contrib[, axes]
  eig <- X$eig[,axes]
  contrib <- contrib[,1]*eig[1,1] +  contrib[,2]*eig[2,1] 
  
  # Find the best positions for labels to avoid overlap
  textpos <-ind[, c("x", "y")]
  if(autolab){
    autopos <- autoLab(ind$x, ind$y, labels =rownames(ind), doPlot=FALSE)
    textpos$x <- autopos$x
    textpos$y <- autopos$y
  }
  
  # data frame to be used for plotting
  ind <- cbind.data.frame(name = rownames(ind), 
                          textpos_x = textpos$x, textpos_y = textpos$y,
                          ind, cos2 = cos2, contrib = contrib, coord=coord)
  
  lab.ind <- lab.ind.sup <- lab.quali <- FALSE
  if(label[1]=="all" | "ind" %in% label) lab.ind =TRUE
  if(label[1]=="all" | "ind.sup" %in% label) lab.ind.sup =TRUE
  if(label[1]=="all" | "quali" %in% label) lab.quali =TRUE
  
  hide.ind <- hide.ind.sup <- hide.quali <- FALSE
  if("ind" %in% invisible) hide.ind =TRUE
  if("ind.sup" %in% invisible) hide.ind.sup =TRUE
  if("quali" %in% invisible) hide.quali =TRUE
  
  # No qualitative variable to color individuals
  if(habillage=="none"){  
    
    # Change automatically the color and transparency
    if(col.ind %in% c("cos2","contrib", "coord", "x", "y") &
         alpha.ind %in% c("cos2","contrib", "coord", "x", "y")){
      p <- ggplot() + geom_point(data = ind, 
                                 aes_string('x','y', color=col.ind, alpha=alpha.ind), shape=19)
      if(lab.ind) p <- p + geom_text(data = ind, 
                                     aes_string('textpos_x','textpos_y', label = 'name', 
                                                color=col.ind, alpha=alpha.ind), size = labelsize)
    }
    else if(col.ind %in% c("cos2","contrib", "coord", "x", "y")){
      # Change the color automatically
      p <- ggplot() + geom_point(data = ind, aes_string('x','y', color=col.ind),
                                 shape=19, alpha=alpha.ind)
      if(lab.ind) p <- p + geom_text(data = ind, aes_string('textpos_x','textpos_y', color=col.ind),
                                     label = ind$name,  size = labelsize, alpha=alpha.ind)
    }
    else if(alpha.ind %in% c("cos2","contrib", "coord", "x", "y")){
      # Change the transparency automatically
      p <- ggplot() + geom_point(data = ind, aes_string('x','y', alpha=alpha.ind), shape=19, color=col.ind)
      if(lab.ind) p <- p + geom_text(data = ind, aes_string('textpos_x','textpos_y', alpha=alpha.ind, label="name"),
                                     size = labelsize, color=col.ind)
    }
    else{
      p <- ggplot(data = ind, aes(x,y))+ geom_point(shape=19, color=col.ind)
      if(lab.ind) p <- p + geom_text(data = ind, aes(textpos_x,textpos_y), 
                                     color = col.ind, label = ind$name, size = labelsize)
    }
  }
  else{
    data <- X$call$X
    ind.sup <- X$call$ind.sup
    if (is.numeric(habillage)) name.quali <- colnames(data)[habillage]
    else name.quali <- habillage
    ind <- cbind.data.frame(data[rownames(ind),name.quali], ind)
    
    colnames(ind)[1]<-name.quali
    ind[, 1]<-as.factor(ind[,1])
    # Plot individuals
    p <- ggplot()
    if(!hide.ind) {
      p <- p+
        geom_point(data = ind, aes_string('x', 'y', color=name.quali, shape = name.quali))
      if(lab.ind) p <- p + geom_text(data = ind, 
                                     aes_string('textpos_x', 'textpos_y', label = 'name', color=name.quali,
                                                shape = name.quali),  size = labelsize)
    }
    
    if(!hide.quali){
      # Plot quali.sup 
      # extract only the levels of interest (when multiple quali.sup are used)
#       quali.levels <- levels(ind[, name.quali])
#       quali.levels <- c(quali.levels, paste0(name.quali, " ", quali.levels ))
#       quali.levels <- as.character(intersect(rownames(X$quali.sup$coord), quali.levels))
#       
#       coord_quali.sup <- X$quali.sup$coord[quali.levels, axes]
      coord_quali.sup <- X$quali.sup$coord[, axes]
      colnames( coord_quali.sup) <-c("x", "y")
      coord_quali.sup <- cbind.data.frame(n = rownames(coord_quali.sup), coord_quali.sup)
      colnames(coord_quali.sup)[1] <- name.quali
      coord_quali.sup[, 1] <- as.factor(coord_quali.sup[,1])
      p <- p + geom_point(data=coord_quali.sup, aes_string('x', 'y', color=name.quali, shape=name.quali),
                          size=4)
      if(lab.quali)
        p <- p + geom_text(data=coord_quali.sup, 
                           aes_string('x', 'y', color=name.quali),
                           label=rownames(coord_quali.sup),
                           size=5, vjust=-1)
      
      if(addEllipses){
        aa <- cbind.data.frame(data[rownames(X$ind$coord), name.quali], X$ind$coord)
        ell<-coord.ellipse(aa,bary=TRUE) 
        ell <- ell$res
        colnames(ell)<-c(name.quali, "x", "y")
        ell[, 1]<-as.factor(ell[,1])
        p <- p + geom_path(data = ell, aes_string('x', 'y', color = name.quali, group = name.quali))
      }
    }
  }
 
  # Add supplementary individuals
  indsup_coord <- X$ind.sup$coord
  if(!is.null(indsup_coord) & !hide.ind.sup){
    indsup_coord <- as.data.frame(indsup_coord[, axes, drop=FALSE])
    colnames(indsup_coord) <- c("x", "y")
    p <- p + geom_point(data=indsup_coord, aes(x, y),
                        shape=19, color=col.ind.sup)
    if(lab.ind.sup) p <- p + geom_text(data = indsup_coord, aes(x,y),
                                       label = rownames(indsup_coord), color=col.ind.sup, 
                                       size = labelsize, hjust=0.8, vjust=0) 
  }
  
  p <- p +
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_vline(xintercept = 0, color = "black", linetype="dashed") +
    labs(title = title, x = xlab, y = ylab)
  p
}