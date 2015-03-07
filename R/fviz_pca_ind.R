#' @include get_pca_ind.R get_eigenvalue.R
NULL
#' Graph of individuals - Principal Component Analysis
#' 
#' @description
#' This function can be used to visualize individual factor maps from the output of several PCA functions : 
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
#'  Allowed values are "none" or the combination of c("ind", "ind.sup", "quali").
#'  "ind" can be used to label only active individuals.
#'  "ind.sup" is for supplementary individuals.
#'  "quali" is for supplementary qualitative variables.
#' @param invisible a character value specifying the elements to be hidden on the plot.
#'  Default value is "none". Allowed values are the combination of c("ind", "ind.sup", "quali").
#' @param labelsize font size for the labels
#' @param habillage an optional factor variable for coloring
#'  the observations by groups. Default value is "none".
#'  If X is an PCA object from FactoMineR package, habillage can also specify
#'  the supplementary qualitative variable (by its index or name) to be used for
#'  coloring individuals by groups (see ?PCA in FactoMineR). 
#' @param addEllipses logical value.
#'  If TRUE, draws ellipses around the individuals when habillage != "none".
#'  @param ellipse.level the size of the concentration ellipse in normal probability
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
#' @param ... optional arguments to be passed to the function get_pca_ind().
#'  This can includes the argument data (the original data used for pca) 
#'  which is required when X is not from FactoMineR or adea4 packages.
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
#' fviz_pca_ind(res.pca, data = iris[, -5])
#' 
#' # Remove the points from the graph
#' fviz_pca_ind(res.pca, data = iris[, -5], geom="text")
#'  
#'  # Color individuals by groups
#'  fviz_pca_ind(res.pca, data = iris[, -5], label="none",
#'  habillage=iris$Species, addEllipses=TRUE, ellipse.level=0.95)
#'  }
#'  
#' @export 
fviz_pca_ind <- function(X,  axes = c(1,2), geom=c("point", "text"),
                 label = "all", invisible="none", labelsize=4, 
                 habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                 col.ind = "black", col.ind.sup = "blue", alpha.ind =1, ...)
{

  if(length(intersect(geom, c("point", "text"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  
  eig.df <- get_eigenvalue(X)
  pca.ind <- get_pca_ind(X, ...)
  
  eig <- eig.df[,2]
  ind <- data.frame(pca.ind$coord[, axes, drop=FALSE])
  colnames(ind)<- c("x", "y")
  
  title <- "Individuals factor map - PCA"
  xlab = paste0("PC", axes[1], " (", round(eig[axes[1]],1), "%)") 
  ylab = paste0("PC", axes[2], " (", round(eig[axes[2]], 1),"%)")
  
  cos2 <- apply(pca.ind$cos2[, axes], 1, sum, na.rm=TRUE)
  coord <- apply(pca.ind$coord[, axes]^2, 1, sum, na.rm=TRUE)
  contrib <- pca.ind$contrib[, axes]
  eig <- eig.df[,axes]
  contrib <- apply(pca.ind$contrib[, axes], 1, sum, na.rm=TRUE)
  
  # Find the best positions for labels to avoid overlap
  textpos <-ind[, c("x", "y")]
  
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
  if(habillage[1]=="none"){  
    
    p <- ggplot() 
    
    if(hide.ind) p <-ggplot()+geom_blank(data=ind, aes(x,y))
    # The color and the transparency of individuals are automatically controlled by
    # their cos2, contrib, coord, "x" or "y" coordinates
    else if(col.ind %in% c("cos2","contrib", "coord", "x", "y") &
         alpha.ind %in% c("cos2","contrib", "coord", "x", "y"))
      {
        if("point" %in% geom) 
          p <- p + geom_point(data = ind, 
                   aes_string('x','y', color=col.ind, alpha=alpha.ind), shape=19)
        
        if(lab.ind & "text"%in% geom) 
          p <- p + geom_text(data = ind, 
          aes_string('textpos_x','textpos_y', label = 'name', 
          color=col.ind, alpha=alpha.ind), size = labelsize)
    }
    # Only the color is controlled automatically
    else if(col.ind %in% c("cos2","contrib", "coord", "x", "y")){
      
      if("point" %in% geom) 
        p <- p + geom_point(data = ind, aes_string('x','y', color=col.ind),
                  shape=19, alpha=alpha.ind)
      
      if(lab.ind & "text" %in% geom) 
        p <- p + geom_text(data = ind,
             aes_string('textpos_x','textpos_y', color=col.ind),
             label = ind$name,  size = labelsize, alpha=alpha.ind, vjust = -0.7)
    }   
    # Only the transparency is controlled automatically
    else if(alpha.ind %in% c("cos2","contrib", "coord", "x", "y")){
      
      if("point" %in% geom) 
        p <- p + geom_point(data = ind, 
                  aes_string('x','y', alpha=alpha.ind), shape=19, color=col.ind)
      
      if(lab.ind & "text" %in% geom) 
        p <- p + geom_text(data = ind, 
             aes_string('textpos_x','textpos_y', alpha=alpha.ind, label="name"),
             size = labelsize, color=col.ind, vjust = -0.7)
    }
    
    else{
      p <- ggplot(data = ind, aes(x,y))
      
      if("point" %in% geom) 
        p <- p + geom_point(shape=19, color=col.ind)
      
      if(lab.ind & "text" %in% geom) 
        p <- p + geom_text(data = ind, aes(textpos_x,textpos_y), 
                 color = col.ind, label = ind$name, size = labelsize, vjust = -0.7)
    }
  }

# qualitative variable is used to color the individuals
  else{
    
    # Plot individuals
    p <- ggplot()
    if(hide.ind & hide.quali) p <-ggplot()+geom_blank(data=ind, aes(x,y))
    
    if(is.factor(habillage)){ 
      if(nrow(ind)!=length(habillage))
        stop("The number of active individuals used in the PCA is different ",
             "from the length of the factor habillage. Please, remove the supplementary ",
             "individuals in the variable habillage.")
      name.quali <- "Groups"
      ind <- cbind.data.frame(Groups = habillage, ind)
      ind[, 1]<-as.factor(ind[,1])
    }
    
    # X is from FactoMineR outputs
    else if(inherits(X, "PCA")){
      data <- X$call$X
      if (is.numeric(habillage)) name.quali <- colnames(data)[habillage]
      else name.quali <- habillage 
      ind <- cbind.data.frame(data[rownames(ind),name.quali], ind)
      colnames(ind)[1]<-name.quali
      ind[, 1]<-as.factor(ind[,1])
    }
    
    if(!hide.ind) {
      
      if("point" %in% geom) 
         p <- p+geom_point(data = ind, 
              aes_string('x', 'y', color=name.quali, shape = name.quali))
      
      if(lab.ind & "text" %in% geom) 
        p <- p + geom_text(data = ind, 
                 aes_string('textpos_x', 'textpos_y', label = 'name',
                 color=name.quali, shape = name.quali),  size = labelsize, vjust = -0.7)
    }
    if(!hide.quali){   
      coord_quali.sup <- .get_coord_quali(ind$x, ind$y, groups = ind[,1])
      coord_quali.sup <- cbind.data.frame(name = rownames(coord_quali.sup),
                                     coord_quali.sup)
      colnames(coord_quali.sup)[1] <- name.quali
      coord_quali.sup[, 1] <- as.factor(coord_quali.sup[,1])
      if("point" %in% geom) 
        p <- p + geom_point(data=coord_quali.sup,
              aes_string('x', 'y', color=name.quali, shape=name.quali), size=4)    
      if(lab.quali & "text" %in% geom)
        p <- p + geom_text(data=coord_quali.sup, 
                 aes_string('x', 'y', color=name.quali),
                 label=rownames(coord_quali.sup), size=labelsize, vjust=-1)
    }
    if(addEllipses){
      ell <- .get_ellipse_by_groups(ind$x, ind$y,
                        groups = ind[, name.quali], ellipse.level=ellipse.level)
      colnames(ell)<-c(name.quali, "x", "y")
      ell[, 1]<-as.factor(ell[,1])
      p <- p + geom_path(data = ell, aes_string('x', 'y', color = name.quali, group = name.quali))
    }
      
    
  }
 
# Add supplementary quantitative individuals
# Available only in FactoMineR
if(inherits(X, 'PCA')){
  indsup_coord <- X$ind.sup$coord
  
  if(!is.null(indsup_coord) & !hide.ind.sup){
    indsup_coord <- as.data.frame(indsup_coord[, axes, drop=FALSE])
    colnames(indsup_coord) <- c("x", "y")
    
    if("point" %in% geom)
     p <- p + geom_point(data=indsup_coord, aes(x, y),
                        shape=19, color=col.ind.sup)
    if(lab.ind.sup & "text" %in% geom) 
      p <- p + geom_text(data = indsup_coord, aes(x,y),
               label = rownames(indsup_coord), color=col.ind.sup, 
               size = labelsize, hjust=0.8, vjust=0) 
  }
}
  
  p <- p +
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_vline(xintercept = 0, color = "black", linetype="dashed") +
    labs(title = title, x = xlab, y = ylab)
  p
}


# Return the coordinates of groups levels
# x : coordinate of individuals on x axis
# y : coordinate of indiviuals on y axis
.get_coord_quali<-function(x, y, groups){
  data.frame(
    x= tapply(x, groups, mean),
              y = tapply(y, groups, mean)
  )
}


#+++++++++++++++++++++
# Helper functions
#+++++++++++++++++++++

#+++++++++++
# .get_ellipse() : Compute the concentration ellipse of the points
#+++++++++++
# x : the coordinates of points. It can be a numeric vector, matrix or data.frame
# y : optional y coordinates of points. y is not required when x
  # is a matrix or data.frame
# result is a data.frame containing the x and y coordinates of
  # the ellipse. Columns are x, y
.get_ellipse <- function(x, y=NULL, ellipse.level = 0.95) {
  if(class(x)%in% c("matrix", "data.frame")){
    y <- x[,2]
    x <- x[,1]
  }
  sigma <- var(cbind(x, y))
  mu <- c(mean(x), mean(y))
  t <- sqrt(qchisq(ellipse.level, df = 2))
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- cbind(cos(theta), sin(theta))
  data.frame(sweep(circle %*% chol(sigma) * t, 2, mu, FUN = '+'))
}

#+++++++++++
# .get_ellipse() : Compute the concentration ellipse of the points by groups
#+++++++++++
# x : the coordinates of points. It can be a numeric vector, matrix or data.frame
# y : optional y coordinates of points. y is not required when x is a matrix or data.frame
# groups  : a factor variable

# result is a data.frame containing the x and y coordinates of
# the ellipse by groups. Columns are : groups, x, y
.get_ellipse_by_groups <-function(x, y=NULL, groups, ellipse.level = 0.95){
  
  if(class(x)%in% c("matrix", "data.frame")){
    y <- x[,2]
    x <- x[,1]
  }
  groups <-as.factor(groups)
  levs <- levels(groups)
  len <- summary(groups) # number of cases per group
  d <- data.frame(x =x, y = y, groups=groups)
  result <- NULL
  for(i in 1:length(levs)){
    res <- .get_ellipse(d[which(groups==levs[i]),, drop=FALSE], ellipse.level=ellipse.level)
    res <- cbind.data.frame(group=rep(levs[i], nrow(res)), res)
    result <- rbind.data.frame(result,res)
  }
  result
}