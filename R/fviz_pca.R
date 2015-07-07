#' @include get_pca.R
 NULL
#' Visualize Principal Component Analysis
#' 
#' @description
#' Graph of individuals/variables from the output of Principal Component Analysis (PCA). \cr\cr
#' \itemize{
#' \item{fviz_pca_ind(): Graph of individuals}
#' \item{fviz_pca_var(): Graph of variables}
#' \item{fviz_pca_biplot(): Biplot of individuals and variables}
#' \item{fviz_pca(): An alias of fviz_pca_biplot()}
#' }
#'  
#' @param X an object of class PCA [FactoMineR]; prcomp and princomp [stats];
#'  dudi and pca [ade4].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param geom a text specifying the geometry to be used for the graph. 
#' Allowed values are the combination of c("point", "arrow", "text"). 
#' Use "point" (to show only points); 
#' "text" to show only labels; c("point", "text") or c("arrow", "text") to show both types.
#' @param label a text specifying the elements to be labelled.
#'  Default value is "all".
#'  Allowed values are "none" or the combination of c("ind", "ind.sup", "quali", "var", "quanti.sup").
#'  "ind" can be used to label only active individuals.
#'  "ind.sup" is for supplementary individuals.
#'  "quali" is for supplementary qualitative variables. "var" is for active variables.
#'  "quanti.sup" is for quantitative supplementary variables.
#' @param invisible a text specifying the elements to be hidden on the plot.
#'  Default value is "none".
#'  Allowed values are the combination of c("ind", "ind.sup", "quali", "var", "quanti.sup").
#' @param labelsize font size for the labels
#' @param pointsize the size of points
#' @param habillage an optional factor variable for coloring
#'  the observations by groups. Default value is "none".
#'  If X is a PCA object from FactoMineR package, habillage can also specify
#'  the supplementary qualitative variable (by its index or name) to be used for
#'  coloring individuals by groups (see ?PCA in FactoMineR). 
#' @param addEllipses logical value.
#'  If TRUE, draws ellipses around the individuals when habillage != "none".
#' @param ellipse.level the size of the concentration ellipse in normal probability
#' @param col.ind,col.var color for individuals and variables, respectively.
#'  Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the colors for individuals/variables are automatically controlled by their 
#'  qualities of representation ("cos2"),
#'  contributions ("contrib"), coordinates (x^2+y^2, "coord"), x values ("x") or y values ("y").
#'  To use automatic coloring (by cos2, contrib, ....), make sure that habillage ="none".
#' @param col.ind.sup color for supplementary individuals
#' @param alpha.ind,alpha.var controls the transparency of
#'  individual and variable colors, respectively.
#' The value can variate from 0 (total transparency) to 1 (no transparency).
#' Default value is 1. Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the transparency for the individual/variable colors are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2+y^2, "coord"), x values("x") or y values("y").
#'  To use this, make sure that habillage ="none".
#' @param col.quanti.sup a color for the quantitative supplementary variables.
#' @param col.circle a color for the correlation circle.
#' @param select.ind,select.var a selection of individuals/variables to be drawn. 
#' Allowed values are NULL or a list containing the arguments name, cos2 or contrib: 
#' \itemize{
#' \item name: is a character vector containing individuals/variables to be drawn
#' \item cos2: if cos2 is in [0, 1], ex: 0.6, then individuals/variables with a cos2 > 0.6 are drawn. 
#' if cos2 > 1, ex: 5, then the top 5 individuals/variables with the highest cos2 are drawn.
#' \item contrib: if contrib > 1, ex: 5,  then the top 5 individuals/variables with the highest cos2 are drawn
#' }
#' @param jitter a parameter used to jitter the points in order to reduce overplotting. 
#' It's a list containing the objects what, width and height (i.e jitter = list(what, width, height)). 
#' \itemize{
#' \item what: the element to be jittered. Possible values are "point" or "p"; "label" or "l"; "both" or "b".
#' \item width: degree of jitter in x direction
#' \item height: degree of jitter in y direction
#' }
#' @param ... Arguments to be passed to the function fviz_pca_biplot().
#'  
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' # Principal component analysis
#' # ++++++++++++++++++++++++++++++
#' data(iris)
#' res.pca <- prcomp(iris[, -5],  scale = TRUE)
#' 
#' # Graph of individuals
#' # +++++++++++++++++++++
#' # Default plot
#' fviz_pca_ind(res.pca)
#' # Change title and axis labels
#' fviz_pca_ind(res.pca) +
#'  labs(title = "PCA", x = "PC1", y ="PC2" )
#' # Change axis limits by specifying the min and max
#' fviz_pca_ind(res.pca) + 
#'    xlim(-4, 4) + ylim (-4, 4)
#' # Use text only
#' fviz_pca_ind(res.pca, geom="text")
#' # Use points only
#' fviz_pca_ind(res.pca, geom="point")
#' # Change the size of points
#' fviz_pca_ind(res.pca, geom="point", pointsize = 4)
#' # Change point color and theme
#' fviz_pca_ind(res.pca, col.ind = "blue")+
#'    theme_minimal()
#'    
#' # Control automatically the color of individuals 
#' # using the cos2 or the contributions
#' # cos2 = the quality of the individuals on the factor map
#' fviz_pca_ind(res.pca, col.ind="cos2") 
#' # Gradient color
#' fviz_pca_ind(res.pca, col.ind="cos2") + 
#'       scale_color_gradient2(low="white", mid="blue", 
#'       high="red", midpoint=0.6)
#' # Change the theme and use only points
#' fviz_pca_ind(res.pca, col.ind="cos2", geom = "point") + 
#'       scale_color_gradient2(low="blue", mid="white", 
#'       high="red", midpoint=0.6)+ theme_minimal()
#'       
#' # Color by the contributions   
#' fviz_pca_ind(res.pca, col.ind="contrib") + 
#'       scale_color_gradient2(low="blue", mid="white", 
#'       high="red", midpoint=4)
#'       
#' # Control the transparency of the color by the
#' # contributions
#' fviz_pca_ind(res.pca, alpha.ind="contrib") +
#'      theme_minimal()        
#'              
#' # Color individuals by groups
#' fviz_pca_ind(res.pca, label="none", habillage=iris$Species)
#' # Add ellipses
#' p <- fviz_pca_ind(res.pca, label="none", habillage=iris$Species, 
#'              addEllipses=TRUE, ellipse.level=0.95)
#' print(p)
#'              
#' # Change group color using RColorBrewer color palettes
#' p + scale_color_brewer(palette="Dark2") +
#'      theme_minimal()
#' p + scale_color_brewer(palette="Paired") +
#'      theme_minimal()
#' p + scale_color_brewer(palette="Set1") +
#'      theme_minimal()
#' # Change color manually
#' p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))           
#' # Select and visualize individuals with cos2 >= 0.96
#' fviz_pca_ind(res.pca, select.ind = list(cos2 = 0.96))
#' # Select the top 20 according to the cos2
#' fviz_pca_ind(res.pca, select.ind = list(cos2 = 20))
#' # Select the top 20 contributing individuals
#' fviz_pca_ind(res.pca, select.ind = list(contrib = 20))
#' # Select by names
#' fviz_pca_ind(res.pca, 
#' select.ind = list(name = c("23", "42", "119")))
#' 
#'  
#' # Graph of variables
#' # ++++++++++++++++++++++++++++
#' # Default plot
#' fviz_pca_var(res.pca)
#' # Use points and text
#' fviz_pca_var(res.pca, geom = c("point", "text"))
#' # Change color and theme
#' fviz_pca_var(res.pca, col.var="steelblue")+
#'  theme_minimal()
#'  
#' # Control variable colors using their contributions
#' fviz_pca_var(res.pca, col.var="contrib")+
#'  scale_color_gradient2(low="white", mid="blue", 
#'            high="red", midpoint=96) +
#'  theme_minimal()          
#' # Control the transparency of variables using their contributions
#' fviz_pca_var(res.pca, alpha.var="contrib") +
#'    theme_minimal()
#'    
#' # Select and visualize variables with cos2 >= 0.96
#' fviz_pca_var(res.pca, select.var = list(cos2 = 0.96))
#' # Select the top 3 contributing variables
#' fviz_pca_var(res.pca, select.var = list(contrib = 3))
#' # Select by names
#' fviz_pca_var(res.pca, 
#'    select.var= list(name = c("Sepal.Width", "Petal.Length")))
#'     
#' # biplot
#' # ++++++++++++++++++++++++++
#' fviz_pca_biplot(res.pca)
#' # Keep only the labels for variables
#' fviz_pca_biplot(res.pca, label ="var")
#' # Keep only labels for individuals
#' fviz_pca_biplot(res.pca, label ="ind")
#' # Hide variables
#' fviz_pca_biplot(res.pca, invisible ="var")
#' # Hide individuals
#' fviz_pca_biplot(res.pca, invisible ="ind")
#'# Control automatically the color of individuals using the cos2
#' fviz_pca_biplot(res.pca, label ="var", col.ind="cos2") +
#'        theme_minimal()
#' # Change the color by groups, add ellipses
#' fviz_pca_biplot(res.pca, label="var", habillage=iris$Species,
#'                addEllipses=TRUE, ellipse.level=0.95) 
#'                
#' # Select the top 30 contributing individuals
#' fviz_pca_biplot(res.pca, label="var", 
#'                select.ind = list(contrib = 30)) 
#' 
#'  }
#'  
#' @rdname fviz_pca
#' @export
fviz_pca <- function(X, ...){
  fviz_pca_biplot(X, ...)
}


#' @rdname fviz_pca 
#' @export 
fviz_pca_ind <- function(X,  axes = c(1,2), geom=c("point", "text"),
                         label = "all", invisible="none", labelsize=4, pointsize = 2,
                         habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                         col.ind = "black", col.ind.sup = "blue", alpha.ind =1,
                         select.ind = list(name = NULL, cos2 = NULL, contrib = NULL), 
                         jitter = list(what = "label", width = NULL, height = NULL),...)
{
  
  if(length(intersect(geom, c("point", "text", "arrow"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  if(length(axes) > 2) stop("axes should be of length 2")
  
  # data frame to be used for plotting
  ind <- facto_summarize(X, element = "ind", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(ind)[2:3] <-  c("x", "y")
  
  # Selection
  ind.all <- ind
  if(!is.null(select.ind)) ind <- .select(ind, select.ind)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.ind %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(ind.all[, alpha.ind])
  
  # No qualitative variable to color individuals
  if(habillage[1]=="none"){ 
    p <- ggplot() 
    if(hide$ind) p <-ggplot()+geom_blank(data=ind, aes_string('x','y'))
    else p <- .ggscatter(data = ind, x = 'x', y = 'y', 
                         col=col.ind,  alpha = alpha.ind, 
                         alpha.limits = alpha.limits, shape = 19, 
                         geom = geom, lab = lab$ind, labelsize = labelsize,
                         pointsize = pointsize, jitter = jitter)
  }
  
  # qualitative variable is used to color the individuals
  else{
    # Plot individuals
    p <- ggplot()
    if(hide$ind & hide$quali) p <-ggplot()+geom_blank(data=ind, aes_string('x','y'))
    
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
    
    if(!hide$ind) {   
      
      
      label_coord <- ind
      # jittering
      if(jitter$what %in% c("both", "b")){
        label_coord <- ind <- .jitter(ind, jitter)
      }
      else if(jitter$what %in% c("point", "p")){
        ind <- .jitter(ind, jitter)
      }
      else if(jitter$what %in% c("label", "l")){
        label_coord <- .jitter(label_coord, jitter)
      }
      
      if("point" %in% geom) 
        p <- p+geom_point(data = ind, 
                          aes_string('x', 'y', color=name.quali, shape = name.quali),
                          size = pointsize)
      if(lab$ind & "text" %in% geom) 
        p <- p + geom_text(data = label_coord, 
                           aes_string('x', 'y', label = 'name',
                                      color=name.quali, shape = name.quali),  size = labelsize, vjust = -0.7)
    }
    
    if(!hide$quali){   
      coord_quali.sup <- .get_coord_quali(ind$x, ind$y, groups = ind[,1])
      coord_quali.sup <- cbind.data.frame(name = rownames(coord_quali.sup),
                                          coord_quali.sup)
      colnames(coord_quali.sup)[1] <- name.quali
      coord_quali.sup[, 1] <- as.factor(coord_quali.sup[,1])
      
      if("point" %in% geom) 
        p <- p + geom_point(data=coord_quali.sup,
                            aes_string('x', 'y', color=name.quali, shape=name.quali),
                            size=pointsize*2)    
      if(lab$quali & "text" %in% geom)
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
  if(inherits(X, 'PCA') & !hide$ind.sup){
    ind_sup <- .get_supp(X, element = "ind.sup", axes = axes,
                         select = select.ind)
    if(!is.null(ind_sup)) colnames(ind_sup)[2:3] <-  c("x", "y")
    if(!is.null(ind_sup)){
      p <- fviz_add(p, df = ind_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.ind.sup, shape = 19, pointsize = pointsize,
                    labelsize = labelsize, addlabel = (lab$ind.sup & "text" %in% geom) , jitter = jitter)
    }  
  }
  
  p <- .fviz_finish(p, X, axes) +
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_vline(xintercept = 0, color = "black", linetype="dashed") +
    labs(title = "Individuals factor map - PCA")
  
  
  p
}


#' @rdname fviz_pca
#' @export 
fviz_pca_var <- function(X, axes=c(1,2), geom=c("arrow", "text"), 
                         label="all",  invisible ="none",
                         labelsize=4, col.var="black", alpha.var=1, 
                         col.quanti.sup="blue", col.circle ="grey70",
                         select.var = list(name = NULL, cos2 = NULL, contrib = NULL),
                         jitter = list(what = "label", width = NULL, height = NULL))
{
  
  scale.unit <- .get_scale_unit(X)
  
  # data frame to be used for plotting
  var <- facto_summarize(X, element = "var", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(var)[2:3] <-  c("x", "y")
  
  # Selection
  var.all <- var
  if(!is.null(select.var)) var <- .select(var, select.var)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.var %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(var.all[, alpha.var])
  
  # Draw correlation circle
  if(scale.unit){
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- data.frame(xcircle = cos(theta), ycircle = sin(theta))
    p <- ggplot(data = circle, aes_string("xcircle", "ycircle")) +
      geom_path(color=col.circle)+
      geom_hline(yintercept = 0, linetype="dashed")+
      geom_vline(xintercept = 0, linetype="dashed")    
  }
  else p <- ggplot()
  
  if(!hide$var){
    p <-.ggscatter(p = p, data = var, x = 'x', y = 'y', 
                   col=col.var,  alpha = alpha.var, 
                   alpha.limits = alpha.limits, 
                   geom =  geom,
                   lab = lab$var, labelsize = labelsize, jitter = jitter)
  }
  
  # Add supplementary quantitative variables
  # Available only in FactoMineR
  if(inherits(X, 'PCA') & !hide$quanti ){
    
    quanti_sup <- .get_supp(X, element = "quanti", axes = axes,
                            select = select.var)
    if(!is.null(quanti_sup)) colnames(quanti_sup)[2:3] <-  c("x", "y")
    if(!is.null(quanti_sup)){
      p <- fviz_add(p, df = quanti_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.quanti.sup, linetype = 2,
                    labelsize = labelsize, addlabel = (lab$quanti), jitter = jitter )
    }  
    
  }
  
  p <- .fviz_finish(p, X, axes) +
    labs(title = "Variables factor map - PCA")
  p 
}



#' @rdname fviz_pca
#' @export
fviz_pca_biplot <- function(X,  axes = c(1,2), geom=c("point", "text"),
                  label = "all", invisible="none", labelsize=4, pointsize = 2,
                  habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                  col.ind = "black", col.ind.sup = "blue", alpha.ind =1,
                  col.var="steelblue",  alpha.var=1, col.quanti.sup="blue",
                  col.circle ="grey70", 
                  select.var = list(name = NULL, cos2 = NULL, contrib = NULL),
                  select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                  jitter = list(what = "label", width = NULL, height = NULL), ...)
{
  
  scale.unit <- .get_scale_unit(X)
  
  # data frame to be used for plotting
  var <- facto_summarize(X, element = "var", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(var)[2:3] <-  c("x", "y")
  
  # Selection
  var.all <- var
  if(!is.null(select.var)) var <- .select(var, select.var)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.var %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(var.all[, alpha.var])
  
  pca.ind <- get_pca_ind(X)
  ind <- data.frame(pca.ind$coord[, axes, drop=FALSE])
  colnames(ind)<- c("x", "y")
  
  # rescale variable coordinates
  r <- min(
    (max(ind[,"x"])-min(ind[,"x"])/(max(var[,"x"])-min(var[,"x"]))),
    (max(ind[,"y"])-min(ind[,"y"])/(max(var[,"y"])-min(var[,"y"])))
  )
  var[, c("x", "y")] <- var[, c("x", "y")]*r*0.7
  
  # Individuals
  p <- fviz_pca_ind(X,  axes = axes, geom = geom, label = label, invisible=invisible,
          labelsize=labelsize, pointsize = pointsize,
          col.ind = col.ind, col.ind.sup = col.ind.sup, alpha.ind=alpha.ind,
          habillage=habillage, addEllipses=addEllipses, ellipse.level=ellipse.level,
          select.ind = select.ind, jitter = jitter)

  if(!hide$var){
    p <-.ggscatter(p = p, data = var, x = 'x', y = 'y', 
                   col=col.var,  alpha = alpha.var, 
                   alpha.limits = alpha.limits, 
                   geom =  c("arrow", "text"),
                   lab = lab$var, labelsize = labelsize, jitter = jitter)
  }
  
  # Add supplementary quantitative variables
  # Available only in FactoMineR
  if(inherits(X, 'PCA') & !hide$quanti ){
    quanti_sup <- .get_supp(X, element = "quanti", axes = axes,
                            select = select.var)
    if(!is.null(quanti_sup)) colnames(quanti_sup)[2:3] <-  c("x", "y")
    if(!is.null(quanti_sup)){
      p <- fviz_add(p, df = quanti_sup[, 2:3, drop = FALSE]*r*0.7, geom = c("arrow", "text"),
                    color = col.quanti.sup, linetype = 2,
                    labelsize = labelsize, addlabel = (lab$quanti), jitter = jitter )
    }  
  }
  p+labs(title="Biplot of variables and individuals")
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

# Return the coordinates of groups levels
# x : coordinate of individuals on x axis
# y : coordinate of indiviuals on y axis
.get_coord_quali<-function(x, y, groups){
  data.frame(
    x= tapply(x, groups, mean),
    y = tapply(y, groups, mean)
  )
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


