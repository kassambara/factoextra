#' @include get_pca.R
 NULL
#' Visualize Principal Component Analysis
#' 
#' 
#' @description Principal component analysis (PCA) reduces the dimensionality of
#'   multivariate data, to two or three that can be visualized graphically with 
#'   minimal loss of information. fviz_pca() provides ggplot2-based elegant 
#'   visualization of PCA outputs from: i) prcomp and princomp [in built-in R 
#'   stats], ii) PCA [in FactoMineR] and iii) dudi.pca [in ade4]. Read more:
#'   \href{http://www.sthda.com/english/wiki/factominer-and-factoextra-principal-component-analysis-visualization-r-software-and-data-mining}{Principal
#'   Component Analysis}
#'   
#'   \itemize{ \item{fviz_pca_ind(): Graph of individuals} \item{fviz_pca_var():
#'   Graph of variables} \item{fviz_pca_biplot(): Biplot of individuals and 
#'   variables} \item{fviz_pca(): An alias of fviz_pca_biplot()} }
#'   
#' @param X an object of class PCA [FactoMineR]; prcomp and princomp [stats]; 
#'   dudi and pca [ade4].
#' @param axes a numeric vector of length 2 specifying the dimensions to be 
#'   plotted.
#' @param geom a text specifying the geometry to be used for the graph. Allowed 
#'   values are the combination of c("point", "arrow", "text"). Use "point" (to 
#'   show only points); "text" to show only labels; c("point", "text") or 
#'   c("arrow", "text") to show both types.
#' @param label a text specifying the elements to be labelled. Default value is 
#'   "all". Allowed values are "none" or the combination of c("ind", "ind.sup", 
#'   "quali", "var", "quanti.sup"). "ind" can be used to label only active 
#'   individuals. "ind.sup" is for supplementary individuals. "quali" is for 
#'   supplementary qualitative variables. "var" is for active variables. 
#'   "quanti.sup" is for quantitative supplementary variables.
#' @param invisible a text specifying the elements to be hidden on the plot. 
#'   Default value is "none". Allowed values are the combination of c("ind", 
#'   "ind.sup", "quali", "var", "quanti.sup").
#' @param labelsize font size for the labels
#' @param pointsize the size of points
#' @param pointshape the shape of points
#' @param title the title of the graph
#' @param habillage an optional factor variable for coloring the observations by
#'   groups. Default value is "none". If X is a PCA object from FactoMineR 
#'   package, habillage can also specify the supplementary qualitative variable 
#'   (by its index or name) to be used for coloring individuals by groups (see 
#'   ?PCA in FactoMineR).
#' @param addEllipses logical value. If TRUE, draws ellipses around the 
#'   individuals when habillage != "none".
#' @param ellipse.level the size of the concentration ellipse in normal 
#'   probability.
#' @param ellipse.type Character specifying frame type. Possible values are 
#'   'convex' or types supporeted by \code{\link[ggplot2]{stat_ellipse}} 
#'   including one of c("t", "norm", "euclid").
#' @param ellipse.alpha Alpha for ellipse specifying the transparency level of 
#'   fill color. Use alpha = 0 for no fill color.
#' @param axes.linetype linetype of x and y axes.
#' @param col.ind,col.var color for individuals and variables, respectively. 
#'   Possible values include also : "cos2", "contrib", "coord", "x" or "y". In 
#'   this case, the colors for individuals/variables are automatically 
#'   controlled by their qualities of representation ("cos2"), contributions 
#'   ("contrib"), coordinates (x^2+y^2, "coord"), x values ("x") or y values 
#'   ("y"). To use automatic coloring (by cos2, contrib, ....), make sure that 
#'   habillage ="none".
#' @param col.ind.sup color for supplementary individuals
#' @param alpha.ind,alpha.var controls the transparency of individual and 
#'   variable colors, respectively. The value can variate from 0 (total 
#'   transparency) to 1 (no transparency). Default value is 1. Possible values 
#'   include also : "cos2", "contrib", "coord", "x" or "y". In this case, the 
#'   transparency for the individual/variable colors are automatically 
#'   controlled by their qualities ("cos2"), contributions ("contrib"), 
#'   coordinates (x^2+y^2, "coord"), x values("x") or y values("y"). To use 
#'   this, make sure that habillage ="none".
#' @param col.quanti.sup a color for the quantitative supplementary variables.
#' @param col.circle a color for the correlation circle.
#' @param repel a boolean, whether to use ggrepel to avoid overplotting text 
#'   labels or not.
#' @param select.ind,select.var a selection of individuals/variables to be 
#'   drawn. Allowed values are NULL or a list containing the arguments name, 
#'   cos2 or contrib: \itemize{ \item name: is a character vector containing 
#'   individuals/variables to be drawn \item cos2: if cos2 is in [0, 1], ex: 
#'   0.6, then individuals/variables with a cos2 > 0.6 are drawn. if cos2 > 1, 
#'   ex: 5, then the top 5 individuals/variables with the highest cos2 are 
#'   drawn. \item contrib: if contrib > 1, ex: 5,  then the top 5 
#'   individuals/variables with the highest contrib are drawn }
#' @inheritParams ggpubr::ggpar
#' @param ... Arguments to be passed to the function fviz_pca_biplot().
#'   
#' @return a ggplot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @seealso \code{\link{fviz_ca}}, \code{\link{fviz_mca}}
#' @examples
#' \donttest{
#' # Principal component analysis
#' # ++++++++++++++++++++++++++++++
#' data(iris)
#' res.pca <- prcomp(iris[, -5],  scale = TRUE)
#' 
#' # Graph of individuals
#' # +++++++++++++++++++++
#' 
#' # Default plot
#' fviz_pca_ind(res.pca, col.ind = "#00AFBB")
#' 
#'  
#' # 1. Control automatically the color of individuals 
#'    # using the "cos2" or the contributions "contrib"
#'    # cos2 = the quality of the individuals on the factor map
#' # 2. To keep only point or text use geom = "point" or geom = "text".
#' # 3. Change themes: http://www.sthda.com/english/wiki/ggplot2-themes
#' 
#' fviz_pca_ind(res.pca, col.ind="cos2", geom = "point")+
#'  theme_minimal() 
#' 
#' # Change gradient color
#' # Use repel = TRUE to avoid overplotting (slow if many points)
#' fviz_pca_ind(res.pca, col.ind="cos2", repel = TRUE) + 
#'       scale_color_gradient2(low = "white", mid = "#2E9FDF", 
#'       high= "#FC4E07", midpoint=0.6, space = "Lab")+
#'       theme_minimal()
#'    
#' # You can also control the transparency 
#' # of the color by the cos2
#' fviz_pca_ind(res.pca, alpha.ind="cos2") +
#'      theme_minimal()        
#'              
#' # Color individuals by groups, add concentration ellipses
#' # Remove labels: label = "none".
#' p <- fviz_pca_ind(res.pca, label="none", habillage=iris$Species,
#'        addEllipses=TRUE, ellipse.level=0.95)
#' print(p)
#'              
#' # Change group colors using RColorBrewer color palettes
#' # Read more: http://www.sthda.com/english/wiki/ggplot2-colors
#' p + scale_color_brewer(palette="Dark2") +
#'     scale_fill_brewer(palette="Dark2") +
#'      theme_minimal()
#'      
#' # Change group colors manually
#' # Read more: http://www.sthda.com/english/wiki/ggplot2-colors
#' p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
#'  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
#'  theme_minimal()    
#'       
#' # Select and visualize some individuals (ind) with select.ind argument.
#'  # - ind with cos2 >= 0.96: select.ind = list(cos2 = 0.96)
#'  # - Top 20 ind according to the cos2: select.ind = list(cos2 = 20)
#'  # - Top 20 contributing individuals: select.ind = list(contrib = 20)
#'  # - Select ind by names: select.ind = list(name = c("23", "42", "119") )
#'  
#'  # Example: Select the top 40 according to the cos2
#' fviz_pca_ind(res.pca, select.ind = list(cos2 = 40))
#' 
#'  
#' # Graph of variables
#' # ++++++++++++++++++++++++++++
#'   
#' # Default plot
#' fviz_pca_var(res.pca, col.var = "steelblue")+
#' theme_minimal()
#'  
#' # Control variable colors using their contributions
#' fviz_pca_var(res.pca, col.var = "contrib")+
#'  scale_color_gradient2(low="white", mid="blue", 
#'            high="red", midpoint=96, space = "Lab") +
#'  theme_minimal()         
#'  
#' # Select variables with select.var argument
#'    # You can select by contrib, cos2 and name 
#'    # as previously described for ind
#' # Select the top 3 contributing variables
#' fviz_pca_var(res.pca, select.var = list(contrib = 3))
#' 
#'     
#' # Biplot of individuals and variables
#' # ++++++++++++++++++++++++++
#' fviz_pca_biplot(res.pca)
#' 
#' # Keep only the labels for variables
#' # Change the color by groups, add ellipses
#' fviz_pca_biplot(res.pca, label = "var", habillage=iris$Species,
#'                addEllipses=TRUE, ellipse.level=0.95)+
#' theme_minimal()
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
fviz_pca_ind <- function(X,  axes = c(1,2), geom=c("point", "text"), repel = FALSE,
                         label = "all", invisible="none", labelsize=4, 
                         pointsize = 2, pointshape = 19,
                         habillage="none", addEllipses=FALSE, ellipse.level = 0.95, 
                         ellipse.type = "norm", ellipse.alpha = 0.1,
                         col.ind = "black", col.ind.sup = "blue", alpha.ind =1,
                         select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                         title = "Individuals factor map - PCA", axes.linetype = "dashed",
                         ggtheme = ggplot2::theme_grey(),
                         ...)
{
  # Deprecated arguments
  extra_args <- list(...)
  if (!is.null(extra_args$jitter)) {
    warning("argument jitter is deprecated; please use repel = TRUE instead, to avoid overlapping of labels.", 
            call. = FALSE)
    if(!is.null(extra_args$jitter$width) | !is.null(extra_args$jitter$height) ) repel = TRUE
  }
  
  if(length(intersect(geom, c("point", "text", "arrow"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  if(length(axes) != 2) stop("axes should be of length 2")
  
  # Data frame to be used for plotting
  ind <- facto_summarize(X, element = "ind", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(ind)[2:3] <-  c("x", "y")
  
  # Selection
  ind.all <- ind
  if(!is.null(select.ind)) ind <- .select(ind, select.ind)
  
  # Elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  # Qualitative variable is used to color the individuals by groups
  #%%%%%%%%%%%%%%%%%%%%%%%%%
  if(habillage[1] !="none"){
    
    # X is from FactoMineR outputs
    if(inherits(X, "PCA") & length(habillage) == 1){
      data <- X$call$X
      if (is.numeric(habillage)) name.quali <- colnames(data)[habillage]
      else name.quali <- habillage 
      ind <- cbind.data.frame(data[rownames(ind),name.quali], ind)
      colnames(ind)[1]<-name.quali
      if(!inherits(ind[, 1], "factor")) ind[, 1]<-as.factor(ind[,1])
    }
    else{
      if(nrow(ind)!=length(habillage))
        stop("The number of active individuals used in the PCA is different ",
             "from the length of the factor habillage. Please, remove the supplementary ",
             "individuals in the variable habillage.")
      name.quali <- "Groups"
      ind <- cbind.data.frame(Groups = habillage, ind)
      if(!inherits(ind[, 1], "factor")) ind[, 1] <- as.factor(ind[,1])
    }
    col.ind <- name.quali
    if(missing(pointshape)) pointshape <- name.quali
  }
  
  # Plot
  #%%%%%%%%%%%%%%%%%%%
  point <- ("point" %in% geom) & (!hide$ind) # to show individuals point should be TRUE
  mean.point <- (habillage[1] !="none") & ("point" %in% geom) & (!hide$quali) # to show mean point
  
  ind_label <- NULL
  if(lab$ind & "text" %in% geom & !hide$ind) ind_label <- "name"
  
  alpha.limits <- NULL
  if(alpha.ind %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(ind.all[, alpha.ind])
  
  p <- ggpubr::ggscatter(data = ind, x = "x", y = "y",
                         color = col.ind, alpha = alpha.ind, shape = pointshape, 
                         point = point, size = pointsize, mean.point = mean.point,
                         label = ind_label, font.label = labelsize*3, repel = repel,
                         ellipse = addEllipses, ellipse.type = ellipse.type,
                         ellipse.alpha = ellipse.alpha, ellipse.level = ellipse.level,
                         main = title,
                         ggtheme = ggtheme, ...
  )
  if(!is.null(alpha.limits)) p <- p + scale_alpha(limits = alpha.limits)
  
  # Add supplementary quantitative individuals
  # Available only in FactoMineR
  if(inherits(X, 'PCA') & !hide$ind.sup){
    ind_sup <- .get_supp(X, element = "ind.sup", axes = axes,
                         select = select.ind)
    if(!is.null(ind_sup)){
      colnames(ind_sup)[2:3] <-  c("x", "y")
      p <- fviz_add(p, df = ind_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.ind.sup, shape = 19, pointsize = pointsize,
                    labelsize = labelsize, addlabel = (lab$ind.sup & "text" %in% geom),
                    repel = repel)
    }  
  }
  
  p <- .fviz_finish(p, X, axes, axes.linetype) 
  
  p
}


#' @rdname fviz_pca
#' @export 
fviz_pca_var <- function(X, axes=c(1,2), geom=c("arrow", "text"), 
                         label="all",  invisible ="none", repel = FALSE,
                         labelsize=4, col.var="black", alpha.var=1, 
                         col.quanti.sup="blue", col.circle ="grey70",
                         select.var = list(name = NULL, cos2 = NULL, contrib = NULL),
                         title = "Variables factor map - PCA", axes.linetype = "dashed", 
                         ggtheme = theme_grey(), ...)
{
  
  # Deprecated arguments
  extra_args <- list(...)
  if (!is.null(extra_args$jitter)) {
    warning("argument jitter is deprecated; please use repel = TRUE instead, to avoid overlapping of labels.", 
            call. = FALSE)
    if(!is.null(extra_args$jitter$width) | !is.null(extra_args$jitter$height) ) repel = TRUE
  }
  
  if(length(axes) != 2) stop("axes should be of length 2")
  
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
  
  # Plot
  #%%%%%%%%%%%%%%%%%%%%%%%
  alpha.limits <- NULL
  if(alpha.var %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(var.all[, alpha.var])
  
  point <- ("point" %in% geom) & (!hide$var) # to show variable point should be TRUE
  var_label <- NULL
  if(lab$var & "text" %in% geom & !hide$var) var_label <- "name"
  # Draw variables
  p <- ggpubr::ggscatter(data = var, x = 'x', y = 'y', 
                 color = col.var,  alpha = alpha.var, 
                 point = point, label = var_label, 
                 font.label = labelsize*3, repel = repel,
                 ggtheme = ggtheme, main = title, ...)
  # Draw arrows
  if("arrow" %in% geom & !hide$var){
    origin <- rep(0, nrow(var))
    dd <- cbind.data.frame(var, xstart = origin, ystart = origin)
    p <- p + ggpubr:::geom_exec(geom_segment, data = dd, 
                                  x = "xstart", y = "ystart", xend = "x", yend = "y",
                                  arrow = grid::arrow(length = grid::unit(0.2, 'cm')),
                                  color = col.var, alpha = alpha.var)
  }
  if(!is.null(alpha.limits)) p <- p + scale_alpha(limits = alpha.limits)
  
  # Draw correlation circle
  if(scale.unit) p <- .add_corr_circle(p, color = col.circle, axes.linetype = axes.linetype)
  
  # Add supplementary quantitative variables
  # Available only in FactoMineR
  if(inherits(X, 'PCA') & !hide$quanti ){
    
    quanti_sup <- .get_supp(X, element = "quanti", axes = axes,
                            select = select.var)
    if(!is.null(quanti_sup)) colnames(quanti_sup)[2:3] <-  c("x", "y")
    if(!is.null(quanti_sup)){
      p <- fviz_add(p, df = quanti_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.quanti.sup, linetype = 2,
                    labelsize = labelsize, addlabel = (lab$quanti), repel = repel)
    }  
    
  }
  
  p <- .fviz_finish(p, X, axes, axes.linetype) 
  p 
}



#' @rdname fviz_pca
#' @export
fviz_pca_biplot <- function(X,  axes = c(1,2), geom=c("point", "text"), 
                  label = "all", invisible="none", labelsize=4, 
                  pointsize = 2, pointshape = 19,
                  habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                  col.ind = "black", col.ind.sup = "blue", alpha.ind =1,
                  col.var="steelblue",  alpha.var=1, col.quanti.sup="blue",
                  col.circle ="grey70", repel = FALSE, axes.linetype = "dashed",
                  select.var = list(name = NULL, cos2 = NULL, contrib = NULL),
                  select.ind = list(name = NULL, cos2 = NULL, contrib = NULL), 
                  title = "Biplot of variables and individuals",
                   ...)
{
  
  if(length(axes) != 2) stop("axes should be of length 2")
  
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
  p <- fviz_pca_ind(X,  axes = axes, geom = geom, repel = repel, label = label, invisible=invisible,
          labelsize=labelsize, pointsize = pointsize, pointshape = pointshape, axes.linetype=axes.linetype,
          col.ind = col.ind, col.ind.sup = col.ind.sup, alpha.ind=alpha.ind,
          habillage=habillage, addEllipses=addEllipses, ellipse.level=ellipse.level,
          select.ind = select.ind,  ...)

  if(!hide$var){
    p <-.ggscatter(p = p, data = var, x = 'x', y = 'y', 
                   col=col.var,  alpha = alpha.var, 
                   alpha.limits = alpha.limits, 
                   geom =  c("arrow", "text"), repel = repel,
                   lab = lab$var, labelsize = labelsize)
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
                    labelsize = labelsize, addlabel = (lab$quanti), repel = repel)
    }  
  }
  p+labs(title=title)
}




#+++++++++++++++++++++
# Helper functions
#+++++++++++++++++++++

# X : an object of class PCA, princomp, prcomp, dudi
# Return TRUE if the data are scaled to unit variance
.get_scale_unit <-function(X){
  scale_unit <- FALSE
  if(inherits(X, 'PCA')) scale_unit <- X$call$scale.unit
  else if(inherits(X, "prcomp" )) scale_unit <- is.numeric(X$scale) 
  else if(inherits(X, "princomp")) scale_unit <- length(unique(X$scale))>1 
  else if(inherits(X, 'pca') & inherits(X, 'dudi')) scale_unit <- length(unique(X$norm))>1 
  else stop("Error in .get_scale_unit function : can't handle an object of class ",
            class(X))
  scale_unit
}

# Add correlation circle to variables plot
.add_corr_circle <- function(p, color = "grey70", axes.linetype = "dashed"){
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- data.frame(xcircle = cos(theta), ycircle = sin(theta))
  p + 
    geom_path(mapping = aes_string("xcircle", "ycircle"), data = circle, color = color)+
    geom_hline(yintercept = 0, linetype=axes.linetype)+
    geom_vline(xintercept = 0, linetype=axes.linetype)
  
}

