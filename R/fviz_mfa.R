#' @include get_mfa.R
NULL
#' Visualize Multiple Factor Analysis
#'
#' @description
#' Graph of individuals/quantitative variables/qualitative variables/group/partial axes from the output of Multiple Factor Analysis (MFA).\cr\cr
#' \itemize{
#' \item{fviz_mfa_ind(): Graph of individuals}
#' \item{fviz_mfa_quanti_var(): Graph of quantitative variables}
#' \item{fviz_mfa_quali_var(): Graph of qualitative variables}
#' \item{fviz_mfa_group(): Graph of the groups representation}
#' \item{fviz_mfa_axes(): Graph of partial axes}
#' \item{fviz_mfa_quali_biplot(): Biplot of individuals and qualitative variables}
#' \item{fviz_mfa(): An alias of fviz_mca_biplot()}
#' }
#' @param X an object of class MFA [FactoMineR].
#' #@inheritParams fviz_mca
#' @param label a text specifying the elements to be labelled.
#'  Default value is "all".
#'  Allowed values are "none" or the combination of c("ind", "ind.sup","var", "quali.sup",  "quanti.sup").
#'  "ind" can be used to label only active individuals.
#'  "ind.sup" is for supplementary individuals.
#' "var" is for active variable categories.
#'  "quali.sup" is for supplementary qualitative variable categories.
#' "quanti.sup" is for quantitative supplementary variables.
#' @param invisible a text specifying the elements to be hidden on the plot.
#'  Default value is "none".
#'  Allowed values are the combination of c("ind", "ind.sup","var", "quali.sup",  "quanti.sup").
#' @param habillage an optional factor variable for coloring
#'  the observations by groups. Default value is "none".
#'  If X is an MFA object from FactoMineR package, habillage can also specify
#'  the index of the factor variable in the data.
#' @param col.ind,col.var color for individuals and variables, respectively.
#'  Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the colors for individuals/variables are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2 + y^2 , "coord"), x values("x") or y values("y").
#'  To use automatic coloring (by cos2, contrib, ....), make sure that habillage ="none".
#' @param alpha.ind,alpha.var controls the transparency of
#'  individual and variable colors, respectively.
#' The value can variate from 0 (total transparency) to 1 (no transparency).
#' Default value is 1. Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the transparency for individual/variable colors are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2 + y^2 , "coord"), x values("x") or y values("y").
#'  To use this, make sure that habillage ="none".
#' @param shape.ind,shape.var point shapes of individuals and variables
#' @param col.quanti.sup,col.quali.sup a color for the quantitative/qualitative supplementary variables.
#' @param select.ind,select.var a selection of individuals/variables to be drawn.
#' Allowed values are NULL or a list containing the arguments name, cos2 or contrib:
#' \itemize{
#' \item name is a character vector containing individuals/variables to be drawn
#' \item cos2 if cos2 is in [0, 1], ex: 0.6, then individuals/variables with a cos2 > 0.6 are drawn.
#' if cos2 > 1, ex: 5, then the top 5 individuals/variables with the highest cos2 are drawn.
#' \item contrib if contrib > 1, ex: 5,  then the top 5 individuals/variables with the highest cos2 are drawn
#' }
#' @param ... Arguments to be passed to the function fviz_mfa_quali_biplot()
#' @param arrows Vector of two logicals specifying if the plot should contain
#'  points (FALSE, default) or arrows (TRUE).
#'  First value sets the rows and the second value sets the columns.
#' @param jitter a parameter used to jitter the points in order to reduce overplotting. 
#' It's a list containing the objects what, width and height (i.e jitter = list(what, width, height)).
#' Otherwise you can use ggrepel for an elegant solution. TODO: ADD FUNCTIONALITY FOR TEXT ONLY!!! 
#' \itemize{
#' \item what: the element to be jittered. Possible values are "point" or "p"; "label" or "l"; "both" or "b"
#' \item width: degree of jitter in x direction
#' \item height: degree of jitter in y direction
#' }
#' @details The default plot of MFA is a "symmetric" plot in which both rows and
#' columns are in principal coordinates. In this situation, it's not possible
#' to interpret the distance between row points and column points. To overcome this
#' problem, the simplest way is to make an asymmetric plot. This means that,
#' the column profiles must be presented in row space or vice-versa.
#' The allowed options for the argument map are:
#' \itemize{
#' \item "rowprincipal" or "colprincipal": asymmetric plots with either rows in principal
#' coordinates and columns in standard coordinates, or vice versa.
#' These plots preserve row metric or column metric respectively.
#' \item "symbiplot": Both rows and columns are scaled to have variances
#' equal to the singular values (square roots of eigenvalues),
#' which gives a symmetric biplot but does not preserve row or column metrics.
#' \item "rowgab" or "colgab": Asymmetric maps, proposed by Gabriel & Odoroff (1990),
#' with rows (respectively, columns) in
#' principal coordinates and columns (respectively, rows) in standard coordinates
#' multiplied by the mass of the corresponding point.
#' \item "rowgreen" or "colgreen": The so-called contribution biplots
#' showing visually the most contributing points (Greenacre 2006b).
#' These are similar to "rowgab" and "colgab" except that the points
#' in standard coordinates are multiplied by the square root of the corresponding masses,
#' giving reconstructions of the standardized residuals.
#' }
#'
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @author Fabian Mundt \email{f.mundt@inventionate.de}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' # Multiple Factor Analysis
#' # ++++++++++++++++++++++++
#' # Install and load FactoMineR to compute MFA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data(poison)
#' res.mfa <- MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
#'                name.group=c("desc","desc2","symptom","eat"),
#'                num.group.sup=1:2, graph=FALSE)
#'
#' # Graph of individuals
#' # ++++++++++++++++++++
#' # Default plot
#' fviz_mfa_ind(res.mfa)
#' # Change title and axis labels
#' fviz_mfa_ind(res.mfa) +
#'  labs(title = "MFA", x = "Dim.1", y ="Dim.2" )
#' # Change axis limits by specifying the min and max
#' fviz_mfa_ind(res.mfa) +
#'    xlim(-0.8, 1.5) + ylim (-1.5, 1.5)
#' # Use text only
#' fviz_mfa_ind(res.mfa, geom = "text")
#' # Use points only
#' fviz_mfa_ind(res.mfa, geom="point")
#' # Change the size of points
#' fviz_mfa_ind(res.mfa, geom="point", pointsize = 4)
#' # Change point color and theme
#' fviz_mfa_ind(res.mfa, col.ind = "blue")+
#'    theme_minimal()
#' # Reduce overplotting
#' fviz_mfa_ind(res.mfa, jitter = list(width = 0.2, height = 0.2))
#'
#' # Control automatically the color of individuals
#' # using the cos2 or the contributions
#' # cos2 = the quality of the individuals on the factor map
#' fviz_mfa_ind(res.mfa, col.ind="cos2")
#' # Gradient color
#' fviz_mfa_ind(res.mfa, col.ind="cos2") +
#'       scale_color_gradient2(low="white", mid="blue",
#'       high="red", midpoint=0.4)
#' # Change the theme and use only points
#' fviz_mfa_ind(res.mfa, col.ind="cos2", geom = "point") +
#'       scale_color_gradient2(low="white", mid="blue",
#'       high="red", midpoint=0.4)+ theme_minimal()
#'
#' # Color by the contributions
#' fviz_mfa_ind(res.mfa, col.ind="contrib") +
#'       scale_color_gradient2(low="white", mid="blue",
#'       high="red", midpoint=1.5)
#'
#' # Control the transparency of the color by the
#' # contributions
#' fviz_mfa_ind(res.mfa, alpha.ind="contrib") +
#'      theme_minimal()
#'
#' # Color individuals by groups
#' grp <- as.factor(poison.active[, "Vomiting"])
#' fviz_mfa_ind(res.mfa, label="none", habillage=grp)
#' # Add ellipses
#' p <- fviz_mfa_ind(res.mfa, label="none", habillage=grp,
#'              addEllipses=TRUE, ellipse.level=0.95)
#' print(p)
#' # Change group colors using RColorBrewer color palettes
#' p + scale_color_brewer(palette="Dark2") +
#'    theme_minimal()
#' p + scale_color_brewer(palette="Paired") +
#'      theme_minimal()
#' p + scale_color_brewer(palette="Set1") +
#'      theme_minimal()
#'
#' # Select and visualize individuals with cos2 >= 0.4
#' fviz_mfa_ind(res.mfa, select.ind = list(cos2 = 0.4))
#' # Select the top 20 according to the cos2
#' fviz_mfa_ind(res.mfa, select.ind = list(cos2 = 20))
#' # Select the top 20 contributing individuals
#' fviz_mfa_ind(res.mfa, select.ind = list(contrib = 20))
#' # Select by names
#' fviz_mfa_ind(res.mfa,
#' select.ind = list(name = c("44", "38", "53",  "39")))
#'
#'
#' # Graph of qantitative variable categories
#' # ++++++++++++++++++++++++++++++++++++++++
#' data(wine)
#' res.mfa <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
#'                ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
#'                num.group.sup=c(1,6), graph=FALSE)
#' # Default plot
#' fviz_mfa_qanti_var(res.mfa)
#' # Change color and theme
#' fviz_mfa_quanti_var(res.mfa, col.var="steelblue")+
#'  theme_minimal()
#'
#' # Control variable colors using their contributions
#' fviz_mfa_quanti_var(res.mfa, col.var = "contrib")+
#'  scale_color_gradient2(low = "white", mid = "blue",
#'            high = "red", midpoint = 2) +
#'  theme_minimal()
#' # Control the transparency of variables using their contributions
#' fviz_mfa_quanti_var(res.mfa, alpha.var = "contrib") +
#'    theme_minimal()
#'
#' # Select and visualize categories with cos2 >= 0.4
#' fviz_mfa_quanti_var(res.mfa, select.var = list(cos2 = 0.4))
#' # Select the top 10 contributing variable categories
#' fviz_mfa_quanti_var(res.mfa, select.var = list(contrib = 10))
#' # Select by names
#' fviz_mfa_quanti_var(res.mfa,
#'  select.var= list(name =  c("Spice.before.shaking", "Aroma.intensity")))
#'  
#' # Graph of categorical variable categories
#' # ++++++++++++++++++++++++++++++++++++++++
#' data(poison)
#' res.mfa <- MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
#'                name.group=c("desc","desc2","symptom","eat"),
#'                num.group.sup=1:2, graph=FALSE)
#'
#' # Default plot
#' fviz_mfa_quali_var(res.mfa)
#' # Change color and theme
#' fviz_mfa_quali_var(res.mfa, col.var="steelblue")+
#'  theme_minimal()
#'
#' # Control variable colors using their contributions
#' fviz_mfa_quali_var(res.mfa, col.var = "contrib")+
#'  scale_color_gradient2(low = "white", mid = "blue",
#'            high = "red", midpoint = 2) +
#'  theme_minimal()
#' # Control the transparency of variables using their contributions
#' fviz_mfa_quali_var(res.mfa, alpha.var = "contrib") +
#'    theme_minimal()
#'
#' # Select and visualize categories with cos2 >= 0.4
#' fviz_mfa_quali_var(res.mfa, select.var = list(cos2 = 0.4))
#' # Select the top 10 contributing variable categories
#' fviz_mfa_quali_var(res.mfa, select.var = list(contrib = 10))
#' # Select by names
#' fviz_mfa_quali_var(res.mfa,
#'  select.var= list(name =  c("Cheese_y", "Nausea_n", "Courg_y")))
#'
#' # Biplot of categorical variable categories and individuals
#' # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' fviz_mfa_quali_biplot(res.mfa)
#' # Keep only the labels for variable categories
#' fviz_mfa_quali_biplot(res.mfa, label ="var")
#' # Keep only labels for individuals
#' fviz_mfa_quali_biplot(res.mfa, label ="ind")
#' # Hide variable categories
#' fviz_mfa_quali_biplot(res.mfa, invisible ="var")
#' # Hide individuals
#' fviz_mfa_quali_biplot(res.mfa, invisible ="ind")
#'# Control automatically the color of individuals using the cos2
#' fviz_mfa_quali_biplot(res.mfa, label ="var", col.ind="cos2") +
#'        theme_minimal()
#' # Change the color by groups, add ellipses
#' grp <- as.factor(poison[, "Vomiting"])
#' fviz_mfa_quali_biplot(res.mfa, label="var", col.var ="blue",
#'    habillage=grp, addEllipses=TRUE, ellipse.level=0.95) +
#'    theme_minimal()
#'
#' # Select the top 30 contributing individuals
#' # And the top 10 variables
#' fviz_mfa_quali_biplot(res.mfa,
#'                select.ind = list(contrib = 30),
#'                select.var = list(contrib = 10))
#'                
#' # Graph of partial individuals (starplot)
#' # +++++++++++++++++++++++++++++++++++++++
#' fviz_mfa_ind_starplot(res.mfa)
#' 
#' # Graph of groups (correlation square)
#' # ++++++++++++++++++++++++++++++++++++
#' fviz_mfa_group(res.mfa)
#' 
#' #' # Graph of partial axes
#' # ++++++++++++++++++++++++
#' fviz_mfa_axes(res.mfa)
#'
#'  }
#' @name fviz_mfa
#' @rdname fviz_mfa
#' @export
fviz_mfa_ind <- function(X,  axes = c(1,2), geom=c("point", "text"),
                         label = "all", invisible="none",
                         labelsize=4, pointsize = 2,
                         habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                         col.ind = "blue", col.ind.sup = "darkblue", alpha.ind =1,
                         shape.ind = 19,
                         select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                         # map ="symmetric",
                         jitter = list(what = "label", width = NULL, height = NULL), ...)
{
  
  if(length(intersect(geom, c("point", "text", "arrow"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  if(length(axes) > 2) stop("axes should be of length 2")
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  ind <- facto_summarize(X, element = "ind",
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(ind)[2:3] <-  c("x", "y")
  
  # scale ind coords according to the type of map
  # ind <- .scale_ca(ind, res.ca = X,  element = "ind",
  #                  type = map, axes = axes)
  
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
                         alpha.limits = alpha.limits, shape = shape.ind,
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
        stop("The number of active individuals used in the MFA is different ",
             "from the length of the factor habillage. Please, remove the supplementary ",
             "individuals in the variable habillage.")
      name.quali <- "Groups"
      ind <- cbind.data.frame(Groups = habillage, ind)
      ind[, 1]<-as.factor(ind[,1])
    }
    # X is from FactoMineR outputs
    else if(inherits(X, "MFA")){
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
        p <- p + ggrepel::geom_text_repel(data = label_coord,
                           aes_string('x', 'y', label = 'name',
                                      color=name.quali, shape = name.quali),  size = labelsize)
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
        p <- p + ggrepel::geom_text_repel(data=coord_quali.sup,
                           aes_string('x', 'y', color=name.quali),
                           label=rownames(coord_quali.sup), size=labelsize)
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
  if(inherits(X, 'MFA') & !hide$ind.sup){
    ind_sup <- .get_supp(X, element = "ind.sup", axes = axes,
                         select = select.ind)
    if(!is.null(ind_sup)) {
      colnames(ind_sup)[2:3] <-  c("x", "y")
      ind_sup <- .scale_ca(ind_sup, res.ca = X,  element = "ind.sup",
                           type = map, axes = axes)
    }
    if(!is.null(ind_sup)){
      p <- fviz_add(p, df = ind_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.ind.sup, shape = 19, pointsize = pointsize,
                    labelsize = labelsize, addlabel = (lab$ind.sup & "text" %in% geom), jitter = jitter )
    }
  }
  
  p <- .fviz_finish(p, X, axes) +
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_vline(xintercept = 0, color = "black", linetype="dashed") +
    labs(title = "Individuals factor map - MFA")
  
  
  p
}


#' @rdname fviz_mfa
#' @export
fviz_mfa_quanti_var <- function(X, axes=c(1,2), geom=c("arrow", "text"), label="all",  invisible ="none",
                                labelsize=4, pointsize = 2, col.var="red", alpha.var=1, shape.var = 17,
                                col.quanti.sup="blue",  col.quali.sup = "darkgreen", col.circle = "grey70",
                                select.var = list(name = NULL, cos2 = NULL, contrib = NULL),
                                # map ="symmetric",
                                jitter = list(what = "label", width = NULL, height = NULL))
{
  # Check if there are quantitative variables.
  if(Hmisc::`%nin%`("c", X$call$type[-X$call$num.group.sup])) 
    if(Hmisc::`%nin%`("s", X$call$type[-X$call$num.group.sup])) 
      stop("There are no quantitative variables to plot.")
  
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  var <- facto_summarize(X, element = "quanti.var",
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(var)[2:3] <-  c("x", "y")
  
  # scale coords according to the type of map
  # var <- .scale_ca(var, res.ca = X,  element = "quanti.var",
  #                  type = map, axes = axes)
  
  
  # Selection
  var.all <- var
  if(!is.null(select.var)) var <- .select(var, select.var)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.var %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(var.all[, alpha.var])
  
  p <- ggplot()
  
  # Draw correlation circle
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- data.frame(xcircle = cos(theta), ycircle = sin(theta))
  p <- ggplot(data = circle, aes_string("xcircle", "ycircle")) +
      geom_path(aes_string("xcircle", "ycircle"), color=col.circle)+
      geom_hline(yintercept = 0, linetype="dashed")+
      geom_vline(xintercept = 0, linetype="dashed")    
  
  if(!hide$var){
    p <-.ggscatter(p = p, data = var, x = 'x', y = 'y',
                   col=col.var,  alpha = alpha.var,
                   alpha.limits = alpha.limits,
                   geom = geom, shape = shape.var,
                   lab = lab$var, labelsize = labelsize,
                   pointsize = pointsize, jitter = jitter)
  }
  
  
  # Add supplementary qualitative variable categories
  # Available only in FactoMineR
  if(inherits(X, 'MFA') & !hide$quali.sup ){
    quali_sup <- .get_supp(X, element = "quali.sup", axes = axes,
                           select = select.var)
    if(!is.null(quali_sup)){
      colnames(quali_sup)[2:3] <-  c("x", "y")
      quali_sup <- .scale_ca(quali_sup, res.ca = X,  element = "quali.sup",
                             type = map, axes = axes)
    }
    if(!is.null(quali_sup)){
      p <- fviz_add(p, df = quali_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.quali.sup, shape = shape.var,
                    labelsize = labelsize, addlabel = (lab$quali.sup),
                    pointsize = pointsize, jitter = jitter)
    }
    
  }
  
  p <- .fviz_finish(p, X, axes) +
    labs(title = "Quantitative Variable categories - MFA")
  p
}



#' @rdname fviz_mfa
#' @export
fviz_mfa_quali_var <- function(X, axes=c(1,2), geom=c("point", "text"), label="all",  invisible ="none",
                               labelsize=4, pointsize = 2, col.var="red", alpha.var=1, shape.var = 17,
                               col.quanti.sup="blue",  col.quali.sup = "darkgreen",
                               select.var = list(name = NULL, cos2 = NULL, contrib = NULL),
                               # map ="symmetric", 
                               jitter = list(what = "label", width = NULL, height = NULL))
{
  
  # Check if there are qualitative variables.
  if(Hmisc::`%nin%`("n", X$call$type[-X$call$num.group.sup])) 
      stop("There are no qualitative variables to plot.")
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  var <- facto_summarize(X, element = "quali.var",
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(var)[2:3] <-  c("x", "y")
  
  # scale coords according to the type of map
  # var <- .scale_ca(var, res.ca = X,  element = "quali.var",
  #                  type = map, axes = axes)
  
  
  # Selection
  var.all <- var
  if(!is.null(select.var)) var <- .select(var, select.var)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.var %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(var.all[, alpha.var])
  
  p <- ggplot()
  
  if(!hide$var){
    p <-.ggscatter(p = p, data = var, x = 'x', y = 'y',
                   col=col.var,  alpha = alpha.var,
                   alpha.limits = alpha.limits,
                   geom = geom, shape = shape.var,
                   lab = lab$var, labelsize = labelsize,
                   pointsize = pointsize, jitter = jitter)
  }
  
  
  # Add supplementary qualitative variable categories
  # Available only in FactoMineR
  if(inherits(X, 'MFA') & !hide$quali.sup ){
    quali_sup <- .get_supp(X, element = "quali.sup", axes = axes,
                           select = select.var)
    if(!is.null(quali_sup)){
      colnames(quali_sup)[2:3] <-  c("x", "y")
      quali_sup <- .scale_ca(quali_sup, res.ca = X,  element = "quali.sup",
                             type = map, axes = axes)
    }
    if(!is.null(quali_sup)){
      p <- fviz_add(p, df = quali_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.quali.sup, shape = shape.var,
                    labelsize = labelsize, addlabel = (lab$quali.sup),
                    pointsize = pointsize, jitter = jitter)
    }
    
  }
  
  p <- .fviz_finish(p, X, axes) +
    labs(title = "Qualitative Variable categories - MFA")
  p
}


#' @rdname fviz_mfa
#' @export
fviz_mfa_quali_biplot <- function(X,  axes = c(1,2), geom=c("point", "text"),
                                  label = "all", invisible="none", labelsize=4, pointsize = 2,
                                  habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                                  col.ind = "blue", col.ind.sup = "darkblue", alpha.ind =1,
                                  col.var="red", alpha.var=1, col.quanti.sup="blue",
                                  col.quali.sup = "darkgreen",
                                  shape.ind = 19, shape.var = 17,
                                  select.var = list(name = NULL, cos2 = NULL, contrib = NULL),
                                  select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                                  # map ="symmetric", 
                                  arrows = c(FALSE, FALSE),
                                  jitter = list(what = "label", width = NULL, height = NULL), ...)
{
  
  # Check if there are qualitative variables.
  if(Hmisc::`%nin%`("n", X$call$type[-X$call$num.group.sup])) 
    stop("There are no qualitative variables to plot.")
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  var <- facto_summarize(X, element = "quali.var",
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(var)[2:3] <-  c("x", "y")
  
  # scale coords according to the type of map
  # var <- .scale_ca(var, res.ca = X,  element = "quali.var",
  #                  type = map, axes = axes)
  
  # Selection
  var.all <- var
  if(!is.null(select.var)) var <- .select(var, select.var)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.var %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(var.all[, alpha.var])
  
  
  # Individuals
  geom2 <- geom
  if(arrows[1]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  p <- fviz_mfa_ind(X,  axes = axes, geom = geom2, label = label, invisible=invisible,
                    labelsize=labelsize, pointsize = pointsize,
                    col.ind = col.ind, col.ind.sup = col.ind.sup, alpha.ind=alpha.ind,
                    shape.ind=shape.ind,
                    habillage=habillage, addEllipses=addEllipses, ellipse.level=ellipse.level,
                    select.ind = select.ind, jitter = jitter)
  
  # geometry for variable
  geom2 <- geom
  if(arrows[2]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  
  if(!hide$var){
    p <-.ggscatter(p = p, data = var, x = 'x', y = 'y',
                   col=col.var,  alpha = alpha.var,
                   alpha.limits = alpha.limits,
                   geom =  geom2, shape = shape.var,
                   lab = lab$var, labelsize = labelsize, pointsize = pointsize, jitter = jitter)
  }
  
  # Add supplementary qualitative variable categories
  # Available only in FactoMineR
  if(inherits(X, 'MFA') & !hide$quali.sup ){
    quali_sup <- .get_supp(X, element = "quali.sup", axes = axes,
                           select = select.var)
    if(!is.null(quali_sup)){
      colnames(quali_sup)[2:3] <- c("x", "y")
      quali_sup <- .scale_ca(quali_sup, res.ca = X,  element = "quali.sup",
                             type = map, axes = axes)
    }
    if(!is.null(quali_sup)){
      p <- fviz_add(p, df = quali_sup[, 2:3, drop = FALSE], geom = geom2,
                    color = col.quali.sup, shape = shape.var,
                    labelsize = labelsize, addlabel = (lab$quali.sup), pointsize = pointsize, jitter = jitter )
    }
    
  }
  p+labs(title="MFA factor map - Biplot")
}


#' @rdname fviz_mfa
#' @export
fviz_mfa_ind_starplot <- function(X,  axes = c(1,2), geom=c("point", "text"),
                                  label = "all", invisible="none", legend.partial.title = NULL,
                                  labelsize=4, pointsize = 2, linesize = 0.5,
                                  habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                                  col.ind = "black", col.ind.sup = "darkblue", col.partial = "black",
                                  alpha.ind = 1, shape.ind = 19, alpha.partial = 1,
                                  select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                                  select.partial = list(name = NULL, cos2 = NULL, contrib = NULL),
                                  # map ="symmetric",
                                  jitter = list(what = "label", width = NULL, height = NULL), ...)
{
  
  if(length(intersect(geom, c("point", "text", "arrow"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  if(length(axes) > 2) stop("axes should be of length 2")
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  ind.sum <- facto_summarize(X, element = "ind",
                             result = c("coord", "contrib", "cos2", "coord.partial"), axes = axes)
  ind <- ind.sum$res
  colnames(ind)[2:3] <-  c("x", "y")
  # partial points
  ind.partial <- ind.sum$res.partial
  colnames(ind.partial)[3:4] <-  c("x.partial", "y.partial")
  ind.partial <- merge(ind, ind.partial, by = "name")
  
  # scale ind coords according to the type of map
  # ind <- .scale_ca(ind, res.ca = X,  element = "ind",
  #                  type = map, axes = axes)
  
  # Selection
  ind.all <- ind
  if(!is.null(select.ind))
    ind <- .select(ind, select.ind)
  
  if(!is.null(select.partial)) {
    if(nrow(ind) != nrow(ind.all)) warning("You've already selected individuals. Partial points are only calculated for them.")
    ind.partial <-  ind.partial[ind.partial$name %in% .select(ind, select.partial)$name, , drop = FALSE]
  }
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  # partial points
  if(col.ind %in% c("cos2","contrib", "coord")) col.partial <- col.ind
  else if(is.null(col.partial)) col.partial <- "group.name"
  
  alpha.limits <- NULL
  if(alpha.ind %in% c("cos2","contrib", "coord", "coord.partial", "x", "y")) 
    alpha.limits = range(ind.all[, alpha.ind])
  
  
  # No qualitative variable to color individuals
  if(habillage[1]=="none"){
    p <- ggplot()
    if(hide$ind) p <-ggplot()+geom_blank(data=ind, aes_string('x','y'))
    else {
      p <- .ggscatter(data = ind, data.partial = ind.partial, x = 'x', y = 'y',
                      col=col.ind,  alpha = alpha.ind, col.partial = col.partial,
                      alpha.limits = alpha.limits, shape = shape.ind,
                      geom = geom, lab = lab$ind, labelsize = labelsize,
                      pointsize = pointsize, jitter = jitter, linesize = linesize)
    }
  }
  # qualitative variable is used to color the individuals
  else{
    
    # Plot individuals
    p <- ggplot()
    if(hide$ind & hide$quali) p <-ggplot()+geom_blank(data=ind, aes_string('x','y'))
    
    if(is.factor(habillage)){
      if(nrow(ind)!=length(habillage))
        stop("The number of active individuals used in the MFA is different ",
             "from the length of the factor habillage. Please, remove the supplementary ",
             "individuals in the variable habillage.")
      name.quali <- "Groups"
      ind <- cbind.data.frame(Groups = habillage, ind)
      ind[, 1]<-as.factor(ind[,1])
    }
    # X is from FactoMineR outputs
    else if(inherits(X, "MFA")){
      data <- X$call$X
      if (is.numeric(habillage)) name.quali <- colnames(data)[habillage]
      else name.quali <- habillage
      ind <- cbind.data.frame(data[rownames(ind),name.quali], ind)
      colnames(ind)[1]<-name.quali
      ind[, 1]<-as.factor(ind[,1])
    }
    # Update partial individuals with habillage infos
    ind.partial <- ind.sum$res.partial
    colnames(ind.partial)[3:4] <-  c("x.partial", "y.partial")
    ind.partial <- merge(ind, ind.partial, by = "name")
    
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
      
      if("point" %in% geom) {
        # Partial points
        p <- p + geom_point(data = ind.partial,
                            aes_string(x = 'x.partial', y = 'y.partial', colour = name.quali, 
                                       shape = name.quali), size = pointsize)
        # Partial segments
        p <- p + geom_segment(data = ind.partial,
                            aes_string(x = 'x', y = 'y', xend = 'x.partial', yend = 'y.partial',
                                       linetype = 'group.name', colour = name.quali), 
                            size = linesize)
        # Centroids
        p <- p+geom_point(data = ind,
                          aes_string('x', 'y', color=name.quali, shape = name.quali),
                          size = pointsize)
      }
      if(lab$ind & "text" %in% geom)
        p <- p + ggrepel::geom_text_repel(data = label_coord,
                                          aes_string('x', 'y', label = 'name',
                                                     color=name.quali, shape = name.quali),  size = labelsize)
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
        p <- p + ggrepel::geom_text_repel(data=coord_quali.sup,
                                          aes_string('x', 'y', color=name.quali),
                                          label=rownames(coord_quali.sup), size=labelsize)
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
  if(inherits(X, 'MFA') & !hide$ind.sup){
    ind_sup <- .get_supp(X, element = "ind.sup", axes = axes,
                         select = select.ind)
    if(!is.null(ind_sup)) {
      colnames(ind_sup)[2:3] <-  c("x", "y")
      ind_sup <- .scale_ca(ind_sup, res.ca = X,  element = "ind.sup",
                           type = map, axes = axes)
    }
    if(!is.null(ind_sup)){
      p <- fviz_add(p, df = ind_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.ind.sup, shape = 19, pointsize = pointsize,
                    labelsize = labelsize, addlabel = (lab$ind.sup & "text" %in% geom), jitter = jitter )
    }
  }
  
  p <- .fviz_finish(p, X, axes) +
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_vline(xintercept = 0, color = "black", linetype="dashed") +
    # Edit legend title
    scale_shape(name = legend.partial.title) + 
    scale_linetype(name = legend.partial.title) +
    # Edit plot title
    labs(title = "Individuals factor map - MFA")
  
  
  p
}


#' @rdname fviz_mfa
#' @export
#' PROBELM LÖSEN, DASS DIE LABELS NICHT IMMER KORREKT ERSCHEINEN
fviz_mfa_group <- function(X,  axes = c(1,2), geom=c("point", "text"), alpha.group=1, shape.group = 17,
                           label = "all", invisible="none", labelsize=4, pointsize = 2,
                           col.group="blue",  col.group.sup = "darkgreen",
                           select.group = list(name = NULL, cos2 = NULL, contrib = NULL),
                           # map ="symmetric", 
                           jitter = list(what = "label", width = NULL, height = NULL), ...)
{
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  group <- facto_summarize(X, element = "group",
                           result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(group)[2:3] <-  c("x", "y")
  
  # Selection
  group.all <- group
  if(!is.null(select.group)) group <- .select(group, select.group)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.group %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(group.all[, alpha.group])
  
  p <- ggplot()
  
  if(!hide$group){
    p <-.ggscatter(p = p, data = group, x = 'x', y = 'y',
                   col=col.group,  alpha = alpha.group,
                   alpha.limits = alpha.limits,
                   geom = geom, shape = shape.group,
                   lab = lab$group, labelsize = labelsize,
                   pointsize = pointsize, jitter = jitter,
                   repel = FALSE)
  }
  
  # Set fix dimensions
  p <- p + xlim(c(0,1)) + ylim(c(0,1))
  
  p + labs(title="MFA - Groups Representations")
}



#' @rdname fviz_mfa
#' @export
fviz_mfa_axes <- function(X,  axes = c(1,2), geom=c("arrow", "text"),
                          label = "all", invisible="none", labelsize=4, pointsize = 2,
                          col.axes="red", alpha.axes=1, col.circle ="grey70",
                          select.axes = list(name = NULL, contrib = NULL),
                          # map ="symmetric",
                          arrows = c(FALSE, FALSE),
                          jitter = list(what = "label", width = NULL, height = NULL), ...)
{
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  partial.axes <- facto_summarize(X, element = "partial.axes",
                         result = c("coord", "contrib"), axes = axes)
  colnames(partial.axes)[2:3] <-  c("x", "y")
  
  
  # scale coords according to the type of map
  # var <- .scale_ca(var, res.ca = X,  element = "partial.axes",
  #                  type = map, axes = axes)
  
  # Selection
  partial.axes.all <- partial.axes
  if(!is.null(select.axes)) partial.axes <- .select(partial.axes, select.axes)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.axes %in% c("contrib", "coord", "x", "y"))
    alpha.limits = range(partial.axes.all[, alpha.axes])
  
  # Draw correlation circle
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- data.frame(xcircle = cos(theta), ycircle = sin(theta))
  p <- ggplot(data = circle, aes_string("xcircle", "ycircle")) +
    geom_path(color=col.circle)+
    geom_hline(yintercept = 0, linetype="dashed")+
    geom_vline(xintercept = 0, linetype="dashed")
  
  # geometry for variable
  geom2 <- geom
  if(arrows[2]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  
  if(!hide$partial.axes){
    p <-.ggscatter(p = p, data = partial.axes, x = 'x', y = 'y',
                   col=col.axes,  alpha = alpha.axes,
                   alpha.limits = alpha.limits,
                   geom =  geom2, lab = lab$var, labelsize = labelsize, 
                   pointsize = pointsize, jitter = jitter, repel = FALSE)
  }
  
  p + labs(title="MFA - Partial Axes Representations")
}


#' @rdname fviz_mfa
#' @export
fviz_mfa <- function(X, ...){
  fviz_mfa_ind(X, ...)
  fviz_mfa_quanti_var(X, ...)
  fviz_mfa_quali_var(X, ...)
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
