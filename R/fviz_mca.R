#' @include utilities.R get_mca.R
NULL
#'Visualize Multiple Correspondence Analysis
#'
#'@description Multiple Correspondence Analysis (MCA) is an extension of simple 
#'  CA to analyse a data table containing more than two categorical variables. 
#'  fviz_mca() provides ggplot2-based elegant visualization of MCA outputs from 
#'  the R functions: MCA [in FactoMineR], and acm [in ade4]. Read more: 
#'  \href{http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining}{Multiple
#'   Correspondence Analysis Essentials.}
#'  
#'  \itemize{ \item{fviz_mca_ind(): Graph of individuals} \item{fviz_mca_var(): 
#'  Graph of variables} \item{fviz_mca_biplot(): Biplot of individuals and 
#'  variables} \item{fviz_mca(): An alias of fviz_mca_biplot()} }
#'@param X an object of class MCA [FactoMineR], acm [ade4].
#'@inheritParams fviz_pca
#'@param label a text specifying the elements to be labelled. Default value is 
#'  "all". Allowed values are "none" or the combination of c("ind", 
#'  "ind.sup","var", "quali.sup",  "quanti.sup"). "ind" can be used to label 
#'  only active individuals. "ind.sup" is for supplementary individuals. "var" 
#'  is for active variable categories. "quali.sup" is for supplementary 
#'  qualitative variable categories. "quanti.sup" is for quantitative 
#'  supplementary variables.
#'@param invisible a text specifying the elements to be hidden on the plot. 
#'  Default value is "none". Allowed values are the combination of c("ind", 
#'  "ind.sup","var", "quali.sup",  "quanti.sup").
#'@param habillage an optional factor variable for coloring the observations by 
#'  groups. Default value is "none". If X is an MCA object from FactoMineR 
#'  package, habillage can also specify the index of the factor variable in the 
#'  data.
#'@param col.ind,col.var color for individuals and variables, respectively. 
#'  Possible values include also : "cos2", "contrib", "coord", "x" or "y". In 
#'  this case, the colors for individuals/variables are automatically controlled
#'  by their qualities ("cos2"), contributions ("contrib"), coordinates (x^2 + 
#'  y^2 , "coord"), x values("x") or y values("y"). To use automatic coloring 
#'  (by cos2, contrib, ....), make sure that habillage ="none".
#'@param alpha.ind,alpha.var controls the transparency of individual and 
#'  variable colors, respectively. The value can variate from 0 (total 
#'  transparency) to 1 (no transparency). Default value is 1. Possible values 
#'  include also : "cos2", "contrib", "coord", "x" or "y". In this case, the 
#'  transparency for individual/variable colors are automatically controlled by 
#'  their qualities ("cos2"), contributions ("contrib"), coordinates (x^2 + y^2 
#'  , "coord"), x values("x") or y values("y"). To use this, make sure that 
#'  habillage ="none".
#'@param axes.linetype linetype of x and y axes.
#'@param shape.ind,shape.var point shapes of individuals and variables.
#'@param col.quanti.sup,col.quali.sup a color for the quantitative/qualitative 
#'  supplementary variables.
#'@param repel a boolean, whether to use ggrepel to avoid overplotting text 
#'  labels or not.
#'@param title the title of the graph
#'@param select.ind,select.var a selection of individuals/variables to be drawn.
#'  Allowed values are NULL or a list containing the arguments name, cos2 or 
#'  contrib: \itemize{ \item name is a character vector containing 
#'  individuals/variables to be drawn \item cos2 if cos2 is in [0, 1], ex: 0.6, 
#'  then individuals/variables with a cos2 > 0.6 are drawn. if cos2 > 1, ex: 5, 
#'  then the top 5 individuals/variables with the highest cos2 are drawn. \item 
#'  contrib if contrib > 1, ex: 5,  then the top 5 individuals/variables with 
#'  the highest contrib are drawn }
#'@param ... Arguments to be passed to the function fviz_mca_biplot()
#'@param map character string specifying the map type. Allowed options include: 
#'  "symmetric", "rowprincipal", "colprincipal", "symbiplot", "rowgab", 
#'  "colgab", "rowgreen" and "colgreen". See details
#'@param arrows Vector of two logicals specifying if the plot should contain 
#'  points (FALSE, default) or arrows (TRUE). First value sets the rows and the 
#'  second value sets the columns.
#'@param jitter a parameter used to jitter the points in order to reduce 
#'  overplotting. It's a list containing the objects what, width and height (i.e
#'  jitter = list(what, width, height)). \itemize{ \item what: the element to be
#'  jittered. Possible values are "point" or "p"; "label" or "l"; "both" or "b" 
#'  \item width: degree of jitter in x direction \item height: degree of jitter 
#'  in y direction }
#'@details The default plot of MCA is a "symmetric" plot in which both rows and 
#'  columns are in principal coordinates. In this situation, it's not possible 
#'  to interpret the distance between row points and column points. To overcome 
#'  this problem, the simplest way is to make an asymmetric plot. This means 
#'  that, the column profiles must be presented in row space or vice-versa. The 
#'  allowed options for the argument map are: \itemize{ \item "rowprincipal" or 
#'  "colprincipal": asymmetric plots with either rows in principal coordinates 
#'  and columns in standard coordinates, or vice versa. These plots preserve row
#'  metric or column metric respectively. \item "symbiplot": Both rows and 
#'  columns are scaled to have variances equal to the singular values (square 
#'  roots of eigenvalues), which gives a symmetric biplot but does not preserve 
#'  row or column metrics. \item "rowgab" or "colgab": Asymmetric maps, proposed
#'  by Gabriel & Odoroff (1990), with rows (respectively, columns) in principal 
#'  coordinates and columns (respectively, rows) in standard coordinates 
#'  multiplied by the mass of the corresponding point. \item "rowgreen" or 
#'  "colgreen": The so-called contribution biplots showing visually the most 
#'  contributing points (Greenacre 2006b). These are similar to "rowgab" and 
#'  "colgab" except that the points in standard coordinates are multiplied by 
#'  the square root of the corresponding masses, giving reconstructions of the 
#'  standardized residuals. }
#'  
#'@return a ggplot2 plot
#'@author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'@seealso \code{\link{get_mca}}, \code{\link{fviz_pca}}, \code{\link{fviz_ca}},
#'  \code{\link{fviz_mfa}}, \code{\link{fviz_hmfa}}
#' @examples
#' # Multiple Correspondence Analysis
#' # ++++++++++++++++++++++++++++++
#' # Install and load FactoMineR to compute MCA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data(poison)
#' poison.active <- poison[1:55, 5:15]
#' head(poison.active)
#' res.mca <- MCA(poison.active, graph=FALSE)
#' 
#' # Graph of individuals
#' # +++++++++++++++++++++
#' 
#' # Default Plot
#' # Color of individuals: col.ind = "steelblue"
#' fviz_mca_ind(res.mca, col.ind = "steelblue")
#' 
#' # 1. Control automatically the color of individuals 
#'    # using the "cos2" or the contributions "contrib"
#'    # cos2 = the quality of the individuals on the factor map
#' # 2. To keep only point or text use geom = "point" or geom = "text".
#' # 3. Change themes: http://www.sthda.com/english/wiki/ggplot2-themes
#' 
#' fviz_mca_ind(res.mca, col.ind = "cos2", repel = TRUE)+
#' theme_minimal()
#'
#' \dontrun{     
#' # You can also control the transparency 
#' # of the color by the cos2
#' fviz_mca_ind(res.mca, alpha.ind="cos2") +
#'      theme_minimal()  
#'}
#'      
#' # Color individuals by groups, add concentration ellipses
#' # Remove labels: label = "none".
#' grp <- as.factor(poison.active[, "Vomiting"])
#' p <- fviz_mca_ind(res.mca, label="none", habillage=grp,
#'        addEllipses=TRUE, ellipse.level=0.95)
#' print(p)
#'       
#'     
#' # Change group colors using RColorBrewer color palettes
#' # Read more: http://www.sthda.com/english/wiki/ggplot2-colors
#' p + scale_color_brewer(palette="Dark2") +
#'     scale_fill_brewer(palette="Dark2") +
#'      theme_minimal()
#'      
#' # Change group colors manually
#' # Read more: http://www.sthda.com/english/wiki/ggplot2-colors
#' p + scale_color_manual(values=c("#999999", "#E69F00"))+
#'  scale_fill_manual(values=c("#999999", "#E69F00"))+
#'  theme_minimal()  
#'              
#'              
#' # Select and visualize some individuals (ind) with select.ind argument.
#'  # - ind with cos2 >= 0.4: select.ind = list(cos2 = 0.4)
#'  # - Top 20 ind according to the cos2: select.ind = list(cos2 = 20)
#'  # - Top 20 contributing individuals: select.ind = list(contrib = 20)
#'  # - Select ind by names: select.ind = list(name = c("44", "38", "53",  "39") )
#'  
#' # Example: Select the top 40 according to the cos2
#' fviz_mca_ind(res.mca, select.ind = list(cos2 = 20))
#' 
#'  
#' # Graph of variable categories
#' # ++++++++++++++++++++++++++++
#' # Default plot: use repel = TRUE to avoid overplotting
#' fviz_mca_var(res.mca, col.var = "#FC4E07")+
#' theme_minimal()
#' 
#' # Control variable colors using their contributions
#' # use repel = TRUE to avoid overplotting
#' fviz_mca_var(res.mca, col.var = "contrib")+
#'  scale_color_gradient2(low="white", mid="blue", 
#'            high="red", midpoint=2, space = "Lab") +
#'  theme_minimal()      
#'         
#'    
#' # Select variables with select.var argument
#'    # You can select by contrib, cos2 and name 
#'    # as previously described for ind
#' # Select the top 10 contributing variables
#' fviz_mca_var(res.mca, select.var = list(contrib = 10))
#'     
#' # Biplot
#' # ++++++++++++++++++++++++++
#' grp <- as.factor(poison.active[, "Vomiting"])
#' fviz_mca_biplot(res.mca, repel = TRUE, col.var = "#E7B800",
#'  habillage = grp, addEllipses = TRUE, ellipse.level = 0.95)+
#'  theme_minimal()
#'  
#'  \dontrun{
#' # Keep only the labels for variable categories: 
#' fviz_mca_biplot(res.mca, label ="var")
#' 
#' # Keep only labels for individuals
#' fviz_mca_biplot(res.mca, label ="ind")
#' 
#' # Hide variable categories
#' fviz_mca_biplot(res.mca, invisible ="var")
#' 
#' # Hide individuals
#' fviz_mca_biplot(res.mca, invisible ="ind")
#' 
#'# Control automatically the color of individuals using the cos2
#' fviz_mca_biplot(res.mca, label ="var", col.ind="cos2") +
#'        theme_minimal()
#'        
#' # Change the color by groups, add ellipses
#' fviz_mca_biplot(res.mca, label="var", col.var ="blue",
#'    habillage=grp, addEllipses=TRUE, ellipse.level=0.95) + 
#'    theme_minimal() 
#'                
#' # Select the top 30 contributing individuals
#' # And the top 10 variables
#' fviz_mca_biplot(res.mca,  
#'                select.ind = list(contrib = 30),
#'                select.var = list(contrib = 10)) 
#' }
#' 
#'@name fviz_mca
#'@rdname fviz_mca
#'@export
fviz_mca_ind <- function(X,  axes = c(1,2), geom=c("point", "text"),
                         label = "all", invisible="none", 
                         labelsize=4, pointsize = 2, repel = FALSE,
                         habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                         ellipse.type = "norm", ellipse.alpha = 0.1,
                         col.ind = "blue", col.ind.sup = "darkblue", alpha.ind =1,
                         shape.ind = 19, axes.linetype = "dashed",
                         select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                         map ="symmetric", title = "Individuals factor map - MCA",
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
  ind <- .scale_ca(ind, res.ca = X,  element = "ind", 
                   type = map, axes = axes)
  
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
                         col=col.ind,  alpha = alpha.ind, repel = repel,
                         alpha.limits = alpha.limits, shape = shape.ind, 
                         geom = geom, lab = lab$ind, labelsize = labelsize,
                         pointsize = pointsize, jitter = jitter)
  }
  # qualitative variable is used to color the individuals
  else{
    
    # Plot individuals
    p <- ggplot()
    if(hide$ind & hide$quali) p <-ggplot()+geom_blank(data=ind, aes_string('x','y'))
    
#     if(is.factor(habillage)){ 
#       if(nrow(ind)!=length(habillage))
#         stop("The number of active individuals used in the MCA is different ",
#              "from the length of the factor habillage. Please, remove the supplementary ",
#              "individuals in the variable habillage.")
#       name.quali <- "Groups"
#       ind <- cbind.data.frame(Groups = habillage, ind)
#       ind[, 1]<-as.factor(ind[,1])
#     }
    
    # X is from FactoMineR outputs
    if(inherits(X, c("MCA")) & length(habillage) == 1){
      data <- X$call$X
      if (is.numeric(habillage)) name.quali <- colnames(data)[habillage]
      else name.quali <- habillage 
      ind <- cbind.data.frame(data[rownames(ind),name.quali], ind)
      colnames(ind)[1]<-name.quali
      ind[, 1]<-as.factor(ind[,1])
    }
    else{
      if(nrow(ind)!=length(habillage))
        stop("The number of active individuals used in the PCA is different ",
             "from the length of the factor habillage. Please, remove the supplementary ",
             "individuals in the variable habillage.")
      name.quali <- "Groups"
      ind <- cbind.data.frame(Groups = habillage, ind)
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
      if(lab$ind & "text" %in% geom) {
        if(repel)
          p <- p + ggrepel::geom_text_repel(data = label_coord, 
                                            aes_string('x', 'y', label = 'name',
                                                       color=name.quali, shape = name.quali),  size = labelsize)
        else
          p <- p + geom_text(data = label_coord, 
                             aes_string('x', 'y', label = 'name',
                                        color=name.quali, shape = name.quali),  size = labelsize, vjust = -0.7)
      }
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
      if(lab$quali & "text" %in% geom) {
        if(repel)
          p <- p + ggrepel::geom_text_repel(data=coord_quali.sup, 
                                            aes_string('x', 'y', color=name.quali),
                                            label=rownames(coord_quali.sup), size=labelsize)
        else 
          p <- p + geom_text(data=coord_quali.sup, 
                             aes_string('x', 'y', color=name.quali),
                             label=rownames(coord_quali.sup), size=labelsize, vjust=-1)
      }
    }
    
#     if(addEllipses){
#       ell <- .get_ellipse_by_groups(ind$x, ind$y,
#                                     groups = ind[, name.quali], ellipse.level=ellipse.level)
#       colnames(ell)<-c(name.quali, "x", "y")
#       ell[, 1]<-as.factor(ell[,1])
#       p <- p + geom_path(data = ell, aes_string('x', 'y', color = name.quali, group = name.quali))
#     }
    
    if(addEllipses){
      if (ellipse.type == 'convex'){
        frame.data <- .cluster_chull(ind[, c("x", "y")], ind[, name.quali])
        colnames(frame.data)[which(colnames(frame.data) == "cluster")] <- name.quali
        mapping = aes_string(x = "x", y = "y", colour =name.quali, fill = name.quali, group = name.quali)
        p <- p + ggplot2::geom_polygon(data = frame.data,  mapping = mapping, alpha = ellipse.alpha)
      }
      else if (ellipse.type %in% c('t', 'norm', 'euclid')) {
        mapping = aes_string(x = "x", y = "y", colour = name.quali, group = name.quali, fill = name.quali)
        p <- p + ggplot2::stat_ellipse(mapping = mapping, data = ind,
                                       level = ellipse.level, type = ellipse.type, alpha = ellipse.alpha,
                                       geom = 'polygon')
      }
    }
    
    
    
  }
  
  # Add supplementary quantitative individuals
  # Available only in FactoMineR
  if(inherits(X, c('MCA')) & !hide$ind.sup){
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
  
  title2 <- title
  p <- .fviz_finish(p, X, axes, axes.linetype) +
    labs(title = title2)
  
  
  p
}


#' @rdname fviz_mca
#' @export 
fviz_mca_var <- function(X, axes=c(1,2), geom=c("point", "text"), label="all",  invisible ="none",
                         labelsize=4, pointsize = 2, col.var="red", alpha.var=1, shape.var = 17, 
                         col.quanti.sup="blue",  col.quali.sup = "darkgreen", repel = FALSE, title = "Variable categories- MCA",
                         select.var = list(name = NULL, cos2 = NULL, contrib = NULL), axes.linetype = "dashed",
                         map ="symmetric", jitter = list(what = "label", width = NULL, height = NULL))
{
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  var <- facto_summarize(X, element = "var", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(var)[2:3] <-  c("x", "y")
  
  # scale coords according to the type of map
  var <- .scale_ca(var, res.ca = X,  element = "var", 
                   type = map, axes = axes)
  
  
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
                   alpha.limits = alpha.limits, repel = repel,
                   geom = geom, shape = shape.var,
                   lab = lab$var, labelsize = labelsize,
                   pointsize = pointsize, jitter = jitter)
  }
  
  
  # Add supplementary qualitative variable categories
  # Available only in FactoMineR
  if(inherits(X, c('MCA')) & !hide$quali.sup ){
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
  
  title2 <- title
  p <- .fviz_finish(p, X, axes, axes.linetype) +
    labs(title = title2)
  p 
}



#' @rdname fviz_mca
#' @export
fviz_mca_biplot <- function(X,  axes = c(1,2), geom=c("point", "text"),
                  label = "all", invisible="none", labelsize=4, pointsize = 2,
                  habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                  col.ind = "blue", col.ind.sup = "darkblue", alpha.ind =1,
                  col.var="red", alpha.var=1, col.quanti.sup="blue",
                  col.quali.sup = "darkgreen", repel = FALSE,
                  shape.ind = 19, shape.var = 17, axes.linetype = "dashed",
                  select.var = list(name = NULL, cos2 = NULL, contrib = NULL),
                  select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                  map ="symmetric", arrows = c(FALSE, FALSE), title = "MCA factor map - Biplot",
                  jitter = list(what = "label", width = NULL, height = NULL), ...)
{
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  var <- facto_summarize(X, element = "var", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(var)[2:3] <-  c("x", "y")
  
  # scale coords according to the type of map
  var <- .scale_ca(var, res.ca = X,  element = "var", 
                   type = map, axes = axes)
  
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
  p <- fviz_mca_ind(X,  axes = axes, geom = geom2, label = label, invisible=invisible,
          labelsize=labelsize, pointsize = pointsize,
          col.ind = col.ind, col.ind.sup = col.ind.sup, alpha.ind=alpha.ind,
          shape.ind=shape.ind, axes.linetype=axes.linetype,
          habillage=habillage, addEllipses=addEllipses, ellipse.level=ellipse.level,
          select.ind = select.ind, jitter = jitter, repel = repel)
    
  # geometry for variable
  geom2 <- geom
  if(arrows[2]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  
  if(!hide$var){
    p <-.ggscatter(p = p, data = var, x = 'x', y = 'y', 
                   col=col.var,  alpha = alpha.var, 
                   alpha.limits = alpha.limits, repel = repel,
                   geom =  geom2, shape = shape.var,
                   lab = lab$var, labelsize = labelsize, pointsize = pointsize, jitter = jitter)
  }
  
  # Add supplementary qualitative variable categories
  # Available only in FactoMineR
  if(inherits(X, c('MCA')) & !hide$quali.sup ){
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
  title2 <- title
  p+labs(title=title2)
}

#' @rdname fviz_mca
#' @export
fviz_mca <- function(X, ...){
  fviz_mca_biplot(X, ...)
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
