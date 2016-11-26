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
#' @inheritParams ggpubr::ggpar
#'@param ... Arguments to be passed to the function fviz_mca_biplot() and ggpubr::ggscatter().
#'@param map character string specifying the map type. Allowed options include: 
#'  "symmetric", "rowprincipal", "colprincipal", "symbiplot", "rowgab", 
#'  "colgab", "rowgreen" and "colgreen". See details
#'@param arrows Vector of two logicals specifying if the plot should contain 
#'  points (FALSE, default) or arrows (TRUE). First value sets the rows and the 
#'  second value sets the columns.
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
                         labelsize=4, pointsize = 1.5, repel = FALSE,
                         habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                         ellipse.type = "norm", ellipse.alpha = 0.1,
                         col.ind = "blue", col.ind.sup = "darkblue", alpha.ind =1,
                         shape.ind = 19, gradient.cols = NULL, axes.linetype = "dashed",
                         select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                         map ="symmetric", title = "Individuals factor map - MCA",
                         ggtheme = theme_grey(),
                         ...)
{
  
  # Deprecated arguments: jitter
  extra_args <- list(...)
  if(!is.null(extra_args$jitter)) repel <- .facto_dep("jitter", "repel", TRUE)
  
  .check_axes(axes, .length = 2)
  if(length(intersect(geom, c("point", "text", "arrow"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  
  # Data frame to be used for plotting
  ind <- facto_summarize(X, element = "ind", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(ind)[2:3] <-  c("x", "y")
  
  # Scale ind coords according to the type of map
  ind <- .scale_ca(ind, res.ca = X,  element = "ind", 
                   type = map, axes = axes)
  
  # Elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  # Qualitative variable is used to color the individuals by groups
  if(habillage[1] !="none"){
    dd <- .add_ind_groups(X, ind, habillage)
    ind <- dd$ind
    col.ind <- dd$name.quali
    if(missing(shape.ind)) shape.ind <- dd$name.quali
  }
  
  # Selection
  ind.all <- ind
  if(!is.null(select.ind)) ind <- .select(ind, select.ind)
  
  # Plot
  #%%%%%%%%%%%%%%%%%%%
  point <- ("point" %in% geom) & (!hide$ind) # to show individuals point should be TRUE
  mean.point <- (habillage[1] !="none") & ("point" %in% geom) & (!hide$quali) # to show mean point
  
  ind_label <- NULL
  if(lab$ind & "text" %in% geom & !hide$ind) ind_label <- "name"
  
  p <- ggpubr::ggscatter(data = ind, x = "x", y = "y",
                         color = col.ind, alpha = alpha.ind, shape = shape.ind, 
                         point = point, size = pointsize, mean.point = mean.point,
                         label = ind_label, font.label = labelsize*3, repel = repel,
                         ellipse = addEllipses, ellipse.type = ellipse.type,
                         ellipse.alpha = ellipse.alpha, ellipse.level = ellipse.level,
                         main = title,
                         ggtheme = ggtheme, ...
  )
  if(alpha.ind %in% c("cos2","contrib", "coord", "x", "y"))
    p <- p + scale_alpha(limits = range(ind.all[, alpha.ind]))
  if(!is.null(gradient.cols) & col.ind %in% c("cos2","contrib", "coord", "x", "y"))
    p <- p + ggpubr:::.gradient_col(gradient.cols)
  if(is.null(extra_args$legend)) p <- p + theme(legend.position = "right" )

  # Add supplementary quantitative individuals
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Available only for FactoMineR
  if(inherits(X, 'MCA') & !hide$ind.sup){
    p <- .add_supp (p, X, element = "ind.sup", axes = axes, select = select.ind,
                    geom = geom, color = col.ind.sup, shape = 19, pointsize = pointsize,
                    labelsize = labelsize, addlabel = (lab$ind.sup & "text" %in% geom),
                    repel = repel
    )
  }
  
  p <- .fviz_finish(p, X, axes, axes.linetype, ...) +
    labs(title = title)

  p
}


#' @rdname fviz_mca
#' @export 
fviz_mca_var <- function(X, axes=c(1,2), geom=c("point", "text"), label="all",  invisible ="none",
                         labelsize=4, pointsize = 2, col.var="red", alpha.var=1, shape.var = 17, 
                         col.quanti.sup="blue",  col.quali.sup = "darkgreen", repel = FALSE, 
                         gradient.cols = NULL,
                         title = "Variable categories- MCA",
                         select.var = list(name = NULL, cos2 = NULL, contrib = NULL), axes.linetype = "dashed",
                         map ="symmetric", 
                         ggtheme = theme_grey(), ...
                         )
{
  
  # Deprecated arguments
  extra_args <- list(...)
  if(!is.null(extra_args$jitter)) repel <- .facto_dep("jitter", "repel", TRUE)
  
  .check_axes(axes, .length = 2)
  
  # Data frame to be used for plotting
  var <- facto_summarize(X, element = "var", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(var)[2:3] <-  c("x", "y")
  
  # Scale coords according to the type of map
  var <- .scale_ca(var, res.ca = X,  element = "var", 
                   type = map, axes = axes)
  
  # Elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  # Selection
  var.all <- var
  if(!is.null(select.var)) var <- .select(var, select.var)
  
  # Plot
  #%%%%%%%%%%%%%%%%%%%%%%%
  point <- ("point" %in% geom) & (!hide$var) # to show variable point should be TRUE
  var_label <- NULL
  if(lab$var & "text" %in% geom & !hide$var) var_label <- "name"
  # Draw variables
  p <- ggpubr::ggscatter(data = var, x = 'x', y = 'y', 
                         color = col.var,  alpha = alpha.var, 
                         size = pointsize, shape = shape.var,
                         point = point, label = var_label, 
                         font.label = labelsize*3, repel = repel,
                         ggtheme = ggtheme, main = title, ...)
  
  
  if(!is.null(gradient.cols) & col.var %in% c("cos2","contrib", "coord", "x", "y"))
    p <- p + ggpubr:::.gradient_col(gradient.cols)
  if(is.null(extra_args$legend)) p <- p + theme(legend.position = "right" )
  if(alpha.var %in% c("cos2","contrib", "coord", "x", "y"))
    p <- p + scale_alpha(limits = range(var.all[, alpha.var]))
  
  
  # Add supplementary qualitative variable categories
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Available only in FactoMineR
  if(inherits(X, 'MCA') & !hide$quali.sup ){
    p <- .add_supp (p, X, element = "quali.sup", axes = axes, select = select.var,
                    geom = geom, color = col.quali.sup, shape = shape.var,
                    labelsize = labelsize, addlabel = (lab$quali.sup & "text" %in% geom),
                    repel = repel, pointsize = pointsize
    )
  }
  
  p <- .fviz_finish(p, X, axes, axes.linetype, ...) +
    labs(title = title)
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
                  ggtheme = theme_grey(),
                   ...)
{
  
  .check_axes(axes, .length = 2)
  
  # Individuals
  geom2 <- geom
  if(arrows[1]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  p <- fviz_mca_ind(X,  axes = axes, geom = geom2, label = label, invisible=invisible,
                    labelsize=labelsize, pointsize = pointsize,
                    col.ind = col.ind, col.ind.sup = col.ind.sup, alpha.ind=alpha.ind,
                    shape.ind=shape.ind, axes.linetype=axes.linetype,
                    habillage=habillage, addEllipses=addEllipses, ellipse.level=ellipse.level,
                    select.ind = select.ind,  repel = repel, ggtheme = ggtheme, ...)
    
  # Variable
  geom2 <- geom
  if(arrows[2]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  # Add variables
  p <- fviz_mca_var(X, axes = axes, geom =  geom2, repel = repel, 
                    label = label, invisible = invisible,
                    labelsize = labelsize, pointsize = pointsize, 
                    col.var = col.var, alpha.var = alpha.var, select.var = select.var,
                    shape.var = shape.var, axes.linetype=axes.linetype,
                    ggp = p, ggtheme = ggtheme)
  
  p+labs(title=title)
}

#' @rdname fviz_mca
#' @export
fviz_mca <- function(X, ...){
  fviz_mca_biplot(X, ...)
}


