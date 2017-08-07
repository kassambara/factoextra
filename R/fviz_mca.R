#' @include utilities.R get_mca.R
NULL
#'Visualize Multiple Correspondence Analysis
#'
#'@description Multiple Correspondence Analysis (MCA) is an extension of simple 
#'  CA to analyse a data table containing more than two categorical variables. 
#'  fviz_mca() provides ggplot2-based elegant visualization of MCA outputs from 
#'  the R functions: MCA [in FactoMineR], acm [in ade4], and expOutput/epMCA [in
#'  ExPosition]. Read more: 
#'  \href{http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining}{Multiple
#'   Correspondence Analysis Essentials.}
#'  
#'  \itemize{ \item{fviz_mca_ind(): Graph of individuals} \item{fviz_mca_var(): 
#'  Graph of variables} \item{fviz_mca_biplot(): Biplot of individuals and 
#'  variables} \item{fviz_mca(): An alias of fviz_mca_biplot()}}
#'@param X an object of class MCA [FactoMineR], acm [ade4] and expOutput/epMCA
#'  [ExPosition].
#'@inheritParams fviz_pca
#' @param geom.ind,geom.var as \code{geom} but for individuals and variables,
#'   respectively. Default is geom.ind = c("point", "text), geom.var =
#'   c("point", "text").
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
#'@param col.ind,col.var color for individuals and variables, respectively. Can 
#'  be a continuous variable or a factor variable. Possible values include also
#'  : "cos2", "contrib", "coord", "x" or "y". In this case, the colors for
#'  individuals/variables are automatically controlled by their qualities
#'  ("cos2"), contributions ("contrib"), coordinates (x^2 + y^2 , "coord"), x
#'  values("x") or y values("y"). To use automatic coloring (by cos2, contrib,
#'  ....), make sure that habillage ="none".
#'@param alpha.ind,alpha.var controls the transparency of individual and 
#'  variable colors, respectively. The value can variate from 0 (total 
#'  transparency) to 1 (no transparency). Default value is 1. Possible values 
#'  include also : "cos2", "contrib", "coord", "x" or "y". In this case, the 
#'  transparency for individual/variable colors are automatically controlled by 
#'  their qualities ("cos2"), contributions ("contrib"), coordinates (x^2 + y^2 
#'  , "coord"), x values("x") or y values("y"). To use this, make sure that 
#'  habillage ="none".
#'@param shape.ind,shape.var point shapes of individuals and variables.
#'@param col.quanti.sup,col.quali.sup a color for the quantitative/qualitative 
#'  supplementary variables.
#'@param repel a boolean, whether to use ggrepel to avoid overplotting text 
#'  labels or not.
#'@param choice the graph to plot. Allowed values include: i) "var" and 
#'  "mca.cor" for plotting the correlation between variables and principal 
#'  dimensions; ii) "var.cat" for variable categories and iii) "quanti.sup" for
#'  the supplementary quantitative variables.
#'@param title the title of the graph
#'@param select.ind,select.var a selection of individuals/variables to be drawn.
#'  Allowed values are NULL or a list containing the arguments name, cos2 or 
#'  contrib: \itemize{ \item name is a character vector containing 
#'  individuals/variables to be drawn \item cos2 if cos2 is in [0, 1], ex: 0.6, 
#'  then individuals/variables with a cos2 > 0.6 are drawn. if cos2 > 1, ex: 5, 
#'  then the top 5 individuals/variables with the highest cos2 are drawn. \item 
#'  contrib if contrib > 1, ex: 5,  then the top 5 individuals/variables with 
#'  the highest contrib are drawn }
#'@inheritParams ggpubr::ggpar
#'@inheritParams fviz
#'@param ... Additional arguments. \itemize{ \item in fviz_mca_ind(), 
#'  fviz_mca_var() and fviz_mca_cor(): Additional arguments are passed to the 
#'  functions fviz() and ggpubr::ggpar(). \item in fviz_mca_biplot() and 
#'  fviz_mca(): Additional arguments are passed to fviz_mca_ind() and 
#'  fviz_mca_var().}
#'@param map character string specifying the map type. Allowed options include: 
#'  "symmetric", "rowprincipal", "colprincipal", "symbiplot", "rowgab", 
#'  "colgab", "rowgreen" and "colgreen". See details
#'@param arrows Vector of two logicals specifying if the plot should contain 
#'  points (FALSE, default) or arrows (TRUE). First value sets the rows and the 
#'  second value sets the columns.
#'@details The default plot of MCA is a "symmetric" plot in which both rows and 
#'  columns are in principal coordinates. In this situation, it's not possible 
#'  to interpret the distance between row points and column points. To overcome 
#'  this problem, the simplest way is to make an asymmetric plot. The argument 
#'  "map" can be used to change the plot type. For more explanation, read the 
#'  details section of fviz_ca documentation.
#'  
#'@return a ggplot
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
#' fviz_mca_ind(res.mca, col.ind = "cos2", repel = TRUE)
#'
#' \dontrun{     
#' # You can also control the transparency 
#' # of the color by the cos2
#' fviz_mca_ind(res.mca, alpha.ind="cos2") 
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
#'     scale_fill_brewer(palette="Dark2") 
#'      
#' # Change group colors manually
#' # Read more: http://www.sthda.com/english/wiki/ggplot2-colors
#' p + scale_color_manual(values=c("#999999", "#E69F00"))+
#'  scale_fill_manual(values=c("#999999", "#E69F00"))
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
#' fviz_mca_var(res.mca, col.var = "#FC4E07")
#' 
#' # Control variable colors using their contributions
#' # use repel = TRUE to avoid overplotting
#' fviz_mca_var(res.mca, col.var = "contrib",
#'   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
#'         
#'     
#' # Biplot
#' # ++++++++++++++++++++++++++
#' grp <- as.factor(poison.active[, "Vomiting"])
#' fviz_mca_biplot(res.mca, repel = TRUE, col.var = "#E7B800",
#'  habillage = grp, addEllipses = TRUE, ellipse.level = 0.95)
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
#' fviz_mca_biplot(res.mca, label ="var", col.ind="cos2")
#'        
#' # Change the color by groups, add ellipses
#' fviz_mca_biplot(res.mca, label="var", col.var ="blue",
#'    habillage=grp, addEllipses=TRUE, ellipse.level=0.95)
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
fviz_mca_ind <- function(X,  axes = c(1,2), geom=c("point", "text"), geom.ind = geom, repel = FALSE,
                         habillage = "none", palette = NULL, addEllipses = FALSE, 
                         col.ind = "blue", col.ind.sup = "darkblue", alpha.ind = 1,
                         shape.ind = 19, map ="symmetric", 
                         select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                         ...)
{
  
  fviz (X, element = "ind", axes = axes, geom = geom.ind, habillage = habillage, 
        addEllipses = addEllipses, palette = palette, pointshape = shape.ind,
        color = col.ind, alpha = alpha.ind,
        shape.sup = shape.ind, col.row.sup = col.ind.sup,
        select = select.ind,  map = map, repel = repel, ...)

  
}


#' @rdname fviz_mca
#' @export 
fviz_mca_var <- function(X, choice = c("var.cat", "mca.cor", "var", "quanti.sup"), 
                         axes=c(1,2), geom = c("point", "text"), geom.var = geom, repel = FALSE, 
                         col.var="red", alpha.var=1, shape.var = 17, col.quanti.sup = "blue", 
                         col.quali.sup = "darkgreen",  map = "symmetric", 
                         select.var = list(name = NULL, cos2 = NULL, contrib = NULL), ...)
{
  
  col.col.sup <- col.quanti.sup
  extra_args <- list(...)
  if(missing(choice) & !is.null(extra_args$choix))
    choice <- extra_args$choix
  
  # Define plot types
  choice <- match.arg(choice)
  if(choice == "var.cat") choice <- "var"
  else if(choice == "var") choice <- "mca.cor"
  # Define color for supplementary element
  if(choice == "mca.cor") col.col.sup <- col.quanti.sup 
  else if(choice == "var") col.col.sup <- col.quali.sup
  # Define geometry for supplementary quantitative variables
  if(choice == "quanti.sup") {
    if(missing(geom)) geom <- c("arrow", "text")
    if(missing(col.var)) col.var <- col.quanti.sup
  }
  fviz (X, element = choice, axes = axes, geom = geom.var,
        color = col.var, alpha = alpha.var,  pointshape = shape.var, 
        shape.sup = shape.var, col.col.sup = col.col.sup, 
        select = select.var, repel = repel,  map = map, ...)
}



#' @rdname fviz_mca
#' @export
fviz_mca_biplot <- function(X,  axes = c(1,2), geom = c("point", "text"),
                            geom.ind = geom, geom.var = geom,
                            repel = FALSE, label = "all", invisible="none",
                            habillage="none", addEllipses=FALSE, palette = NULL,
                            arrows = c(FALSE, FALSE), map ="symmetric", 
                            title = "MCA - Biplot", ...)
{
  
  # Individuals
  geom2 <- geom.ind
  if(arrows[1]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  p <- fviz_mca_ind(X,  axes = axes, geom = geom2, repel = repel,
                    label = label, invisible=invisible, habillage = habillage,
                    addEllipses = addEllipses, palette = palette,  ...)
    
  # Variable
  geom2 <- geom.var
  if(arrows[2]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  # Add variables
  p <- fviz_mca_var(X, axes = axes, geom =  geom2, repel = repel, 
                    label = label, invisible = invisible, ggp = p, ...)
  
  p+labs(title=title)
}

#' @rdname fviz_mca
#' @export
fviz_mca <- function(X, ...){
  fviz_mca_biplot(X, ...)
}



