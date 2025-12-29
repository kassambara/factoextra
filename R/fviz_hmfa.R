#' @include get_hmfa.R
NULL
#'Visualize Hierarchical Multiple Factor Analysis
#'
#'@description Hierarchical Multiple Factor Analysis (HMFA) is, an extension of
#'  MFA, used in a situation where the data are organized into a hierarchical
#'  structure.  fviz_hmfa() provides ggplot2-based elegant visualization of HMFA
#'  outputs from the R function: HMFA [FactoMineR].\cr\cr \itemize{
#'  \item{fviz_hmfa_ind(): Graph of individuals} \item{fviz_hmfa_var(): Graph of
#'  variables} \item{fviz_hmfa_quali_biplot(): Biplot of individuals and
#'  qualitative variables} \item{fviz_hmfa(): An alias of fviz_hmfa_ind()} }
#'@param X an object of class HMFA [FactoMineR].
#'@inheritParams fviz_mca
#'@inheritParams fviz_pca
#'@inheritParams fviz
#'@param habillage an optional factor variable for coloring the observations by 
#'  groups. Default value is "none". If X is an HMFA object from FactoMineR 
#'  package, habillage can also specify the index of the factor variable in the 
#'  data.
#'@param col.ind,col.var color for individuals, partial individuals and 
#'  variables, respectively. Can be a continuous variable or a factor variable.
#'  Possible values include also : "cos2", "contrib", "coord", "x" or "y". In
#'  this case, the colors for individuals/variables are automatically controlled
#'  by their qualities ("cos2"), contributions ("contrib"), coordinates (x^2 +
#'  y^2 , "coord"), x values("x") or y values("y"). To use automatic coloring
#'  (by cos2, contrib, ....), make sure that habillage ="none".
#'@param col.partial color for partial individuals. By default, points are 
#'  colored according to the groups.
#'@param alpha.ind,alpha.var controls the transparency of individual, partial 
#'  individual and variable, respectively. The value can variate from 0 (total 
#'  transparency) to 1 (no transparency). Default value is 1. Possible values 
#'  include also : "cos2", "contrib", "coord", "x" or "y". In this case, the 
#'  transparency for individual/variable colors are automatically controlled by 
#'  their qualities ("cos2"), contributions ("contrib"), coordinates (x^2 + y^2 
#'  , "coord"), x values("x") or y values("y"). To use this, make sure that 
#'  habillage ="none".
#'@param shape.ind,shape.var point shapes of individuals and variables, 
#'  respectively.
#'@param group.names a vector containing the name of the groups (by default, 
#'  NULL and the group are named group.1, group.2 and so on).
#'@param node.level a single number indicating the HMFA node level to plot.
#'@param title the title of the graph
#'@param select.ind,select.var a selection of individuals and variables to be 
#'  drawn. Allowed values are NULL or a list containing the arguments name, cos2
#'  or contrib: \itemize{ \item name is a character vector containing 
#'  individuals/variables to be drawn \item cos2 if cos2 is in [0, 1], ex: 0.6, 
#'  then individuals/variables with a cos2 > 0.6 are drawn. if cos2 > 1, ex: 5, 
#'  then the top 5 individuals/variables with the highest cos2 are drawn. \item 
#'  contrib if contrib > 1, ex: 5,  then the top 5 individuals/variables with 
#'  the highest cos2 are drawn }
#'@param choice the graph to plot. Allowed values include one of c("quanti.var",
#'  "quali.var", "group") for plotting quantitative variables, qualitative 
#'  variables and group of variables, respectively.
#'@param ... Arguments to be passed to the function fviz() and ggpubr::ggpar()
#'@param partial list of the individuals for which the partial points should be
#'  drawn. (by default, partial = NULL and no partial points are drawn). Use
#'  partial = "all" to visualize partial points for all individuals.
#'@param col.var.sup color for supplementary variables.
#'@param repel a boolean, whether to use ggrepel to avoid overplotting text 
#'  labels or not.
#'@return a ggplot
#'@author Fabian Mundt \email{f.mundt@inventionate.de}
#'@author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'@references http://www.sthda.com/english/
#' @examples
#' # Hierarchical Multiple Factor Analysis
#' # ++++++++++++++++++++++++
#' # Install and load FactoMineR to compute MFA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data(wine)
#' hierar <- list(c(2,5,3,10,9,2), c(4,2))
#' res.hmfa <- HMFA(wine, H = hierar, type=c("n",rep("s",5)), graph = FALSE)
#'
#' # Graph of individuals
#' # ++++++++++++++++++++
#' # Color of individuals: col.ind = "#2E9FDF"
#' # Use repel = TRUE to avoid overplotting (slow if many points)
#' fviz_hmfa_ind(res.hmfa, repel = TRUE, col.ind = "#2E9FDF")
#' 
#' # Color individuals by groups, add concentration ellipses
#' # Remove labels: label = "none".
#' # Change color palette to "jco". See ?ggpubr::ggpar
#' grp <- as.factor(wine[,1])
#' p <- fviz_hmfa_ind(res.hmfa, label="none", habillage=grp,
#'        addEllipses=TRUE, palette = "jco")
#' print(p)
#'  
#'
#' # Graph of variables
#' # ++++++++++++++++++++++++++++++++++++++++
#' # Quantitative variables
#' fviz_hmfa_var(res.hmfa, "quanti.var")
#' # Graph of categorical variable categories
#' fviz_hmfa_var(res.hmfa, "quali.var")
#' # Groups of variables (correlation square)
#' fviz_hmfa_var(res.hmfa, "group")
#'
#'
#' # Biplot of categorical variable categories and individuals
#' # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' fviz_hmfa_quali_biplot(res.hmfa)
#'     
#' # Graph of partial individuals (starplot)
#' # +++++++++++++++++++++++++++++++++++++++
#' fviz_hmfa_ind(res.hmfa, partial = "all", palette = "Dark2")
#'  
#'
#'@name fviz_hmfa
#'@rdname fviz_hmfa
#'@export
fviz_hmfa_ind <- function(X,  axes = c(1,2), geom=c("point", "text"), repel = FALSE,
                          habillage="none", addEllipses=FALSE, 
                          shape.ind = 19, col.ind = "blue", col.ind.sup = "darkblue",
                          alpha.ind = 1,
                          select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                          partial = NULL, col.partial = "group", 
                          group.names = NULL, node.level = 1,  ...)
{
  extra_args <- list(...)
  
  p <- fviz (X, element = "ind", axes = axes, geom = geom, habillage = habillage, 
             addEllipses = addEllipses, pointshape = shape.ind,
             color = col.ind, alpha = alpha.ind,
             shape.sup = shape.ind, col.row.sup = col.ind.sup,
             select = select.ind, repel = repel, ...)
  
  # Add partial points
  if(!is.null(partial)){
    invisible <- ifelse(is.null(extra_args$invisible), "none", extra_args$invisible)
    if(!(partial[1] %in% c("All", "all"))) select.partial = list(name = partial)
    else select.partial <- NULL
    if(col.partial %in% c("group", "groups")) col.partial <- "group.name"
    
    # Data for individuals
    ind.sum <- facto_summarize(X, element = "ind",
                               result = c("coord", "contrib", "cos2"), axes = axes)
    ind <- ind.sum
    colnames(ind)[2:3] <-  c("x", "y")
    # partial points
    if(is.null(group.names)) group.names <- rownames(X$group$coord[[node.level]])
    ind.partial <- facto_summarize(X, element = "partial.node", node.level = node.level, 
                                   group.names = group.names, result = c("coord.node.partial"), 
                                   axes = axes)
   
    
    colnames(ind.partial)[3:4] <-  c("x.partial", "y.partial")
    ind.partial <- merge(ind, ind.partial, by = "name")
    # Selection
    ind.all <- ind
    if(!is.null(select.ind)) ind <- .select(ind, select.ind)
    if(!is.null(select.partial)) {
      if(nrow(ind) != nrow(ind.all)) warning("You've already selected individuals. Partial points are only calculated for them.")
      ind.partial <-  ind.partial[ind.partial$name %in% .select(ind, select.partial)$name, , drop = FALSE]
    }
    # elements to be hidden
    hide <- .hide(invisible)
    # Plot
    if(!hide$ind & "point" %in% geom) {
      # Partial point
      p <- p + ggpubr::geom_exec(geom_point, data = ind.partial,
                                 x = "x.partial", y = "y.partial", 
                                 colour = col.partial,
                                 shape = shape.ind, size = 1)
      # Partial segments
      # FIX: ggplot2 3.4.0+ deprecation - size replaced with linewidth for geom_segment
      p <- p + ggpubr::geom_exec(geom_segment, data = ind.partial,
                                 x = "x", y = "y", xend = 'x.partial', yend = 'y.partial',
                                 linetype = "group.name", colour = col.partial, linewidth = 0.5)
    }
    # Edit plot title and legend title
    p <- p  + labs(colour = "Groups", linetype = "Groups")
  }
  
  p
}


#' @rdname fviz_hmfa
#' @export
fviz_hmfa_var <- function(X, choice = c("quanti.var", "quali.var", "group"), axes=c(1,2), geom=c("point", "text"), repel = FALSE, 
                          col.var = "red", alpha.var = 1, shape.var = 17, col.var.sup = "darkgreen", 
                          select.var = list(name = NULL, cos2 = NULL, contrib = NULL), ...)
{
  
  choice <- match.arg(choice)
  if(choice == "quanti.var") {
    if(missing(geom)) geom <- c("arrow", "text")
  }
  
  fviz (X, element = choice, axes = axes, geom = geom,
        color = col.var, alpha = alpha.var,  pointshape = shape.var, 
        shape.sup = shape.var, col.col.sup = col.var.sup, 
        select = select.var, repel = repel,  ...)
}


#' @rdname fviz_hmfa
#' @export
fviz_hmfa_quali_biplot <- function(X,  axes = c(1,2), geom=c("point", "text"), repel = FALSE, 
                                   habillage = "none",
                                   title = "Biplot of individuals and qualitative variables - HMFA",  ...)
{
  # Individuals
  p <- fviz_hmfa_ind(X,  axes = axes, geom = geom, repel = repel, habillage = habillage,  ...)
  
  # Variable
  # Add variables
  p <- fviz_hmfa_var(X, "quali.var", axes = axes, geom =  geom, repel = repel, 
                    ggp = p, ...)
  
  p+labs(title=title)
}

#' @rdname fviz_hmfa
#' @export
fviz_hmfa <- function(X, ...){
  # fviz_hmfa_ind_starplot(X, ...)
  fviz_hmfa_ind(X, ...)
}

