#' @include get_mfa.R
NULL
#'Visualize Multiple Factor Analysis
#'
#'@description Multiple factor analysis (MFA) is used to analyze a data set in 
#'  which individuals are described by several sets of variables (quantitative 
#'  and/or qualitative) structured into groups. fviz_mfa() provides
#'  ggplot2-based elegant visualization of MFA outputs from the R function: MFA
#'  [FactoMineR].\cr\cr \itemize{ \item{fviz_mfa_ind(): Graph of individuals} 
#'  \item{fviz_mfa_var(): Graph of variables} \item{fviz_mfa_axes(): Graph of 
#'  partial axes} \item{fviz_mfa(): An alias of fviz_mfa_ind(res.mfa, partial = 
#'  "all")} \item{fviz_mfa_quali_biplot(): Biplot of individuals and qualitative
#'  variables} }
#'  
#'@param X an object of class MFA [FactoMineR].
#'@inheritParams fviz_mca
#'@inheritParams fviz_pca
#'@inheritParams fviz
#'@inheritParams ggpubr::ggpar
#'@param choice the graph to plot. Allowed values include one of c("quanti.var",
#'  "quali.var", "group") for plotting quantitative variables, qualitative 
#'  variables and group of variables, respectively.
#'@param habillage an optional factor variable for coloring the observations by 
#'  groups. Default value is "none". If X is an MFA object from FactoMineR 
#'  package, habillage can also specify the index of the factor variable in the 
#'  data.
#'@param col.ind,col.var,col.axes color for individuals, variables and col.axes 
#'  respectively. Can be a continuous variable or a factor variable. Possible
#'  values include also : "cos2", "contrib", "coord", "x" or "y". In this case,
#'  the colors for individuals/variables are automatically controlled by their
#'  qualities ("cos2"), contributions ("contrib"), coordinates (x^2 + y^2 ,
#'  "coord"), x values("x") or y values("y"). To use automatic coloring (by
#'  cos2, contrib, ....), make sure that habillage ="none".
#'@param col.partial color for partial individuals. By default, points are 
#'  colored according to the groups.
#'@param col.var.sup color for supplementary variables.
#'@param alpha.ind,alpha.var,alpha.axes controls the transparency of individual,
#'  variable, group and axes colors, respectively. The value can variate from 0 
#'  (total transparency) to 1 (no transparency). Default value is 1. Possible 
#'  values include also : "cos2", "contrib", "coord", "x" or "y". In this case, 
#'  the transparency for individual/variable colors are automatically controlled
#'  by their qualities ("cos2"), contributions ("contrib"), coordinates (x^2 + 
#'  y^2 , "coord"), x values("x") or y values("y"). To use this, make sure that 
#'  habillage ="none".
#'@param shape.ind,shape.var point shapes of individuals, variables, groups and 
#'  axes
#'@param col.quali.var.sup color for supplementary qualitative variables. 
#'  Default is "black".
#'@param title the title of the graph
#'@param select.ind,select.var,select.axes a selection of individuals/partial 
#'  individuals/ variables/groups/axes to be drawn. Allowed values are NULL or a
#'  list containing the arguments name, cos2 or contrib: \itemize{ \item name is
#'  a character vector containing individuals/variables to be drawn \item cos2 
#'  if cos2 is in [0, 1], ex: 0.6, then individuals/variables with a cos2 > 0.6 
#'  are drawn. if cos2 > 1, ex: 5, then the top 5 individuals/variables with the
#'  highest cos2 are drawn. \item contrib if contrib > 1, ex: 5,  then the top 5
#'  individuals/variables with the highest cos2 are drawn }
#'@param ... Arguments to be passed to the function fviz()
#'@param repel a boolean, whether to use ggrepel to avoid overplotting text 
#'  labels or not.
#'@param partial list of the individuals for which the partial points should be
#'  drawn. (by default, partial = NULL and no partial points are drawn). Use
#'  partial = "all" to visualize partial points for all individuals.
#'  
#'@return a ggplot2 plot
#'@author Fabian Mundt \email{f.mundt@inventionate.de}
#'@author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'@references http://www.sthda.com/english/
#' @examples
#' # Compute Multiple Factor Analysis
#' library("FactoMineR")
#' data(wine)
#' res.mfa <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
#'                ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
#'                num.group.sup=c(1,6), graph=FALSE)
#'                
#' # Eigenvalues/variances of dimensions
#' fviz_screeplot(res.mfa)
#' # Group of variables
#' fviz_mfa_var(res.mfa, "group")
#' # Quantitative variables
#' fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
#'   col.var.sup = "violet", repel = TRUE)
#' # Graph of individuals colored by cos2
#' fviz_mfa_ind(res.mfa, col.ind = "cos2", 
#'   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#'   repel = TRUE)
#' # Partial individuals
#' fviz_mfa_ind(res.mfa, partial = "all") 
#' # Partial axes
#' fviz_mfa_axes(res.mfa)
#' 
#'
#' # Graph of categorical variable categories
#' # ++++++++++++++++++++++++++++++++++++++++
#' data(poison)
#' res.mfa <- MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
#'                name.group=c("desc","desc2","symptom","eat"),
#'                num.group.sup=1:2, graph=FALSE)
#'
#' # Plot of qualitative variables
#' fviz_mfa_var(res.mfa, "quali.var")
#'  
#'  
#' 
#' # Biplot of categorical variable categories and individuals
#' # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'  # Use repel = TRUE to avoid overplotting
#' grp <- as.factor(poison[, "Vomiting"])
#' fviz_mfa_quali_biplot(res.mfa, repel = FALSE, col.var = "#E7B800",
#'    habillage = grp, addEllipses = TRUE, ellipse.level = 0.95)
#' 
#'@name fviz_mfa
#'@rdname fviz_mfa
#'@export
fviz_mfa_ind <- function(X,  axes = c(1,2), geom=c("point", "text"), repel = FALSE,
                         habillage = "none", palette = NULL, addEllipses = FALSE, 
                         col.ind = "blue", col.ind.sup = "darkblue", alpha.ind = 1,
                         shape.ind = 19, col.quali.var.sup = "black",
                         select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                         partial = NULL, col.partial = "group",
                         ...)
{
  extra_args <- list(...)
  
  if(col.ind %in% c("cos2","contrib", "coord")) partial = NULL
  if(!is.null(partial) & missing(col.ind)) col.ind <- "black"
  
  p <- fviz (X, element = "ind", axes = axes, geom = geom, habillage = habillage, 
        addEllipses = addEllipses, palette = palette, pointshape = shape.ind,
        color = col.ind, alpha = alpha.ind,
        shape.sup = shape.ind, col.row.sup = col.ind.sup,
        select = select.ind, repel = repel, ...)
  
  # Add supplementary qualitative variables if they exist
  show.quali.var <- !("quali.var" %in% extra_args$invisible) & is.null(partial)
  is_habillage <- habillage[1] != "none" 
  if(!is.null(X$quali.var.sup) & show.quali.var){
    if(!(col.ind %in% c("cos2", "contrib")) & !is_habillage) col.quali.var.sup  = "quali.sup"
    quali.sup <- .get_supp(X, element = "quali.var.sup", axes = axes)
    quali.sup$quali.sup <- .get_quali_var_sup_names(X)
    colnames(quali.sup)[2:3] <-  c("x", "y")
    p <- fviz_add(p, df = quali.sup[, -1], 
                  geom = c("point", "text"), color = col.quali.var.sup, shape = 0, 
                  repel = repel, addlabel = TRUE)
    
  }
  
  # Add partial points
  if(!is.null(partial)){
    invisible <- ifelse(is.null(extra_args$invisible), "none", extra_args$invisible)
    if(!(partial[1] %in% c("All", "all"))) select.partial = list(name = partial)
    else select.partial <- NULL
    if(col.partial %in% c("group", "groups")) col.partial <- "group.name"
    
    # Data for individuals
    ind.sum <- facto_summarize(X, element = "ind",
                               result = c("coord", "contrib", "cos2", "coord.partial"), axes = axes)
    ind <- ind.sum$res
    colnames(ind)[2:3] <-  c("x", "y")
    # partial points
    ind.partial <- ind.sum$res.partial
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



#' @rdname fviz_mfa
#' @export
fviz_mfa_quali_biplot <- function(X,  axes = c(1,2), geom=c("point", "text"), repel = repel,
                                  title = "Biplot of individuals and qualitative variables - MFA", ...)
{
  # Individuals
  p <- fviz_mfa_ind(X,  axes = axes, geom = geom, repel = repel,  ...)
  
  # Variable
  # Add variables
  p <- fviz_mfa_var(X, "quali.var", axes = axes, geom =  geom, repel = repel, 
                  ggp = p, ...)
  
  p+labs(title=title)
}




#' @rdname fviz_mfa
#' @export
fviz_mfa_var <- function(X, choice = c("quanti.var", "group", "quali.var"), axes = c(1,2), 
                         geom = c("point", "text"), repel = FALSE, habillage = "none",
                         col.var ="red", alpha.var=1, shape.var = 17, 
                         col.var.sup = "darkgreen", palette = NULL,
                         select.var = list(name = NULL, cos2 = NULL, contrib = NULL), ...)
{
  
  extra_args <- list(...)
  if(missing(choice) & !is.null(extra_args$choix)) choice <- extra_args$choix
  choice <- match.arg(choice)
 
 
  # Define habillage if quanti.var. Quantitative variables are colored by groups
  if(choice == "quanti.var") {
    .check_if_quanti_exists(X)
    if(missing(geom)) geom <- c("arrow", "text")
    group <- data.frame(name = rownames(X$group$Lg[-nrow(X$group$Lg),,drop=FALSE]),
                        nvar = X$call$group, type = X$call$type, stringsAsFactors = TRUE)
    is.group.sup <- !is.null(X$call$num.group.sup)
    if(is.group.sup) group <- group[-X$call$num.group.sup, , drop = FALSE]
    group <- subset(group, group$type == "c")
    habillage <- rep(group$name, group$nvar)
  }
  else if(choice== "quali.var"){
    .check_if_quali_exists(X)
  }
  if(!missing(col.var)) habillage = "none"
  # Main plot
  fviz (X, element = choice, axes = axes, geom = geom,
        color = col.var, alpha = alpha.var,  pointshape = shape.var, 
        shape.sup = shape.var, col.col.sup = col.var.sup, 
        habillage = habillage,  palette = palette,
        select = select.var, repel = repel,  ...)
}



#' @rdname fviz_mfa
#' @export
fviz_mfa_axes <- function(X,  axes = c(1,2), geom=c("arrow", "text"),
                          col.axes = NULL, alpha.axes=1, col.circle ="grey70",
                          select.axes = list(name = NULL, contrib = NULL),
                          repel = FALSE, ...)
{
  
  # Color partial axes by groups
  habillage <- "none"
  if(is.null(col.axes)){
    axes.names <- rownames(X$partial.axes$coord)
    axes.groups <- sapply(axes.names, 
                          function(x) gsub("^Dim[0-9]+\\.",  "", x, perl = TRUE)
                          )
    habillage <- as.factor(axes.groups)
  }
  fviz (X, element = "partial.axes", axes = axes, geom = geom,
        color = col.axes, alpha = alpha.axes,  select = select.axes,
        repel = repel, col.circle = col.circle, habillage = habillage, ...)
}


#' @rdname fviz_mfa
#' @export
fviz_mfa <- function(X, partial = "all", ...){
  fviz_mfa_ind(X, partial = partial, ...)
}



#+++++++++++++++++++++
# Helper functions
#+++++++++++++++++++++

# Check if there are quantitative variables.
.check_if_quanti_exists <- function(X){
  if(!is.null(X$call$num.group.sup))
    group.all <- X$call$type[-X$call$num.group.sup]
  else
    group.all <- X$call$type
  if(!c("c" %in% group.all) )
    if(!c("s" %in% group.all)) 
      stop("There are no quantitative variables to plot.")
  
}

# Check if qualitative variables exists
.check_if_quali_exists <- function (X){
  # Check if there are qualitative variables.
  if(!is.null(X$call$num.group.sup))
    group.all <- X$call$type[-X$call$num.group.sup]
  else
    group.all <- X$call$type
  if(!("n" %in% group.all))
    stop("There are no qualitative variables to plot.")
}


