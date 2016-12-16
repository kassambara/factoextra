#' @include get_mfa.R
NULL
#' Visualize Multiple Factor Analysis
#'
#' @description
#' Graph of individuals/quantitative variables/qualitative variables/group/partial axes from the output of Multiple Factor Analysis (MFA).\cr\cr
#' \itemize{
#' \item{fviz_mfa_ind(): Graph of individuals}
#' \item{fviz_mfa_ind_starplot(): Star graph of individuals (draws partial points). 
#' Deprecated function. It will be removed in the next version. 
#' Use fviz_mfa_ind(res.mfa, partial = "All") instead.}
#' \item{fviz_mfa_quanti_var(): Graph of quantitative variables}
#' \item{fviz_mfa_quali_var(): Graph of qualitative variables}
#' \item{fviz_mfa_quali_biplot(): Biplot of individuals and qualitative variables}
#' \item{fviz_mfa_group(): Graph of the groups representation}
#' \item{fviz_mfa_axes(): Graph of partial axes}
#' \item{fviz_mfa(): An alias of fviz_mfa_ind_starplot()}
#' }
#' @param X an object of class MFA [FactoMineR].
#' @inheritParams fviz_mca
#' @inheritParams fviz_pca
#' @inheritParams fviz
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
#' @param axes.linetype linetype of x and y axes.
#' @param col.ind,col.partial,col.var,col.axes color for individuals, partial individuals, variables, 
#' groups and axes, respectively.
#'  Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the colors for individuals/variables are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2 + y^2 , "coord"), x values("x") or y values("y").
#'  To use automatic coloring (by cos2, contrib, ....), make sure that habillage ="none".
#' @param col.var.sup color for supplementary variables.
#' @param alpha.ind,alpha.var,alpha.axes controls the transparency of
#'  individual, variable, group and axes colors, respectively.
#' The value can variate from 0 (total transparency) to 1 (no transparency).
#' Default value is 1. Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the transparency for individual/variable colors are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2 + y^2 , "coord"), x values("x") or y values("y").
#'  To use this, make sure that habillage ="none".
#' @param shape.ind,shape.var point shapes of individuals, variables, groups and axes
#' @param col.quali.var.sup color for supplementary qualitative variables. Default is "black".
#' @param title the title of the graph
#' @param col.quanti.sup,col.quali.sup a color for the quantitative/qualitative supplementary variables.
#' @param select.ind,select.var,select.axes a selection of individuals/partial individuals/
#' variables/groups/axes to be drawn.
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
#' @param repel a boolean, whether to use ggrepel to avoid overplotting text labels or not.
#' @param jitter a parameter used to jitter the points in order to reduce overplotting. 
#' It's a list containing the objects what, width and height (i.e jitter = list(what, width, height)).
#' \itemize{
#' \item what: the element to be jittered. Possible values are "point" or "p"; "label" or "l"; "both" or "b"
#' \item width: degree of jitter in x direction
#' \item height: degree of jitter in y direction
#' }
#' @param partial list of the individuals for which the partial points should be drawn. 
#' (by default, partial = NULL and no partial points are drawn). 
#' Use partial = "All" to visualize partial points for all individuals.
#'
#' @return a ggplot2 plot
#' @author Fabian Mundt \email{f.mundt@inventionate.de}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
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
#' @name fviz_mfa
#' @rdname fviz_mfa
#' @export
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
  is_habillage <- habillage != "none" 
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
      p <- p + ggpubr::geom_exec(geom_segment, data = ind.partial,
                                 x = "x", y = "y", xend = 'x.partial', yend = 'y.partial',
                                 linetype = "group.name", colour = col.partial, size = 0.5)
    }
    # Edit plot title and legend title
    p <- p  + labs(colour = "Groups", linetype = "Groups")
  }
  
  p
  
}

#' @rdname fviz_mfa
#' @export
fviz_mfa_ind_starplot <- function(X,  partial = "All",  ...){
  warning("This function is deprecated. ", 
          "It will be removed in the next version. ",
           "Use fviz_mfa_ind(res.mfa, partial = 'All') instead.")
  fviz_mfa_ind (X, partial = partial, ...)
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
                                  axes.linetype = "dashed", title = "MFA factor map - Biplot",# map ="symmetric", 
                                  arrows = c(FALSE, FALSE), repel = FALSE,
                                  jitter = list(what = "label", width = NULL, height = NULL), ...)
{
  
  # Check if there are qualitative variables.
  if(!is.null(X$call$num.group.sup))
    group.all <- X$call$type[-X$call$num.group.sup]
  else
    group.all <- X$call$type
  if(!("n" %in% group.all))
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
                    shape.ind=shape.ind, repel = repel, axes.linetype=axes.linetype,
                    habillage=habillage, addEllipses=addEllipses, ellipse.level=ellipse.level,
                    select.ind = select.ind, jitter = jitter)
  
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
  if(inherits(X, 'MFA') & !hide$quali.sup ){
    quali_sup <- .get_supp(X, element = "quali.sup", axes = axes,
                           select = select.var)
    if(!is.null(quali_sup)){
      colnames(quali_sup)[2:3] <- c("x", "y")
      # quali_sup <- .scale_ca(quali_sup, res.ca = X,  element = "quali.sup",
      #                      type = map, axes = axes)
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




#' @rdname fviz_mfa
#' @export
fviz_mfa_var <- function(X, choice = c("quanti.var", "group", "quali.var"), axes = c(1,2), 
                         geom = c("point", "text"), repel = FALSE,
                         col.var ="red", alpha.var=1, shape.var = 17, 
                         col.var.sup = "darkgreen", palette = NULL,
                         select.var = list(name = NULL, cos2 = NULL, contrib = NULL), ...)
{
  
  extra_args <- list(...)
  if(missing(choice) & !is.null(extra_args$choix)) choice <- extra_args$choix
  choice <- match.arg(choice)
 
 
  # Define habillage if quanti.var. Quantitative variables are colored by groups
  habillage <- "none"
  if(choice == "quanti.var") {
    .check_if_quanti_exists(X)
    if(missing(geom)) geom <- c("arrow", "text")
    group <- data.frame(name = rownames(X$group$Lg[-nrow(X$group$Lg),,drop=FALSE]),
                        nvar = X$call$group, type = X$call$type)
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
fviz_mfa_group <- function(X,  ...){
  warning("Deprecated function. Use fviz_mfa_var(res.mfa, 'group') instead.")
  fviz_mfa_var(X, choice = "group", ...)
}

#' @rdname fviz_mfa
#' @export
fviz_mfa_quanti_var <- function(X, ...){
  warning("Deprecated function. Use fviz_mfa_var(res.mfa, 'quanti.var') instead.")
  fviz_mfa_var(X, choice = "quanti.var", ...)
}


#' @rdname fviz_mfa
#' @export
fviz_mfa_quali_var <- function(X, ...){
  warning("Deprecated function. Use fviz_mfa_var(res.mfa, 'quali.var') instead.")
  fviz_mfa_var(X, choice = "quali.var", ...)
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
fviz_mfa <- function(X, ...){
  fviz_mfa_ind_starplot(X, ...)
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


