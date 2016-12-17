#' @include get_hmfa.R
NULL
#' Visualize Hierarchical Multiple Factor Analysis
#'
#' @description
#' Graph of individuals/quantitative variables/qualitative variables/group/partial axes from the output of Hierarchical Multiple Factor Analysis (HMFA).\cr\cr
#' \itemize{
#' \item{fviz_hmfa_ind(): Graph of individuals}
#' \item{fviz_hmfa_ind_starplot(): Graph of partial individuals}
#' \item{fviz_hmfa_quanti_var(): Graph of quantitative variables}
#' \item{fviz_hmfa_quali_var(): Graph of qualitative variables}
#' \item{fviz_hmfa_quali_biplot(): Biplot of individuals and qualitative variables}
#' \item{fviz_hmfa_group(): Graph of the groups representation}
#' \item{fviz_hmfa(): An alias of fviz_hmfa_ind()}
#' }
#' @param X an object of class HMFA [FactoMineR].
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
#' @param col.ind,col.partial,col.var,col.group color for individuals, partial individuals, variables, 
#' groups and axes, respectively.
#'  Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the colors for individuals/variables are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2 + y^2 , "coord"), x values("x") or y values("y").
#'  To use automatic coloring (by cos2, contrib, ....), make sure that habillage ="none".
#' @param alpha.ind,alpha.partial,alpha.var,alpha.group controls the transparency of
#'  individual, partial individual, variable, group and axes colors, respectively.
#' The value can variate from 0 (total transparency) to 1 (no transparency).
#' Default value is 1. Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the transparency for individual/variable colors are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2 + y^2 , "coord"), x values("x") or y values("y").
#'  To use this, make sure that habillage ="none".
#' @param shape.ind,shape.var,shape.group point shapes of individuals, variables, groups and axes
#' @param col.quanti.sup,col.quali.sup,col.group.sup a color for the quantitative/qualitative supplementary variables.
#' @param group.names a vector containing the name of the groups (by default, NULL and the group are named group.1, group.2 and so on).
#' @param legend.partial.title the title of the partal groups legend.
#' @param linesize size of partial point connecting line.
#' @param node.level a single number indicating the HMFA node level to plot.
#' @param title the title of the graph
#' @param select.ind,select.partial,select.var,select.group a selection of individuals/partial individuals/
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
#'
#' @return a ggplot2 plot
#' @author Fabian Mundt \email{f.mundt@inventionate.de}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
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
#' # Default plot
#' # Color of individuals: col.ind = "#2E9FDF"
#' # Use repel = TRUE to avoid overplotting (slow if many points)
#' fviz_hmfa_ind(res.hmfa, repel = TRUE, col.ind = "#2E9FDF")
#' 
#' \dontrun{
#' # 1. Control automatically the color of individuals 
#'    # using the "cos2" or the contributions "contrib"
#'    # cos2 = the quality of the individuals on the factor map
#' # 2. To keep only point or text use geom = "point" or geom = "text".
#' # 3. Change themes: http://www.sthda.com/english/wiki/ggplot2-themes
#'
#' fviz_hmfa_ind(res.hmfa, col.ind="cos2")+
#' theme_minimal()
#' }
#' 
#' # Color individuals by groups, add concentration ellipses
#' # Remove labels: label = "none".
#' grp <- as.factor(wine[,1])
#' p <- fviz_hmfa_ind(res.hmfa, label="none", habillage=grp,
#'        addEllipses=TRUE, ellipse.level=0.95)+
#'        theme_minimal()
#' print(p)
#' 
#' \dontrun{
#' # Change group colors using RColorBrewer color palettes
#' # Read more: http://www.sthda.com/english/wiki/ggplot2-colors
#' p + scale_color_brewer(palette="Paired") +
#'     scale_fill_brewer(palette="Paired") +
#'      theme_minimal()
#' }
#'      
#' # Change group colors manually
#' # Read more: http://www.sthda.com/english/wiki/ggplot2-colors
#' p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
#'  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
#'  theme_minimal()  
#'    
#' \dontrun{
#' # Select and visualize some individuals (ind) with select.ind argument.
#'  # - ind with cos2 >= 0.1: select.ind = list(cos2 = 0.1)
#'  # - Top 20 ind according to the cos2: select.ind = list(cos2 = 20)
#'  # - Top 20 contributing individuals: select.ind = list(contrib = 20)
#'  # - Select ind by names: select.ind = list(name = c("1VAU", "1FON") )
#'  
#' # Example: Select the top 10 according to the cos2
#' fviz_hmfa_ind(res.hmfa, select.ind = list(cos2 = 100))
#' }
#' 
#'
#' # Graph of qantitative variable categories
#' # ++++++++++++++++++++++++++++++++++++++++
#' data(wine)
#' hierar <- list(c(2,5,3,10,9,2), c(4,2))
#' res.hmfa <- HMFA(wine, H = hierar, type=c("n",rep("s",5)), graph = FALSE)
#' 
#' # Plot
#' # Control variable colors using their contributions
#' fviz_hmfa_quanti_var(res.hmfa, col.var = "contrib")+
#'  scale_color_gradient2(low = "white", mid = "blue",
#'            high = "red", midpoint = 12) +
#'  theme_minimal()
#' 
#' \dontrun{
#' # Select variables with select.var argument
#'    # You can select by contrib, cos2 and name 
#'    # as previously described for ind
#' # Select the top 10 contributing variables
#' fviz_hmfa_quanti_var(res.hmfa, select.var = list(contrib = 10))
#' }
#'  
#' # Graph of categorical variable categories
#' # ++++++++++++++++++++++++++++++++++++++++
#' data(poison)
#' hierar <- list(c(2,2,5,6), c(1,3))
#' res.hmfa <- HMFA(poison, H = hierar, type=c("s","n","n","n"), graph = FALSE)
#'
#' # Default plot
#' fviz_hmfa_quali_var(res.hmfa, col.var = "contrib")+
#'  theme_minimal()
#'
#'
#' # Biplot of categorical variable categories and individuals
#' # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' grp <- as.factor(poison[, "Vomiting"])
#' # Use repel = TRUE to avoid overplotting
#' fviz_hmfa_quali_biplot(res.hmfa, col.var = "#E7B800", repel = FALSE,
#'     habillage = grp, addEllipses = TRUE)+ 
#' theme_minimal()
#' 
#'                
#' # Graph of partial individuals (starplot)
#' # +++++++++++++++++++++++++++++++++++++++
#' fviz_hmfa_ind_starplot(res.hmfa, col.partial = "group.name")+
#' scale_color_brewer(palette = "Dark2")+
#'  theme_minimal()
#'  
#'  
#' \dontrun{
#' # Select the partial points of the top 5
#' # contributing individuals
#' fviz_hmfa_ind_starplot(res.hmfa, 
#'                       select.partial = list(contrib = 2))+
#'                       theme_minimal()
#'                       
#' # Change colours of star segments
#' fviz_hmfa_ind_starplot(res.hmfa, select.partial = list(contrib = 5), 
#'                       col.partial = "group.name") +
#'                       scale_color_brewer(palette = "Dark2") +
#'                       theme_minimal()
#'  }
#'   
#'   
#' # Graph of groups (correlation square)
#' # ++++++++++++++++++++++++++++++++++++
#' fviz_hmfa_group(res.hmfa)
#'
#' @name fviz_hmfa
#' @rdname fviz_hmfa
#' @export
fviz_hmfa_ind <- function(X,  axes = c(1,2), geom=c("point", "text"), repel = FALSE,
                          habillage="none", addEllipses=FALSE, 
                          shape.ind = 19, col.ind = "blue", col.ind.sup = "darkblue",
                          alpha.ind = 1,
                          select.ind = list(name = NULL, cos2 = NULL, contrib = NULL), ...)
{
  p <- fviz (X, element = "ind", axes = axes, geom = geom, habillage = habillage, 
             addEllipses = addEllipses, pointshape = shape.ind,
             color = col.ind, alpha = alpha.ind,
             shape.sup = shape.ind, col.row.sup = col.ind.sup,
             select = select.ind, repel = repel, ...)
  
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
fviz_hmfa_quanti_var <- function(X, ...){
  warning("Deprecated function. Use fviz_hmfa_var(X, 'quanti.var') instead.")
  fviz_hmfa_var(X, "quanti.var", ...)
}


#' @rdname fviz_hmfa
#' @export
fviz_hmfa_quali_var <- function(X, ... )
{
  warning("Deprecated function. Use fviz_hmfa_var(X, 'quali.var') instead.")
  fviz_hmfa_var(X, "quali.var", ...)
}


#' @rdname fviz_hmfa
#' @export
fviz_hmfa_quali_biplot <- function(X,  axes = c(1,2), geom=c("point", "text"), repel = FALSE, 
                                   title = "Biplot of individuals and qualitative variables - HMFA",  ...)
{
  # Individuals
  p <- fviz_hmfa_ind(X,  axes = axes, geom = geom, repel = repel,  ...)
  
  # Variable
  # Add variables
  p <- fviz_hmfa_var(X, "quali.var", axes = axes, geom =  geom, repel = repel, 
                    ggp = p, ...)
  
  p+labs(title=title)
}

#' @rdname fviz_hmfa
#' @export
fviz_hmfa_ind_starplot <- function(X,  axes = c(1,2), geom=c("point", "text"), group.names = NULL,
                                  label = "all", invisible="none", legend.partial.title = NULL,
                                  labelsize=4, pointsize = 2, linesize = 0.5, repel = FALSE,
                                  habillage="none", addEllipses=FALSE, ellipse.level = 0.95,
                                  ellipse.type = "norm", ellipse.alpha = 0.1,
                                  col.ind = "black", col.ind.sup = "darkblue", col.partial = "black",
                                  alpha.ind = 1, shape.ind = 19, alpha.partial = 1, node.level = 1,
                                  select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                                  select.partial = list(name = NULL, cos2 = NULL, contrib = NULL),
                                  axes.linetype = "dashed", title = "Individuals factor map - MFA",# map ="symmetric",
                                  jitter = list(what = "label", width = NULL, height = NULL), ...)
{

  if(length(intersect(geom, c("point", "text", "arrow"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  if(length(axes) > 2) stop("axes should be of length 2")

  if(is.null(jitter$what)) jitter$what <- "label"

  if(is.null(group.names))
    group.names <- rownames(X$group$coord[[node.level]])
  
  # data frame to be used for plotting
  ind.sum <- facto_summarize(X, element = "ind",
                             result = c("coord", "contrib", "cos2"), axes = axes)
  
  ind.partial <- facto_summarize(X, element = "partial.node", node.level = node.level, 
                                 group.names = group.names, result = c("coord.node.partial"), 
                                 axes = axes)
  ind <- ind.sum
  colnames(ind)[2:3] <-  c("x", "y")
  # partial points
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
                      alpha.limits = alpha.limits, shape = shape.ind, repel = repel,
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
      if(lab$ind & "text" %in% geom) {
        if(repel)
          p <- p + ggrepel::geom_text_repel(data = label_coord,
                                            aes_string('x', 'y', label = 'name',
                                                       color=name.quali, shape = name.quali),  size = labelsize)
        else
          p <- p + geom_text(data = label_coord,
                             aes_string('x', 'y', label = 'name',
                                        color=name.quali, shape = name.quali),  size = labelsize)
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
                             label=rownames(coord_quali.sup), size=labelsize)
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
  if(inherits(X, 'MFA') & !hide$ind.sup){
    ind_sup <- .get_supp(X, element = "ind.sup", axes = axes,
                         select = select.ind)
    if(!is.null(ind_sup)) {
      colnames(ind_sup)[2:3] <-  c("x", "y")
      # ind_sup <- .scale_ca(ind_sup, res.ca = X,  element = "ind.sup",
      #                     type = map, axes = axes)
    }
    if(!is.null(ind_sup)){
      p <- fviz_add(p, df = ind_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.ind.sup, shape = 19, pointsize = pointsize,
                    labelsize = labelsize, addlabel = (lab$ind.sup & "text" %in% geom), jitter = jitter )
    }
  }

  
  title2 <- title
  p <- .fviz_finish(p, X, axes, axes.linetype) +
    # Edit legend title
    scale_shape(name = legend.partial.title) +
    scale_linetype(name = legend.partial.title) +
    # Edit plot title
    labs(title = title2)


  p
}


#' @rdname fviz_hmfa
#' @export
fviz_hmfa_group <- function(X,  ...)
{
  warning("Deprecated function. Use fviz_hmfa_var(X, 'group') instead.")
  fviz_hmfa_var(X, choice = "group", ...)
}

# 
# fviz_mfa_axes <- function(X,  axes = c(1,2), geom=c("arrow", "text"),
#                           label = "all", invisible="none", labelsize=4, pointsize = 2,
#                           col.axes="red", alpha.axes=1, col.circle ="grey70",
#                           select.axes = list(name = NULL, contrib = NULL),
#                           # map ="symmetric",
#                           arrows = c(FALSE, FALSE), repel = FALSE,
#                           jitter = list(what = "label", width = NULL, height = NULL), ...)
# {
#   
#   if(is.null(jitter$what)) jitter$what <- "label"
#   
#   # data frame to be used for plotting
#   partial.axes <- facto_summarize(X, element = "partial.axes",
#                                   result = c("coord", "contrib"), axes = axes)
#   colnames(partial.axes)[2:3] <-  c("x", "y")
#   
#   
#   # scale coords according to the type of map
#   # var <- .scale_ca(var, res.ca = X,  element = "partial.axes",
#   #                  type = map, axes = axes)
#   
#   # Selection
#   partial.axes.all <- partial.axes
#   if(!is.null(select.axes)) partial.axes <- .select(partial.axes, select.axes)
#   
#   # elements to be labelled or hidden
#   lab <- .label(label)
#   hide <- .hide(invisible)
#   
#   alpha.limits <- NULL
#   if(alpha.axes %in% c("contrib", "coord", "x", "y"))
#     alpha.limits = range(partial.axes.all[, alpha.axes])
#   
#   # Draw correlation circle
#   theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
#   circle <- data.frame(xcircle = cos(theta), ycircle = sin(theta))
#   p <- ggplot(data = circle, aes_string("xcircle", "ycircle")) +
#     geom_path(color=col.circle)+
#     geom_hline(yintercept = 0, linetype="dashed")+
#     geom_vline(xintercept = 0, linetype="dashed")
#   
#   # geometry for variable
#   geom2 <- geom
#   if(arrows[2]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
#   
#   if(!hide$partial.axes){
#     p <-.ggscatter(p = p, data = partial.axes, x = 'x', y = 'y',
#                    col=col.axes,  alpha = alpha.axes,
#                    alpha.limits = alpha.limits,
#                    geom =  geom2, lab = lab$var, labelsize = labelsize, 
#                    pointsize = pointsize, jitter = jitter, repel = repel)
#   }
#   
#   p + labs(title="MFA - Partial Axes Representations")
# }


#' @rdname fviz_hmfa
#' @export
fviz_hmfa <- function(X, ...){
  # fviz_hmfa_ind_starplot(X, ...)
  fviz_hmfa_ind(X, ...)
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
