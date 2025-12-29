#' @include get_pca.R fviz.R
 NULL
#' Visualize Principal Component Analysis
#' 
#' 
#' @description Principal component analysis (PCA) reduces the dimensionality of
#'   multivariate data, to two or three that can be visualized graphically with 
#'   minimal loss of information. fviz_pca() provides ggplot2-based elegant 
#'   visualization of PCA outputs from: i) prcomp and princomp [in built-in R 
#'   stats], ii) PCA [in FactoMineR], iii) dudi.pca [in ade4] and epPCA 
#'   [ExPosition]. Read more: 
#'   \href{http://www.sthda.com/english/wiki/factominer-and-factoextra-principal-component-analysis-visualization-r-software-and-data-mining}{Principal
#'    Component Analysis}
#'   
#'   \itemize{ \item{fviz_pca_ind(): Graph of individuals} \item{fviz_pca_var():
#'   Graph of variables} \item{fviz_pca_biplot(): Biplot of individuals and 
#'   variables} \item{fviz_pca(): An alias of fviz_pca_biplot()} }
#'   
#'   Note that, \code{fviz_pca_xxx()} functions are wrapper around the core
#'   function \code{\link{fviz}()}, which is also a wrapper around the
#'   function \code{\link[ggpubr]{ggscatter}()} [in ggpubr]. Therefore, further arguments, to be
#'   passed to the function \code{\link{fviz}()} and \code{\link[ggpubr]{ggscatter}()}, can be specified in
#'   \code{\link{fviz_pca_ind}()} and \code{\link{fviz_pca_var}()}.
#'   
#' @param X an object of class PCA [FactoMineR]; prcomp and princomp [stats]; 
#'   dudi and pca [ade4]; expOutput/epPCA [ExPosition].
#' @param axes a numeric vector of length 2 specifying the dimensions to be 
#'   plotted.
#' @param geom a text specifying the geometry to be used for the graph. Allowed 
#'   values are the combination of \code{c("point", "arrow", "text")}. Use 
#'   \code{"point"} (to show only points); \code{"text"} to show only labels; 
#'   \code{c("point", "text")} or \code{c("arrow", "text")} to show arrows and 
#'   texts. Using \code{c("arrow", "text")} is sensible only for the graph of 
#'   variables.
#' @param geom.ind,geom.var as \code{geom} but for individuals and variables, 
#'   respectively. Default is geom.ind = c("point", "text), geom.var = 
#'   c("arrow", "text").
#' @param label a text specifying the elements to be labelled. Default value is 
#'   "all". Allowed values are "none" or the combination of c("ind", "ind.sup", 
#'   "quali", "var", "quanti.sup"). "ind" can be used to label only active 
#'   individuals. "ind.sup" is for supplementary individuals. "quali" is for 
#'   supplementary qualitative variables. "var" is for active variables. 
#'   "quanti.sup" is for quantitative supplementary variables.
#' @param invisible a text specifying the elements to be hidden on the plot. 
#'   Default value is "none". Allowed values are the combination of c("ind", 
#'   "ind.sup", "quali", "var", "quanti.sup").
#' @param title the title of the graph
#' @param habillage an optional factor variable for coloring the observations by
#'   groups. Default value is "none". If X is a PCA object from FactoMineR 
#'   package, habillage can also specify the supplementary qualitative variable 
#'   (by its index or name) to be used for coloring individuals by groups (see 
#'   ?PCA in FactoMineR).
#' @param addEllipses logical value. If TRUE, draws ellipses around the 
#'   individuals when habillage != "none".
#' @param col.ind,col.var color for individuals and variables, respectively. Can
#'   be a continuous variable or a factor variable. Possible values include also
#'   : "cos2", "contrib", "coord", "x" or "y". In this case, the colors for 
#'   individuals/variables are automatically controlled by their qualities of 
#'   representation ("cos2"), contributions ("contrib"), coordinates (x^2+y^2, 
#'   "coord"), x values ("x") or y values ("y"). To use automatic coloring (by 
#'   cos2, contrib, ....), make sure that habillage ="none".
#' @param fill.ind,fill.var same as col.ind and col.var but for the fill color.
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
#' @param select.ind,select.var a selection of individuals/variables to be
#'   drawn. Allowed values are NULL or a list containing the arguments name,
#'   cos2 or contrib: \itemize{ \item name: is a character vector containing
#'   individuals/variables to be drawn \item cos2: if cos2 is in [0, 1], ex:
#'   0.6, then individuals/variables with a cos2 > 0.6 are drawn. if cos2 > 1,
#'   ex: 5, then the top 5 individuals/variables with the highest cos2 are
#'   drawn. \item contrib: if contrib > 1, ex: 5,  then the top 5
#'   individuals/variables with the highest contrib are drawn }
#' @param biplot.type type of biplot scaling for fviz_pca_biplot(). Options are:
#'   \itemize{
#'     \item "auto" (default): Uses range-based rescaling for visualization
#'     \item "form": Form biplot (Gabriel, 1971). Distances between individuals
#'       approximate Euclidean distances. Use when focus is on individual relationships.
#'     \item "covariance": Covariance biplot (Gabriel, 1971). Angles between variable
#'       vectors approximate correlations, lengths approximate standard deviations.
#'       Use when focus is on variable relationships.
#'   }
#'   Note: "form" and "covariance" scaling requires prcomp or princomp objects.
#' @inheritParams ggpubr::ggpar
#' @inheritParams fviz
#' @param ... Additional arguments. \itemize{ \item in fviz_pca_ind() and 
#'   fviz_pca_var(): Additional arguments are passed to the functions fviz() and
#'   ggpubr::ggpar(). \item in fviz_pca_biplot() and fviz_pca(): Additional 
#'   arguments are passed to fviz_pca_ind() and fviz_pca_var().}
#'   
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
#' # Use repel = TRUE to avoid overplotting (slow if many points)
#' fviz_pca_ind(res.pca, col.ind = "#00AFBB",
#'    repel = TRUE)
#' 
#'  
#' # 1. Control automatically the color of individuals 
#'    # using the "cos2" or the contributions "contrib"
#'    # cos2 = the quality of the individuals on the factor map
#' # 2. To keep only point or text use geom = "point" or geom = "text".
#' # 3. Change themes using ggtheme: http://www.sthda.com/english/wiki/ggplot2-themes
#' 
#' fviz_pca_ind(res.pca, col.ind="cos2", geom = "point",
#'    gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
#' 
#' # Color individuals by groups, add concentration ellipses
#' # Change group colors using RColorBrewer color palettes
#' # Read more: http://www.sthda.com/english/wiki/ggplot2-colors
#' # Remove labels: label = "none".
#' fviz_pca_ind(res.pca, label="none", habillage=iris$Species,
#'      addEllipses=TRUE, ellipse.level=0.95, palette = "Dark2")
#'              
#'      
#' # Change group colors manually
#' # Read more: http://www.sthda.com/english/wiki/ggplot2-colors
#' fviz_pca_ind(res.pca, label="none", habillage=iris$Species,
#'      addEllipses=TRUE, ellipse.level=0.95,
#'      palette = c("#999999", "#E69F00", "#56B4E9"))
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
#' fviz_pca_var(res.pca, col.var = "steelblue")
#'  
#' # Control variable colors using their contributions
#' fviz_pca_var(res.pca, col.var = "contrib", 
#'    gradient.cols = c("white", "blue", "red"),
#'    ggtheme = theme_minimal())
#'  
#'     
#' # Biplot of individuals and variables
#' # ++++++++++++++++++++++++++
#' # Keep only the labels for variables
#' # Change the color by groups, add ellipses
#' fviz_pca_biplot(res.pca, label = "var", habillage=iris$Species,
#'                addEllipses=TRUE, ellipse.level=0.95,
#'                ggtheme = theme_minimal())
#'
#' # Biplot types (Gabriel, 1971):
#' # Form biplot - focus on individual distances
#' fviz_pca_biplot(res.pca, biplot.type = "form",
#'                label = "var", habillage = iris$Species)
#' # Covariance biplot - focus on variable correlations
#' fviz_pca_biplot(res.pca, biplot.type = "covariance",
#'                label = "var", habillage = iris$Species)
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
fviz_pca_ind <- function(X,  axes = c(1,2), geom = c("point", "text"),
                         geom.ind = geom, repel = FALSE,
                         habillage="none", palette = NULL, addEllipses=FALSE, 
                         col.ind = "black", fill.ind = "white", col.ind.sup = "blue", alpha.ind =1,
                         select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                         ...)
{
 
  fviz (X, element = "ind", axes = axes, geom = geom.ind,
                 habillage = habillage, palette = palette, addEllipses = addEllipses, 
                 color = col.ind, fill = fill.ind, alpha = alpha.ind, col.row.sup = col.ind.sup,
                select = select.ind, repel = repel,  ...)
  
}


#' @rdname fviz_pca
#' @export 
fviz_pca_var <- function(X, axes=c(1,2), geom = c("arrow", "text"), 
                         geom.var = geom,
                         repel = FALSE, col.var="black", fill.var = "white", alpha.var=1, 
                         col.quanti.sup="blue", col.circle ="grey70", 
                         select.var = list(name = NULL, cos2 = NULL, contrib = NULL),
                          ...)
{
  fviz (X, element = "var", axes = axes, geom = geom.var,
                color = col.var, fill = fill.var, alpha = alpha.var,  select = select.var,
                repel = repel, col.col.sup = col.quanti.sup, 
                col.circle = col.circle,...)
  
}



#' @rdname fviz_pca
#' @export
fviz_pca_biplot <- function(X,  axes = c(1,2), geom = c("point", "text"),
                            geom.ind = geom, geom.var = c("arrow", "text"),
                            col.ind = "black", fill.ind = "white",
                            col.var = "steelblue", fill.var = "white", gradient.cols = NULL,
                            label = "all", invisible="none", repel = FALSE,
                            habillage = "none", palette = NULL, addEllipses=FALSE,
                            title = "PCA - Biplot",
                            biplot.type = c("auto", "form", "covariance"), ...)
{
  biplot.type <- match.arg(biplot.type)
  
  # Check if individials or variables are colored by variables
  is.individuals.colored.by.variable <- .is_grouping_var(fill.ind) | .is_grouping_var(col.ind)
  is.variables.colored.by.variable <-  .is_continuous_var(col.var) | .is_grouping_var(col.var)
  # If coloring variable are continuous, then gradient coloring shoulld be applied
  is.gradient.color <- .is_continuous_var(col.ind) | .is_continuous_var(col.var) 
  is.gradient.fill <- .is_continuous_var(fill.ind) | .is_continuous_var(fill.var)
  # If coloring variables are qualitative, then discrete coloring should be applied
  is.discrete.color <- .is_grouping_var(col.ind) | .is_grouping_var(habillage) | .is_grouping_var(col.var)
  is.discrete.fill <- .is_grouping_var(fill.ind) | .is_grouping_var(fill.var) |
                       .is_grouping_var(habillage) | (.is_grouping_var(col.ind) & addEllipses)
  
  
  # Data frame to be used for plotting
  var <- facto_summarize(X, element = "var", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(var)[2:3] <-  c("x", "y")
  
  pca.ind <- get_pca_ind(X)
  ind <- data.frame(pca.ind$coord[, axes, drop=FALSE], stringsAsFactors = TRUE)
  colnames(ind)<- c("x", "y")
  
  # Rescale variable coordinates based on biplot.type

  # Gabriel (1971) biplot scaling: observations scaled by lambda^(1-scale),

  # variables scaled by lambda^scale, where scale=0 is form biplot, scale=1 is covariance
  if(biplot.type == "form" || biplot.type == "covariance") {
    # Get eigenvalues (singular values squared for scaling)
    if(inherits(X, "prcomp")) {
      sdev <- X$sdev[axes]
    } else if(inherits(X, "princomp")) {
      sdev <- X$sdev[axes]
    } else {
      # Fall back to auto for other object types
      warning("biplot.type 'form' or 'covariance' requires prcomp/princomp objects. Using 'auto'.")
      biplot.type <- "auto"
    }

    if(biplot.type == "form") {
      # Form biplot (scale=0): preserves distances between individuals
      # Variables are scaled down, individuals keep their scores
      r <- 1 / max(abs(var[, c("x", "y")])) * 0.8 * max(abs(ind[, c("x", "y")]))
    } else if(biplot.type == "covariance") {
      # Covariance biplot (scale=1): preserves correlations between variables
      # Variables scaled by sqrt(n) * sdev, individuals scaled down
      n_obs <- nrow(ind)
      # Scale factor to make variable vectors meaningful while keeping individuals visible
      r <- sqrt(n_obs) * 0.15
    }
  }

  if(biplot.type == "auto") {
    # Default range-based rescaling for visualization
    # Fix for PR #129: correct parenthesis placement for ratio calculation
    r <- min(
      ((max(ind[,"x"])-min(ind[,"x"]))/(max(var[,"x"])-min(var[,"x"]))),
      ((max(ind[,"y"])-min(ind[,"y"]))/(max(var[,"y"])-min(var[,"y"])))
    )
  }
  
  # When fill.ind = grouping variable & col.var = continuous variable,
  # we should inactivate ellipse border and ind.point border colors,
  # otherwise --> error: Discrete value supplied to continuous scale
  # Reason: individuals are in discrete color and variable in gradient colors, 
  # and we can't change the color (https://github.com/kassambara/factoextra/issues/42)
  ellipse.border.remove  <- FALSE
  if(is.individuals.colored.by.variable & is.variables.colored.by.variable)
    ellipse.border.remove <- TRUE

  
  
  # Individuals
  p <- fviz_pca_ind(X,  axes = axes, geom = geom.ind, repel = repel,
                    col.ind = col.ind, fill.ind = fill.ind,
                    label = label, invisible=invisible, habillage = habillage,
                    addEllipses = addEllipses, # palette = palette, 
                    ellipse.border.remove = ellipse.border.remove,
                    ...)
  # Add variables
  p <- fviz_pca_var(X, axes = axes, geom =  geom.var, repel = repel,
                    col.var = col.var, fill.var = fill.var,
                    label = label, invisible = invisible,
                    scale.= r*0.7, ggp = p,  ...)
  
  if(!is.null(gradient.cols)){
    if(is.gradient.color) p <- p + ggpubr::gradient_color(gradient.cols)
    if(is.gradient.fill) p <- p + ggpubr::gradient_fill(gradient.cols)
  }
  
  if(!is.null(palette)){
    if(is.discrete.color) p <- p + ggpubr::color_palette(palette)
    if(is.discrete.fill) p <- p + ggpubr::fill_palette(palette)
     
  }
  
  p+labs(title=title)
}

