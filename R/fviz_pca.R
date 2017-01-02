#' @include get_pca.R fviz.R
 NULL
#' Visualize Principal Component Analysis
#' 
#' 
#' @description Principal component analysis (PCA) reduces the dimensionality of
#'   multivariate data, to two or three that can be visualized graphically with 
#'   minimal loss of information. fviz_pca() provides ggplot2-based elegant 
#'   visualization of PCA outputs from: i) prcomp and princomp [in built-in R 
#'   stats], ii) PCA [in FactoMineR], iii) dudi.pca [in ade4] and epPCA [ExPosition]. Read more: 
#'   \href{http://www.sthda.com/english/wiki/factominer-and-factoextra-principal-component-analysis-visualization-r-software-and-data-mining}{Principal
#'    Component Analysis}
#'   
#'   \itemize{ \item{fviz_pca_ind(): Graph of individuals} \item{fviz_pca_var():
#'   Graph of variables} \item{fviz_pca_biplot(): Biplot of individuals and 
#'   variables} \item{fviz_pca(): An alias of fviz_pca_biplot()} }
#'   
#' @param X an object of class PCA [FactoMineR]; prcomp and princomp [stats]; 
#'   dudi and pca [ade4]; expOutput/epPCA [ExPosition].
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
#' @param title the title of the graph
#' @param habillage an optional factor variable for coloring the observations by
#'   groups. Default value is "none". If X is a PCA object from FactoMineR 
#'   package, habillage can also specify the supplementary qualitative variable 
#'   (by its index or name) to be used for coloring individuals by groups (see 
#'   ?PCA in FactoMineR).
#' @param addEllipses logical value. If TRUE, draws ellipses around the 
#'   individuals when habillage != "none".
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
#' @param select.ind,select.var a selection of individuals/variables to be 
#'   drawn. Allowed values are NULL or a list containing the arguments name, 
#'   cos2 or contrib: \itemize{ \item name: is a character vector containing 
#'   individuals/variables to be drawn \item cos2: if cos2 is in [0, 1], ex: 
#'   0.6, then individuals/variables with a cos2 > 0.6 are drawn. if cos2 > 1, 
#'   ex: 5, then the top 5 individuals/variables with the highest cos2 are 
#'   drawn. \item contrib: if contrib > 1, ex: 5,  then the top 5 
#'   individuals/variables with the highest contrib are drawn }
#' @inheritParams ggpubr::ggpar
#' @inheritParams fviz
#' @param ... Additional arguments. \itemize{ \item in fviz_pca_ind() and 
#'   fviz_pca_var(): Additional arguments are passed to the functions 
#'   fviz() and ggpubr::ggpar(). \item in fviz_pca_biplot() and fviz_pca(): Additional
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
#'  }
#'  
#' @rdname fviz_pca
#' @export
fviz_pca <- function(X, ...){
  fviz_pca_biplot(X, ...)
}


#' @rdname fviz_pca 
#' @export 
fviz_pca_ind <- function(X,  axes = c(1,2), geom = c("point", "text"), repel = FALSE,
                         habillage="none", palette = NULL, addEllipses=FALSE, 
                         col.ind = "black", col.ind.sup = "blue", alpha.ind =1,
                         select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                         ...)
{
 
  fviz (X, element = "ind", axes = axes, geom = geom,
                 habillage = habillage, palette = palette, addEllipses = addEllipses, 
                 color = col.ind, alpha = alpha.ind, col.row.sup = col.ind.sup,
                select = select.ind, repel = repel,  ...)
  
}


#' @rdname fviz_pca
#' @export 
fviz_pca_var <- function(X, axes=c(1,2), geom = c("arrow", "text"), 
                         repel = FALSE, col.var="black", alpha.var=1, 
                         col.quanti.sup="blue", col.circle ="grey70", 
                         select.var = list(name = NULL, cos2 = NULL, contrib = NULL),
                          ...)
{
  fviz (X, element = "var", axes = axes, geom = geom,
                color = col.var, alpha = alpha.var,  select = select.var,
                repel = repel, col.col.sup = col.quanti.sup, 
                col.circle = col.circle,...)
  
}



#' @rdname fviz_pca
#' @export
fviz_pca_biplot <- function(X,  axes = c(1,2), geom = c("point", "text"),
                            col.ind = "black", col.var = "steelblue",
                            label = "all", invisible="none", repel = FALSE, 
                            habillage = "none", palette = NULL, addEllipses=FALSE, 
                            title = "PCA - Biplot", ...)
{
  # Data frame to be used for plotting
  var <- facto_summarize(X, element = "var", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(var)[2:3] <-  c("x", "y")
  
  pca.ind <- get_pca_ind(X)
  ind <- data.frame(pca.ind$coord[, axes, drop=FALSE])
  colnames(ind)<- c("x", "y")
  
  # rescale variable coordinates
  r <- min(
    (max(ind[,"x"])-min(ind[,"x"])/(max(var[,"x"])-min(var[,"x"]))),
    (max(ind[,"y"])-min(ind[,"y"])/(max(var[,"y"])-min(var[,"y"])))
  )
  
  # Individuals
  p <- fviz_pca_ind(X,  axes = axes, geom = geom, repel = repel,
                    col.ind = col.ind,
                    label = label, invisible=invisible, habillage = habillage,
                    addEllipses = addEllipses, palette = palette, ...)
  # Add variables
  p <- fviz_pca_var(X, axes = axes, geom =  c("arrow", "text"), repel = repel,
                    col.var = col.var,
                    label = label, invisible = invisible,
                    scale.= r*0.7, ggp = p,  ...)
  p+labs(title=title)
}







