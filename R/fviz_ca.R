#' @include utilities.R
NULL
#'Visualize Correspondence Analysis
#'
#'@description Correspondence analysis (CA) is an extension of Principal 
#'  Component Analysis (PCA) suited to analyze frequencies formed by two 
#'  categorical variables. fviz_ca() provides ggplot2-based elegant 
#'  visualization of CA outputs from the R functions: CA [in FactoMineR], ca [in
#'  ca], coa [in ade4], correspondence [in MASS] and expOutput/epCA [in
#'  ExPosition]. Read more: 
#'  \href{http://www.sthda.com/english/wiki/correspondence-analysis-in-r-the-ultimate-guide-for-the-analysis-the-visualization-and-the-interpretation-r-software-and-data-mining}{Correspondence
#'   Analysis}
#'  
#'  \itemize{ \item{fviz_ca_row(): Graph of row variables} \item{fviz_ca_col():
#'  Graph of column variables} \item{fviz_ca_biplot(): Biplot of row and column
#'  variables} \item{fviz_ca(): An alias of fviz_ca_biplot()} }
#'  
#'@param X an object of class CA [FactoMineR], ca [ca], coa [ade4]; 
#'  correspondence [MASS] and expOutput/epCA [ExPosition].
#'@param axes a numeric vector of length 2 specifying the dimensions to be 
#'  plotted.
#'@param shape.row,shape.col the point shapes to be used for row/column 
#'  variables. Default values are 19 for rows and 17 for columns.
#'@param geom a character specifying the geometry to be used for the graph. 
#'  Allowed values are the combination of c("point", "arrow", "text"). Use 
#'  "point" (to show only points); "text" to show only labels; c("point", 
#'  "text") or c("arrow", "text") to show both types.
#' @param geom.row,geom.col as \code{geom} but for row and column elements,
#'   respectively. Default is geom.row = c("point", "text), geom.col =
#'   c("point", "text").
#'@param label a character vector specifying the elements to be labelled. 
#'  Default value is "all". Allowed values are "none" or the combination of 
#'  c("row", "row.sup", "col", "col.sup"). Use "col" to label only active column
#'  variables; "col.sup" to label only supplementary columns; etc
#'@param invisible a character value specifying the elements to be hidden on the
#'  plot. Default value is "none". Allowed values are the combination of 
#'  c("row", "row.sup","col", "col.sup").
#'@param title the title of the graph
#'@param col.col,col.row color for column/row points. The default values are 
#'  "red" and "blue", respectively. Can be a continuous variable or a factor
#'  variable. Allowed values include also : "cos2", "contrib", "coord", "x" or
#'  "y". In this case, the colors for row/column variables are automatically
#'  controlled by their qualities ("cos2"), contributions ("contrib"),
#'  coordinates (x^2 + y^2, "coord"), x values("x") or y values("y")
#'@param alpha.col,alpha.row controls the transparency of colors. The value can 
#'  variate from 0 (total transparency) to 1 (no transparency). Default value is
#'  1. Allowed values include also : "cos2", "contrib", "coord", "x" or "y" as 
#'  for the arguments col.col and col.row.
#'@param col.col.sup,col.row.sup colors for the supplementary column and row 
#'  points, respectively.
#'@param repel a boolean, whether to use ggrepel to avoid overplotting text 
#'  labels or not.
#'@param select.col,select.row a selection of columns/rows to be drawn. Allowed 
#'  values are NULL or a list containing the arguments name, cos2 or contrib: 
#'  \itemize{ \item name is a character vector containing column/row names to be
#'  drawn \item cos2 if cos2 is in [0, 1], ex: 0.6, then columns/rows with a 
#'  cos2 > 0.6 are drawn. if cos2 > 1, ex: 5, then the top 5 columns/rows with 
#'  the highest cos2 are drawn. \item contrib if contrib > 1, ex: 5,  then the 
#'  top 5 columns/rows with the highest contrib are drawn }
#'@param map character string specifying the map type. Allowed options include: 
#'  "symmetric", "rowprincipal", "colprincipal", "symbiplot", "rowgab", 
#'  "colgab", "rowgreen" and "colgreen". See details
#'@param arrows Vector of two logicals specifying if the plot should contain 
#'  points (FALSE, default) or arrows (TRUE). First value sets the rows and the 
#'  second value sets the columns.
#'@param ... Additional arguments. \itemize{ \item in fviz_ca_row() and 
#'  fviz_ca_col(): Additional arguments are passed to the functions fviz() and
#'  ggpubr::ggpar(). \item in fviz_ca_biplot() and fviz_ca(): Additional 
#'  arguments are passed to fviz_ca_row() and fviz_ca_col().}
#'@details The default plot of (M)CA is a "symmetric" plot in which both rows
#'  and columns are in principal coordinates. In this situation, it's not
#'  possible to interpret the distance between row points and column points. To
#'  overcome this problem, the simplest way is to make an asymmetric plot. This
#'  means that, the column profiles must be presented in row space or
#'  vice-versa. The allowed options for the argument map are: \itemize{ \item
#'  "rowprincipal" or "colprincipal": asymmetric plots with either rows in
#'  principal coordinates and columns in standard coordinates, or vice versa.
#'  These plots preserve row metric or column metric respectively. \item
#'  "symbiplot": Both rows and columns are scaled to have variances equal to the
#'  singular values (square roots of eigenvalues), which gives a symmetric
#'  biplot but does not preserve row or column metrics. \item "rowgab" or
#'  "colgab": Asymmetric maps, proposed by Gabriel & Odoroff (1990), with rows
#'  (respectively, columns) in principal coordinates and columns (respectively,
#'  rows) in standard coordinates multiplied by the mass of the corresponding
#'  point. \item "rowgreen" or "colgreen": The so-called contribution biplots
#'  showing visually the most contributing points (Greenacre 2006b). These are
#'  similar to "rowgab" and "colgab" except that the points in standard
#'  coordinates are multiplied by the square root of the corresponding masses,
#'  giving reconstructions of the standardized residuals. }
#'@return a ggplot
#'@author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'@seealso \code{\link{get_ca}}, \code{\link{fviz_pca}}, \code{\link{fviz_mca}}
#'@references http://www.sthda.com
#' @examples
#' # Correspondence Analysis
#' # ++++++++++++++++++++++++++++++
#' # Install and load FactoMineR to compute CA
#' # install.packages("FactoMineR")
#' 
#' library("FactoMineR")
#' data(housetasks)
#' head(housetasks)
#' res.ca <- CA(housetasks, graph=FALSE)
#' 
#' # Biplot of rows and columns
#' # ++++++++++++++++++++++++++
#' # Symetric Biplot of rows and columns
#' fviz_ca_biplot(res.ca)
#' 
#' # Asymetric biplot, use arrows for columns
#' fviz_ca_biplot(res.ca, map ="rowprincipal",
#'  arrow = c(FALSE, TRUE))
#'  
#' # Keep only the labels for row points
#' fviz_ca_biplot(res.ca, label ="row")
#' 
#' # Keep only labels for column points
#' fviz_ca_biplot(res.ca, label ="col")
#' 
#'        
#' # Select the top 7 contributing rows
#' # And the top 3 columns
#' fviz_ca_biplot(res.ca,  
#'                select.row = list(contrib = 7),
#'                select.col = list(contrib = 3))
#' 
#' # Graph of row variables
#' # +++++++++++++++++++++
#'    
#' # Control automatically the color of row points
#'    # using the "cos2" or the contributions "contrib"
#'    # cos2 = the quality of the rows on the factor map
#'    # Change gradient color
#'    # Use repel = TRUE to avoid overplotting (slow if many points)
#' fviz_ca_row(res.ca, col.row = "cos2",
#'    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    repel = TRUE)
#' 
#' # You can also control the transparency 
#' # of the color by the "cos2" or "contrib"
#' fviz_ca_row(res.ca, alpha.row="contrib") 
#'       
#' # Select and visualize some rows with select.row argument.
#'  # - Rows with cos2 >= 0.5: select.row = list(cos2 = 0.5)
#'  # - Top 7 rows according to the cos2: select.row = list(cos2 = 7)
#'  # - Top 7 contributing rows: select.row = list(contrib = 7)
#'  # - Select rows by names: select.row = list(name = c("Breakfeast", "Repairs", "Holidays"))
#'  
#'  # Example: Select the top 7 contributing rows
#' fviz_ca_row(res.ca, select.row = list(contrib = 7))
#' 
#'  
#' # Graph of column points
#' # ++++++++++++++++++++++++++++
#' 
#'  
#' # Control colors using their contributions
#' fviz_ca_col(res.ca, col.col = "contrib",
#'    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
#'        
#' # Select columns with select.col argument
#'    # You can select by contrib, cos2 and name 
#'    # as previously described for ind
#' # Select the top 3 contributing columns
#' fviz_ca_col(res.ca, select.col = list(contrib = 3))
#'     
#'     
#'  
#'@name fviz_ca
#'  
#'@rdname fviz_ca
#'@export
fviz_ca_row <-function(X,  axes = c(1,2), geom = c("point", "text"), geom.row = geom,
                       shape.row = 19, col.row ="blue", alpha.row = 1,
                       col.row.sup="darkblue",  
                       select.row = list(name = NULL, cos2 = NULL, contrib = NULL),
                       map ="symmetric", repel = FALSE,
                       ...)
{
  
  p <- fviz (X, element = "row", axes = axes, geom = geom.row,
                      color = col.row, alpha = alpha.row,
                      pointshape = shape.row, select = select.row, 
                      map = map, repel = repel, 
                      col.row.sup = col.row.sup, shape.sup = shape.row,   ...)
  p
  
}

#' @rdname fviz_ca
#' @export 
fviz_ca_col <-function(X,  axes = c(1,2), shape.col = 17, 
                       geom=c("point", "text"), geom.col = geom,
                       col.col ="red", col.col.sup="darkred",  alpha.col = 1,
                       select.col = list(name = NULL, cos2 = NULL, contrib = NULL),
                       map ="symmetric", repel = FALSE,
                       ...)
{
  
  fviz (X, element = "col", axes = axes, geom = geom.col,
        color = col.col, alpha = alpha.col,
        pointshape = shape.col, select = select.col, 
        map = map, repel = repel, 
        col.col.sup = col.col.sup, shape.sup = shape.col, ...)
  
  
}




#' @rdname fviz_ca
#' @export 
fviz_ca_biplot <-function(X,  axes = c(1,2), geom=c("point", "text"), 
                          geom.row = geom, geom.col = geom,
                          label = "all", invisible="none", arrows = c(FALSE, FALSE),
                          repel = FALSE, title = "CA - Biplot",
                       ...)
{
  # Rows
  geom2 <- geom.row
  if(arrows[1]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  p <- fviz_ca_row(X,  axes = axes, geom=geom2, repel = repel,
        label = label, invisible = invisible, ...)
  # Add columns
  geom2 <- geom.col
  if(arrows[2]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  p <- fviz_ca_col(X,  axes = axes, geom=geom2, repel = repel,
                   label = label, invisible = invisible, 
                   ggp = p, ...)
  
  p + labs(title=title)
  
}

#' @rdname fviz_ca
#' @export
fviz_ca <- function(X, ...){
  fviz_ca_biplot(X, ...)
}
