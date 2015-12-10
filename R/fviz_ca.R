#' @include utilities.R
NULL
#' Visualize Correspondence Analysis
#' 
#' @description
#' Graph of column/row variables from the output of Correspondence Analysis (CA).\cr\cr
#' \itemize{
#' \item{fviz_ca_row(): Graph of row variables}
#' \item{fviz_ca_col(): Graph of column variables}
#' \item{fviz_ca_biplot(): Biplot of row and column variables}
#' \item{fviz_ca(): An alias of fviz_ca_biplot()}
#' } 
#' @param X an object of class CA [FactoMineR], ca [ca], coa [ade4];
#'  correspondence [MASS].
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted.
#' @param shape.row,shape.col the point shapes to be used for row/column variables. 
#' Default values are 19 for rows and 17 for columns.
#' @param geom a character specifying the geometry to be used for the graph. 
#' Allowed values are the combination of c("point", "arrow", "text"). 
#' Use "point" (to show only points); 
#' "text" to show only labels; c("point", "text") or c("arrow", "text") to show both types.
#' @param label a character vector specifying the elements to be labelled. 
#' Default value is "all". Allowed values are "none" or the combination of c("row", "row.sup", "col", "col.sup"). 
#' Use "col" to label only active column variables; "col.sup" to label only supplementary columns; etc
#' @param invisible a character value specifying the elements to be hidden on the plot. 
#' Default value is "none". Allowed values are 
#' the combination of c("row", "row.sup","col", "col.sup").
#' @param labelsize font size for the labels
#' @param pointsize the size of points
#' @param col.col,col.row color for column/row points. 
#' The default values are "red" and "blue", respectively. 
#' Allowed values include also : "cos2", "contrib", "coord", "x" or "y".
#'  In this case, the colors for row/column variables are automatically controlled by their qualities ("cos2"),
#'  contributions ("contrib"), coordinates (x^2 + y^2, "coord"), x values("x") or y values("y")
#' @param alpha.col,alpha.row controls the transparency of colors.
#' The value can variate from 0 (total transparency) to 1 (no transparency).
#' Default value is 1. Allowed values include also : "cos2", "contrib", "coord", "x" or "y" 
#' as for the arguments col.col and col.row.
#' @param col.col.sup,col.row.sup colors for the supplementary column and row points, respectively.
#' @param select.col,select.row a selection of columns/rows to be drawn. 
#' Allowed values are NULL or a list containing the arguments name, cos2 or contrib: 
#' \itemize{
#' \item name is a character vector containing column/row names to be drawn
#' \item cos2 if cos2 is in [0, 1], ex: 0.6, then columns/rows with a cos2 > 0.6 are drawn. 
#' if cos2 > 1, ex: 5, then the top 5 columns/rows with the highest cos2 are drawn.
#' \item contrib if contrib > 1, ex: 5,  then the top 5 columns/rows with the highest contrib are drawn
#' }
#' @param map character string specifying the map type. Allowed options include: 
#' "symmetric", "rowprincipal", "colprincipal", "symbiplot", "rowgab", 
#' "colgab", "rowgreen" and "colgreen". See details
#' @param arrows Vector of two logicals specifying if the plot should contain
#'  points (FALSE, default) or arrows (TRUE).
#'  First value sets the rows and the second value sets the columns.
#' @param jitter a parameter used to jitter the points in order to reduce overplotting. 
#' It's a list containing the objects what, width and height (i.e jitter = list(what, width, height)). 
#' \itemize{
#' \item what: the element to be jittered. Possible values are "point" or "p"; "label" or "l"; "both" or "b".
#' \item width: degree of jitter in x direction
#' \item height: degree of jitter in y direction
#' }
#' @param ... optional arguments.
#' @details The default plot of CA is a "symmetric" plot in which both rows and 
#' columns are in principal coordinates. In this situation, it's not possible 
#' to interpret the distance between row points and column points. To overcome this 
#' problem, the simplest way is to make an asymmetric plot. This means that, 
#' the column profiles must be presented in row space or vice-versa. 
#' The allowed options for the argument map are: 
#' \itemize{
#' \item "rowprincipal" or "colprincipal": asymmetric plots with either rows in principal 
#' coordinates and columns in standard coordinates, or vice versa. 
#' These plots preserve row metric or column metric respectively.
#' \item "symbiplot": Both rows and columns are scaled to have variances 
#' equal to the singular values (square roots of eigenvalues), 
#' which gives a symmetric biplot but does not preserve row or column metrics. 
#' \item "rowgab" or "colgab": Asymmetric maps, proposed by Gabriel & Odoroff (1990), 
#' with rows (respectively, columns) in 
#' principal coordinates and columns (respectively, rows) in standard coordinates 
#' multiplied by the mass of the corresponding point.
#' \item "rowgreen" or "colgreen": The so-called contribution biplots 
#' showing visually the most contributing points (Greenacre 2006b). 
#' These are similar to "rowgab" and "colgab" except that the points 
#' in standard coordinates are multiplied by the square root of the corresponding masses, 
#' giving reconstructions of the standardized residuals.
#' }
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' # Correspondence Analysis
#' # ++++++++++++++++++++++++++++++
#' # Install and load FactoMineR to compute CA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data(housetasks)
#' head(housetasks)
#' res.ca <- CA(housetasks, graph=FALSE)
#' 
#' # Graph of row variables
#' # +++++++++++++++++++++
#' # Default plot
#' fviz_ca_row(res.ca)
#' # Change title and axis labels
#' fviz_ca_row(res.ca) +
#'  labs(title = "CA", x = "Dim.1", y ="Dim.2" )
#' # Change axis limits by specifying the min and max
#' fviz_ca_row(res.ca) + 
#'    xlim(-1.3, 1.7) + ylim (-1.5, 1)
#' # Use text only
#' fviz_ca_row(res.ca, geom = "text")
#' # Use points only
#' fviz_ca_row(res.ca, geom="point")
#' # Change the size of points
#' fviz_ca_row(res.ca, geom="point", pointsize = 4)
#' # Change point color and theme
#' fviz_ca_row(res.ca, col.row = "violet")+
#'    theme_minimal()
#'    
#' # Control automatically the color of row points
#' # using the cos2 or the contributions
#' # cos2 = the quality of the rows on the factor map
#' fviz_ca_row(res.ca, col.row="cos2") 
#' # Gradient color
#' fviz_ca_row(res.ca, col.row="cos2") + 
#'       scale_color_gradient2(low="white", mid="blue", 
#'       high="red", midpoint=0.5, space = "Lab")
#' # Change the theme and use only points
#' fviz_ca_row(res.ca, col.row="cos2", geom = "point") + 
#'       scale_color_gradient2(low="white", mid="blue", 
#'       high="red", midpoint=0.4, space = "Lab")+ theme_minimal()
#'       
#' # Color by the contributions   
#' fviz_ca_row(res.ca, col.row="contrib") + 
#'       scale_color_gradient2(low="white", mid="blue", 
#'       high="red", midpoint=10, space = "Lab")
#'       
#' # Control the transparency of the color by the
#' # contributions
#' fviz_ca_row(res.ca, alpha.row="contrib") +
#'      theme_minimal()        
#'              
#' # Select and visualize rows with cos2 > 0.5
#' fviz_ca_row(res.ca, select.row = list(cos2 = 0.5))
#' # Select the top 7 according to the cos2
#' fviz_ca_row(res.ca, select.row = list(cos2 = 7))
#' # Select the top 7 contributing rows
#' fviz_ca_row(res.ca, select.row = list(contrib = 7))
#' # Select by names
#' fviz_ca_row(res.ca, 
#' select.row = list(name = c("Breakfeast", "Repairs", "Holidays")))
#' 
#'  
#' # Graph of column points
#' # ++++++++++++++++++++++++++++
#' # Default plot
#' fviz_ca_col(res.ca)
#' # Change color and theme
#' fviz_ca_col(res.ca, col.col="steelblue")+
#'  theme_minimal()
#'  
#' # Control colors using their contributions
#' fviz_ca_col(res.ca, col.col = "contrib")+
#'  scale_color_gradient2(low = "white", mid = "blue", 
#'            high = "red", midpoint = 25, space = "Lab") +
#'  theme_minimal()          
#' # Control the transparency of variables using their contributions
#' fviz_ca_col(res.ca, alpha.col = "contrib") +
#'    theme_minimal()
#'    
#' # Select and visualize columns with cos2 >= 0.4
#' fviz_ca_col(res.ca, select.col = list(cos2 = 0.4))
#' # Select the top 3 contributing columns
#' fviz_ca_col(res.ca, select.col = list(contrib = 3))
#' # Select by names
#' fviz_ca_col(res.ca, 
#'  select.col= list(name = c("Wife", "Husband", "Jointly")))
#'     
#' # biplot
#' # ++++++++++++++++++++++++++
#' # Symetric Biplot of rows and columns
#' fviz_ca_biplot(res.ca)
#' # Asymetric biplot, use arrows for columns
#' fviz_ca_biplot(res.ca, map ="rowprincipal",
#'  arrow = c(FALSE, TRUE))
#' # Keep only the labels for row points
#' fviz_ca_biplot(res.ca, label ="row")
#' # Keep only labels for column points
#' fviz_ca_biplot(res.ca, label ="col")
#' # Hide row points
#' fviz_ca_biplot(res.ca, invisible ="row")
#' # Hide column points
#' fviz_ca_biplot(res.ca, invisible ="col")
#'# Control automatically the color of rows using the cos2
#' fviz_ca_biplot(res.ca, col.row="cos2") +
#'        theme_minimal()
#' # Select the top 7 contributing rows
#' # And the top 3 columns
#' fviz_ca_biplot(res.ca,  
#'                select.row = list(contrib = 7),
#'                select.col = list(contrib = 3)) 
#' }
#'  
#' @name fviz_ca
#' 
#' @rdname fviz_ca 
#' @export 
fviz_ca_row <-function(X,  axes = c(1,2), shape.row = 19, 
                       geom=c("point", "text"),
                       label = "all", invisible="none", labelsize=4, pointsize = 2,
                       col.row ="blue", col.row.sup="darkblue",  alpha.row = 1,
                       select.row = list(name = NULL, cos2 = NULL, contrib = NULL),
                       map ="symmetric",
                       jitter = list(what = "label", width = NULL, height = NULL),...)
{
  
  if(length(intersect(geom, c("point", "text", "arrow"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  if(length(axes) > 2) stop("axes should be of length 2")
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  row <- facto_summarize(X, element = "row", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(row)[2:3] <-  c("x", "y")
  
  # scale row coords according to the type of map
  row <- .scale_ca(row, res.ca = X,  element = "row", 
                   type = map, axes = axes)
  
  row.all <- row
  if(!is.null(select.row)) row <- .select(row, select.row)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.row %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(row.all[, alpha.row])
  
  p <- ggplot() 
  if(hide$row) p <-ggplot()+geom_blank(data=row, aes_string("x","y"))
  else p <- .ggscatter(data = row, x = 'x', y = 'y', 
                       col=col.row,  alpha = alpha.row, 
                       alpha.limits = alpha.limits, shape = shape.row, 
                       geom = geom, lab = lab$row, labelsize = labelsize,
                       pointsize = pointsize, jitter = jitter)
  
  # Add supplementary rows
  if(inherits(X, c('CA', 'ca')) & !hide$row.sup){
    row_sup <- .get_supp(X, element = "row.sup", axes = axes,
                         select = select.row)
    if(!is.null(row_sup)){
      colnames(row_sup)[2:3] <-  c("x", "y")
      row_sup <- .scale_ca(row_sup, res.ca = X,  element = "row.sup", 
                           type = map, axes = axes)
    }
    
    if(!is.null(row_sup)){
      p <- fviz_add(p, df = row_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.row.sup, shape = shape.row,
                    labelsize = labelsize, addlabel = (lab$row.sup & "text" %in% geom),
                    pointsize = pointsize, jitter = jitter)
      
    }   
  } 
  
  p <- .fviz_finish(p, X, axes)
  p
  
}

#' @rdname fviz_ca
#' @export 
fviz_ca_col <-function(X,  axes = c(1,2), shape.col = 17, 
                       geom=c("point", "text"),
                       label = "all", invisible="none", labelsize=4, pointsize = 2,
                       col.col ="red", col.col.sup="darkred",  alpha.col = 1,
                       select.col = list(name = NULL, cos2 = NULL, contrib = NULL),
                       map ="symmetric",
                       jitter = list(what = "label", width = NULL, height = NULL), ...)
{
  
  if(length(intersect(geom, c("point", "text", "arrow"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed")
  if(length(axes) > 2) stop("axes should be of length 2")
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  col <- facto_summarize(X, element = "col", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(col)[2:3] <-  c("x", "y")
  
  # scale coords according to the type of map
  col <- .scale_ca(col, res.ca = X,  element = "col", 
                   type = map, axes = axes)
  
  col.all <- col
  if(!is.null(select.col)) col <- .select(col, select.col)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.col %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(col.all[, alpha.col])
  
  p <- ggplot() 
  if(hide$col) p <-ggplot()+geom_blank(data=col, aes_string("x","y"))
  else p <- .ggscatter(data = col, x = 'x', y = 'y', 
                       col=col.col,  alpha = alpha.col, 
                       alpha.limits = alpha.limits, shape = shape.col, 
                       geom = geom, lab = lab$col, labelsize = labelsize,
                       pointsize = pointsize, jitter = jitter)
  
  # Add supplementary cols
  if(inherits(X, c('CA', 'ca')) & !hide$col.sup ){
    col_sup <- .get_supp(X, element = "col.sup", axes = axes,
                         select = select.col)
    if(!is.null(col_sup)){
      colnames(col_sup)[2:3] <-  c("x", "y")
      col_sup <- .scale_ca(col_sup, res.ca = X,  element = "col.sup", 
                           type = map, axes = axes)
    }   

    if(!is.null(col_sup)){
      p <- fviz_add(p, df = col_sup[, 2:3, drop = FALSE], geom = geom,
                    color = col.col.sup, shape = shape.col,
                    labelsize = labelsize, addlabel = (lab$col.sup & "text" %in% geom),
                    pointsize = pointsize, jitter = jitter)
    }   
  } 
  
  p <- .fviz_finish(p, X, axes) 
  p
  
}




#' @rdname fviz_ca
#' @export 
fviz_ca_biplot <-function(X,  axes = c(1,2), shape.row = 19, shape.col = 17, 
                       geom=c("point", "text"),
                       label = "all", invisible="none", labelsize=4, pointsize =2,
                       col.col ="red", col.col.sup="darkred",  alpha.col = 1,
                       col.row ="blue", col.row.sup="darkblue",  alpha.row = 1,
                       select.col = list(name = NULL, cos2 = NULL, contrib = NULL),
                       select.row = list(name = NULL, cos2 = NULL, contrib = NULL),
                       map ="symmetric", arrows = c(FALSE, FALSE), 
                       jitter = list(what = "label", width = NULL, height = NULL), ...)
{
  
  if(length(intersect(geom, c("point", "text", "arrow"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  if(length(axes) > 2) stop("axes should be of length 2")
  
  if(is.null(jitter$what)) jitter$what <- "label"
  
  # data frame to be used for plotting
  col <- facto_summarize(X, element = "col", 
                         result = c("coord", "contrib", "cos2"), axes = axes)
  colnames(col)[2:3] <-  c("x", "y")
  
  # scale row coords according to the type of map
  col <- .scale_ca(col, res.ca = X,  element = "col", 
                   type = map, axes = axes)
  
  col.all <- col
  if(!is.null(select.col)) col <- .select(col, select.col)
  
  # elements to be labelled or hidden
  lab <- .label(label)
  hide <- .hide(invisible)
  
  alpha.limits <- NULL
  if(alpha.col %in% c("cos2","contrib", "coord", "x", "y"))
    alpha.limits = range(col.all[, alpha.col])
  
  geom2 <- geom
  if(arrows[1]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  p <- fviz_ca_row(X,  axes = axes, shape.row = shape.row, 
        geom=geom2,
        label = label, invisible = invisible, labelsize=labelsize, pointsize = pointsize,
        col.row =col.row, col.row.sup=col.row.sup,  alpha.row = alpha.row, select.row = select.row,
        map = map, jitter = jitter)
  
  
  # geom for columns
  geom2 <- geom
  if(arrows[2]==TRUE) geom2 <- setdiff(unique(c(geom2, "arrow")), "point")
  
  if(!hide$col){
    p <- .ggscatter(p = p, data = col, x = 'x', y = 'y', 
                    col=col.col,  alpha = alpha.col, 
                    alpha.limits = alpha.limits, shape = shape.col, 
                    geom = geom2, lab = lab$col, labelsize = labelsize,
                    pointsize = pointsize, jitter = jitter)
  }
    
  # Add supplementary cols
  if(inherits(X, c('CA', 'ca')) & !hide$col.sup ){
      col_sup <- .get_supp(X, element = "col.sup", axes = axes,
                         select = select.col)
      if(!is.null(col_sup)){
        colnames(col_sup)[2:3] <-  c("x", "y")
        col_sup <- .scale_ca(col_sup, res.ca = X,  element = "col.sup", 
                             type = map, axes = axes)
      }
    if(!is.null(col_sup)){
      p <- fviz_add(p, df = col_sup[, 2:3, drop = FALSE], geom = geom2,
                    color = col.col.sup, shape = shape.col,
                    labelsize = labelsize, addlabel = (lab$col.sup & "text" %in% geom),
                    pointsize = pointsize, jitter = jitter)
    }   
  }    
      
  
  p + labs(title="CA factor map - Biplot")
  
}

#' @rdname fviz_ca
#' @export
fviz_ca <- function(X, ...){
  fviz_ca_biplot(X, ...)
}
