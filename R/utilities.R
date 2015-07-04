#' @include eigenvalue.R
NULL
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_blank
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_alpha
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom grid arrow
#' @importFrom grid unit
# Check and get the class of the output of a factor analysis
# ++++++++++++++++++++++++++++
# X: an output of factor analysis (PCA, CA, MCA) 
# from different packages (FactoMineR, ade4, ....) 
.get_facto_class <- function(X){
  if(inherits(X, c('PCA', 'princomp', 'prcomp')))
    facto_class ="PCA"
  else if(inherits(X, 'pca') & inherits(X, 'dudi'))
    facto_class ="PCA"
  else if(inherits(X, c("CA", "ca", "coa", "correspondence"))) facto_class="CA"
  else if(inherits(X, c("MCA", "acm"))) facto_class = "MCA"
  else stop("An object of class : ", class(X), 
            " can't be handled by factoextra")   
}

# Get the result for supplementary points
#++++++++++++++++++++++++++
## X: an output of factor analysis (PCA, CA, MCA) 
## element possible values are "row.sup", "col.sup" (CA);
# quanti, ind.sup (PCA); quali.sup (MCA)
## result the result tobe extracted for the element. Possible values are
#  the combination of c("cos2", "contrib", "coord")
## axes a numeric vector specifying the axes of interest. Default values are 1:2
##  for axes 1 and 2
## select: a selection of variables. See the function .select()
.get_supp <- function(X, element = NULL, axes = 1:2, 
                      result = c("coord", "cos2"), select = NULL){
  
  if(inherits(X, c("CA", "PCA", "MCA"))) {
    exprs <- paste0("X$", element)
    elmt <- eval(parse(text=exprs ))
  }
  else if(inherits(X, "ca")){
    if(element == "col.sup") elmt <- .get_ca_col_sup(X)
    else if(element == "row.sup") elmt <- .get_ca_row_sup(X)
  }
  else stop("An object of class : ", class(X), 
            " can't be handled by the function .get_supp()")
  
  # summarize the result
  res = NULL
  if(!is.null(elmt)){
    # check axes
    if(max(axes) > ncol(elmt$coord))
      stop("The value of the argument axes is incorrect. ",
           "The number of axes in the data is: ", ncol(elmt$coord), 
           ". Please try again with axes between 1 - ", ncol(elmt$coord))
    
    # 1.Extract the coordinates x, y and coord
    if("coord" %in% result){
      dd <- data.frame(elmt$coord[, axes, drop=FALSE])
      coord <- apply(dd^2, 1, sum) # x^2 + y2 + ...
      res = cbind(dd, coord = coord)
    }
    
    # 2. Extract the cos2
    if("cos2" %in% result){
      cos2 <- elmt$cos2[, axes]
      if(length(axes) > 1) cos2 <- apply(cos2, 1, sum, na.rm=TRUE)
      res <- cbind(res, cos2 = cos2)
    }
    
    res <- cbind.data.frame(name =rownames(elmt$coord), res)
    
    # selection of variables
    if(!is.null(select)){
      if(!is.null(select$contrib)) res <- NULL # supp points don't have contrib
      else res <- .select(res, select, check = FALSE)
    }
  }
  res
}


# get supplementary columns from ca output (ca package)
.get_ca_col_sup <- function(res.ca){
  # supplementary points
  index <- res.ca$colsup
  cols <- NULL 
  if(length(index) > 0){
    # principal coord = standard coord X sqrt(eig)
    coord <- t(apply(res.ca$colcoord, 1, "*", res.ca$sv))
    cos2 <- apply(coord^2, 2, "/", res.ca$coldist^2)
    cols <- list(coord = coord[index, , drop = FALSE], 
                cos2 = cos2[index, , drop = FALSE]) 
  }
  cols
}

# get supplementary rows from ca output (ca package)
.get_ca_row_sup <- function(res.ca){
  rows <- NULL
  # supplementary points
  index <- res.ca$rowsup
  if(length(index) > 0){
    # principal coord = standard coord X sqrt(eig)
    coord <- t(apply(res.ca$rowcoord, 1, "*", res.ca$sv))
    cos2 <- apply(coord^2, 2, "/", res.ca$rowdist^2)
    rows <- list(coord = coord[index, , drop = FALSE], 
                                     cos2 = cos2[index, , drop = FALSE]) 
  }
  rows
}

# Scale row coordinates depending on the map type
#++++++++++++++++++++++
## row.res, data: an output of facto_summarize containing the
# name, x, y, coord,... of the row variables
## res.ca an object of class CA, MCA
## type: a character string specifying the map type. 
# Allowed values include "symmetric", "rowprincipal", "colprincipal", "rowgab", "colgab", "rowgreen", "colgreen".
## element: possible values are "row", "col", "row.sup", "col.sup",
# "ind", "var", "ind.sup", "quali.sup"
.scale_ca <- function(data, res.ca, element="row", type="symmetric", axes = 1:2)
{
  res <- NULL
  if(!is.null(data)){
  if(element %in% c("row", "ind")) res <- .scale_ca_row(data, res.ca, type, axes)
  else if(element %in% c("col", "var")) res <- .scale_ca_col(data, res.ca, type, axes)
  else if(element %in% c( "row.sup", "ind.sup")) res <- .scale_ca_rowsupp(data, res.ca, type, axes)
  else if(element %in% c("col.sup", "quali.sup")) res <- .scale_ca_colsupp(data, res.ca, type, axes)
  } 
  res
}

.scale_ca_row <- function(row.res, res.ca, type ="symmetric", axes = 1:2){
  data <- row.res
  res <- data
  
  eig <- get_eigenvalue(res.ca)[axes,1]
  sv <- sqrt(eig)
  mass <- .get_ca_mass(res.ca, "row")[as.vector(data$name)]
  
  # rows/columns in principal coordinates
  if(type %in% c("symmetric", "rowprincipal", "rowgab", "rowgreen")) res <- data
  else{
    # get standard coordinates 
    x <- res$x/sqrt(eig[1])
    y <- res$y/sqrt(eig[2])
    
    # standard coordinate
    if(type %in% c("colprincipal")){
      res$x <- x
      res$y <- y
      res$coord <- res$x^2 + res$y^2
    }
    # standard coordinate X mass
    else if(type %in% c("colgab")){
      res$x <- x*mass
      res$y <- y*mass
      res$coord <- res$x^2 + res$y^2
    }
    
    # standard coordinate X sqrt(mass)
    else if(type %in% c("colgreen")){
      res$x <- x*sqrt(mass)
      res$y <- y*sqrt(mass)
      res$coord <- res$x^2 + res$y^2
    }
    else  if(type %in% c("symbiplot")){
      res$x <- x*sqrt(sv[1])
      res$y <- y*sqrt(sv[2])
      res$coord <- res$x^2 + res$y^2
    }
  }
  
  return(res)
}

# Scale ca column
.scale_ca_col <- function(col.res, res.ca, type ="symmetric", axes = 1:2){
  data <- col.res
  res <- data
  
  eig <- get_eigenvalue(res.ca)[axes,1]
  sv <- sqrt(eig)
  mass <- .get_ca_mass(res.ca, "col")[as.vector(data$name)]
  
  # rows/columns in principal coordinates
  if(type %in% c("symmetric", "colprincipal", "colgab", "colgreen")) res <- data
  else{
    # get standard coordinates 
    x <- res$x/sqrt(eig[1])
    y <- res$y/sqrt(eig[2])
    
    # standard coordinate
    if(type %in% c("rowprincipal")){
      res$x <- x
      res$y <- y
      res$coord <- res$x^2 + res$y^2
    }
    # standard coordinate X mass
    else if(type %in% c("rowgab")){
      res$x <- x*mass
      res$y <- y*mass
      res$coord <- res$x^2 + res$y^2
    }
    
    # standard coordinate X sqrt(mass)
    else if(type %in% c("rowgreen")){
      res$x <- x*sqrt(mass)
      res$y <- y*sqrt(mass)
      res$coord <- res$x^2 + res$y^2
    }
    else  if(type %in% c("symbiplot")){
      res$x <- x*sqrt(sv[1])
      res$y <- y*sqrt(sv[2])
      res$coord <- res$x^2 + res$y^2
    }
  }
  
  return(res)
}

# Scale ca row sup
.scale_ca_rowsupp <- function(rowsup.res, res.ca, type ="symmetric", axes = 1:2){
  data <- rowsup.res
  res <- data
  
  eig <- get_eigenvalue(res.ca)[axes,1]
  sv <- sqrt(eig)
  
  # rows/columns in principal coordinates
  if(type %in% c("symmetric", "rowprincipal", "rowgab", "rowgreen")) res <- data
  else{
    # get standard coordinates 
    x <- res$x/sqrt(eig[1])
    y <- res$y/sqrt(eig[2])
    
    # standard coordinate
    if(type %in% c("colprincipal")){
      res$x <- x
      res$y <- y
      res$coord <- res$x^2 + res$y^2
    }
    # standard coordinate X mass
    else if(type %in% c("colgab")) res <- NULL
    
    # standard coordinate X sqrt(mass)
    else if(type %in% c("colgreen")) res <- NULL
    
    else  if(type %in% c("symbiplot")){
      res$x <- x*sqrt(sv[1])
      res$y <- y*sqrt(sv[2])
      res$coord <- res$x^2 + res$y^2
    }
  }
  
  return(res)
}


.scale_ca_colsupp <- function(colsup.res, res.ca, type ="symmetric", axes = 1:2){
  data <- colsup.res
  res <- data
  
  eig <- get_eigenvalue(res.ca)[axes,1]
  sv <- sqrt(eig)
  
  # rows/columns in principal coordinates
  if(type %in% c("symmetric", "colprincipal", "colgab", "cogreen")) res <- data
  else{
    # get standard coordinates 
    x <- res$x/sqrt(eig[1])
    y <- res$y/sqrt(eig[2])
    
    # standard coordinate
    if(type %in% c("rowprincipal")){
      res$x <- x
      res$y <- y
      res$coord <- res$x^2 + res$y^2
    }
    # standard coordinate X mass
    else if(type %in% c("rowgab")) res <- NULL
    # standard coordinate X sqrt(mass)
    else if(type %in% c("rowgreen")) res <- NULL
    else  if(type %in% c("symbiplot")){
      res$x <- x*sqrt(sv[1])
      res$y <- y*sqrt(sv[2])
      res$coord <- res$x^2 + res$y^2
    }
  }
  
  return(res)
}


# get the mass of an element
#++++++++++++++++++++++++++++
# res.ca : CA object
# element: possible values are "row" or "col"
.get_ca_mass <- function(res.ca, element){
  
  if(inherits(res.ca, "ca")){
    if(element == "row"){
    mass <- res.ca$rowmass
    names(mass) <- res.ca$rownames
    }
    else if(element == "col"){
      mass <- res.ca$colmass
      names(mass) <- res.ca$colnames
    }
  }
  # FactoMiner
  else if(inherits(res.ca, c("CA", "MCA"))){
    if(element %in% c("row", "ind")) mass <- res.ca$call$marge.row
    else if(element %in% c("col", "var")) mass <- res.ca$call$marge.col
    
  }
  # Ade4
  else if(inherits(res.ca, c("coa", "acm"))){
    if(element =="row") mass <- res.ca$lw
    else if(element == "col") mass <- res.ca$cw
  }
  # Mass package
  else if(inherits(res.ca, "correspondence")){
    row.sum <- apply(res.ca$Freq, 1, sum)
    col.sum <- apply(res.ca$Freq, 2, sum)
    n <- sum(res.ca$Freq)
    if(element =="row") mass <- row.sum/n
    else if(element =="col") mass <- col.sum/n
  }
  else stop("An object of class : ", class(res.ca), 
            " can't be handled") 
  return(mass)
}


# Reconstruct data: get the original data
# res.pca is an object of class prcomp or princomp
.prcomp_reconst <- function(res.pca){
  if(inherits(res.pca, "prcomp")){
    centered <- is.numeric(res.pca$center)
    scaled <- is.numeric(res.pca$scale)
    
    if(centered & scaled) 
      t(t(res.pca$x %*% t(res.pca$rotation)) * res.pca$scale + res.pca$center)
    else if(centered)
      t(t(res.pca$x %*% t(res.pca$rotation)) + res.pca$center)
    else if(scaled)
      t(t(res.pca$x %*% t(res.pca$rotation)) * res.pca$scale)
    else t(t(res.pca$x %*% t(res.pca$rotation)))
  }
  
  else if(inherits(res.pca, "princomp")){ 
    if(!is.null(res.pca$scores))
      t(t(res.pca$scores %*% t(res.pca$loadings)) * res.pca$scale + res.pca$center)
    else stop("The object res.pca doesn't have the element scores. ", 
              "Please use the function princomp() with the argument ",
              "scores = TRUE.")
  }
  
  else stop("Can't handle an object of class ", class(res.pca))
}



# Build plot title for contribution, cos2 plots
# used in fviz_cos2, fviz_contrib, etc
#+++++++++++++++++++++++
# element: possible values are row, col, var, ind
# varname: the name of the variable name to be plotted
  # possible values are "Cos2", "Contribution"
# axes: a numeric vector specifying the axes of interest
# example: .build_title("row", "Cos2", 1:2)
.build_title <- function(element, varname,  axes){
  if(varname =="cos2") varname = "Cos2"
  else if(varname =="contrib") varname = "Contribution"
  
  if(element=="row") 
    title <- paste0(varname, " of rows to Dim-", paste(axes, collapse="-"))
  else if(element=="col")
    title <- paste0(varname, " of columns to Dim-", paste(axes, collapse="-"))
  else if(element=="var")
    title <- paste0(varname, " of variables to Dim-", paste(axes, collapse="-"))
  else if(element=="ind")
    title <- paste0(varname, " of individuals to Dim-", paste(axes, collapse="-"))
  
  return(title)
}

# Select rows according to a filter
# ++++++++++++++++++++
# d a data frame containing row names, coordinates, cos2, contrib, etc
# filter: a filter to be applied. Allowed values are NULL or a list containing the arguments either
#  name, cos2 or contrib
# - name: is a character vector containing row names of interest
# - cos2: if cos2 is in [0, 1], ex: 0.6, then rows with a cos2 > 0.6 are extracted.
#   if cos2 > 1, ex: 5, then the top 5 rows with the highest cos2 are extracted
# - contrib: if contrib > 1, ex: 5,  then the top 5 rows with the highest cos2 are extracted
# check: if TRUE, check the data after filtering
.select <- function(d, filter = NULL, check= TRUE){
  
  if(!is.null(filter)){
    
    # Filter by name
    if(!is.null(filter$name)){
      name <- filter$name
      common <- intersect(name, d$name)
      diff <- setdiff(name, d$name)
      #if(check & length(common) == 0) stop("Can't find the specified names")
      # if(check & length(diff)!=0) warning("Can't find the the following name(s): ", diff)
      d <- d[common, , drop = FALSE]
    }
    
    # Filter by cos2
    if(!is.null(filter$cos2) & nrow(d) >= 1){
      # case 1 cos2 is in [0, 1]
      # rows with cos2 > value are selected
      if(0 <= filter$cos2 & filter$cos2 <= 1){
        d <- d[which(d$cos2 >= filter$cos2), , drop = FALSE]
        if(check & nrow(d)==0)
          stop("There are no observations with cos2 >=", filter$cos2, 
               ". Please, change the value of cos2 and try again.")
      }
      # case 2 - cos2 > 1 : the top rows are selected 
      else if(filter$cos2 > 1){
        cos2 <- round(filter$cos2)
        d <- d[order(d$cos2, decreasing = TRUE), , drop = FALSE]
        d <- d[1:min(filter$cos2, nrow(d)),, drop = FALSE]
      }
    }
    
    # Filter by contrib: the top rows are selected 
    if(!is.null(filter$contrib) & nrow(d) >= 1){
      contrib <- round(filter$contrib)
      if(contrib < 1) stop("The value of the argument contrib >", 1)
      d <- d[order(d$contrib, decreasing = TRUE), , drop = FALSE]
      d <- d[1:min(contrib, nrow(d)), , drop = FALSE]
    }
    
  }
  
  return (d)
}


# Add supplementary points

# Make a ggplot2 bar plot
#+++++++++++++++++++++++++
# x: a numeric vector
# title, xlab, ylab : labels for the graph
# fill: a fill color for the bar plot
# color: an outline color for the bar plot
# sort.value: a string specifying whether x should be sorted or not. 
# Allowed values are "none" (no sorting), "asc" (for ascending) or "desc" (for descending)
# top a numeric value specifing the top  elements to be shown
.ggbarplot <- function(x,  title ="", xlab ="", ylab="",
                       fill="steelblue", color = "steelblue",  
                       sort.value = c("desc", "asc", "none"), top = Inf ){
  
 
  # top elements
  if(top!=Inf & top < length(x))
    x <- sort(x, decreasing=TRUE)[1:top]
  # sorting
  if(sort.value[1]=="desc") x <- sort(x, decreasing = TRUE)
  else if(sort.value[1]=="asc") x <- sort(x, decreasing = FALSE)
  # bar names
  if(is.null(names(x))) names(x) <- 1:length(x)
  #data frame for ggplot2
  d <- cbind.data.frame(name = factor(names(x), levels = names(x)), val = x)
  # plot
  p <- ggplot(d, aes_string("name", "val")) + 
    geom_bar(stat="identity", fill=fill, color = color) +
    labs(title = title,  x =xlab, y = ylab)+
    theme(axis.text.x = element_text(angle=45), 
                   axis.title.x = element_blank())
  
  return(p)
}


# Make a scatter plot
#++++++++++++++++++++++++++
## p: a ggplot. If not null, points are added on the existing plot
## data: is a data frame of form: name|x|y|coord|cos2|contrib
## col: point color. The default value is "black".
#  Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#  In this case, the colors for variables are automatically controlled by their qualities ("cos2"),
#  contributions ("contrib"), coordinates (x^2+y2, "coord"), x values("x") or y values("y")
## x, y: contains the name of x and y variables
## alpha: controls the transparency of colors.
# The value can variate from 0 (total transparency) to 1 (no transparency).
# Default value is 1. Possible values include also : "cos2", "contrib", "coord", "x" or "y".
## alpha.limits: the range of alpha values. Used only when alpha is 
# a continuous variable(i.e, cos2, contrib, ...)
## shape: point shape
## pointsize: point size
## lab: if TRUE points are labelled
## geom: a character specifying the geometry to be used for the graph.
#  Allowed values are the combination of c("point", "arrow", "text"). Use "point" (to show only points),
#  "text" to show only labels or c("point", "text") to show both types.
.ggscatter <- function(p = NULL, data, x = 'x', y = 'y', col="black",  alpha = 1,
                       alpha.limits = NULL, shape = 19, pointsize = 2, 
                       geom=c("point", "text"), lab = TRUE, labelsize = 4, ...)
  {
  
  data <- as.data.frame(data)
  
#   uvar = "xxx" # user variable used for coloring
#   # Color is a factor variable (Groups)
#   if(is.factor(col)){ 
#     if(nrow(data)!=length(col))
#       stop("The number of points and the length of the factor variable for coloring are different.")
#     uvar <- "Groups"
#     data<- cbind.data.frame(Groups = col, data)
#     data[, 1]<-as.factor(data[,1])
#   }
#   # Color is a numeric vector
#   else if(is.numeric(col) & length(col) == nrow(data)){
#     uvar <- "Col.var"
#     data<- cbind.data.frame(Col.var = col, data)
#     data[, 1]<-as.factor(data[,1])
#   }
#   else if(!(col %in% c("cos2","contrib", "coord", "x", "y"))){
#     stop("The argument col is incorrect.")
#   }
  
 
  
  if(is.null(p)) p <- ggplot() 
  # The color and the transparency of variables are automatically controlled by
  # their cos2, contrib,  "x" or "y" coordinates
  if(col %in% c("cos2","contrib", "coord", "x", "y") &
            alpha %in% c("cos2","contrib", "coord", "x", "y"))
  {
    if("point" %in% geom) 
      p <- p + geom_point(data = data, 
                          aes_string(x,y, color=col, alpha=alpha),
                          shape=shape, size = pointsize)
    else if("arrow" %in% geom) 
      p <- p + geom_segment(data = data,
                  aes_string(x = 0, y = 0, xend = x, yend = y, color=col, alpha=alpha),
                  arrow = grid::arrow(length = grid::unit(0.2, 'cm')))
  
    if(lab & "text"%in% geom) 
      p <- p + geom_text(data = data, 
                         aes_string(x,y, label = 'name', 
                                    color=col, alpha=alpha), size = labelsize)
    if(!is.null(alpha.limits)) p <- p + scale_alpha(limits = alpha.limits)
  }
  # Only the color is controlled automatically
  else if(col %in% c("cos2","contrib", "coord", "x", "y")){
    
    if("point" %in% geom) 
      p <- p + geom_point(data = data, aes_string(x,y, color=col),
                          shape=shape, alpha=alpha, size=pointsize)
    else if("arrow" %in% geom) 
      p <- p + geom_segment(data = data,
                            aes_string(x = 0, y = 0, xend = x, yend = y, color=col),
                            arrow = grid::arrow(length = grid::unit(0.2, 'cm')), alpha = alpha)
    
    if(lab & "text" %in% geom) 
      p <- p + geom_text(data = data,
                         aes_string(x, y, color=col),
                         label = data$name,  size = labelsize, alpha=alpha, vjust = -0.7)
  }
  
  # Only the transparency is controlled automatically
  else if(alpha %in% c("cos2","contrib", "coord", "x", "y")){
    
    if("point" %in% geom) 
      p <- p + geom_point(data = data, 
                          aes_string(x, y, alpha=alpha), 
                          shape=shape, color=col, size = pointsize)
    else if("arrow" %in% geom) 
      p <- p + geom_segment(data = data,
                            aes_string(x = 0, y = 0, xend = x, yend = y, alpha=alpha),
                            arrow = grid::arrow(length = grid::unit(0.2, 'cm')), color=col)
    
    if(lab & "text" %in% geom) 
      p <- p + geom_text(data = data, 
                         aes_string(x, y, alpha=alpha, label="name"),
                         size = labelsize, color=col, vjust = -0.7)
    
    if(!is.null(alpha.limits)) p <- p + scale_alpha(limits = alpha.limits)
  }
  
  else{
    if("point" %in% geom) 
      p <- p + geom_point(data = data, aes_string(x, y),
                          shape=shape, color=col, size = pointsize)
    else if("arrow" %in% geom) 
      p <- p + geom_segment(data = data,
                            aes_string(x = 0, y = 0, xend = x, yend = y),
                            arrow = grid::arrow(length = grid::unit(0.2, 'cm')), color=col)
    if(lab & "text" %in% geom) 
      p <- p + geom_text(data = data, aes_string(x,y), 
                         color = col, label = data$name, size = labelsize, vjust = -0.7)
  }
  
  return(p)
}

# Make arrows from the origine to the point (x, y)
#+++++++++++++++++++++++++++++++++++
## p: a ggplot. If not null, points are added on the existing plot
## data: is a data frame of form: name|x|y|coord|cos2|contrib
## col: point color. The default value is "black".
#  Possible values include also : "cos2", "contrib", "coord", "x" or "y".
#  In this case, the colors for variables are automatically controlled by their qualities ("cos2"),
#  contributions ("contrib"), coordinates (x^2+y2, "coord"), x values("x") or y values("y")
## x, y: contains the name of x and y variables
## alpha: controls the transparency of colors.
# The value can variate from 0 (total transparency) to 1 (no transparency).
# Default value is 1. Possible values include also : "cos2", "contrib", "coord", "x" or "y".
## alpha.limits: the range of alpha values. Used only when alpha is 
# a continuous variable(i.e, cos2, contrib, ...)
## lab: if TRUE points are labelled
## geom: a character specifying the geometry to be used for the graph.
#  Allowed values are "point" (to show only points),
#  "text" to show only labels or c("point", "text") to show both types.
.ggarrow <- function(p = NULL, data, x = 'x', y = 'y', col="black",  alpha = 1,
                       alpha.limits = NULL,
                       shape = "19", 
                       geom=c("arrow", "text"), lab = TRUE, labelsize = 4 )
{
  
  if(is.null(p)) p <- ggplot() 
  # The color and the transparency of variables are automatically controlled by
  # their cos2, contrib,  "x" or "y" coordinates
  if(col %in% c("cos2","contrib", "coord", "x", "y") &
       alpha %in% c("cos2","contrib", "coord", "x", "y"))
  {
    if("point" %in% geom) 
      p <- p + geom_point(data = data, 
                          aes_string(x,y, color=col, alpha=alpha), shape=shape)
    
    if(lab & "text"%in% geom) 
      p <- p + geom_text(data = data, 
                         aes_string(x,y, label = 'name', 
                                    color=col, alpha=alpha), size = labelsize)
    if(!is.null(alpha.limits)) p <- p + scale_alpha(limits = alpha.limits)
  }
  # Only the color is controlled automatically
  else if(col %in% c("cos2","contrib", "coord", "x", "y")){
    
    if("point" %in% geom) 
      p <- p + geom_point(data = data, aes_string(x,y, color=col),
                          shape=shape, alpha=alpha)
    
    if(lab & "text" %in% geom) 
      p <- p + geom_text(data = data,
                         aes_string(x, y, color=col),
                         label = data$name,  size = labelsize, alpha=alpha, vjust = -0.7)
  }
  
  # Only the transparency is controlled automatically
  else if(alpha %in% c("cos2","contrib", "coord", "x", "y")){
    
    if("point" %in% geom) 
      p <- p + geom_point(data = data, 
                          aes_string(x, y, alpha=alpha), shape=shape, color=col)
    
    if(lab & "text" %in% geom) 
      p <- p + geom_text(data = data, 
                         aes_string(x, y, alpha=alpha, label="name"),
                         size = labelsize, color=col, vjust = -0.7)
    
    if(!is.null(alpha.limits)) p <- p + scale_alpha(limits = alpha.limits)
  }
  
  else{
    if("point" %in% geom) 
      p <- p + geom_point(data = data, aes(x, y), shape=shape, color=col)
    if(lab & "text" %in% geom) 
      p <- p + geom_text(data = data, aes_string(x,y), 
                         color = col, label = data$name, size = labelsize, vjust = -0.7)
  }
  
  return(p)
}


# Points jitter, to reduce overploting
# data : a result from facto_summarize containing the x and y coordinates of points
# jitter: a vector of length 2 containing the width and the height of jitter
.jitter <- function(data, jitter = list(width = NULL, height = NULL)){
  
  if(!is.null(data)){
    if(!is.null(jitter$width)){
      width <- abs(jitter$width)
      set.seed(1234)
      xjit <- runif(nrow(data), min = -width, max = width)
      data$x <- data$x + xjit
    }
    
    if(!is.null(jitter$height)){
      height <- abs(jitter$height)
      set.seed(12345)
      yjit <- runif(nrow(data), min = -height, max = height)
      data$y <- data$y + yjit
    }
  }
  
  return(data)
}


# Finih a plot
#++++++++++++++++++++++++++++++
# p a ggplot2
# X an object of class PCA, MCA or CA
# axes the plotted axes
.fviz_finish <- function(p, X, axes = 1:2){
  
  cc <- .get_facto_class(X)
  title <- paste0(cc, " factor map")
  
  eig <- get_eigenvalue(X)[,2]
  xlab = paste0("Dim", axes[1], " (", round(eig[axes[1]],1), "%)") 
  ylab = paste0("Dim", axes[2], " (", round(eig[axes[2]], 1),"%)")
  
  p <- p +
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_vline(xintercept = 0, color = "black", linetype="dashed") +
    labs(title = title, x = xlab, y = ylab)
  
  return(p)
}

# Check the element to be labelled
#+++++++++++++++++
## label: a character vector specifying the elements to be labelled
# possible values are "all", "none" or the combination of 
# c("row", "row.sup", "col", "col.sup", "ind", "ind.sup", "quali", "var", "quanti.sup")
## Returns a list 
.label <- function(label){
  lab  <- list()
  # var - PCA, MCA
  lab$var <- lab$quanti <- lab$quali.sup <- FALSE
  if(label[1]=="all" | "var" %in% label) lab$var =TRUE
  if(label[1]=="all" | "quanti.sup" %in% label) lab$quanti =TRUE
  if(label[1]=="all" | "quali.sup" %in% label) lab$quali.sup =TRUE
  # ind - PCA, MCA
  lab$ind <- lab$ind.sup <- lab$quali <- FALSE
  if(label[1]=="all" | "ind" %in% label) lab$ind =TRUE
  if(label[1]=="all" | "ind.sup" %in% label) lab$ind.sup =TRUE
  if(label[1]=="all" | "quali" %in% label) lab$quali =TRUE
  # row - ca
  lab$row <- lab$row.sup <- FALSE
  if(label[1]=="all" | "row" %in% label) lab$row =TRUE
  if(label[1]=="all" | "row.sup" %in% label) lab$row.sup =TRUE
  # col - ca
  lab$col <- lab$col.sup <- FALSE
  if(label[1]=="all" | "col" %in% label) lab$col =TRUE
  if(label[1]=="all" | "col.sup" %in% label) lab$col.sup =TRUE
  
  lab
}


# Check the element to be hidden
#+++++++++++++++++
## invisible: a character vector specifying the elements to be hidden
# possible values are "all", "none" or the combination of 
# c("row", "row.sup", "col", "col.sup", "ind", "ind.sup", "quali", "quali.sup",  "var", "quanti.sup")
## Returns a list 
.hide <- function(invisible){
  hide  <- list()
  # var - PCA, MCA
  hide$var <- hide$quanti <- hide$quali.sup <- FALSE
  if("var" %in% invisible) hide$var =TRUE
  if("quanti.sup" %in% invisible) hide$quanti =TRUE
  if("quali.sup" %in% invisible) hide$quali.sup = TRUE
  # ind - PCA, MCA
  hide$ind <- hide$ind.sup <- hide$quali <- FALSE
  if("ind" %in% invisible) hide$ind =TRUE
  if("ind.sup" %in% invisible) hide$ind.sup =TRUE
  if("quali" %in% invisible) hide$quali =TRUE
  # row - ca
  hide$row <- hide$row.sup <- FALSE
  if("row" %in% invisible) hide$row =TRUE
  if("row.sup" %in% invisible) hide$row.sup =TRUE
  # col - ca
  hide$col <- hide$col.sup <- FALSE
  if("col" %in% invisible) hide$col =TRUE
  if("col.sup" %in% invisible) hide$col.sup =TRUE
  
  hide
}

