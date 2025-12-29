#' @include eigenvalue.R fviz_add.R
NULL
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom ggrepel geom_text_repel
#' @importFrom grDevices chull
#' @importFrom graphics plot
#' @importFrom utils data
#' @importFrom stats as.dendrogram
#' @importFrom stats as.dist
#' @importFrom stats kmeans
#' @importFrom stats na.omit
#' @importFrom stats qchisq
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats var
#'

# Helper function to check if a palette name is a predefined color palette
# Replaces ggpubr:::.is_col_palette to avoid using unexported functions
# Known palette names from RColorBrewer, ggsci, and ggpubr defaults
.is_color_palette <- function(pal) {
 if (is.null(pal)) return(FALSE)
 # RColorBrewer palettes
 brewer_pals <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
                  "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
                  "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "BrBG", "PiYG", "PRGn",
                  "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral",
                  "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
                  "Set2", "Set3")
 # ggsci palettes
 ggsci_pals <- c("npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb",
                 "d3", "locuszoom", "igv", "uchicago", "startrek", "tron",
                 "futurama", "rickandmorty", "simpsons")
 # ggpubr built-in
 other_pals <- c("default", "hue", "grey_pal", "gray_pal")
 all_pals <- c(brewer_pals, ggsci_pals, other_pals)
 return(length(pal) == 1 && pal[1] %in% all_pals)
}

# Check and get the class of the output of a factor analysis
# ++++++++++++++++++++++++++++
# X: an output of factor analysis (PCA, CA, MCA, MFA) 
# from different packages (FactoMineR, ade4, ....) 
.get_facto_class <- function(X){
  
  if(inherits(X, c('PCA', 'princomp', 'prcomp')))
    facto_class ="PCA"
  else if(inherits(X, 'pca') & inherits(X, 'dudi'))
    facto_class ="PCA"
  else if(inherits(X, c("CA", "ca", "coa", "correspondence"))) facto_class="CA"
  else if(inherits(X, c("MCA", "acm"))) facto_class = "MCA"
  else if(inherits(X, c("MFA","mfa"))) facto_class = "MFA"
  else if(inherits(X, c("HMFA"))) facto_class = "HMFA"
  else if(inherits(X, c("FAMD"))) facto_class = "FAMD"
  else if (inherits(X, "expoOutput")){
   if (inherits(X$ExPosition.Data,'epCA')) facto_class="CA"
   else if (inherits(X$ExPosition.Data,'epPCA')) facto_class="PCA"
   else if (inherits(X$ExPosition.Data,'epMCA')) facto_class="MCA"
  }
  else stop("An object of class : ", class(X), 
            " can't be handled by factoextra")   
}

# Get the result for supplementary points
#++++++++++++++++++++++++++
## X: an output of factor analysis (PCA, CA, MCA, MFA, HMFA) 
## element possible values are "row.sup", "col.sup" (CA);
# quanti, ind.sup (PCA); quali.sup (MCA); quanti.var.sup, quali.var.sup (MFA)
## result the result tobe extracted for the element. Possible values are
#  the combination of c("cos2", "coord")
## axes a numeric vector specifying the axes of interest. Default values are 1:2
##  for axes 1 and 2
## select: a selection of variables. See the function .select()
.get_supp <- function(X, element = NULL, axes = 1:2, 
                      result = c("coord", "cos2"), select = NULL){
  
  if(inherits(X, "MFA") & element == "group")
    elmt <- .get_mfa_group_sup(X)
  else if(inherits(X, c("CA", "PCA", "MCA", "MFA", "HMFA", "FAMD"))) {
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
    if(inherits(X, "MCA")){
      if(element %in% c("quanti.sup", "quali.sup$eta2")) result <- "coord"
      if(element == "quanti.sup") elmt$coord <- elmt$coord^2
      else if(element == "quali.sup$eta2") elmt<-list(coord = elmt)
    }
    # check axes
    if(max(axes) > ncol(elmt$coord))
      stop("The value of the argument axes is incorrect. ",
           "The number of axes in the data is: ", ncol(elmt$coord), 
           ". Please try again with axes between 1 - ", ncol(elmt$coord))
    
    # 1.Extract the coordinates x, y and coord
    if("coord" %in% result){
      dd <- data.frame(elmt$coord[, axes, drop=FALSE], stringsAsFactors = TRUE)
      coord <- apply(dd^2, 1, sum) # x^2 + y2 + ...
      res <- cbind(dd, coord = coord)
    }
    
    # 2. Extract the cos2
    if("cos2" %in% result){
      cos2 <- data.frame(elmt$cos2[, axes, drop=FALSE], stringsAsFactors = TRUE)
      if(length(axes) > 1) cos2 <- apply(cos2, 1, sum, na.rm=TRUE)
      res <- cbind(res, cos2 = cos2)
    }
    
    res <- cbind.data.frame(name =rownames(elmt$coord), res, stringsAsFactors = TRUE)
    
    # selection of variables
    if(!is.null(select)){
      if(!is.null(select$contrib)) res <- NULL # supp points don't have contrib
      else res <- .select(res, select, check = FALSE)
    }
  }
  if(!is.null(res)){
    if(nrow(res) == 0) res <- NULL
  }
  res
}


# Add supplementary elements to a plot
# p a ggplot
# X an object from FactoMiner
# element: "ind.sup", "quanti"
# axes: axes of interest
# scale.: numeric, the data is multiplied by scale. before ploting
# select: a selection of individuals/variables to be drawn.
# ca_map: applied to (m)ca only, see fviz_ca map argument
.add_supp <- function(p, X, element, axes, select, scale.=1, 
                       ca_map = NULL,
                      ...){
  for(el in element){
    dd <- .get_supp(X, element = el, axes = axes, select = select)
    
    if(!is.null(dd)){
      colnames(dd)[2:3] <-  c("x", "y")
      if(!is.null(ca_map)) dd <- .scale_ca(dd, res.ca = X,  element = el, 
                           type = ca_map, axes = axes)
      p <- fviz_add(p, df = dd[, 2:3, drop = FALSE]*scale., ...)
    }
  }
  p
}

# get supplementary columns from ca output (ca package)
#
# OPTIMIZATION: Replaced t(apply()) with sweep() for column-wise multiplication
# sweep() is much faster for element-wise row/column operations
.get_ca_col_sup <- function(res.ca){
  # supplementary points
  index <- res.ca$colsup
  cols <- NULL
  if(length(index) > 0){
    # OPTIMIZED: principal coord = standard coord X sqrt(eig)
    # Using sweep() instead of t(apply()) - multiply each column by sv
    coord <- sweep(res.ca$colcoord, 2, res.ca$sv, "*")
    # OPTIMIZED: cos2 = coord^2 / dist^2 (divide each row by its distance squared)
    cos2 <- coord^2 / res.ca$coldist^2
    cols <- list(coord = coord[index, , drop = FALSE],
                cos2 = cos2[index, , drop = FALSE])
  }
  cols
}

# get supplementary rows from ca output (ca package)
#
# OPTIMIZATION: Replaced t(apply()) with sweep() for column-wise multiplication
.get_ca_row_sup <- function(res.ca){
  rows <- NULL
  # supplementary points
  index <- res.ca$rowsup
  if(length(index) > 0){
    # OPTIMIZED: principal coord = standard coord X sqrt(eig)
    coord <- sweep(res.ca$rowcoord, 2, res.ca$sv, "*")
    # OPTIMIZED: cos2 = coord^2 / dist^2
    cos2 <- coord^2 / res.ca$rowdist^2
    rows <- list(coord = coord[index, , drop = FALSE],
                                     cos2 = cos2[index, , drop = FALSE])
  }
  rows
}

# get supplementary groups from MFA
.get_mfa_group_sup <- function(res.mfa){
  res <- NULL
  # supplementary points
  coord <- res.mfa$group$coord.sup
  if(!is.null(coord)){
    res <- list(coord = coord, cos2 = res.mfa$group$cos2.sup) 
  }
 res
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
  # ExPosition
  else if (inherits(res.ca, "expoOutput")){
    if(element =="row") mass <- res.ca$ExPosition.Data$M
    else if(element =="col") mass <- res.ca$ExPosition.Data$W
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
  else if(element=="quanti.var")
    title <- paste0(varname, " of quantitive variables to Dim-", paste(axes, collapse="-"))
  else if(element=="quali.var")
    title <- paste0(varname, " of qualitive variables to Dim-", paste(axes, collapse="-"))
  else if(element=="group")
    title <- paste0(varname, " of groups to Dim-", paste(axes, collapse="-"))
  else if(element=="partial.axes")
    title <- paste0(varname, " of partial axes to Dim-", paste(axes, collapse="-"))
  
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
  d <- cbind.data.frame(name = factor(names(x), levels = names(x)), val = x, stringsAsFactors = TRUE)
  
  # plot
  # FIX: ggplot2 3.0.0+ deprecation - aes_string() replaced with aes() + .data pronoun
  # aes_string() was soft-deprecated in ggplot2 3.0.0
  # See: https://github.com/kassambara/factoextra/issues/190
  p <- ggplot(d, aes(x = .data[["name"]], y = .data[["val"]])) +
    geom_bar(stat="identity", fill=fill, color = color) +
    labs(title = title,  x =xlab, y = ylab)+
    theme(axis.text.x = element_text(angle=45), 
                   axis.title.x = element_blank())
  
  return(p)
}



# Points jitter, to reduce overploting
# data : a result from facto_summarize containing the x and y coordinates of points
# jitter: a vector of length 3 containing the width and the height of jitter and the 
# element to be jittered ("label", "point", "both")
.jitter <- function(data, jitter = list(what = "label", width = NULL, height = NULL)){
  
  if(!is.null(data)){
    if(!is.null(jitter$width)){
      width <- abs(jitter$width)
     set.seed(1234)
      xjit <- runif(nrow(data), min = -width, max = width)
      data$x <- data$x + ((xjit + rev(xjit))/2)
    }
    
    if(!is.null(jitter$height)){
      height <- abs(jitter$height)
       set.seed(12345)
      yjit <- runif(nrow(data), min = -height, max = height)
      data$y <- data$y + ((yjit + rev(yjit))/2)
    }
    
  }
  
  return(data)
}



# Finih a plot
#++++++++++++++++++++++++++++++
# p a ggplot2
# X an object of class PCA, MCA or CA
# axes the plotted axes
.fviz_finish <- function(p, X, axes = 1:2, linetype = "dashed", xlab = NULL, ylab=NULL, ...){
  
  cc <- .get_facto_class(X)
  title <- paste0(cc, " factor map")
  
  eig <- get_eigenvalue(X)[,2]
  if(is.null(xlab)) xlab = paste0("Dim", axes[1], " (", round(eig[axes[1]],1), "%)") 
  if(is.null(ylab)) ylab = paste0("Dim", axes[2], " (", round(eig[axes[2]], 1),"%)")
  
  p <- p +
    geom_hline(yintercept = 0, color = "black", linetype=linetype) +
    geom_vline(xintercept = 0, color = "black", linetype=linetype) +
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
  element <- c("var", "quanti.sup", "quali.sup", "quanti.sup","quanti", # var - PCA, MCA, MFA
               "quanti.var.sup", "quanti.var", "quali.var", # MFA
               "ind", "ind.sup", "quali", "mca.cor",  # ind - PCA, MCA, MFA
               "group", "group.sup", # group - MFA, HMFA
               "partial.axes", # partial.axes - MFA
               "row", "row.sup", # row - ca
               "col", "col.sup" # col - ca
               )
  for(el in element){
    if(label[1] == "all" | el %in% label) lab[[el]] <- TRUE
    else lab[[el]] <- FALSE
  }
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
  element <- c("var", "quanti.sup", "quali.sup", "quanti.sup","quanti", # var - PCA, MCA, MFA
               "quanti.var.sup", "quanti.var",  "quali.var", # MFA
               "ind", "ind.sup", "quali", "mca.cor",  # ind - PCA, MCA, MFA
               "group", "group.sup", # group - MFA, HMFA
               "partial.axes", # partial.axes - MFA
               "row", "row.sup", # row - ca
               "col", "col.sup" # col - ca
  )
  for(el in element){
    if(el %in% invisible) hide[[el]] <- TRUE
    else hide[[el]] <- FALSE
  }
  hide
}

# Generate a data containing a cluster of any shapes
# For comparison between dbscan and k-means
.generate_multishapes <- function(){
  # First circle (big)
  set.seed(1234)
  x <- matrix(rnorm(800, sd = 2), ncol=2)
  y <- x/sqrt(rowSums(x^2))
  y[,1] <- y[,1] + rnorm(400, 0, 0.1)
  y[,2] <- y[,2] + rnorm(400, 0, 0.1)
  x1 <- y[, 1]
  y1 <- y[, 2]
  # Second circle (small)
  x2 <- x1/2.5
  y2 <- y1/2.5
  shape <- rep(c(1,2), each = 400)
  # Line 1
  x3 <- runif(100, min = -1.5, 0)
  y3 <- rnorm(100, -2, 0.1)
  shape <- c(shape, rep(3, 100))
  # Line 2
  x4 <- runif(100, min = -1.5, 0)
  y4 <- rnorm(100, -3, 0.1)
  shape <- c(shape, rep(4, 100))
  # compact points
  x6 <- rnorm(50, 1, 0.1)
  y6 <- rnorm(50, -2.5, 0.1)
  shape <- c(shape, rep(5, 50))
  # noises/outliers
  x5 <- runif(50, min = -1.5, 1.5)
  y5 <- rnorm(50, -1, 1)
  shape <- c(shape, rep(6, 50))
  multishapes <- data.frame(x = c(x1, x2, x3, x4, x5, x6), y = c(y1, y2, y3, y4, y5, y6), shape = shape, stringsAsFactors = TRUE)
  multishapes
}


# Deprecated argument
.facto_dep <- function(arg, replace, return_val){
    warning("argument ", arg, " is deprecated; please use ", replace, " instead.", 
            call. = FALSE)
  return_val
}

# Check axis lengths
.check_axes <- function(axes, .length){
  if(length(axes) != .length) stop("axes should be of length ", 2)
}

# Add individual groups column
# x an object of class PCA, MCA, ...
# ind: individuals data generated by facto_summarize()
# grp: group column index or factor
.add_ind_groups <- function(X, ind, grp){
  if(inherits(X, c("PCA", "MCA", "MFA", "FAMD")) & length(grp) > 1){
    if(is.numeric(grp) | is.character(grp)) grp <- as.data.frame(X$call$X[rownames(ind), grp, drop = FALSE], stringsAsFactors = TRUE)
    #if(!is.null(X$call$ind.sup)) grp <- grp[-X$call$ind.sup, , drop = FALSE]
  }
  habillage <- grp
  # Data frame containing multiple grouping variables
  if(inherits(grp, c("matrix", "data.frame"))){
    if(nrow(ind) != nrow(grp)) stop("The length of grouping variables ",
                                    "should be the same as the number of individuals.")
    ind <- cbind.data.frame(ind, grp, stringsAsFactors = TRUE)
    ind[, colnames(grp)] <- apply(ind[, colnames(grp)], 2, as.character)
    # Convert wide to long format using base R (replaces tidyr::pivot_longer)
    grp_cols <- colnames(grp)
    id_cols <- setdiff(colnames(ind), grp_cols)
    ind <- stats::reshape(ind, direction = "long",
                          varying = grp_cols,
                          v.names = "Groups",
                          timevar = "facet_vars",
                          times = grp_cols,
                          idvar = id_cols)
    rownames(ind) <- NULL
    ind$facet_vars <- as.factor(ind$facet_vars)
    ind$Groups <- as.factor(ind$Groups)
    name.quali <- "Groups"
  }
  else{
    # X is from FactoMineR outputs
    if(inherits(X, c("PCA", "MCA", "MFA", "FAMD")) & length(habillage) == 1){
      data <- X$call$X
      if (is.numeric(habillage)) name.quali <- colnames(data)[habillage]
      else name.quali <- habillage 
      ind <- cbind.data.frame(data[rownames(ind),name.quali], ind, stringsAsFactors = TRUE)
      colnames(ind)[1]<-name.quali
      if(!inherits(ind[, 1], "factor")) ind[, 1]<-as.factor(ind[,1])
    }
    else{
      if(nrow(ind)!=length(habillage))
        stop("The number of active individuals is different ",
             "from the length of the factor habillage. Please, remove the supplementary ",
             "individuals in the variable habillage.")
      name.quali <- "Groups"
      ind <- cbind.data.frame(Groups = habillage, ind, stringsAsFactors = TRUE)
      if(!inherits(ind[, 1], "factor")) ind[, 1] <- as.factor(ind[,1])
    }
  }
  
  list(ind = ind, name.quali = name.quali, is_multiple_habillage = is.data.frame(grp))
}


# MFA: get quantitative variables groups
# For plotting
.get_quanti_var_groups <- function(X){
  group <- data.frame(name = rownames(X$group$Lg[-nrow(X$group$Lg),,drop=FALSE]),
                      nvar = X$call$group, type = X$call$type, stringsAsFactors = TRUE)
  is.group.sup <- !is.null(X$call$num.group.sup)
  if(is.group.sup) group <- group[-X$call$num.group.sup, , drop = FALSE]
  group <- subset(group, group$type == "c")
  group <- rep(group$name, group$nvar)
  group
}

# Get qualitative supplementary variables
# Each variables is repeated xtimes = levels(variables)
# Used to color variable categories by variables
.get_quali_var_sup_names <- function(X){
  group <- data.frame(name = rownames(X$group$Lg[-nrow(X$group$Lg),,drop=FALSE]),
                      nvar = X$call$group, type = X$call$type, stringsAsFactors = TRUE)
  is.group.sup <- !is.null(X$call$num.group.sup)
  res <- NULL
  if(is.group.sup){
    # Get group sup names
    group.sup <- group[X$call$num.group.sup, , drop = FALSE]
    group.sup <- subset(group.sup, group.sup$type == "n", drop = FALSE)
    group.sup.name <- as.character(group.sup$name)
    # Names of variables in the data
    data <- X$call$X
    vars <- colnames(data)
    vars.groups <- rep(group$name, group$nvar)
    group.sup.vars <- vars[vars.groups %in% group.sup.name]
    for(v in group.sup.vars){
      res <- c(res, rep(v, length(levels(data[,v]))))
    }
  }
  res
}


# Principal component methods with Fcatominer
f_pca <- function(X, graph = FALSE){
  FactoMineR::PCA(X, graph = FALSE)
}

