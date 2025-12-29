#' @include print.factoextra.R utilities.R
NULL
#' Extract the results for individuals/variables - PCA
#' 
#' @description
#' Extract all the results (coordinates, squared cosine, contributions) for 
#' the active individuals/variables from Principal Component Analysis (PCA) outputs.\cr\cr
#' \itemize{
#' \item get_pca(): Extract the results for variables and individuals
#' \item get_pca_ind(): Extract the results for individuals only
#' \item get_pca_var(): Extract the results for variables only
#' }
#' @param res.pca an object of class PCA [FactoMineR]; 
#' prcomp and princomp [stats]; pca, dudi [adea4]; epPCA [ExPosition].
#' @param element the element to subset from the output. Allowed values are 
#' "var" (for active variables) or "ind" (for active individuals).
#' @param ... not used
#' @return a list of matrices containing all the results for the active individuals/variables including: 
#' \item{coord}{coordinates for the individuals/variables}
#' \item{cos2}{cos2 for the individuals/variables}
#' \item{contrib}{contributions of the individuals/variables}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com/english/
#' @examples
#' \donttest{
#' # Principal Component Analysis
#' # +++++++++++++++++++++++++++++
#'  data(iris)
#'  res.pca <- prcomp(iris[, -5],  scale = TRUE)
#'  # Extract the results for individuals
#'  ind <- get_pca_ind(res.pca)
#'  print(ind)
#'  head(ind$coord) # coordinates of individuals
#'  head(ind$cos2) # cos2 of individuals
#'  head(ind$contrib) # contributions of individuals
#'  
#'  # Extract the results for variables
#'  var <- get_pca_var(res.pca)
#'  print(var)
#'  head(var$coord) # coordinates of variables
#'  head(var$cos2) # cos2 of variables
#'  head(var$contrib) # contributions of variables
#'  
#'  # You can also use the function get_pca()
#'  get_pca(res.pca, "ind") # Results for individuals
#'  get_pca(res.pca, "var") # Results for variable categories
#'  }
#' @name get_pca
#' 
#' @rdname get_pca
#' @export 
get_pca <- function(res.pca, element = c("var", "ind")){
  elmt <- match.arg(element)
  if(elmt =="var") get_pca_var(res.pca)
  else if(elmt == "ind") get_pca_ind(res.pca)
}

#' @rdname get_pca
#' @export
get_pca_ind<-function(res.pca, ...){
  
  # FactoMineR package
  if(inherits(res.pca, c('PCA'))) ind <- res.pca$ind
  
  # ade4 package
  else if(inherits(res.pca, 'pca') & inherits(res.pca, 'dudi')){
    ind.coord <- res.pca$li
    # OPTIMIZED: get the original data using vectorized sweep() instead of apply()
    # sweep() is much faster than t(apply()) for element-wise row operations
    data <- res.pca$tab
    data <- sweep(data, 2, res.pca$norm, "*")
    data <- sweep(data, 2, res.pca$cent, "+")
    ind <- .get_pca_ind_results(ind.coord, data, res.pca$eig,
                                res.pca$cent, res.pca$norm)
  }
  
  # stats package
  else if(inherits(res.pca, 'princomp')){  
    ind.coord <- res.pca$scores
    data <- .prcomp_reconst(res.pca)
    ind <- .get_pca_ind_results(ind.coord, data, res.pca$sdev^2,
                                res.pca$center, res.pca$scale)
    
  }
  else if(inherits(res.pca, 'prcomp')){
    ind.coord <- res.pca$x
    data <- .prcomp_reconst(res.pca)
    ind <- .get_pca_ind_results(ind.coord, data, res.pca$sdev^2,
                                res.pca$center, res.pca$scale)
  }
  # ExPosition package
  else if (inherits(res.pca, "expoOutput") & inherits(res.pca$ExPosition.Data,'epPCA')){
    res <- res.pca$ExPosition.Data
    ind <- list(coord = res$fi,  cos2 = res$ri, contrib = res$ci*100)
  }
  else stop("An object of class : ", class(res.pca), 
            " can't be handled by the function get_pca_ind()")
  
  class(ind)<-c("factoextra", "pca_ind")
  
  ind
}


#' @rdname get_pca
#' @export 
get_pca_var<-function(res.pca){
  # FactoMineR package
  if(inherits(res.pca, c('PCA'))) var <- res.pca$var
  # ade4 package
  else if(inherits(res.pca, 'pca') & inherits(res.pca, 'dudi')){
    var <- .get_pca_var_results(res.pca$co)
  }
  # stats package
  else if(inherits(res.pca, 'princomp')){
    # OPTIMIZED: Correlation of variables with the principal component
    # Using sweep() instead of t(apply()) - much faster for element-wise operations
    # var.cor[i,j] = loadings[i,j] * sdev[j]
    var.cor <- sweep(as.matrix(res.pca$loadings), 2, res.pca$sdev, "*")
    var <- .get_pca_var_results(var.cor)
  }
  else if(inherits(res.pca, 'prcomp')){
    # OPTIMIZED: Correlation of variables with the principal component
    # Using sweep() instead of t(apply()) - much faster for element-wise operations
    # var.cor[i,j] = rotation[i,j] * sdev[j]
    var.cor <- sweep(res.pca$rotation, 2, res.pca$sdev, "*")
    var <- .get_pca_var_results(var.cor)
  }
  # ExPosition package
  else if (inherits(res.pca, "expoOutput") & inherits(res.pca$ExPosition.Data,'epPCA')){
    res <- res.pca$ExPosition.Data
    data_matrix <- res$X
    factor_scores <- res$fi
    var.coord <- var.cor <- stats::cor(res$X, res$fi) # cor(t(data_matrix), factor_scores)
    var.coord <- replace(var.coord, is.na(var.coord), 0)
    var <- list(coord = var.coord, cor = var.coord, cos2 = res$rj, contrib = res$cj*100)
  }
  else stop("An object of class : ", class(res.pca), 
            " can't be handled by the function get_pca_var()")
  class(var)<-c("factoextra", "pca_var")
  var
}




# Helper functions
#++++++++++++++++++++

# compute all the results for individuals : coord, cor, cos2, contrib
# ind.coord : coordinates of variables on the principal component
# pca.center, pca.scale : numeric vectors corresponding to the pca
# center and scale respectively
# data : the orignal data used during the pca analysis
# eigenvalues : principal component eigenvalues
#
# OPTIMIZATION: Replaced apply() loops with vectorized matrix operations
# - Uses sweep() and rowSums() instead of row-wise apply()
# - Avoids repeated function calls and memory allocations
# - Maintains full econometric precision (identical numerical results)
.get_pca_ind_results <- function(ind.coord, data, eigenvalues, pca.center, pca.scale){

  eigenvalues <- eigenvalues[1:ncol(ind.coord)]
  n.ind <- nrow(ind.coord)
  n.dim <- ncol(ind.coord)

  # Handle centering/scaling defaults
  if(isFALSE(pca.center[1])) pca.center <- rep(0, ncol(data))
  if(isFALSE(pca.scale[1])) pca.scale <- rep(1, ncol(data))

  # OPTIMIZED: Compute squared distances using vectorized operations
  # d2[i] = sum(((data[i,] - center) / scale)^2)
  # Using sweep() for center/scale and rowSums() for summation
  data_centered <- sweep(data, 2, pca.center, "-")
  data_scaled <- sweep(data_centered, 2, pca.scale, "/")
  d2 <- rowSums(data_scaled^2)

  # OPTIMIZED: Compute cos2 using vectorized division

  # cos2[i,j] = coord[i,j]^2 / d2[i]
  ind.coord.sq <- ind.coord^2
  ind.cos2 <- ind.coord.sq / d2  # Recycling divides each column by d2

  # OPTIMIZED: Compute contributions using vectorized operations
  # contrib[i,j] = 100 * (1/n) * (coord[i,j]^2 / eigenvalue[j])
  # Use sweep to divide each column by its eigenvalue
  ind.contrib <- sweep(ind.coord.sq, 2, eigenvalues, "/") * (100 / n.ind)

  # Set column and row names
  dim.names <- paste0("Dim.", 1:n.dim)
  colnames(ind.coord) <- colnames(ind.cos2) <- colnames(ind.contrib) <- dim.names

  rnames <- rownames(ind.coord)
  if(is.null(rnames)) rnames <- as.character(1:n.ind)
  rownames(ind.coord) <- rownames(ind.cos2) <- rownames(ind.contrib) <- rnames

  # Return results
  list(coord = ind.coord, cos2 = ind.cos2, contrib = ind.contrib)
}

# compute all the results for variables : coord, cor, cos2, contrib
# var.coord : coordinates of variables on the principal component
#
# OPTIMIZATION: Replaced apply() with colSums() and sweep()
# - colSums() is faster than apply(x, 2, sum)
# - sweep() is faster than t(apply()) for column-wise operations
# - Maintains full econometric precision
.get_pca_var_results <- function(var.coord){

  # Preserve row names from input
  rnames <- rownames(var.coord)

  var.cor <- var.coord # correlation
  var.cos2 <- var.cor^2 # variable qualities

  # OPTIMIZED: variable contributions (in percent)
  # var.cos2*100/total Cos2 of the component
  # Using colSums instead of apply(, 2, sum) - much faster

  comp.cos2 <- colSums(var.cos2)

  # OPTIMIZED: Using sweep instead of t(apply()) for column-wise division
  # contrib[i,j] = var.cos2[i,j] * 100 / comp.cos2[j]
  var.contrib <- sweep(var.cos2, 2, comp.cos2, "/") * 100

  # Set column names
  dim.names <- paste0("Dim.", 1:ncol(var.coord))
  colnames(var.coord) <- colnames(var.cor) <- colnames(var.cos2) <-
    colnames(var.contrib) <- dim.names

  # Preserve row names
  if(!is.null(rnames)) {
    rownames(var.coord) <- rownames(var.cor) <- rownames(var.cos2) <-
      rownames(var.contrib) <- rnames
  }

  # Variable coord, cor, cos2 and contrib
  list(coord = var.coord, cor = var.cor, cos2 = var.cos2, contrib = var.contrib)
}
