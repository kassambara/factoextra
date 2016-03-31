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
#' prcomp and princomp [stats]; pca, dudi [adea4].
#' @param element the element to subset from the output. Allowed values are 
#' "var" (for active variables) or "ind" (for active individuals).
#' @param ... not used
#' @return a list of matrices containing all the results for the active individuals/variables including: 
#' \item{coord}{coordinates for the individuals/variables}
#' \item{cos2}{cos2 for the individuals/variables}
#' \item{contrib}{contributions of the individuals/variables}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
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
  elmt <- element[1]
  if(elmt =="var") get_pca_var(res.pca)
  else if(elmt == "ind") get_pca_ind(res.pca)
  else stop("Allowed values for the argument element are: 'ind' or 'var'.")
}

#' @rdname get_pca
#' @export
get_pca_ind<-function(res.pca, ...){
  
  # FactoMineR package
  if(inherits(res.pca, c('PCA'))) ind <- res.pca$ind
  
  # ade4 package
  else if(inherits(res.pca, 'pca') & inherits(res.pca, 'dudi')){  
    ind.coord <- res.pca$li
    # get the original data
    data <- res.pca$tab
    data <- t(apply(data, 1, function(x){x*res.pca$norm} ))
    data <- t(apply(data, 1, function(x){x+res.pca$cent}))
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
    # Correlation of variables with the principal component
    var_cor_func <- function(var.loadings, comp.sdev){var.loadings*comp.sdev}
    var.cor <- t(apply(res.pca$loadings, 1, var_cor_func, res.pca$sdev))
    var <- .get_pca_var_results(var.cor)
  }
  else if(inherits(res.pca, 'prcomp')){
    # Correlation of variables with the principal component
    var_cor_func <- function(var.loadings, comp.sdev){var.loadings*comp.sdev}
    var.cor <- t(apply(res.pca$rotation, 1, var_cor_func, res.pca$sdev))
    var <- .get_pca_var_results(var.cor)
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
.get_pca_ind_results <- function(ind.coord, data, eigenvalues, pca.center, pca.scale ){
  
  eigenvalues <- eigenvalues[1:ncol(ind.coord)]
  
  if(pca.center[1] == FALSE) pca.center <- rep(0, ncol(data))
  if(pca.scale[1] == FALSE) pca.scale <- rep(1, ncol(data))
  
  # Compute the square of the distance between an individual and the
  # center of gravity
  getdistance <- function(ind_row, center, scale){
    return(sum(((ind_row-center)/scale)^2))
  }
  d2 <- apply(data, 1,getdistance, pca.center, pca.scale)
  
  # Compute the cos2
  cos2 <- function(ind.coord, d2){return(ind.coord^2/d2)}
  ind.cos2 <- apply(ind.coord, 2, cos2, d2)
  
  # Individual contributions 
  contrib <- function(ind.coord, eigenvalues, n.ind){
    100*(1/n.ind)*(ind.coord^2/eigenvalues)
  }
  ind.contrib <- t(apply(ind.coord, 1, contrib,  eigenvalues, nrow(ind.coord)))
  
  colnames(ind.coord) <- colnames(ind.cos2) <-
    colnames(ind.contrib) <- paste0("Dim.", 1:ncol(ind.coord)) 
  
  rnames <- rownames(ind.coord)
  if(is.null(rnames)) rnames <- as.character(1:nrow(ind.coord))
  rownames(ind.coord) <- rownames(ind.cos2) <- rownames(ind.contrib) <- rnames
  
  # Individuals coord, cos2 and contrib
  ind = list(coord = ind.coord,  cos2 = ind.cos2, contrib = ind.contrib)
  ind
}

# compute all the results for variables : coord, cor, cos2, contrib
# var.coord : coordinates of variables on the principal component
.get_pca_var_results <- function(var.coord){
  
  var.cor <- var.coord # correlation
  var.cos2 <- var.cor^2 # variable qualities 
  
  # variable contributions (in percent)
  # var.cos2*100/total Cos2 of the component
  comp.cos2 <- apply(var.cos2, 2, sum)
  contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
  var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
  
  colnames(var.coord) <- colnames(var.cor) <- colnames(var.cos2) <-
    colnames(var.contrib) <- paste0("Dim.", 1:ncol(var.coord)) 
  
  # Variable coord, cor, cos2 and contrib
  list(coord = var.coord, cor = var.cor, cos2 = var.cos2, contrib = var.contrib)
}
