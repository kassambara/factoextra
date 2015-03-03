#' Extract the results for the individuals after a principal component analysis
#' 
#' @description
#' Ectract all the results for the active individuals from a principal component analysis output.
#'  This output contains individuals coordinates, square cosine and contributions.
#'  The allowed PCA outputs are the ones from FactoMineR (PCA),
#'  stats (princomp() and prcomp()), ade4 (dudi.pca()) packages.
#' @param res.pca an object of class PCA (from FactoMineR);
#'  prcomp and princomp (from stats package); pca, dudi (dudi.pca, from adea4 package).
#'  @param data the original data used for the pca. 
#'   This argument is required only when res.pca is not from FactoMineR.
#'   It's used to calculate the cos2 of the individuals.
#' @return a list of matrices containing all the results for the active individuals including : 
#' \item{coord}{coordinates for the individuals}
#' \item{cos2}{cos2 for the individuals}
#' \item{contrib}{contributions of the individuals}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#'  res.pca <- princomp(iris[, -5],  cor = TRUE)
#'  ind <- get_pca_ind(res.pca, data = iris[, -5])
#'  
#'  head(ind$coord)
#'  
#'  head(ind$cos2)
#'  
#'  head(ind$contrib)
#'  }
#'  
#' @export
get_pca_ind<-function(res.pca, data = NULL){
  
  # FactoMineR package
  if(inherits(res.pca, 'PCA')) ind <- res.pca$ind
  
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
    if(is.null(data)) { 
      data.name <- readline(
        paste0("The original data used during the PCA analysis are required. ",
               "What's the name of the object data ? : ")
      )
      data <- eval(parse(text=data.name))
      if(is.null(data)) stop("The argument data is NULL. ",
                             "The original data used for the pca analysis are required.")
    }
    
    ind <- .get_pca_ind_results(ind.coord, data, res.pca$sdev^2,
                                res.pca$center, res.pca$scale)
    
  }
  else if(inherits(res.pca, 'prcomp')){
    ind.coord <- res.pca$x
    if(is.null(data)) { 
      data.name <- readline(
        paste0("The original data used during the PCA analysis are required. ",
               "What's the name of the object data ? : ")
      )
      data <- eval(parse(text=data.name))
      if(is.null(data)) stop("The argument data is NULL. ",
                             "The original data used for the pca analysis are required.")
    }
    ind <- .get_pca_ind_results(ind.coord, data, res.pca$sdev^2,
                                res.pca$center, res.pca$scale)
  }
  else stop("An object of class : ", class(res.pca), 
            " can't be handled by the function get_pca_ind()")
  
  class(ind)<-"pca_ind"
  
  ind
}

# Print method for PCA individuals
print.pca_ind<-function(x){
  if(!inherits(x, "pca_ind"))
    stop("Can't handle data of class ", clas(x))
  cat("Principal Component Analysis Results for individuals\n",
      "===================================================\n")
  res <- array(data="", dim=c(3,2), dimnames=list(1:3, c("Name", "Description")))
  res[1, ] <- c("$coord", "Coordinates for the individuals")
  res[2, ] <- c("$cos2", "Cos2 for the individuals")
  res[3, ] <- c("$contrib", "contributions of the individuals")
  print(res[1:3,])
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
  
  eigenvalues <- eigenvalues[ncol(ind.coord)]
  
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
    100*(1/n.ind)*ind.coord^2/eigenvalues
  }
  ind.contrib <- t(apply(ind.coord,1, contrib,  eigenvalues, nrow(ind.coord)))
  colnames(ind.coord) <- colnames(ind.cos2) <-
    colnames(ind.contrib) <- paste0("Dim.", 1:ncol(ind.coord)) 
  
  # Individuals coord, cos2 and contrib
  ind = list(coord = ind.coord,  cos2 = ind.cos2, contrib = ind.contrib)
  ind
}
