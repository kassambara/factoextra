#' Extract the results for the variables after a principal component analysis
#' 
#' @description
#' Ectract all the results for the active variables from a principal component analysis output.
#'  This output contains variable coordinates,
#'  correlation between variables and axes, square cosine and contributions.
#'  The allowed PCA outputs are the ones from FactoMineR (PCA),
#'  stats (princomp() and prcomp()), ade4 (dudi.pca()) packages.
#' @param res.pca an object of class PCA (from FactoMineR);
#'  prcomp and princomp (from stats package); pca, dudi (dudi.pca, from adea4 package)
#' @return a list of matrices containing all the results for the active variables including : 
#' \item{coord}{coordinates for the variables}
#' \item{cor}{correlations between variables and dimensions}
#' \item{cos2}{cos2 for the variables}
#' \item{contrib}{contributions of the variables}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#'  res.pca <- princomp(iris[, -5])
#'  var <- get_pca_var(res.pca)
#'  var
#'  }
#'  
#'  
get_pca_var<-function(res.pca){
  
  # FactoMineR package
  if(inherits(res.pca, 'PCA')) var <- res.pca$var
  
  # stats package
  else if(inherits(res.pca, 'princomp')){   
    # Correlation of variables with the principal component
    var_cor_func <- function(var.loadings, comp.sdev){var.loadings*comp.sdev}
    var.cor <- t(apply(res.pca$loadings, 1, var_cor_func, res.pca$sdev))
  }
  
  else stop("An object of class : ", class(res.pca), 
            " can't be handled by the function get_pca_var()")
  
  # Compute the coordinates, the cos2 and contributions
  # of variables
  if(inherits(res.pca, 'princomp')){  
    
    var.coord <- var.cor # variable coordinates
    var.cos2 <- var.cor^2 # variable qualities 
    
    # variable contributions (in percent)
    # var.cos2*100/total Cos2 of the component
    comp.cos2 <- apply(var.cos2, 2, sum)
    contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
    var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
    
    colnames(var.coord) <- colnames(var.cor) <- colnames(var.cos2) <-
      colnames(var.contrib) <- paste0("Dim ", 1:ncol(var.coord)) 
    
    # Variable coord, cor, cos2 and contrib
   var = list(coord = var.coord, cor = var.cor, cos2 = var.cos2, contrib = var.contrib)
  }
  
  var
}
