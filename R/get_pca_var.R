#' Extract the results for variables - principal component analysis
#' 
#' @description
#' Ectract all the results for the active variables from a principal component analysis outputs.
#'  The output contains variable coordinates,
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
#'  data(iris)
#'  res.pca <- princomp(iris[, -5],  cor = TRUE)
#'  var <- get_pca_var(res.pca)
#'  
#'  head(var$coord)
#'  
#'  head(var$cos2)
#'  
#'  head(var$contrib)
#'  }
#'  
#' @export 
get_pca_var<-function(res.pca){
  # FactoMineR package
  if(inherits(res.pca, 'PCA')) var <- res.pca$var
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
  class(var)<-'pca_var'
  var
}


# Print method for PCA variables
print.pca_var<-function(x){
  if(!inherits(x, "pca_var"))
    stop("Can't handle data of class ", clas(x))
  cat("Principal Component Analysis Results for variables\n",
      "===================================================\n")
  res <- array(data="", dim=c(4,2), dimnames=list(1:4, c("Name", "Description")))
  res[1, ] <- c("$coord", "Coordinates for the variables")
  res[2, ] <- c("$cor", "Correlations between variables and dimensions")
  res[3, ] <- c("$cos2", "Cos2 for the variables")
  res[4, ] <- c("$contrib", "contributions of the variables")
  print(res[1:4,])
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

  