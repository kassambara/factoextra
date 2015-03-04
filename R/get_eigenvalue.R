#' Extract eigenvalues/variances - Principal Component Analysis
#' 
#' @description
#' Ectract eigenvalues/variances from the results of several PCA functions : 
#' PCA() from FactoMineR package; prcomp() and princomp() from stats package;
#'  dudi.pca() from ade4 package.
#' @param X an object of class PCA (FactoMineR); prcomp (stats); princomp (stats);
#'  dudi and pca (ade4).
#'  
#' @return a data.frame containing 3 columns :
#'  eigenvalue, percentage of variance and cumulative percentage of variance.
#'  
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#'  data(iris)
#'  res.pca <- princomp(iris[, -5],  cor = TRUE)
#'  eig <- get_eigenvalue(res.pca)
#'  head(eig)
#'  }
#' @export
get_eigenvalue<-function(X){
  
  # FactoMineR package
  if(inherits(X, 'PCA')) eig <- X$eig
  
  # stats package
  else if(inherits(X, 'prcomp') | inherits(X, 'princomp')){
    eig <- (X$sdev)^2
    variance <- eig*100/sum(eig)
    cumvar <- cumsum(variance)
    eig <- data.frame(eigenvalue = eig, variance = variance, 
                      cumvariance = cumvar)
  }
  # ade4 package
  else if(inherits(X, 'pca') & inherits(X, 'dudi'))
  {
    eig <- X$eig
    variance <- eig*100/sum(eig)
    cumvar <- cumsum(variance)
    eig <- data.frame(eigenvalue = eig, variance = variance, 
                      cumvariance = cumvar)
  }
  else stop("An object of class : ", class(X), 
            " can't be handled by the function get_eigenvalue()")
  
  colnames(eig) <- c("eigenvalue", "variance.percent", 
                     "cumulative.variance.percent")
  
  rownames(eig) <- paste0("Dim ", 1:nrow(eig))
  
  eig
}
