#' Extract eigenvalues/variances after factor analysis
#' 
#' @description
#' Ectract eigenvalues/variances from the results of different PCA, MCA and CA functions in R.
#' Results from FactoMineR (PCA, MCA, CA functions), ade4 (dudi.pca function) 
#' and stats (prcomp, princomp functions) packages are allowed.
#' @param X Object of class PCA, MCA and CA (from FactoMineR);
#'  prcomp and princomp (from stats package); pca, dudi (dudi.pca, from adea4 package)
#'  
#' @return a data.frame containing 3 columns :
#'  eigenvalue, percentage of variance and cumulative percentage of variance.
#'  
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#'  res.pca <- princomp(iris[, -5],  cor = TRUE)
#'  eig <- get_eigenvalue(res.pca)
#'  head(eig)
#'  }
#'  
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
