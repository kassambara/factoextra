#' @include get_pca_var.R get_pca_ind.R
NULL
#' Contributions of variables/individuals - Principal Component Analysis
#' 
#' @description
#' This function can be used to visualize the contributions of variables/individuals from the output of several PCA functions : 
#' PCA() from FactoMineR package; prcomp() and princomp() from stats package;
#'  dudi.pca() from ade4 package.
#' 
#' @param X an object of class PCA (FactoMineR); prcomp (stats); princomp (stats);
#'  dudi and pca (ade4).
#'  @param choice allowed values are "var" (for variable contributions) or "ind" 
#'  for the contribution of individuals
#' @param axes a numeric vector specifying the component(s) of interest.
#' @param fill a fill color for the bar plot
#' @param color an outline color the bar plot
#' @param sortcontrib a string specifying whether the contributions should be sorted. 
#' Allowed values are "none" (no sorting), "asc" (for ascending) or "desc" (for descending)
#' @param top a numeric value specifing the top contributing elements to be shown
#' @param ... optional arguments to be passed to the function get_pca_ind().
#'  This can includes the argument data (the original data used for pca) 
#'  which is required when X is not from FactoMineR or adea4 packages.
#'  
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' data(decathlon2)
#' decathlon2.active <- decathlon2[1:23, 1:10]
#' # Principal component analysis
#' res.pca <- princomp(decathlon2.active,  cor = TRUE)
#' 
#' # variable contributions on axis 1
#' fviz_pca_contrib(res.pca, choice="var", axes = 1 )
#' # sorting
#' fviz_pca_contrib(res.pca, choice="var", axes = 1, sort = "asc" )
#' 
#' variable contributions on axis 2
#' fviz_pca_contrib(res.pca, choice="var", axes = 2, sort = "asc" )
#' 
#' # Contributions of individuals on axis 1
#' fviz_pca_contrib(res.pca, choice="var", axes = 1, sort = "asc" )
#'  }
#'  @export 
fviz_pca_contrib <- function(X, choice = c("var", "ind"), axes=1,
                   fill="steelblue", color = "steelblue", 
                   sortcontrib = c("none", "desc", "asc"), top = Inf,...)
{
  
  if(choice[1]=="var") {
    pca.contrib <- get_pca_var(X)$contrib
    title <- paste0("Contribution of variables on PC-", paste(axes, collapse="-"))
  }
  else if(choice[1]=="ind"){
    pca.contrib <- get_pca_ind(X, ...)$contrib
    title <- paste0("Contribution of individuals on PC-", paste(axes, collapse="-"))
  }
  
  # Extract contribution
  if(length(axes) > 1) {
    eig <- get_eigenvalue(X)[axes,1]
    contrib <- pca.contrib[, axes]
    # Adjust variable contributions by the PC eigenvalues
    contrib <- t(apply(contrib, 1, 
                    function(var.contrib, pc.eig){var.contrib*pc.eig},
                    eig))
    contrib <-apply(contrib, 1, sum)
    # Theorical contribution of each variables
    theo_contrib <- sum((100/nrow(pca.contrib))*eig)
  }
  else{
    contrib <- pca.contrib[, axes]
    # Theorical contribution of each variables
    theo_contrib <- 100/nrow(pca.contrib)
  }
  
  # top contributing elements
  if(top!=Inf & top < length(contrib)){
    top_element <- names(sort(contrib, decreasing=TRUE)[1:top])
    contrib <- contrib[top_element]
  }
  # sorting
  if(sortcontrib[1]=="desc") contrib <- sort(contrib, decreasing = TRUE)
  else if(sortcontrib[1]=="asc") contrib <- sort(contrib, decreasing = FALSE)
    
  # Contribution data frame
  pca.contrib <- cbind.data.frame(name = factor(names(contrib), levels = names(contrib)), 
                                  contrib = contrib)
  
  xlab = ""
  ylab = "Contributions (%)"
  
  p <- ggplot(pca.contrib, aes(name, contrib)) + 
    geom_bar(stat="identity", fill=fill, color = color,...) + 
    geom_hline(yintercept=theo_contrib, linetype=2, color="red")+
    theme(axis.text.x = element_text(angle=45))+
    labs(title = title, x = xlab, y = ylab)
  
  p 
}

