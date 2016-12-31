#'Extract and visualize the eigenvalues/variances of dimensions
#'
#'@description Eigenvalues correspond to the amount of the variation explained 
#'  by each principal component (PC). 
#'  
#'  \itemize{ \item{get_eig(): Extract the eigenvalues/variances of the 
#'  principal dimensions} \item{fviz_eig(): Plot the eigenvalues/variances 
#'  against the number of dimensions} \item{get_eigenvalue(): an alias of 
#'  get_eig()} \item{fviz_screeplot(): an alias of fviz_eig()} }
#'  
#'  These functions support the results of Principal Component Analysis (PCA), 
#'  Correspondence Analysis (CA), Multiple Correspondence Analysis (MCA), Factor Analysis of Mixed Data (FAMD),
#'  Multiple Factor Analysis (MFA) and Hierarchical Multiple Factor Analysis 
#'  (HMFA) functions.
#'  
#'  
#'@param X an object of class PCA, CA, MCA, FAMD, MFA and HMFA [FactoMineR]; prcomp 
#'  and princomp [stats]; dudi, pca, coa and acm [ade4]; ca and mjca [ca 
#'  package].
#'@param choice a text specifying the data to be plotted. Allowed values are 
#'  "variance" or "eigenvalue".
#'@param geom a text specifying the geometry to be used for the graph. Allowed 
#'  values are "bar" for barplot, "line" for lineplot or c("bar", "line") to use
#'  both types.
#'@param barfill fill color for bar plot.
#'@param barcolor outline color for bar plot.
#'@param linecolor color for line plot (when geom contains "line").
#'@param ncp a numeric value specifying the number of dimensions to be shown.
#'@param addlabels logical value. If TRUE, labels are added at the top of bars 
#'  or points showing the information retained by each dimension.
#'@param hjust horizontal adjustment of the labels.
#'@param main,xlab,ylab plot main and axis titles.
#' @inheritParams ggpubr::ggpar
#'@param ... optional arguments to be passed to the function \link[ggpubr]{ggpar}.
#'  
#'@return \itemize{ \item{get_eig() (or get_eigenvalue()): returns a data.frame 
#'  containing 3 columns: the eigenvalues, the percentage of variance and  the 
#'  cumulative percentage of variance retained by each dimension.} 
#'  \item{fviz_eig() (or fviz_screeplot()): returns a ggplot2} }
#'  
#'@author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'@seealso \code{\link{fviz_pca}}, \code{\link{fviz_ca}},
#'  \code{\link{fviz_mca}}, \code{\link{fviz_mfa}}, \code{\link{fviz_hmfa}}
#'@references http://www.sthda.com/english/
#' @examples
#' # Principal Component Analysis
#' # ++++++++++++++++++++++++++
#' data(iris)
#' res.pca <- prcomp(iris[, -5],  scale = TRUE)
#' 
#' # Extract eigenvalues/variances
#' get_eig(res.pca)
#' 
#' # Default plot
#' fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 85))
#'   
#' # Scree plot - Eigenvalues
#' fviz_eig(res.pca, choice = "eigenvalue", addlabels=TRUE)
#' 
#' # Use only bar  or line plot: geom = "bar" or geom = "line"
#' fviz_eig(res.pca, geom="line")
#'  
#' \dontrun{         
#' # Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(housetasks)
#' res.ca <- CA(housetasks, graph = FALSE)
#' get_eig(res.ca)
#' fviz_eig(res.ca, linecolor = "#FC4E07",
#'    barcolor = "#00AFBB", barfill = "#00AFBB")
#' 
#' # Multiple Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mca <- MCA(poison, quanti.sup = 1:2, 
#'               quali.sup = 3:4, graph=FALSE)
#' get_eig(res.mca)
#' fviz_eig(res.mca, linecolor = "#FC4E07",
#'    barcolor = "#2E9FDF", barfill = "#2E9FDF")
#' }
#' 
#'@name eigenvalue
NULL
#' @rdname eigenvalue
#' @export
get_eig<-function(X){
  
  # FactoMineR package
  if(inherits(X, c('PCA', 'CA', 'MCA', 'FAMD', 'MFA', 'HMFA', 'sPCA', 'sCA', 'sMCA', 'sMFA', 'sHMFA'))) eig <- X$eig
  else{
    # stats package
    if(inherits(X, 'prcomp') | inherits(X, 'princomp')) eig <- (X$sdev)^2
    # ade4 package
    else if(inherits(X, c('pca', 'coa', 'acm')) & inherits(X, 'dudi')) eig <- X$eig
    # ca package
    else if(inherits(X, 'ca'))  eig <- X$sv^2
    else if(inherits(X, 'mjca')) eig <- X$inertia.e
    # MASS
    else if(inherits(X, 'correspondence'))  eig <- X$cor^2
    # ExPosition package
    else if (inherits(X, "expoOutput")) eig <- X$ExPosition.Data$eigs
    else stop("An object of class : ", class(X), 
              " can't be handled by the function get_eigenvalue()")
    
    variance <- eig*100/sum(eig)
    cumvar <- cumsum(variance)
    eig <- data.frame(eigenvalue = eig, variance = variance, 
                      cumvariance = cumvar)
  }
  
  colnames(eig) <- c("eigenvalue", "variance.percent", 
                     "cumulative.variance.percent")
  rownames(eig) <- paste0("Dim.", 1:nrow(eig))
  
  eig 
}

#' @rdname eigenvalue
#' @export
get_eigenvalue <- function(X){
  get_eig(X)
}

#' @rdname eigenvalue
#' @export
fviz_eig<-function(X, choice=c("variance", "eigenvalue"), geom=c("bar", "line"),
                  barfill="steelblue", barcolor="steelblue", linecolor = "black",
                  ncp=10, addlabels=FALSE, hjust = 0, 
                   main = NULL, xlab = NULL, ylab = NULL, 
                  ggtheme = theme_minimal(), ...)
{
  
  eig <- get_eigenvalue(X)
  eig <-eig[1:min(ncp, nrow(eig)), , drop=FALSE]
  
  choice <- choice[1]
  if(choice=="eigenvalue") {
    eig <- eig[,1]
    text_labels <- round(eig,1)
    if(is.null(ylab)) ylab <- "Eigenvalue"
  }
  else if(choice=="variance") {
    eig <- eig[,2]
    text_labels <- paste0(round(eig,1), "%")
  }
  else stop("Allowed values for the argument choice are : 'variance' or 'eigenvalue'")
  
  if(length(intersect(geom, c("bar", "line"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  
  
  
  df.eig <- data.frame(dim = factor(1:length(eig)), eig=eig )
  
  extra_args <- list(...)
  bar_width <- extra_args$bar_width
  linetype <- extra_args$linetype
  if(is.null(linetype)) linetype <- "solid"
  
  p <- ggplot(df.eig, aes(dim, eig, group=1 ))
  if("bar" %in% geom) p <- p + geom_bar(stat="identity", fill= barfill, 
                                        color = barcolor, width = bar_width)
  if("line" %in% geom) p <- p + geom_line(color = linecolor, linetype = linetype)+
    geom_point(shape=19, color=linecolor)
  if(addlabels) p <- p + geom_text(label = text_labels, vjust=-0.4, hjust = hjust)
  
  if(is.null(main)) main <- "Scree plot"
  if(is.null(xlab)) xlab <- "Dimensions"
  if(is.null(ylab)) ylab <- "Percentage of explained variances"
  
  p <- p + labs(title = main, x = xlab, y = ylab)
  ggpubr::ggpar(p, ggtheme = ggtheme,  ...)
}


#' @rdname eigenvalue
#' @export 
fviz_screeplot<- function(...){
  fviz_eig(...)
} 
