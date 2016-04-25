#'Extract and visualize the eigenvalues/variances of dimensions
#'
#'@description Eigenvalues correspond to the amount of the variation explained 
#'  by each principal component (PC). Read more: 
#'  \href{http://www.sthda.com/english/wiki/factominer-and-factoextra-principal-component-analysis-visualization-r-software-and-data-mining}{Principal
#'   Component Analysis}
#'  
#'  \itemize{ \item{get_eig(): Extract the eigenvalues/variances of the 
#'  principal dimensions} \item{fviz_eig(): Plot the eigenvalues/variances 
#'  against the number of dimensions} \item{get_eigenvalue(): an alias of 
#'  get_eig()} \item{fviz_screeplot(): an alias of fviz_eig()} }
#'  
#'  These functions support the results of Principal Component Analysis (PCA), 
#'  Correspondence Analysis (CA), Multiple Correspondence Analysis (MCA), 
#'  Multiple Factor Analysis (MFA) and Hierarchical Multiple Factor Analysis 
#'  (HMFA) functions.
#'  
#'  
#'@param X an object of class PCA, CA, MCA, MFA and HMFA [FactoMineR]; prcomp 
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
#'@param ... optional arguments to be passed to the functions geom_bar(), 
#'  geom_line(), geom_text() or fviz_eig().
#'  
#'@return \itemize{ \item{get_eig() (or get_eigenvalue()): returns a data.frame 
#'  containing 3 columns: the eigenvalues, the percentage of variance and  the 
#'  cumulative percentage of variance retained by each dimension.} 
#'  \item{fviz_eig() (or fviz_screeplot()): returns a ggplot2} }
#'  
#'@author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'@seealso \code{\link{fviz_pca}}, \code{\link{fviz_ca}},
#'  \code{\link{fviz_mca}}, \code{\link{fviz_mfa}}, \code{\link{fviz_hmfa}}
#'@references http://www.sthda.com
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
#' fviz_eig(res.pca)
#' 
#' # Customize the plot
#'    # - Add labels
#'    # - Change line color, bar fill and color. 
#'    # - Change axis limits and themes
#'    
#' p <- fviz_eig(res.pca, addlabels = TRUE, hjust = -0.3,
#'            linecolor = "#FC4E07", 
#'            barfill="white", barcolor ="darkblue")+ 
#'      ylim(0, 85)+ # y axis limits
#'      theme_minimal() # themes: http://www.sthda.com/english/wiki/ggplot2-themes
#' print(p)
#' 
#' # Change plot title and axis labels
#' p + labs(title = "Variances - PCA",
#'         x = "Principal Components", y = "% of variances")
#' 
#'   
#' # Scree plot - Eigenvalues
#' fviz_eig(res.pca, choice = "eigenvalue", addlabels=TRUE)
#' 
#' # Use only bar  or line plot: geom = "bar" or geom = "line"
#' fviz_eig(res.pca, geom="line")
#'  
#'         
#' # Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(housetasks)
#' res.ca <- CA(housetasks, graph = FALSE)
#' get_eig(res.ca)
#' fviz_eig(res.ca, linecolor = "#FC4E07",
#'    barcolor = "#00AFBB", barfill = "#00AFBB")+
#'    theme_minimal()
#' 
#' # Multiple Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mca <- MCA(poison, quanti.sup = 1:2, 
#'               quali.sup = 3:4, graph=FALSE)
#' get_eig(res.mca)
#' fviz_eig(res.mca, linecolor = "#FC4E07",
#'    barcolor = "#2E9FDF", barfill = "#2E9FDF")+
#'    theme_minimal()
#'  
#'  
#' # Multiple Factor Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(wine)
#' res.mfa <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
#'                ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
#'                num.group.sup=c(1,6), graph=FALSE)
#' get_eig(res.mfa)
#' fviz_eig(res.mfa, linecolor = "#FC4E07",
#'    barcolor = "#E7B800", barfill = "#E7B800")+
#'    theme_minimal()
#' 
#' 
#'@name eigenvalue
NULL
#' @rdname eigenvalue
#' @export
get_eig<-function(X){
  
  # FactoMineR package
  if(inherits(X, c('PCA', 'CA', 'MCA', 'MFA', 'HMFA', 'sPCA', 'sCA', 'sMCA', 'sMFA', 'sHMFA'))) eig <- X$eig
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
                         ncp=10, addlabels=FALSE, hjust = 0,  ...)
{
  
  eig <- get_eigenvalue(X)
  eig <-eig[1:min(ncp, nrow(eig)), , drop=FALSE]
  
  title <- "Scree plot"
  xlab <- "Dimensions"
  ylab <- "Percentage of explained variances"
  
  choice <- choice[1]
  if(choice=="eigenvalue") {
    eig <- eig[,1]
    text_labels <- round(eig,1)
    ylab <- "Eigenvalue"
  }
  else if(choice=="variance") {
    eig <- eig[,2]
    text_labels <- paste0(round(eig,1), "%")
  }
  else stop("Allowed values for the argument choice are : 'variance' or 'eigenvalue'")
  
  if(length(intersect(geom, c("bar", "line"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  
  df.eig <- data.frame(dim = factor(1:length(eig)), eig=eig )
  p <- ggplot(df.eig, aes(dim, eig, group=1 ))
  if("bar" %in% geom) p <- p + geom_bar(stat="identity", fill=barfill, color = barcolor,...)
  if("line" %in% geom) p <- p + geom_line(color = linecolor, ...)+
    geom_point(shape=19, color=linecolor)
  if(addlabels) p <- p + geom_text(label = text_labels,
                                   vjust=-0.4, hjust = hjust, ...)
  p <- p + labs(title = title, x = xlab, y = ylab)
  
  p 
}


#' @rdname eigenvalue
#' @export 
fviz_screeplot<- function(...){
  fviz_eig(...)
} 
