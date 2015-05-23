#' Extract and visualize the eigenvalues/variances of dimensions
#' 
#' @description
#' Extracts and plots the eigenvalues/variances of the dimensions 
#' from the results of Principal Component Analysis (PCA), 
#' Correspondence Analysis (CA) and 
#' Multiple Correspondence Analysis (MCA) functions.\cr\cr
#' \itemize{
#' \item{get_eig(): Extract the eigenvalues/variances of the principal dimensions}
#' \item{fviz_eig(): Plot the eigenvalues/variances against the number of dimensions}
#' \item{get_eigenvalue(): an alias of get_eig()}
#' \item{fviz_screeplot(): an alias of fviz_eig()}
#' }
#' 
#' @param X an object of class PCA, CA and MCA [FactoMineR]; prcomp and princomp [stats]; 
#'  dudi, pca, coa and acm [ade4]; ca and mjca [ca package].
#' @param choice a text specifying the data to be plotted. 
#' Allowed values are "variance" or "eigenvalue".
#' @param geom a text specifying the geometry to be used for the graph.
#'  Allowed values are "bar" for barplot, "line" for lineplot or c("bar", "line") to use both types.
#' @param barfill fill color for bar plot.
#' @param barcolor outline color for bar plot.
#' @param linecolor color for line plot (when geom contains "line").
#' @param ncp a numeric value specifying the number of dimensions to be shown.
#' @param addlabels logical value. If TRUE, labels are added at the top of bars or points
#'  showing the information retained by each dimension.
#'  @param ... optional arguments to be passed to the functions geom_bar(), 
#'  geom_line(), geom_text() or fviz_eig().
#'  
#' @return 
#' \itemize{
#' \item{get_eig() (or get_eigenvalue()): returns a data.frame containing 3 columns: 
#' the eigenvalues, the percentage of variance and  the cumulative percentage of variance 
#' retained by each dimension.}
#' \item{fviz_eig() (or fviz_screeplot()): returns a ggplot2}
#' }
#'  
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
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
#' # Add labels
#' fviz_eig(res.pca, addlabels=TRUE)
#' 
#' # Change the y axis limits
#' fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3) +
#'    ylim(0, 80)
#' # Scree plot - Eigenvalues
#' fviz_eig(res.pca, choice = "eigenvalue", addlabels=TRUE)
#' 
#' # Use only bar plot
#' fviz_eig(res.pca, geom="bar", width=0.8)
#' 
#' # Use only line plot
#' fviz_eig(res.pca, geom="line")
#' 
#' # Change theme
#' fviz_screeplot(res.pca) + theme_minimal()
#' # theme_classic()
#' fviz_eig(res.pca) + theme_classic()
#' 
#' # Customized plot
#' fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3,
#'            linecolor ="red") + theme_minimal()
#' # Change colors, y axis limits and theme           
#' p <- fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3,
#'                barfill="white", barcolor ="darkblue",
#'                linecolor ="red") + ylim(0, 85) + 
#'                theme_minimal()
#' print(p)
#' # Change titles
#' p + labs(title = "Variances - PCA",
#'         x = "Principal Components", y = "% of variances")
#'         
#' # Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(housetasks)
#' res.ca <- CA(housetasks, graph = FALSE)
#' get_eig(res.ca)
#' fviz_eig(res.ca)
#' 
#' # Multiple Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mca <- MCA(poison, quanti.sup = 1:2, 
#'               quali.sup = 3:4, graph=FALSE)
#' get_eig(res.mca)
#' fviz_eig(res.mca)
#'  }
#'
#' @name eigenvalue
NULL
#' @rdname eigenvalue
#' @export
get_eig<-function(X){
  
  # FactoMineR package
  if(inherits(X, c('PCA', 'CA', 'MCA'))) eig <- X$eig
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
                         ncp=10, addlabels=FALSE, ...)
{
  
  eig <- get_eigenvalue(X)
  eig <-eig[1:min(ncp, nrow(eig)), , drop=FALSE]
  
  title <- "Scree plot"
  xlab <- "Dimensions"
  ylab <- "Percentage of variances"
  
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
                                   vjust=-0.4, ...)
  p <- p + labs(title = title, x = xlab, y = ylab)
  
  p 
}


#' @rdname eigenvalue
#' @export 
fviz_screeplot<- function(...){
  fviz_eig(...)
} 
