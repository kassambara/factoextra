#' @include get_eigenvalue.R
NULL
#' Plot the variances/eigenvalues against the number of dimensions
#' 
#' @param X an object of class PCA (FactoMineR); prcomp (stats); princomp (stats);
#'  dudi and pca (ade4)
#' @param choice a text specifyinng the type of the data to be plotted.
#'  Allowed values are "variance" or "eigenvalue"
#' @param geom a text specifying the geometry to be used for the graph.
#'  Allowed values are "bar" for barplot, "line" for lineplot or c("bar", "line") to use both type
#' @param barfill fill color for bar plot
#' @param barcolor outline color for bar plot
#' @param linecolor color for line plot (when geom contains "line")
#' @param ncp a numeric value specifying the number of components to be shown
#' @param addlabels logical value. If TRUE, labels are added at the top of bars or points
#'  showing the information retained by each dimension
#'  @param ... optional arguments to be passed to the functions geom_bar(), geom_line() or geom_text()
#'  
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' data(iris)
#' res.pca <- princomp(iris[, -5],  cor = TRUE)
#' 
#' # Scree plot
#' fviz_screeplot(res.pca, addlabels=TRUE)
#' 
#' # Use only barplot
#' fviz_screeplot(res.pca, geom="bar", width=0.8)
#' 
#' # Change theme
#' fviz_screeplot(res.pca) + theme_minimal()
#'  }
#'  
#' @export
fviz_screeplot<-function(X, choice=c("variance", "eigenvalue"), geom=c("bar", "line"),
                         barfill="steelblue", barcolor="steelblue", linecolor = "black",
                         ncp=5, addlabels=FALSE, ...)
{
 
  eig <- get_eigenvalue(X)
  eig <-eig[1:min(ncp, nrow(eig)), , drop=FALSE]
  
  title <- "Scree plot"
  xlab <- "Dimensions"
  ylab <- "Percentage of variances"
  if(inherits(X, "PCA")) xlab = "Principal Components"
  
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
