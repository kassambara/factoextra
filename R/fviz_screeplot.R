#' Screeplot : Plots the variances/eigenvalues against the number of dimensions
#' 
#' @param X Object of class PCA, MCA, CA (from FactoMineR)
#' @param choice Character specifing the type of the data to be plotted.
#'  Allowed values are "variance" or "eigenvalue"
#' @param geom Character specifing the geometry to be used for the graph.
#'  Allowed values are "bar" for barplot, "line" for lineplot or c("bar", "line") to use both type
#' @param barfill Fill color for bar plot
#' @param barcolor Outline color for bar plot
#' @param linecolor Color for line plot (when geom contains "line")
#' @param ncp Numeric value specifing the number of components to be shown
#' @param addlabels Logical value. If TRUE, labels are added at the top of bars or points
#'  showing the information retained by each dimension
#'  
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#'  library("FactoMineR")
#'  data(decathlon)
#'  res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
#'  fviz_screeplot(res.pca)
#'  }
fviz_screeplot<-function(X, choice=c("variance", "eigenvalue"), geom=c("bar", "line"),
                         barfill="steelblue", barcolor="steelblue", linecolor = "black",
                         ncp=5, addlabels=FALSE, ...)
{
  library("ggplot2")
  
  eig <- X$eig
  if(ncp > 0 & nrow(eig) > ncp) eig <- eig[1:ncp, , drop=FALSE]
  
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
