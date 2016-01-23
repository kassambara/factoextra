#' @include facto_summarize.R
NULL
#' Visualize the quality of representation of rows/columns
#' 
#' @description
#' This function can be used to visualize the quality of representation (cos2) of rows/columns 
#' from the results of Principal Component Analysis (PCA), 
#' Correspondence Analysis (CA), Multiple Correspondence Analysis (MCA) and 
#' Multiple Factor Analysis (MFA) functions.
#' @param X an object of class PCA, CA, MCA and MFA [FactoMineR]; prcomp and princomp [stats]; 
#'  dudi, pca, coa and acm [ade4]; ca [ca package].
#' @param choice allowed values are "row" and "col" for CA;  "var" and "ind" for PCA or MCA
#' @param axes a numeric vector specifying the dimension(s) of interest.
#' @param fill a fill color for the bar plot.
#' @param color an outline color for the bar plot.
#' @param sort.val a string specifying whether the value should be sorted. 
#' Allowed values are "none" (no sorting), "asc" (for ascending) or "desc" (for descending).
#' @param top a numeric value specifying the number of top elements to be shown.
#'  
#' @return a ggplot2 plot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' # Principal component analysis
#' # ++++++++++++++++++++++++++
#' data(decathlon2)
#' decathlon2.active <- decathlon2[1:23, 1:10]
#' res.pca <- prcomp(decathlon2.active,  scale = TRUE)
#' 
#' # variable cos2 on axis 1
#' fviz_cos2(res.pca, choice="var", axes = 1 )
#' # sorting
#' fviz_cos2(res.pca, choice="var", axes = 1, 
#'            sort.val ="asc")
#'            
#' # select the top 7 contributing variables
#' fviz_cos2(res.pca, choice="var", axes = 1, top = 7 )
#' 
#' # Change theme and color
#' fviz_cos2(res.pca, choice="var", axes = 1,
#'          fill = "lightgray", color = "black") +
#'          theme_minimal() + 
#'          theme(axis.text.x = element_text(angle=45))
#'          
#' # Variable cos2 on axis 2
#' fviz_cos2(res.pca, choice="var", axes = 2)
#' # Variable cos2 on axes 1 + 2
#' fviz_cos2(res.pca, choice="var", axes = 1:2)
#' 
#' # cos2 of individuals on axis 1
#' fviz_cos2(res.pca, choice="ind", axes = 1)
#' 
#' # Correspondence Analysis
#' # ++++++++++++++++++++++++++
#' # Install and load FactoMineR to compute CA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data("housetasks")
#' res.ca <- CA(housetasks, graph = FALSE)
#' 
#' # Visualize row cos2 on axes 1
#' fviz_cos2(res.ca, choice ="row", axes = 1)
#' # Visualize row cos2 on axes 1 + 2
#' fviz_cos2(res.ca, choice ="row", axes = 1:2)
#' # Visualize column cos2 on axes 1
#' fviz_cos2(res.ca, choice ="col", axes = 1)
#' 
#' # Multiple Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mca <- MCA(poison, quanti.sup = 1:2, 
#'               quali.sup = 3:4, graph=FALSE)
#'               
#' # Visualize individual cos2 on axes 1
#' fviz_cos2(res.mca, choice ="ind", axes = 1)
#' # Select the top 20
#' fviz_cos2(res.mca, choice ="ind", axes = 1, top = 20)
#' # Visualize variable categorie cos2 on axes 1
#' fviz_cos2(res.mca, choice ="var", axes = 1)
#' 
#' # Multiple Factor Analysis
#' # ++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mfa <- MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
#'                name.group=c("desc","desc2","symptom","eat"),
#'                num.group.sup=1:2, graph=FALSE)
#' # Visualize individual cos2 on axes 1
#' fviz_cos2(res.mfa, choice ="ind", axes = 1)
#' # Select the top 20
#' fviz_cos2(res.mfa, choice ="ind", axes = 1, top = 20)
#' # Visualize catecorical variable categorie cos2 on axes 1
#' fviz_cos2(res.mfa, choice ="quali.var", axes = 1)
#'                
#'  }
#' @export 
fviz_cos2 <- function(X, choice = c("row", "col", "var", "ind", "quanti.var", "quali.var", "group"), axes=1,
                   fill="steelblue", color = "steelblue",  
                   sort.val = c("desc", "asc", "none"), top = Inf)
{

   title <- .build_title(choice[1], "Cos2", axes)
  
   dd <- facto_summarize(X, element = choice, result = "cos2", axes = axes)
   cos2 <- dd$cos2
   
   names(cos2) <-rownames(dd)
  p <- .ggbarplot(cos2, fill =fill, color = color,
                  sort.value = sort.val, top = top,
                  title = title, ylab ="Cos2 - Quality of representation")
  
  p 
}

