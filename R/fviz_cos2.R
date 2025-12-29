#' @include facto_summarize.R
NULL
#' Visualize the quality of representation of rows/columns
#' 
#' @description This function can be used to visualize the quality of
#' representation (cos2) of rows/columns from the results of Principal Component
#' Analysis (PCA), Correspondence Analysis (CA), Multiple Correspondence
#' Analysis (MCA), Factor Analysis of Mixed Data (FAMD), Multiple Factor
#' Analysis (MFA) and Hierarchical Multiple Factor Analysis (HMFA) functions.
#' @param X an object of class PCA, CA, MCA, FAMD, MFA and HMFA [FactoMineR];
#'   prcomp and princomp [stats]; dudi, pca, coa and acm [ade4]; ca [ca
#'   package].
#' @param choice allowed values are "row" and "col" for CA;  "var" and "ind" for
#'   PCA or MCA; "var", "ind", "quanti.var", "quali.var" and "group" for FAMD, MFA and HMFA.
#' @param axes a numeric vector specifying the dimension(s) of interest.
#' @param fill a fill color for the bar plot.
#' @param color an outline color for the bar plot.
#' @param sort.val a string specifying whether the value should be sorted. 
#'   Allowed values are "none" (no sorting), "asc" (for ascending) or "desc"
#'   (for descending).
#' @param top a numeric value specifying the number of top elements to be shown.
#' @param xtickslab.rt rotation angle for x axis tick labels. Default is 45 degrees.
#' @inheritParams ggpubr::ggpar
#'   
#' @return a ggplot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com/english/
#' @examples
#' \donttest{
#' # Principal component analysis
#' # ++++++++++++++++++++++++++
#' data(decathlon2)
#' decathlon2.active <- decathlon2[1:23, 1:10]
#' res.pca <- prcomp(decathlon2.active,  scale = TRUE)
#' 
#' # variable cos2 on axis 1
#' fviz_cos2(res.pca, choice="var", axes = 1, top = 10 )
#' 
#' # Change color
#' fviz_cos2(res.pca, choice="var", axes = 1,
#'          fill = "lightgray", color = "black") 
#'          
#' # Variable cos2 on axes 1 + 2
#' fviz_cos2(res.pca, choice="var", axes = 1:2)
#' 
#' # cos2 of individuals on axis 1
#' fviz_cos2(res.pca, choice="ind", axes = 1)
#' 
#' \dontrun{
#' # Correspondence Analysis
#' # ++++++++++++++++++++++++++
#' library("FactoMineR")
#' data("housetasks")
#' res.ca <- CA(housetasks, graph = FALSE)
#' 
#' # Visualize row cos2 on axes 1
#' fviz_cos2(res.ca, choice ="row", axes = 1)
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
#' # Select the top 20
#' fviz_cos2(res.mfa, choice ="ind", axes = 1, top = 20)
#' # Visualize catecorical variable categorie cos2 on axes 1
#' fviz_cos2(res.mfa, choice ="quali.var", axes = 1)
#' }
#'                
#'  }
#' @export
fviz_cos2 <- function(X, choice = c("row", "col", "var", "ind", "quanti.var", "quali.var", "group"), axes=1,
                   fill="steelblue", color = "steelblue",  
                   sort.val = c("desc", "asc", "none"), top = Inf, 
                   xtickslab.rt = 45, ggtheme = theme_minimal(), ...)
{
   sort.val <- match.arg(sort.val)
   title <- .build_title(choice[1], "Cos2", axes)
  
   dd <- facto_summarize(X, element = choice, result = "cos2", axes = axes)
   cos2 <- dd$cos2
   
   names(cos2) <-rownames(dd)
   
   df <- data.frame(name = factor(names(cos2), levels = names(cos2)), cos2 = cos2)
   
   p <- ggpubr::ggbarplot(df, x = "name", y = "cos2", fill = fill, color = color,
                          sort.val = sort.val, top = top,
                          main = title, xlab = FALSE, ylab ="Cos2 - Quality of representation",
                          xtickslab.rt = xtickslab.rt, ggtheme = ggtheme, ...
   )
  p 
}

