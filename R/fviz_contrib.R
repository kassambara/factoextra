#' @include facto_summarize.R
NULL
#' Visualize the contributions of row/column elements
#'
#' @description
#' This function can be used to visualize the quality of representation (cos2) of rows/columns
#' from the results of Principal Component Analysis (PCA),
#' Correspondence Analysis (CA), Multiple Correspondence Analysis (MCA)
#' and Multiple Factor Analysis (MFA) functions.
#' @param ... not used
#' @inheritParams fviz_cos2
#' @details
#' The function fviz_contrib() creates a barplot of row/column contributions.
#' A reference dashed line is also shown on the barplot. This reference line
#' corresponds to the expected value if the contribution where uniform.\cr\cr
#' For a given dimension, any row/column with a contribution above the reference line could be
#' considered as important in contributing to the dimension.
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
#' # variable contributions on axis 1
#' fviz_contrib(res.pca, choice="var", axes = 1 )
#' # sorting
#' fviz_contrib(res.pca, choice="var", axes = 1,
#'            sort.val ="asc")
#'
#' # select the top 7 contributing variables
#' fviz_contrib(res.pca, choice="var", axes = 1, top = 7 )
#'
#' # Change theme and color
#' fviz_contrib(res.pca, choice="var", axes = 1,
#'          fill = "lightgray", color = "black") +
#'          theme_minimal() +
#'          theme(axis.text.x = element_text(angle=45))
#'
#' # Variable contributions on axis 2
#' fviz_contrib(res.pca, choice="var", axes = 2)
#' # Variable contributions on axes 1 + 2
#' fviz_contrib(res.pca, choice="var", axes = 1:2)
#'
#' # Contributions of individuals on axis 1
#' fviz_contrib(res.pca, choice="ind", axes = 1)
#'
#' # Correspondence Analysis
#' # ++++++++++++++++++++++++++
#' # Install and load FactoMineR to compute CA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data("housetasks")
#' res.ca <- CA(housetasks, graph = FALSE)
#'
#' # Visualize row contributions on axes 1
#' fviz_contrib(res.ca, choice ="row", axes = 1)
#' # Visualize row contributions on axes 1 + 2
#' fviz_contrib(res.ca, choice ="row", axes = 1:2)
#' # Visualize column contributions on axes 1
#' fviz_contrib(res.ca, choice ="col", axes = 1)
#'
#' # Multiple Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mca <- MCA(poison, quanti.sup = 1:2,
#'               quali.sup = 3:4, graph=FALSE)
#'
#' # Visualize individual contributions on axes 1
#' fviz_contrib(res.mca, choice ="ind", axes = 1)
#' # Select the top 20
#' fviz_contrib(res.mca, choice ="ind", axes = 1, top = 20)
#' # Visualize variable categorie contributions on axes 1
#' fviz_contrib(res.mca, choice ="var", axes = 1)
#'
#' # Multiple Factor Analysis
#' # ++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mfa <- MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
#'                name.group=c("desc","desc2","symptom","eat"),
#'                num.group.sup=1:2, graph=FALSE)
#'
#' # Visualize individual contributions on axes 1
#' fviz_contrib(res.mfa, choice ="ind", axes = 1)
#' # Select the top 20
#' fviz_contrib(res.mfa, choice ="ind", axes = 1, top = 20)
#' # Visualize catecorical variable categorie contributions on axes 1
#' fviz_contrib(res.mfa, choice ="quali.var", axes = 1)
#'
#'  }
#' @export
fviz_contrib <- function(X, choice = c("row", "col", "var", "ind", "quanti.var", "quali.var", "group", "partial.axes"), axes=1,
                   fill="steelblue", color = "steelblue",
                   sort.val = c("desc", "asc", "none"), top = Inf)
{

  title <- .build_title(choice[1], "Contribution", axes)

  dd <- facto_summarize(X, element = choice, result = "contrib", axes = axes)
  contrib <- dd$contrib
  names(contrib) <-rownames(dd)

  # expected Average contribution
  theo_contrib <- 100/length(contrib)
  if(length(axes) > 1) {
    # Adjust variable contributions by the Dimension eigenvalues
    eig <- get_eigenvalue(X)[axes,1]
    theo_contrib <- sum(theo_contrib*eig)
  }

  p <- .ggbarplot(contrib, fill =fill, color = color,
                  sort.value = sort.val[1], top = top,
                  title = title, ylab ="Contributions (%)")+
    geom_hline(yintercept=theo_contrib, linetype=2, color="red")

  p
}


#' @describeIn fviz_contrib deprecated function. Use fviz_contrib()
#' @param sortcontrib see the argument sort.val
#' @export
fviz_pca_contrib <- function(X, choice = c("var", "ind"), axes=1,
                             fill="steelblue", color = "steelblue",
                             sortcontrib = c("desc", "asc", "none"), top = Inf,...)
{

  warning("The function fviz_pca_contrib() is deprecated. ",
          "Please use the function fviz_contrib() which can handle outputs ",
          " of PCA, CA and MCA functions.")

  p <- fviz_contrib(X = X, choice = choice, axes = axes,
               fill = fill, color = color, sort.val = sortcontrib,
               top = top)
  p
}


