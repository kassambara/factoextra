#' @include get_mfa.R
NULL
#' Visualize Factor Analysis of Mixed Data
#' 
#' @description Factor analysis of mixed data (FAMD) is, a particular case of 
#'   MFA, used to analyze a data set containing both quantitative and
#'   qualitative variables. fviz_famd() provides ggplot2-based elegant
#'   visualization of FAMD outputs from the R function: FAMD [FactoMineR].\cr\cr
#'   \itemize{ \item fviz_famd_ind(): Graph of individuals
#'   \item fviz_famd_var(): Graph of variables \item fviz_famd(): An alias of
#'   fviz_famd_ind(res.famd) }
#'   
#' @param X an object of class FAMD [FactoMineR].
#' @inheritParams fviz_mca
#' @inheritParams fviz_pca
#' @inheritParams fviz
#' @inheritParams ggpubr::ggpar
#' @param choice The graph to plot in fviz_famd_var(). Allowed values include
#'   one of c("var", "quanti.var", "quali.var", "quali.sup").
#' @param habillage an optional factor variable for coloring the observations by
#'   groups. Default value is "none". If X is an MFA object from FactoMineR 
#'   package, habillage can also specify the index of the factor variable in the
#'   data.
#' @param col.ind,col.var color for individuals and variables, respectively. Can
#'   be a continuous variable or a factor variable. Possible values include also
#'   : "cos2", "contrib", "coord", "x" or "y". In this case, the colors for
#'   individuals/variables are automatically controlled by their qualities
#'   ("cos2"), contributions ("contrib"), coordinates (x^2 + y^2 , "coord"), x
#'   values("x") or y values("y"). To use automatic coloring (by cos2, contrib,
#'   ....), make sure that habillage ="none".
#' @param col.var.sup color for supplementary variables.
#' @param col.quali.var color for qualitative variables in fviz_mfa_ind(). 
#'   Default is "black".
#' @param alpha.ind,alpha.var controls the transparency of individuals and 
#'   variables, respectively. The value can variate from 0 (total transparency) 
#'   to 1 (no transparency). Default value is 1. Possible values include also : 
#'   "cos2", "contrib", "coord", "x" or "y". In this case, the transparency for 
#'   individual/variable colors are automatically controlled by their qualities 
#'   ("cos2"), contributions ("contrib"), coordinates (x^2 + y^2 , "coord"), x 
#'   values("x") or y values("y"). To use this, make sure that habillage 
#'   ="none".
#' @param shape.ind,shape.var point shapes of individuals, variables, groups and
#'   axes
#' @param select.ind,select.var a selection of individuals and variables to be 
#'   drawn. Allowed values are NULL or a list containing the arguments name, 
#'   cos2 or contrib: \itemize{ \item name is a character vector containing 
#'   individuals/variables to be drawn \item cos2 if cos2 is in [0, 1], ex: 0.6,
#'   then individuals/variables with a cos2 > 0.6 are drawn. if cos2 > 1, ex: 5,
#'   then the top 5 individuals/variables with the highest cos2 are drawn. \item
#'   contrib if contrib > 1, ex: 5,  then the top 5 individuals/variables with 
#'   the highest cos2 are drawn }
#' @param ... Arguments to be passed to the function fviz()
#' @param repel a boolean, whether to use ggrepel to avoid overplotting text
#'   labels or not. The old \code{jitter} argument is kept for backward
#'   compatibility and is converted to \code{repel = TRUE} with a deprecation warning.
#' @return a ggplot
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @examples
#' # Compute FAMD
#'  library("FactoMineR")
#'  data(wine)
#'  res.famd <- FAMD(wine[,c(1,2, 16, 22, 29, 28, 30,31)], graph = FALSE)
#'  res.famd.sup <- FAMD(wine[,c(1,2, 16, 22, 29, 28, 30,31)],
#'                       sup.var = 2, graph = FALSE)
#'                
#' # Eigenvalues/variances of dimensions
#' fviz_screeplot(res.famd)
#' # Graph of variables
#' fviz_famd_var(res.famd)
#' # Quantitative variables
#' fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")
#' # Qualitative variables
#' fviz_famd_var(res.famd, "quali.var", col.var = "black")
#' # Supplementary qualitative variable categories
#' fviz_famd_var(res.famd.sup, "quali.sup", col.var = "darkgreen")
#' # Graph of individuals colored by cos2
#' fviz_famd_ind(res.famd, col.ind = "cos2", 
#'   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#'   repel = TRUE)
#' 
#' 
#' @name fviz_famd
#' @rdname fviz_famd
#' @export
fviz_famd_ind <- function(X,  axes = c(1,2), geom=c("point", "text"), repel = FALSE,
                         habillage = "none", palette = NULL, addEllipses = FALSE, 
                         col.ind = "blue", col.ind.sup = "darkblue", alpha.ind = 1,
                         shape.ind = 19, col.quali.var = "black",
                         select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
                         gradient.cols = NULL,
                         ...)
{
  extra_args <- list(...)
  invisible <- if(is.null(extra_args$invisible)) "none" else extra_args$invisible
  quali.sup <- .get_factominer_quali_sup(X)
  
  if(col.ind %in% c("cos2","contrib", "coord")) partial = NULL
  
  show.quali.var <- !("quali.var" %in% invisible) && !is.null(X$quali.var)
  show.quali.sup <- !("quali.sup" %in% invisible) && !is.null(quali.sup)
  p <- NULL
  
  if(show.quali.var)
    p <- fviz_famd_var(X, "quali.var", axes = axes, geom =  geom, repel = repel,
                       col.var = col.quali.var,  ...)
  else if(show.quali.sup)
    p <- fviz_famd_var(X, "quali.sup", axes = axes, geom = geom, repel = repel,
                       col.var = col.quali.var, ...)
  
  # Individuals
  p <- fviz (X, element = "ind", axes = axes, geom = geom, habillage = habillage, 
        addEllipses = addEllipses, palette = palette, pointshape = shape.ind,
        color = col.ind, alpha = alpha.ind,
        shape.sup = shape.ind, col.row.sup = col.ind.sup,
        select = select.ind, repel = repel, ggp = p,  ...)
  
  if(!is.null(gradient.cols))
    p <- p + ggpubr::gradient_color(gradient.cols)
  
 
  p
  
}


#' @rdname fviz_famd
#' @export
fviz_famd_var <- function(X, choice = c("var", "quanti.var", "quali.var", "quali.sup"), axes = c(1,2),
                         geom = c("point", "text"), repel = FALSE, 
                         col.var ="red", alpha.var=1, shape.var = 17, 
                         col.var.sup = "darkgreen", 
                         select.var = list(name = NULL, cos2 = NULL, contrib = NULL), ...)
{
  extra_args <- list(...)
  if(missing(choice) && !is.null(extra_args$choix)) choice <- extra_args$choix
  choice <- match.arg(choice)

  if(choice == "quanti.var") {
    if(missing(geom)) geom <- c("arrow", "text")
  }
  else if(choice == "quali.var" && is.null(X$quali.var)) {
    stop("There are no qualitative variables to plot.")
  }
  else if(choice == "quali.sup" && is.null(.get_factominer_quali_sup(X))) {
    stop("There are no supplementary qualitative variables to plot.")
  }
  # Main plot
  fviz (X, element = choice, axes = axes, geom = geom,
        color = col.var, alpha = alpha.var,  pointshape = shape.var, 
        shape.sup = shape.var, col.col.sup = col.var.sup, 
        select = select.var, repel = repel,  ...)
}



#' @rdname fviz_famd
#' @export
fviz_famd <- function(X,  ...){
  fviz_famd_ind(X,  ...)
}
