#' @include print.factoextra.R
NULL
#' Extract the results for individuals/variables - MCA
#' 
#' @description
#' Extract all the results (coordinates, squared cosine and contributions) 
#' for the active individuals/variable categories from Multiple Correspondence Analysis (MCA) outputs.\cr\cr
#' \itemize{
#' \item get_mca(): Extract the results for variables and individuals
#' \item get_mca_ind(): Extract the results for individuals only
#' \item get_mca_var(): Extract the results for variables only
#' }
#' @param res.mca an object of class MCA [FactoMineR], acm [ade4].
#' @param element the element to subset from the output. Possible values are "var" or "ind".
#' @return a list of matrices containing the results for the active 
#' individuals/variable categories including : 
#' \item{coord}{coordinates for the individuals/variable categories}
#' \item{cos2}{cos2 for the individuals/variable categories}
#' \item{contrib}{contributions of the individuals/variable categories}
#' \item{inertia}{inertia of the individuals/variable categories}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' # Multiple Correspondence Analysis
#' # ++++++++++++++++++++++++++++++
#' # Install and load FactoMineR to compute MCA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data(poison)
#' poison.active <- poison[1:55, 5:15]
#' head(poison.active[, 1:6])
#' res.mca <- MCA(poison.active, graph=FALSE)
#'  
#'  # Extract the results for variable categories
#'  var <- get_mca_var(res.mca)
#'  print(var)
#'  head(var$coord) # coordinates of variables
#'  head(var$cos2) # cos2 of variables
#'  head(var$contrib) # contributions of variables
#'  
#'  # Extract the results for individuals
#'  ind <- get_mca_ind(res.mca)
#'  print(ind)
#'  head(ind$coord) # coordinates of individuals
#'  head(ind$cos2) # cos2 of individuals
#'  head(ind$contrib) # contributions of individuals
#'  
#'  # You can also use the function get_mca()
#'  get_mca(res.mca, "ind") # Results for individuals
#'  get_mca(res.mca, "var") # Results for variable categories
#'  }
#'  
#' @name get_mca
#' 
#' @rdname get_mca
#' @export 
get_mca <- function(res.mca, element = c("var", "ind")){
 elmt <- element[1]
 if(elmt =="var") get_mca_var(res.mca)
 else if(elmt == "ind") get_mca_ind(res.mca)
 else stop("Allowed values for the argument element are: 'var' or 'ind'.")
}


#' @rdname get_mca
#' @export
get_mca_var <- function(res.mca){
  # FactoMineR package
  if(inherits(res.mca, c("MCA"))) vars <- res.mca$var
  # ade4 package
  else if(inherits(res.mca, "acm") & inherits(res.mca, 'dudi')){
    if (!requireNamespace("ade4", quietly = TRUE)) {
      stop("ade4 package needed for this function to work. Please install it.")
    }
    coord <- res.mca$co
    inertia <- ade4::inertia.dudi(res.mca, row.inertia = FALSE, col.inertia = TRUE)
    cos2 <- abs(inertia$col.rel/10000)[, colnames(coord)]
    contrib <- (inertia$col.abs/100)[, colnames(coord)]
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    vars <- list(coord = coord, contrib = contrib, cos2 = cos2)
  }
  else stop("An object of class : ", class(res.mca), 
            " can't be handled by the function get_mca_var()")
  class(vars)<-c("factoextra", "mca_var")
  return(vars)
}

#' @rdname get_mca
#' @export
get_mca_ind <- function(res.mca){
  # FactoMineR package
  if(inherits(res.mca, c("MCA"))) ind <- res.mca$ind
  # ade4 package
  else if(inherits(res.mca, "acm") & inherits(res.mca, 'dudi')){
    if (!requireNamespace("ade4", quietly = TRUE)) {
      stop("ade4 package needed for this function to work. Please install it.")
    }
    coord <- res.mca$li
    inertia <- ade4::inertia.dudi(res.mca, row.inertia = TRUE, col.inertia = FALSE)
    cos2 <- abs(inertia$row.rel/10000)[, colnames(coord)]
    contrib <- (inertia$row.abs/100)[, colnames(coord)]
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    ind <- list(coord = coord, contrib = contrib, cos2 = cos2)
  }
  
  else stop("An object of class : ", class(res.mca), 
            " can't be handled by the function get_mca_ind()")
  class(ind)<-c("factoextra", "mca_ind")
  return(ind)
}



