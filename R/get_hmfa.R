#' @include print.factoextra.R
NULL
#' Extract the results for individuals/quantitative variables/qualitative variables/group/partial axes - HMFA
#' 
#' @description
#' Extract all the results (coordinates, squared cosine and contributions) 
#' for the active individuals/quantitative variable categories/qualitative variable categories/groups/partial axes from Hierarchical Multiple Factor Analysis (HMFA) outputs.\cr\cr
#' \itemize{
#' \item get_hmfa(): Extract the results for variables and individuals
#' \item get_hmfa_ind(): Extract the results for individuals only
#' \item get_hmfa_var_qanti(): Extract the results for quantitative variables only
#' \item get_hmfa_var_qali(): Extract the results for qualitative variables only
#' \item get_hmfa_group(): Extract the results for groups only
#' }
#' @param res.hmfa an object of class HMFA [FactoMineR].
#' @param element the element to subset from the output. Possible values are "ind", "quanti.var", "quali.var" or "group".
#' @return a list of matrices containing the results for the active 
#' individuals/quantitative variable categories/qualitative variable categories/groups/partial axes including : 
#' \item{coord}{coordinates for the individuals/quantitative variable categories/qualitative variable categories/groups/partial axes}
#' \item{cos2}{cos2 for the individuals/quantitative variable categories/qualitative variable categories/groups/partial axes}
#' \item{contrib}{contributions of the individuals/quantitative variable categories/qualitative variable categories/groups/partial axes}
#' \item{inertia}{inertia of the individuals/quantitative variable categories/qualitative variable categories/groups/partial axes}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @author Fabian Mundt \email{f.mundt@@inventionate.de}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' # Multiple Factor Analysis
#' # ++++++++++++++++++++++++
#' # Install and load FactoMineR to compute MFA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data(wine)
#' hierar <- list(c(2,5,3,10,9,2), c(4,2))
#' res.hmfa <- HMFA(wine, H = hierar, type=c("n",rep("s",5)), graph = FALSE)
#'  
#'  # Extract the results for qualitative variable categories
#'  var <- get_hmfa_quali_var(res.hmfa)
#'  print(var)
#'  head(var$coord) # coordinates of qualitative variables
#'  head(var$cos2) # cos2 of qualitative variables
#'  head(var$contrib) # contributions of qualitative variables
#'  
#'  # Extract the results for individuals
#'  ind <- get_hmfa_ind(res.hmfa)
#'  print(ind)
#'  head(ind$coord) # coordinates of individuals
#'  head(ind$cos2) # cos2 of individuals
#'  head(ind$contrib) # contributions of individuals
#'  
#'  # You can also use the function get_hmfa()
#'  get_hmfa(res.hmfa, "ind") # Results for individuals
#'  get_hmfa(res.hmfa, "quali.var") # Results for qualitative variable categories
#'  }
#'  
#' @name get_hmfa
#' 
#' @rdname get_hmfa
#' @export 
get_hmfa <- function(res.hmfa, element = c("ind", "quanti.var", "quali.var", "group")){
  elmt <- element[1]
  if(elmt == "ind") get_hmfa_ind(res.hmfa)
  else if(elmt == "quanti.var") get_hmfa_quanti_var(res.hmfa)
  else if(elmt == "quali.var") get_hmfa_quali_var(res.hmfa)
  else if(elmt == "group") get_hmfa_group(res.hmfa)
  else stop("Allowed values for the argument element are: 'ind', 'quanti.var', 'quali.var' or 'group'.")
}

#' @rdname get_hmfa
#' @export
get_hmfa_ind <- function(res.hmfa){
  # FactoMineR package
  if(inherits(res.hmfa, c("HMFA"))) ind <- res.hmfa$ind
  # @todo ade4 Support muss noch eingebaut werden!
  else stop("An object of class : ", class(res.hmfa), 
            " can't be handled by the function get_hmfa_ind()")
  class(ind)<-c("factoextra", "hmfa_ind")
  return(ind)
}

#' @rdname get_hmfa
#' @export
get_hmfa_quanti_var <- function(res.hmfa){
  # FactoMineR package
  if(is.null(res.hmfa$quanti.var)) stop("There are no quantitative variables in this HMFA.")
  else if(inherits(res.hmfa, c("HMFA"))) quanti_vars <- res.hmfa$quanti.var
  else stop("An object of class : ", class(res.hmfa), 
            " can't be handled by the function get_hmfa_quanti_var()")
  class(quanti_vars)<-c("factoextra", "hmfa_quanti_var")
  return(quanti_vars)
}

#' @rdname get_hmfa
#' @export
get_hmfa_quali_var <- function(res.hmfa){
  # FactoMineR package
  if(is.null(res.hmfa$quali.var)) stop("There are no qualitative variables in this HMFA.")
  if(inherits(res.hmfa, c("HMFA"))) quali_vars <- res.hmfa$quali.var
  else stop("An object of class : ", class(res.hmfa), 
            " can't be handled by the function get_hmfa_quali_var()")
  class(quali_vars)<-c("factoextra", "hmfa_quali_var")
  return(quali_vars)
}

#' @rdname get_hmfa
#' @export
get_hmfa_group <- function(res.hmfa){
  # FactoMineR package
  # Group calculation is only for first layer valid (see Pages 2015)
  if(inherits(res.hmfa, c("HMFA"))) group <- list(coord = res.hmfa$group$coord[[1]], canonical = res.hmfa$group$canonical)
  else stop("An object of class : ", class(res.hmfa), 
            " can't be handled by the function get_hmfa_group()")
  class(group)<-c("factoextra", "hmfa_group")
  return(group)
}

#' @rdname get_hmfa
#' @export
get_hmfa_partial <- function(res.hmfa){
  # FactoMineR package
  # Group calculation is only for first layer valid (see Pages 2015)
  if(inherits(res.hmfa, c("HMFA"))) partial <- res.hmfa$partial
  else stop("An object of class : ", class(res.hmfa), 
            " can't be handled by the function get_hmfa_partial()")
  class(partial)<-c("factoextra", "hmfa_partial")
  return(partial)
}