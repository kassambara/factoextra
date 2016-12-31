#' @include print.factoextra.R
NULL
#' Extract the results for individuals/variables/group/partial axes - HMFA
#' 
#' @description Extract all the results (coordinates, squared cosine and
#' contributions) for the active individuals/quantitative variables/qualitative
#' variable categories/groups/partial axes from Hierarchical Multiple Factor
#' Analysis (HMFA) outputs.\cr\cr \itemize{ \item get_hmfa(): Extract the
#' results for variables and individuals \item get_hmfa_ind(): Extract the
#' results for individuals only \item get_mfa_var(): Extract the results for
#' variables (quantitatives, qualitatives and groups) 
#' \item get_hmfa_partial(): Extract the results for partial.node. }
#' 
#' @param res.hmfa an object of class HMFA [FactoMineR].
#' @param element the element to subset from the output. Possible values are
#'   "ind", "quanti.var", "quali.var", "group" or "partial.node".
#' @return a list of matrices containing the results for the active 
#'   individuals, variables, groups and partial nodes, including : 
#'   \item{coord}{coordinates} \item{cos2}{cos2} 
#'   \item{contrib}{contributions}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @author Fabian Mundt \email{f.mundt@@inventionate.de}
#' @examples
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
#'  var <- get_hmfa_var(res.hmfa, "quali.var")
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
#'  
#' @name get_hmfa
#'   
#' @rdname get_hmfa
#' @export
get_hmfa <- function(res.hmfa, element = c("ind", "quanti.var", "quali.var", "group", "partial.node")){
  elmt <- match.arg(element)
  if(elmt == "ind") get_hmfa_ind(res.hmfa)
  else if(elmt %in% c("quanti.var", "quali.var", "group"))
    get_hmfa_var(res.hmfa, elmt)
  else if(elmt == "partial.node") get_hmfa_partial(res.hmfa)
}

#' @rdname get_hmfa
#' @export
get_hmfa_ind <- function(res.hmfa){
  # FactoMineR package
  if(inherits(res.hmfa, c("HMFA"))) ind <- res.hmfa$ind
  # @todo ade4 Support muss noch eingebaut werden!
  else stop("An object of class : ", class(res.hmfa), 
            " can't be handled by the function get_hmfa_ind()")
  class(ind)<-c("factoextra", "hmfa", "hmfa_ind")
  attr(ind, "element") <- "individuals"
  return(ind)
}

#' @rdname get_hmfa
#' @export
get_hmfa_var <- function(res.hmfa, element = c( "quanti.var", "quali.var", "group")){
  
  choice <- match.arg(element)
  if(!inherits(res.hmfa, "HMFA"))
    stop("An object of class : ", class(res.hmfa), " can't be handled.")
  
  if(choice == "quanti.var" & is.null(res.hmfa$quanti.var)) 
    stop("There are no quantitative variables in this HMFA.")
  else if(choice == "quali.var" & is.null(res.hmfa$quali.var)) 
    stop("There are no qualitative variables in this HMFA.")
  
  
  vars <- switch(choice,
                 quanti.var = res.hmfa$quanti.var,
                 quali.var = res.hmfa$quali.var,
                 group = list(coord = res.hmfa$group$coord[[1]], canonical = res.hmfa$group$canonical)
  )
  element_desc <- switch(choice,
                         quanti.var = "quantitative variables",
                         quali.var = "qualitative variable categories",
                         group = "variable groups"
  )
  
  class(vars)<-c("factoextra",  "hmfa", paste0("hmfa_", gsub(".", "_", choice, fixed = TRUE)))
  attr(vars, "element") <- element_desc
  return(vars)
}

#' @rdname get_hmfa
#' @export
get_hmfa_partial <- function(res.hmfa){
  # FactoMineR package
  # Group calculation is only for first layer valid (see Pages 2015)
  if(inherits(res.hmfa, c("HMFA"))) partial <- res.hmfa$partial
  else stop("An object of class : ", class(res.hmfa), 
            " can't be handled by the function get_hmfa_partial()")
  class(partial)<-c("list",  "hmfa_partial")
  attr(partial, "element") <- "partial node"
  return(partial)
}