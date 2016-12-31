#' @include print.factoextra.R
NULL
#' Extract the results for individuals/variables/group/partial axes - MFA
#' 
#' @description
#' Extract all the results (coordinates, squared cosine and contributions) 
#' for the active individuals/quantitative variables/qualitative variable categories/groups/partial axes from Multiple Factor Analysis (MFA) outputs.\cr\cr
#' \itemize{
#' \item get_mfa(): Extract the results for variables and individuals
#' \item get_mfa_ind(): Extract the results for individuals only
#' \item get_mfa_var(): Extract the results for variables (quantitatives, qualitatives and groups)
#' \item get_mfa_partial_axes(): Extract the results for partial axes only
#' }   
#'     
#' 
#' @param res.mfa an object of class MFA [FactoMineR].
#' @param element the element to subset from the output. Possible values are "ind", "quanti.var", "quali.var", "group" or "partial.axes".
#' @return a list of matrices containing the results for the active 
#' individuals/quantitative variable categories/qualitative variable categories/groups/partial axes including : 
#' \item{coord}{coordinates for the individuals/quantitative variable categories/qualitative variable categories/groups/partial axes}
#' \item{cos2}{cos2 for the individuals/quantitative variable categories/qualitative variable categories/groups/partial axes}
#' \item{contrib}{contributions of the individuals/quantitative variable categories/qualitative variable categories/groups/partial axes}
#' \item{inertia}{inertia of the individuals/quantitative variable categories/qualitative variable categories/groups/partial axes}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @author Fabian Mundt \email{f.mundt@@inventionate.de}
#' @examples
#' # Multiple Factor Analysis
#' # ++++++++++++++++++++++++
#' # Install and load FactoMineR to compute MFA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data(poison)
#' res.mfa <- MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
#' name.group=c("desc","desc2","symptom","eat"), num.group.sup=1:2,
#' graph = FALSE)
#'  
#'  # Extract the results for qualitative variable categories
#'  var <- get_mfa_var(res.mfa, "quali.var")
#'  print(var)
#'  head(var$coord) # coordinates of qualitative variables
#'  head(var$cos2) # cos2 of qualitative variables
#'  head(var$contrib) # contributions of qualitative variables
#'  
#'  # Extract the results for individuals
#'  ind <- get_mfa_ind(res.mfa)
#'  print(ind)
#'  head(ind$coord) # coordinates of individuals
#'  head(ind$cos2) # cos2 of individuals
#'  head(ind$contrib) # contributions of individuals
#'  
#'  # You can also use the function get_mfa()
#'  get_mfa(res.mfa, "ind") # Results for individuals
#'  get_mfa(res.mfa, "quali.var") # Results for qualitative variable categories
#'  
#' @name get_mfa
#' 
#' @rdname get_mfa
#' @export 
get_mfa <- function(res.mfa, element = c("ind", "quanti.var", "quali.var", "group", "partial.axes")){
  elmt <- match.arg(element)
  if(elmt == "ind") get_mfa_ind(res.mfa)
  else if(elmt %in% c("quanti.var", "quali.var", "group"))
    get_mfa_var(res.mfa, elmt)
  else if(elmt == "partial.axes") get_mfa_partial_axes(res.mfa)
}

#' @rdname get_mfa
#' @export
get_mfa_ind <- function(res.mfa){
  # FactoMineR package
  if(inherits(res.mfa, c("MFA"))) ind <- res.mfa$ind
  # @todo ade4 Support muss noch eingebaut werden!
  else stop("An object of class : ", class(res.mfa), 
            " can't be handled by the function get_mfa_ind()")
  class(ind)<-c("factoextra", "mfa", "mfa_ind")
  attr(ind, "element") <- "individuals"
  return(ind)
}

#' @rdname get_mfa
#' @export
get_mfa_var <- function(res.mfa, element = c( "quanti.var", "quali.var", "group")){
  
  choice <- match.arg(element)
  if(!inherits(res.mfa, "MFA"))
    stop("An object of class : ", class(res.mfa), " can't be handled.")
  
  if(choice == "quanti.var" & is.null(res.mfa$quanti.var)) 
    stop("There are no quantitative variables in this MFA.")
  else if(choice == "quali.var" & is.null(res.mfa$quali.var)) 
    stop("There are no qualitative variables in this MFA.")
    
  
    vars <- switch(choice,
                   quanti.var = res.mfa$quanti.var,
                   quali.var = res.mfa$quali.var,
                   group = res.mfa$group
    )
    element_desc <- switch(choice,
                           quanti.var = "quantitative variables",
                           quali.var = "qualitative variable categories",
                           group = "variable groups"
    )
  
  class(vars)<-c("factoextra",  "mfa", paste0("mfa_", gsub(".", "_", choice, fixed = TRUE)))
  attr(vars, "element") <- element_desc
  return(vars)
}

#' @rdname get_mfa
#' @export
get_mfa_partial_axes <- function(res.mfa){
  # FactoMineR package
  if(inherits(res.mfa, c("MFA"))) partial_axes <- res.mfa$partial.axes
  else stop("An object of class : ", class(res.mfa), 
            " can't be handled by the function get_mfa_partial_axes()")
  class(partial_axes)<-c("factoextra", "mfa", "mfa_partial_axes")
  attr(partial_axes, "element") <- "partial axes"
  return(partial_axes)
}