#' @include print.factoextra.R
NULL
#' Extract the results for individuals and variables - FAMD
#' 
#' @description
#' Extract all the results (coordinates, squared cosine and contributions) 
#' for the active individuals and variables from Factor Analysis of Mixed Data (FAMD) outputs.\cr\cr
#' \itemize{
#' \item get_famd(): Extract the results for variables and individuals
#' \item get_famd_ind(): Extract the results for individuals only
#' \item get_famd_var(): Extract the results for quantitative and qualitative variables only
#' }
#' @param res.famd an object of class FAMD [FactoMineR].
#' @param element the element to subset from the output. Possible values are
#'   "ind", "var", "quanti.var", "quali.var" or "quali.sup".
#' @return a list of matrices containing the results for the active 
#' individuals and variables, including : 
#' \item{coord}{coordinates of individuals/variables.}
#' \item{cos2}{cos2 values representing the quality of representation on the factor map.}
#' \item{contrib}{contributions of individuals / variables to the principal components.}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @examples
#' 
#'  # Compute FAMD
#'  library("FactoMineR")
#'  data(wine)
#'  res.famd <- FAMD(wine[,c(1,2, 16, 22, 29, 28, 30,31)], graph = FALSE)
#'  res.famd.sup <- FAMD(wine[,c(1,2, 16, 22, 29, 28, 30,31)],
#'                       sup.var = 2, graph = FALSE)
#'  
#'  # Extract the results for qualitative variable categories
#'  quali.var <- get_famd_var(res.famd, "quali.var")
#'  print(quali.var)
#'  head(quali.var$coord) # coordinates of qualitative variables
#' 
#'  # Extract the results for supplementary qualitative variable categories
#'  quali.sup <- get_famd_var(res.famd.sup, "quali.sup")
#'  print(quali.sup)
#'  head(quali.sup$coord) # coordinates of supplementary qualitative variables
#'  
#'  # Extract the results for quantitative variables
#'  quanti.var <- get_famd_var(res.famd, "quanti.var")
#'  print(quanti.var)
#'  head(quanti.var$coord) # coordinates
#'  
#'  # Extract the results for individuals
#'  ind <- get_famd_ind(res.famd)
#'  print(ind)
#'  head(ind$coord) # coordinates of individuals
#'  
#'  
#' @name get_famd
#' 
#' @rdname get_famd
#' @export 
get_famd <- function(res.famd, element = c("ind",  "var", "quanti.var", "quali.var", "quali.sup")){
  elmt <- match.arg(element)
  switch(elmt,
         ind = get_famd_ind(res.famd),
         var = get_famd_var(res.famd, "var"),
         quanti.var = get_famd_var(res.famd, "quanti.var"),
         quali.var = get_famd_var(res.famd, "quali.var"),
         quali.sup = get_famd_var(res.famd, "quali.sup")
         )
}

#' @rdname get_famd
#' @export
get_famd_ind <- function(res.famd){
  # FactoMineR package
  if(inherits(res.famd, c("FAMD"))) ind <- res.famd$ind
  else stop("An object of class : ", paste(class(res.famd), collapse = ", "), 
            " can't be handled by the function get_famd_ind()")
  class(ind)<-c("factoextra", "famd", "famd_ind")
  attr(ind, "element") <- "individuals"
  return(ind)
}

#' @rdname get_famd
#' @export
get_famd_var <- function(res.famd, element = c("var", "quanti.var", "quali.var", "quali.sup")){
  choice <- match.arg(element)
  # FactoMineR package
  if(inherits(res.famd, "FAMD")) {
    quali.sup <- .get_factominer_quali_sup(res.famd)
    if(choice == "quanti.var" && is.null(res.famd$quanti.var))
      stop("There are no quantitative variables in this FAMD.")
    else if(choice == "quali.var" && is.null(res.famd$quali.var))
      stop("There are no qualitative variables in this FAMD.")
    else if(choice == "quali.sup" && is.null(quali.sup))
      stop("There are no supplementary qualitative variables in this FAMD.")

    vars <- switch(choice,
                   var = res.famd$var,
                   quanti.var = res.famd$quanti.var,
                   quali.var = res.famd$quali.var,
                   quali.sup = quali.sup
                   )
    element_desc <- switch(choice,
                      var = "variables",
                      quanti.var = "quantitative variables",
                      quali.var = "qualitative variable categories",
                      quali.sup = "supplementary qualitative variable categories"
    )
  }
  else stop("An object of class : ", paste(class(res.famd), collapse = ", "), 
            " can't be handled.")
  
  class(vars)<-c("factoextra",  "famd", paste0("famd_", gsub(".", "_", choice, fixed = TRUE)))
  attr(vars, "element") <- element_desc
  return(vars)
}
