#' @include print.factoextra.R
NULL
#' Extract the results for individuals/variables - MCA
#' 
#' @description Extract all the results (coordinates, squared cosine and 
#'   contributions) for the active individuals/variable categories from Multiple
#'   Correspondence Analysis (MCA) outputs.\cr\cr \itemize{ \item get_mca(): 
#'   Extract the results for variables and individuals \item get_mca_ind(): 
#'   Extract the results for individuals only \item get_mca_var(): Extract the 
#'   results for variables only }
#' @param res.mca an object of class MCA [FactoMineR], acm [ade4], expoOutput/epMCA [ExPosition].
#' @param element the element to subset from the output. Possible values are 
#'   "var" for variables, "ind" for individuals, "mca.cor" for correlation
#'   between variables and principal dimensions, "quanti.sup" for quantitative supplementary variables.
#' @return a list of matrices containing the results for the active 
#'   individuals/variable categories including : \item{coord}{coordinates for 
#'   the individuals/variable categories} \item{cos2}{cos2 for the 
#'   individuals/variable categories} \item{contrib}{contributions of the 
#'   individuals/variable categories} \item{inertia}{inertia of the 
#'   individuals/variable categories}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com/english/
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
get_mca <- function(res.mca, element = c("var", "ind", "mca.cor", "quanti.sup")){
 elmt <- match.arg(element)
 if(elmt == "ind") get_mca_ind(res.mca)
 else get_mca_var(res.mca, elmt)
}


#' @rdname get_mca
#' @export
get_mca_var <- function(res.mca, element = c( "var", "mca.cor", "quanti.sup")){
  choice <- match.arg(element)
  element_desc <- "variables"
  # FactoMineR package
  if(inherits(res.mca, c("MCA"))) {
    vars <- switch(choice,
                   var = res.mca$var,
                   mca.cor = list(coord = res.mca$var$eta2),
                   quanti.sup = res.mca$quanti.sup
    )
    element_desc <- switch(choice,
                           var = "variables",
                           mca.cor = "correlation between variables and dimensions",
                           quanti.sup = "quantitative supplementary variables"
    )
  }
  # ade4 package
  else if(inherits(res.mca, "acm") & inherits(res.mca, 'dudi')){
    if (!requireNamespace("ade4", quietly = TRUE)) {
      stop("ade4 package needed for this function to work. Please install it.")
    }
    if(choice == "var"){
    coord <- res.mca$co
    inertia <- ade4::inertia.dudi(res.mca, row.inertia = FALSE, col.inertia = TRUE)
    vv <- as.character(utils::packageVersion("ade4"))
    cc <- utils::compareVersion(vv, "1.7.4") > 0
    if(cc){
      # "v>1.7.4"
      cos2 <- abs(inertia$col.rel/100)[, 1:ncol(coord)]
      contrib <- (inertia$col.abs)[, 1:ncol(coord)]
    }
    # v<=1.7.4
    else {
      cos2 <- abs(inertia$col.rel/10000)[, 1:ncol(coord)]
      contrib <- (inertia$col.abs/100)[, 1:ncol(coord)]
    }
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    vars <- list(coord = coord, contrib = contrib, cos2 = cos2)
    }
    else{
      vars <- list()
      stop("Don't handle ", choice, " for MCA computed with ade4. Use FactoMineR instead.")
    }
  }
  # ExPosition package
  else if (inherits(res.mca, "expoOutput") & inherits(res.mca$ExPosition.Data,'epMCA')){
    if(choice != "var")  stop("Don't handle ", choice, " for MCA computed with ExPosition.",
                              " Use FactoMineR instead.")
    res <- res.mca$ExPosition.Data
    coord <- res$fj
    inertia <- res$dj*res$W
    cos2 <- res$rj
    contrib <- res$cj*100
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    vars <- list(coord = coord, contrib = contrib, cos2 = cos2)
  }
  else stop("An object of class : ", class(res.mca), 
            " can't be handled by the function get_mca_var()")
  element_class <-switch(choice,
                         var = "mca_var", mca.cor = "mca_cor", quanti.sup = "quanti.sup")
  class(vars)<-c("factoextra",  "mca", element_class)
  attr(vars, "element") <- element_desc
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
    vv <- as.character(utils::packageVersion("ade4"))
    cc <- utils::compareVersion(vv, "1.7.4") > 0
    if(cc){
      # "v>1.7.4"
      cos2 <- abs(inertia$row.rel/100)[, 1:ncol(coord)]
      contrib <- (inertia$row.abs)[, 1:ncol(coord)]
    }
    # v<=1.7.4
    else {
      cos2 <- abs(inertia$row.rel/10000)[, 1:ncol(coord)]
      contrib <- (inertia$row.abs/100)[, 1:ncol(coord)]
    }
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    ind <- list(coord = coord, contrib = contrib, cos2 = cos2)
  }
  # ExPosition package
  else if (inherits(res.mca, "expoOutput") & inherits(res.mca$ExPosition.Data,'epMCA')){
    res <- res.mca$ExPosition.Data
    coord <- res$fi
    inertia <- res$di*res$M
    cos2 <- res$ri
    contrib <- res$ci*100
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    ind <- list(coord = coord, contrib = contrib, cos2 = cos2)
  }
  
  else stop("An object of class : ", class(res.mca), 
            " can't be handled by the function get_mca_ind()")
  class(ind)<-c("factoextra", "mca", "mca_ind")
  attr(ind, "element") <- "individuals"
  return(ind)
}



