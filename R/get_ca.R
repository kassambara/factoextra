#' @include print.factoextra.R
NULL
#' Extract the results for rows/columns - CA
#' 
#' @description
#' Extract all the results (coordinates, squared cosine, contributions and inertia) 
#' for the active row/column variables from Correspondence Analysis (CA) outputs.\cr\cr
#' \itemize{
#' \item get_ca(): Extract the results for rows and columns
#' \item get_ca_row(): Extract the results for rows only
#' \item get_ca_col(): Extract the results for columns only
#' }
#' @param res.ca an object of class CA [FactoMineR], ca [ca], coa [ade4];
#'  correspondence [MASS].
#' @param element the element to subset from the output. Possible values are "row" or "col".
#' @return a list of matrices containing the results for the active rows/columns including : 
#' \item{coord}{coordinates for the rows/columns}
#' \item{cos2}{cos2 for the rows/columns}
#' \item{contrib}{contributions of the rows/columns}
#' \item{inertia}{inertia of the rows/columns}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' # Install and load FactoMineR to compute CA
#' # install.packages("FactoMineR")
#'  library("FactoMineR")
#'  data("housetasks")
#'  res.ca <- CA(housetasks, graph = FALSE)
#'  
#' # Result for column variables
#'  col <- get_ca_col(res.ca)
#'  col # print
#'  head(col$coord) # column coordinates
#'  head(col$cos2) # column cos2
#'  head(col$contrib) # column contributions
#'  
#' # Result for row variables
#'  row <- get_ca_row(res.ca)
#'  row # print
#'  head(row$coord) # row coordinates
#'  head(row$cos2) # row cos2
#'  head(row$contrib) # row contributions
#'  
#'  # You can also use the function get_ca()
#'  get_ca(res.ca, "row") # Results for rows
#'  get_ca(res.ca, "col") # Results for columns
#'  }
#' @name get_ca
#' 
#' @rdname get_ca
#' @export 
get_ca <- function(res.ca, element = c("row", "col")){
 elmt <- element[1]
 if(elmt =="row") get_ca_row(res.ca)
 else if(elmt == "col") get_ca_col(res.ca)
 else stop("Allowed values for the argument element are: 'row' or 'col'.")
}


#' @rdname get_ca
#' @export
get_ca_col <- function(res.ca){
  # FactoMineR package
  if(inherits(res.ca, c("CA"))) cols <- res.ca$col
  
  # ca package
  else if(inherits(res.ca, "ca")){
    # principal coord = standard coord X sqrt(eig)
    coord <- t(apply(res.ca$colcoord, 1, "*", res.ca$sv))
    cos2 <- apply(coord^2, 2, "/", res.ca$coldist^2)
    # col.contrib <- res.ca$colmass * col.coord^2/res.ca$sv^2
    cc <- apply(coord^2, 2, "*", res.ca$colmass)
    contrib <- t(apply(cc, 1, "/", res.ca$sv^2)) *100
    inertia <- res.ca$colinertia
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    # remove supplementary points
    index <- setdiff(1:nrow(res.ca$colcoord), res.ca$colsup)
    cols <- list(coord = coord[index, , drop = FALSE], 
                 contrib = contrib[index, , drop = FALSE],
                 cos2 = cos2[index, , drop = FALSE], inertia = inertia[index]) 
  }
  # Mass package
  else if(inherits(res.ca, "correspondence")){
    # principal coord = standard coord X sqrt(eig)
    coord <- t(apply(res.ca$cscore, 1, "*", res.ca$cor))
    # cos2 = coord^2/d^2
    row.sum <- apply(res.ca$Freq, 1, sum)
    col.sum <- apply(res.ca$Freq, 2, sum)
    n <- sum(res.ca$Freq)
    profile <- t(apply(res.ca$Freq, 1, "/", col.sum))
    average.profile <- row.sum/n
    d2 <- apply(profile, 2, 
                function(row.p, av.p){sum(((row.p - av.p)^2)/av.p)}, 
                average.profile)
    cos2 <- apply(coord^2, 2, "/", d2)
    # contrib <- mass * coord^2/eig
    mass <- col.sum/n
    cc <- apply(coord^2, 2, "*", mass)
    contrib <- t(apply(cc, 1, "/", res.ca$cor^2)) *100
    # inertia = mass * d^2
    inertia <- mass * d2
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    cols <- list(coord = coord, contrib = contrib, cos2 = cos2, inertia = inertia)
  }
  # ade4 package
  else if(inherits(res.ca, "coa") & inherits(res.ca, 'dudi')){
    if (!requireNamespace("ade4", quietly = TRUE)) {
      stop("ade4 package needed for this function to work. Please install it.")
    }
    coord <- res.ca$co
    inertia <- ade4::inertia.dudi(res.ca, row.inertia = FALSE, col.inertia = TRUE)
    cos2 <- abs(inertia$col.rel/10000)[, colnames(coord)]
    contrib <- (inertia$col.abs/100)[, colnames(coord)]
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    cols <- list(coord = coord, contrib = contrib, cos2 = cos2, inertia = NA)
  }
  
  else stop("An object of class : ", class(res.ca), 
            " can't be handled by the function get_ca_col()")
  class(cols)<-c("factoextra", "ca_col")
  return(cols)
}

#' @rdname get_ca
#' @export
get_ca_row <- function(res.ca){
  
  # FactoMineR package
  if(inherits(res.ca, c("CA"))) row <- res.ca$row
  
  # ca package
  else if(inherits(res.ca, "ca")){
    # principal coord = standard coord X sqrt(eig)
    coord <- t(apply(res.ca$rowcoord, 1, "*", res.ca$sv))
    cos2 <- apply(coord^2, 2, "/", res.ca$rowdist^2)
    # contrib <- res.ca$rowmass * coord^2/res.ca$sv^2
    cc <- apply(coord^2, 2, "*", res.ca$rowmass)
    contrib <- t(apply(cc, 1, "/", res.ca$sv^2)) *100
    inertia <- res.ca$rowinertia
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    # remove supplementary points
    index <- setdiff(1:nrow(res.ca$rowcoord), res.ca$rowsup)
    row <- list(coord = coord[index, , drop = FALSE], 
                contrib = contrib[index, , drop = FALSE],
                cos2 = cos2[index, , drop = FALSE], inertia = inertia[index])  
  }
  # Mass package
  else if(inherits(res.ca, "correspondence")){
    # principal coord = standard coord X sqrt(eig)
    coord <- t(apply(res.ca$rscore, 1, "*", res.ca$cor))
    # cos2 = coord^2/d^2
    row.sum <- apply(res.ca$Freq, 1, sum)
    col.sum <- apply(res.ca$Freq, 2, sum)
    n <- sum(res.ca$Freq)
    profile <- res.ca$Freq/row.sum
    average.profile <- col.sum/n
    d2 <- apply(profile, 1, 
                function(row.p, av.p){sum(((row.p - av.p)^2)/av.p)}, 
                average.profile)
    cos2 <- apply(coord^2, 2, "/", d2)
    # contrib <- mass * coord^2/eig
    mass <- row.sum/n
    cc <- apply(coord^2, 2, "*", mass)
    contrib <- t(apply(cc, 1, "/", res.ca$cor^2)) *100
    # inertia = mass * d^2
    inertia <- mass * d2
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    row <- list(coord = coord, contrib = contrib, cos2 = cos2, inertia = inertia)
  }
  
  # ade4 package
  else if(inherits(res.ca, "coa") & inherits(res.ca, 'dudi')){
    if (!requireNamespace("ade4", quietly = TRUE)) {
      stop("ade4 package needed for this function to work. Please install it.")
    }
    coord <- res.ca$li
    inertia <- ade4::inertia.dudi(res.ca, row.inertia = TRUE, col.inertia = FALSE)
    cos2 <- abs(inertia$row.rel/10000)[, colnames(coord)]
    contrib <- (inertia$row.abs/100)[, colnames(coord)]
    colnames(coord) <- colnames(cos2) <- colnames(contrib) <- paste0("Dim.", 1:ncol(coord)) 
    row <- list(coord = coord, contrib = contrib, cos2 = cos2, inertia = NA)
  }
  else stop("An object of class : ", class(res.ca), 
            " can't be handled by the function get_ca_row()")
  class(row)<-c("factoextra", "ca_row")
  return(row)
}



