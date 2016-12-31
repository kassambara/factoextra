#' Print method for an object of class factoextra
#' 
#' @description
#' Print method for an object of class factoextra
#' @param x an object of class factoextra
#' @param ... further arguments to be passed to print method
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @examples
#'  data(iris)
#'  res.pca <- prcomp(iris[, -5],  scale = TRUE)
#'  ind <- get_pca_ind(res.pca, data = iris[, -5])
#'  print(ind)
#'  
#' @export
print.factoextra<-function(x, ...){
  if(!inherits(x, "factoextra"))
    stop("Can't handle data of class ", class(x))
  
  if(inherits(x, "pca_ind")){
    cat("Principal Component Analysis Results for individuals\n",
        "===================================================\n")
    res <- array(data="", dim=c(3,2), dimnames=list(1:3, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for the individuals")
    res[2, ] <- c("$cos2", "Cos2 for the individuals")
    res[3, ] <- c("$contrib", "contributions of the individuals")
    print(res[1:3,], ...)
  }
  
  else if(inherits(x, "pca_var")){
    cat("Principal Component Analysis Results for variables\n",
        "===================================================\n")
    res <- array(data="", dim=c(4,2), dimnames=list(1:4, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for the variables")
    res[2, ] <- c("$cor", "Correlations between variables and dimensions")
    res[3, ] <- c("$cos2", "Cos2 for the variables")
    res[4, ] <- c("$contrib", "contributions of the variables")
    print(res[1:4,])
  }
  
  else if(inherits(x, "ca_row")){
    cat("Correspondence Analysis - Results for rows\n",
        "===================================================\n")
    res <- array(data="", dim=c(4,2), dimnames=list(1:4, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for the rows")
    res[2, ] <- c("$cos2", "Cos2 for the rows")
    res[3, ] <- c("$contrib", "contributions of the rows")
    res[4, ] <- c("$inertia", "Inertia of the rows")
    print(res[1:4,])
  }
  
  else if(inherits(x, "ca_col")){
    cat("Correspondence Analysis - Results for columns\n",
        "===================================================\n")
    res <- array(data="", dim=c(4,2), dimnames=list(1:4, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for the columns")
    res[2, ] <- c("$cos2", "Cos2 for the columns")
    res[3, ] <- c("$contrib", "contributions of the columns")
    res[4, ] <- c("$inertia", "Inertia of the columns")
    print(res[1:4,])
  }
  else if(inherits(x, "mca_ind")){
    cat("Multiple Correspondence Analysis Results for individuals\n",
        "===================================================\n")
    res <- array(data="", dim=c(3,2), dimnames=list(1:3, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for the individuals")
    res[2, ] <- c("$cos2", "Cos2 for the individuals")
    res[3, ] <- c("$contrib", "contributions of the individuals")
    print(res[1:3,], ...)
  }
  else if(inherits(x, "mca_var")){
    cat("Multiple Correspondence Analysis Results for variables\n",
        "===================================================\n")
    res <- array(data="", dim=c(3,2), dimnames=list(1:3, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for categories")
    res[2, ] <- c("$cos2", "Cos2 for categories")
    res[3, ] <- c("$contrib", "contributions of categories")
    print(res[1:3,])
  }
  else if(inherits(x, "famd")){
    element <- attr(x, "element") # description
    cat("FAMD results for", element, "\n",
        "===================================================\n")
    res <- array(data="", dim=c(3,2), dimnames=list(1:3, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates")
    res[2, ] <- c("$cos2", "Cos2, quality of representation")
    res[3, ] <- c("$contrib", "Contributions")
    print(res[1:3,])
  }
  else if(inherits(x, "mfa")){
    # result: nrows x 2 cols
    if(inherits(x, "mfa_ind")) nrows <- 6
    else if(inherits(x, c("mfa_quali_var", "mfa_quanti_var", "mfa_partial_axes"))) nrows <-3
    else if(inherits(x, "mfa_group")) nrows <- 4
    res <- array(data="", dim = c(nrows, 2), dimnames=list(1:nrows, c("Name", "Description")))
    
    # Element
    element <- attr(x, "element") # description
    cat("Multiple Factor Analysis results for", element, "\n",
        "===================================================\n")
    res[1, ] <- c("$coord", "Coordinates")
    res[2, ] <- c("$cos2", "Cos2, quality of representation")
    res[3, ] <- c("$contrib", "Contributions")
    
    if(inherits(x, "mfa_group")) 
      res[4, ] <- c("$correlation", "Correlation between groups and principal dimensions")
    else if(inherits(x, "mfa_ind")){
      res[4, ] <- c("$coord.partiel", "Partial coordinates")
      res[5, ] <- c("$within.inertia", "Within inertia")
      res[6, ] <- c("$within.partial.inertia", "Within partial inertia")
    }
    print(res[1:nrows,], ...)
  }
  else if(inherits(x, c("hmfa_ind", "hmfa_quali_var", "hmfa_quanti_var"))){
    element <- attr(x, "element") # description
    cat("Hierarchical Multiple Factor Analysis results for", element, "\n",
        "===================================================\n")
    res <- array(data="", dim=c(3,2), dimnames=list(1:3, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates")
    res[2, ] <- c("$cos2", "Cos2, quality of representation")
    res[3, ] <- c("$contrib", "Contributions")
    print(res[1:3,], ...)
  }
  # canonical correlation coefficients added
  else if(inherits(x, "hmfa_group")){
    cat("Hierarchical Multiple Factor Analysis Results for groups\n",
        "===================================================\n")
    res <- array(data="", dim=c(4,2), dimnames=list(1:4, c("Name", "Description")))
    res[1, ] <- c("$coord", "Coordinates for groups")
    res[4, ] <- c("$canonical", "canonical correlation coefficient")
    print(res[1:4,], ...)
  }
  
}
