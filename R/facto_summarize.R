#' @include utilities.R get_pca.R eigenvalue.R
NULL
#' Subset and summarize the output of factor analyses
#' 
#' @description
#'  Subset and summarize the results of Principal Component Analysis (PCA), 
#' Correspondence Analysis (CA), Multiple Correspondence Analysis (MCA) and
#' Multiple Factor Analysis (MFA) functions from several packages.
#' @param X an object of class PCA, CA, MCA and MFA [FactoMineR]; prcomp and princomp [stats]; 
#'  dudi, pca, coa and acm [ade4]; ca [ca package].
#' @param element the element to subset from the output. Possible values are
#'  "row" or "col" for CA; "var" or "ind" for PCA and MCA; 'quanti.var', 'quali.var' or 'ind' for MFA
#' @param result the result to be extracted for the element. Possible values are
#'  the combination of c("cos2", "contrib", "coord")
#' @param axes a numeric vector specifying the axes of interest. Default values are 1:2
#'  for axes 1 and 2.
#' @param select a selection of variables. Allowed values are NULL or a list containing the arguments
#'  name, cos2 or contrib. Default is list(name = NULL, cos2 = NULL, contrib = NULL):
#'  \itemize{
#'  \item name: is a character vector containing variable names to be selected
#'  \item cos2: if cos2 is in [0, 1], ex: 0.6, then variables with a cos2 > 0.6 are selected.
#'   if cos2 > 1, ex: 5, then the top 5 variables with the highest cos2 are selected
#' \item contrib: if contrib > 1, ex: 5,  then the top 5 variables with the highest cos2 are selected. 
#'  }
#' @return A data frame containing the (total) coord, cos2 and the contribution for the axes.
#' @details If length(axes) > 1, then the columns contrib and cos2 correspond to the total contributions and total cos2
#'  of the axes. In this case, the column coord is calculated as x^2 + y^2 + ...+; x, y, ... are the coordinates of
#'  the points on the specified axes.
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @author Fabian Mundt \email{f.mundt@inventionate.de}
#' @references http://www.sthda.com
#' @examples
#' \donttest{
#' # Principal component analysis
#' # +++++++++++++++++++++++++++++
#' data(decathlon2)
#' decathlon2.active <- decathlon2[1:23, 1:10]
#' res.pca <- prcomp(decathlon2.active,  scale = TRUE)
#' 
#' # Summarize variables on axes 1:2
#' facto_summarize(res.pca, "var", axes = 1:2)[,-1]
#' # Select the top 5 contributing variables
#' facto_summarize(res.pca, "var", axes = 1:2,
#'            select = list(contrib = 5))[,-1]
#' # Select variables with cos2 >= 0.6
#' facto_summarize(res.pca, "var", axes = 1:2,
#'            select = list(cos2 = 0.6))[,-1]
#' # Select by names
#' facto_summarize(res.pca, "var", axes = 1:2,
#'      select = list(name = c("X100m", "Discus", "Javeline")))[,-1]
#'            
#' # Summarize individuals on axes 1:2
#' facto_summarize(res.pca, "ind", axes = 1:2)[,-1]
#' 
#' # Correspondence Analysis
#' # ++++++++++++++++++++++++++
#' # Install and load FactoMineR to compute CA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data("housetasks")
#' res.ca <- CA(housetasks, graph = FALSE)
#' # Summarize row variables on axes 1:2
#' facto_summarize(res.ca, "row", axes = 1:2)[,-1]
#' # Summarize column variables on axes 1:2
#' facto_summarize(res.ca, "col", axes = 1:2)[,-1]
#' 
#' # Multiple Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mca <- MCA(poison, quanti.sup = 1:2, 
#'               quali.sup = 3:4, graph=FALSE)
#' # Summarize variables on axes 1:2
#' res <- facto_summarize(res.mca, "var", axes = 1:2)
#' head(res)
#' # Summarize individuals on axes 1:2
#' res <- facto_summarize(res.mca, "ind", axes = 1:2)
#' head(res)
#' 
#' # Multiple factor Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mfa <- MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
#'                name.group=c("desc","desc2","symptom","eat"),
#'                num.group.sup=1:2, graph=FALSE)
#' # Summarize categorcial variables on axes 1:2
#' res <- facto_summarize(res.mfa, "quali.var", axes = 1:2)
#' head(res)
#' # Summarize individuals on axes 1:2
#' res <- facto_summarize(res.mfa, "ind", axes = 1:2)
#' head(res)
#'  }
#' @export 
facto_summarize <- function(X, element,
                            result = c("coord", "cos2", "contrib"),
                            axes=1:2, select = NULL)
                            
  { 
  # check element
  if(!element %in% c("row", "col", "var", "ind", "quanti.var", "quali.var", "group", "partial.axes"))
    stop('Te argument element should be one of "row", "col", "var", "ind", "quanti.var", "quali.var", "group", "partial.axes"')
  
  # check and get the classe of X
  facto_class <- .get_facto_class(X)
  
  # Extract the element
  element <- element[1]
  if(facto_class=="CA"){
  if(element %in% c("ind", "row")) elmt<- get_ca_row(X)
  else if(element  %in% c("var", "col") ) elmt <- get_ca_col(X)
  }
  else if(facto_class=="PCA"){
    if(element %in% c("var", "col")) elmt<- get_pca_var(X)
    else if(element %in% c("ind", "row")) elmt <- get_pca_ind(X)
  }
  else if(facto_class=="MCA"){
    if(element %in% c("var", "col")) elmt<- get_mca_var(X)
    else if(element %in% c("ind", "row")) elmt <- get_mca_ind(X)
  }
  else if (facto_class == "MFA") {
  if (element %in% c("quanti.var", "col")) elmt <- get_mfa_quanti_var(X)
  else if (element %in% c("quali.var", "col")) elmt <- get_mfa_quali_var(X)
  else if (element %in% c("group", "col")) elmt <- get_mfa_group(X)
  else if (element %in% c("partial.axes","col")) elmt <- get_mfa_partial_axes(X) 
  else if (element %in% c("ind", "row")) elmt <- get_mfa_ind(X)
}
  
  
  # check axes
  if(max(axes) > ncol(elmt$coord))
    stop("The value of the argument axes is incorrect. ",
         "The number of axes in the data is: ", ncol(elmt$coord), 
         ". Please try again with axes between 1 - ", ncol(elmt$coord))
  
  # summarize the result
  res = NULL
  
  # 1.Extract the coordinates x, y and coord
  if("coord" %in% result){
    dd <- data.frame(elmt$coord[, axes, drop=FALSE])
    coord <- apply(dd^2, 1, sum) # x^2 + y2 + ...
    res = cbind(dd, coord = coord)
  }
  
  # 2. Extract the cos2
  if("cos2" %in% result){
    cos2 <- elmt$cos2[, axes]
    if(length(axes) > 1) cos2 <- apply(cos2, 1, sum, na.rm=TRUE)
    res <- cbind(res, cos2 = cos2)
  }
  
  # 3. Extract the contribution
  if("contrib" %in% result){
    contrib <- elmt$contrib[, axes]
    if(length(axes) > 1) {
      eig <- get_eigenvalue(X)[axes,1]
      # Adjust variable contributions by the Dimension eigenvalues
      contrib <- t(apply(contrib, 1, 
                         function(var.contrib, pc.eig){var.contrib*pc.eig},
                         eig))
      contrib <-apply(contrib, 1, sum)
    }
    res <- cbind(res, contrib = contrib)
  }
  
  # 4.Extract the coordinates x, y and coord partial
  if("coord.partial" %in% result){
    dd <- data.frame(elmt$coord.partiel[, axes, drop=FALSE])
    # groupnames 
    groupnames <- data.frame(do.call('rbind', strsplit(as.character(rownames(dd)), '.', fixed = TRUE)))
    colnames(groupnames) <- c("name", "group.name")
    coord.partial <- apply(dd^2, 1, sum) # x^2 + y2 + ...
    res.partial <- data.frame(groupnames, dd, coord.partial)
  }
  
  name <- rownames(elmt$coord)
  if(is.null(name)) name <- as.character(1:nrow(elmt$coord))
  res <- cbind.data.frame(name = name, res)
  if(!is.null(select)) res <- .select(res, select)
  
  if("coord.partial" %in% result){
  res = list(res = res, res.partial = res.partial)
  }

  res
}
