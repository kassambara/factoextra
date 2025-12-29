#' @include utilities.R get_pca.R eigenvalue.R
NULL
#' Subset and summarize the output of factor analyses
#' 
#' @description Subset and summarize the results of Principal Component Analysis
#'   (PCA), Correspondence Analysis (CA), Multiple Correspondence Analysis
#'   (MCA), Factor Analysis of Mixed Data (FAMD), Multiple Factor Analysis
#'   (MFA) and Hierarchical Multiple Factor Analysis (HMFA) functions from several packages.
#' @param X an object of class PCA, CA, MCA, FAMD, MFA and HMFA [FactoMineR]; prcomp
#'   and princomp [stats]; dudi, pca, coa and acm [ade4]; ca [ca package]; expoOutput [ExPosition].
#' @param element the element to subset from the output. Possible values are 
#'   "row" or "col" for CA; "var" or "ind" for PCA and MCA; "mca.cor" for MCA; 
#'   'quanti.var', 'quali.var' , 'group' or 'ind' for FAMD, MFA and HMFA.
#' @param result the result to be extracted for the element. Possible values are
#'   the combination of c("cos2", "contrib", "coord")
#' @param group.names a vector containing the name of the groups (by default, 
#'   NULL and the group are named group.1, group.2 and so on).
#' @param node.level a single number indicating the HMFA node level.
#' @param axes a numeric vector specifying the axes of interest. Default values 
#'   are 1:2 for axes 1 and 2.
#' @param select a selection of variables. Allowed values are NULL or a list 
#'   containing the arguments name, cos2 or contrib. Default is list(name = 
#'   NULL, cos2 = NULL, contrib = NULL): \itemize{ \item name: is a character 
#'   vector containing variable names to be selected \item cos2: if cos2 is in 
#'   [0, 1], ex: 0.6, then variables with a cos2 > 0.6 are selected. if cos2 > 
#'   1, ex: 5, then the top 5 variables with the highest cos2 are selected \item
#'   contrib: if contrib > 1, ex: 5,  then the top 5 variables with the highest 
#'   cos2 are selected. }
#' @return A data frame containing the (total) coord, cos2 and the contribution 
#'   for the axes.
#' @details If length(axes) > 1, then the columns contrib and cos2 correspond to
#'   the total contributions and total cos2 of the axes. In this case, the 
#'   column coord is calculated as x^2 + y^2 + ...+; x, y, ... are the 
#'   coordinates of the points on the specified axes.
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com/english/
#' @examples
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
#' @export
facto_summarize <- function(X, element, node.level = 1, group.names, 
                            result = c("coord", "cos2", "contrib"),
                            axes=1:2, select = NULL)
                            
  { 
  # check element
  allowed_elmts <- c("row", "col", "var", "ind", "quanti.var", "quali.var",
                     "mca.cor", "quanti.sup",  "group", "partial.axes", "partial.node")
  if(!element %in% allowed_elmts) stop("Can't handle element = '", element, "'") 
  if(element %in% c("mca.cor", "quanti.sup")) {
    if(!inherits(X, "MCA")) stop("element = 'mca_cor' is supported only for FactoMineR::MCA().")
    result <- NULL
  }
  
  # Check and get the classe of X
  facto_class <- .get_facto_class(X)
  # Extract the element
  element <- element[1]
  elmt <- switch(facto_class,
                 CA = get_ca(X, element),
                 PCA = get_pca(X, element),
                 MCA = get_mca(X, element),
                 FAMD = get_famd(X, element),
                 MFA = get_mfa(X, element),
                 HMFA = get_hmfa(X, element)
                 )
  
  # Check axes
  if(inherits(elmt, "hmfa_partial"))
    ndim <- ncol(elmt[[1]])
  else
    ndim <- ncol(elmt$coord)
  if(max(axes) > ndim)
    stop("The value of the argument axes is incorrect. ",
         "The number of axes in the data is: ", ncol(elmt$coord), 
         ". Please try again with axes between 1 - ", ncol(elmt$coord))
  
  # Summarize the result
  res = NULL
  # Fix for PR #143: respect axes parameter for mca.cor and quanti.sup
  if(element %in% c("mca.cor", "quanti.sup")) res <- elmt$coord[, axes, drop = FALSE]
  
  # 1.Extract the coordinates x, y and coord
  # OPTIMIZATION: Replaced apply(dd^2, 1, sum) with rowSums(dd^2)
  # rowSums() is significantly faster than apply() for row summation
  if("coord" %in% result){
    dd <- data.frame(elmt$coord[, axes, drop=FALSE], stringsAsFactors = TRUE)
    coord <- rowSums(dd^2) # x^2 + y2 + ... (vectorized)
    res = cbind(dd, coord = coord)
  }

  # 2. Extract the cos2
  # OPTIMIZATION: Replaced apply() with rowSums() for multi-axis case
  if("cos2" %in% result){
    cos2 <- elmt$cos2[, axes, drop = FALSE]
    if(length(axes) > 1) {
      cos2 <- rowSums(cos2, na.rm=TRUE)
    } else {
      # Single axis - convert to vector to ensure column name is 'cos2'
      cos2 <- as.vector(cos2)
    }
    res <- cbind(res, cos2 = cos2)
  }

  # 3. Extract the contribution
  # OPTIMIZATION: Replaced t(apply()) with sweep() and rowSums()
  # sweep() for element-wise multiplication, rowSums() for summation
  if("contrib" %in% result){
    contrib <- elmt$contrib[, axes, drop = FALSE]
    if(length(axes) > 1) {
      eig <- get_eigenvalue(X)[axes,1]
      # OPTIMIZED: Adjust variable contributions by the Dimension eigenvalues
      # Using sweep() instead of t(apply()) - much faster
      contrib <- sweep(contrib, 2, eig, "*")
      contrib <- rowSums(contrib) / sum(eig)
    } else {
      # Single axis - convert to vector to ensure column name is 'contrib'
      contrib <- as.vector(contrib)
    }
    res <- cbind(res, contrib = contrib)
  }
  
  # 4.Extract the coordinates x, y and coord partial - MFA
  # OPTIMIZATION: Replaced lapply/do.call with vectorized string operations
  # and apply() with rowSums()
  if("coord.partial" %in% result){
    dd <- data.frame(elmt$coord.partiel[, axes, drop=FALSE], stringsAsFactors = TRUE)
    # OPTIMIZED: Extract group names using vectorized sub() and strsplit()
    rnames <- rownames(dd)
    # Split at the first instance of "." - vectorized approach
    split_pos <- regexpr(".", rnames, fixed = TRUE)
    name_part <- substr(rnames, 1, split_pos - 1)
    group_part <- substr(rnames, split_pos + 1, nchar(rnames))
    groupnames <- data.frame(name = name_part, group.name = group_part, stringsAsFactors = TRUE)
    # OPTIMIZED: Use rowSums instead of apply
    coord.partial <- rowSums(dd^2)
    res.partial <- data.frame(groupnames, dd, coord.partial, stringsAsFactors = TRUE)
  }
  
  # 5. Extract the coordinates x, y and coord partial - HMFA
  # OPTIMIZATION: Pre-allocate vectors and use rowSums instead of apply
  if("coord.node.partial" %in% result){
    # Select hierarchical node
    node <- as.data.frame(elmt[[node.level]], stringsAsFactors = TRUE)
    n_rows <- nrow(node)
    n_groups <- length(group.names)
    n_axes <- length(axes)

    name <- rep(rownames(node), n_groups)

    # OPTIMIZED: Pre-allocate matrix instead of growing with cbind
    dd <- matrix(0, nrow = n_rows * n_groups, ncol = n_axes)
    dim.group <- character(n_rows * n_groups)

    # Fill the matrix
    row_idx <- 1
    for(j in seq_len(n_groups)) {
      for(i in seq_along(axes)) {
        dim.name <- paste0("Dim", axes[i], ".", j)
        dd[row_idx:(row_idx + n_rows - 1), i] <- node[, dim.name]
      }
      dim.group[row_idx:(row_idx + n_rows - 1)] <- group.names[j]
      row_idx <- row_idx + n_rows
    }

    colnames(dd) <- paste0("Dim", axes)
    dd <- as.data.frame(dd, stringsAsFactors = TRUE)

    # OPTIMIZED: Use rowSums instead of apply
    coord.partial <- rowSums(dd^2)
    res.partial <- data.frame(group.name = dim.group, name, dd, coord.partial, stringsAsFactors = TRUE)
  }
  
  if("coord.node.partial" %in% result) 
    res <- res.partial
  else {
    name <- rownames(elmt$coord)
    if(is.null(name)) name <- as.character(1:nrow(elmt$coord))
    name <- as.character(name)
    res <- cbind.data.frame(name = name, res, stringsAsFactors = TRUE)
    rownames(res) <- name
    if(!is.null(select)) res <- .select(res, select)
    if("coord.partial" %in% result){
    res = list(res = res, res.partial = res.partial)
    }
  }
  
  res
}
