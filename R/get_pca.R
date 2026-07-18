#' @include print.factoextra.R utilities.R
NULL
#' Extract the results for individuals/variables - PCA
#' 
#' @description
#' Extract all the results (coordinates, squared cosines, and contributions) for
#' the active individuals/variables from Principal Component Analysis (PCA) outputs.\cr\cr
#' \itemize{
#' \item get_pca(): Extract the results for variables and individuals
#' \item get_pca_ind(): Extract the results for individuals only
#' \item get_pca_var(): Extract the results for variables only
#' }
#' @param res.pca an object of class PCA [FactoMineR]; \code{prcomp} or
#' \code{princomp} [stats]; \code{factoextra_pca}; \code{pca}, \code{dudi},
#' \code{between}, or \code{within} [ade4]; or \code{expoOutput}/\code{epPCA}
#' [ExPosition].
#' @param element the element to subset from the output. Allowed values are 
#' "var" (for active variables) or "ind" (for active individuals).
#' @param ... not used
#' @return a list of matrices containing all the results for the active individuals/variables including: 
#' \item{coord}{coordinates for the individuals/variables}
#' \item{cos2}{cos2 for the individuals/variables}
#' \item{contrib}{contributions of the individuals/variables; contributions to
#' each nonzero-inertia axis sum to 100 percent, while a zero-inertia axis
#' contains zeros}
#' \item{cor}{loading-times-component-standard-deviation coordinates for PCA
#' objects from \code{stats}; these equal variable-component correlations when
#' the input variables were standardized. Returned for variables.}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references \url{https://www.sthda.com/english/}
#' @examples
#' \donttest{
#' # Principal Component Analysis
#' # +++++++++++++++++++++++++++++
#'  data(iris)
#'  res.pca <- prcomp(iris[, -5],  scale = TRUE)
#'  # Extract the results for individuals
#'  ind <- get_pca_ind(res.pca)
#'  print(ind)
#'  head(ind$coord) # coordinates of individuals
#'  head(ind$cos2) # cos2 of individuals
#'  head(ind$contrib) # contributions of individuals
#'  
#'  # Extract the results for variables
#'  var <- get_pca_var(res.pca)
#'  print(var)
#'  head(var$coord) # coordinates of variables
#'  head(var$cos2) # cos2 of variables
#'  head(var$contrib) # contributions of variables
#'  
#'  # You can also use the function get_pca()
#'  get_pca(res.pca, "ind") # Results for individuals
#'  get_pca(res.pca, "var") # Results for variable categories
#'  }
#' @name get_pca
#' 
#' @rdname get_pca
#' @export 
get_pca <- function(res.pca, element = c("var", "ind")){
  elmt <- match.arg(element)
  if(elmt =="var") get_pca_var(res.pca)
  else if(elmt == "ind") get_pca_ind(res.pca)
}

#' @rdname get_pca
#' @export
get_pca_ind<-function(res.pca, ...){
  
  # FactoMineR package
  if(inherits(res.pca, c('PCA'))) ind <- res.pca$ind

  # user-supplied coordinates wrapped by as_factoextra_pca()
  else if(inherits(res.pca, "factoextra_pca")) ind <- res.pca$ind

  # ade4 package. Use the dudi row/column metrics for both ordinary and
  # between-/within-class PCA so nonuniform row weights are respected.
  else if(inherits(res.pca, "pca") && inherits(res.pca, "dudi")){
    ind <- .get_dudi_class_ind_results(res.pca)
  }

  # ade4 between-class / within-class PCA (bca/wca). These dudi objects carry
  # row coordinates ($li), the transformed table ($tab) and row/column weights
  # ($lw/$cw) but not $cent/$norm, so cos2/contrib use the weighted-dudi formula.
  else if(inherits(res.pca, c("between", "within")) && inherits(res.pca, "dudi")){
    ind <- .get_dudi_class_ind_results(res.pca)
  }

  # stats package
  else if(inherits(res.pca, 'princomp')){
    ind.coord <- res.pca$scores
    data <- .prcomp_reconst(res.pca)
    ind <- .get_pca_ind_results(ind.coord, data, res.pca$sdev^2,
                                res.pca$center, res.pca$scale)
    
  }
  else if(inherits(res.pca, 'prcomp')){
    ind.coord <- res.pca$x
    data <- .prcomp_reconst(res.pca)
    ind <- .get_pca_ind_results(ind.coord, data, res.pca$sdev^2,
                                res.pca$center, res.pca$scale)
  }
  # ExPosition package
  else if (inherits(res.pca, "expoOutput") && inherits(res.pca$ExPosition.Data, "epPCA")){
    res <- res.pca$ExPosition.Data
    ind <- list(coord = res$fi,  cos2 = res$ri, contrib = res$ci*100)
  }
  else stop("An object of class : ", paste(class(res.pca), collapse = ", "), 
            " can't be handled by the function get_pca_ind()")
  
  class(ind)<-c("factoextra", "pca_ind")
  
  ind
}


#' @rdname get_pca
#' @export 
get_pca_var<-function(res.pca){
  # FactoMineR package
  if(inherits(res.pca, c('PCA'))) var <- res.pca$var
  # user-supplied coordinates wrapped by as_factoextra_pca()
  else if(inherits(res.pca, "factoextra_pca")){
    if(is.null(res.pca$var))
      stop("This object has no variable coordinates; supply `var.coord` to ",
           "as_factoextra_pca() to use fviz_pca_var()/fviz_pca_biplot().",
           call. = FALSE)
    var <- res.pca$var
  }
  # ade4 package (incl. between-class/within-class PCA: bca/wca). Variable/column
  # coordinates live in $co for all dudi flavours.
  else if(inherits(res.pca, "dudi") &&
          inherits(res.pca, c("pca", "between", "within"))){
    var <- .get_pca_var_results(res.pca$co)
  }
  # stats package
  else if(inherits(res.pca, 'princomp')){
    # Loading-times-component-standard-deviation coordinates. For standardized
    # variables these equal the variable-component correlations.
    # unclass() strips the base R "loadings" S3 class so coord/cos2/contrib are
    # returned as plain numeric matrices. Otherwise print.loadings() hides values
    # with |x| < 0.1 (its cutoff) and the result breaks downstream manipulation.
    var.cor <- sweep(unclass(res.pca$loadings), 2, res.pca$sdev, "*")
    var <- .get_pca_var_results(var.cor)
  }
  else if(inherits(res.pca, 'prcomp')){
    # Loading-times-component-standard-deviation coordinates. For standardized
    # variables these equal the variable-component correlations.
    var.cor <- sweep(res.pca$rotation, 2, res.pca$sdev, "*")
    var <- .get_pca_var_results(var.cor)
  }
  # ExPosition package
  else if (inherits(res.pca, "expoOutput") && inherits(res.pca$ExPosition.Data, "epPCA")){
    res <- res.pca$ExPosition.Data
    data_matrix <- res$X
    factor_scores <- res$fi
    var.coord <- var.cor <- stats::cor(res$X, res$fi) # cor(t(data_matrix), factor_scores)
    var.coord <- replace(var.coord, is.na(var.coord), 0)
    var <- list(coord = var.coord, cor = var.coord, cos2 = res$rj, contrib = res$cj*100)
  }
  else stop("An object of class : ", paste(class(res.pca), collapse = ", "), 
            " can't be handled by the function get_pca_var()")
  class(var)<-c("factoextra", "pca_var")
  var
}




# Helper functions
#++++++++++++++++++++

# compute all the results for individuals : coord, cor, cos2, contrib
# ind.coord : coordinates of variables on the principal component
# pca.center, pca.scale : numeric vectors corresponding to the pca
# center and scale respectively
# data : the original data used during the pca analysis
# eigenvalues : principal component eigenvalues
#
# OPTIMIZATION: Replaced apply() loops with vectorized matrix operations
# - Uses sweep() and rowSums() instead of row-wise apply()
# - Avoids repeated function calls and memory allocations
# - Maintains full econometric precision (identical numerical results)
.get_pca_ind_results <- function(ind.coord, data, eigenvalues, pca.center, pca.scale){

  # Coerce to plain matrices: ade4 dudi objects store $li/$tab as data.frames,
  # and assigning a data.frame into a matrix slice below (ind.cos2[pos, ] <- ...)
  # silently turns the matrix into a list, losing its dimensions. stats prcomp/
  # princomp already pass matrices, so this is a no-op for them.
  ind.coord <- as.matrix(ind.coord)
  data <- as.matrix(data)

  n.ind <- nrow(ind.coord)
  n.dim <- ncol(ind.coord)

  # Handle centering/scaling defaults
  if(isFALSE(pca.center[1])) pca.center <- rep(0, ncol(data))
  if(isFALSE(pca.scale[1])) pca.scale <- rep(1, ncol(data))

  # Compute distances in the PCA metric. The squared-share helper rescales each
  # row before squaring, so valid globally tiny or large inputs do not
  # underflow or overflow merely because of their measurement units.
  data_centered <- sweep(data, 2, pca.center, "-")
  data_scaled <- sweep(data_centered, 2, pca.scale, "/")
  ind.cos2 <- .pca_row_squared_shares(ind.coord, data_scaled)

  # Contributions are shares of a component's score sum of squares. This
  # definition is backend-independent: stats::prcomp() reports eigenvalues with
  # an (n - 1) divisor whereas stats::princomp() uses n, so dividing by the
  # backend eigenvalue made prcomp contributions sum to 100 * (n - 1) / n.
  ind.contrib <- .pca_column_contributions(ind.coord)

  # Set column and row names
  dim.names <- paste0("Dim.", 1:n.dim)
  colnames(ind.coord) <- colnames(ind.cos2) <- colnames(ind.contrib) <- dim.names

  rnames <- rownames(ind.coord)
  if(is.null(rnames)) rnames <- as.character(1:n.ind)
  rownames(ind.coord) <- rownames(ind.cos2) <- rownames(ind.contrib) <- rnames

  # Return results
  list(coord = ind.coord, cos2 = ind.cos2, contrib = ind.contrib)
}

# compute all the results for variables : coord, cor, cos2, contrib
# var.coord : coordinates of variables on the principal component
#
# OPTIMIZATION: Replaced apply() with colSums() and sweep()
# - colSums() is faster than apply(x, 2, sum)
# - sweep() is faster than t(apply()) for column-wise operations
# - Maintains full econometric precision
.get_pca_var_results <- function(var.coord){

  var.coord <- as.matrix(var.coord)

  # Preserve row names from input
  rnames <- rownames(var.coord)

  var.cor <- var.coord # correlation
  var.cos2 <- var.cor^2 # variable qualities

  # OPTIMIZED: variable contributions (in percent)
  # var.cos2*100/total Cos2 of the component
  # Using colSums instead of apply(, 2, sum) - much faster

  var.contrib <- .pca_column_contributions(var.coord)

  # Set column names
  dim.names <- paste0("Dim.", seq_len(ncol(var.coord)))
  colnames(var.coord) <- colnames(var.cor) <- colnames(var.cos2) <-
    colnames(var.contrib) <- dim.names

  # Preserve row names
  if(!is.null(rnames)) {
    rownames(var.coord) <- rownames(var.cor) <- rownames(var.cos2) <-
      rownames(var.contrib) <- rnames
  }

  # Variable coord, cor, cos2 and contrib
  list(coord = var.coord, cor = var.cor, cos2 = var.cos2, contrib = var.contrib)
}

# Individual results for ade4 PCA, including ordinary dudi.pca and
# between-/within-class PCA (bca/wca). These objects expose row coordinates
# ($li), the transformed table ($tab), and row/column weights ($lw/$cw), so use
# the general weighted-dudi definitions:
#   d2[i]        = sum_j cw[j] * tab[i, j]^2      (squared distance, column metric)
#   cos2[i, k]   = li[i, k]^2 / d2[i]
#   contrib[i,k] = 100 * lw[i] * li[i, k]^2 / eig[k]
# For ordinary dudi.pca (uniform lw = 1/n, cw = 1) these reduce exactly to the
# values produced by .get_pca_ind_results(); contrib matches ade4::inertia.dudi.
.get_dudi_class_ind_results <- function(res.pca){
  ind.coord <- as.matrix(res.pca$li)
  tab <- as.matrix(res.pca$tab)
  cw  <- res.pca$cw
  lw  <- res.pca$lw
  n.dim <- ncol(ind.coord)

  ind.cos2 <- .pca_row_squared_shares(ind.coord, tab, column_weights = cw)
  ind.contrib <- .pca_column_contributions(ind.coord, row_weights = lw)

  dim.names <- paste0("Dim.", seq_len(n.dim))
  colnames(ind.coord) <- colnames(ind.cos2) <- colnames(ind.contrib) <- dim.names
  rnames <- rownames(ind.coord)
  if(is.null(rnames)) rnames <- as.character(seq_len(nrow(ind.coord)))
  rownames(ind.coord) <- rownames(ind.cos2) <- rownames(ind.contrib) <- rnames

  list(coord = ind.coord, cos2 = ind.cos2, contrib = ind.contrib)
}

# Stable row-wise squared shares. Each row is normalized before squaring, with
# the same scale applied to the numerator coordinates and reference metric.
.pca_row_squared_shares <- function(coord, reference, column_weights = NULL){
  coord <- as.matrix(coord)
  reference <- as.matrix(reference)
  result <- matrix(0, nrow = nrow(coord), ncol = ncol(coord),
                   dimnames = dimnames(coord))

  finite_rows <- apply(is.finite(coord), 1, all) &
    apply(is.finite(reference), 1, all)
  result[!finite_rows, ] <- NA_real_
  if(!any(finite_rows)) return(result)

  row_scale <- pmax(
    apply(abs(coord[finite_rows, , drop = FALSE]), 1, max),
    apply(abs(reference[finite_rows, , drop = FALSE]), 1, max)
  )
  positive <- row_scale > 0
  if(!any(positive)) return(result)

  finite_indices <- which(finite_rows)
  target_rows <- finite_indices[positive]
  scaled_coord <- sweep(coord[target_rows, , drop = FALSE], 1,
                        row_scale[positive], "/")
  scaled_reference <- sweep(reference[target_rows, , drop = FALSE], 1,
                            row_scale[positive], "/")
  reference_sq <- scaled_reference^2
  if(!is.null(column_weights))
    reference_sq <- sweep(reference_sq, 2, column_weights, "*")
  denominator <- rowSums(reference_sq)
  valid <- is.finite(denominator) & denominator > 0
  if(any(valid)) {
    result[target_rows[valid], ] <- scaled_coord[valid, , drop = FALSE]^2 /
      denominator[valid]
  }
  result
}

# Stable per-axis contributions. Rescaling each coordinate column cancels from
# numerator and denominator while avoiding underflow/overflow in x^2.
.pca_column_contributions <- function(coord, row_weights = NULL){
  coord <- as.matrix(coord)
  result <- matrix(0, nrow = nrow(coord), ncol = ncol(coord),
                   dimnames = dimnames(coord))
  finite_rows <- apply(is.finite(coord), 1, all)
  if(!is.null(row_weights))
    finite_rows <- finite_rows & is.finite(row_weights) & row_weights >= 0
  result[!finite_rows, ] <- NA_real_
  if(!any(finite_rows)) return(result)

  complete_coord <- coord[finite_rows, , drop = FALSE]
  column_scale <- apply(abs(complete_coord), 2, max)
  positive <- column_scale > 0
  if(!any(positive)) return(result)

  scaled_sq <- sweep(complete_coord[, positive, drop = FALSE], 2,
                     column_scale[positive], "/")^2
  if(!is.null(row_weights))
    scaled_sq <- sweep(scaled_sq, 1, row_weights[finite_rows], "*")
  denominator <- colSums(scaled_sq)
  valid <- is.finite(denominator) & denominator > 0
  if(any(valid)) {
    target_columns <- which(positive)[valid]
    result[finite_rows, target_columns] <- sweep(
      scaled_sq[, valid, drop = FALSE], 2, denominator[valid], "/"
    ) * 100
  }
  result
}
