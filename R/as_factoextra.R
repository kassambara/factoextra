#' @include utilities.R
NULL

#' Build a factoextra-ready object from pre-computed coordinates
#'
#' @description
#' \code{as_factoextra_pca()} wraps pre-computed individual (and, optionally,
#' variable) coordinates into an object that the \code{\link{fviz_pca}} family
#' (\code{fviz_pca_ind()}, \code{fviz_pca_var()}, \code{fviz_pca_biplot()}),
#' \code{\link{fviz_eig}}, \code{\link{fviz_contrib}} and \code{\link{fviz_cos2}}
#' can plot directly.
#'
#' It lets you apply factoextra's visualizations to the output of \emph{any}
#' dimension-reduction method - for example \code{stats::cmdscale()},
#' \code{ape::pcoa()}, UMAP/t-SNE embeddings, \code{vegan::rda()}/\code{cca()},
#' or a custom analysis - without having to write a dedicated backend. You bring
#' the coordinates; factoextra draws the biplot, scree plot, contributions and
#' cos2.
#'
#' @param ind.coord individual (observation) coordinates: a numeric matrix or
#'   data frame with one column per dimension (the "scores"). Required.
#' @param var.coord optional variable coordinates / loadings: a numeric matrix or
#'   data frame with one column per dimension. Supplying it enables
#'   \code{fviz_pca_var()} and \code{fviz_pca_biplot()}.
#' @param eig optional numeric vector of eigenvalues (length \eqn{\ge} number of
#'   dimensions). Used by the scree plot and to label the axes with the percentage
#'   of explained variance. When \code{NULL} (default) it is set to the variance
#'   of each coordinate column (the natural definition of a PCA eigenvalue).
#' @param ind.cos2,ind.contrib,var.cos2,var.contrib,var.cor optional pre-computed
#'   quality (\code{cos2}), contribution and (variable) correlation matrices, with
#'   the same dimensions as the corresponding coordinates. When omitted they are
#'   derived from the coordinates (see \strong{Details}).
#' @param scale.unit logical. If \code{TRUE}, the variable coordinates are treated
#'   as correlations and the correlation circle is drawn by \code{fviz_pca_var()}.
#'   Default is \code{FALSE}.
#'
#' @details
#' When \code{cos2}/\code{contrib} are not supplied they are computed from the
#' coordinates:
#' \itemize{
#'   \item \code{contrib = 100 * coord^2 / colSums(coord^2)} - the exact
#'     contribution of each element to each dimension.
#'   \item \code{cos2 = coord^2 / rowSums(coord^2)} - the quality of
#'     representation \emph{within the supplied dimensions}. This equals the true
#'     cos2 only when all components are provided; with a truncated set of
#'     dimensions it is the quality restricted to that sub-space. Pass
#'     \code{ind.cos2}/\code{var.cos2} explicitly if you have the exact values.
#' }
#'
#' @return An object of class \code{c("factoextra_pca", "list")} holding the
#'   standardized \code{ind} (and \code{var}) results and eigenvalues, ready for
#'   the \code{fviz_pca_*()} functions.
#'
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'
#' @examples
#' # 1. Bring your own coordinates: classical MDS (cmdscale) -> factoextra
#' d <- dist(scale(mtcars))
#' mds <- cmdscale(d, k = 3)
#' obj <- as_factoextra_pca(ind.coord = mds)
#' fviz_pca_ind(obj, repel = TRUE)
#' fviz_eig(obj)
#'
#' # 2. Round-trip a prcomp result through the constructor (biplot)
#' pca <- prcomp(iris[, -5], scale. = TRUE)
#' obj2 <- as_factoextra_pca(
#'   ind.coord = pca$x,
#'   var.coord = pca$rotation,
#'   eig       = pca$sdev^2
#' )
#' fviz_pca_biplot(obj2, label = "var", col.ind = "steelblue")
#'
#' @rdname as_factoextra
#' @export
as_factoextra_pca <- function(ind.coord, var.coord = NULL, eig = NULL,
                              ind.cos2 = NULL, ind.contrib = NULL,
                              var.cos2 = NULL, var.contrib = NULL, var.cor = NULL,
                              scale.unit = FALSE){

  if(missing(ind.coord) || is.null(ind.coord))
    stop("`ind.coord` is required.", call. = FALSE)

  ind.coord <- .fe_as_dim_matrix(ind.coord, "ind.coord")
  k <- ncol(ind.coord)

  # Eigenvalues: default to the variance of each coordinate column.
  if(is.null(eig)) eig <- apply(ind.coord, 2, stats::var)
  if(!is.numeric(eig) || length(eig) < k)
    stop("`eig` must be a numeric vector with at least ", k,
         " value(s) (one per dimension).", call. = FALSE)

  ind <- list(
    coord   = ind.coord,
    cos2    = if(is.null(ind.cos2))    .fe_cos2(ind.coord)    else .fe_check_like(ind.cos2,    ind.coord, "ind.cos2"),
    contrib = if(is.null(ind.contrib)) .fe_contrib(ind.coord) else .fe_check_like(ind.contrib, ind.coord, "ind.contrib")
  )

  var <- NULL
  if(!is.null(var.coord)){
    var.coord <- .fe_as_dim_matrix(var.coord, "var.coord")
    if(ncol(var.coord) != k)
      stop("`var.coord` must have the same number of dimensions as `ind.coord` (",
           k, ").", call. = FALSE)
    var <- list(
      coord   = var.coord,
      cor     = if(is.null(var.cor))     var.coord               else .fe_check_like(var.cor,     var.coord, "var.cor"),
      cos2    = if(is.null(var.cos2))    .fe_cos2(var.coord)     else .fe_check_like(var.cos2,    var.coord, "var.cos2"),
      contrib = if(is.null(var.contrib)) .fe_contrib(var.coord) else .fe_check_like(var.contrib, var.coord, "var.contrib")
    )
  }

  structure(
    list(ind = ind, var = var, eig.values = as.numeric(eig),
         scale.unit = isTRUE(scale.unit)),
    class = c("factoextra_pca", "list")
  )
}

# ---- internal helpers -------------------------------------------------------

# Coerce coordinates to a numeric matrix with Dim.1..k colnames and row names.
.fe_as_dim_matrix <- function(x, argname){
  x <- as.matrix(x)
  if(!is.numeric(x))
    stop("`", argname, "` must be numeric.", call. = FALSE)
  if(ncol(x) < 1)
    stop("`", argname, "` must have at least one column (dimension).", call. = FALSE)
  colnames(x) <- paste0("Dim.", seq_len(ncol(x)))
  if(is.null(rownames(x))) rownames(x) <- as.character(seq_len(nrow(x)))
  x
}

# Validate that a user-supplied cos2/contrib/cor matrix matches the coordinates.
.fe_check_like <- function(x, coord, argname){
  x <- as.matrix(x)
  if(!is.numeric(x))
    stop("`", argname, "` must be numeric.", call. = FALSE)
  if(!all(dim(x) == dim(coord)))
    stop("`", argname, "` must have the same dimensions as its coordinates (",
         nrow(coord), " x ", ncol(coord), ").", call. = FALSE)
  colnames(x) <- colnames(coord)
  rownames(x) <- rownames(coord)
  x
}

# Per-dimension contribution (in percent): exact from coordinates.
.fe_contrib <- function(coord){
  ss <- colSums(coord^2)
  ss[ss == 0] <- 1   # avoid 0/0 for an all-zero dimension
  contrib <- sweep(coord^2, 2, ss, "/") * 100
  dimnames(contrib) <- dimnames(coord)
  contrib
}

# Per-row quality of representation within the supplied dimensions.
.fe_cos2 <- function(coord){
  d2 <- rowSums(coord^2)
  d2[d2 == 0] <- 1
  cos2 <- sweep(coord^2, 1, d2, "/")
  dimnames(cos2) <- dimnames(coord)
  cos2
}
