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
#' It lets you apply factoextra's visualizations to the output of an
#' \emph{eigenvalue-based} dimension reduction - for example
#' \code{stats::cmdscale()}, \code{ape::pcoa()}, \code{vegan::rda()}/\code{cca()},
#' a tidymodels \code{recipe}/\code{workflow} with \code{step_pca()} (see the
#' methods below), or a custom analysis - without having to write a dedicated
#' backend. You bring the coordinates; factoextra draws the biplot, scree plot,
#' contributions and cos2.
#'
#' The scree plot, contributions and cos2 assume real eigenvalues, so this
#' constructor is for PCA-family results. Non-linear embeddings (UMAP, t-SNE) have
#' no eigenvalues; plot their coordinates directly (a scree/loadings display would
#' be meaningless for them).
#'
#' @param ind.coord the object to convert. For the default method, individual
#'   (observation) coordinates: a numeric matrix or data frame with one column per
#'   dimension (the "scores"). For the \code{recipe} / \code{workflow} methods, a
#'   prepped \code{recipe} or a fitted \code{workflow} whose PCA is done with
#'   \code{recipes::step_pca()} (the scores, loadings and eigenvalues are then
#'   extracted for you). Required.
#' @param ... passed to methods (unused by the default method).
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
#' @seealso For UMAP / t-SNE embeddings, which have no eigenvalues, use
#'   \code{\link{fviz_umap}} / \code{\link{fviz_tsne}} instead.
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
#'   var.coord = sweep(pca$rotation, 2, pca$sdev, "*"),
#'   eig       = pca$sdev^2,
#'   scale.unit = TRUE
#' )
#' fviz_pca_biplot(obj2, label = "var", col.ind = "steelblue")
#'
#' # 3. A tidymodels recipe PCA (step_pca) -> factoextra biplot + honest scree
#' if (requireNamespace("recipes", quietly = TRUE)) {
#'   library(recipes)
#'   rec <- recipe(~ ., data = iris[, 1:4]) |>
#'     step_normalize(all_numeric_predictors()) |>
#'     step_pca(all_numeric_predictors(), num_comp = 4)
#'   obj3 <- as_factoextra_pca(prep(rec))
#'   fviz_pca_biplot(obj3, label = "var")
#'   fviz_eig(obj3, addlabels = TRUE)
#' }
#'
#' @rdname as_factoextra
#' @export
as_factoextra_pca <- function(ind.coord, ...) UseMethod("as_factoextra_pca")

#' @rdname as_factoextra
#' @export
as_factoextra_pca.default <- function(ind.coord, var.coord = NULL, eig = NULL,
                              ind.cos2 = NULL, ind.contrib = NULL,
                              var.cos2 = NULL, var.contrib = NULL, var.cor = NULL,
                              scale.unit = FALSE, ...){

  if(missing(ind.coord) || is.null(ind.coord))
    stop("`ind.coord` is required.", call. = FALSE)

  if(!is.logical(scale.unit) || length(scale.unit) != 1L || is.na(scale.unit))
    stop("`scale.unit` must be TRUE or FALSE.", call. = FALSE)

  ind.coord <- .fe_as_dim_matrix(ind.coord, "ind.coord")
  k <- ncol(ind.coord)

  # Eigenvalues: default to the variance of each coordinate column.
  if(is.null(eig)) {
    if(nrow(ind.coord) < 2L)
      stop("At least two observations are required to infer `eig` from `ind.coord`.",
           call. = FALSE)
    eig <- apply(ind.coord, 2, stats::var)
  }
  if(!is.numeric(eig) || length(eig) < k)
    stop("`eig` must be a numeric vector with at least ", k,
         " value(s) (one per dimension).", call. = FALSE)
  eig <- as.numeric(eig)
  if(any(!is.finite(eig)))
    stop("`eig` must contain finite values only.", call. = FALSE)
  eig.tol <- sqrt(.Machine$double.eps) * max(abs(eig))
  if(any(eig < -eig.tol))
    stop("`eig` must contain non-negative eigenvalues.", call. = FALSE)
  eig[eig < 0] <- 0
  if(!any(eig > 0))
    stop("`eig` must contain at least one positive eigenvalue.", call. = FALSE)

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
         scale.unit = scale.unit),
    class = c("factoextra_pca", "list")
  )
}

#' @details
#' \strong{tidymodels (recipe / workflow).} The \code{recipe} and \code{workflow}
#' methods extract a PCA fitted with \code{recipes::step_pca()} through the public
#' recipes/workflows API: the scores from the baked training data (or, for a fitted
#' workflow, \code{workflows::extract_mold()}), the loadings from
#' \code{tidy(step, type = "coef")}, and the \emph{full} set of eigenvalues from
#' \code{tidy(step, type = "variance")} (so the scree plot and axis percentages are
#' honest even when \code{num_comp} keeps only a few components). Variable
#' coordinates are loading times the square root of the eigenvalue. Exact
#' variable-component correlations and cos2 are recovered from the full PCA
#' inertia when every PCA input is provably centered. Uncentered recipe PCA is
#' rejected because Pearson correlations cannot be recovered from its fitted
#' \code{step_pca()} object. \code{scale.unit} is set to \code{TRUE} only when every
#' PCA input is both centered and unit-scaled at the PCA boundary; partial scaling
#' therefore does not draw a correlation circle. For fully normalized data,
#' variable coordinates, correlations, cos2 and contributions, and the eigenvalue
#' percentages match a full \code{FactoMineR::PCA()} regardless of how many
#' components \code{step_pca()} keeps; the \emph{individual} cos2 is computed over
#' the retained components (it equals the full-space cos2 only when all components
#' are kept). The two-dimensional plots (\code{fviz_pca_ind()},
#' \code{fviz_pca_var()}, \code{fviz_pca_biplot()}) need \code{num_comp >= 2}. The
#' recipe must be prepped and its PCA step must be \code{step_pca()} (non-linear
#' embedding steps such as \code{step_umap()} have no eigenvalues and are rejected
#' with a message). For the loadings display of a recipe PCA see also
#' \code{learntidymodels::plot_top_loadings()}.
#'
#' @rdname as_factoextra
#' @export
as_factoextra_pca.recipe <- function(ind.coord, ...){
  .fe_need("recipes")
  ex <- .fe_extract_recipe_pca(ind.coord, scores = NULL)
  as_factoextra_pca.default(ind.coord = ex$scores, var.coord = ex$var.coord,
                            var.cos2 = ex$var.cos2, var.cor = ex$var.cor,
                            eig = ex$eig,
                            scale.unit = ex$scale.unit)
}

#' @rdname as_factoextra
#' @export
as_factoextra_pca.workflow <- function(ind.coord, ...){
  .fe_need("workflows"); .fe_need("recipes")
  wf <- ind.coord
  # A workflow may use a formula/variables preprocessor rather than a recipe;
  # check that first so the error names the real cause (not "fit it first").
  pp <- tryCatch(workflows::extract_preprocessor(wf), error = function(e) NULL)
  if(!is.null(pp) && !inherits(pp, "recipe"))
    stop("This workflow uses a ", class(pp)[1], " preprocessor. as_factoextra_pca() ",
         "needs a workflow whose recipe contains step_pca().", call. = FALSE)
  rec <- tryCatch(workflows::extract_recipe(wf),
                  error = function(e)
                    stop("`as_factoextra_pca()` needs a fitted workflow. Fit it ",
                         "first with fit(). (", conditionMessage(e), ")",
                         call. = FALSE))
  # Workflows do not retain the recipe's training set, so bake(new_data = NULL)
  # fails; take the scores from the fitted mold instead.
  mold <- workflows::extract_mold(wf)
  ex <- .fe_extract_recipe_pca(rec, scores = as.matrix(mold$predictors))
  as_factoextra_pca.default(ind.coord = ex$scores, var.coord = ex$var.coord,
                            var.cos2 = ex$var.cos2, var.cor = ex$var.cor,
                            eig = ex$eig,
                            scale.unit = ex$scale.unit)
}

# ---- internal helpers -------------------------------------------------------

# Soft-dependency guard for the tidymodels methods.
.fe_need <- function(pkg){
  if(!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' is required for this method. Install it with ",
         "install.packages('", pkg, "').", call. = FALSE)
}

# Extract scores / loadings / eigenvalues from a prepped recipe whose dimension
# reduction is step_pca(). `scores` may be supplied (fitted-workflow path, from the
# mold) to avoid bake(new_data = NULL), which fails when the training set was not
# retained. Returns a list(scores, var.coord, var.cor, var.cos2, eig, scale.unit).
.fe_extract_recipe_pca <- function(rec, scores = NULL){
  if(!isTRUE(recipes::fully_trained(rec)))
    stop("The recipe is not prepped. Call prep() on it first.", call. = FALSE)

  is_pca <- vapply(rec$steps, inherits, logical(1), "step_pca")
  if(!any(is_pca)){
    dr <- c("step_umap", "step_ica", "step_pls", "step_kpca", "step_nnmf")
    hit <- dr[vapply(dr, function(s) any(vapply(rec$steps, inherits, logical(1), s)), logical(1))]
    if(length(hit))
      stop("This recipe's dimension-reduction step is ", hit[1], "(), not ",
           "step_pca(). ", if(hit[1] == "step_umap")
             "UMAP is a non-linear embedding with no eigenvalues, so a scree / "
           else "That step has no eigenvalues, so a scree / ",
           "loadings display would be meaningless. ",
           if(hit[1] == "step_umap") "Plot the baked coordinates with fviz_umap()."
           else "Plot its baked coordinates directly instead.", call. = FALSE)
    stop("No step_pca() found in this recipe.", call. = FALSE)
  }
  if(sum(is_pca) > 1L){
    ids <- vapply(rec$steps[is_pca], function(s) s$id, character(1))
    stop("This recipe has more than one step_pca() (", paste(ids, collapse = ", "),
         "). as_factoextra_pca() supports a single PCA step.", call. = FALSE)
  }
  st  <- rec$steps[[which(is_pca)]]
  k   <- st$num_comp
  # The baked/mold SCORE columns carry the step's prefix and are zero-padded to the
  # digit width of the component count (PC01.. at >=10 comps). Reuse recipes' own
  # name generator so we match those columns exactly for any prefix and any k.
  score_comp <- recipes::names0(k, st$prefix)

  # Loadings (rotation) via the tidier. Its `component` labels are fixed (PC1..PCp)
  # and independent of the step's `prefix`, so keep them separate from the score
  # column names; order by the trailing index and take the first k retained ones.
  load_long <- recipes::tidy(rec, id = st$id, type = "coef")
  all_comp  <- unique(load_long$component)
  all_comp  <- all_comp[order(suppressWarnings(as.integer(gsub("\\D", "", all_comp))))]
  if(k < 1L || k > length(all_comp))
    stop("The fitted step_pca() retained an invalid number of components.",
         call. = FALSE)
  load_comp <- all_comp[seq_len(k)]
  terms <- unique(load_long$terms)
  full.loadings <- matrix(NA_real_, nrow = length(terms), ncol = length(all_comp),
                          dimnames = list(terms, all_comp))
  for(j in seq_along(all_comp)){
    cj <- load_long[load_long$component == all_comp[j], ]
    full.loadings[cj$terms, j] <- cj$value
  }
  loadings <- full.loadings[, seq_len(k), drop = FALSE]
  colnames(loadings) <- score_comp

  # Eigenvalues: the FULL set (all components) for an honest scree / percentages.
  var_long <- recipes::tidy(rec, id = st$id, type = "variance")
  vv  <- var_long[var_long$terms == "variance", , drop = FALSE]
  eig <- vv$value[order(vv$component)]
  if(length(eig) < ncol(full.loadings))
    stop("Could not recover the full eigenvalue set from the fitted step_pca().",
         call. = FALSE)

  # Variable coordinates use the ordinary PCA loading * component-SD definition.
  var.coord <- sweep(loadings, 2, sqrt(eig[seq_len(k)]), "*")

  # Scores: from the mold (workflow) or the baked training data (recipe).
  if(is.null(scores)){
    baked <- recipes::bake(rec, new_data = NULL)
    miss <- setdiff(score_comp, names(baked))
    if(length(miss))
      stop("Could not find the PCA score columns (", paste(miss, collapse = ", "),
           ") in the baked data. Was the recipe prepped with retain = TRUE?",
           call. = FALSE)
    scores <- as.matrix(baked[, score_comp, drop = FALSE])
  } else {
    scores <- scores[, score_comp, drop = FALSE]
  }

  # Correlations require centered PCA inputs. Preprocessing state is tracked in
  # recipe order and unknown value-changing steps reset the proof; an internal
  # prcomp center/scale option, when present, is applied at the PCA boundary.
  pca_pos <- which(is_pca)
  prep.state <- .fe_recipe_pca_preprocessing(rec, pca_pos, st)
  if(!prep.state$centered)
    stop("The fitted step_pca() used uncentered inputs. Exact variable-component ",
         "correlations cannot be recovered; add step_center()/step_normalize() ",
         "or set step_pca(options = list(center = TRUE)).", call. = FALSE)

  full.var.coord <- sweep(full.loadings, 2,
                          sqrt(eig[seq_len(ncol(full.loadings))]), "*")
  var.inertia <- rowSums(full.var.coord^2)
  if(any(!is.finite(var.inertia)) || any(var.inertia <= 0))
    stop("Variable correlations are undefined for a zero-inertia PCA input.",
         call. = FALSE)
  var.cor <- sweep(var.coord, 1, sqrt(var.inertia), "/")
  var.cos2 <- var.cor^2

  unit.tol <- sqrt(.Machine$double.eps)
  unit.inertia <- all(abs(var.inertia - 1) <= unit.tol * pmax(1, var.inertia))
  scale.unit <- prep.state$scaled && unit.inertia

  list(scores = scores, var.coord = var.coord, var.cor = var.cor,
       var.cos2 = var.cos2, eig = eig, scale.unit = scale.unit)
}

# Prove whether every fitted step_pca() input is centered and unit-scaled at the
# PCA boundary. Unknown intervening transformations fail closed; a later known
# center/scale/normalize step can re-establish the relevant property.
.fe_recipe_pca_preprocessing <- function(rec, pca_pos, pca_step){
  columns <- unname(pca_step$columns)
  centered <- stats::setNames(rep(FALSE, length(columns)), columns)
  scaled <- stats::setNames(rep(FALSE, length(columns)), columns)

  preceding <- rec$steps[seq_len(pca_pos - 1L)]
  for(step in preceding){
    if(inherits(step, "step_normalize")){
      hit <- intersect(columns, intersect(names(step$means), names(step$sds)))
      centered[hit] <- TRUE
      scaled[hit] <- TRUE
    }
    else if(inherits(step, "step_center")){
      hit <- intersect(columns, names(step$means))
      centered[hit] <- TRUE
    }
    else if(inherits(step, "step_scale")){
      hit <- intersect(columns, names(step$sds))
      is.unit <- is.numeric(step$factor) && length(step$factor) == 1L &&
        is.finite(step$factor) && abs(step$factor - 1) <= sqrt(.Machine$double.eps)
      scaled[hit] <- is.unit
    }
    else if(inherits(step, c("step_rm", "step_select", "step_zv", "step_nzv",
                             "step_corr", "step_lincomb"))){
      # These fitted steps only remove columns. Values of the surviving PCA
      # inputs are unchanged, so an earlier centering/scaling proof remains valid.
    }
    else {
      centered[] <- FALSE
      scaled[] <- FALSE
    }
  }

  # Only a literal center = TRUE proves centering at the training means. prcomp()
  # also accepts arbitrary numeric centers, which must not be treated as means.
  if(isTRUE(pca_step$options$center)) centered[] <- TRUE

  internal.scale <- pca_step$res$scale
  if(is.numeric(internal.scale) && length(internal.scale) == length(columns) &&
     all(is.finite(internal.scale)) && all(internal.scale > 0)) scaled[] <- TRUE

  list(centered = length(columns) > 0L && all(centered),
       scaled = length(columns) > 0L && all(scaled))
}

# Coerce coordinates to a numeric matrix with Dim.1..k colnames and row names.
.fe_as_dim_matrix <- function(x, argname){
  x <- as.matrix(x)
  if(!is.numeric(x))
    stop("`", argname, "` must be numeric.", call. = FALSE)
  if(ncol(x) < 1)
    stop("`", argname, "` must have at least one column (dimension).", call. = FALSE)
  if(any(!is.finite(x)))
    stop("`", argname, "` must contain finite values only.", call. = FALSE)
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
  if(any(!is.finite(x)))
    stop("`", argname, "` must contain finite values only.", call. = FALSE)
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
