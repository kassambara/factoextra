#' @include utilities.R fviz_cluster.R
NULL

#' Visualize a UMAP or t-SNE embedding
#'
#' @description
#' \code{fviz_umap()} and \code{fviz_tsne()} draw a ggplot2 scatter plot of a
#' two-dimensional \strong{embedding} produced by UMAP or t-SNE, with
#' factoextra's grouping, ellipse and palette styling. They accept the result of
#' \code{uwot::umap()} / \code{umap::umap()} / \code{Rtsne::Rtsne()}, or a plain
#' matrix / data frame of coordinates.
#'
#' Unlike PCA, an embedding has \strong{no eigenvalues}: the axes explain no fixed
#' percentage of variance, so they are labelled \code{UMAP1}/\code{UMAP2}
#' (\code{tSNE1}/\code{tSNE2}) without a percentage, and there is deliberately no
#' scree plot, no variable loadings, and no correlation circle - those are
#' meaningless for an embedding. For methods that \emph{do} have eigenvalues (PCA,
#' CA, MCA, MDS) see \code{\link{fviz_pca}} and \code{\link{as_factoextra_pca}}.
#'
#' @param X an embedding: a \code{uwot::umap()} result (a matrix, or a list with
#'   \code{$embedding}), an \code{Rtsne::Rtsne()} result (\code{$Y}), a
#'   \code{umap::umap()} result (\code{$layout}), or a numeric matrix / data frame
#'   of coordinates (one column per dimension).
#' @param dims two distinct positive integer indices specifying which embedding
#'   dimensions to plot.
#' @param habillage an optional factor / vector used to colour the points by group
#'   (discrete). Its length must equal the number of embedded observations.
#' @param col.ind point colour: a factor / character vector (discrete groups) or a
#'   \strong{numeric} vector to colour by a continuous feature value (as in a
#'   Seurat feature plot). A single colour name is also allowed. The eigenvalue
#'   metrics accepted by \code{\link{fviz_pca_ind}} (\code{"cos2"}, \code{"contrib"},
#'   ...) are rejected here - an embedding has no such quantities.
#' @param geom character: \code{"point"} and/or \code{"text"}.
#' @param label logical; if \code{TRUE}, label the points by row name.
#' @param pointsize,labelsize point and label size.
#' @param addEllipses logical; if \code{TRUE}, draw an outline around each group.
#'   Default \code{FALSE}. When \code{TRUE} the default \code{ellipse.type} is
#'   \code{"convex"} (a hull) rather than a normal/\code{t} confidence ellipse: a
#'   confidence ellipse assumes a metric that UMAP/t-SNE do not preserve.
#' @param ellipse.type,ellipse.level ellipse type and confidence level. See
#'   \code{\link{fviz_cluster}}.
#' @param mean.point logical; if \code{TRUE}, show group mean points. Default
#'   \code{FALSE} - a centroid in embedding space is not a feature-space mean.
#' @param palette,gradient.cols discrete group palette / continuous colour
#'   gradient, passed to \code{\link[ggpubr]{ggpar}}.
#' @param density logical; if \code{TRUE}, overlay 2D density contour lines (a
#'   visual aid, not a probability readout).
#' @param facet.by optional variable (length = number of observations) to facet by.
#' @param legend.title colour legend title. Defaults to \code{"Groups"} for a
#'   discrete colouring and \code{"value"} for a continuous feature.
#' @param repel logical; if \code{TRUE}, use ggrepel for labels.
#' @param max.points,sample.seed passed to the downsampling used for large
#'   embeddings; see \code{\link{fviz_pca_ind}}.
#' @param caption optional plot caption (e.g. an interpretation reminder). Off by
#'   default.
#' @param ggtheme,... passed to \code{\link[ggpubr]{ggscatter}} / ggplot2.
#'
#' @return a ggplot2 object.
#'
#' @details UMAP and t-SNE produce a low-dimensional embedding, not a variance
#' decomposition. Local relationships are often more informative than global
#' geometry, but both remain method- and parameter-dependent. In particular, the
#' \strong{size} of a cluster, the \strong{distance} between well-separated
#' clusters, and the amount of empty space between them are not quantitatively
#' meaningful and change with \code{perplexity} or \code{n_neighbors}
#' (Wattenberg et al., 2016). Similar embedding scatter plots are drawn by \code{Seurat::DimPlot()}
#' / \code{FeaturePlot()} and \code{ggpca}.
#'
#' @references
#' McInnes, L., Healy, J. and Melville, J. (2018). UMAP: Uniform Manifold
#' Approximation and Projection for Dimension Reduction. \emph{arXiv:1802.03426}.
#'
#' Wattenberg, M., \enc{Viégas}{Viegas}, F. and Johnson, I. (2016). How to Use
#' t-SNE Effectively. \emph{Distill}. \doi{10.23915/distill.00002}.
#'
#' @seealso \code{\link{fviz_pca}}, \code{\link{as_factoextra_pca}},
#'   \code{\link{fviz_cluster}}.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("uwot", quietly = TRUE)) {
#'   set.seed(123)
#'   um <- uwot::umap(iris[, 1:4], n_neighbors = 15)
#'   fviz_umap(um, habillage = iris$Species, addEllipses = TRUE)
#'   # colour by a continuous feature value
#'   fviz_umap(um, col.ind = iris$Petal.Length)
#' }
#' }
#' @rdname fviz_umap
#' @export
fviz_umap <- function(X, dims = c(1L, 2L), habillage = NULL, col.ind = NULL,
                      geom = "point", label = FALSE, pointsize = 1.5, labelsize = 4,
                      addEllipses = FALSE, ellipse.type = "convex", ellipse.level = 0.95,
                      mean.point = FALSE, palette = NULL, gradient.cols = NULL,
                      density = FALSE, facet.by = NULL, repel = TRUE, max.points = NULL,
                      sample.seed = 123, legend.title = NULL, caption = NULL,
                      ggtheme = theme_minimal(), ...) {
  .fviz_embedding(X, method = "UMAP", dims = dims, habillage = habillage,
                  col.ind = col.ind, geom = geom, label = label, pointsize = pointsize,
                  labelsize = labelsize, addEllipses = addEllipses,
                  ellipse.type = ellipse.type, ellipse.level = ellipse.level,
                  mean.point = mean.point, palette = palette, gradient.cols = gradient.cols,
                  density = density, facet.by = facet.by, repel = repel,
                  max.points = max.points, sample.seed = sample.seed,
                  legend.title = legend.title, caption = caption, ggtheme = ggtheme, ...)
}

#' @rdname fviz_umap
#' @export
fviz_tsne <- function(X, dims = c(1L, 2L), habillage = NULL, col.ind = NULL,
                      geom = "point", label = FALSE, pointsize = 1.5, labelsize = 4,
                      addEllipses = FALSE, ellipse.type = "convex", ellipse.level = 0.95,
                      mean.point = FALSE, palette = NULL, gradient.cols = NULL,
                      density = FALSE, facet.by = NULL, repel = TRUE, max.points = NULL,
                      sample.seed = 123, legend.title = NULL, caption = NULL,
                      ggtheme = theme_minimal(), ...) {
  .fviz_embedding(X, method = "tSNE", dims = dims, habillage = habillage,
                  col.ind = col.ind, geom = geom, label = label, pointsize = pointsize,
                  labelsize = labelsize, addEllipses = addEllipses,
                  ellipse.type = ellipse.type, ellipse.level = ellipse.level,
                  mean.point = mean.point, palette = palette, gradient.cols = gradient.cols,
                  density = density, facet.by = facet.by, repel = repel,
                  max.points = max.points, sample.seed = sample.seed,
                  legend.title = legend.title, caption = caption, ggtheme = ggtheme, ...)
}


# Core embedding plotter shared by fviz_umap()/fviz_tsne(). Builds the scatter
# with ggpubr::ggscatter directly (like fviz_cluster) - NOT fviz()/facto_summarize,
# which assume an eigenvalue-based result.
.fviz_embedding <- function(X, method = "UMAP", dims = c(1L, 2L),
                            habillage = NULL, col.ind = NULL,
                            geom = "point", label = FALSE,
                            pointsize = 1.5, labelsize = 4,
                            addEllipses = FALSE, ellipse.type = "convex",
                            ellipse.level = 0.95, mean.point = FALSE,
                            palette = NULL, gradient.cols = NULL,
                            density = FALSE, facet.by = NULL, repel = TRUE,
                            max.points = NULL, sample.seed = 123,
                            legend.title = NULL,
                            caption = NULL, ggtheme = theme_minimal(), ...) {

  coord <- .fe_layout(X, dims)
  n <- nrow(coord)
  axes <- paste0(method, dims)

  plot.data <- data.frame(name = rownames(coord), x = coord[, 1], y = coord[, 2],
                          stringsAsFactors = FALSE)

  # Resolve the colour source (discrete group vs continuous feature), rejecting
  # the eigenvalue-only metavalues that make no sense for an embedding.
  color <- "black"; is_grp <- FALSE
  vals <- if(!is.null(habillage)) habillage else col.ind
  if(!is.null(vals)) {
    if(is.character(vals) && length(vals) == 1L &&
       vals %in% c("cos2", "contrib", "coord", "x", "y"))
      stop("`col.ind = \"", vals, "\"` is an eigenvalue-based quantity; a UMAP/",
           "t-SNE embedding has none. Colour by a numeric feature vector or a ",
           "factor instead.", call. = FALSE)
    if(length(vals) == 1L) {                       # a single fixed colour name
      color <- vals
    } else {
      if(length(vals) != n)
        stop("The colour/grouping vector has ", length(vals), " values but the ",
             "embedding has ", n, " observations. Align it to the embedded rows ",
             "(e.g. if you removed duplicate rows before embedding, subset it the ",
             "same way).", call. = FALSE)
      is_grp <- .is_grouping_var(vals)
      plot.data$.colour <- if(is_grp) as.factor(vals) else vals
      color <- ".colour"
    }
  }
  if(!is.null(facet.by)) {
    if(length(facet.by) != n)
      stop("`facet.by` has ", length(facet.by), " values but the embedding has ",
           n, " observations.", call. = FALSE)
    plot.data$.facet <- facet.by
  }

  # Downsample large embeddings: stratify only on a DISCRETE colour (never a
  # continuous feature); thin points/labels and keep any group vector in lockstep.
  sampled <- NULL
  if(!is.null(max.points)) {
    max.points <- .coerce_integerish(max.points, "max.points", lower = 1L)
    if(n > max.points) {
      grps <- if(is_grp) plot.data$.colour else NULL
      sampled <- .sample_indices(n, max.points, groups = grps, seed = sample.seed)
      message("Showing a random ", length(sampled), " of ", n,
              " points (max.points); set max.points = NULL to show all.")
    }
  }

  lab <- if(isTRUE(label) || "text" %in% geom) "name" else NULL
  ellipse.type <- if(addEllipses) ellipse.type else "convex"

  ggscatter_args <- c(
    list(data = plot.data, x = "x", y = "y", color = color,
         size = pointsize, point = "point" %in% geom, label = lab,
         font.label = labelsize * 3, repel = repel, mean.point = mean.point,
         ellipse = addEllipses, ellipse.type = ellipse.type,
         ellipse.level = ellipse.level,
         xlab = axes[1], ylab = axes[2], ggtheme = ggtheme,
         palette = palette),
    list(...))
  p <- do.call(ggpubr::ggscatter, ggscatter_args)

  # Thin only the drawn scatter/labels to the sampled subset; any ellipse/hull is
  # left on the full data (its input is not shrunk by the draw).
  if(!is.null(sampled)) {
    sub <- plot.data[sampled, , drop = FALSE]
    for(i in seq_along(p$layers)) {
      g <- p$layers[[i]]$geom; s <- p$layers[[i]]$stat
      if((inherits(g, "GeomPoint") && inherits(s, "StatIdentity")) ||
         inherits(g, c("GeomText", "GeomLabel", "GeomTextRepel", "GeomLabelRepel")))
        p$layers[[i]]$data <- sub
    }
  }

  # A tidy legend title (not the internal ".colour" column name). Only label the
  # `fill` guide when an ellipse actually maps fill, else ggplot2 emits an
  # "Ignoring unknown labels: fill" message on the (common) no-ellipse plot.
  if(color == ".colour") {
    ttl <- if(!is.null(legend.title)) legend.title else if(is_grp) "Groups" else "value"
    p <- p + ggplot2::labs(color = ttl)
    if(isTRUE(addEllipses)) p <- p + ggplot2::labs(fill = ttl)
  }
  if(!is.null(gradient.cols) && !is_grp && color == ".colour")
    p <- p + ggpubr::gradient_color(gradient.cols)
  if(isTRUE(density))
    p <- p + ggplot2::stat_density_2d(
      data = plot.data, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      color = "grey40", linewidth = 0.2, inherit.aes = FALSE)
  if(!is.null(facet.by))
    p <- p + ggplot2::facet_wrap(~ .facet)
  if(!is.null(caption))
    p <- p + ggplot2::labs(caption = caption)
  p
}


# Extract an n x 2 coordinate matrix from an embedding object. Matrix/data.frame
# are handled FIRST so `$` is never applied to an atomic matrix.
.fe_layout <- function(x, dims = c(1L, 2L)) {
  if(length(dims) != 2L || !is.numeric(dims) || anyNA(dims) ||
     any(!is.finite(dims)) || any(dims > .Machine$integer.max) ||
     any(dims %% 1 != 0) || any(dims < 1) ||
     length(unique(dims)) != 2L)
    stop("`dims` must be two distinct positive integer column indices, e.g. c(1, 2).",
         call. = FALSE)
  dims <- as.integer(dims)

  if(is.data.frame(x)) {
    num <- vapply(x, is.numeric, logical(1))
    if(sum(num) < 2L)
      stop("The data frame needs at least two numeric columns of coordinates.",
           call. = FALSE)
    m <- as.matrix(x[, num, drop = FALSE])
  } else if(is.matrix(x)) {
    if(!is.numeric(x))
      stop("The coordinate matrix must be numeric.", call. = FALSE)
    m <- x
  } else if(is.list(x)) {
    coord <- if(!is.null(x$embedding)) x$embedding     # uwot (ret_model = TRUE)
             else if(!is.null(x$Y)) x$Y                # Rtsne
             else if(!is.null(x$layout)) x$layout      # umap package
             else NULL
    if(is.null(coord))
      stop("Unrecognized embedding object. Supply a uwot / Rtsne / umap result, ",
           "or a numeric matrix / data frame of coordinates.", call. = FALSE)
    m <- as.matrix(coord)
    if(!is.numeric(m))
      stop("The embedding coordinates must be numeric.", call. = FALSE)
  } else {
    stop("Unrecognized embedding object. Supply a uwot / Rtsne / umap result, ",
         "or a numeric matrix / data frame of coordinates.", call. = FALSE)
  }

  if(ncol(m) < 2L)
    stop("The embedding has ", ncol(m), " dimension(s); at least 2 are needed to ",
         "plot.", call. = FALSE)
  if(max(dims) > ncol(m) || min(dims) < 1L)
    stop("`dims` = c(", dims[1], ", ", dims[2], ") are out of range for a ",
         ncol(m), "-dimensional embedding.", call. = FALSE)
  m <- m[, dims, drop = FALSE]
  if(is.null(rownames(m))) rownames(m) <- as.character(seq_len(nrow(m)))
  m
}
