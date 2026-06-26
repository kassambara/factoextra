## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  # All chunks are illustrative: they document the extension contract rather
  # than execute backend code, so the vignette builds without any optional
  # backend package (ExPosition, ade4, vegan, ...) installed.
  eval = FALSE
)

## ----quick-start-prcomp-------------------------------------------------------
# library(factoextra)
#
# # A base-R PCA (any analysis that yields coordinates would do)
# pca <- prcomp(iris[, -5], scale. = TRUE)
#
# # Wrap the coordinates into a factoextra-ready object
# res <- as_factoextra_pca(
#   ind.coord = pca$x,         # individual scores  (n x k)
#   var.coord = pca$rotation,  # variable loadings  (p x k)
#   eig       = pca$sdev^2     # eigenvalues        (optional)
# )
#
# # Now the full fviz_pca_* family works on `res`
# fviz_pca_biplot(res, label = "var", habillage = iris$Species,
#                 addEllipses = TRUE)
# fviz_eig(res)
# fviz_contrib(res, choice = "var", axes = 1)

## ----quick-start-mds----------------------------------------------------------
# mds <- stats::cmdscale(dist(scale(mtcars)), k = 3)   # classical MDS
# fviz_pca_ind(as_factoextra_pca(ind.coord = mds), repel = TRUE)

## -----------------------------------------------------------------------------
# # inside .get_facto_class()
# else if (inherits(X, "myPCA"))            # your new class
#   facto_class <- "PCA"

## -----------------------------------------------------------------------------
# else if (inherits(X, "expoOutput")) {
#   if      (inherits(X$ExPosition.Data, "epCA"))  facto_class <- "CA"
#   else if (inherits(X$ExPosition.Data, "epPCA")) facto_class <- "PCA"
#   else if (inherits(X$ExPosition.Data, "epMCA")) facto_class <- "MCA"
# }

## -----------------------------------------------------------------------------
# # inside get_eig(), before the final stop()
# else if (inherits(X, "myPCA"))
#   eig <- X$eigenvalues          # a numeric vector of eigenvalues

## -----------------------------------------------------------------------------
# else if (inherits(X, "expoOutput"))
#   eig <- X$ExPosition.Data$eigs

## -----------------------------------------------------------------------------
# # get_pca_ind(): stats::prcomp template
# else if (inherits(res.pca, "prcomp")) {
#   ind.coord <- res.pca$x
#   data      <- .prcomp_reconst(res.pca)
#   ind <- .get_pca_ind_results(ind.coord, data, res.pca$sdev^2,
#                               res.pca$center, res.pca$scale)
# }
#
# # get_pca_ind(): ade4 dudi template
# else if (inherits(res.pca, "pca") && inherits(res.pca, "dudi")) {
#   ind.coord <- res.pca$li
#   data <- sweep(res.pca$tab, 2, res.pca$norm, "*")
#   data <- sweep(data,        2, res.pca$cent, "+")
#   ind <- .get_pca_ind_results(ind.coord, data, res.pca$eig,
#                               res.pca$cent, res.pca$norm)
# }

## -----------------------------------------------------------------------------
# # get_pca_var(): stats::prcomp template
# else if (inherits(res.pca, "prcomp")) {
#   var.cor <- sweep(res.pca$rotation, 2, res.pca$sdev, "*")
#   var <- .get_pca_var_results(var.cor)
# }
#
# # get_pca_var(): ade4 dudi template
# else if (inherits(res.pca, "pca") && inherits(res.pca, "dudi")) {
#   var <- .get_pca_var_results(res.pca$co)
# }

## -----------------------------------------------------------------------------
# # get_ca_row(): build the standardized list directly
# else if (inherits(res.ca, "myCA")) {
#   coord   <- res.ca$row.coord
#   cos2    <- res.ca$row.cos2
#   contrib <- res.ca$row.contrib * 100
#   inertia <- res.ca$row.inertia
#   colnames(coord) <- colnames(cos2) <- colnames(contrib) <-
#     paste0("Dim.", seq_len(ncol(coord)))
#   row <- list(coord = coord, cos2 = cos2, contrib = contrib, inertia = inertia)
# }

## -----------------------------------------------------------------------------
# # inside .get_ca_mass()
# else if (inherits(res.ca, "myCA")) {
#   if      (element == "row") mass <- res.ca$row.mass
#   else if (element == "col") mass <- res.ca$col.mass
# }

## -----------------------------------------------------------------------------
# # R/get_pca.R — ExPosition epPCA, individuals
# else if (inherits(res.pca, "expoOutput") &&
#          inherits(res.pca$ExPosition.Data, "epPCA")) {
#   res <- res.pca$ExPosition.Data
#   ind <- list(coord = res$fi, cos2 = res$ri, contrib = res$ci * 100)
# }

## -----------------------------------------------------------------------------
# library(ExPosition)
# library(factoextra)
#
# res.pca <- epPCA(iris[, -5], graph = FALSE)
# fviz_pca_ind(res.pca, habillage = iris$Species, addEllipses = TRUE)
# fviz_eig(res.pca)
# fviz_contrib(res.pca, choice = "var")

## -----------------------------------------------------------------------------
# # .get_facto_class(): treat an unconstrained CA component as CA
# else if (inherits(X, c("cca", "rda")))
#   facto_class <- "CA"
#
# # get_eig(): vegan stores eigenvalues per axis
# else if (inherits(X, c("cca", "rda")))
#   eig <- vegan::eigenvals(X)
#
# # get_ca_row()/get_ca_col(): use vegan::scores() for sites / species
# else if (inherits(res.ca, c("cca", "rda"))) {
#   sc      <- vegan::scores(res.ca, display = "sites", scaling = "sites")
#   coord   <- as.matrix(sc)
#   colnames(coord) <- paste0("Dim.", seq_len(ncol(coord)))
#   # cos2 / contrib / inertia derived from the ordination as appropriate
#   row <- list(coord = coord, cos2 = cos2, contrib = contrib, inertia = inertia)
# }

## -----------------------------------------------------------------------------
# test_that("CA extractors work with <your package> outputs", {
#   skip_if_not_installed("yourpkg")
#   data("housetasks", package = "factoextra")
#   res <- yourpkg::your_ca(housetasks)
#
#   cols <- get_ca_col(res)
#   rows <- get_ca_row(res)
#
#   expect_s3_class(cols, "factoextra")
#   expect_true(all(c("coord", "cos2", "contrib") %in% names(cols)))
#   expect_equal(nrow(cols$coord), ncol(housetasks))
#   expect_equal(nrow(rows$coord), nrow(housetasks))
# })
