geom_present <- function(p, cls) {
  any(vapply(p$layers, function(l) inherits(l$geom, cls), logical(1)))
}
point_data <- function(p) {
  b <- ggplot2::ggplot_build(p)
  i <- which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomPoint") && inherits(l$stat, "StatIdentity"), logical(1)))[1]
  b$data[[i]]
}

test_that(".fe_layout extracts coordinates from every embedding source", {
  m <- matrix(rnorm(20), 10, 2, dimnames = list(paste0("o", 1:10), NULL))
  expect_equal(unname(factoextra:::.fe_layout(m)), unname(m))
  # uwot ret_model list ($embedding), Rtsne ($Y), umap ($layout)
  expect_equal(factoextra:::.fe_layout(list(embedding = m)), m, ignore_attr = TRUE)
  expect_equal(factoextra:::.fe_layout(structure(list(Y = m), class = "Rtsne")), m,
               ignore_attr = TRUE)
  expect_equal(factoextra:::.fe_layout(list(layout = m)), m, ignore_attr = TRUE)
  # data frame: numeric columns only
  df <- data.frame(a = m[, 1], b = m[, 2], lab = letters[1:10])
  expect_equal(dim(factoextra:::.fe_layout(df)), c(10L, 2L))
  # dims selects columns; a bare matrix never hits `$`
  m3 <- matrix(rnorm(30), 10, 3)
  expect_equal(factoextra:::.fe_layout(m3, dims = c(1, 3)), m3[, c(1, 3)], ignore_attr = TRUE)
})

test_that("fviz_umap/fviz_tsne accept the real uwot / Rtsne / umap objects", {
  X <- as.matrix(iris[, 1:4])
  if (requireNamespace("uwot", quietly = TRUE)) {
    set.seed(1)
    um <- uwot::umap(X, n_neighbors = 15)                 # a bare matrix
    p <- fviz_umap(um, habillage = iris$Species)
    expect_s3_class(p, "ggplot")
    expect_equal(sort(point_data(p)$x), sort(um[, 1]), tolerance = 1e-9)
  }
  if (requireNamespace("Rtsne", quietly = TRUE)) {
    set.seed(1)
    ts <- Rtsne::Rtsne(X, check_duplicates = FALSE)       # list with $Y
    p <- fviz_tsne(ts, habillage = iris$Species)
    expect_identical(p$labels$x, "tSNE1")
    expect_equal(sort(point_data(p)$x), sort(ts$Y[, 1]), tolerance = 1e-9)
  }
  if (requireNamespace("umap", quietly = TRUE)) {
    set.seed(1)
    uu <- umap::umap(X)                                   # umap object with $layout
    expect_s3_class(uu, "umap")
    p <- fviz_umap(uu, habillage = iris$Species)
    expect_s3_class(p, "ggplot")
    expect_equal(sort(point_data(p)$x), sort(uu$layout[, 1]), tolerance = 1e-9)
  }
})

test_that(".fe_layout errors clearly on degenerate / unknown input", {
  expect_error(factoextra:::.fe_layout(matrix(1:10, ncol = 1)), "least 2|2 are needed|dimension")
  expect_error(factoextra:::.fe_layout(list(foo = 1)), "Unrecognized")
  expect_error(factoextra:::.fe_layout("not an embedding"), "Unrecognized")
  expect_error(factoextra:::.fe_layout(matrix(rnorm(20), 10, 2), dims = c(1, 5)), "range")
})

test_that("fviz_umap/fviz_tsne draw an honest embedding scatter (no eigen surface)", {
  set.seed(1)
  m <- matrix(rnorm(60), 30, 2)
  grp <- factor(rep(letters[1:3], 10))

  pu <- fviz_umap(m, habillage = grp, addEllipses = TRUE)
  expect_s3_class(pu, "ggplot")
  # axes labelled by method, with NO percentage / "Dim"
  expect_identical(pu$labels$x, "UMAP1")
  expect_identical(pu$labels$y, "UMAP2")
  expect_false(grepl("%|Dim", paste(pu$labels$x, pu$labels$y)))
  # the guardrail: no variable-arrow (GeomSegment) layer ever
  expect_false(geom_present(pu, "GeomSegment"))
  # addEllipses defaults to a convex hull, not a normal confidence ellipse
  expect_true(geom_present(pu, "GeomPolygon"))
  expect_equal(nrow(point_data(pu)), 30L)

  pt <- fviz_tsne(m)
  expect_identical(pt$labels$x, "tSNE1")
  expect_identical(pt$labels$y, "tSNE2")

  # dims relabel correctly
  p3 <- fviz_umap(matrix(rnorm(90), 30, 3), dims = c(1, 3))
  expect_identical(p3$labels$y, "UMAP3")
})

test_that("fviz_umap plotted coordinates equal the embedding (identity passthrough)", {
  set.seed(1); m <- matrix(rnorm(40), 20, 2)
  d <- point_data(fviz_umap(m, geom = "point"))
  expect_equal(sort(d$x), sort(m[, 1]), tolerance = 1e-9)
  expect_equal(sort(d$y), sort(m[, 2]), tolerance = 1e-9)
})

test_that("fviz_umap colour: continuous feature works, eigen metavalues rejected", {
  set.seed(1); m <- matrix(rnorm(60), 30, 2)
  # continuous feature (FeaturePlot use case) -> a continuous colour scale
  p <- fviz_umap(m, col.ind = rnorm(30))
  sc <- ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")
  expect_false(is.null(sc)); expect_false(sc$is_discrete())
  # eigen-only metavalues have no meaning for an embedding -> clear error
  expect_error(fviz_umap(m, col.ind = "cos2"), "eigenvalue")
  expect_error(fviz_umap(m, col.ind = "contrib"), "eigenvalue")
  # length mismatch -> actionable error naming both lengths
  expect_error(fviz_umap(m, habillage = factor(1:10)), "10 values but the embedding has 30")
})

test_that("fviz_umap max.points downsamples (stratified only on discrete colour)", {
  set.seed(1)
  m <- matrix(rnorm(2000), 1000, 2)
  grp <- factor(rep(letters[1:2], 500))
  p <- suppressMessages(fviz_umap(m, habillage = grp, max.points = 200))
  expect_equal(nrow(point_data(p)), 200L)
  # continuous colour + max.points must NOT stratify (no error, count capped)
  p2 <- suppressMessages(fviz_umap(m, col.ind = rnorm(1000), max.points = 200))
  expect_equal(nrow(point_data(p2)), 200L)
})

test_that("fviz_umap labels are legible and colouring emits no stray message", {
  set.seed(1); m <- matrix(rnorm(60), 30, 2); grp <- factor(rep(letters[1:3], 10))
  # text labels use the same size semantics as fviz_pca_ind (labelsize * 3),
  # so the default is readable, not the invisible size of a raw labelsize.
  txt_size <- function(p) {
    i <- which(vapply(p$layers, function(l)
      inherits(l$geom, c("GeomText", "GeomTextRepel")), logical(1)))[1]
    p$layers[[i]]$aes_params$size
  }
  res <- prcomp(iris[, 1:4])
  expect_equal(txt_size(fviz_umap(m, geom = c("point", "text"), label = TRUE, labelsize = 4)),
               txt_size(fviz_pca_ind(res, geom = c("point", "text"), labelsize = 4)))
  # no "Ignoring unknown labels: fill" message when there is no fill aesthetic
  expect_no_message(ggplot2::ggplotGrob(fviz_umap(m, habillage = grp)))
  expect_no_message(ggplot2::ggplotGrob(fviz_umap(m, col.ind = rnorm(30))))
  # ellipse case (fill mapped) is also clean
  expect_no_message(ggplot2::ggplotGrob(fviz_umap(m, habillage = grp, addEllipses = TRUE)))
})

test_that("fviz_umap density and facet compose without error", {
  set.seed(1); m <- matrix(rnorm(120), 60, 2); grp <- factor(rep(letters[1:2], 30))
  expect_no_error(ggplot2::ggplot_build(fviz_umap(m, density = TRUE)))
  expect_no_error(ggplot2::ggplot_build(fviz_umap(m, habillage = grp, facet.by = grp)))
})
