# Edge-case correctness and input-validation guards (PR #274).
# Batch B populates the additive-validation blocks; later batches add the
# numeric-correctness blocks (PCA metric stability, Hopkins sampling, etc.).

test_that("embedding dimensions require distinct finite positive integers", {
  x <- matrix(rnorm(30), nrow = 10, ncol = 3)
  for(dims in list(c(1, 1), c(1.5, 2), c(1, Inf), c(0, 1), 1:3)){
    expect_error(factoextra:::.fe_layout(x, dims),
                 "two distinct positive integer")
  }
  expect_equal(
    factoextra:::.fe_layout(x, c(1, 3)), x[, c(1, 3), drop = FALSE],
    ignore_attr = TRUE
  )
})

test_that("fviz_nbclust validates k.max as a single integer >= 2", {
  x <- scale(iris[1:20, 1:4])
  for(k_max in list(1, NA_real_, Inf, 2.5, c(2, 3))){
    expect_error(
      fviz_nbclust(x, stats::kmeans, method = "wss", k.max = k_max),
      "k.max must be a single integer value"
    )
  }
  expect_s3_class(
    fviz_nbclust(x, stats::kmeans, method = "wss", k.max = 2, nstart = 2),
    "ggplot"
  )
})

test_that("fviz_dend validates k against the number of leaves", {
  hc <- hclust(dist(matrix(seq_len(15), nrow = 5)))
  expect_s3_class(fviz_dend(hc, k = 5, rect = TRUE), "ggplot")
  expect_error(fviz_dend(hc, k = 6), "in \\[1, 5\\]")
  expect_error(fviz_dend(hc, k = 2.5), "single integer value")
  expect_error(fviz_dend(hc, k = NA_real_), "single integer value")
  expect_error(fviz_dend(hc, k = 1, rect = TRUE), "in \\[2, 5\\]")
  expect_s3_class(fviz_dend(hc, k = 1), "ggplot")
})

# ---- Batch C (PR #274) scoped bug fixes -------------------------------------

test_that("fviz_cluster aligns exact named assignments, falls back to positional", {
  set.seed(901)
  x <- scale(USArrests)
  km <- kmeans(x, centers = 3, nstart = 10)
  reordered <- x[rev(seq_len(nrow(x))), , drop = FALSE]

  # Exact, unique, matching names on both sides -> aligned by name.
  p <- fviz_cluster(km, data = reordered, stand = FALSE, geom = "point",
                    ellipse = FALSE, show.clust.cent = FALSE)
  got <- setNames(as.character(p$data$cluster), p$data$name)
  expect_equal(unname(got[rownames(reordered)]),
               as.character(km$cluster[rownames(reordered)]))

  # Non-regression: names that do not line up are used positionally (they still
  # plot, as they did before this change) rather than erroring.
  mismatched <- reordered; rownames(mismatched)[1] <- "not-an-observation"
  expect_s3_class(fviz_cluster(km, data = mismatched, stand = FALSE,
                               geom = "point", ellipse = FALSE), "ggplot")
  duplicated <- reordered; rownames(duplicated)[1] <- rownames(duplicated)[2]
  expect_s3_class(fviz_cluster(km, data = duplicated, stand = FALSE,
                               geom = "point", ellipse = FALSE), "ggplot")
  partially_named <- km; names(partially_named$cluster)[1] <- ""
  expect_s3_class(fviz_cluster(partially_named, data = reordered, stand = FALSE,
                               geom = "point", ellipse = FALSE), "ggplot")

  # A genuine length mismatch is still a clear error.
  short <- list(data = x[1:4, 1:2, drop = FALSE], cluster = 1:3)
  expect_error(fviz_cluster(short, stand = FALSE), "do not match in length")

  # `clustering` is accepted; when both are present, `cluster` wins.
  both <- list(data = x[, 1:2, drop = FALSE],
               cluster = setNames(rep(1L, nrow(x)), rownames(x)),
               clustering = setNames(rep(2L, nrow(x)), rownames(x)))
  both_plot <- fviz_cluster(both, stand = FALSE, geom = "point", ellipse = FALSE,
                            show.clust.cent = FALSE)
  expect_true(all(as.character(both_plot$data$cluster) == "1"))

  clustering_only <- list(data = x[, 1:2, drop = FALSE],
                          clustering = setNames(rep(3L, nrow(x)), rownames(x)))
  expect_s3_class(fviz_cluster(clustering_only, stand = FALSE, geom = "point",
                               ellipse = FALSE), "ggplot")
})

test_that("supplementary CA columns and invisible = all use the right flags", {
  lab <- factoextra:::.label("all")
  visible <- factoextra:::.define_element_sup(
    structure(list(), class = "CA"), "col", "point", lab,
    factoextra:::.hide("none")
  )
  hidden <- factoextra:::.define_element_sup(
    structure(list(), class = "CA"), "col", "point", lab,
    factoextra:::.hide("col.sup")
  )
  expect_identical(visible$name, "col.sup")
  expect_null(hidden$name)
  expect_true(all(unlist(factoextra:::.hide("all"))))

  pca <- prcomp(iris[, -5], scale. = TRUE)
  p <- fviz_pca_ind(pca, invisible = "all")
  primary_geoms <- vapply(p$layers, function(layer) {
    inherits(layer$geom, c("GeomPoint", "GeomText", "GeomTextRepel"))
  }, logical(1))
  expect_false(any(primary_geoms))
})

test_that("selection warns about unmatched names without dropping matches", {
  # The plot path warns (esup-aware), keeping the matched names.
  pca <- prcomp(iris[, -5], scale. = TRUE)
  expect_warning(
    p <- fviz_pca_ind(pca, select.ind = list(name = c("1", "typo")), geom = "point"),
    "Selection name.*typo"
  )
  expect_true("1" %in% as.character(p$data$name))
  expect_false("typo" %in% as.character(p$data$name))

  # .select itself is silent by default (no leak into fviz_contrib/fviz_cos2),
  # and warns only when explicitly asked (warn_unmatched = TRUE).
  d <- data.frame(name = c("a", "b", "c"), cos2 = c(0.9, 0.2, 0.8),
                  contrib = c(5, 3, 1), row.names = c("a", "b", "c"))
  expect_silent(factoextra:::.select(d, list(name = c("a", "typo"))))
  expect_warning(
    selected <- factoextra:::.select(d, list(name = c("a", "typo")),
                                     warn_unmatched = TRUE),
    "Selection name.*typo"
  )
  expect_identical(selected$name, "a")
})

test_that("fviz_gap_stat maxSE fallback uses firstSEmax (honours SE.factor)", {
  fake_gap <- structure(list(Tab = cbind(
    gap = c(1, 1.5, 1.4), SE.sim = c(0.1, 0.6, 0.1)
  )), class = "clusGap")
  vline <- function(p){
    layer <- Filter(function(x) inherits(x$geom, "GeomVline"), p$layers)[[1]]
    layer$data$xintercept
  }
  # default call: firstSEmax (unchanged)
  expect_equal(vline(fviz_gap_stat(fake_gap)), 1)
  # partial list without method: NOW falls back to firstSEmax (was firstmax -> 2)
  expect_equal(vline(fviz_gap_stat(fake_gap, maxSE = list(SE.factor = 1))), 1)
  # explicit method is still honoured
  expect_equal(vline(fviz_gap_stat(
    fake_gap, maxSE = list(method = "firstmax", SE.factor = 1)
  )), 2)
})
