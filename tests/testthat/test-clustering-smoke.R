test_that("Gap statistic plot helper returns ggplot", {
  set.seed(123)
  x <- scale(iris[, 1:4])
  gap <- cluster::clusGap(x, FUN = stats::kmeans, K.max = 4, B = 5)

  p <- fviz_gap_stat(gap)
  expect_s3_class(p, "ggplot")
})

test_that("Hopkins tendency returns statistic and optional plot", {
  old_opt <- getOption("factoextra.warn_hopkins", TRUE)
  on.exit(options(factoextra.warn_hopkins = old_opt), add = TRUE)
  options(factoextra.warn_hopkins = FALSE)

  res <- get_clust_tendency(iris[, 1:4], n = 10, graph = FALSE)
  expect_type(res$hopkins_stat, "double")
  expect_false(is.null(res$hopkins_stat))
  expect_null(res$plot)
})

test_that("eclust() accepts a precomputed dist for hierarchical clustering (#182)", {
  set.seed(1)
  df <- matrix(rpois(20 * 6, 5), nrow = 20)
  rownames(df) <- paste0("s", seq_len(nrow(df)))

  # Bray-Curtis dissimilarity, computed locally (no extra dependency)
  braycurtis <- function(m) {
    n <- nrow(m)
    d <- matrix(0, n, n)
    for (i in seq_len(n)) for (j in seq_len(n)) {
      d[i, j] <- sum(abs(m[i, ] - m[j, ])) / sum(m[i, ] + m[j, ])
    }
    stats::as.dist(d)
  }
  bc <- braycurtis(df)

  res <- eclust(bc, "hclust", k = 3, graph = FALSE)
  expect_s3_class(res, "eclust")
  # The supplied distance is honored, not recomputed as Euclidean
  expect_equal(res$height, stats::hclust(bc, method = "ward.D2")$height)

  # Auto-k cannot run on a bare dist (gap statistic needs the original data)
  expect_error(eclust(bc, "hclust", graph = FALSE), "number of clusters 'k'")
  # Partitioning methods that need raw data reject a dist clearly
  expect_error(eclust(bc, "kmeans", k = 3, graph = FALSE), "hierarchical clustering")
  # Scaling a dissimilarity is rejected
  expect_error(eclust(bc, "hclust", k = 3, stand = TRUE, graph = FALSE), "stand = TRUE")
})

test_that("eclust() hierarchical clustering on raw data is unchanged (#182 no-regression)", {
  res <- eclust(iris[, 1:4], "hclust", hc_metric = "manhattan", k = 3, graph = FALSE)
  ref <- stats::hclust(get_dist(iris[, 1:4], method = "manhattan"), method = "ward.D2")
  expect_equal(res$height, ref$height)
})
