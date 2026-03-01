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
