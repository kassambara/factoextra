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
