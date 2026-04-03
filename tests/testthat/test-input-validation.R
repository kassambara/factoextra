test_that("hcut validates k and scaled inputs", {
  x <- iris[, 1:4]
  expect_error(hcut(x, k = 1), "k must be a single integer >= 2")
  expect_error(
    hcut(x, k = nrow(x) + 1),
    "elements of 'k' must be between 1 and"
  )

  x_const <- data.frame(a = 1:10, b = rep(1, 10))
  expect_error(
    hcut(x_const, k = 2, stand = TRUE),
    "Scaling produced NA values"
  )
})

test_that("hcut requires dist input when isdiss is TRUE", {
  x <- iris[, 1:4]
  bad_dist <- stats::as.dist(matrix(c(
    0, NA, 1,
    NA, 0, 2,
    1, 2, 0
  ), nrow = 3, byrow = TRUE))

  expect_error(
    hcut(x, k = 2, isdiss = TRUE),
    "must be an object of class dist"
  )
  expect_error(
    hcut(bad_dist, k = 2, isdiss = TRUE),
    "distance matrix contains NA/NaN/Inf values"
  )
})

test_that("hkmeans validates inputs and k bounds", {
  x <- iris[, 1:4]
  expect_error(hkmeans(x, k = 1), "k must be a single integer >= 2")
  expect_error(
    hkmeans(x, k = nrow(x) + 1),
    "elements of 'k' must be between 1 and"
  )
  expect_error(hkmeans(1:10, k = 2), "x must be a matrix or data.frame")
})

test_that("get_clust_tendency validates numeric data and n", {
  bad_x <- as.matrix(data.frame(a = letters[1:5], b = letters[1:5]))
  expect_error(get_clust_tendency(bad_x, n = 2, graph = FALSE), "numeric")
  expect_error(get_clust_tendency(iris[, 1:4], n = 0, graph = FALSE), "positive integer")
  expect_error(get_clust_tendency(iris[, 1:4], n = nrow(iris), graph = FALSE), "no larger")

  x_na <- matrix(c(1, 2, NA, NA), ncol = 2)
  expect_error(get_clust_tendency(x_na, n = 1, graph = FALSE), "at least two complete rows")
})

test_that("get_dist validates scaled inputs and fviz_dist validates dist values", {
  x_const <- data.frame(a = 1:10, b = rep(1, 10))
  bad_dist <- stats::as.dist(matrix(c(
    0, NA, 1,
    NA, 0, 2,
    1, 2, 0
  ), nrow = 3, byrow = TRUE))

  expect_error(
    get_dist(x_const, method = "pearson", stand = TRUE),
    "Scaling produced NA values"
  )
  expect_error(
    fviz_dist(bad_dist),
    "distance matrix contains NA/NaN/Inf values"
  )
})

test_that("fviz_cluster validates scaled plotting data", {
  degenerate_2d <- data.frame(a = 1:10, b = rep(1, 10))
  degenerate_3d <- data.frame(a = 1:10, b = rep(1, 10), c = rep(2, 10))
  cluster_obj_2d <- list(data = degenerate_2d, cluster = rep(1:2, each = 5))
  cluster_obj_3d <- list(data = degenerate_3d, cluster = rep(1:2, each = 5))

  expect_error(
    fviz_cluster(cluster_obj_2d, stand = TRUE),
    "Scaling produced NA values"
  )
  expect_error(
    fviz_cluster(cluster_obj_3d, stand = TRUE),
    "Scaling produced NA values"
  )
})
