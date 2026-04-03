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

test_that("eclust validates scaled inputs consistently across backends", {
  x_const <- data.frame(a = 1:10, b = rep(1, 10))

  expect_error(
    eclust(x_const, FUNcluster = "hclust", k = 2, stand = TRUE, graph = FALSE),
    "Scaling produced NA values"
  )
  expect_error(
    eclust(x_const, FUNcluster = "kmeans", k = 2, stand = TRUE, graph = FALSE),
    "Scaling produced NA values"
  )
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

test_that("MCA quanti.sup workflows error cleanly when absent", {
  skip_if_not_installed("FactoMineR")

  data(poison, package = "FactoMineR")
  res.mca <- FactoMineR::MCA(poison[, 5:15], graph = FALSE)

  expect_error(
    get_mca_var(res.mca, "quanti.sup"),
    "There are no quantitative supplementary variables in this MCA"
  )
  expect_error(
    facto_summarize(res.mca, "quanti.sup", axes = 1:2),
    "There are no quantitative supplementary variables in this MCA"
  )
  expect_error(
    fviz(res.mca, "quanti.sup"),
    "There are no quantitative supplementary variables in this MCA"
  )
})

test_that("axes validation rejects invalid axis indices", {
  res.pca <- stats::prcomp(iris[, 1:4], scale. = TRUE)

  expect_error(
    facto_summarize(res.pca, "var", axes = c(1, NA)),
    "The value of the argument axes is incorrect"
  )
  expect_error(
    facto_summarize(res.pca, "var", axes = 0),
    "The value of the argument axes is incorrect"
  )
  expect_error(
    facto_summarize(res.pca, "var", axes = c(1, 1.5)),
    "The value of the argument axes is incorrect"
  )
  expect_error(
    fviz_cos2(res.pca, choice = "var", axes = c(1, NA)),
    "The value of the argument axes is incorrect"
  )
  expect_error(
    fviz_contrib(res.pca, choice = "var", axes = c(1, 0)),
    "The value of the argument axes is incorrect"
  )
  expect_error(
    fviz_ellipses(res.pca, habillage = iris$Species, axes = c(1, 1.5)),
    "The value of the argument axes is incorrect"
  )
})

test_that("fviz_eig validates ncp", {
  res.pca <- stats::prcomp(iris[, 1:4], scale. = TRUE)

  expect_error(
    fviz_eig(res.pca, ncp = NA),
    "ncp must be a single positive integer value in"
  )
  expect_error(
    fviz_eig(res.pca, ncp = -1),
    "ncp must be a single positive integer value in"
  )
  expect_error(
    fviz_eig(res.pca, ncp = 1.5),
    "ncp must be a single positive integer value in"
  )
})
