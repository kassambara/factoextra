test_that("PCA extractors return expected structures", {
  res <- stats::prcomp(iris[, 1:4], scale. = TRUE)

  ind <- get_pca_ind(res)
  var <- get_pca_var(res)

  expect_s3_class(ind, "factoextra")
  expect_s3_class(var, "factoextra")
  expect_true(all(c("coord", "cos2", "contrib") %in% names(ind)))
  expect_true(all(c("coord", "cor", "cos2", "contrib") %in% names(var)))
  expect_true(all(startsWith(colnames(ind$coord), "Dim.")))
  expect_true(all(startsWith(colnames(var$coord), "Dim.")))
})

test_that("CA extractors work with FactoMineR outputs", {
  skip_if_not_installed("FactoMineR")

  data("housetasks", package = "factoextra")
  res.ca <- FactoMineR::CA(housetasks, graph = FALSE)

  cols <- get_ca_col(res.ca)
  rows <- get_ca_row(res.ca)

  expect_s3_class(cols, "factoextra")
  expect_s3_class(rows, "factoextra")
  expect_true(all(c("coord", "cos2", "contrib") %in% names(cols)))
  expect_true(all(c("coord", "cos2", "contrib") %in% names(rows)))
})
