test_that("fviz_eig warns for a truncated FactoMineR PCA spectrum", {
  skip_if_not_installed("FactoMineR")

  complete <- FactoMineR::PCA(
    decathlon2[1:23, 1:10], ncp = 10, graph = FALSE
  )
  truncated <- complete
  truncated$eig <- truncated$eig[1:5, , drop = FALSE]
  truncated$svd$sumvp <- sum(complete$eig[, 1])

  expect_warning(
    fviz_eig(truncated),
    "does not include the complete spectrum",
    class = "factoextra_truncated_pca_spectrum"
  )
  expect_no_warning(fviz_eig(complete))
  expect_no_warning(fviz_eig(complete, ncp = 2))

  stored <- get_eigenvalue(truncated)
  expect_equal(
    unname(as.matrix(stored)),
    unname(as.matrix(truncated$eig))
  )
})

test_that("fviz_eig recognizes truncated spectra produced by FactoMineR", {
  skip_if_not_installed("FactoMineR")

  truncated <- FactoMineR::PCA(
    decathlon2[1:23, 1:10], graph = FALSE
  )
  eig <- get_eigenvalue(truncated)
  skip_if_not(
    factoextra:::.fm_pca_spectrum_truncated(truncated, eig),
    "this FactoMineR version does not record detectable truncation"
  )

  expect_warning(
    fviz_eig(truncated),
    "does not include the complete spectrum",
    class = "factoextra_truncated_pca_spectrum"
  )
})

test_that("alternate PCA backends do not trigger truncation warnings", {
  set.seed(29)
  x <- matrix(rnorm(80), ncol = 4)

  expect_no_warning(fviz_eig(prcomp(x)))
  expect_no_warning(fviz_eig(princomp(x)))

  custom <- as_factoextra_pca(
    ind.coord = prcomp(x)$x,
    eig = prcomp(x)$sdev^2
  )
  expect_no_warning(fviz_eig(custom))
})

test_that("rank-deficient complete FactoMineR PCA spectra do not warn", {
  skip_if_not_installed("FactoMineR")

  x <- data.frame(
    a = seq_len(20),
    b = rep(c(-1, 1), 10),
    c = seq_len(20) + rep(c(-1, 1), 10),
    d = seq_len(20)
  )
  complete <- FactoMineR::PCA(x, ncp = 4, graph = FALSE)

  expect_no_warning(fviz_eig(complete))
})

test_that("sparse and ade4 PCA classes do not trigger FactoMineR warnings", {
  skip_if_not_installed("FactoMineR")

  sparse_like <- FactoMineR::PCA(
    decathlon2[1:23, 1:10], graph = FALSE
  )
  class(sparse_like) <- c("sPCA", class(sparse_like))
  expect_no_warning(fviz_eig(sparse_like))

  skip_if_not_installed("ade4")
  data(meaudret, package = "ade4")
  pca <- ade4::dudi.pca(meaudret$env, scannf = FALSE, nf = 3)
  group <- meaudret$design$season
  between <- ade4::bca(pca, group, scannf = FALSE, nf = 2)
  within <- ade4::wca(pca, group, scannf = FALSE, nf = 2)

  expect_no_warning(fviz_eig(between))
  expect_no_warning(fviz_eig(within))
})

test_that("malformed optional FactoMineR metadata does not add failures", {
  skip_if_not_installed("FactoMineR")

  template <- FactoMineR::PCA(iris[, 1:4], ncp = 4, graph = FALSE)
  variants <- list(
    no_svd = within(template, svd <- NULL),
    null = within(template, svd$sumvp <- NULL),
    missing = within(template, svd$sumvp <- NA_real_),
    infinite = within(template, svd$sumvp <- Inf),
    zero = within(template, svd$sumvp <- 0),
    negative = within(template, svd$sumvp <- -1),
    character = within(template, svd$sumvp <- "4"),
    vector = within(template, svd$sumvp <- c(4, 4)),
    missing_eigenvalue = within(template, eig[1, 1] <- NA_real_)
  )

  for (object in variants) {
    expect_no_warning(fviz_eig(object))
  }
})
