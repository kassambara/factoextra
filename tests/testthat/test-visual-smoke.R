test_that("PCA/MCA visualization helpers return ggplot objects", {
  skip_if_not_installed("FactoMineR")

  pca <- FactoMineR::PCA(iris[, 1:4], graph = FALSE)
  p_ind <- fviz_pca_ind(pca)
  p_var <- fviz_pca_var(pca)

  expect_s3_class(p_ind, "ggplot")
  expect_s3_class(p_var, "ggplot")

  set.seed(123)
  mca_df <- data.frame(
    a = factor(sample(letters[1:3], 40, TRUE)),
    b = factor(sample(LETTERS[1:3], 40, TRUE)),
    c = factor(sample(c("x", "y"), 40, TRUE)),
    sup_q = factor(sample(c("q1", "q2"), 40, TRUE)),
    sup_n = rnorm(40)
  )
  mca <- FactoMineR::MCA(mca_df, quali.sup = 4, quanti.sup = 5, graph = FALSE)
  p_mca <- fviz_mca_var(mca, choice = "mca.cor")
  expect_s3_class(p_mca, "ggplot")
})

test_that("cluster visual helpers work on hcut/hkmeans outputs", {
  x <- scale(USArrests)

  hc <- hcut(x, k = 4)
  km <- hkmeans(x, k = 4)

  p1 <- fviz_cluster(hc)
  p2 <- fviz_cluster(km)
  p3 <- fviz_silhouette(hc, print.summary = FALSE)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("fviz_dend supports rectangular dendrogram layout", {
  hc <- hclust(dist(iris[, 1:4]))
  p <- fviz_dend(hc, k = 3, rect = TRUE, show_labels = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("fviz_pca_biplot supports form and covariance scaling modes", {
  res.pca <- stats::prcomp(iris[, 1:4], scale. = TRUE)
  p_form <- fviz_pca_biplot(res.pca, biplot.type = "form")
  p_cov <- fviz_pca_biplot(res.pca, biplot.type = "covariance")
  expect_s3_class(p_form, "ggplot")
  expect_s3_class(p_cov, "ggplot")
})

test_that("fviz_eig parallel analysis is reproducible with parallel.seed", {
  res.pca <- stats::prcomp(iris[, 1:4], scale. = TRUE)

  extract_parallel_threshold <- function(p) {
    layers <- ggplot2::ggplot_build(p)$data
    idx <- which(vapply(layers, function(df) {
      "shape" %in% names(df) && all(df$shape == 4)
    }, logical(1)))
    if(length(idx) == 0) return(numeric(0))
    layers[[idx[1]]]$y
  }

  p1 <- fviz_eig(
    res.pca, choice = "eigenvalue", parallel = TRUE,
    parallel.iter = 20, parallel.seed = 42
  )
  p2 <- fviz_eig(
    res.pca, choice = "eigenvalue", parallel = TRUE,
    parallel.iter = 20, parallel.seed = 42
  )
  p3 <- fviz_eig(
    res.pca, choice = "eigenvalue", parallel = TRUE,
    parallel.iter = 20, parallel.seed = 43
  )

  th1 <- extract_parallel_threshold(p1)
  th2 <- extract_parallel_threshold(p2)
  th3 <- extract_parallel_threshold(p3)
  expect_true(length(th1) > 0)
  expect_equal(th1, th2)
  expect_false(isTRUE(all.equal(th1, th3)))
})
