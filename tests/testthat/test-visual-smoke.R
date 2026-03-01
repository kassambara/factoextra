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
