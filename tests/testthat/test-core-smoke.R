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

test_that("facto_summarize respects axes for MCA supplementary elements", {
  skip_if_not_installed("FactoMineR")

  set.seed(123)
  mca_df <- data.frame(
    a = factor(sample(letters[1:3], 80, TRUE)),
    b = factor(sample(LETTERS[1:4], 80, TRUE)),
    c = factor(sample(c("x", "y", "z"), 80, TRUE)),
    sup_q = factor(sample(c("q1", "q2"), 80, TRUE)),
    sup_n = rnorm(80)
  )
  mca <- FactoMineR::MCA(mca_df, quali.sup = 4, quanti.sup = 5, graph = FALSE)

  mca12 <- facto_summarize(mca, element = "mca.cor", axes = 1:2)
  mca23 <- facto_summarize(mca, element = "mca.cor", axes = 2:3)
  expect_equal(ncol(mca12), 3)
  expect_equal(ncol(mca23), 3)
  expect_false(isTRUE(all.equal(mca12[, 2:3], mca23[, 2:3])))

  q12 <- facto_summarize(mca, element = "quanti.sup", axes = 1:2)
  q23 <- facto_summarize(mca, element = "quanti.sup", axes = 2:3)
  expect_equal(ncol(q12), 3)
  expect_equal(ncol(q23), 3)
  expect_false(isTRUE(all.equal(q12[, 2:3], q23[, 2:3])))

  ind12 <- facto_summarize(mca, element = "ind", axes = 1:2)
  ind23 <- facto_summarize(mca, element = "ind", axes = 2:3)
  expect_equal(ncol(ind12), 6)
  expect_equal(ncol(ind23), 6)
  expect_identical(colnames(ind12)[2:3], c("Dim.1", "Dim.2"))
  expect_identical(colnames(ind23)[2:3], c("Dim.2", "Dim.3"))
})

test_that("FactoMineR category mapping helpers map legacy labels", {
  skip_if_not_installed("FactoMineR")

  set.seed(456)
  mca_df <- data.frame(
    color = factor(sample(c("red", "blue", "green"), 100, TRUE)),
    shape = factor(sample(c("round", "square"), 100, TRUE)),
    size = factor(sample(c("small", "medium", "large"), 100, TRUE))
  )
  mca <- FactoMineR::MCA(mca_df, graph = FALSE)
  map <- factominer_category_map(mca, element = "var")
  expect_s3_class(map, "data.frame")
  expect_true(all(c("current", "legacy_underscore") %in% colnames(map)))

  candidates <- map$legacy_underscore
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  uniq <- candidates[!(duplicated(candidates) | duplicated(candidates, fromLast = TRUE))]
  skip_if(length(uniq) < 1)

  mapped <- map_factominer_legacy_names(mca, uniq[1], element = "var", quiet = TRUE)
  expect_true(mapped %in% map$current)

  expect_warning(
    map_factominer_legacy_names(mca, uniq[1], element = "var", quiet = FALSE),
    "Mapped legacy FactoMineR category labels"
  )
  expect_warning(
    map_factominer_legacy_names(mca, "unknown_level", element = "var", quiet = FALSE),
    "not found in the current FactoMineR output"
  )
  expect_identical(
    map_factominer_legacy_names(mca, "unknown_level", element = "var", quiet = TRUE),
    "unknown_level"
  )
})

test_that(".add_ind_groups handles multi-column habillage reshape path", {
  skip_if_not_installed("FactoMineR")

  pca <- FactoMineR::PCA(iris[, 1:4], graph = FALSE)
  ind <- facto_summarize(pca, element = "ind", axes = 1:2)
  grp <- data.frame(
    species = iris$Species,
    width_band = ifelse(
      iris$Sepal.Width > stats::median(iris$Sepal.Width),
      "high", "low"
    ),
    stringsAsFactors = FALSE
  )

  res <- factoextra:::.add_ind_groups(pca, ind, grp)
  expect_true(is.list(res))
  expect_true(res$is_multiple_habillage)
  expect_identical(res$name.quali, "Groups")
  expect_true(all(c("facet_vars", "Groups") %in% colnames(res$ind)))
  expect_equal(nrow(res$ind), nrow(ind) * ncol(grp))
  expect_setequal(levels(res$ind$facet_vars), colnames(grp))
})
