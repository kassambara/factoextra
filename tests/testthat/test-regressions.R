test_that("get_clust_tendency restores RNG state when seed is provided", {
  old_opt <- getOption("factoextra.warn_hopkins", TRUE)
  on.exit(options(factoextra.warn_hopkins = old_opt), add = TRUE)
  options(factoextra.warn_hopkins = FALSE)

  set.seed(2026)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  get_clust_tendency(iris[, 1:4], n = 10, graph = FALSE, seed = 123)
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
})

test_that("fviz_nbclust gap_stat handles maxSE in dots and forwards clusGap args", {
  set.seed(123)
  x <- scale(iris[, 1:4])
  p <- fviz_nbclust(
    x, FUNcluster = stats::kmeans, method = "gap_stat",
    k.max = 3, nboot = 3, verbose = FALSE,
    nstart = 5,
    maxSE = list(method = "firstmax", SE.factor = 1)
  )
  expect_s3_class(p, "ggplot")
})

test_that(".is_color returns a logical vector type-stably", {
  res_empty <- factoextra:::.is_color(character(0))
  expect_type(res_empty, "logical")
  expect_length(res_empty, 0)

  res <- factoextra:::.is_color(c("red", "not-a-color"))
  expect_type(res, "logical")
  expect_identical(unname(res), c(TRUE, FALSE))
})

test_that("eclust restores RNG state after execution", {
  set.seed(2027)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  eclust(scale(USArrests), FUNcluster = "kmeans", k = 2, graph = FALSE)
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
})

test_that("internal jitter and multishape helpers preserve RNG state", {
  set.seed(3030)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  out <- factoextra:::.jitter(
    data.frame(x = 1:5, y = 1:5),
    jitter = list(width = 0.2, height = 0.3)
  )
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
  expect_equal(dim(out), c(5, 2))

  set.seed(3031)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  generated <- factoextra:::.generate_multishapes()
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
  expect_true(nrow(generated) > 0)
})

test_that("phylogenic dendrogram layout does not leak RNG state", {
  skip_if_not_installed("igraph")
  set.seed(4040)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  hc <- hclust(dist(iris[, 1:4]))
  p <- fviz_dend(hc, k = 3, phylogenic = TRUE, labels = FALSE)
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
  expect_s3_class(p, "ggplot")
})
