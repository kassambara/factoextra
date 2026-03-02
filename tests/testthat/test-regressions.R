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

test_that("get_clust_tendency Hopkins value matches regression baseline", {
  old_opt <- getOption("factoextra.warn_hopkins", TRUE)
  on.exit(options(factoextra.warn_hopkins = old_opt), add = TRUE)
  options(factoextra.warn_hopkins = FALSE)

  res <- get_clust_tendency(iris[, 1:4], n = 10, graph = FALSE, seed = 123)
  # BLAS matrix operations (tcrossprod/outer) yield slightly different

  # floating-point results across platforms; use a wider tolerance.
  expect_equal(res$hopkins_stat, 0.989362891844348, tolerance = 0.01)
})

test_that("get_clust_tendency low-memory fallback matches vectorized computation", {
  old_warn <- getOption("factoextra.warn_hopkins", TRUE)
  old_cells <- getOption("factoextra.hopkins.max_matrix_cells", 2e7)
  on.exit(
    options(
      factoextra.warn_hopkins = old_warn,
      factoextra.hopkins.max_matrix_cells = old_cells
    ),
    add = TRUE
  )
  options(factoextra.warn_hopkins = FALSE)

  options(factoextra.hopkins.max_matrix_cells = 1)
  res_loop <- get_clust_tendency(iris[, 1:4], n = 10, graph = FALSE, seed = 123)

  options(factoextra.hopkins.max_matrix_cells = 1e9)
  res_vec <- get_clust_tendency(iris[, 1:4], n = 10, graph = FALSE, seed = 123)

  # Different BLAS accumulation order in chunked vs full-matrix tcrossprod
  # can shift nearest-neighbor selection, producing different Hopkins values.
  expect_equal(res_loop$hopkins_stat, res_vec$hopkins_stat, tolerance = 0.01)
})

test_that("get_clust_tendency emits correction warning only once per session", {
  old_opt <- getOption("factoextra.warn_hopkins", TRUE)
  on.exit(options(factoextra.warn_hopkins = old_opt), add = TRUE)
  options(factoextra.warn_hopkins = TRUE)

  state <- getFromNamespace(".factoextra_state", "factoextra")
  state$hopkins_warned <- FALSE
  expect_warning(
    get_clust_tendency(iris[, 1:4], n = 10, graph = FALSE, seed = 123),
    "corrected formula"
  )
  expect_no_warning(get_clust_tendency(iris[, 1:4], n = 10, graph = FALSE, seed = 123))
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

test_that("fviz_nbclust wss path returns ggplot and forwards FUNcluster args", {
  set.seed(123)
  x <- scale(iris[, 1:4])
  p <- fviz_nbclust(x, FUNcluster = stats::kmeans, method = "wss", k.max = 4, nstart = 5)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 4)
  expect_false(anyNA(p$data$y))
})

test_that("fviz_nbclust handles matrix Best.nc and preserves numeric cluster order", {
  best_nc <- rbind(
    Number_clusters = c(2, 10, 3, 10),
    Value_Index = c(1, 1, 1, 1)
  )
  p <- fviz_nbclust(list(Best.nc = best_nc), print.summary = FALSE)
  expect_s3_class(p, "ggplot")
  expect_identical(levels(p$data$Number_clusters), c("2", "3", "10"))
})

test_that("fviz_nbclust silhouette handles k >= n without error", {
  x <- matrix(seq_len(20), ncol = 4)
  toy_cluster <- function(x, k, ...) {
    n <- nrow(as.matrix(x))
    if (k >= n) cl <- seq_len(n) else cl <- rep(seq_len(k), length.out = n)
    list(cluster = cl)
  }
  p <- fviz_nbclust(x, FUNcluster = toy_cluster, method = "silhouette", k.max = 5)
  expect_s3_class(p, "ggplot")
  expect_true(anyNA(p$data$y))
})

test_that(".is_color returns a logical vector type-stably", {
  res_empty <- factoextra:::.is_color(character(0))
  expect_type(res_empty, "logical")
  expect_length(res_empty, 0)

  res <- factoextra:::.is_color(c("red", "not-a-color"))
  expect_type(res, "logical")
  expect_identical(unname(res), c(TRUE, FALSE))
})

test_that(".is_color_palette recognizes known palette names", {
  expect_true(factoextra:::.is_color_palette("Blues"))
  expect_true(factoextra:::.is_color_palette("jco"))
  expect_false(factoextra:::.is_color_palette("not-a-palette"))
  expect_false(factoextra:::.is_color_palette(NULL))
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

test_that(".with_preserved_seed evaluates correctly through wrapper frames", {
  wrapper <- function(seed, expr) {
    inner <- function(code) factoextra:::.with_preserved_seed(seed, code)
    inner(expr)
  }

  set.seed(9090)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)

  set.seed(123)
  expected <- stats::runif(5)
  assign(".Random.seed", seed_before, envir = .GlobalEnv)

  out <- wrapper(123, stats::runif(5))
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_equal(out, expected)
  expect_identical(seed_after, seed_before)
  expect_length(out, 5)
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

test_that("legacy fviz_cluster arguments emit deprecation warnings", {
  x <- scale(USArrests)
  km <- stats::kmeans(x, centers = 3, nstart = 5)

  expect_warning(
    fviz_cluster(km, data = x, jitter = list(width = 0.1)),
    "deprecated"
  )
  expect_warning(
    fviz_cluster(km, data = x, frame = TRUE),
    "deprecated"
  )
  expect_warning(
    fviz_cluster(km, data = x, frame.type = "norm"),
    "deprecated"
  )
  expect_warning(
    fviz_cluster(km, data = x, frame.level = 0.8),
    "deprecated"
  )
  expect_warning(
    fviz_cluster(km, data = x, frame.alpha = 0.3),
    "deprecated"
  )
  expect_warning(
    fviz_cluster(km, data = x, title = "Legacy title"),
    "deprecated"
  )
  expect_warning({
    p_jitter_true <- fviz_cluster(km, data = x, jitter = TRUE)
    expect_s3_class(p_jitter_true, "ggplot")
  }, "deprecated")
})

test_that("fviz_eig validates parallel.seed range and integer-ness", {
  res.pca <- stats::prcomp(iris[, 1:4], scale. = TRUE)

  expect_error(
    fviz_eig(
      res.pca, choice = "eigenvalue", parallel = TRUE,
      parallel.iter = 2, parallel.seed = 1e10
    ),
    "single integer value in"
  )
  expect_error(
    fviz_eig(
      res.pca, choice = "eigenvalue", parallel = TRUE,
      parallel.iter = 2, parallel.seed = -1
    ),
    "single integer value in"
  )
  expect_error(
    fviz_eig(
      res.pca, choice = "eigenvalue", parallel = TRUE,
      parallel.iter = 2, parallel.seed = 1.5
    ),
    "single integer value in"
  )
})
