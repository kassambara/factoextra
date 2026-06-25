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

test_that("fviz_nbclust wss handles clustering helpers that reject k = 1", {
  x <- scale(iris[, 1:4])

  p_hcut <- fviz_nbclust(
    x, FUNcluster = hcut, method = "wss", k.max = 4,
    hc_method = "complete"
  )
  expect_s3_class(p_hcut, "ggplot")
  expect_equal(nrow(p_hcut$data), 4)
  expect_false(anyNA(p_hcut$data$y))

  p_hkmeans <- fviz_nbclust(x, FUNcluster = hkmeans, method = "wss", k.max = 4)
  expect_s3_class(p_hkmeans, "ggplot")
  expect_equal(nrow(p_hkmeans$data), 4)
  expect_false(anyNA(p_hkmeans$data$y))
})

test_that("fviz_nbclust gap_stat continues to support hcut", {
  set.seed(123)
  x <- scale(iris[, 1:4])
  p <- fviz_nbclust(
    x, FUNcluster = hcut, method = "gap_stat",
    k.max = 3, nboot = 2, verbose = FALSE,
    hc_method = "complete"
  )
  expect_s3_class(p, "ggplot")
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

test_that("fviz_nbclust silhouette omits the undefined k = 1 point", {
  x <- rbind(
    matrix(rnorm(20, mean = 0, sd = 0.1), ncol = 2),
    matrix(rnorm(20, mean = 5, sd = 0.1), ncol = 2)
  )
  toy_cluster <- function(x, k, ...) {
    n <- nrow(as.matrix(x))
    list(cluster = rep(seq_len(k), length.out = n))
  }

  p <- fviz_nbclust(x, FUNcluster = toy_cluster, method = "silhouette", k.max = 3)
  expect_s3_class(p, "ggplot")
  expect_identical(as.integer(as.character(p$data$clusters)), 2:3)

  built <- ggplot2::ggplot_build(p)
  vline_data <- built$data[[length(built$data)]]
  expect_equal(as.numeric(vline_data$xintercept), 1)
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

test_that("eclust hierarchical auto-k handles a one-cluster gap selection", {
  x <- scale(iris[, 1:4])
  fake_gap <- structure(
    list(Tab = cbind(gap = c(0.5, 0.4, 0.3), SE.sim = c(0.05, 0.05, 0.05))),
    class = "clusGap"
  )
  testthat::local_mocked_bindings(
    .gap_stat = function(...) list(stat = fake_gap, k = 1L),
    .env = environment(eclust)
  )

  for(fun in c("hclust", "agnes", "diana")) {
    res <- eclust(
      x, FUNcluster = fun, k = NULL, k.max = 3, nboot = 2,
      verbose = FALSE, graph = FALSE, hc_method = "complete", seed = 1
    )
    expect_s3_class(res, "eclust")
    expect_identical(res$nbclust, 1L)
    expect_true(all(res$cluster == 1L))
    expect_identical(names(res$cluster), rownames(x))
    expect_identical(res$size, nrow(x))
  }
})

test_that("fviz_silhouette errors cleanly for one-cluster hierarchical results", {
  x <- scale(iris[, 1:4])
  fake_gap <- structure(
    list(Tab = cbind(gap = c(0.5, 0.4, 0.3), SE.sim = c(0.05, 0.05, 0.05))),
    class = "clusGap"
  )
  testthat::local_mocked_bindings(
    .gap_stat = function(...) list(stat = fake_gap, k = 1L),
    .env = environment(eclust)
  )

  res <- eclust(
    x, FUNcluster = "hclust", k = NULL, k.max = 3, nboot = 2,
    verbose = FALSE, graph = FALSE, hc_method = "complete", seed = 1
  )
  expect_error(
    fviz_silhouette(res),
    "Silhouette information is unavailable"
  )
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
  p <- expect_no_warning(
    fviz_dend(hc, k = 3, type = "phylogenic", show_labels = FALSE)
  )
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
  expect_s3_class(p, "ggplot")
})

test_that("phylogenic dendrogram accepts layout_nicely compatibility alias", {
  skip_if_not_installed("igraph")
  set.seed(5050)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  hc <- hclust(dist(iris[, 1:4]))
  p <- expect_no_warning(
    fviz_dend(hc, k = 3, type = "phylogenic", phylo_layout = "layout_nicely", show_labels = FALSE)
  )
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
  expect_s3_class(p, "ggplot")
})

test_that("phylogenic dendrogram keeps legacy layout.gem warning-free", {
  skip_if_not_installed("igraph")
  set.seed(6060)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  hc <- hclust(dist(iris[, 1:4]))
  p <- expect_no_warning(
    fviz_dend(hc, k = 3, type = "phylogenic", phylo_layout = "layout.gem", show_labels = FALSE)
  )
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
  expect_s3_class(p, "ggplot")
})

test_that("phylogenic dendrogram accepts layout_with_gem alias", {
  skip_if_not_installed("igraph")
  set.seed(7070)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  hc <- hclust(dist(iris[, 1:4]))
  p <- expect_no_warning(
    fviz_dend(hc, k = 3, type = "phylogenic", phylo_layout = "layout_with_gem", show_labels = FALSE)
  )
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
  expect_s3_class(p, "ggplot")
})

test_that("phylogenic dendrogram keeps legacy layout.mds warning-free", {
  skip_if_not_installed("igraph")
  set.seed(8080)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  hc <- hclust(dist(iris[, 1:4]))
  p <- expect_no_warning(
    fviz_dend(hc, k = 3, type = "phylogenic", phylo_layout = "layout.mds", show_labels = FALSE)
  )
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
  expect_s3_class(p, "ggplot")
})

test_that("phylogenic dendrogram accepts layout_with_mds alias", {
  skip_if_not_installed("igraph")
  set.seed(9090)
  seed_before <- get(".Random.seed", envir = .GlobalEnv)
  hc <- hclust(dist(iris[, 1:4]))
  p <- expect_no_warning(
    fviz_dend(hc, k = 3, type = "phylogenic", phylo_layout = "layout_with_mds", show_labels = FALSE)
  )
  seed_after <- get(".Random.seed", envir = .GlobalEnv)
  expect_identical(seed_after, seed_before)
  expect_s3_class(p, "ggplot")
})

test_that("hmfa group printing lists only available result components", {
  skip_if_not_installed("FactoMineR")

  data(wine, package = "FactoMineR")
  hierar <- list(c(2, 5, 3, 10, 9, 2), c(4, 2))
  res.hmfa <- FactoMineR::HMFA(wine, H = hierar, type = c("n", rep("s", 5)), graph = FALSE)
  grp <- get_hmfa_var(res.hmfa, "group")

  out <- capture.output(print(grp))
  expect_true(any(grepl("\\$coord", out)))
  expect_true(any(grepl("\\$canonical", out)))
  expect_false(any(grepl('""\\s+""', out)))
})

test_that("get_pca_ind returns finite cos2 for zero-distance rows", {
  x <- data.frame(a = c(-1, 0, 1), b = c(-2, 0, 2))
  res <- stats::prcomp(x, center = TRUE, scale. = FALSE)

  ind <- get_pca_ind(res)

  expect_true(all(is.finite(ind$cos2)))
  expect_equal(unname(ind$cos2[2, ]), c(0, 0))
})

test_that("fviz text labels do not leak a glyph into the legend (#14)", {
  res <- stats::prcomp(mtcars[, 1:7], scale. = TRUE)
  p <- fviz_pca_ind(res, habillage = factor(mtcars$carb))

  text_geoms <- c("GeomText", "GeomTextRepel", "GeomLabel", "GeomLabelRepel")
  text_layers <- Filter(function(L) inherits(L$geom, text_geoms), p$layers)

  expect_gt(length(text_layers), 0)
  expect_true(all(vapply(text_layers, function(L) isFALSE(L$show.legend), logical(1))))
})

test_that("FAMD plots handle qualitative variables with shared factor levels (#184, #140)", {
  skip_if_not_installed("FactoMineR")

  set.seed(1)
  n <- 60
  df <- data.frame(
    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
    f1 = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE)),
    f2 = factor(sample(c("Low", "High"), n, replace = TRUE)) # shares Low/High with f1
  )
  res <- FactoMineR::FAMD(df, graph = FALSE)

  # Previously errored with "duplicate 'row.names' are not allowed"
  expect_s3_class(fviz_contrib(res, choice = "quali.var"), "ggplot")
  expect_s3_class(fviz_contrib(res, choice = "quanti.var"), "ggplot")
  expect_s3_class(fviz_famd_ind(res), "ggplot")
  expect_s3_class(fviz_famd_var(res, "quali.var"), "ggplot")

  # Colliding categories are disambiguated as variable_level; unique ones stay plain
  fc <- facto_summarize(res, element = "quali.var", result = "contrib", axes = 1)
  expect_false(any(duplicated(fc$name)))
  expect_true(all(c("f1_Low", "f1_High", "f2_Low", "f2_High") %in% fc$name))
  expect_true("Medium" %in% fc$name) # Medium only in f1 -> not prefixed
})

test_that("FAMD without shared factor levels keeps plain category labels (#184)", {
  skip_if_not_installed("FactoMineR")

  data(wine, package = "FactoMineR")
  res <- FactoMineR::FAMD(wine, graph = FALSE)
  fc <- facto_summarize(res, element = "quali.var", result = "contrib", axes = 1)

  expect_true(all(c("Saumur", "Env1") %in% fc$name))
  expect_false(any(grepl("_", fc$name))) # no disambiguation when names are unique
})

test_that("fviz_pca_biplot auto scaling keeps plot coordinates finite", {
  x <- data.frame(a = c(1, 1, 1), b = c(-1, 0, 1), c = c(-2, 0, 2))
  res <- stats::prcomp(x, center = TRUE, scale. = FALSE)

  p <- fviz_pca_biplot(res, axes = c(2, 3), biplot.type = "auto")
  built <- ggplot2::ggplot_build(p)
  coords <- lapply(
    built$data,
    function(layer) layer[, intersect(c("x", "y", "xend", "yend"), names(layer)), drop = FALSE]
  )

  expect_false(any(vapply(coords, function(layer) any(!is.finite(as.matrix(layer))), logical(1))))
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

test_that("fviz_eig validates parallel.iter for parallel analysis", {
  res.pca <- stats::prcomp(iris[, 1:4], scale. = TRUE)

  expect_error(
    fviz_eig(
      res.pca, choice = "eigenvalue", parallel = TRUE,
      parallel.iter = NA, parallel.seed = 1
    ),
    "parallel.iter must be a single positive integer value in"
  )
  expect_error(
    fviz_eig(
      res.pca, choice = "eigenvalue", parallel = TRUE,
      parallel.iter = -1, parallel.seed = 1
    ),
    "parallel.iter must be a single positive integer value in"
  )
  expect_error(
    fviz_eig(
      res.pca, choice = "eigenvalue", parallel = TRUE,
      parallel.iter = 1.5, parallel.seed = 1
    ),
    "parallel.iter must be a single positive integer value in"
  )
})

test_that("arrow.linetype controls variable-arrow linetype; default unchanged (#73)", {
  skip_if_not_installed("FactoMineR")
  res <- FactoMineR::PCA(decathlon2[1:23, 1:10], graph = FALSE)

  arrow_lt <- function(p) {
    seg <- Filter(function(l) inherits(l$geom, "GeomSegment") &&
                    !is.null(l$geom_params$arrow), p$layers)
    if (!length(seg)) return(NA_character_)
    lt <- seg[[1]]$aes_params$linetype
    if (is.null(lt)) "solid" else as.character(lt)
  }

  expect_equal(arrow_lt(fviz_pca_var(res)), "solid")                       # default
  expect_equal(arrow_lt(fviz_pca_var(res, arrow.linetype = "dashed")), "dashed")
  expect_equal(arrow_lt(fviz_pca_biplot(res, arrow.linetype = "dotted")), "dotted")
})

test_that("fviz_dend renders sub only when set; default has no subtitle (#54)", {
  hc <- hclust(dist(USArrests[1:15, ]), method = "ward.D2")

  # default: no subtitle (unchanged behavior)
  expect_null(fviz_dend(hc)$labels$subtitle)
  # explicit subtitle is rendered
  expect_equal(fviz_dend(hc, sub = "Method: ward.D2")$labels$subtitle, "Method: ward.D2")
  expect_s3_class(fviz_dend(hc, k = 3), "ggplot")
})

test_that("fviz_nbclust accepts a precomputed dist for diss-capable methods (#90)", {
  skip_if_not_installed("cluster")
  df <- scale(USArrests)
  d  <- get_dist(df, method = "euclidean")

  # diss-capable methods cluster on the dissimilarity
  expect_s3_class(fviz_nbclust(d, cluster::pam, method = "silhouette"), "ggplot")
  expect_s3_class(fviz_nbclust(d, hcut, method = "wss"), "ggplot")

  # non-diss methods are rejected with a clear error (no silent mis-clustering)
  expect_error(fviz_nbclust(d, stats::kmeans, method = "wss"), "distance matrix")
  expect_error(fviz_nbclust(d, cluster::clara, method = "silhouette"), "distance matrix")
  # gap_stat needs raw data
  expect_error(fviz_nbclust(d, cluster::pam, method = "gap_stat"), "gap_stat")

  # NO-REGRESSION: data input still works for all methods
  expect_s3_class(fviz_nbclust(df, stats::kmeans, method = "silhouette"), "ggplot")
  expect_s3_class(fviz_nbclust(df, cluster::pam, method = "silhouette", diss = d), "ggplot")
})
