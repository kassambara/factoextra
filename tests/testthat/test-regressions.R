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

test_that("fviz_dend keeps leaf labels out of the legend (#14 sibling)", {
  hc <- hclust(dist(scale(USArrests[1:20, ])), method = "ward.D2")
  text_geoms <- c("GeomText", "GeomTextRepel", "GeomLabel", "GeomLabelRepel")
  for (p in list(fviz_dend(hc), fviz_dend(hc, k = 3, cex = 0.5))) {
    txt <- Filter(function(L) inherits(L$geom, text_geoms), p$layers)
    expect_true(all(vapply(txt, function(L) isFALSE(L$show.legend), logical(1))))
  }
})

test_that("fviz_dend rectangles match k even with tied heights (#154, #168)", {
  nrects <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1)))[1]
    nrow(ggplot2::ggplot_build(p)$data[[i]])
  }

  # seed 234: binary/ward.D2 dendrogram whose heights_per_k has duplicate names
  # (k = 2 matches >1 height) -> used to make the rect data frame longer than k,
  # erroring with "Aesthetics must be either length 1 or the same as the data".
  set.seed(234)
  m <- matrix(sample(0:1, 16 * 4, replace = TRUE), ncol = 4)
  rownames(m) <- paste0("o", seq_len(nrow(m)))
  hc <- hclust(dist(m), method = "ward.D2")

  expect_equal(nrects(fviz_dend(hc, k = 2, rect = TRUE, rect_fill = TRUE,
                                rect_border = c("red", "blue"))), 2)
  expect_equal(nrects(fviz_dend(hc, k = 4, rect = TRUE,
                                rect_border = ggpubr::get_palette("jco", 4))), 4)
  # more colours than rectangles are recycled/trimmed, not errored
  expect_equal(nrects(fviz_dend(hc, k = 3, rect = TRUE, rect_border = rainbow(6))), 3)

  # NO-REGRESSION: well-behaved dendrogram still yields exactly k rectangles
  hc2 <- hclust(dist(scale(USArrests)), method = "ward.D2")
  for (k in 2:5) expect_equal(nrects(fviz_dend(hc2, k = k, rect = TRUE)), k)
})

test_that("fviz_dend lower_rect scales for short trees; tall trees unchanged (#55)", {
  rect_ymin <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1)))[1]
    min(ggplot2::ggplot_build(p)$data[[i]]$ymin)
  }

  # short tree (max height < 1): default lower_rect is now height-relative
  set.seed(7)
  m <- matrix(rnorm(18 * 3, sd = 0.04), ncol = 3); rownames(m) <- paste0("s", 1:18)
  hc <- hclust(dist(m), "ward.D2"); mh <- max(hc$height)
  expect_lt(mh, 1)
  expect_equal(rect_ymin(fviz_dend(hc, k = 3, rect = TRUE)), -mh / 4)

  # tall tree (max height >= 1): previous default preserved (no regression)
  hc2 <- hclust(dist(scale(USArrests)), "ward.D2"); mh2 <- max(hc2$height)
  expect_equal(rect_ymin(fviz_dend(hc2, k = 4, rect = TRUE)), -(mh2 / 8 + 0.5))
})

test_that("fviz_dend match_coord_colors aligns colours to cluster labels (#103)", {
  skip_if_not_installed("dendextend")
  hc <- hclust(dist(scale(USArrests)), "ward.D2"); k <- 4
  pal <- ggpubr::get_palette("default", k)

  label_colors <- function(match) {
    dd <- attr(fviz_dend(hc, k = k, match_coord_colors = match), "dendrogram")
    gd <- dendextend::cutree(dd, k = k, order_clusters_as_data = TRUE)
    lc <- dendextend::get_leaves_branches_col(dd)
    as.vector(tapply(lc, gd[order.dendrogram(dd)], function(x) x[1])[as.character(seq_len(k))])
  }

  # opt-in: cluster label i -> palette[i], matching fviz_cluster()/fviz_silhouette()
  expect_equal(label_colors(TRUE), as.vector(pal))
  # this dataset is a known mismatch case, so the default differs from palette order
  expect_false(identical(label_colors(FALSE), as.vector(pal)))

  # NO-REGRESSION: default (FALSE) is byte-identical to omitting the argument
  expect_identical(
    dendextend::get_leaves_branches_col(attr(fviz_dend(hc, k = k), "dendrogram")),
    dendextend::get_leaves_branches_col(attr(fviz_dend(hc, k = k, match_coord_colors = FALSE), "dendrogram"))
  )
})

test_that("fviz_dend labels_font sets leaf-label font face; default unchanged (#121)", {
  hc <- hclust(dist(scale(USArrests[1:20, ])), "ward.D2")
  text_layer <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))[1]
    p$layers[[i]]
  }

  # opt-in font face applies to the leaf-label layer
  expect_equal(text_layer(fviz_dend(hc, k = 3, labels_font = "italic"))$aes_params$fontface, "italic")
  expect_equal(text_layer(fviz_dend(hc, k = 3, labels_font = "bold.italic"))$aes_params$fontface, "bold.italic")

  # NO-REGRESSION: default (and explicit "plain") leave the layer untouched
  expect_null(text_layer(fviz_dend(hc, k = 3))$aes_params$fontface)
  expect_null(text_layer(fviz_dend(hc, k = 3, labels_font = "plain"))$aes_params$fontface)

  expect_s3_class(fviz_dend(hc, k = 3, rect = TRUE, labels_font = "italic"), "ggplot")
})

test_that("alpha.var/alpha.ind fade text labels too; default unchanged (#130)", {
  res <- prcomp(iris[1:20, -5], scale. = TRUE)
  text_alpha <- function(p) {
    L <- Filter(function(l) inherits(l$geom, c("GeomText", "GeomTextRepel")), p$layers)
    vapply(L, function(l) { a <- l$aes_params$alpha; if (is.null(a)) NA_real_ else a }, numeric(1))
  }

  # labels now fade together with arrows/points
  expect_true(all(text_alpha(fviz_pca_var(res, alpha.var = 0.3)) == 0.3))
  expect_true(all(text_alpha(fviz_pca_ind(res, alpha.ind = 0.3)) == 0.3))

  # NO-REGRESSION: default (alpha = 1) keeps labels opaque
  expect_true(all(text_alpha(fviz_pca_var(res)) == 1))
  expect_true(all(text_alpha(fviz_pca_ind(res)) == 1))
  # metric-mapped alpha does not fade labels to a constant
  expect_true(all(text_alpha(fviz_pca_var(res, alpha.var = "contrib")) == 1))

  # biplot: alpha.var fades variable labels but leaves individual labels opaque
  ta <- text_alpha(fviz_pca_biplot(res, alpha.var = 0.3, label = "all"))
  expect_true(0.3 %in% ta && 1 %in% ta)
})

test_that("fviz_mclust_bic optimal-cluster line uses factor position (#116)", {
  skip_if_not_installed("mclust")
  # mclust::Mclust() calls mclustBIC() by name, which requires mclust to be
  # ATTACHED (not just namespaced). Attach it (mclust is in Suggests) and detach
  # on exit to keep the search path clean.
  suppressPackageStartupMessages(library(mclust))
  on.exit(detach("package:mclust", unload = FALSE), add = TRUE)
  vline_x <- function(p) {
    L <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)
    if (!length(L)) return(NA_real_)
    L[[1]]$data$xintercept
  }

  # restricted G range: cluster levels start at 3, so the line must sit at the
  # *position* of the optimal G, landing on the correctly-labelled tick.
  set.seed(1)
  m_restr <- Mclust(iris[, -5], G = 3:9, verbose = FALSE)
  p_restr <- fviz_mclust_bic(m_restr)
  lv <- levels(p_restr$data$cluster)
  expect_equal(vline_x(p_restr), match(as.character(m_restr$G), lv))
  expect_equal(lv[vline_x(p_restr)], as.character(m_restr$G))

  # NO-REGRESSION: standard G = 1:9 -> levels start at 1, position == G
  set.seed(1)
  m_std <- Mclust(iris[, -5], verbose = FALSE)
  expect_equal(vline_x(fviz_mclust_bic(m_std)), m_std$G)
})

test_that("fviz_pca add.circle forces/suppresses the correlation circle (#88)", {
  has_circle <- function(p) {
    any(vapply(p$layers, function(l) inherits(l$geom, c("GeomPath", "GeomCircle")),
               logical(1)))
  }
  sc <- prcomp(iris[, -5], scale. = TRUE)
  un <- prcomp(iris[, -5], scale. = FALSE)

  # NO-REGRESSION: default (NULL) keeps the auto behavior
  expect_true(has_circle(fviz_pca_var(sc)))   # scaled -> circle
  expect_false(has_circle(fviz_pca_var(un)))  # unscaled -> no circle

  # opt-in: force on a manually-scaled (scale = FALSE) fit, or suppress
  expect_true(has_circle(fviz_pca_var(un, add.circle = TRUE)))
  expect_false(has_circle(fviz_pca_var(sc, add.circle = FALSE)))
  expect_true(has_circle(fviz_pca_biplot(un, add.circle = TRUE)))
})

test_that("fviz_cluster plots diss-based pam/fanny when data is supplied (#128)", {
  skip_if_not_installed("cluster")
  df <- scale(USArrests)
  d  <- get_dist(df, method = "euclidean")

  pr <- cluster::pam(d, k = 4, diss = TRUE)
  p  <- fviz_cluster(pr, data = df, geom = "point")
  expect_s3_class(p, "ggplot")
  # cluster assignments come from the (dissimilarity-based) object, aligned per
  # observation by name (not recomputed from data=)
  pt_cluster <- function(p) {
    setNames(as.integer(as.character(p$data$cluster)), rownames(p$data))
  }
  expect_equal(pt_cluster(p)[names(pr$clustering)], pr$clustering)

  fa <- cluster::fanny(d, k = 3, diss = TRUE)
  expect_s3_class(fviz_cluster(fa, data = df, geom = "point"), "ggplot")

  # data= in a DIFFERENT row order than the dissimilarity must still colour each
  # point with its own cluster (aligned by row name), not silently mis-mapped
  dfs <- df[sample(nrow(df)), ]
  ps  <- fviz_cluster(pr, data = dfs, geom = "point")
  expect_equal(pt_cluster(ps)[names(pr$clustering)], pr$clustering)

  # data= for different observations (row names don't match) -> clear error
  expect_error(fviz_cluster(pr, data = scale(mtcars[, 1:4])), "do not match")

  # no data available -> clear, actionable error (not the cryptic NULL one)
  expect_error(fviz_cluster(pr), "dissimilarity matrix")

  # NO-REGRESSION: pam fitted on raw data (object$data present) is unchanged
  expect_s3_class(fviz_cluster(cluster::pam(df, k = 4), geom = "point"), "ggplot")
})

test_that("fviz colour/fill length-mismatch gives a clear message; valid usage works (#139)", {
  res <- prcomp(iris[, -5], scale. = TRUE)   # 150 individuals, 4 variables

  # colouring VARIABLES by an observation-level group (length 150 != 4 variables)
  expect_error(fviz_pca_var(res, col.var = iris$Species),
               "number of elements being plotted")
  expect_error(fviz_pca_ind(res, fill.ind = c("a", "b")),
               "number of elements being plotted")

  # valid usages still work (no behavior change)
  expect_s3_class(fviz_pca_var(res, col.var = "contrib"), "ggplot")
  expect_s3_class(fviz_pca_var(res, col.var = factor(c("a", "a", "b", "b"))), "ggplot")
  expect_s3_class(fviz_pca_ind(res, col.ind = iris$Species), "ggplot")
})

test_that("fviz_cluster keeps point labels out of the legend (#14 sibling)", {
  set.seed(1)
  dat <- scale(matrix(rnorm(30 * 4), nrow = 30)); rownames(dat) <- paste0("s", 1:30)
  km <- kmeans(dat, centers = 8)
  text_geoms <- c("GeomText", "GeomTextRepel", "GeomLabel", "GeomLabelRepel")
  for (p in list(fviz_cluster(list(data = dat, cluster = km$cluster)),
                 fviz_cluster(km, data = dat))) {
    txt <- Filter(function(l) inherits(l$geom, text_geoms), p$layers)
    expect_gt(length(txt), 0)
    expect_true(all(vapply(txt, function(l) isFALSE(l$show.legend), logical(1))))
  }
})

test_that("fviz_mca_biplot forwards `map` to ind and var (#142)", {
  skip_if_not_installed("FactoMineR")
  data(poison, package = "factoextra")
  res.mca <- FactoMineR::MCA(poison[, 5:15], graph = FALSE)
  co <- function(p) ggplot2::ggplot_build(p)$data[[1]][, c("x", "y")]

  sym <- co(fviz_mca_biplot(res.mca, map = "symmetric", geom = "point"))
  col <- co(fviz_mca_biplot(res.mca, map = "colprincipal", geom = "point"))

  # The bug: `map` was dropped, so every map produced identical coordinates.
  expect_false(isTRUE(all.equal(sym, col)))

  # No behavior change for the default symmetric map.
  def <- co(fviz_mca_biplot(res.mca, geom = "point"))
  expect_equal(def, sym)
})

test_that("get_pca_ind works for ade4 dudi.pca (data.frame $li) (#126)", {
  skip_if_not_installed("ade4")
  pca <- ade4::dudi.pca(iris[, -5], scannf = FALSE, nf = 3)

  ind <- get_pca_ind(pca)
  # The bug: a data.frame $li turned the cos2 matrix into a list (lost dims).
  expect_s3_class(ind, "factoextra")
  expect_true(all(c("coord", "cos2", "contrib") %in% names(ind)))
  expect_true(is.matrix(ind$coord) && is.matrix(ind$cos2) && is.matrix(ind$contrib))
  expect_equal(nrow(ind$coord), nrow(iris))
  expect_true(all(rowSums(ind$cos2) <= 1 + 1e-9))
  expect_true(all(abs(colSums(ind$contrib) - 100) < 1e-6))
  expect_s3_class(fviz_pca_ind(pca), "ggplot")
})

test_that("prcomp/princomp get_pca_ind unchanged by the dudi fix (#126 no-regression)", {
  pc <- prcomp(iris[, -5], scale. = TRUE)
  pr <- princomp(iris[, -5], cor = TRUE)
  for (ind in list(get_pca_ind(pc), get_pca_ind(pr))) {
    expect_true(is.matrix(ind$coord) && is.matrix(ind$cos2) && is.matrix(ind$contrib))
    expect_equal(nrow(ind$coord), nrow(iris))
    expect_true(all(rowSums(ind$cos2) <= 1 + 1e-9))
  }
})

test_that("ade4 between-/within-class PCA (bca/wca) are supported (#126)", {
  skip_if_not_installed("ade4")
  data(meaudret, package = "ade4")
  pca <- ade4::dudi.pca(meaudret$env, scannf = FALSE, nf = 3)
  fac <- meaudret$design$season
  bet <- ade4::bca(pca, fac, scannf = FALSE, nf = 2)
  wit <- ade4::wca(pca, fac, scannf = FALSE, nf = 2)

  for (obj in list(bet, wit)) {
    expect_equal(factoextra:::.get_facto_class(obj), "PCA")
    expect_s3_class(get_eig(obj), "data.frame")
    ind <- get_pca_ind(obj)
    var <- get_pca_var(obj)
    expect_true(all(c("coord", "cos2", "contrib") %in% names(ind)))
    expect_true(all(rowSums(ind$cos2) <= 1 + 1e-9))
    expect_true(all(c("coord", "cos2", "contrib") %in% names(var)))
    expect_s3_class(fviz_pca_ind(obj), "ggplot")
    expect_s3_class(fviz_pca_var(obj), "ggplot")
    expect_s3_class(fviz_pca_biplot(obj), "ggplot")
    expect_s3_class(fviz_eig(obj), "ggplot")

    # contributions must match ade4's own inertia.dudi (row.abs, in %)
    inr <- ade4::inertia.dudi(obj, row.inertia = TRUE)
    ade_contrib <- as.matrix(inr$row.abs[, seq_len(ncol(ind$contrib))])
    expect_equal(unname(as.matrix(ind$contrib)), unname(ade_contrib),
                 tolerance = 1e-4)
  }
})

test_that("fviz_dend honors an explicit k for HCPC, defaults to HCPC count (#81)", {
  skip_if_not_installed("FactoMineR")
  # PCA -> HCPC is the canonical, version-robust path (the FAMD+HCPC combo errors
  # internally in some FactoMineR versions). Default k is read from the result
  # rather than hard-coded, so the test is robust to the auto cluster count.
  res.pca <- FactoMineR::PCA(iris[, -5], ncp = 3, graph = FALSE)
  hc <- FactoMineR::HCPC(res.pca, nb.clust = 3, graph = FALSE)
  default_k <- length(unique(hc$data.clust$clust))

  n_branch_cols <- function(p) {
    b <- ggplot2::ggplot_build(p)$data
    cols <- unique(unlist(lapply(b, function(d)
      if ("colour" %in% names(d)) unique(d$colour))))
    length(setdiff(cols, c("black", "#000000", "grey50", "gray50")))
  }

  # default: unchanged -> HCPC cluster count
  expect_equal(n_branch_cols(fviz_dend(hc)), default_k)
  # explicit k now honored (previously ignored)
  expect_equal(n_branch_cols(fviz_dend(hc, k = 2)), 2)
  expect_equal(n_branch_cols(fviz_dend(hc, k = 4)), 4)
})

test_that("rotate.labels rotates variable labels, default off (#98)", {
  res.pca <- prcomp(iris[, -5], scale. = TRUE)
  text_geoms <- c("GeomText", "GeomTextRepel", "GeomLabel", "GeomLabelRepel")
  text_layer <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, text_geoms), logical(1)))[1]
    p$layers[[i]]
  }

  # default: no angle aesthetic (unchanged behavior)
  lay0 <- text_layer(fviz_pca_var(res.pca, repel = FALSE))
  expect_false("angle" %in% names(lay0$mapping))
  expect_false(".fviz_angle" %in% names(as.data.frame(lay0$data)))

  # rotate.labels = TRUE: angle aesthetic + per-label angle column
  lay1 <- text_layer(fviz_pca_var(res.pca, repel = FALSE, rotate.labels = TRUE))
  expect_true("angle" %in% names(lay1$mapping))
  d <- as.data.frame(lay1$data)
  expect_true(".fviz_angle" %in% names(d))
  # angles match arrow directions (flipped into the readable [-90, 90] range)
  expect_true(all(d$.fviz_angle >= -90 - 1e-6 & d$.fviz_angle <= 90 + 1e-6))

  # rotation is restricted to arrow-bearing (variable) labels: fviz_pca_ind has
  # no arrows, so rotate.labels must be a no-op there.
  lay_ind <- text_layer(fviz_pca_ind(res.pca, repel = FALSE, rotate.labels = TRUE))
  expect_false("angle" %in% names(lay_ind$mapping))

  # biplot: only the variable labels are rotated, NOT the individual labels.
  pb <- fviz_pca_biplot(res.pca, repel = FALSE, rotate.labels = TRUE)
  angled <- vapply(pb$layers, function(l)
    inherits(l$geom, text_geoms) &&
      ".fviz_angle" %in% names(as.data.frame(l$data)), logical(1))
  n_rows <- vapply(pb$layers, function(l)
    if (inherits(l$geom, text_geoms)) nrow(as.data.frame(l$data)) else NA_integer_,
    integer(1))
  expect_true(any(angled))
  # angled layer holds the variables (ncol), not the individuals (nrow of data)
  expect_true(all(n_rows[angled] == ncol(iris[, -5])))
  expect_s3_class(fviz_pca_biplot(res.pca), "ggplot")
})

test_that("shape.ind maps point shape to a second factor, default unchanged (#36, #51)", {
  res.pca <- prcomp(iris[, -5], scale. = TRUE)
  set.seed(1)
  g2 <- factor(sample(c("a", "b"), nrow(iris), TRUE))

  scales_of <- function(p) {
    b <- ggplot2::ggplot_build(p)
    unlist(lapply(b$plot$scales$scales, function(s) s$aesthetics))
  }

  # default (no shape.ind): unchanged, builds fine
  expect_s3_class(fviz_pca_ind(res.pca, habillage = iris$Species, geom = "point"), "ggplot")

  # two groupings: colour by Species, shape by g2 -> both a colour and a shape scale
  p <- fviz_pca_ind(res.pca, col.ind = iris$Species, shape.ind = g2, geom = "point")
  sc <- scales_of(p)
  expect_true("colour" %in% sc)
  expect_true("shape" %in% sc)

  # a length-mismatched shape factor errors clearly
  expect_error(fviz_pca_ind(res.pca, shape.ind = factor(c("a", "b"))),
               "number of elements being plotted")

  # biplot forwards shape.ind to individuals only and still builds
  expect_s3_class(
    fviz_pca_biplot(res.pca, col.ind = iris$Species, shape.ind = g2, geom.ind = "point"),
    "ggplot")
})

test_that("numeric pointshape path is unchanged by the shape.ind feature (#36 no-regression)", {
  res.pca <- prcomp(iris[, -5], scale. = TRUE)
  # default fviz_pca_ind with a habillage still maps shape to the habillage group
  # (no Shape. column injected; pointshape stays the auto-mapped grouping)
  p <- fviz_pca_ind(res.pca, habillage = iris$Species, geom = "point")
  has_shape_col <- any(vapply(p$layers, function(l) {
    d <- tryCatch(as.data.frame(l$data), error = function(e) data.frame())
    "Shape." %in% names(d)
  }, logical(1)))
  expect_false(has_shape_col)
})
