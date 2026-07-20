# Edge-case correctness and input-validation guards (PR #274).
# Batch B populates the additive-validation blocks; later batches add the
# numeric-correctness blocks (PCA metric stability, Hopkins sampling, etc.).

test_that("embedding dimensions require distinct finite positive integers", {
  x <- matrix(rnorm(30), nrow = 10, ncol = 3)
  for(dims in list(c(1, 1), c(1.5, 2), c(1, Inf), c(1, 3e9), c(0, 1), 1:3)){
    expect_error(factoextra:::.fe_layout(x, dims),
                 "two distinct positive integer")
  }
  expect_equal(
    factoextra:::.fe_layout(x, c(1, 3)), x[, c(1, 3), drop = FALSE],
    ignore_attr = TRUE
  )
})

test_that("fviz_nbclust validates k.max as a single integer >= 2", {
  x <- scale(iris[1:20, 1:4])
  for(k_max in list(1, NA_real_, Inf, 2.5, c(2, 3))){
    expect_error(
      fviz_nbclust(x, stats::kmeans, method = "wss", k.max = k_max),
      "k.max must be a single integer value"
    )
  }
  expect_s3_class(
    fviz_nbclust(x, stats::kmeans, method = "wss", k.max = 2, nstart = 2),
    "ggplot"
  )
})

test_that("fviz_dend validates k against the number of leaves", {
  hc <- hclust(dist(matrix(seq_len(15), nrow = 5)))
  expect_s3_class(fviz_dend(hc, k = 5, rect = TRUE), "ggplot")
  expect_error(fviz_dend(hc, k = 6), "in \\[1, 5\\]")
  expect_error(fviz_dend(hc, k = 2.5), "single integer value")
  expect_error(fviz_dend(hc, k = NA_real_), "single integer value")
  expect_error(fviz_dend(hc, k = 1, rect = TRUE), "in \\[2, 5\\]")
  expect_s3_class(fviz_dend(hc, k = 1), "ggplot")
})

# ---- Batch C (PR #274) scoped bug fixes -------------------------------------

test_that("fviz_cluster aligns exact named assignments, falls back to positional", {
  set.seed(901)
  x <- scale(USArrests)
  km <- kmeans(x, centers = 3, nstart = 10)
  reordered <- x[rev(seq_len(nrow(x))), , drop = FALSE]

  # Exact, unique, matching names on both sides -> aligned by name.
  p <- fviz_cluster(km, data = reordered, stand = FALSE, geom = "point",
                    ellipse = FALSE, show.clust.cent = FALSE)
  got <- setNames(as.character(p$data$cluster), p$data$name)
  expect_equal(unname(got[rownames(reordered)]),
               as.character(km$cluster[rownames(reordered)]))

  fake_dbscan <- structure(list(cluster = km$cluster), class = "dbscan")
  db <- fviz_cluster(
    fake_dbscan, data = reordered, stand = FALSE, geom = "point",
    ellipse = FALSE, show.clust.cent = FALSE
  )
  db_got <- setNames(as.character(db$data$cluster), db$data$name)
  expect_equal(
    unname(db_got[rownames(reordered)]),
    as.character(km$cluster[rownames(reordered)])
  )

  # Non-regression: names that do not line up are used positionally (they still
  # plot, as they did before this change) rather than erroring.
  mismatched <- reordered; rownames(mismatched)[1] <- "not-an-observation"
  expect_s3_class(fviz_cluster(km, data = mismatched, stand = FALSE,
                               geom = "point", ellipse = FALSE), "ggplot")
  duplicated <- reordered; rownames(duplicated)[1] <- rownames(duplicated)[2]
  expect_s3_class(fviz_cluster(km, data = duplicated, stand = FALSE,
                               geom = "point", ellipse = FALSE), "ggplot")
  partially_named <- km; names(partially_named$cluster)[1] <- ""
  expect_s3_class(fviz_cluster(partially_named, data = reordered, stand = FALSE,
                               geom = "point", ellipse = FALSE), "ggplot")

  # A genuine length mismatch is still a clear error.
  short <- list(data = x[1:4, 1:2, drop = FALSE], cluster = 1:3)
  expect_error(fviz_cluster(short, stand = FALSE), "do not match in length")

  # `clustering` is accepted; when both are present, `cluster` wins.
  both <- list(data = x[, 1:2, drop = FALSE],
               cluster = setNames(rep(1L, nrow(x)), rownames(x)),
               clustering = setNames(rep(2L, nrow(x)), rownames(x)))
  both_plot <- fviz_cluster(both, stand = FALSE, geom = "point", ellipse = FALSE,
                            show.clust.cent = FALSE)
  expect_true(all(as.character(both_plot$data$cluster) == "1"))

  clustering_only <- list(data = x[, 1:2, drop = FALSE],
                          clustering = setNames(rep(3L, nrow(x)), rownames(x)))
  expect_s3_class(fviz_cluster(clustering_only, stand = FALSE, geom = "point",
                               ellipse = FALSE), "ggplot")
})

test_that("dissimilarity partition plots reject complete mismatched row names", {
  set.seed(902)
  x <- scale(USArrests)
  fit <- cluster::pam(stats::dist(x), 3)
  order <- sample(seq_len(nrow(x)))
  reordered <- x[order, , drop = FALSE]

  aligned <- fviz_cluster(
    fit, data = reordered, stand = FALSE, geom = "point",
    ellipse = FALSE, show.clust.cent = FALSE
  )
  got <- setNames(as.character(aligned$data$cluster), aligned$data$name)
  expect_equal(
    unname(got[rownames(reordered)]),
    as.character(fit$clustering[rownames(reordered)])
  )

  rownames(reordered) <- paste0("unknown-", seq_len(nrow(reordered)))
  expect_error(
    fviz_cluster(fit, data = reordered, stand = FALSE, geom = "point"),
    "row names of data do not match"
  )

  hfit <- hcut(stats::dist(x), k = 3)
  aligned_hcut <- fviz_cluster(
    hfit, data = x[order, , drop = FALSE], stand = FALSE,
    geom = "point", ellipse = FALSE, show.clust.cent = FALSE
  )
  hgot <- setNames(as.character(aligned_hcut$data$cluster),
                   aligned_hcut$data$name)
  expect_equal(
    unname(hgot[rownames(x)[order]]),
    as.character(hfit$cluster[rownames(x)[order]])
  )

  hbad <- x
  rownames(hbad) <- paste0("unknown-", seq_len(nrow(hbad)))
  expect_error(
    fviz_cluster(hfit, data = hbad, stand = FALSE, geom = "point"),
    "row names of data do not match"
  )
})

test_that("supplementary CA columns and invisible = all use the right flags", {
  lab <- factoextra:::.label("all")
  visible <- factoextra:::.define_element_sup(
    structure(list(), class = "CA"), "col", "point", lab,
    factoextra:::.hide("none")
  )
  hidden <- factoextra:::.define_element_sup(
    structure(list(), class = "CA"), "col", "point", lab,
    factoextra:::.hide("col.sup")
  )
  expect_identical(visible$name, "col.sup")
  expect_null(hidden$name)
  expect_true(all(unlist(factoextra:::.hide("all"))))

  pca <- prcomp(iris[, -5], scale. = TRUE)
  p <- fviz_pca_ind(pca, invisible = "all")
  primary_geoms <- vapply(p$layers, function(layer) {
    inherits(layer$geom, c("GeomPoint", "GeomText", "GeomTextRepel"))
  }, logical(1))
  expect_false(any(primary_geoms))
})

test_that("selection warns about unmatched names without dropping matches", {
  # The plot path warns (esup-aware), keeping the matched names.
  pca <- prcomp(iris[, -5], scale. = TRUE)
  expect_warning(
    p <- fviz_pca_ind(pca, select.ind = list(name = c("1", "typo")), geom = "point"),
    "Selection name.*typo"
  )
  expect_true("1" %in% as.character(p$data$name))
  expect_false("typo" %in% as.character(p$data$name))

  # .select itself is silent by default (no leak into fviz_contrib/fviz_cos2),
  # and warns only when explicitly asked (warn_unmatched = TRUE).
  d <- data.frame(name = c("a", "b", "c"), cos2 = c(0.9, 0.2, 0.8),
                  contrib = c(5, 3, 1), row.names = c("a", "b", "c"))
  expect_silent(factoextra:::.select(d, list(name = c("a", "typo"))))
  expect_warning(
    selected <- factoextra:::.select(d, list(name = c("a", "typo")),
                                     warn_unmatched = TRUE),
    "Selection name.*typo"
  )
  expect_identical(selected$name, "a")

  expect_warning(
    union <- factoextra:::.select(
      d, list(name = "typo", cos2 = 0.8, union = TRUE), check = TRUE,
      warn_unmatched = TRUE
    ),
    "Selection name.*typo"
  )
  expect_identical(union$name, c("a", "c"))
  expect_silent(factoextra:::.select(
    d, list(name = "typo"), check = FALSE
  ))
})

test_that("PCA supplementary quantitative names are valid selections", {
  skip_if_not_installed("FactoMineR")
  pca <- FactoMineR::PCA(iris[, 1:4], quanti.sup = 4, graph = FALSE)
  expect_no_warning(
    p <- fviz_pca_var(pca, select.var = list(name = "Petal.Width"))
  )
  layer_names <- unique(unlist(lapply(p$layers, function(layer){
    data <- as.data.frame(layer$data)
    if("name" %in% names(data)) as.character(data$name) else rownames(data)
  })))
  expect_true("Petal.Width" %in% layer_names)
})

test_that("fviz_gap_stat maxSE fallback uses firstSEmax (honours SE.factor)", {
  fake_gap <- structure(list(Tab = cbind(
    gap = c(1, 1.5, 1.4), SE.sim = c(0.1, 0.6, 0.1)
  )), class = "clusGap")
  vline <- function(p){
    layer <- Filter(function(x) inherits(x$geom, "GeomVline"), p$layers)[[1]]
    layer$data$xintercept
  }
  # default call: firstSEmax (unchanged)
  expect_equal(vline(fviz_gap_stat(fake_gap)), 1)
  # partial list without method: NOW falls back to firstSEmax (was firstmax -> 2)
  expect_equal(vline(fviz_gap_stat(fake_gap, maxSE = list(SE.factor = 1))), 1)
  # explicit method is still honoured
  expect_equal(vline(fviz_gap_stat(
    fake_gap, maxSE = list(method = "firstmax", SE.factor = 1)
  )), 2)
})

# ---- Batch D1 (PR #274) get_pca contribution normalization ----------------

test_that("PCA metrics remain finite and scale-invariant on tiny or zero axes", {
  x <- as.matrix(iris[, 1:4])
  ordinary <- get_pca_ind(prcomp(x, center = TRUE, scale. = FALSE))
  tiny <- get_pca_ind(prcomp(x * 1e-10, center = TRUE, scale. = FALSE))
  subnormal_fit <- prcomp(x * 1e-170, center = TRUE, scale. = FALSE)
  subnormal_square <- get_pca_ind(subnormal_fit)

  expect_equal(tiny$cos2, ordinary$cos2, tolerance = 1e-12)
  expect_equal(tiny$contrib, ordinary$contrib, tolerance = 1e-12)
  expect_equal(subnormal_square$cos2, ordinary$cos2, tolerance = 1e-12)
  expect_equal(subnormal_square$contrib, ordinary$contrib, tolerance = 1e-12)
  expect_equal(unname(colSums(tiny$contrib)), rep(100, ncol(x)), tolerance = 1e-10)
  expect_equal(
    unname(colSums(subnormal_square$contrib)),
    rep(100, ncol(x)), tolerance = 1e-10
  )
  expect_equal(
    get_pca_var(subnormal_fit)$contrib,
    get_pca_var(prcomp(x, center = TRUE, scale. = FALSE))$contrib,
    tolerance = 1e-12
  )

  rank_deficient <- prcomp(cbind(signal = seq_len(8), constant = 1))
  ind <- get_pca_ind(rank_deficient)
  var <- get_pca_var(rank_deficient)
  expect_true(all(is.finite(ind$cos2)))
  expect_true(all(is.finite(ind$contrib)))
  expect_true(all(is.finite(var$contrib)))
  expect_equal(unname(var$contrib[, 2]), c(0, 0))
})

test_that("PCA metrics preserve excluded rows without corrupting complete shares", {
  x <- iris[, 1:4]
  x[c(2, 11), 1] <- NA_real_
  fit <- prcomp(
    ~ ., data = x, center = TRUE, scale. = TRUE,
    na.action = na.exclude
  )
  ind <- get_pca_ind(fit)

  expect_true(all(is.na(ind$cos2[c(2, 11), ])))
  expect_true(all(is.na(ind$contrib[c(2, 11), ])))
  expect_true(all(is.finite(ind$cos2[-c(2, 11), ])))
  expect_equal(
    unname(colSums(ind$contrib, na.rm = TRUE)),
    rep(100, ncol(ind$contrib)), tolerance = 1e-10
  )
})


# ---- Batch D3 (PR #274) Hopkins sampling ----------------------------------

test_that("Hopkins sampling is finite, duplicate-aware, and fail-closed", {
  old_warn <- getOption("factoextra.warn_hopkins", TRUE)
  old_cells <- getOption("factoextra.hopkins.max_matrix_cells", 2e7)
  on.exit(options(factoextra.warn_hopkins = old_warn,
                  factoextra.hopkins.max_matrix_cells = old_cells), add = TRUE)
  options(factoextra.warn_hopkins = FALSE)

  for(limit in c(1, 1e9)){
    options(factoextra.hopkins.max_matrix_cells = limit)
    out <- get_clust_tendency(iris[, 1:4], n = 1, graph = FALSE, seed = 4)
    expect_true(is.finite(out$hopkins_stat))
    expect_true(out$hopkins_stat >= 0 && out$hopkins_stat <= 1)
  }

  set.seed(904)
  scale_probe <- matrix(runif(80, min = -2, max = 3), ncol = 4)
  reference <- get_clust_tendency(
    scale_probe, n = 5, graph = FALSE, seed = 11
  )$hopkins_stat
  for(multiplier in c(1e-200, 1e200)){
    for(show_graph in c(FALSE, TRUE)){
      scaled <- get_clust_tendency(
        scale_probe * multiplier, n = 5, graph = show_graph, seed = 11
      )
      expect_equal(scaled$hopkins_stat, reference, tolerance = 1e-12)
      if(show_graph) expect_s3_class(scaled$plot, "ggplot")
    }
  }

  for(limit in c(1, 1e9)){
    options(factoextra.hopkins.max_matrix_cells = limit)
    baseline <- get_clust_tendency(
      iris[, 1:4], n = 30, graph = FALSE, seed = 7
    )$hopkins_stat
    augmented <- get_clust_tendency(
      cbind(iris[, 1:4], constant = 7), n = 30, graph = FALSE, seed = 7
    )$hopkins_stat
    expect_identical(augmented, baseline)
  }

  expect_error(
    get_clust_tendency(matrix(c(1, Inf, 2, 3), ncol = 2), 1,
                        graph = FALSE),
    "finite values"
  )
  expect_error(get_clust_tendency(iris[, 1:4], Inf, graph = FALSE),
               "positive integer")
  expect_error(
    get_clust_tendency(matrix(1, nrow = 4, ncol = 2), 2,
                        graph = FALSE, seed = 1),
    "undefined"
  )

  duplicate_data <- matrix(c(0, 0, 1, 1), ncol = 1)
  duplicate_out <- get_clust_tendency(
    duplicate_data, n = 2, graph = FALSE, seed = 8
  )
  expect_equal(duplicate_out$hopkins_stat, 1)
  set.seed(903)
  sampled <- factoextra:::.sample_hopkins_rows(10, 9)
  expect_length(sampled, 9)
  expect_equal(length(unique(sampled)), 9)

  set.seed(902)
  high_dimensional <- matrix(rnorm(20 * 200, sd = 1e6), nrow = 20)
  high_dimensional_out <- get_clust_tendency(
    high_dimensional, n = 5, graph = FALSE, seed = 9
  )
  expect_true(is.finite(high_dimensional_out$hopkins_stat))
})

test_that("Hopkins statistic matches an independent computation (cross-validation)", {
  old <- getOption("factoextra.warn_hopkins", TRUE)
  on.exit(options(factoextra.warn_hopkins = old), add = TRUE)
  options(factoextra.warn_hopkins = FALSE)

  # Independent reference: matched sample points (same seed), but nearest-neighbour
  # distances computed by a brute-force loop rather than the package's vectorized
  # squared-distance path. A modest tolerance absorbs floating-point accumulation
  # differences; a formula error (wrong exponent/normalization) would be far larger.
  indep <- function(X, n, seed){
    X <- as.matrix(stats::na.omit(X)); N <- nrow(X); d <- ncol(X)
    set.seed(seed)
    mins <- apply(X, 2, min); maxs <- apply(X, 2, max)
    p <- matrix(NA_real_, n, d)
    for(j in seq_len(d)) p[, j] <- runif(n, mins[j], maxs[j])
    k <- sample.int(N, n, replace = FALSE)
    minp <- apply(p, 1, function(pt) sqrt(min(colSums((t(X) - pt)^2))))
    minq <- vapply(k, function(i){
      dd <- sqrt(colSums((t(X) - X[i, ])^2)); dd[i] <- Inf; min(dd)
    }, numeric(1))
    sum(minp^d) / (sum(minp^d) + sum(minq^d))
  }
  for(cfg in list(list(iris[, -5], 10L, 1L), list(scale(USArrests), 8L, 7L))){
    X <- cfg[[1]]; n <- cfg[[2]]; s <- cfg[[3]]
    got <- get_clust_tendency(X, n = n, graph = FALSE, seed = s)$hopkins_stat
    expect_equal(got, indep(X, n, s), tolerance = 1e-3)
  }
})
