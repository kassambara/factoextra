test_that("fviz_cluster aligns exact named assignments and rejects ambiguity", {
  set.seed(901)
  x <- scale(USArrests)
  km <- kmeans(x, centers = 3, nstart = 10)
  reordered <- x[rev(seq_len(nrow(x))), , drop = FALSE]

  p <- fviz_cluster(
    km, data = reordered, stand = FALSE, geom = "point",
    ellipse = FALSE, show.clust.cent = FALSE
  )
  got <- setNames(as.character(p$data$cluster), p$data$name)
  expect_equal(
    unname(got[rownames(reordered)]),
    as.character(km$cluster[rownames(reordered)])
  )

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

  mismatched <- reordered
  rownames(mismatched)[1] <- "not-an-observation"
  expect_error(fviz_cluster(km, data = mismatched), "do not match")

  duplicated <- reordered
  rownames(duplicated)[1] <- rownames(duplicated)[2]
  expect_error(fviz_cluster(km, data = duplicated), "must be unique")

  partially_named <- km
  names(partially_named$cluster)[1] <- ""
  expect_error(
    fviz_cluster(partially_named, data = reordered),
    "complete and non-empty"
  )

  short <- list(data = x[1:4, 1:2, drop = FALSE], cluster = 1:3)
  expect_error(fviz_cluster(short, stand = FALSE), "do not match in length")

  both <- list(
    data = x[, 1:2, drop = FALSE],
    cluster = setNames(rep(1L, nrow(x)), rownames(x)),
    clustering = setNames(rep(2L, nrow(x)), rownames(x))
  )
  both_plot <- fviz_cluster(
    both, stand = FALSE, geom = "point", ellipse = FALSE,
    show.clust.cent = FALSE
  )
  expect_true(all(as.character(both_plot$data$cluster) == "1"))
})

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

test_that("embedding dimensions require distinct finite positive integers", {
  x <- matrix(rnorm(30), nrow = 10, ncol = 3)
  for(dims in list(c(1, 1), c(1.5, 2), c(1, Inf), c(0, 1), 1:3)){
    expect_error(factoextra:::.fe_layout(x, dims),
                 "two distinct positive integer")
  }
  expect_equal(
    factoextra:::.fe_layout(x, c(1, 3)), x[, c(1, 3), drop = FALSE],
    ignore_attr = TRUE
  )
})

test_that("k.max and partial maxSE lists honor their documented contracts", {
  x <- scale(iris[1:20, 1:4])
  for(k_max in list(1, NA_real_, Inf, 2.5, c(2, 3))){
    expect_error(
      fviz_nbclust(x, stats::kmeans, method = "wss", k.max = k_max),
      "k.max must be a single integer value"
    )
  }
  expect_s3_class(
    fviz_nbclust(x, stats::kmeans, method = "wss", k.max = 2,
                 nstart = 2),
    "ggplot"
  )

  fake_gap <- structure(list(Tab = cbind(
    gap = c(1, 1.5, 1.4), SE.sim = c(0.1, 0.6, 0.1)
  )), class = "clusGap")
  vline <- function(p){
    layer <- Filter(function(x) inherits(x$geom, "GeomVline"), p$layers)[[1]]
    layer$data$xintercept
  }
  expect_equal(vline(fviz_gap_stat(fake_gap)), 1)
  expect_equal(vline(fviz_gap_stat(
    fake_gap, maxSE = list(SE.factor = 1)
  )), 1)
  expect_equal(vline(fviz_gap_stat(
    fake_gap, maxSE = list(method = "firstmax", SE.factor = 1)
  )), 2)
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

test_that("selection warns once about unmatched names without dropping matches", {
  d <- data.frame(
    name = c("a", "b", "c"), cos2 = c(0.9, 0.2, 0.8),
    contrib = c(5, 3, 1), row.names = c("a", "b", "c")
  )
  expect_warning(
    selected <- factoextra:::.select(
      d, list(name = c("a", "typo")), check = TRUE
    ),
    "Selection name.*typo"
  )
  expect_identical(selected$name, "a")

  expect_warning(
    union <- factoextra:::.select(
      d, list(name = "typo", cos2 = 0.8, union = TRUE), check = TRUE
    ),
    "Selection name.*typo"
  )
  expect_identical(union$name, c("a", "c"))
  expect_silent(factoextra:::.select(
    d, list(name = "typo"), check = FALSE
  ))
})
