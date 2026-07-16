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
