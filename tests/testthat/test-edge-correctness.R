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
