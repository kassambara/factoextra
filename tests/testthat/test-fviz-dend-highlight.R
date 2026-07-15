seg_data <- function(p) {
  b <- ggplot2::ggplot_build(p)
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)))[1]
  b$data[[i]]
}

test_that("fviz_dend(highlight=) emphasizes the chosen branches, layering on k colours", {
  hc <- hclust(dist(scale(USArrests)))
  labs <- labels(as.dendrogram(hc))

  p <- fviz_dend(hc, k = 4, highlight = labs[1:4], highlight.col = "red")
  cols <- seg_data(p)$colour
  # the highlight colour is drawn ...
  expect_true("red" %in% cols)
  # ... and the k cluster colours are still present (highlight layered on top)
  expect_gt(length(setdiff(unique(cols), c("red", "black"))), 1L)

  # by default the highlighted branches are thickened (2 * lwd) so emphasis
  # holds even if highlight.col collides with a cluster colour
  pd <- fviz_dend(hc, k = 4, highlight = labs[1:4], lwd = 0.7)
  expect_true(any(seg_data(pd)$linewidth == 1.4))
  # an explicit highlight.lwd is honoured
  p2 <- fviz_dend(hc, k = 4, highlight = labs[1:4], highlight.lwd = 3)
  expect_true(any(seg_data(p2)$linewidth == 3))
  # colour-only emphasis when highlight.lwd = lwd
  p3 <- fviz_dend(hc, k = 4, highlight = labs[1:4], lwd = 0.7, highlight.lwd = 0.7)
  expect_setequal(unique(seg_data(p3)$linewidth), 0.7)
})

test_that("fviz_dend(highlight=) warns for type = 'phylogenic' (branches not coloured)", {
  skip_if_not_installed("igraph")
  hc <- hclust(dist(scale(USArrests)))
  expect_warning(fviz_dend(hc, highlight = labels(as.dendrogram(hc))[1:3],
                           type = "phylogenic"), "phylogenic")
})

test_that("fviz_dend(highlight=NULL) is byte-identical to no highlight (no regression)", {
  hc <- hclust(dist(scale(USArrests)))
  a <- ggplot2::ggplot_build(fviz_dend(hc, k = 3))
  b <- ggplot2::ggplot_build(fviz_dend(hc, k = 3, highlight = NULL))
  expect_identical(a$data, b$data)
})

test_that("fviz_dend(highlight=) errors on labels not in the dendrogram", {
  hc <- hclust(dist(scale(USArrests)))
  expect_error(fviz_dend(hc, highlight = c("Alabama", "NotAState")),
               "not in the dendrogram")
})
