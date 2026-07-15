test_that("PCA/MCA visualization helpers return ggplot objects", {
  skip_if_not_installed("FactoMineR")

  pca <- FactoMineR::PCA(iris[, 1:4], graph = FALSE)
  p_ind <- fviz_pca_ind(pca)
  p_var <- fviz_pca_var(pca)

  expect_s3_class(p_ind, "ggplot")
  expect_s3_class(p_var, "ggplot")

  set.seed(123)
  mca_df <- data.frame(
    a = factor(sample(letters[1:3], 40, TRUE)),
    b = factor(sample(LETTERS[1:3], 40, TRUE)),
    c = factor(sample(c("x", "y"), 40, TRUE)),
    sup_q = factor(sample(c("q1", "q2"), 40, TRUE)),
    sup_n = rnorm(40)
  )
  mca <- FactoMineR::MCA(mca_df, quali.sup = 4, quanti.sup = 5, graph = FALSE)
  p_mca <- fviz_mca_var(mca, choice = "mca.cor")
  expect_s3_class(p_mca, "ggplot")
})

test_that("cluster visual helpers work on hcut/hkmeans outputs", {
  x <- scale(USArrests)

  hc <- hcut(x, k = 4)
  km <- hkmeans(x, k = 4)

  p1 <- fviz_cluster(hc)
  p2 <- fviz_cluster(km)
  p3 <- fviz_silhouette(hc, print.summary = FALSE)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("fviz_dend supports rectangular dendrogram layout", {
  hc <- hclust(dist(iris[, 1:4]))
  p <- fviz_dend(hc, k = 3, rect = TRUE, show_labels = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("fviz_dend supports circular and horizontal dendrogram layouts", {
  # Exercises the coord_polar (circular) and coord_flip (horizontal) branches of
  # .ggplot_dend. These layout+theme paths were otherwise untested, leaving the
  # ggplot2-dev CI leg blind to a break isolated there.
  hc <- hclust(dist(scale(USArrests)))
  p_circular <- fviz_dend(hc, k = 3, type = "circular")
  p_horiz <- fviz_dend(hc, k = 3, horiz = TRUE)

  expect_s3_class(p_circular, "ggplot")
  expect_s3_class(p_horiz, "ggplot")
  # Force the layout transforms to run (coord_polar / coord_flip fire at build).
  expect_no_error(ggplot2::ggplot_build(p_circular))
  expect_no_error(ggplot2::ggplot_build(p_horiz))
})

test_that("fviz_dend applies lwd to branch segments without adding a linewidth guide", {
  hc <- hclust(dist(scale(USArrests)))

  p_default <- fviz_dend(hc)
  p_custom <- fviz_dend(hc, lwd = 2)

  default_layers <- ggplot2::ggplot_build(p_default)
  custom_layers <- ggplot2::ggplot_build(p_custom)
  legend_labels <- function(plot) {
    gt <- ggplot2::ggplotGrob(plot)
    idx <- which(gt$layout$name == "guide-box-right")
    if(length(idx) == 0) return(character(0))
    legend <- gt$grobs[[idx[1]]]$grobs[[1]]
    extract_labels <- function(grob) {
      out <- character()
      if(inherits(grob, "text")) out <- c(out, grob$label)
      if(!is.null(grob$children)) {
        for(child in grob$children) out <- c(out, extract_labels(child))
      }
      if(!is.null(grob$grobs)) {
        for(child in grob$grobs) out <- c(out, extract_labels(child))
      }
      out
    }
    unique(Filter(nzchar, extract_labels(legend)))
  }

  expect_equal(unique(default_layers$data[[1]]$linewidth), 0.7)
  expect_equal(unique(custom_layers$data[[1]]$linewidth), 2)
  expect_false("lwd" %in% legend_labels(p_custom))
})

test_that("fviz_pca_biplot supports form and covariance scaling modes", {
  res.pca <- stats::prcomp(iris[, 1:4], scale. = TRUE)
  p_form <- fviz_pca_biplot(res.pca, biplot.type = "form")
  p_cov <- fviz_pca_biplot(res.pca, biplot.type = "covariance")
  expect_s3_class(p_form, "ggplot")
  expect_s3_class(p_cov, "ggplot")
})

test_that("fviz_eig parallel analysis is reproducible with parallel.seed", {
  res.pca <- stats::prcomp(iris[, 1:4], scale. = TRUE)

  extract_parallel_threshold <- function(p) {
    layers <- ggplot2::ggplot_build(p)$data
    idx <- which(vapply(layers, function(df) {
      "shape" %in% names(df) && all(df$shape == 4)
    }, logical(1)))
    if(length(idx) == 0) return(numeric(0))
    layers[[idx[1]]]$y
  }

  p1 <- fviz_eig(
    res.pca, choice = "eigenvalue", parallel = TRUE,
    parallel.iter = 20, parallel.seed = 42
  )
  p2 <- fviz_eig(
    res.pca, choice = "eigenvalue", parallel = TRUE,
    parallel.iter = 20, parallel.seed = 42
  )
  p3 <- fviz_eig(
    res.pca, choice = "eigenvalue", parallel = TRUE,
    parallel.iter = 20, parallel.seed = 43
  )

  th1 <- extract_parallel_threshold(p1)
  th2 <- extract_parallel_threshold(p2)
  th3 <- extract_parallel_threshold(p3)
  expect_true(length(th1) > 0)
  expect_equal(th1, th2)
  expect_false(isTRUE(all.equal(th1, th3)))
})

test_that("fviz_famd_var supports supplementary qualitative categories", {
  skip_if_not_installed("FactoMineR")

  set.seed(321)
  famd_df <- data.frame(
    cat_a = factor(sample(c("a", "b", "c"), 60, TRUE)),
    cat_b = factor(sample(c("x", "y"), 60, TRUE)),
    num_a = rnorm(60),
    num_b = rnorm(60),
    sup_q = factor(sample(c("north", "south"), 60, TRUE))
  )
  res.famd <- FactoMineR::FAMD(famd_df, sup.var = 5, graph = FALSE)

  p_sup <- fviz_famd_var(res.famd, choice = "quali.sup", repel = TRUE, col.var = "cos2")
  p_overlay <- fviz_famd_var(res.famd, choice = "quali.var", invisible = "var", repel = TRUE)

  expect_s3_class(p_sup, "ggplot")
  expect_s3_class(p_overlay, "ggplot")
  expect_gt(length(p_overlay$layers), 1)
})

test_that("fviz_famd_ind can show supplementary qualitative categories without active ones", {
  skip_if_not_installed("FactoMineR")

  set.seed(4242)
  famd_df <- data.frame(
    cat_a = factor(sample(c("a", "b", "c"), 60, TRUE)),
    num_a = rnorm(60),
    num_b = rnorm(60),
    sup_q = factor(sample(c("north", "south"), 60, TRUE))
  )
  res.famd <- FactoMineR::FAMD(famd_df, sup.var = 4, graph = FALSE)

  p <- fviz_famd_ind(res.famd, invisible = "quali.var", repel = TRUE)
  layer_rows <- vapply(ggplot2::ggplot_build(p)$data, nrow, integer(1))

  expect_s3_class(p, "ggplot")
  expect_true(any(layer_rows == nlevels(famd_df$sup_q)))
})

test_that("fviz_famd_ind overlays active and supplementary qualitative categories together", {
  skip_if_not_installed("FactoMineR")

  set.seed(4243)
  famd_df <- data.frame(
    cat_a = factor(sample(c("a", "b", "c"), 60, TRUE)),
    num_a = rnorm(60),
    num_b = rnorm(60),
    sup_q = factor(sample(c("north", "south"), 60, TRUE))
  )
  res.famd <- FactoMineR::FAMD(famd_df, sup.var = 4, graph = FALSE)

  p <- fviz_famd_ind(res.famd, repel = TRUE)
  layer_rows <- vapply(ggplot2::ggplot_build(p)$data, nrow, integer(1))

  expect_s3_class(p, "ggplot")
  expect_true(any(layer_rows == nrow(res.famd$quali.var$coord)))
  expect_true(any(layer_rows == nlevels(famd_df$sup_q)))
})

test_that("fviz_famd_ind respects multi-element invisible vectors", {
  skip_if_not_installed("FactoMineR")

  set.seed(4343)
  famd_df <- data.frame(
    cat_a = factor(sample(c("a", "b", "c"), 60, TRUE)),
    num_a = rnorm(60),
    num_b = rnorm(60),
    sup_q = factor(sample(c("north", "south"), 60, TRUE))
  )
  res.famd <- FactoMineR::FAMD(famd_df, sup.var = 4, graph = FALSE)

  p <- fviz_famd_ind(res.famd, invisible = c("quali.var", "ind"), repel = TRUE)
  layer_rows <- vapply(ggplot2::ggplot_build(p)$data, nrow, integer(1))

  expect_s3_class(p, "ggplot")
  expect_true(any(layer_rows == nlevels(famd_df$sup_q)))
  expect_false(any(layer_rows == nrow(famd_df)))
})

test_that("fviz_mfa_var supports supplementary qualitative categories", {
  skip_if_not_installed("FactoMineR")

  data(poison, package = "FactoMineR")
  res.mfa <- FactoMineR::MFA(
    poison,
    group = c(2, 2, 5, 6),
    type = c("s", "n", "n", "n"),
    name.group = c("desc", "desc2", "symptom", "eat"),
    num.group.sup = 1:2,
    graph = FALSE
  )

  p_sup <- fviz_mfa_var(res.mfa, choice = "quali.sup", repel = TRUE, col.var = "cos2")
  p_overlay <- fviz_mfa_var(res.mfa, choice = "quali.var", invisible = "quali.var", repel = TRUE)
  p_biplot <- fviz_mfa_quali_biplot(res.mfa, repel = TRUE)
  p_biplot_default <- fviz_mfa_quali_biplot(res.mfa)
  p_biplot_hidden <- fviz_mfa_quali_biplot(res.mfa, repel = TRUE, invisible = "ind")
  p_biplot_choice <- fviz_mfa_quali_biplot(res.mfa, choice = "group")
  p_biplot_ggp <- fviz_mfa_quali_biplot(res.mfa, ggp = ggplot2::ggplot())

  overlay_rows <- vapply(ggplot2::ggplot_build(p_overlay)$data, nrow, integer(1))
  biplot_rows <- vapply(ggplot2::ggplot_build(p_biplot)$data, nrow, integer(1))
  biplot_default_rows <- vapply(ggplot2::ggplot_build(p_biplot_default)$data, nrow, integer(1))
  biplot_choice_rows <- vapply(ggplot2::ggplot_build(p_biplot_choice)$data, nrow, integer(1))
  biplot_ggp_rows <- vapply(ggplot2::ggplot_build(p_biplot_ggp)$data, nrow, integer(1))
  sup_rows <- vapply(ggplot2::ggplot_build(p_sup)$data, nrow, integer(1))
  sup_n <- nrow(res.mfa$quali.var.sup$coord)

  expect_s3_class(p_sup, "ggplot")
  expect_s3_class(p_overlay, "ggplot")
  expect_s3_class(p_biplot, "ggplot")
  expect_s3_class(p_biplot_default, "ggplot")
  expect_s3_class(p_biplot_hidden, "ggplot")
  expect_s3_class(p_biplot_choice, "ggplot")
  expect_s3_class(p_biplot_ggp, "ggplot")
  expect_true(any(overlay_rows == sup_n))
  expect_equal(sum(biplot_rows == sup_n), sum(sup_rows == sup_n))
  expect_equal(sum(biplot_default_rows == sup_n), sum(sup_rows == sup_n))
  expect_equal(sum(biplot_choice_rows == sup_n), sum(sup_rows == sup_n))
  expect_equal(sum(biplot_ggp_rows == sup_n), sum(sup_rows == sup_n))
})

test_that("fviz_mfa_ind respects multi-element invisible vectors for partial plots", {
  skip_if_not_installed("FactoMineR")

  data(poison, package = "FactoMineR")
  res.mfa <- FactoMineR::MFA(
    poison,
    group = c(2, 2, 5, 6),
    type = c("s", "n", "n", "n"),
    name.group = c("desc", "desc2", "symptom", "eat"),
    num.group.sup = 1:2,
    graph = FALSE
  )

  p <- fviz_mfa_ind(res.mfa, partial = "all", invisible = c("quali.var", "ind"), repel = TRUE)
  layer_rows <- vapply(ggplot2::ggplot_build(p)$data, nrow, integer(1))
  partial_n <- nrow(facto_summarize(
    res.mfa, element = "ind",
    result = c("coord", "contrib", "cos2", "coord.partial"),
    axes = 1:2
  )$res.partial)

  expect_s3_class(p, "ggplot")
  expect_false(any(layer_rows == partial_n))
})

test_that("fviz_hmfa_ind respects multi-element invisible vectors for partial plots", {
  skip_if_not_installed("FactoMineR")

  data(wine, package = "FactoMineR")
  hierar <- list(c(2, 5, 3, 10, 9, 2), c(4, 2))
  res.hmfa <- FactoMineR::HMFA(wine, H = hierar, type = c("n", rep("s", 5)), graph = FALSE)

  p <- fviz_hmfa_ind(res.hmfa, partial = "all", invisible = c("quali.var", "ind"), repel = TRUE)
  layer_rows <- vapply(ggplot2::ggplot_build(p)$data, nrow, integer(1))
  partial_n <- nrow(facto_summarize(
    res.hmfa, element = "partial.node", node.level = 1,
    group.names = rownames(res.hmfa$group$coord[[1]]),
    result = c("coord.node.partial"), axes = 1:2
  ))

  expect_s3_class(p, "ggplot")
  expect_false(any(layer_rows == partial_n))
})

test_that("quali.sup plots reject contrib-based selection cleanly", {
  skip_if_not_installed("FactoMineR")

  set.seed(5151)
  famd_df <- data.frame(
    cat_a = factor(sample(c("a", "b", "c"), 60, TRUE)),
    cat_b = factor(sample(c("x", "y"), 60, TRUE)),
    num_a = rnorm(60),
    num_b = rnorm(60),
    sup_q = factor(sample(c("north", "south"), 60, TRUE))
  )
  res.famd <- FactoMineR::FAMD(famd_df, sup.var = 5, graph = FALSE)

  data(poison, package = "FactoMineR")
  res.mfa <- FactoMineR::MFA(
    poison,
    group = c(2, 2, 5, 6),
    type = c("s", "n", "n", "n"),
    name.group = c("desc", "desc2", "symptom", "eat"),
    num.group.sup = 1:2,
    graph = FALSE
  )

  expect_error(
    fviz_famd_var(res.famd, choice = "quali.sup", select.var = list(contrib = 1)),
    "Contributions are not available"
  )
  expect_error(
    fviz_mfa_var(res.mfa, choice = "quali.sup", select.var = list(contrib = 1)),
    "Contributions are not available"
  )

  res.famd.no.sup <- FactoMineR::FAMD(famd_df[, 1:4], graph = FALSE)
  expect_error(
    fviz_famd_var(res.famd.no.sup, choice = "quali.sup"),
    "There are no supplementary qualitative variables"
  )

  res.mfa.no.sup <- FactoMineR::MFA(
    poison,
    group = c(2, 2, 5, 6),
    type = c("s", "n", "n", "n"),
    name.group = c("desc", "desc2", "symptom", "eat"),
    graph = FALSE
  )
  expect_error(
    fviz_mfa_var(res.mfa.no.sup, choice = "quali.sup"),
    "There are no supplementary qualitative variables"
  )
})

test_that("fviz_cos2/fviz_contrib heatmap shows per-dimension values; bar default unchanged", {
  data(decathlon2)
  res <- prcomp(decathlon2[1:23, 1:10], scale. = TRUE)
  has_geom <- function(p, g) any(vapply(p$layers, function(l) inherits(l$geom, g), logical(1)))

  # Default is the barplot (no tiles) -- unchanged.
  expect_false(has_geom(fviz_cos2(res, choice = "var", axes = 1:2), "GeomTile"))
  expect_false(has_geom(fviz_contrib(res, choice = "var", axes = 1:2), "GeomTile"))

  # display = "heatmap" draws a tile grid.
  p_heat <- fviz_cos2(res, choice = "var", axes = 1:3, display = "heatmap")
  expect_s3_class(p_heat, "ggplot")
  expect_true(has_geom(p_heat, "GeomTile"))
  expect_true(has_geom(fviz_contrib(res, choice = "var", axes = 1:2, display = "heatmap"), "GeomTile"))

  # The tile values equal get_pca_var()$cos2 / $contrib for the shown axes, cell-for-cell.
  ref <- get_pca_var(res)$cos2[, 1:3]
  m <- factoextra:::.facto_element_matrix(res, "var", "cos2", 1:3)
  colnames(m) <- colnames(ref)
  expect_equal(m[rownames(ref), ], ref)

  refc <- get_pca_var(res)$contrib[, 1:2]
  mc <- factoextra:::.facto_element_matrix(res, "var", "contrib", 1:2)
  colnames(mc) <- colnames(refc)
  expect_equal(mc[rownames(refc), ], refc)

  # `top` keeps the leading elements.
  p_top <- fviz_cos2(res, choice = "var", axes = 1:2, top = 4, display = "heatmap")
  expect_equal(length(unique(ggplot2::ggplot_build(p_top)$data[[1]]$y)), 4)
})

test_that("fviz_cos2/fviz_contrib heatmap works across methods (CA)", {
  skip_if_not_installed("FactoMineR")
  data(housetasks)
  res.ca <- FactoMineR::CA(housetasks, graph = FALSE)
  expect_s3_class(fviz_cos2(res.ca, choice = "row", axes = 1:2, display = "heatmap"), "ggplot")
  expect_s3_class(fviz_contrib(res.ca, choice = "col", axes = 1:2, display = "heatmap"), "ggplot")
})

test_that("fviz_mca quanti.sup overlays scaled correlation arrows; default off", {
  skip_if_not_installed("FactoMineR")
  data(poison, package = "FactoMineR")
  res <- FactoMineR::MCA(poison, quanti.sup = 1:2, quali.sup = 3:4, graph = FALSE)
  has_seg <- function(p) any(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)))
  seg_ends <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)))[1]
    ggplot2::layer_data(p, i)[, c("xend", "yend")]
  }

  # Default off: no arrow overlay for ind or biplot.
  expect_false(has_seg(fviz_mca_ind(res)))
  expect_false(has_seg(fviz_mca_biplot(res)))

  # quanti.sup = TRUE overlays arrows whose endpoints equal the signed
  # supplementary correlations times a single positive scale factor (arrow
  # length proportional to |correlation|, sign/direction preserved).
  p <- fviz_mca_ind(res, quanti.sup = TRUE)
  expect_true(has_seg(p))
  qs   <- res$quanti.sup$coord[, 1:2]
  ends <- seg_ends(p)
  ends <- ends[order(ends$xend), ]
  qso  <- qs[order(qs[, 1]), ]
  k <- ends$xend[1] / qso[1, 1]           # implied scale factor
  expect_gt(k, 0)                          # arrows not flipped
  expect_equal(unname(as.matrix(ends)), unname(as.matrix(qso * k)), tolerance = 1e-6)
  # Sign preserved: a variable negatively correlated with Dim1 points left.
  expect_true(any(ends$xend < 0))
  expect_true(has_seg(fviz_mca_biplot(res, quanti.sup = TRUE)))

  # No supplementary quantitative variables -> warn and no arrows (no error).
  res2 <- FactoMineR::MCA(poison[, 5:15], graph = FALSE)
  expect_warning(p2 <- fviz_mca_ind(res2, quanti.sup = TRUE), "supplementary quantitative")
  expect_false(has_seg(p2))
})
