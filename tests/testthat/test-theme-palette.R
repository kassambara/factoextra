test_that("factoextra_palette returns the Okabe-Ito colorblind-safe colours", {
  okabe <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00",
             "#0072B2", "#CC79A7", "#F0E442", "#999999", "#000000")
  expect_identical(factoextra_palette("okabe"), okabe)
  expect_identical(factoextra_palette(), okabe)            # default palette
  expect_identical(factoextra_palette("okabe-ito"), okabe) # alias
  # these are exactly the base-R Okabe-Ito colours (reordered, neutrals last)
  expect_setequal(factoextra_palette("okabe"),
                  unname(grDevices::palette.colors(palette = "Okabe-Ito")))
})

test_that("factoextra_palette honours n and recycles with a message", {
  expect_identical(factoextra_palette("okabe", n = 3),
                   c("#E69F00", "#56B4E9", "#009E73"))
  expect_length(factoextra_palette("okabe", n = 1), 1L)
  # asking for more than available recycles (and warns the user)
  expect_message(rc <- factoextra_palette("okabe", n = 12), "recycling")
  expect_length(rc, 12L)
  expect_identical(rc[1:9], factoextra_palette("okabe"))
  # invalid inputs error clearly
  expect_error(factoextra_palette("not-a-palette"))
  expect_error(factoextra_palette("okabe", n = 0))
  expect_error(factoextra_palette("okabe", n = -3))
})

test_that("theme_factoextra returns a complete ggplot2 theme with a coordinate grid", {
  th <- theme_factoextra()
  expect_s3_class(th, "theme")
  expect_true(attr(th, "complete"))
  # the deliberate difference from theme_pubr(): a light major grid, no axis line
  expect_false(inherits(th$panel.grid.major, "element_blank"))
  expect_s3_class(th$axis.line, "element_blank")
  expect_s3_class(th$panel.grid.minor, "element_blank")
  # publication-grade readability: tick labels (values) are dark, not the faint
  # grey30 theme_minimal default; titles are bold and dark
  expect_identical(th$axis.text$colour, "grey20")
  expect_identical(th$axis.title$face, "bold")
  expect_identical(th$axis.title$colour, "grey10")
  # base_size flows through
  expect_equal(theme_factoextra(base_size = 20)$text$size, 20)
})

test_that("theme_factoextra and factoextra_palette apply to fviz_* plots without error", {
  set.seed(1)
  km <- stats::kmeans(scale(iris[, 1:4]), 3, nstart = 10)
  p <- fviz_cluster(km, data = iris[, 1:4],
                    palette = factoextra_palette("okabe"),
                    ggtheme = theme_factoextra())
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  # discrete groups get the Okabe-Ito colours (routed by the existing palette arg)
  b <- ggplot2::ggplot_build(p)
  li <- which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomPoint") && inherits(l$stat, "StatIdentity"), logical(1)))[1]
  expect_true(all(unique(b$data[[li]]$colour) %in% factoextra_palette("okabe")))

  # composes onto an existing plot too
  res <- prcomp(iris[, 1:4], scale. = TRUE)
  expect_no_error(ggplot2::ggplot_build(fviz_pca_var(res) + theme_factoextra()))
})
