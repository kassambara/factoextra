# Coverage for the CA/MCA "contribution biplots" (Greenacre "colgreen"/"rowgreen"
# maps). In a contribution biplot a point is drawn in *contribution coordinates*:
# its standard coordinate times the square root of its mass. A key property makes
# these maps interpretable and gives an exact, independent check of the scaling:
# the SQUARED contribution coordinate on an axis equals that element's relative
# contribution to the axis (contrib / 100). These maps ship but were untested.

test_that("CA contribution biplot: column contribution coordinates match the definition (rowgreen)", {
  skip_if_not_installed("FactoMineR")
  data(housetasks)
  res <- FactoMineR::CA(housetasks, graph = FALSE)
  cc  <- get_ca_col(res)
  eig <- get_eigenvalue(res)[1:2, 1]

  d <- data.frame(name = rownames(cc$coord), x = cc$coord[, 1], y = cc$coord[, 2],
                  coord = NA_real_, stringsAsFactors = TRUE)
  green <- factoextra:::.scale_ca_col(d, res, type = "rowgreen", axes = 1:2)

  # Mechanical: contribution coord = standard coord (principal / sqrt(eig)) * sqrt(mass).
  mass  <- factoextra:::.get_ca_mass(res, "col")[rownames(cc$coord)]
  std_x <- cc$coord[, 1] / sqrt(eig[1])
  std_y <- cc$coord[, 2] / sqrt(eig[2])
  expect_equal(green$x, unname(std_x * sqrt(mass)), tolerance = 1e-8)
  expect_equal(green$y, unname(std_y * sqrt(mass)), tolerance = 1e-8)

  # Defining property: squared contribution coordinate == relative contribution.
  expect_equal(green$x^2, unname(cc$contrib[, 1] / 100), tolerance = 1e-8)
  expect_equal(green$y^2, unname(cc$contrib[, 2] / 100), tolerance = 1e-8)
})

test_that("CA contribution biplot: row contribution coordinates match the definition (colgreen)", {
  skip_if_not_installed("FactoMineR")
  data(housetasks)
  res <- FactoMineR::CA(housetasks, graph = FALSE)
  cr <- get_ca_row(res)

  d <- data.frame(name = rownames(cr$coord), x = cr$coord[, 1], y = cr$coord[, 2],
                  coord = NA_real_, stringsAsFactors = TRUE)
  green <- factoextra:::.scale_ca_row(d, res, type = "colgreen", axes = 1:2)

  expect_equal(green$x^2, unname(cr$contrib[, 1] / 100), tolerance = 1e-8)
  expect_equal(green$y^2, unname(cr$contrib[, 2] / 100), tolerance = 1e-8)
})

test_that("CA symmetric map leaves principal coordinates unchanged (no-regression)", {
  skip_if_not_installed("FactoMineR")
  data(housetasks)
  res <- FactoMineR::CA(housetasks, graph = FALSE)
  cc <- get_ca_col(res)

  d <- data.frame(name = rownames(cc$coord), x = cc$coord[, 1], y = cc$coord[, 2],
                  coord = NA_real_, stringsAsFactors = TRUE)
  # The default "symmetric" map keeps both sets in principal coordinates.
  expect_identical(factoextra:::.scale_ca_col(d, res, type = "symmetric", axes = 1:2), d)
  expect_identical(factoextra:::.scale_ca_row(d, res, type = "symmetric", axes = 1:2), d)
})

test_that("CA contribution biplot holds for a ca::ca object (backend parity)", {
  skip_if_not_installed("ca")
  data(housetasks)
  res <- ca::ca(housetasks)
  cc  <- get_ca_col(res)

  d <- data.frame(name = rownames(cc$coord), x = cc$coord[, 1], y = cc$coord[, 2],
                  coord = NA_real_, stringsAsFactors = TRUE)
  green <- factoextra:::.scale_ca_col(d, res, type = "rowgreen", axes = 1:2)
  expect_equal(green$x^2, unname(cc$contrib[, 1] / 100), tolerance = 1e-8)
  expect_equal(green$y^2, unname(cc$contrib[, 2] / 100), tolerance = 1e-8)
})

test_that("fviz_ca_* render the Greenacre contribution maps", {
  skip_if_not_installed("FactoMineR")
  data(housetasks)
  res <- FactoMineR::CA(housetasks, graph = FALSE)
  expect_s3_class(fviz_ca_biplot(res, map = "rowgreen"), "ggplot")
  expect_s3_class(fviz_ca_biplot(res, map = "colgreen"), "ggplot")
  expect_s3_class(fviz_ca_row(res, map = "colgreen"), "ggplot")
  expect_s3_class(fviz_ca_col(res, map = "rowgreen"), "ggplot")
})
