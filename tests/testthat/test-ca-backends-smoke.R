test_that("CA extractors work with ca package outputs", {
  skip_if_not_installed("ca")

  data("housetasks", package = "factoextra")
  res <- ca::ca(housetasks)

  cols <- get_ca_col(res)
  rows <- get_ca_row(res)

  expect_s3_class(cols, "factoextra")
  expect_s3_class(rows, "factoextra")
  expect_true(all(c("coord", "cos2", "contrib") %in% names(cols)))
  expect_true(all(c("coord", "cos2", "contrib") %in% names(rows)))
  expect_equal(nrow(cols$coord), ncol(housetasks))
  expect_equal(nrow(rows$coord), nrow(housetasks))
})

test_that("CA extractors work with MASS correspondence outputs", {
  skip_if_not_installed("MASS")

  data("housetasks", package = "factoextra")
  res <- MASS::corresp(housetasks, nf = 2)

  cols <- get_ca_col(res)
  rows <- get_ca_row(res)

  expect_s3_class(cols, "factoextra")
  expect_s3_class(rows, "factoextra")
  expect_equal(ncol(cols$coord), 2)
  expect_equal(ncol(rows$coord), 2)
  expect_false(any(is.na(cols$cos2)))
  expect_false(any(is.na(rows$cos2)))
})
