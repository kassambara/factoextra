skip_if_not_installed("recipes")

make_pca_recipe <- function(num_comp = 3) {
  recipes::recipe(~ ., data = iris[, 1:4]) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = num_comp)
}

test_that("as_factoextra_pca(recipe) extracts scores/loadings/eig matching prcomp", {
  pr  <- recipes::prep(make_pca_recipe(3))
  obj <- as_factoextra_pca(pr)
  pca <- prcomp(scale(iris[, 1:4]))

  expect_s3_class(obj, "factoextra_pca")
  # scores == prcomp scores (up to per-axis sign)
  expect_equal(abs(unname(obj$ind$coord)), abs(unname(pca$x[, 1:3])), tolerance = 1e-6)
  # eigenvalues: the FULL set (all 4), honest % of total variance (not just the 3 kept)
  expect_length(obj$eig.values, 4L)
  expect_equal(obj$eig.values, pca$sdev^2, tolerance = 1e-6)
  # variable coords are the true variable-component correlations = loading * sqrt(eig)
  fm_cor <- sweep(pca$rotation[, 1:3], 2, pca$sdev[1:3], "*")
  expect_equal(abs(unname(obj$var$coord)), abs(unname(fm_cor)), tolerance = 1e-6)
  # a step_normalize precedes the pca -> correlation circle is valid
  expect_true(obj$scale.unit)
})

test_that("as_factoextra_pca(recipe) keeps a full honest scree when num_comp < p", {
  obj <- as_factoextra_pca(recipes::prep(make_pca_recipe(2)))
  # only 2 retained score/loading dimensions ...
  expect_equal(ncol(obj$ind$coord), 2L)
  expect_equal(ncol(obj$var$coord), 2L)
  # ... but the eigenvalue set is complete, so fviz_eig shows the true percentages
  expect_length(obj$eig.values, 4L)
  eig <- get_eigenvalue(obj)
  expect_equal(sum(eig[, "variance.percent"]), 100, tolerance = 1e-6)
})

test_that("as_factoextra_pca(recipe) cross-validates against base-R ground truth", {
  # Independent, version-stable cross-validation of the values factoextra derives
  # with its own formula, using base R (cor / prcomp) rather than the
  # version-fragile FactoMineR object. This always runs and gates CI.
  obj <- as_factoextra_pca(recipes::prep(make_pca_recipe(2)))   # keep 2 of 4 comps
  X <- scale(iris[, 1:4])

  # var$coord (our loading*sqrt(eig)) IS the variable-component correlation,
  # computed here a completely different way (cor of the scaled data vs the
  # scores). Sign is tied to the same scores, so no sign alignment is needed.
  truth <- cor(X, obj$ind$coord)
  expect_equal(unname(obj$var$coord), unname(truth), tolerance = 1e-9)
  expect_equal(unname(obj$var$cos2), unname(truth^2), tolerance = 1e-9)
  expect_equal(unname(obj$var$contrib),
               unname(sweep(truth^2, 2, colSums(truth^2), "/") * 100), tolerance = 1e-8)

  # the honest full scree: percentages match prcomp for ALL components (not just
  # the 2 kept), so fviz_eig() shows the true variance breakdown.
  pc <- prcomp(X)
  expect_equal(unname(get_eigenvalue(obj)[, "variance.percent"]),
               unname(100 * pc$sdev^2 / sum(pc$sdev^2)), tolerance = 1e-9)
  # scores equal prcomp's, up to per-axis sign
  expect_equal(abs(unname(obj$ind$coord)), abs(unname(pc$x[, 1:2])), tolerance = 1e-6)
})

test_that("recipe PCA separates coordinates, correlations and scaling state", {
  centered <- recipes::recipe(~ ., data = iris[, 1:4]) |>
    recipes::step_center(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 2)
  obj <- as_factoextra_pca(recipes::prep(centered))
  x_centered <- scale(iris[, 1:4], center = TRUE, scale = FALSE)
  truth <- stats::cor(x_centered, obj$ind$coord)

  expect_false(obj$scale.unit)
  expect_equal(unname(obj$var$cor), unname(truth), tolerance = 1e-9)
  expect_equal(unname(obj$var$cos2), unname(truth^2), tolerance = 1e-9)
  expect_false(isTRUE(all.equal(unname(obj$var$coord), unname(obj$var$cor))))

  partial <- recipes::recipe(~ ., data = iris[, 1:4]) |>
    recipes::step_center(recipes::all_numeric_predictors()) |>
    recipes::step_scale(Sepal.Length) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 2)
  partial_obj <- as_factoextra_pca(recipes::prep(partial))
  x_partial <- scale(iris[, 1:4], center = TRUE, scale = FALSE)
  x_partial[, "Sepal.Length"] <- x_partial[, "Sepal.Length"] /
    stats::sd(x_partial[, "Sepal.Length"])
  partial_truth <- stats::cor(x_partial, partial_obj$ind$coord)

  expect_false(partial_obj$scale.unit)
  expect_equal(unname(partial_obj$var$cor), unname(partial_truth), tolerance = 1e-9)
  expect_equal(unname(partial_obj$var$cos2), unname(partial_truth^2), tolerance = 1e-9)
})

test_that("recipe PCA detects internal scaling and degrades uncentered semantics", {
  internal <- recipes::recipe(~ ., data = iris[, 1:4]) |>
    recipes::step_pca(
      recipes::all_numeric_predictors(), num_comp = 2,
      options = list(center = TRUE, scale. = TRUE)
  )
  internal_obj <- as_factoextra_pca(recipes::prep(internal))
  internal_truth <- stats::prcomp(
    iris[, 1:4], center = TRUE, scale. = TRUE, rank. = 2
  )
  expect_true(internal_obj$scale.unit)
  expect_equal(unname(internal_obj$ind$coord),
               unname(internal_truth$x[, 1:2]), tolerance = 1e-9)
  expect_equal(unname(internal_obj$var$cor),
               unname(stats::cor(iris[, 1:4], internal_truth$x[, 1:2])),
               tolerance = 1e-9)
  expect_equal(internal_obj$var$coord, internal_obj$var$cor,
               tolerance = 1e-9)

  # Uncentered recipe PCA cannot yield variable-component correlations, so the
  # correlation circle is dropped (scale.unit = FALSE) with a warning, but the
  # scores / eigenvalues / variable coordinates are still returned so the scatter
  # and scree plots keep working.
  uncentered <- recipes::recipe(~ ., data = iris[, 1:4]) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 2)
  expect_warning(u_obj <- as_factoextra_pca(recipes::prep(uncentered)),
                 "correlation circle is omitted")
  expect_false(u_obj$scale.unit)
  expect_null(u_obj$var$cor)
  expect_null(u_obj$var$cos2)
  printed_var <- capture.output(print(get_pca_var(u_obj)))
  expect_true(any(grepl("$coord", printed_var, fixed = TRUE)))
  expect_false(any(grepl("$cor", printed_var, fixed = TRUE)) ||
               any(grepl("$cos2", printed_var, fixed = TRUE)))
  expect_equal(nrow(get_eig(u_obj)), 4L)
  expect_s3_class(fviz_pca_ind(u_obj, geom = "point"), "ggplot")
  expect_s3_class(fviz_eig(u_obj), "ggplot")
  expect_s3_class(fviz_pca_var(u_obj), "ggplot")
  expect_s3_class(fviz_pca_biplot(u_obj), "ggplot")
  expect_error(fviz_cos2(u_obj, choice = "var"), "cos2.*not available")
  expect_error(fviz_pca_var(u_obj, col.var = "cos2"), "cos2.*not available")
  expect_error(fviz_pca_var(u_obj, alpha.var = "cos2"), "cos2.*not available")
  expect_error(
    fviz_pca_var(u_obj, geom = "point", pointsize = "cos2"),
    "cos2.*not available"
  )
  expect_error(fviz_pca_var(u_obj, select.var = list(cos2 = 1)),
               "Cos2.*not available")

  scale_only <- recipes::recipe(~ ., data = iris[, 1:4]) |>
    recipes::step_scale(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 2)
  expect_warning(so_obj <- as_factoextra_pca(recipes::prep(scale_only)),
                 "correlation circle is omitted")
  expect_false(so_obj$scale.unit)
  expect_null(so_obj$var$cor)
  expect_null(so_obj$var$cos2)

  arbitrary_center <- recipes::recipe(~ ., data = iris[, 1:4]) |>
    recipes::step_pca(
      recipes::all_numeric_predictors(), num_comp = 2,
      options = list(center = rep(0, 4))
    )
  expect_warning(ac_obj <- as_factoextra_pca(recipes::prep(arbitrary_center)),
                 "correlation circle is omitted")
  expect_null(ac_obj$var$cor)
  expect_null(ac_obj$var$cos2)

  with_constant <- cbind(iris[, 1:4], constant = 1)
  zero_inertia <- recipes::recipe(~ ., data = with_constant) |>
    recipes::step_center(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 2)
  expect_warning(z_obj <- as_factoextra_pca(recipes::prep(zero_inertia)),
                 "zero or non-finite inertia")
  expect_false(z_obj$scale.unit)
  expect_null(z_obj$var$cor)
  expect_null(z_obj$var$cos2)
  expect_s3_class(fviz_pca_var(z_obj), "ggplot")
})

test_that("column-only recipe steps preserve centering and scaling evidence", {
  rec <- recipes::recipe(~ ., data = iris[, 1:4]) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_corr(recipes::all_numeric_predictors(), threshold = 0.99) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 2)
  prepped <- recipes::prep(rec)
  obj <- as_factoextra_pca(prepped)
  baked <- recipes::bake(prepped, new_data = NULL)
  score_names <- recipes::names0(2, prepped$steps[[3]]$prefix)

  expect_true(obj$scale.unit)
  expect_equal(
    abs(unname(obj$ind$coord)),
    abs(unname(as.matrix(baked[, score_names, drop = FALSE]))),
    tolerance = 1e-10
  )
})

test_that("as_factoextra_pca(recipe) matches FactoMineR (authoritative engine)", {
  skip_if_not_installed("FactoMineR")
  # A step_normalize + step_pca recipe is a scaled PCA, directly comparable to
  # FactoMineR::PCA(). Use a full-rank FactoMineR fit so $eig / $var are complete
  # on any FactoMineR version (newer versions truncate them to ncp); compare the
  # retained components. Kept as a looser, robust confirmation against the
  # authoritative engine (the tight, CI-gating check is the base-R test above).
  obj <- as_factoextra_pca(recipes::prep(make_pca_recipe(2)))
  p   <- ncol(iris[, 1:4])
  fm  <- FactoMineR::PCA(iris[, 1:4], graph = FALSE, ncp = p)
  k   <- ncol(obj$var$coord)
  signfix <- function(a, b) sweep(a, 2, sign(colSums(a * b)), "*")

  expect_equal(unname(signfix(obj$var$coord, fm$var$coord[, seq_len(k)])),
               unname(fm$var$coord[, seq_len(k)]), tolerance = 1e-6)
  expect_equal(unname(obj$var$cos2), unname(fm$var$cos2[, seq_len(k)]), tolerance = 1e-6)
  # full eigenvalue percentages (both full-length here)
  expect_equal(unname(get_eigenvalue(obj)[, "variance.percent"]),
               unname(fm$eig[, "percentage of variance"]), tolerance = 1e-6)
})

test_that("as_factoextra_pca(recipe) handles a custom step_pca prefix", {
  # Regression: tidy(type='coef') labels components PC1.. regardless of prefix, so
  # the loadings must be keyed off the coef labels, not the prefixed score columns.
  rec <- recipes::recipe(~ ., data = iris[, 1:4]) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 3, prefix = "Comp")
  obj <- as_factoextra_pca(recipes::prep(rec))
  expect_false(anyNA(obj$var$coord))          # not a blank correlation circle
  expect_equal(dim(obj$var$coord), c(4L, 3L))
  # same numbers as the default-prefix recipe
  obj_def <- as_factoextra_pca(recipes::prep(make_pca_recipe(3)))
  expect_equal(abs(obj$var$coord), abs(obj_def$var$coord), tolerance = 1e-10,
               ignore_attr = TRUE)
})

test_that("as_factoextra_pca(recipe) handles >= 10 components (zero-padded score names)", {
  # recipes names score columns PC01..PC12 at >=10 comps; the extractor must match
  # those (a happy-path iris test with <=4 comps would miss this).
  set.seed(1)
  X <- as.data.frame(matrix(rnorm(200 * 12), 200, 12))
  names(X) <- paste0("v", 1:12)
  rec <- recipes::recipe(~ ., data = X) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 12)
  obj <- as_factoextra_pca(recipes::prep(rec))
  expect_false(anyNA(obj$ind$coord))
  expect_false(anyNA(obj$var$coord))
  expect_equal(ncol(obj$ind$coord), 12L)
  expect_length(obj$eig.values, 12L)
})

test_that("as_factoextra_pca(recipe) feeds the fviz_pca family without error", {
  obj <- as_factoextra_pca(recipes::prep(make_pca_recipe(4)))
  expect_no_error(ggplot2::ggplot_build(fviz_pca_ind(obj)))
  expect_no_error(ggplot2::ggplot_build(fviz_pca_var(obj)))
  expect_no_error(ggplot2::ggplot_build(fviz_pca_biplot(obj)))
  expect_no_error(ggplot2::ggplot_build(fviz_eig(obj)))
  expect_no_error(ggplot2::ggplot_build(fviz_contrib(obj, "var")))
})

test_that("as_factoextra_pca(recipe) errors clearly on non-PCA / unprepped recipes", {
  # not prepped
  expect_error(as_factoextra_pca(make_pca_recipe()), "prep")
  # no step_pca
  rec_plain <- recipes::prep(recipes::recipe(~ ., data = iris[, 1:4]) |>
                               recipes::step_center(recipes::all_numeric_predictors()))
  expect_error(as_factoextra_pca(rec_plain), "step_pca")

  post_pca <- recipes::recipe(~ ., data = iris[, 1:4]) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 2) |>
    recipes::step_scale(recipes::all_numeric_predictors()) |>
    recipes::prep()
  expect_error(as_factoextra_pca(post_pca), "must be the final recipe step")

  weighted_data <- iris[, 1:4]
  weighted_data$case_weight <- hardhat::frequency_weights(
    c(100, rep(1, nrow(weighted_data) - 1L))
  )
  weighted_pca <- recipes::recipe(~ ., data = weighted_data) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 2) |>
    recipes::prep()
  expect_error(as_factoextra_pca(weighted_pca), "Case-weighted step_pca")
})

test_that("as_factoextra_pca(recipe) names a non-PCA dimension-reduction step in its error", {
  skip_if_not_installed("fastICA")  # step_ica() needs fastICA to prep
  rec_ica <- recipes::prep(recipes::recipe(~ ., data = iris[, 1:4]) |>
                             recipes::step_ica(recipes::all_numeric_predictors(), num_comp = 2))
  expect_error(as_factoextra_pca(rec_ica), "step_ica")
})

test_that("as_factoextra_pca(workflow) matches the recipe path (fitted workflow)", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  rec <- recipes::recipe(mpg ~ ., data = mtcars) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 3)
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(parsnip::linear_reg()) |>
    parsnip::fit(data = mtcars)

  obj_wf  <- as_factoextra_pca(wf)
  obj_rec <- as_factoextra_pca(recipes::prep(rec))
  expect_s3_class(obj_wf, "factoextra_pca")
  expect_equal(obj_wf$eig.values, obj_rec$eig.values, tolerance = 1e-6)
  expect_equal(abs(obj_wf$ind$coord), abs(obj_rec$ind$coord), tolerance = 1e-6)
  # the workflow path recovers the same variable correlations and scaling state
  expect_equal(abs(obj_wf$var$cor), abs(obj_rec$var$cor), tolerance = 1e-6)
  expect_identical(obj_wf$scale.unit, obj_rec$scale.unit)

  raw_rec <- recipes::recipe(mpg ~ ., data = mtcars) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 3)
  raw_wf <- workflows::workflow() |>
    workflows::add_recipe(raw_rec) |>
    workflows::add_model(parsnip::linear_reg()) |>
    parsnip::fit(data = mtcars)
  expect_warning(raw_obj <- as_factoextra_pca(raw_wf),
                 "correlation circle is omitted")
  expect_null(raw_obj$var$cor)
  expect_null(raw_obj$var$cos2)
  expect_s3_class(fviz_pca_var(raw_obj), "ggplot")

  post_pca_rec <- recipes::recipe(mpg ~ ., data = mtcars) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 3) |>
    recipes::step_scale(recipes::all_numeric_predictors())
  post_pca_wf <- workflows::workflow() |>
    workflows::add_recipe(post_pca_rec) |>
    workflows::add_model(parsnip::linear_reg()) |>
    parsnip::fit(data = mtcars)
  expect_error(as_factoextra_pca(post_pca_wf), "must be the final recipe step")

  weighted_mtcars <- mtcars
  weighted_mtcars$case_weight <- hardhat::frequency_weights(
    c(10, rep(1, nrow(weighted_mtcars) - 1L))
  )
  weighted_rec <- recipes::recipe(mpg ~ ., data = weighted_mtcars) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_pca(recipes::all_numeric_predictors(), num_comp = 3)
  weighted_wf <- workflows::workflow() |>
    workflows::add_recipe(weighted_rec) |>
    workflows::add_model(parsnip::linear_reg()) |>
    parsnip::fit(data = weighted_mtcars)
  expect_error(as_factoextra_pca(weighted_wf), "Case-weighted step_pca")

  # unfitted workflow (recipe added but not fit) errors with a fit()/prep() hint
  wf_unfit <- workflows::workflow() |> workflows::add_recipe(rec)
  expect_error(as_factoextra_pca(wf_unfit))
})

test_that("as_factoextra_pca() default method keeps its published call forms", {
  pca <- prcomp(iris[, -5], scale. = TRUE)
  # named `ind.coord =` (the shipped example form)
  o_named <- as_factoextra_pca(ind.coord = pca$x, var.coord = pca$rotation, eig = pca$sdev^2)
  # positional multi-arg (also published-callable) must bind the same way
  o_pos <- as_factoextra_pca(pca$x, pca$rotation, pca$sdev^2)
  expect_s3_class(o_named, "factoextra_pca")
  expect_false(is.null(o_pos$var))                 # var.coord not dropped
  expect_identical(o_named$ind$coord, o_pos$ind$coord)
  expect_identical(o_named$var$coord, o_pos$var$coord)
  expect_identical(o_named$eig.values, o_pos$eig.values)
})

test_that("as_factoextra_pca() derives scale-invariant finite metrics", {
  coord <- matrix(c(-1, 1, -2, 2, -3, 3, -4, 4), ncol = 2)
  reference <- as_factoextra_pca(
    coord, var.coord = coord, eig = c(1, 1)
  )
  metrics <- function(x) list(
    ind.cos2 = x$ind$cos2,
    ind.contrib = x$ind$contrib,
    var.cos2 = x$var$cos2,
    var.contrib = x$var$contrib
  )

  for(multiplier in c(1e-200, 1e200)){
    scaled <- as_factoextra_pca(
      coord * multiplier, var.coord = coord * multiplier, eig = c(1, 1)
    )
    expect_equal(metrics(scaled), metrics(reference), tolerance = 1e-14)
  }

  large_eigenvalues <- as_factoextra_pca(coord, eig = c(1e308, 1e308))
  expect_equal(
    unname(get_eig(large_eigenvalues)[, "variance.percent"]),
    c(50, 50)
  )
})
