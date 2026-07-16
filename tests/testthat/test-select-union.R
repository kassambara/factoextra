# A small fixture mirroring the columns .select() operates on. rownames == name,
# matching how facto_summarize()/.get_supp() build the frame (name-indexed).
.mk <- function() {
  d <- data.frame(
    name    = c("a", "b", "c", "d", "e"),
    cos2    = c(0.90, 0.20, 0.70, 0.95, 0.10),
    contrib = c(5,    50,   20,   3,    30),
    stringsAsFactors = FALSE
  )
  rownames(d) <- d$name
  d
}

test_that("union combines conditions with OR, default stays AND (intersection)", {
  d <- .mk()
  # name = {b} (cos2 0.20) is disjoint from the cos2 >= 0.8 set {a, d}.
  f <- list(name = "b", cos2 = 0.8)
  and <- factoextra:::.select(d, f, check = FALSE)
  or  <- factoextra:::.select(d, modifyList(f, list(union = TRUE)), check = FALSE)
  # AND: name -> {b}, then cos2>=0.8 on {b} -> empty
  expect_equal(nrow(and), 0L)
  # OR: {b} union {a, d}, kept in d's original row order
  expect_identical(or$name, c("a", "b", "d"))
})

test_that("single-condition union is byte-identical to the AND path", {
  d <- .mk()
  for (f in list(list(name = c("d", "a")),   # name (user-vector order preserved by AND)
                 list(cos2 = 0.8),           # cos2 threshold
                 list(cos2 = 2),             # cos2 top-N (integer)
                 list(cos2 = 2.6),           # cos2 top-N (non-integer, unrounded)
                 list(contrib = 2),          # contrib top-N
                 list(contrib = 2.6))) {     # contrib top-N (rounded)
    a <- factoextra:::.select(d, f, check = FALSE)
    u <- factoextra:::.select(d, modifyList(f, list(union = TRUE)), check = FALSE)
    expect_identical(a, u)
  }
})

test_that("union matches an independent base-R set union of the same conditions", {
  d <- .mk()
  f <- list(name = c("b", "e"), cos2 = 0.8, contrib = 2)
  got <- factoextra:::.select(d, modifyList(f, list(union = TRUE)), check = FALSE)
  # Independent engine: compute each index set by hand with base R, then union().
  i_name    <- which(d$name %in% f$name)                       # b, e
  i_cos2    <- which(d$cos2 >= f$cos2)                          # a (0.90), d (0.95)
  i_contrib <- order(d$contrib, decreasing = TRUE)[seq_len(2)] # b (50), e (30)
  expect_true(setequal(union(union(i_name, i_cos2), i_contrib), match(got$name, d$name)))
  expect_identical(got$name, d$name[sort(union(union(i_name, i_cos2), i_contrib))])
})

test_that("an empty single condition under union is not fatal if another supplies rows", {
  d <- .mk()
  # name = {zzz} matches nothing; cos2 >= 0.8 supplies {a, d}
  or <- factoextra:::.select(d, list(name = "zzz", cos2 = 0.8, union = TRUE), check = FALSE)
  expect_identical(or$name, c("a", "d"))
})

test_that("union errors only when the FINAL union is empty (and only under check)", {
  d <- .mk()
  f <- list(name = "zzz", cos2 = 0.99, union = TRUE) # both empty (max cos2 = 0.95)
  expect_warning(
    expect_error(factoextra:::.select(d, f, check = TRUE), "union"),
    "Selection name"
  )
  expect_silent(res <- factoextra:::.select(d, f, check = FALSE))
  expect_equal(nrow(res), 0L)
})

test_that("contrib < 1 still errors under union (misuse, not empty result)", {
  d <- .mk()
  expect_error(
    factoextra:::.select(d, list(name = "a", contrib = 0.4, union = TRUE), check = TRUE),
    "contrib"
  )
})

test_that("union reports the selection count once, on the active (check=TRUE) pass", {
  d <- .mk()
  expect_message(
    factoextra:::.select(d, list(name = "b", cos2 = 0.8, union = TRUE), check = TRUE),
    "union / OR"
  )
  # supplementary-point pass uses check = FALSE and must stay silent
  expect_silent(
    factoextra:::.select(d, list(name = "b", cos2 = 0.8, union = TRUE), check = FALSE)
  )
})

test_that("end-to-end: fviz_pca_var union adds the named var to the top set", {
  skip_if_not_installed("ggplot2")
  res <- prcomp(iris[, 1:4], scale. = TRUE)
  labs_of <- function(p) {
    b  <- ggplot2::ggplot_build(p)$data
    li <- which(vapply(p$layers, function(l) inherits(l$geom, c("GeomText", "GeomTextRepel")),
                       logical(1)))[1]
    sort(as.character(b[[li]]$label))
  }
  # Petal.Width has the lowest contribution -> not in the top-2 contrib set.
  and <- suppressMessages(fviz_pca_var(res, select.var = list(name = "Petal.Width", contrib = 2)))
  or  <- suppressMessages(fviz_pca_var(res, select.var = list(name = "Petal.Width", contrib = 2,
                                                              union = TRUE)))
  la <- labs_of(and); lo <- labs_of(or)
  expect_identical(la, "Petal.Width")            # AND collapses to the named var
  expect_true("Petal.Width" %in% lo)             # OR keeps it ...
  expect_true(length(lo) == 3L)                  # ... plus the two top-contrib vars
  expect_true(all(la %in% lo))                   # AND result is a subset of OR result
})

test_that("union keeps name-matched supplementary individuals despite a contrib condition", {
  skip_if_not_installed("FactoMineR")
  pcaF <- FactoMineR::PCA(iris[, 1:4], ind.sup = 1:10, graph = FALSE)
  sup_name <- rownames(iris)[3]
  labs_of <- function(p) {
    b <- ggplot2::ggplot_build(p)$data
    txt <- lapply(seq_along(p$layers), function(i) {
      if (inherits(p$layers[[i]]$geom, c("GeomText", "GeomTextRepel")))
        as.character(b[[i]]$label) else character(0)
    })
    sort(unique(unlist(txt)))
  }
  # AND: contrib present -> supplementary point #3 is dropped (no contribution).
  and <- suppressMessages(fviz_pca_ind(pcaF, select.ind = list(name = sup_name, contrib = 20)))
  # OR: supplementary point #3 matches by name and must reappear.
  or  <- suppressMessages(fviz_pca_ind(pcaF, select.ind = list(name = sup_name, contrib = 20,
                                                               union = TRUE)))
  expect_false(sup_name %in% labs_of(and))
  expect_true(sup_name %in% labs_of(or))
})
