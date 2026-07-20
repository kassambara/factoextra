## Submission — factoextra 2.2.0

A minor release. Most changes are additive or opt-in, but this version also
**corrects a few numeric outputs** and clarifies several error/warning messages
(all documented in NEWS.md and listed below), so a small number of results differ
from 2.1.0. We checked the reverse dependencies with this in mind (see
"Downstream dependencies").

## Corrected / changed outputs in this version

For transparency, the changes that alter a value or message returned by 2.1.0:

* `get_pca_ind()`: individual contributions for `prcomp` and non-uniform-weight
  `ade4` PCA objects are now normalized to sum to 100% per axis, matching
  `FactoMineR::PCA()` (the `prcomp` values previously summed to `100 * (n - 1) / n`).
  Coordinates, `princomp` contributions, and ordinary full-rank cos2 are
  unchanged. Variable contributions are unchanged on nonzero, ordinarily scaled
  axes; zero-inertia and extreme-magnitude axes now return stable finite values.
* `get_clust_tendency()`: the Hopkins statistic now samples the observed points
  without replacement (a correctness fix), so its value changes for a given seed.
* `fviz_gap_stat()` / `fviz_nbclust(method = "gap_stat")`: when a partial `maxSE`
  list omits `method`, the fallback is now `"firstSEmax"` (the documented default)
  instead of `"firstmax"`.
* `fviz_dend()`: the `cex` argument now scales the leaf-label size for the
  `"rectangle"`/`"circular"` types (it previously had no effect, as the size was
  rescaled away by a continuous size scale). Only the leaf-label font size
  changes; at the default `cex = 0.8` the labels are marginally smaller. No
  other element of the plot, and no returned value, changes.
* Opt-in paths only: `fviz_eig(parallel = TRUE)` (Horn parallel analysis, with the
  covariance-PCA reference corrected) and
  `fviz_pca_biplot(biplot.type = "form"/"covariance")` (now the exact Gabriel
  factorization, matching `stats::biplot(scale = 0/1)`).
* Clearer input-validation errors/warnings across `fviz_umap()`/`fviz_tsne()`,
  `fviz_dend()`, `fviz_nbclust()`, `get_clust_tendency()`, and `as_factoextra_pca()`.
* `fviz_pca_var()` / `fviz_pca_biplot()` now draw the supplementary quantitative
  variables of a FactoMineR `PCA(..., quanti.sup=)` object, which were previously
  omitted (the code read the wrong slot). PCA objects without supplementary
  quantitative variables are unchanged.
* New adapter/plotting guards raise clear errors on previously-unsupported inputs:
  `as_factoextra_pca()` requires `step_pca()` to be the final recipe step and
  rejects case-weighted fits; `fviz_cluster()` errors (rather than mis-colouring)
  when a clustering and its supplied `data` both have complete, unique, but
  non-matching row names.

The remaining output and message changes are itemized in NEWS.md.

## New features (additive)

See NEWS.md; highlights: `fviz_umap()` / `fviz_tsne()`; `theme_factoextra()` and
`factoextra_palette()`; `as_factoextra_pca()` `recipe` / `workflow` methods; and new
opt-in arguments (`fviz_dend(highlight=)`, `max.points`, `display = "heatmap"`,
`mark_optimal`, `fviz_mca_*(quanti.sup=)`, and a `union` element in `select.*`).

## Test environments

* Local: macOS — R 4.5.1 / R 4.6.1 (`R CMD check --as-cran --run-donttest`: OK)
* GitHub Actions: macOS (release), Windows (release), Ubuntu
  (release, devel, oldrel-1), and an Ubuntu leg against ggplot2 development —
  all pass.

## R CMD check results

0 errors | 0 warnings | 0 notes on the CI environments (which include pandoc).

A local `R CMD check --as-cran` reports only environment-only NOTEs that do not
appear where the tooling is current: "Files 'README.md' or 'NEWS.md' cannot be
checked without 'pandoc'", an HTML Tidy version notice, and an "unable to verify
current time" timestamp notice. As in 2.1.0, a few `fviz_*` examples (MCA / MFA /
CA) run inherently slow FactoMineR computations and may trip the "examples > 5s"
NOTE on slower machines; we can wrap them in `\donttest{}` if preferred.

## Downstream dependencies

We checked all 49 reverse dependencies by downloading their sources and reading
the code, tests, and vignettes that call the functions whose output or messages
changed. No reverse dependency snapshots the changed numeric outputs
(`get_pca_ind()` contributions, the Hopkins statistic) or uses the opt-in changed
paths (`biplot.type`, `fviz_eig(parallel = TRUE)`). The `fviz_dend()` `cex` change
is a leaf-label font-size adjustment in a returned ggplot object; no reverse
dependency asserts against dendrogram label sizes.

The one reverse dependency that pins a factoextra error message — **chooseGCM**,
whose tests assert `stats::cutree()`'s native "k must be between 1 and N" error via
`hcut()` — is unaffected: `hcut()` / `hkmeans()` are unchanged and still surface
that native message verbatim (verified). RelativeDistClust reads
`get_pca_ind()$coord` (coordinates are unchanged), and PLNmodels calls
`get_pca_ind()` on a non-`prcomp` object (the contribution change does not apply).
No new problems.

## Notes

* `datanovia.com` URLs can return HTTP 503 to automated crawlers but resolve
  correctly in a browser.
