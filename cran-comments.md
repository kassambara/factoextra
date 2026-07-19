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
* Opt-in paths only: `fviz_eig(parallel = TRUE)` (Horn parallel analysis, with the
  covariance-PCA reference corrected) and
  `fviz_pca_biplot(biplot.type = "form"/"covariance")` (now the exact Gabriel
  factorization, matching `stats::biplot(scale = 0/1)`).
* Clearer input-validation errors/warnings across `fviz_umap()`/`fviz_tsne()`,
  `fviz_dend()`, `fviz_nbclust()`, `get_clust_tendency()`, and `as_factoextra_pca()`.

The remaining output and message changes are itemized in NEWS.md.

## New features (additive)

See NEWS.md; highlights: `fviz_umap()` / `fviz_tsne()`; `theme_factoextra()` and
`factoextra_palette()`; `as_factoextra_pca()` `recipe` / `workflow` methods; and new
opt-in arguments (`fviz_dend(highlight=)`, `max.points`, `display = "heatmap"`,
`mark_optimal`, `fviz_mca_*(quanti.sup=)`, and a `union` element in `select.*`).

## Test environment

* Local: macOS 26.5.2, arm64 — R 4.6.1 (2026-06-24)

No new hosted-CI or win-builder result is claimed for the final correction stage.

## R CMD check results

* `R CMD check --run-donttest --run-dontrun`: status OK
* `R CMD check --as-cran --run-donttest --run-dontrun`: status OK

Both checks used a freshly built source tarball from the current release branch
projection and completed package tests, examples, vignette rebuilding, and
manual checks.

## Downstream dependencies

We checked all 49 reverse dependencies by downloading their sources and reading
the code, tests, and vignettes that call the functions whose output or messages
changed. No reverse dependency snapshots the changed numeric outputs
(`get_pca_ind()` contributions, the Hopkins statistic) or uses the opt-in changed
paths (`biplot.type`, `fviz_eig(parallel = TRUE)`).

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
