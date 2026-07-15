## Submission — factoextra 2.2.0

A minor release. All new behavior is opt-in and gated, so existing inputs
produce byte-identical output; there are no breaking changes.

## Changes in this version (2.2.0)

Minor update from 2.1.0 (see NEWS.md for the full list):

* New `fviz_umap()` / `fviz_tsne()` to visualize a 2-D UMAP or t-SNE embedding
  (from uwot, Rtsne, umap, or a plain coordinate matrix) with factoextra's
  grouping, ellipse and palette styling. An embedding has no eigenvalues, so the
  axes carry no percentage and there is no scree plot or correlation circle.
* New `theme_factoextra()` and `factoextra_palette()`: a clean publication theme
  and an Okabe-Ito colorblind-safe palette, both stateless (no global option).
* `as_factoextra_pca()` gains `recipe` and `workflow` methods, so a PCA fitted
  inside a tidymodels recipe or workflow plots with the `fviz_pca_*()` family.
* New arguments (all default to the current behavior): `fviz_dend(highlight=)`;
  `max.points` for `fviz_pca_ind()` / `fviz_cluster()` and the other point
  plots; `display = "heatmap"` for `fviz_cos2()` / `fviz_contrib()`;
  `mark_optimal` for `fviz_nbclust()` / `fviz_gap_stat()`; `quanti.sup` for
  `fviz_mca_ind()` / `fviz_mca_biplot()`; and a `union` element in the
  `select.*` selection lists (OR selection; the default stays AND).

## Test environments

* Local: macOS (Darwin 24.6.0) — R 4.5.1 (2025-06-13)
* win-builder: R release and R devel
* GitHub Actions: macOS (release), Windows (release), Ubuntu
  (release, devel, oldrel-1), and an Ubuntu leg against ggplot2 development —
  all pass.

## R CMD check results

0 errors | 0 warnings | 0 notes

The previously reported "examples > 5s" NOTE comes from a few `fviz_*` examples
that run inherently slow FactoMineR computations (MCA / MFA / CA) as correctness
illustrations; happy to wrap them in `\donttest{}` if preferred.

## Downstream dependencies

We ran R CMD check on the reverse dependencies. No new problems were introduced
by this release. <!-- revdep summary finalized after revdepcheck run -->

## Notes

* `datanovia.com` URLs can return HTTP 503 to automated crawlers but resolve
  correctly in a browser.
