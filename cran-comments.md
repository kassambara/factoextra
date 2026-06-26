## Submission — factoextra 2.1.0

A minor release adding new visualization features and bug fixes; no breaking
changes (new behavior is gated, existing inputs are unchanged).

## Test environments

* macOS Tahoe 26.5.1 — R 4.6.0 (2026-04-24)
* Additional Windows/Linux checks should be filled in after CI completes.

## R CMD check results

Local macOS result:

0 errors | 0 warnings | 0 notes

## Additional local pre-submission checks (June 26, 2026)

* macOS Tahoe 26.5.1 — R 4.6.0 (2026-04-24)
* `R CMD check --as-cran factoextra_2.1.0.tar.gz`: 0 errors | 0 warnings | 0 notes
* `devtools::test()`: 404 passed, 0 failed, 0 warnings, 0 skipped
* `tools::checkRd()`: no Rd issues
* `urlchecker::url_check()`: no problems

## Changes in this version (2.1.0)

Minor update from 2.0.0. Highlights (see NEWS.md for the full list):

* New `as_factoextra_pca()` constructor to visualize coordinates from any
  dimension-reduction method with the `fviz_pca_*()` family.
* New arguments: `shape.ind` (shape individuals by a second factor),
  `rotate.labels` (ggbiplot-style variable-label rotation).
* Support for ade4 between-class / within-class PCA (`bca()`/`wca()`).
* New vignette "Extending factoextra to support new analysis backends".
* Bug fixes: `fviz_mca_biplot()` `map` argument, `get_pca_ind()` for ade4
  `dudi.pca`, `fviz_dend()` honoring `k` for HCPC, and clearer warnings for
  unrecognized `label`/`invisible` values.

(The last CRAN release was 1.0.7; version 2.0.0 introduced the larger
modernization documented in NEWS.md.)

## Notes

* No local notes.

## Downstream dependencies

Current CRAN metadata lists 32 direct reverse dependencies. Reverse-dependency
checks were not rerun in this local pass.
