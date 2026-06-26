## Submission — factoextra 2.1.0

A minor release adding new visualization features and bug fixes; no breaking
changes (new behavior is gated, existing inputs are unchanged).

## Test environments

* macOS (latest) — R release
* Windows (latest) — R release
* Ubuntu 22.04 — R release, R devel, R oldrel-1

All checks run via GitHub Actions.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Additional local pre-submission checks (June 26, 2026)

* macOS — R 4.5.x
* `R CMD check --as-cran factoextra_2.1.0.tar.gz`: 0 errors | 0 warnings | 0 notes
* `devtools::test()`: all tests pass
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

* Some `datanovia.com` URLs (in DESCRIPTION, README, and man pages) return
  HTTP 503 to automated crawlers but are valid and accessible in a browser.

## Downstream dependencies

Reverse dependencies checked; all packages that could be installed passed
R CMD check (no new problems introduced by this release).
