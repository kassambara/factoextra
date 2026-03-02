## Test environments

* macOS (latest) — R release
* Windows (latest) — R release
* Ubuntu 22.04 — R release, R devel, R oldrel-1

All checks run via GitHub Actions.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Major version bump

This is a major update (1.0.7 → 2.0.0). The last CRAN release was in April 2020. Key changes include:

* Replaced deprecated tidyr internals with base R (`stats::reshape()`)
* Updated Hopkins statistic to use corrected formula (Wright 2022)
* Added parallel analysis support in `fviz_eig()`
* Replaced deprecated ggplot2 functions (`aes_string()`, `stat()`, `guide_legend(override.aes)`)
* Added comprehensive test suite (113 tests via testthat)
* Fixed documentation issues (lost braces, bare URLs, missing `\value` tags)

## Downstream dependencies

Checked reverse dependencies. All packages that could be installed passed R CMD check.
