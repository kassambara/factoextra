## Submission — factoextra 2.2.0

This release adds new visualization and interoperability features and hardens
existing numerical, validation, and documentation paths. Corrected numerical
results and stricter validation are described in `NEWS.md`; this submission does
not claim byte-identical behavior for affected inputs.

## Changes in this version (2.2.0)

See `NEWS.md` for the complete list. The principal changes are:

* new two-dimensional UMAP and t-SNE visualization helpers;
* new publication theme and colorblind-safe palette helpers;
* support for adapting PCA results from tidymodels recipes and workflows;
* corrected PCA contributions, Gabriel biplot scaling, and Horn parallel
  analysis;
* stronger validation and edge-case handling in clustering, selection,
  supplementary-element, and Hopkins-statistic paths; and
* refreshed help pages, examples, the extension vignette, pkgdown
  configuration, and release notes.

## Test environment

* Local: macOS Tahoe 26.5.2, arm64 — R 4.6.0 (2026-04-24)

No win-builder, GitHub Actions, or external reverse-dependency result is claimed
in this local preparation packet.

## R CMD check results

* `R CMD check`: 0 errors | 0 warnings | 0 notes
* `R CMD check --as-cran --run-donttest --run-dontrun`:
  0 errors | 0 warnings | 0 notes

Both checks used a freshly built source tarball from a clean exported source
tree. The CRAN-style check completed incoming-feasibility checks, package tests,
all example classes, vignette rebuilds, and PDF and HTML manual checks.

## Additional local validation

* The complete testthat suite passed.
* Standard, `\donttest`, and `\dontrun` examples all completed successfully.
* Package coverage increased from 77.75% on the fetched branch to 79.59% after
  the added regression tests.
* roxygen documentation generation, manual consistency checks, pkgdown site
  generation, R-source parsing, and PDF structural and visual checks passed.
* Package-wide spelling output was reviewed; reported tokens are proper names,
  software/API identifiers, accepted British spellings, hyphen fragments, or an
  intentionally quoted historical misspelling.

`urlchecker` reports HTTP 403 for the canonical DOI
<https://doi.org/10.1093/bioinformatics/btv428> because the publisher blocks the
automated request. The DOI is retained, and the CRAN incoming-feasibility check
passes.

## Reverse dependencies

A fresh full reverse-dependency matrix was not run locally, so no reverse-
dependency pass count is claimed here.
