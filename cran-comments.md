## Submission — factoextra 2.1.0

A minor release adding new visualization features and bug fixes; no breaking
changes (new behavior is gated, existing inputs are unchanged).

## Resubmission

This is a resubmission addressing the auto-check feedback on the previous
2.1.0 upload:

* **Reverse dependency (chooseGCM) fixed.** `hcut()`/`hkmeans()` had added a
  custom early error for `k > number-of-observations` that pre-empted (and so
  changed the message of) `stats::cutree()`'s native error. chooseGCM's test
  pins that native message (`"elements of 'k' must be between 1 and 11"`). We
  removed the custom early error so the native message is restored — matching the
  long-published behaviour. chooseGCM's tests pass again. A regression test now
  guards this so it cannot recur.
* **Spelling NOTE fixed.** "phylogenic" → "phylogenetic" in DESCRIPTION.
* The "examples > 5s" NOTE comes from a few `fviz_*` examples that run inherently
  slow FactoMineR computations (MCA/MFA/CA); these are correctness illustrations.
  Happy to wrap them in `\donttest{}` if preferred.

## Test environments

* Local: macOS Tahoe 26.5.1 — R 4.6.0 (2026-04-24)
* win-builder: R release — OK
* GitHub Actions: macOS (release), Windows (release), Ubuntu 22.04
  (release, devel, oldrel-1) — all pass

## R CMD check results

0 errors | 0 warnings | 0 notes

(A local "unable to verify current time" / future-file-timestamps NOTE seen on a
sandboxed machine without network is environment-specific and does not occur on
win-builder or the CI machines.)

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

The previously failing reverse dependency **chooseGCM** now passes: its
`hclust_gcms()` k > n test again receives `stats::cutree()`'s native
`"elements of 'k' must be between 1 and N"` error (verified against the fixed
package). No other reverse dependency is affected by this change.
