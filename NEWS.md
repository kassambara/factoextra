# factoextra 1.0.8

## Issues Addressed

This version addresses the following **30 issues** from the original factoextra repository:

| Issue | Description |
|-------|-------------|
| #192 | aes_string() deprecation warnings |
| #191 | aes_string() and size aesthetic deprecation |
| #190 | aes_string() deprecation warnings |
| #189 | tidyr::gather_() deprecation |
| #188 | aes_string() deprecation warnings |
| #183 | guides(FALSE) deprecation in fviz_dend |
| #180 | fviz_dend condition has length > 1 error |
| #179 | guides(FALSE) deprecation warning |
| #178 | linewidth for ellipses (size deprecation) |
| #174 | guides(FALSE) warning in factoextra |
| #173 | guides(FALSE) warning in ggplot_dend |
| #171 | fviz_nbclust condition has length > 1 error |
| #167 | PR: facto_summarize dimension selection fix |
| #166 | fviz_mca_var axes parameter ignored |
| #163 | fviz_dend rect_border error |
| #162 | tidyr::gather_() replacement |
| #159 | PR: guides(FALSE) deprecation fix |
| #156 | guides(FALSE) deprecation warning |
| #151 | fviz_dend condition has length > 1 error |
| #149 | fviz_nbclust class() comparison error |
| #148 | PR: class() == comparison fix |
| #143 | PR: facto_summarize axes parameter fix |
| #141 | fviz_dend guides(FALSE) deprecation |
| #133 | Hopkins statistic biased sampling |
| #131 | fviz_nbclust cluster ordering with k > 9 |
| #129 | PR: fviz_pca_biplot rescaling fix |
| #150 | fviz_ca_col() typo colcol.sup -> col.col.sup |
| #147 | fviz_nbclust silhouette NA with fewer rows than columns |
| #120 | fviz_mca_var mca.cor axes ignored |
| #113 | fviz_nbclust silhouette error with k.max > 15 |
| #64  | Documentation typos |

## Major Changes

### Package Metadata Updates

* **Raised minimum R version** to R >= 4.1.0 (from R >= 3.1.2) to align with
  current dependency requirements (ggplot2, rlang) and modern CRAN policies.

* **Added ggplot2 version requirement** (>= 3.5.2) to ensure compatibility
  with the modernized code that uses linewidth and other 3.5.2+ features.

* **Added LazyDataCompression: xz** to comply with modern CRAN policies for
  efficient data storage.

### ggplot2 3.5.2+ Compatibility Fixes

* **aes_string() deprecation** (Issues #179, #183, #188, #190, #191, #192):
  Replaced all uses of the deprecated aes_string() function with aes() +
  .data pronoun from rlang package.

* **guides(... = FALSE) deprecation** (Issues #141, #156, #159, #173, #174, #179, #183):
  Replaced FALSE with "none" in all guides() calls as required by ggplot2 3.3.4+.

* **size aesthetic deprecation for lines** (Issues #178, #191): Replaced size
  with linewidth for line-based geoms (geom_path, geom_segment, ellipses).

### Dependency Reduction

* **Removed tidyr dependency** (Issues #162, #189): Replaced deprecated
  tidyr::gather_() with base R stats::reshape().

* **Removed reshape2 dependency**: Replaced reshape2::melt() with base R.

### R 4.0+ Compatibility Fixes

* **class() == comparison bug** (Issues #148, #149, #151, #163, #171, #180):
  Fixed condition has length > 1 warnings by using inherits() checks.

## Bug Fixes

* Fixed fviz_dend() rect_border error (Issues #151, #163, #180)
* Fixed fviz_nbclust() condition length error (Issues #148, #149, #171)
* Fixed biased Hopkins statistic sampling (Issue #133)
* Fixed facto_summarize() axes parameter (Issues #120, #143, #166, #167)
* Fixed fviz_pca_biplot() rescaling (PR #129)
* Fixed fviz_nbclust() cluster ordering k > 9 (Issue #131)
* Fixed fviz_nbclust() silhouette error when k >= n (Issues #113, #147)
* Fixed fviz_ca_col() parameter name typo (Issue #150)
* Fixed Hopkins statistic formula (Wright 2022)

## New Features

* fviz_eig() parallel analysis (Horn 1965)
* fviz_pca_biplot() scaling types (Gabriel 1971)

## Documentation Fixes

* Fixed typos (Issue #64)
* Added Hopkins statistic clarification
* Added fviz_gap_stat() documentation

## Contributors

* Laszlo Erdey (University of Debrecen, Hungary)

---

# factoextra 1.0.7

See previous NEWS at https://github.com/kassambara/factoextra
