# factoextra 2.2.0

## New features

* `fviz_dend()` gains a `highlight` argument to emphasize the branches leading to
  specific leaves (with `highlight.col` / `highlight.lwd`); it layers on top of
  the cluster colouring, so highlighted branches stand out while every other
  branch keeps its colour. `highlight = NULL` (default) is unchanged. For other
  branch styling (e.g. dashed branches) pass a pre-styled `dendextend` object,
  which `fviz_dend()` honours (see `?fviz_dend`).

* New `fviz_umap()` and `fviz_tsne()` to visualize a 2-D UMAP or t-SNE embedding
  (from `uwot`, `Rtsne`, `umap`, or a plain coordinate matrix) with factoextra's
  grouping, ellipse and palette styling, including colouring points by a
  continuous feature value. An embedding has no eigenvalues, so the axes are
  labelled `UMAP1`/`UMAP2` (`tSNE1`/`tSNE2`) without a percentage and there is no
  scree plot, loadings or correlation circle; when requested, the group outline
  defaults to a convex hull rather than a normal confidence ellipse (which would
  assume a metric the embedding does not preserve). `uwot` and `Rtsne` are
  Suggests.

* `as_factoextra_pca()` gains `recipe` and `workflow` methods, so a PCA fitted
  inside a tidymodels `recipe` (`recipes::step_pca()`) or a fitted `workflow`
  plots directly with the `fviz_pca_*`/`fviz_eig`/`fviz_contrib`/`fviz_cos2`
  family: `prep(rec) |> as_factoextra_pca() |> fviz_pca_biplot()`. Scores,
  loadings and the full set of eigenvalues are extracted through the public
  recipes/workflows API, so the scree plot and axis percentages are honest even
  when only a few components are kept. When inputs are provably centered,
  variable-component correlations and cos2 are recovered separately from the
  loading-times-component-SD arrow coordinates; the two coincide for centered,
  unit-scaled inputs. `recipes` and `workflows` are Suggests.

* New `factoextra_palette()` and `theme_factoextra()`. `factoextra_palette("okabe")`
  returns the Okabe-Ito colorblind-safe categorical colors as a vector to pass to
  the existing `palette` argument (e.g. `fviz_cluster(res, palette =
  factoextra_palette("okabe"))`); `theme_factoextra()` is a clean publication theme
  with a light coordinate grid, passed via `ggtheme` or added with `+`. Both are
  explicit and stateless (no global option). The Okabe-Ito colors are those of
  `grDevices::palette.colors("Okabe-Ito")`.

* `fviz_nbclust()` and `fviz_gap_stat()` gain a `mark_optimal` argument. Set
  `mark_optimal = TRUE` to mark the elbow of the `"wss"` plot with a dashed guide
  line (a deterministic maximum-distance heuristic; see `?fviz_nbclust`);
  `mark_optimal = FALSE` omits the optimal-cluster guide line for every method.
  The default (`NULL`) keeps each method's existing behavior: the guide line is
  shown for `"silhouette"` and `"gap_stat"` and omitted for `"wss"`.
* `fviz_cos2()` and `fviz_contrib()` gain a `display` argument. `display = "heatmap"`
  draws a grid with one tile per element and dimension, filled and labelled by the
  per-dimension cos2/contribution, so the quality/contribution across several
  dimensions can be read at once. The default (`display = "bar"`) is unchanged.
* `fviz_pca_ind()`, `fviz_cluster()` (and the other individual / row / column
  plots) gain a `max.points` argument for large datasets. When there are more
  than `max.points` points, a reproducible random subset of that many is drawn so
  labels, colours and ellipses stay readable instead of over-plotting. When points
  are grouped (e.g. `habillage`, or clusters in `fviz_cluster()`) the draw is
  stratified so every group keeps a minimum number of points and none is
  decimated, and a message reports how many points are shown. Only the drawn
  points are thinned: any ellipse (`addEllipses` / the cluster frame) and the
  group/cluster centres are still computed on the full data, so a convex hull or
  confidence ellipse is not distorted by dropping the extreme points a random
  draw tends to lose. A continuous colour, fill or size mapping (e.g. `col.ind =
  "cos2"`) and its legend are likewise pinned to the full-data range, so a
  point's colour, fill and size do not depend on how many points are drawn. The
  draw does not disturb the caller's random stream.
  `max.points = NULL` (default) draws every point, and `sample.seed` controls
  which subset is chosen.
* `fviz_mca_ind()` and `fviz_mca_biplot()` gain a `quanti.sup` argument. Set
  `quanti.sup = TRUE` to overlay the supplementary quantitative variables of a
  FactoMineR MCA on the map as correlation arrows, so a continuous covariate's
  direction can be read against the category/individual cloud. Each arrow's length
  is proportional to the variable's absolute correlation with the dimensions
  (relative to the cloud extent). The default (`quanti.sup = FALSE`) leaves the map
  unchanged.
* The `select.ind` / `select.var` (and `select.row` / `select.col`) lists gain a
  `union` element. When several of `name` / `cos2` / `contrib` are supplied they
  are combined with AND by default (each condition further narrows the selection,
  as before); `union = TRUE` combines them with OR instead, so named elements are
  kept *together with* the top-`cos2` or top-`contrib` ones, e.g. `select.var =
  list(name = c("V1", "V2"), contrib = 10, union = TRUE)`. The same result can
  still be obtained by precomputing the set and passing it as `name`; `union` is a
  convenience for building it inline. The default (no `union`, or `union = FALSE`)
  is unchanged. Thanks to @qfazille (#53).

## Main changes

* `get_pca_ind()`: individual contributions for `prcomp` objects, and for `ade4`
  PCA objects with non-uniform row weights, are now normalized to sum to 100
  percent per axis, matching `FactoMineR::PCA()`. Previously `prcomp` individual
  contributions were divided by the (n-1)-normalized eigenvalue and summed to
  `100 * (n - 1) / n` instead of 100. This changes the individual contribution
  values returned for those objects (and any `fviz_contrib()` / `fviz_pca()`
  colouring derived from them). `princomp` individual contributions,
  coordinates, and cos2 are unchanged for ordinary finite inputs. Variable
  contributions are unchanged on nonzero, ordinarily scaled axes; zero-inertia
  and extreme-magnitude axes now return stable finite values instead of NaN or
  overflow. For a rank-truncated `prcomp` object, individual cos2 is explicitly
  defined within the retained component subspace because discarded row inertia
  is not stored in the fit. The corrected values match `FactoMineR::PCA()` and
  `ade4::inertia.dudi()`. Thanks to @erdeyl (#274).

* `fviz_pca_biplot()`: `biplot.type = "form"` and `biplot.type = "covariance"`
  now use the exact Gabriel biplot factorization, matching
  `stats::biplot(scale = 0)` and `stats::biplot(scale = 1)` respectively, instead
  of the previous display heuristic. The usual `biplot.type = "auto"` scaling
  algorithm is otherwise unchanged. Rank-truncated `prcomp` fits now use only
  the retained component standard deviations when computing variable
  coordinates, avoiding recycled values and incorrect arrows. The exact modes
  also handle formula fits with `na.exclude`; covariance mode uses `n.obs` for
  `princomp`, matching `stats::biplot()`. The exact modes require a `prcomp` /
  `princomp` object. Thanks to @erdeyl (#274).

* `as_factoextra_pca()` recipe / workflow methods (tidymodels): variable-component
  correlations and cos2 are now recovered from the full PCA inertia, so they match
  `FactoMineR::PCA()` of the same data even when `step_pca()` keeps only a few
  components (previously the cos2 was normalized within the retained subspace and
  matched only when all components were kept). `scale.unit` is set to `TRUE` only
  when every PCA input is both centered and unit-scaled at the PCA boundary.
  When the metrics cannot be recovered (e.g. a bare `step_pca()` or a
  zero-inertia variable), `get_pca_var()` returns `NULL` for its correlation and
  cos2 entries instead of synthesizing them from the retained coordinates.
  Scores, eigenvalues, variable coordinates and contributions remain available,
  so ordinary variable arrows still work; correlation/cos2-dependent displays
  fail explicitly, the correlation circle is omitted, and a warning gives the
  applicable centering or zero-inertia remedy. For fully normalized inputs, the
  recovered metrics match `FactoMineR::PCA()`. To keep scores consistent with the
  fitted loadings and eigenvalues, `step_pca()` must be the final recipe step.
  Case-weighted fits fail explicitly until their weights can be propagated into
  individual contributions. Thanks to @erdeyl (#274).

* `get_clust_tendency()`: the Hopkins statistic now samples the observed points
  **without replacement** (the previous code sampled with replacement, which could
  draw the same observation more than once), counts a duplicated row as a valid
  zero-distance neighbour, and is computed on a normalized distance scale for
  numerical stability. Zero-range columns are ignored, so adding a redundant
  constant variable does not change the statistic. This changes the Hopkins value
  returned for a given `seed`.
  It now errors clearly when every nearest-neighbour distance is zero (the
  statistic is undefined). Thanks to @erdeyl (#274).

* `fviz_eig(parallel = TRUE)` (Horn's parallel analysis, an opt-in overlay):
  the simulated eigenvalue thresholds now use a corrected reference distribution.
  Covariance PCA (`prcomp(scale. = FALSE)` / `princomp(cor = FALSE)`) previously
  simulated unit-variance random data, giving statistically wrong thresholds; the
  reference now matches the fitted object's marginal variances. Full-rank
  correlation PCA is unaffected in distribution (parallel analysis is a
  Monte-Carlo procedure, so exact threshold values depend on the seed);
  wide (n <= p) fits now simulate over the original variable count. A
  rank-truncated correlation fit is accepted only when a retained formula call
  proves literal `scale. = TRUE`; default-method and custom/symbolic scale fits
  cannot be distinguished from the truncated object and fail closed. Other fits
  whose reference distribution cannot be reconstructed (uncentered fits,
  rank-truncated covariance fits, ambiguous `princomp` `cor`) also error clearly
  instead of returning misleading thresholds. Separately, `fviz_eig()` warns when a
  FactoMineR PCA object stores an incomplete eigenvalue spectrum (refit with a
  larger `ncp` for a complete scree plot). Thanks to @erdeyl (#274).

* `fviz_gap_stat()` (and `fviz_nbclust(method = "gap_stat")`): when a partial
  `maxSE` list is supplied without a `method`, the fallback is now `"firstSEmax"`
  (the documented default and `cluster::maxSE`'s default) instead of `"firstmax"`.
  This only affects callers who pass, e.g., `maxSE = list(SE.factor = 2)` with no
  `method`; the previous `"firstmax"` fallback silently ignored the `SE.factor`
  they set (that rule does not use it). Default calls and calls that pass a
  `method` are unchanged. `eclust()`'s internal gap default is aligned for
  consistency. Thanks to @erdeyl (#274).

## Minor changes

* `fviz_dend()`: corrected the documentation of the `type` argument, which listed
  a `"triangle"` value that the function does not accept (the valid values are
  `"rectangle"`, `"circular"` and `"phylogenic"`). Thanks to @Nelson-Gon (#144).
* `?fviz_dend` now documents how to compare two dendrograms (a tanglegram) with
  `dendextend::tanglegram()` / `untangle()` / `entanglement()`, which factoextra
  already depends on.
* Fixed a typo in the default title of `fviz_cos2()` / `fviz_contrib()` for
  quantitative variables ("quantitive" -> "quantitative").
* Clarified and corrected documentation across several help pages: the
  contribution-based selection help now says "highest contributions" (was
  "highest cos2"), `fviz_famd()`'s `habillage` help refers to a FAMD (not MFA)
  object, the ExPosition class name is spelled `expoOutput` in `fviz_ca()` /
  `fviz_mca()`, the generated `ggtheme` help no longer claims a single default
  that disagrees with function signatures, and the `clean_lock_files()` example
  is now self-contained. The HMFA selection help now includes the supported
  `union` option.
  Thanks to @erdeyl (#274).
* Clearer, earlier input validation for edge cases, so mistakes fail with an
  informative message instead of a downstream error: `fviz_umap()`/`fviz_tsne()`
  require two distinct positive integer `dims`; `fviz_nbclust()` validates
  `k.max`; `fviz_dend()` validates `k`/`h` against the tree; `get_clust_tendency()`
  checks `n` and requires finite data; oversized axis indices are rejected before
  integer conversion; and `as_factoextra_pca()` validates `scale.unit` and the
  supplied eigenvalues. Valid calls are unaffected.
  Thanks to @erdeyl (#274).
* `as_factoextra_pca()` now derives cos2, contributions, and eigenvalue
  percentages on rescaled intermediate values and infers omitted eigenvalues
  without premature overflow, so representable results remain stable at very
  small or very large magnitudes. Its PCA-variable print method lists only
  metrics that are actually available while preserving the established
  descriptions for metrics that remain present.
* `get_eigenvalue()` (and the `fviz_eig()` scree plot) computes the variance
  percentages on a rescaled intermediate, so an extreme-magnitude eigenvalue
  spectrum returns finite percentages instead of `NaN` or an overflow. Ordinary
  spectra are unchanged to floating-point precision.
* `get_pca_var()`: for `ade4` `dudi.pca` objects, the `coord` and `contrib`
  components are now returned as plain numeric matrices, matching the `prcomp`
  and `princomp` output; the values are unchanged.
* Refreshed the documentation links from the retired `sthda.com` pages to their
  current `datanovia.com` locations across the help pages and README.

## Bug fixes

* `fviz_dend()`: the `cex` argument now scales the leaf-label size for the
  `"rectangle"` and `"circular"` types. The label size was mapped through
  ggplot2's default continuous size scale, which collapsed a single per-plot
  `cex` value to a fixed size, so `cex` had no visible effect; leaf labels are
  now sized directly from `cex` (and a per-leaf `labels_cex` set through
  dendextend is honoured). At the default `cex = 0.8` the labels are marginally
  smaller than before. Thanks to @dir21 (#281).
* `fviz_ca_biplot()` / `fviz_ca()`: `invisible = "col.sup"` now hides
  supplementary columns (the column branch was keyed off the row-supplementary
  flag, so supplementary columns stayed visible). Thanks to @erdeyl (#274).
* `fviz_mclust()` now applies the `ggtheme` argument to every plot type
  (`"classification"`, `"uncertainty"`, `"BIC"`); it previously ignored it and
  always used `theme_classic()`. The default is unchanged. Thanks to @erdeyl (#274).
* `fviz_cluster()` now aligns a named clustering to the plotted data by row name
  when both carry complete, unique, matching names, so points are not
  mis-coloured when `data` is ordered differently from the clustering; it also
  accepts a `clustering` component (as produced by `pam()`/`clara()`) in a custom
  `list(data=, clustering=)` object. Assignments that do not line up by name are
  used positionally, as before, except that `pam()` / `fanny()` and `hcut()`
  objects fitted on a dissimilarity reject complete non-matching row-name sets
  because positional use would mis-colour observations. Thanks to @erdeyl (#274).
* `invisible = "all"` now hides all plotted elements (it was silently accepted
  but had no effect); on the individual and variable maps, a selection by `name`
  that includes names not present now warns instead of silently dropping them.
  `fviz_pca_var()` / `fviz_pca_biplot()` now draw a FactoMineR PCA's
  supplementary quantitative variables, which were previously omitted because the
  wrong result slot was read; their names are also recognised by the selection
  validation, so a `select.*` list naming one no longer warns or silently drops
  it. `select.ind` now also
  limits MFA/HMFA partial-point and segment overlays, including union selections,
  instead of leaving every individual's partial geometry visible. A lone
  unavailable contribution condition wrapped in `union = TRUE` now gives the
  same explicit unavailable-metric error as the ordinary single-condition path.
  Thanks to @erdeyl (#274).

# factoextra 2.1.0

## New features

### New functions

* New `as_factoextra_pca()` constructor builds a fviz-ready object from
  pre-computed coordinates, so the `fviz_pca_*()` family, `fviz_eig()`,
  `fviz_contrib()` and `fviz_cos2()` can visualize the output of **any**
  dimension-reduction method (e.g. `stats::cmdscale()`, `ape::pcoa()`, UMAP/t-SNE,
  `vegan::rda()`/`cca()`, or a custom analysis) without writing a backend. Supply
  `ind.coord` (and optionally `var.coord`, `eig`); `cos2`/`contrib` are derived
  from the coordinates when not provided. (#57, #75, #136)

### New arguments

* New `shape.ind` argument in `fviz_pca_ind()` and `fviz_pca_biplot()` maps point
  **shape** to a grouping factor independently of colour, so individuals can be
  coloured by one variable and shaped by another (e.g.
  `fviz_pca_ind(res, col.ind = group1, shape.ind = group2)`). `fviz()` likewise
  now accepts a factor for `pointshape`. Default behaviour is unchanged. (#36, #51)
* New `rotate.labels` argument in `fviz()` (and thus `fviz_pca_var()`,
  `fviz_pca_biplot()`, etc.): when `TRUE`, variable text labels are rotated to
  the angle of their arrows (ggbiplot style). Defaults to `FALSE` (unchanged);
  use with `repel = FALSE`. (#98)
* `fviz_pca_var()`/`fviz_pca_biplot()` (via `fviz()`) gain an `add.circle` argument to
  force (`TRUE`) or suppress (`FALSE`) the variable correlation circle. Default `NULL`
  keeps the previous automatic behavior (shown only for unit-variance PCA). Useful when
  scaling manually and fitting `prcomp(scale = FALSE)`. (#88)
* `fviz_pca_var()` / `fviz_pca_biplot()` (and other arrow plots) gain an
  `arrow.linetype` argument to set the variable-arrow line type (e.g. `"dashed"`).
  Default `"solid"` reproduces the previous appearance. (#73)
* `fviz_dend()` gains a `labels_font` argument ("plain"/"bold"/"italic"/"bold.italic")
  to set the leaf-label font face, e.g. italic species names. Default "plain" leaves
  labels unchanged. (#121)
* `fviz_dend()` gains a `match_coord_colors` argument (default `FALSE`). When `TRUE`,
  cluster colours are remapped from left-to-right leaf order to cluster-label order,
  so a dendrogram's colours match `fviz_cluster()`/`fviz_silhouette()` for the same
  clustering. Default `FALSE` keeps the previous colouring unchanged. (#103)
* `fviz_dend()` now renders the `sub` argument as a plot subtitle. `sub`
  defaults to `NULL` (no subtitle, as before); set it to a string to add one. (#54)

### New backend and input support

* PCA extractors and `fviz_pca_*()` now support **ade4 between-class and
  within-class PCA** (`ade4::bca()` / `ade4::wca()`). Individual contributions
  match `ade4::inertia.dudi()`. (#126)
* `fviz_nbclust()` now accepts a precomputed dissimilarity (`"dist"`) as `x` for
  `method = "silhouette"`/`"wss"`, passing it to dissimilarity-capable `FUNcluster`s
  (e.g. `cluster::pam`, `factoextra::hcut`). Non-diss methods (kmeans/clara) and
  `method = "gap_stat"` raise a clear error instead of mis-clustering. (#90)
* `eclust()` gains `"hkmeans"` as a `FUNcluster` option, so hierarchical k-means
  can be run through the same enhanced-clustering interface (with gap-statistic
  `k` selection, silhouette info, and plotting). (#78)
* `eclust()` now accepts a precomputed dissimilarity matrix (an object of class
  `"dist"`) as `x` for hierarchical clustering (`"hclust"`, `"agnes"`,
  `"diana"`), so custom distances such as Bray-Curtis
  (e.g. `vegan::vegdist(df, "bray")`) can be used. In that case `hc_metric` is
  ignored and `k` must be supplied. Previously a `"dist"` input was silently
  recomputed as a Euclidean distance. (#182)
* `get_famd()`, `get_mfa()`, `facto_summarize()`, `fviz_famd_*()`, and
  `fviz_mfa_*()` now support supplementary qualitative variable categories
  via `quali.sup`, including overlay, print, and category-name compatibility
  paths. Regression coverage and examples expanded accordingly. (#202,
  @erdeyl)

## Main changes

### Documentation

* New vignette **"Extending factoextra to support new analysis backends"**
  documenting the `get_*()` / `fviz_*()` extension contract (which functions to
  edit and the standardized return structure), with ExPosition as a worked
  example and a vegan (RDA/CCA) template, plus a "Quick start: bring your own
  coordinates" section for `as_factoextra_pca()`. (#23)

### Validation and messaging

* `fviz_*()` now **warn** when `label` or `invisible` contain an unrecognized
  value (e.g. `label = "id"` instead of `"ind"`) and list the valid values,
  instead of silently drawing nothing. Recognized values are unchanged. (#165)
* `get_dist()`, `eclust()`, `hcut()`, and `fviz_cluster()` reject `stand = TRUE`
  scaling that produces `NA` values with a package-level error instead of
  leaking low-level clustering or plotting failures. (#209, @erdeyl)
* `fviz_dist()` and `hcut(isdiss = TRUE)` reject non-finite distance objects.
  (#209, @erdeyl)
* `fviz_eig()` validates `ncp`, `parallel.iter`, and `parallel.seed` before
  plotting. Integer-like numeric values are accepted; fractional, `NA`, or
  out-of-range values are rejected. (#209, @erdeyl)
* `facto_summarize()` and the `fviz_*` axis helpers reject `NA`, zero,
  negative, or fractional axis indices consistently. (#209, @erdeyl)
* `get_mca_var()` reports missing quantitative supplementary MCA variables
  with a package-level error. (#209, @erdeyl)

### Compatibility

* `fviz_dend()` uses current `igraph` phylogenic helpers internally while
  keeping `phylo_layout = "layout.auto"`, `"layout.gem"`, and `"layout.mds"`
  as backward-compatible aliases. Modern names `"layout_nicely"`,
  `"layout_with_gem"`, and `"layout_with_mds"` are also accepted. (#209,
  @erdeyl)

## Minor changes

* `alpha.var`/`alpha.ind` (and `alpha`) now also fade the variable/individual text
  labels, not just the points and arrows, in `fviz_pca_*()` and the other biplots.
  Only a numeric `alpha < 1` is affected; the default (`alpha = 1`) is unchanged. (#130)
* Clearer error when a `col`/`fill` vector passed to a `fviz_*` plot has the wrong length
  (e.g. colouring variables in `fviz_pca_var()` by an observation-level group): the message
  now explains the length must match the elements plotted (individuals for `fviz_*_ind()`,
  variables for `fviz_*_var()`) or be a metric. (#139)
* Help pages, examples, README, and package metadata refreshed to document
  the new helper-level `k = 1` paths, the stricter validation surface across
  the clustering, MCA, and eigenvalue helpers, and the phylogenic layout
  compatibility surface. (#209, @erdeyl)

## Bug fixes

* `hcut()` and `hkmeans()` no longer pre-empt `k > number-of-observations` with a
  custom error; the native `stats::cutree()` error
  (`"elements of 'k' must be between 1 and N"`) is restored. This keeps backward
  compatibility with the published behaviour and fixes the **chooseGCM** reverse
  dependency on CRAN.
* `get_pca_ind()` now works for **ade4 `dudi.pca`** objects. Their `$li`/`$tab`
  are data frames, which previously collapsed the internal `cos2` matrix into a
  list and raised "attempt to set 'colnames' on an object with less than two
  dimensions"; `fviz_pca_ind()` on a `dudi.pca` failed as a result. `prcomp`/
  `princomp` output is unchanged. (#126)
* `fviz_dend()` now honors an explicit `k` for `HCPC` objects (e.g.
  `fviz_dend(res.hcpc, k = 5)`); previously the user-supplied `k` was silently
  overwritten by the HCPC cluster count. With `k = NULL` (default) the behavior
  is unchanged. (#81)
* `fviz_mca_biplot()` now forwards the `map` argument to the individuals and
  variable categories, so asymmetric maps (e.g. `"rowprincipal"`,
  `"colprincipal"`, `"symbiplot"`) take effect instead of always drawing the
  symmetric map. (#142)
* `fviz_dend()`: `lwd` now controls ggplot branch thickness correctly and no
  longer triggers a spurious linewidth legend. (#200, @erdeyl)
* `fviz_nbclust()` computes the `k = 1` WSS baseline internally, so helpers
  such as `hcut()` and `hkmeans()` no longer crash when used as `FUNcluster`.
  `eclust()` handles hierarchical auto-selected `k = 1` results and preserves
  observation names; `fviz_nbclust(method = "silhouette")` omits the undefined
  `k = 1` point and keeps the optimum guide aligned with the displayed cluster
  count. `fviz_silhouette()` errors cleanly when silhouette information is
  unavailable (one-cluster `eclust`/`hcut` results). (#203, #209, @erdeyl)
* `print()` for HMFA group results lists only available components, dropping
  the blank placeholder rows from the previous fixed-size layout. (#209,
  @erdeyl)
* `get_pca_ind()` returns `cos2 = 0` (not `NaN`) for rows at the PCA centroid;
  `fviz_pca_biplot(biplot.type = "auto")` falls back to a safe scaling ratio
  when variable coordinates are degenerate. (#209, @erdeyl)
* `get_pca_var()` strips the base R `"loadings"` S3 class from results for
  `princomp()` objects, so `coord`, `cor`, `cos2`, and `contrib` are returned as
  plain numeric matrices. Previously they inherited the `"loadings"` class, whose
  `print()` method hid values with `|x| < 0.1` and broke downstream manipulation.
  (#212)
* FAMD/MFA plots (`fviz_contrib()`, `fviz_famd_ind()`, `fviz_famd_var()`) no
  longer error with `duplicate 'row.names' are not allowed` when qualitative
  variables share factor-level names (e.g. several variables with `Low`/`High`).
  Colliding categories are relabelled `variable_level` (e.g. `Acidity_Low`);
  non-colliding labels are unchanged. (#184, #140)
* `fviz_dend(rect = TRUE)`: the default `lower_rect` (rectangle depth) now scales
  with tree height for short trees (max height < 1, e.g. correlation/gower distances),
  where the previous fixed -0.5 offset pushed rectangles far below the labels. Taller
  trees keep the previous default. (#55)
* `fviz_dend(rect = TRUE)` no longer errors ("Aesthetics must be either length 1
  or the same as the data") for dendrograms with tied merge heights; the cluster
  rectangles now always match `k`, and `rect_border`/`rect_fill` colour vectors are
  recycled to `k`. (#154, #168)
* `fviz_mclust_bic()`: the red "optimal clusters" line now lands on the correct cluster
  when the model's cluster counts don't start at 1 (e.g. a restricted `G` range or a
  noise/outlier model). It previously used the numeric `G` as a position on the discrete
  x-axis; standard `G = 1:k` models are unaffected. (#116)
* `fviz_cluster()` no longer leaks its point-label text layer into the legend (the stray
  `a` key), matching the scatter and dendrogram plots. (#14)
* `fviz_dend()` no longer leaks its leaf-label text layer into the legend
  (the stray `a`/`cex` key), matching the scatter-plot cleanup. (#14)
* `fviz_cluster()` now plots `pam()`/`fanny()` results fitted on a dissimilarity
  matrix (`diss = TRUE`) when the original data is passed via `data=` (previously the
  user's `data=` was overwritten by the object's empty data slot, erroring with "'data'
  must be of vector type, was NULL"). The clusters come from the object; `data=` is used
  only for the 2-D layout. A clear message is shown if no data is available. (#128)
* Point/individual labels no longer add a stray `a` glyph to the colour/fill
  legend (e.g. `fviz_pca_ind(..., habillage = )`). Text layers are now excluded
  from the legend keys; labels still appear on the plot. (#14)

# factoextra 2.0.0

Major modernization release after 6 years. Resolves 30+ open issues and aligns
with the current R/ggplot2/FactoMineR ecosystem.

## Breaking Changes

* **R >= 4.1.0** required (was >= 3.1.2).
* **ggplot2 >= 3.5.2** required (previously no minimum).
* **FactoMineR >= 2.13** required.
* `get_clust_tendency()`: Hopkins statistic now uses the corrected Wright (2022)
  formula — values will differ from earlier versions. Default `seed` changed
  from `123` to `NULL`.
* **tidyr and reshape2 no longer needed** — replaced with base R internally.

## ggplot2 Compatibility Fixes

* Replaced all `aes_string()` with `aes()` + `.data` pronoun (rlang).
  (#188, #190, #191, #192)
* Replaced `guides(... = FALSE)` with `guides(... = "none")`.
  (#141, #156, #174, #179, #183)
* Replaced `size` with `linewidth` for line-based geoms. (#178, #191)

## Bug Fixes

* `fviz_dend()`: fixed `rect_border` error and "condition has length > 1" crash.
  (#151, #163, #180)
* `fviz_nbclust()`: fixed class-comparison crash, cluster ordering for k > 9,
  and silhouette error when k >= n. (#113, #131, #147, #148, #149, #171)
* `facto_summarize()`: axes parameter now correctly selects requested dimensions.
  (#120, #143, #166, #167)
* `fviz_ca_col()`: fixed `col.col.sup` parameter name typo. (#150)
* `fviz_pca_biplot()`: fixed rescaling. (#129)
* `get_clust_tendency()`: fixed biased Hopkins sampling. (#133)
* `.onAttach()`: startup message no longer falsely claims ggpubr/FactoMineR
  are "loaded" (they are imported, not attached).
* `.add_ind_groups()`: no longer crashes with single-column habillage data frame.

## New Features

* `fviz_eig()`: parallel analysis support (Horn 1965).
* `fviz_pca_biplot()`: scaling types (Gabriel 1971).
* `get_clust_tendency()`: stricter input validation, RNG state preservation,
  `options(factoextra.warn_hopkins = FALSE)` to suppress one-time warning.
* `hcut()` and `hkmeans()`: improved input checks and error handling.
  Redundant `k >= n` checks removed to preserve backward-compatible error
  messages from `cutree()` / `kmeans()` (avoids breaking reverse dependencies
  such as `chooseGCM`). ([@erdeyl, #199](https://github.com/kassambara/factoextra/pull/199))
* Expanded `testthat` test suite (113 tests).
  ([@erdeyl, #199](https://github.com/kassambara/factoextra/pull/199))
* GitHub Actions R-CMD-check workflow.

## Internal

* CA/MCA/PCA extractor internals refactored to vectorized, type-stable code.
* `factominer_category_map()` and `map_factominer_legacy_names()` helpers
  for FactoMineR category-name compatibility.
* `clean_lock_files()` helper for removing stale `00LOCK-*` directories.
* Added `LazyDataCompression: xz` for CRAN compliance.
* Laszlo Erdey added as contributor.


# factoextra 1.0.7

## Minor changes

- Adding stringsAsFactors = TRUE to the relevant calls to data.frame to anticipate compatibility with future R 4.0.0


# factoextra 1.0.6

## Minor changes

- the function `fviz_nbclust()` checks now whether the argument `FUNcluster` is correctly specified ([@robsalasco, #82](https://github.com/kassambara/factoextra/issues/82)).
- Clusters are now correctly order in `fviz_mclust_bic()` ([@hpsprecher, #84](https://github.com/kassambara/factoextra/issues/84))
- New arguments `outlier.pointsize` and `outlier.labelsize` added in `fviz_cluster()` to customize outliers detected with DBSCAN ([@choonghyunryu, #74](https://github.com/kassambara/factoextra/issues/74))
- `pointsize` in the function `fviz()` can now be a continuous variable.

## Bug fixes

- Now `hkmeans()` takes other distance metrics ([@santsang, #52](https://github.com/kassambara/factoextra/issues/52))
- `get_clust_tendency()` updated to return the correct value of hopkins statistics as explained at: https://www.datanovia.com/en/lessons/assessing-clustering-tendency/


# factoextra 1.0.5
     
## Bug fixes
   
- Now, the argument `invisible` works properly in the function `fviz_pca_biplot()`([@ginolhac, #26](https://github.com/kassambara/factoextra/issues/26)).
- The function `fviz_dend()` now works for an object of class `diana` ([@qfazille, #30](https://github.com/kassambara/factoextra/issues/30)).
- Now, `fviz_cluster()` supports HCPC results ([@famuvie, #34](https://github.com/kassambara/factoextra/issues/34)).
    
## Minor changes
   
- New argument `mean.point` in the function `fviz()`. logical value. If TRUE, group mean points are added to the plot.
- Now, PCA correlation circles have fixed coordinates so they don't appear as ellipses ([@scoavoux, #38](https://github.com/kassambara/factoextra/pull/38).
- New argument `fill.ind` and `fill.var` added in `fviz_pca()` ([@ginolhac, #27](https://github.com/kassambara/factoextra/issues/27) and [@Confurious, #42](https://github.com/kassambara/factoextra/issues/42)).
- New arguments `geom.ind` and `geom.var` in `fviz_pca_xxx()` and `fviz_mca_xxx()` functions to have more controls on the individuals/variables geometry in the functions `fviz_pca_biplot()` and `fviz_mca_biplot()` ([@Confurious, #42](https://github.com/kassambara/factoextra/issues/42)).
- New arguments `geom.row` and `geom.col`  in `fviz_ca_xxx()` functions to have more controls on the individuals/variables geometry in the function `fviz_ca_biplot()` ([@Confurious, #42](https://github.com/kassambara/factoextra/issues/42)).
- New argument `gradient.cols` in `fviz_pca_biplot()`
- New argument `axes` in `fviz_cluster()` to specify the dimension to plot.
- New argument `circlesize` in the function `fviz()` to change the size of the variable correlation circle size.
   
- It's now possible to color individuals using a custom continuous variable ([#29](https://github.com/kassambara/factoextra/issues/29)). This is done using the argument **col.ind**.


```r
library(factoextra)
data(iris)
res.pca <- prcomp(iris[, -5],  scale = TRUE)

# Visualize and color by a custom continuous variable
fviz_pca_ind(res.pca, col.ind = iris$Sepal.Length,
             legend.title = "Sepal.Length")
```
   
   
- factoextra can now handle Japanese characters by using the argument font.family = "HiraKakuProN-W3"` ([#31](https://github.com/kassambara/factoextra/issues/31)). For example:
    
    
```r
library(FactoMineR)
library(factoextra)

.tbl2.1 <- matrix(c(395, 2456,1758,
                    147, 153, 916, 
                    694, 327, 1347),byrow=T,3,3)
dimnames(.tbl2.1) <- list(地域=c("オスロ","中部地域","北部地域"),
                            犯罪=c("強盗", "詐欺","破壊") )


res.CA <- CA(.tbl2.1,graph=FALSE)

fviz_ca_biplot(res.CA,map="simbiplot",title="simbiplot",
               font.family = "HiraKakuProN-W3")
```


   
# factoextra 1.0.4

## New features
    
- New function `fviz_mclust()` for plotting model-based clustering using ggplot2.
   
- New function `fviz()`: Generic function to create a scatter plot of multivariate analysis outputs, including PCA, CA and MCA, MFA, ...
   
- New functions `fviz_mfa_var()` and `fviz_hmfa_var()` for plotting MFA and HMFA variables, respectively.  
     
- New function `get_mfa_var()`: Extract the results for variables (quantitatives, qualitatives and groups). Deprecated functions: `get_mfa_var_quanti()`, `get_mfa_var_quali()` and `get_mfa_group()`.
    
- New functions added for extracting and visualizing the results of FAMD (factor analysis of mixed data): `get_famd_ind()`, `get_famd_var()`, `fviz_famd_ind()` and `fviz_famd_var()`.
   
- Now `fviz_dend()` returns a ggplot. It can be used to plot circular dendrograms and phylogenic-like trees. Additionally, it supports an object of class HCPC (from FactoMineR).
    
- New arguments in `fviz_cluster()`:
    - main, xlab, ylab in `fviz_cluster()`: to change the plot main title and axis labels.
    - ellipse, ellipse.type, ellipse.level and ellipse.alpha
    - choose.vars: a character vector containing variables to be considered for plotting.
       

- New argument pointshape in `fviz_pca()`. When you use habillage, point shapes change automatically by groups. To avoid this behaviour use for example pointshape = 19 in combination with habillage ([@raynamharris, #15](https://github.com/kassambara/factoextra/issues/20)).
- New argument repel in `fviz_add()`.
- New argument gradient.cols in fviz_*() functions.
    
- Support for the ExPosition package added (epCA, epPCA, epMCA) ([#23](https://github.com/kassambara/factoextra/issues/23))
     
## Minor changing
   
- Check point added in the function `fviz_nbclust()` to make sure that x is an object of class data.frame or matrix ([Jakub Nowosad, #15](https://github.com/kassambara/factoextra/issues/15)).
- The following arguments are deprecated in `fviz_cluster`(): title, frame, frame.type, frame.level, frame.alpha. Now, use main, ellipse, ellipse.type, ellipse.level and ellipse.alpha instead.
  
- Now, by default, the function `fviz_cluster`() doesn't show cluster mean points for an object of class PAM and CLARA, when the argument show.clust.cent is missing . This is because cluster centers are medoids in the case of PAM and CLARA but not means. However, user can force the function to display the mean points by using the argument show.clust.cent = TRUE.  
   
- The argument jitter is deprecated; use repel = TRUE instead, to avoid overlapping of labels.
  
- New argument "sub" in `fviz_dend()` for adding a subtitle to the dendrogram. If NULL, the method used hierarchical clustering is shown. To remove the subtitle use sub = "".

   
## Bug fixes

- Now `fviz_cluster()` can handle HCPC object obtained from MCA ([Alejandro Juarez-Escario, #13](https://github.com/kassambara/factoextra/pull/13))
- Now `fviz_ca_biplot()` reacts when repel = TRUE used
- In `facto_summarize()`, now the contribution values computed for >=2 axes are in percentage ([#22](https://github.com/kassambara/factoextra/issues/22))
- `fviz_ca()` and `fviz_mca()` now work with the latest version of ade4 v1.7-5 ([#24](https://github.com/kassambara/factoextra/issues/24))
     

# factoextra 1.0.3
  
  
## NEW FEATURES


* New fviz_mfa function to plot MFA individuals, partial individuals, quantitive variables, categorical variables, groups relationship square and partial axes ([@inventionate, #4](https://github.com/kassambara/factoextra/pull/4)).

* New fviz_hmfa function to plot HMFA individuals, quantitive variables, categorical variables and groups relationship square ([@inventionate, #4](https://github.com/kassambara/factoextra/pull/4)).
  
* New get_mfa and get_hmfa function ([@inventionate, #4](https://github.com/kassambara/factoextra/pull/4)).

* fviz_ca, fviz_pca, fviz_mca, fviz_mfa and fviz_hmfa ggrepel support ([@inventionate, #4](https://github.com/kassambara/factoextra/pull/4)).
  
* Updated fviz_summarize, eigenvalue, fviz_contrib and fviz_cos2 functions, to compute FactoMineR MFA and HMFA results ([@inventionate, #4](https://github.com/kassambara/factoextra/pull/4)).


* fviz_cluster() added. This function can be used to visualize the outputs of clustering methods including:  kmeans() [stats package]; pam(), clara(), fanny() [cluster package]; dbscan() [fpc package]; Mclust() [mclust package]; HCPC() [FactoMineR package]; hkmeans() [factoextra].

* fviz_silhouette() added. Draws the result of cluster silhouette analyses computed using the function silhouette()[cluster package] 

* fviz_nbclust(): Determines and visualizes the optimal number of clusters

* fviz_gap_stat(): Visualize the gap statistic generated by the function clusGap() [in cluster package]

* hcut(): Computes hierarchical clustering and cut the tree into k clusters. 

* hkmeans(): Hierarchical k-means clustering. Hybrid approach to avoid the initial random selection of cluster centers.

* get_clust_tendency(): Assessing clustering tendency

* fviz_dend(): Enhanced visualization of dendrogram

* eclust(): Visual enhancement of clustering analysis

* get_dist() and fviz_dist(): Enhanced Distance Matrix Computation and Visualization

* eclust(): Visual enhancement of clustering analysis


## MINOR CHANGING

* Require R >= 3.1.0
* A dataset named "multishapes" has been added. It contains clusters of multiple shapes. Useful for comparing density-based clustering and partitioning methods such as k-means
* The argument jitter is added to the functions fviz_pca(), fviz_mca() and fviz_ca() and fviz_cluster() in order to reduce overplotting of points and texts
* The functions fviz_*() now use ggplot2::stat_ellipse() for drawing ellipses.

## BUG FIXES
    
    
- Unknown parameters "shape" removed from geom_text ([@bdboy, #5](https://github.com/kassambara/factoextra/issues/5))


# factoextra 1.0.2


## NEW FEATURES
   
* Visualization of Correspondence Analysis outputs from different R packages (FactoMineR, ca, ade4, MASS)
- fviz_ca_row()
- fviz_ca_col()
- fviz_ca_biplot()

* Extract results from CA output
- get_ca_row()
- get_ca_col()
- get_ca()

* Visualize the cos2 and the contributions of rows/columns. The functions can handle the output of PCA, CA and MCA
- fviz_cos2()
- fviz_contrib()

* Sumarize the results of PCA, CA, MCA
- facto_summarize()


## DEPRECATED FUNCTION

* fviz_pca_contrib() is deprecated -> use fviz_contrib()
 

## MINOR CHANGING

* fviz_add: "text" are included in the allowed values for the argument geom
* fviz_screeplot: the X parameter can be also an object of class ca [ca], coa [ade4], correspondence [MASS]
* get_eigenvalue: X parameters and description changed
* get_pca_ind: the argument data are no longer required


# factoextra 1.0.1

## FEATURES

* Easy to use functions to extract and visualize the output of principal component analysis.
