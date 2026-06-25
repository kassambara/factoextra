# factoextra 2.0.0.999

## Documentation

* New vignette **"Extending factoextra to support new analysis backends"**
  documenting the `get_*()` / `fviz_*()` extension contract (which functions to
  edit and the standardized return structure), with ExPosition as a worked
  example and a vegan (RDA/CCA) template. (#23)

## New Features

* PCA extractors and `fviz_pca_*()` now support **ade4 between-class and
  within-class PCA** (`ade4::bca()` / `ade4::wca()`). Individual contributions
  match `ade4::inertia.dudi()`. (#126)

## Bug Fixes

* `get_pca_ind()` now works for **ade4 `dudi.pca`** objects. Their `$li`/`$tab`
  are data frames, which previously collapsed the internal `cos2` matrix into a
  list and raised "attempt to set 'colnames' on an object with less than two
  dimensions"; `fviz_pca_ind()` on a `dudi.pca` failed as a result. `prcomp`/
  `princomp` output is unchanged. (#126)
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
* `alpha.var`/`alpha.ind` (and `alpha`) now also fade the variable/individual text
  labels, not just the points and arrows, in `fviz_pca_*()` and the other biplots.
  Only a numeric `alpha < 1` is affected; the default (`alpha = 1`) is unchanged. (#130)
* `fviz_cluster()` now plots `pam()`/`fanny()` results fitted on a dissimilarity
  matrix (`diss = TRUE`) when the original data is passed via `data=` (previously the
  user's `data=` was overwritten by the object's empty data slot, erroring with "'data'
  must be of vector type, was NULL"). The clusters come from the object; `data=` is used
  only for the 2-D layout. A clear message is shown if no data is available. (#128)
* Clearer error when a `col`/`fill` vector passed to a `fviz_*` plot has the wrong length
  (e.g. colouring variables in `fviz_pca_var()` by an observation-level group): the message
  now explains the length must match the elements plotted (individuals for `fviz_*_ind()`,
  variables for `fviz_*_var()`) or be a metric. (#139)
* Point/individual labels no longer add a stray `a` glyph to the colour/fill
  legend (e.g. `fviz_pca_ind(..., habillage = )`). Text layers are now excluded
  from the legend keys; labels still appear on the plot. (#14)

## Input Validation

* `get_dist()`, `eclust()`, `hcut()`, and `fviz_cluster()` reject `stand = TRUE`
  scaling that produces `NA` values with a package-level error instead of
  leaking low-level clustering or plotting failures. (#209, @erdeyl)
* `fviz_dist()` and `hcut(isdiss = TRUE)` reject non-finite distance objects.
  (#209, @erdeyl)
* `hcut()` and `hkmeans()` reject `k > n_obs` early with a clear message.
  (#209, @erdeyl)
* `fviz_eig()` validates `ncp`, `parallel.iter`, and `parallel.seed` before
  plotting. Integer-like numeric values are accepted; fractional, `NA`, or
  out-of-range values are rejected. (#209, @erdeyl)
* `facto_summarize()` and the `fviz_*` axis helpers reject `NA`, zero,
  negative, or fractional axis indices consistently. (#209, @erdeyl)
* `get_mca_var()` reports missing quantitative supplementary MCA variables
  with a package-level error. (#209, @erdeyl)

## New Features

* `fviz_dend()` now renders the `sub` argument as a plot subtitle. `sub`
  defaults to `NULL` (no subtitle, as before); set it to a string to add one. (#54)
* `fviz_pca_var()`/`fviz_pca_biplot()` (via `fviz()`) gain an `add.circle` argument to
  force (`TRUE`) or suppress (`FALSE`) the variable correlation circle. Default `NULL`
  keeps the previous automatic behavior (shown only for unit-variance PCA). Useful when
  scaling manually and fitting `prcomp(scale = FALSE)`. (#88)
* `fviz_pca_var()` / `fviz_pca_biplot()` (and other arrow plots) gain an
  `arrow.linetype` argument to set the variable-arrow line type (e.g. `"dashed"`).
  Default `"solid"` reproduces the previous appearance. (#73)
* `fviz_nbclust()` now accepts a precomputed dissimilarity (`"dist"`) as `x` for
  `method = "silhouette"`/`"wss"`, passing it to dissimilarity-capable `FUNcluster`s
  (e.g. `cluster::pam`, `factoextra::hcut`). Non-diss methods (kmeans/clara) and
  `method = "gap_stat"` raise a clear error instead of mis-clustering. (#90)
* `fviz_dend()` gains a `labels_font` argument ("plain"/"bold"/"italic"/"bold.italic")
  to set the leaf-label font face, e.g. italic species names. Default "plain" leaves
  labels unchanged. (#121)
* `fviz_dend()` gains a `match_coord_colors` argument (default `FALSE`). When `TRUE`,
  cluster colours are remapped from left-to-right leaf order to cluster-label order,
  so a dendrogram's colours match `fviz_cluster()`/`fviz_silhouette()` for the same
  clustering. Default `FALSE` keeps the previous colouring unchanged. (#103)
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

## Compatibility

* `fviz_dend()` uses current `igraph` phylogenic helpers internally while
  keeping `phylo_layout = "layout.auto"`, `"layout.gem"`, and `"layout.mds"`
  as backward-compatible aliases. Modern names `"layout_nicely"`,
  `"layout_with_gem"`, and `"layout_with_mds"` are also accepted. (#209,
  @erdeyl)

## Internal

* Help pages, examples, README, and package metadata refreshed to document
  the new helper-level `k = 1` paths, the stricter validation surface across
  the clustering, MCA, and eigenvalue helpers, and the phylogenic layout
  compatibility surface. (#209, @erdeyl)

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
- `pointsize` in the function `fviz()` canbe now a continuous variable.

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
- New argument `àxes` in `fviz_cluster`() to specify the dimension to plot.
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
   
- New function `fviz()`: Generic function to create a scatter plot of multivariate analyse outputs, including PCA, CA and MCA, MFA, ...
   
- New functions `fviz_mfa_var()` and `fviz_hmfa_var()` for plotting MFA and HMFA variables, respectively.  
     
- New function `get_mfa_var()`: Extract the results for variables (quantitatives, qualitatives and groups). Deprecated functions: `get_mfa_var_quanti()`, `get_mfa_var_quali()` and `get_mfa_group()`.
    
- New functions added for extracting and visualizing the results of FAMD (factor analysis of mixed data): `get_famd_ind()`, `get_famd_var()`, `fviz_famd_ind()` and `fviz_famd_var()`.
   
- Now `fviz_dend()` returns a ggplot. It can be used to plot circular dendrograms and phylogenic-like trees. Additionnally, it supports an object of class HCPC (from FactoMineR).  
    
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

* fviz_nbclust(): Dertemines and visualize the optimal number of clusters

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

* fviz_pca_contrib() is dreprecated -> use fviz_contrib()
 

## MINOR CHANGING

* fviz_add: "text" are included in the allowed values for the argument geom
* fviz_screeplot: the X parameter can be also an object of class ca [ca], coa [ade4], correspondence [MASS]
* get_eigenvalue: X parameters and description changed
* get_pca_ind: the argument data are no longer required


# factoextra 1.0.1

## FEATURES

* Easy to use functions to extract and visualize the output of principal component analysis.
