factoextra : Extract and Visualize the Results of Multivariate Data Analyses
=============================================================================

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/factoextra)](https://cran.r-project.org/package=factoextra)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/factoextra)](https://cran.r-project.org/package=factoextra)

[**factoextra**](http://www.sthda.com/english/rpkgs/factoextra) is an R package making easy to *extract* and *visualize* the output of exploratory **multivariate data analyses**, including:

1.  [Principal Component Analysis (PCA)](http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/), which is used to summarize the information contained in a continuous (i.e, quantitative) multivariate data by reducing the dimensionality of the data without loosing important information.

2.  [Correspondence Analysis (CA)](http://www.sthda.com/english/wiki/correspondence-analysis-in-r-the-ultimate-guide-for-the-analysis-the-visualization-and-the-interpretation-r-software-and-data-mining), which is an extension of the principal component analysis suited to analyse a large contingency table formed by two *qualitative variables* (or categorical data).

3.  [Multiple Correspondence Analysis (MCA)](http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining), which is an adaptation of CA to a data table containing more than two categorical variables.

4.  [Multiple Factor Analysis (MFA)](http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_mfa.html) dedicated to datasets where variables are organized into groups (qualitative and/or quantitative variables).

5.  [Hierarchical Multiple Factor Analysis (HMFA)](http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_hmfa.html): An extension of MFA in a situation where the data are organized into a hierarchical structure.

6.  [Factor Analysis of Mixed Data (FAMD)](http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_famd.html), a particular case of the MFA, dedicated to analyze a data set containing both quantitative and qualitative variables.

There are a number of R packages implementing principal component methods. These packages include: *FactoMineR*, *ade4*, *stats*, *ca*, *MASS* and *ExPosition*.

However, the result is presented differently according to the used packages. To help in the interpretation and in the visualization of multivariate analysis - such as cluster analysis and dimensionality reduction analysis - we developed an easy-to-use R package named [factoextra](http://www.sthda.com/english/rpkgs/factoextra).

-   The R package **factoextra** has flexible and easy-to-use methods to extract quickly, in a human readable standard data format, the analysis results from the different packages mentioned above.
-   It produces a **ggplot2**-based **elegant data visualization** with less typing.
-   It contains also many functions facilitating clustering analysis and visualization.

> We'll use i) the FactoMineR package (Sebastien Le, et al., 2008) to compute PCA, (M)CA, FAMD, MFA and HCPC; ii) and the factoextra package for extracting and visualizing the results.

The figure below shows methods, which outputs can be visualized using the factoextra package. The official online documentation is available at: <http://www.sthda.com/english/rpkgs/factoextra>.

![factoextra R package](tools/factoextra-r-package.png)

Why using factoextra?
---------------------

1.  The *factoextra* R package can handle the results of PCA, CA, MCA, MFA, FAMD and HMFA from several packages, for extracting and visualizing the most important information contained in your data.

2.  *After PCA, CA, MCA, MFA, FAMD and HMFA, the most important row/column elements* can be highlighted using :

-   their cos2 values corresponding to their quality of representation on the factor map
-   their contributions to the definition of the principal dimensions.

Installing FactoMineR
---------------------

The FactoMineR package can be installed and loaded as follow:

``` r
# Install
install.packages("FactoMineR")

# Load
library("FactoMineR")
```

Installing and loading factoextra
---------------------------------

``` r
# Install from CRAN
install.packages("factoextra")

# Or install the latest development version from GitHub
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
```

-   Load factoextra as follow:

``` r
library("factoextra")
```

Documentation
-------------

Complete documentation is available at: <http://www.sthda.com/english/rpkgs/factoextra>

For a complete list of changes in version 1.0.8, see the [NEWS.md](NEWS.md) file.
