[![Build Status](https://api.travis-ci.org/kassambara/factoextra.png)](https://travis-ci.org/kassambara/factoextra) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/factoextra)](https://cran.r-project.org/package=factoextra) [![Downloads](http://cranlogs.r-pkg.org/badges/factoextra)](https://cran.r-project.org/package=factoextra) [![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/factoextra?color=orange)](http://cranlogs.r-pkg.org/badges/grand-total/factoextra) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Pending Pull-Requests](http://githubbadges.herokuapp.com/kassambara/factoextra/pulls.svg?style=flat)](https://github.com/kassambara/factoextra/pulls) [![Github Issues](http://githubbadges.herokuapp.com/kassambara/factoextra/issues.svg)](https://github.com/kassambara/factoextra/issues)

factoextra : Extract and Visualize the Results of Multivariate Data Analyses
============================================================================

Provides some easy-to-use functions to extract and visualize the output of multivariate data analyses, including PCA (Principal Component Analysis), CA (Correspondence Analysis), MCA (Multiple Correspondence Analysis), FAMD (Factor Analysis of Mixed Data), MFA (Multiple Factor Analysis) and HMFA (Hierarchical Multiple Factor Analysis) functions from several packages : PCA, CA, MCA, MFA, HMFA \[FactoMineR\]; prcomp and princomp \[stats\]; dudi.pca, dudi.coa, dudi.acm \[ade4\]; ca \[ca\]; corresp \[MASS\]; epPCA, epCA, epMCA \[ExPosition\]. It contains also many functions for simplifying clustering analysis workflows. The ggplot2 plotting system is used. See <http://www.sthda.com/english/rpkgs/factoextra> for more information, documentation and examples.

![factoextra R package](factoextra-r-package.png)

Installation
------------

Install from [CRAN](https://cran.r-project.org/package=factoextra) as follow:

``` r
install.packages("factoextra")
```

Or, install the latest version from [GitHub](https://github.com/kassambara/factoextra):

``` r
# Install
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
```

Geting started
--------------

Find out more at <http://www.sthda.com/english/wiki/factoextra-r-package>
