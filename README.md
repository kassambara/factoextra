---
output:
  md_document:
    variant: markdown_github
---



   
    
[![R-CMD-check](https://github.com/kassambara/factoextra/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kassambara/factoextra/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/factoextra)](https://cran.r-project.org/package=factoextra)
[![Downloads](https://cranlogs.r-pkg.org/badges/factoextra)](https://cran.r-project.org/package=factoextra)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/factoextra?color=orange)](https://cranlogs.r-pkg.org/badges/grand-total/factoextra)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Pending Pull-Requests](https://githubbadges.herokuapp.com/kassambara/factoextra/pulls.svg?style=flat)](https://github.com/kassambara/factoextra/pulls)
[![Github Issues](https://githubbadges.herokuapp.com/kassambara/factoextra/issues.svg)](https://github.com/kassambara/factoextra/issues)
   
   
# factoextra : Extract and Visualize the Results of Multivariate Data Analyses
       
     
[**factoextra**](https://rpkgs.datanovia.com/factoextra/index.html) is an R package making easy to *extract* and *visualize* the output of exploratory **multivariate data analyses**, including:    
     
1. [Principal Component Analysis (PCA)](https://www.sthda.com/english/articles/index.php?url=/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/), which is used to summarize the information contained in a continuous (i.e, quantitative) multivariate data by reducing the dimensionality of the data without loosing important information.   
      
      
2. [Correspondence Analysis (CA)](https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/113-ca-correspondence-analysis-in-r-essentials/), which is an extension of the principal component analysis suited to analyse a large contingency table formed by two *qualitative variables* (or categorical data).
        
        
3. [Multiple Correspondence Analysis (MCA)](https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/), which is an adaptation of CA to a data table containing more than two categorical variables.
     
     
4. [Multiple Factor Analysis (MFA)](https://rpkgs.datanovia.com/factoextra/reference/fviz_mfa.html) dedicated to datasets where variables are organized into groups (qualitative and/or quantitative variables).   
     
     
5. [Hierarchical Multiple Factor Analysis (HMFA)](https://rpkgs.datanovia.com/factoextra/reference/fviz_hmfa.html): An extension of MFA in a situation where the data are organized into a hierarchical structure.
    
6. [Factor Analysis of Mixed Data (FAMD)](https://rpkgs.datanovia.com/factoextra/reference/fviz_famd.html), a particular case of the MFA, dedicated to analyze a data set containing both quantitative and qualitative variables.
      
      
There are a number of R packages implementing principal component methods. These packages include: *FactoMineR*, *ade4*, *stats*, *ca*, *MASS* and *ExPosition*.   
    
However, the result is presented differently according to the used packages. To help in the interpretation and in the visualization of multivariate analysis - such as cluster analysis and dimensionality reduction analysis - we developed an easy-to-use R package named [factoextra](https://rpkgs.datanovia.com/factoextra/index.html).
   
   

- The R package **factoextra** has flexible and easy-to-use methods to extract quickly, in a human readable standard data format, the analysis results from the different packages mentioned above.    
- It produces a **ggplot2**-based **elegant data visualization** with less typing. 
- It contains also many functions facilitating clustering analysis and visualization.

   
     
> We'll use i) the FactoMineR package (Sebastien Le, et al., 2008) to compute PCA, (M)CA, FAMD, MFA and HCPC; ii) and the factoextra package for extracting and visualizing the results.   
    
        
          
The figure below shows methods, which outputs can be visualized using the factoextra package. The official online documentation is available at: https://rpkgs.datanovia.com/factoextra/index.html. 
     
     
![factoextra R package](tools/factoextra-r-package.png)
     
     
## Why using factoextra?
   
1. The *factoextra* R package can handle the results of PCA, CA, MCA, MFA, FAMD and HMFA from several packages, for extracting and visualizing the most important information contained in your data.
     
     
2. *After PCA, CA, MCA, MFA, FAMD and HMFA, the most important row/column elements* can be highlighted using :  
  - their cos2 values corresponding to their quality of representation on the factor map
  - their contributions to the definition of the principal dimensions.
     
<span class = "success">If you want to do this, the factoextra package provides a convenient solution.</span>
     
     
3. *PCA and (M)CA are used sometimes for prediction problems* : one can predict the coordinates of new supplementary variables (quantitative and qualitative) and supplementary individuals using the information provided by the previously performed PCA or (M)CA. This can be done easily using the [FactoMineR](https://www.sthda.com/english/articles/index.php?url=/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/) package. 
         
<span class="success">If you want to make predictions with PCA/MCA and to visualize the position of the supplementary variables/individuals on the factor map using ggplot2: then factoextra can help you. It's quick, write less and do more...</span>
      
      
4. *Several functions from different packages - FactoMineR, ade4, ExPosition, stats - are available in R for performing PCA, CA or MCA*. However, The components of the output vary from package to package. 

<span class = "success">No matter the package you decided to use, factoextra can give you a human understandable output.</span>
    
    
## Installing FactoMineR
    
The FactoMineR package can be installed and loaded as follow:   
    
    

``` r
# Install
install.packages("FactoMineR")

# Load
library("FactoMineR")
```

     
     
## Installing and loading factoextra
        
        
- factoextra can be installed from [CRAN](https://cran.r-project.org/package=factoextra) as follow:
   

``` r
install.packages("factoextra")
```
       
       
- Or, install the latest version from [Github](https://github.com/kassambara/factoextra)
     
         

``` r
if(!require(remotes)) install.packages("remotes")
remotes::install_github("kassambara/factoextra")
```

The current maintenance baseline targets:

- `R >= 4.1.0`
- `ggplot2 >= 3.5.2`
- `ggpubr >= 0.6.3` (CRAN)
- `FactoMineR >= 2.13`
     
     
- Load factoextra as follow :    
    

``` r
library("factoextra")
```

## Recent compatibility updates


``` r
# New helper: map legacy FactoMineR category labels
map_factominer_legacy_names(res.mfa, c("var.level"))

# New helper: remove stale package lock directories
clean_lock_files()

# Hopkins statistic uses corrected formula (Wright 2022)
# Set this option to silence the one-time warning
options(factoextra.warn_hopkins = FALSE)
```
      
      
## Main functions in the factoextra package
    
    
<span class="warning">See the online documentation (https://rpkgs.datanovia.com/factoextra/index.html) for a complete list.</span>
      
      
      
      
      
### Visualizing dimension reduction analysis outputs
      
     


Functions | Description
--------- | -----------------------------------------------------------
*fviz_eig (or fviz_eigenvalue)* | Extract and visualize the eigenvalues/variances of dimensions.
*fviz_pca* | Graph of individuals/variables from the output of *Principal Component Analysis* (PCA).
*fviz_ca* | Graph of column/row variables from the output of *Correspondence Analysis* (CA).
*fviz_mca* | Graph of individuals/variables from the output of *Multiple Correspondence Analysis* (MCA).
*fviz_mfa* | Graph of individuals/variables from the output of *Multiple Factor Analysis* (MFA).
*fviz_famd* | Graph of individuals/variables from the output of *Factor Analysis of Mixed Data* (FAMD).
*fviz_hmfa* | Graph of individuals/variables from the output of *Hierarchical Multiple Factor Analysis* (HMFA).
*fviz_ellipses* | Draw confidence ellipses around the categories.
*fviz_cos2* | Visualize the quality of representation of the row/column variable from the results of PCA, CA, MCA functions.
*fviz_contrib* | Visualize the contributions of row/column elements from the results of PCA, CA, MCA functions.

    
   
### Extracting data from dimension reduction analysis outputs
    
    

Functions | Description
--------- | -----------------------------------------------------------
*get_eigenvalue* | Extract and visualize the eigenvalues/variances of dimensions.
*get_pca* | Extract all the results (coordinates, squared cosine, contributions) for the active individuals/variables from *Principal Component Analysis* (PCA) outputs.
*get_ca* | Extract all the results (coordinates, squared cosine, contributions) for the active column/row variables from *Correspondence Analysis* outputs.
*get_mca* | Extract results from *Multiple Correspondence Analysis* outputs.
*get_mfa* | Extract results from *Multiple Factor Analysis* outputs.
*get_famd* | Extract results from *Factor Analysis of Mixed Data* outputs.
*get_hmfa* | Extract results from *Hierarchical Multiple Factor Analysis* outputs.
*facto_summarize* | Subset and summarize the output of factor analyses. 

   
        
### Clustering analysis and visualization  
       
       

Functions | Description
--------- | -----------------------------------------------------------
*dist*(fviz_dist, get_dist) | Enhanced Distance Matrix Computation and Visualization.
*get_clust_tendency* | Assessing Clustering Tendency.
*fviz_nbclust*(fviz_gap_stat) | Determining and Visualizing the Optimal Number of Clusters.
*fviz_dend* | Enhanced Visualization of Dendrogram
*fviz_cluster* | Visualize Clustering Results
*fviz_mclust* | Visualize Model-based Clustering Results
*fviz_silhouette* | Visualize Silhouette Information from Clustering.
*hcut* | Computes Hierarchical Clustering and Cut the Tree
*hkmeans* (hkmeans_tree, print.hkmeans) | Hierarchical k-means clustering.
*eclust* | Visual enhancement of clustering analysis

     
## Dimension reduction and factoextra
      
As depicted in the figure below, the type of analysis to be performed depends on the data set formats and structures.  
      
![dimension reduction and factoextra](tools/multivariate-analysis-factoextra.png)
        
    
In this section we start by illustrating classical methods - such as PCA, CA and MCA - for analyzing a data set containing continuous variables, contingency table and qualitative variables, respectively.
    
We continue by discussing advanced methods - such as FAMD, MFA and HMFA - for analyzing a data set containing a mix of variables (qualitatives & quantitatives) organized or not into groups.
    
    
Finally, we show how to perform hierarchical clustering on principal components (HCPC), which useful for performing clustering with a data set containing only qualitative variables or with a mixed data of qualitative and quantitative variables.
        
        
### Principal component analysis
     
     
- Data: *decathlon2* [in *factoextra* package]
- PCA function: *FactoMineR::PCA*()
- Visualization *factoextra::fviz_pca*()
     
     
<span class = "success">Read more about computing and interpreting principal component analysis at: [**Principal Component Analysis** (PCA)](https://www.sthda.com/english/articles/index.php?url=/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/).</span>
     
     
1. **Loading data**
    
    

``` r
library("factoextra")
data("decathlon2")
df <- decathlon2[1:23, 1:10]
```
    
    
2. **Principal component analysis**
    

``` r
library("FactoMineR")
res.pca <- PCA(df,  graph = FALSE)
```
        
        
3. **Extract and visualize eigenvalues/variances**:  
    
    

``` r
# Extract eigenvalues/variances
get_eig(res.pca)
#> Error in `get_eig()`:
#> ! could not find function "get_eig"
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
#> Error in `fviz_screeplot()`:
#> ! could not find function "fviz_screeplot"
```
     
     
     
4.**Extract and visualize results for variables**:
    
    

``` r
# Extract the results for variables
var <- get_pca_var(res.pca)
#> Error in `get_pca_var()`:
#> ! could not find function "get_pca_var"
var
#> function (x, y = NULL, na.rm = FALSE, use) 
#> {
#>     if (missing(use)) 
#>         use <- if (na.rm) 
#>             "na.or.complete"
#>         else "everything"
#>     na.method <- pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs", 
#>         "everything", "na.or.complete"))
#>     if (is.na(na.method)) 
#>         stop("invalid 'use' argument")
#>     if (is.data.frame(x)) 
#>         x <- as.matrix(x)
#>     else if (!is.null(x)) 
#>         stopifnot(is.atomic(x))
#>     if (is.data.frame(y)) 
#>         y <- as.matrix(y)
#>     else if (!is.null(y)) 
#>         stopifnot(is.atomic(y))
#>     .Call(C_cov, x, y, na.method, FALSE)
#> }
#> <bytecode: 0x779ca9c40>
#> <environment: namespace:stats>
# Coordinates of variables
head(var$coord)
#> Error in `var$coord`:
#> ! object of type 'closure' is not subsettable
# Contribution of variables
head(var$contrib)
#> Error in `var$contrib`:
#> ! object of type 'closure' is not subsettable
# Graph of variables: default plot
fviz_pca_var(res.pca, col.var = "black")
#> Error in `fviz_pca_var()`:
#> ! could not find function "fviz_pca_var"
```
      
      
It's possible to control variable colors using their contributions ("contrib") to the principal axes:   
     
     

``` r
# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
             )
#> Error in `fviz_pca_var()`:
#> ! could not find function "fviz_pca_var"
```
   

   
5. **Variable contributions to the principal axes**:   
    
    

``` r
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
#> Error in `fviz_contrib()`:
#> ! could not find function "fviz_contrib"

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
#> Error in `fviz_contrib()`:
#> ! could not find function "fviz_contrib"
```
    
    
6. **Extract and visualize results for individuals**: 
     
     

``` r
# Extract the results for individuals
ind <- get_pca_ind(res.pca)
#> Error in `get_pca_ind()`:
#> ! could not find function "get_pca_ind"
ind
#> Error:
#> ! object 'ind' not found
# Coordinates of individuals
head(ind$coord)
#> Error:
#> ! object 'ind' not found
# Graph of individuals
# 1. Use repel = TRUE to avoid overplotting
# 2. Control automatically the color of individuals using the cos2
    # cos2 = the quality of the individuals on the factor map
    # Use points only
# 3. Use gradient color
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
             )
#> Error in `fviz_pca_ind()`:
#> ! could not find function "fviz_pca_ind"
# Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE)
#> Error in `fviz_pca_biplot()`:
#> ! could not find function "fviz_pca_biplot"
```
         
         
7. **Color individuals by groups**:
         
         


``` r
# Compute PCA on the iris data set
# The variable Species (index = 5) is removed
# before PCA analysis
iris.pca <- PCA(iris[,-5], graph = FALSE)

# Visualize
# Use habillage to specify groups for coloring
fviz_pca_ind(iris.pca,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
             )
#> Error in `fviz_pca_ind()`:
#> ! could not find function "fviz_pca_ind"
```
     
     
    
### Correspondence analysis
     
- Data: *housetasks* [in factoextra]
- CA function *FactoMineR::CA*()
- Visualize with *factoextra::fviz_ca*()
       
       
<span class = "success">Read more about computing and interpreting correspondence analysis at: [**Correspondence Analysis** (CA)](https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/113-ca-correspondence-analysis-in-r-essentials/).</span>
     
     
- **Compute CA**:  
   

``` r
 # Loading data
data("housetasks")

 # Computing CA
library("FactoMineR")
res.ca <- CA(housetasks, graph = FALSE)
```
      
      
- **Extract results for row/column variables**:   
     
     

``` r
# Result for row variables
get_ca_row(res.ca)
  
# Result for column variables
get_ca_col(res.ca)
```
    
    
- **Biplot of rows and columns**
      


``` r
fviz_ca_biplot(res.ca, repel = TRUE)
#> Error in `fviz_ca_biplot()`:
#> ! could not find function "fviz_ca_biplot"
```
   
   
To visualize only row points or column points, type this:   
     

``` r
# Graph of row points
fviz_ca_row(res.ca, repel = TRUE)

# Graph of column points
fviz_ca_col(res.ca)

# Visualize row contributions on axes 1
fviz_contrib(res.ca, choice ="row", axes = 1)
     
# Visualize column contributions on axes 1
fviz_contrib(res.ca, choice ="col", axes = 1)
```
    

      
### Multiple correspondence analysis
     
     
    
- Data: **poison** [in factoextra]
- MCA function **FactoMineR::MCA**()
- Visualization **factoextra::fviz_mca**()
       
       
<span class = "success">Read more about computing and interpreting multiple correspondence analysis at: [**Multiple Correspondence Analysis** (MCA)](https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/).</span>
   
   
1. **Computing MCA**:
     


``` r
library(FactoMineR)
data(poison)
res.mca <- MCA(poison, quanti.sup = 1:2,
              quali.sup = 3:4, graph=FALSE)
```
     
     
2. **Extract results for variables and individuals**:   
     
     

``` r
# Extract the results for variable categories
get_mca_var(res.mca)

# Extract the results for individuals
get_mca_ind(res.mca)
```

     
3. **Contribution of variables and individuals to the principal axes**:   
     
     

``` r
# Visualize variable categorie contributions on axes 1
fviz_contrib(res.mca, choice ="var", axes = 1)

# Visualize individual contributions on axes 1
# select the top 20
fviz_contrib(res.mca, choice ="ind", axes = 1, top = 20)

```
       
       
4. **Graph of individuals**
       
       

``` r
# Color by groups
# Add concentration ellipses
# Use repel = TRUE to avoid overplotting
grp <- as.factor(poison[, "Vomiting"])
fviz_mca_ind(res.mca,  habillage = grp,
             addEllipses = TRUE, repel = TRUE)
#> Error in `fviz_mca_ind()`:
#> ! could not find function "fviz_mca_ind"
```
      
      
5. **Graph of variable categories**:
     
     

``` r
fviz_mca_var(res.mca, repel = TRUE)
#> Error in `fviz_mca_var()`:
#> ! could not find function "fviz_mca_var"
```
     
     
       
6. **Biplot of individuals and variables**:
      
      

``` r
fviz_mca_biplot(res.mca, repel = TRUE)
#> Error in `fviz_mca_biplot()`:
#> ! could not find function "fviz_mca_biplot"
```
      
      
### Advanced methods
     
     
The factoextra R package has also functions that support the visualization of advanced methods such:   
   
- Factor Analysis of Mixed Data (FAMD): : [FAMD Examples](https://rpkgs.datanovia.com/factoextra/reference/fviz_famd.html)
- Multiple Factor Analysis (MFA): [MFA Examples](https://rpkgs.datanovia.com/factoextra/reference/fviz_mfa.html)
- Hierarchical Multiple Factor Analysis (HMFA): [HMFA Examples](https://rpkgs.datanovia.com/factoextra/reference/fviz_hmfa.html)
- [Hierachical Clustering on Principal Components (HCPC)](https://www.sthda.com/english/articles/index.php?url=/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/)
     

## Cluster analysis and factoextra
         
To learn more about cluster analysis, you can refer to the book available at: [Practical Guide to Cluster Analysis in R](https://www.datanovia.com/en/product/practical-guide-to-cluster-analysis-in-r/)
              
              
<a href = "https://www.datanovia.com/en/product/practical-guide-to-cluster-analysis-in-r/"><img src = "tools/clustering-e1-cover.png" alt = "clustering book cover"/></a>
     
     
The main parts of the book include:   
    
- *distance measures*, 
- *partitioning clustering*,
- *hierarchical clustering*, 
- *cluster validation methods*, as well as, 
- *advanced clustering methods* such as fuzzy clustering, density-based clustering and model-based clustering.   
     
     
The book presents the basic principles of these tasks and provide many examples in R. It offers solid guidance in data mining for students and researchers.
      
      
      
### Partitioning clustering
    
    
![Partitioning cluster analysis](tools/partitioning-clustering.png)
     


``` r
# 1. Loading and preparing data
data("USArrests")
df <- scale(USArrests)

# 2. Compute k-means
set.seed(123)
km.res <- kmeans(scale(USArrests), 4, nstart = 25)

# 3. Visualize
library("factoextra")
fviz_cluster(km.res, data = df,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
             )
#> Error in `fviz_cluster()`:
#> ! could not find function "fviz_cluster"
```
    
 
<br/>   
<div class = "success">
Read more:   
   
1. [Cluster analysis in R: All what you should know](https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/).
    
2. [Partitioning cluster analysis](https://www.datanovia.com/en/courses/partitional-clustering-in-r-the-essentials/).
</div> 
<br/>
  

    
    
### Hierarchical clustering
    


``` r
library("factoextra")
# Compute hierarchical clustering and cut into 4 clusters
res <- hcut(USArrests, k = 4, stand = TRUE)
#> Error in `hcut()`:
#> ! could not find function "hcut"

# Visualize
fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))
#> Error in `fviz_dend()`:
#> ! could not find function "fviz_dend"
```
      
      
<br/>   
<div class = "success">
Read more:   
   
1. [Cluster analysis in R: All what you should know](https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/)
    
2. [Hierarchical clustering essentials](https://www.datanovia.com/en/courses/hierarchical-clustering-in-r-the-essentials/)
</div> 
<br/>
      
      
### Determine the optimal number of clusters
      
      
   

``` r
# Optimal number of clusters for k-means
library("factoextra")
my_data <- scale(USArrests)
fviz_nbclust(my_data, kmeans, method = "gap_stat")
#> Error in `fviz_nbclust()`:
#> ! could not find function "fviz_nbclust"
```
      
      
## Acknoweledgment
   

Original package authors:

- [Alboukadel Kassambara](https://github.com/kassambara)
- [Fabian Mundt](https://github.com/inventionate)

Contributor:

- Laszlo Erdey (Faculty of Economics and Business, University of Debrecen, Hungary)

We sincerely thank all developers for their efforts behind the packages that **factoextra** depends on, namely,
[ggplot2](https://cran.r-project.org/package=ggplot2) (Hadley Wickham, Springer-Verlag New York, 2009), [FactoMineR](https://cran.r-project.org/package=FactoMineR) (Sebastien Le et al., Journal of Statistical Software, 2008), [dendextend](https://cran.r-project.org/package=dendextend) (Tal Galili, Bioinformatics, 2015), [cluster](https://cran.r-project.org/package=dendextend) (Martin Maechler et al., 2016) and more .....
      
      
      
## References
   
- H. Wickham (2009). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.
- Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., Hornik, K.(2016).  cluster: Cluster Analysis Basics and Extensions. R package version 2.0.5.
- Sebastien Le, Julie Josse, Francois Husson (2008). FactoMineR: An R Package for Multivariate Analysis. Journal of Statistical Software, 25(1), 1-18. 10.18637/jss.v025.i01
- Tal Galili (2015). dendextend: an R package for visualizing, adjusting, and comparing trees of hierarchical clustering. Bioinformatics. DOI: 10.1093/bioinformatics/btv428
       
     
