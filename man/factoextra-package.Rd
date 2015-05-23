\name{factoextra-package}
\alias{factoextra-package}
\alias{factoextra}
\docType{package}
\title{
Extract and Visualize the results of PCA, CA and MCA
}
\description{
Provides some easy-to-use functions to extract and visualize the
    output of PCA (Principal Component Analysis), CA (Correspondence Analysis)
    and MCA (Multiple Correspondence Analysis) functions from several packages
    : PCA, CA, MCA [FactoMineR]; prcomp and princomp [stats]; dudi.pca,
    dudi.coa, dudi.acm [ade4]; ca [ca]; corresp [MASS]. Ggplot2 plotting system
    is used.
}
\details{
\tabular{ll}{
Package: \tab factoextra\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2015-02-21\cr
License: \tab GPL-2\cr
}
}
\author{
Alboukadel Kassambara

Maintainer: Alboukadel Kassambara <alboukadel.kassambara@gmail.com>
}
\references{
http://www.sthda.com
}
\keyword{ package }
\examples{

# Principal Component Analysis
# ++++++++++++++++++++++++++
data(iris)
res.pca <- prcomp(iris[, -5],  scale = TRUE)

# Extract eigenvalues/variances
get_eig(res.pca)
# Visualize eigenvalues/variances
fviz_eig(res.pca)
# Add labels, change theme
fviz_screeplot(res.pca,  addlabels=TRUE, hjust = -0.3) + 
    theme_minimal()
    
# Graph of variables
fviz_pca_var(res.pca)
# Control variable colors using their contributions
# Use gradient color
fviz_pca_var(res.pca, col.var="contrib")+
scale_color_gradient2(low="white", mid="blue", 
      high="red", midpoint = 96) + theme_minimal()
      
# Graph of individuals
fviz_pca_ind(res.pca)
# Color by groups and add ellipse
fviz_pca_ind(res.pca, label="none",
             habillage=iris$Species,
             addEllipses=TRUE, ellipse.level=0.95)
# Biplot
 fviz_pca_biplot(res.pca,  label="var",
    habillage=iris$Species, addEllipses=TRUE, ellipse.level=0.95)
}
