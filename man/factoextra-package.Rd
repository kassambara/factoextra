\name{factoextra-package}
\alias{factoextra-package}
\alias{factoextra}
\docType{package}
\title{
Visualization of principal component analysis results
}
\description{
Provide some easy-to-use functions to visualize the output of
    several PCA functions : PCA() from FactoMineR package; prcomp() and
    princomp() from stats package; dudi.pca() from ade4 package.
}
\details{
\tabular{ll}{
Package: \tab factoextra\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2015-02-21\cr
License: \tab GPL-3\cr
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
\seealso{
}
\examples{
data(iris)
# Principal component analysis
res.pca <- princomp(iris[, -5],  cor = TRUE)
# variables
fviz_pca_var(res.pca)
# Individuals
fviz_pca_ind(res.pca, data = iris[, -5])
# biplot
 fviz_pca_biplot(res.pca, data = iris[, -5])
 # Color by groups, ellipses
 fviz_pca_biplot(res.pca, data = iris[, -5], label="var",
    habillage=iris$Species, addEllipses=TRUE, ellipse.level=0.95)
}
