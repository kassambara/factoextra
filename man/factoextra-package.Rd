\name{factoextra-package}
\alias{factoextra-package}
\alias{factoextra}
\docType{package}
\title{
Visualization of FactoMineR outputs (PCA, CA, MCA) using ggplot2
}
\description{
Provide easy to use functions to visualize FactoMineR PCA, CA and MCA outputs using ggplot2
}
\details{
\tabular{ll}{
Package: \tab factoextra\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2015-02-21\cr
License: \tab GPL-3\cr
}
~~ An overview of how to use the package, including the most important functions ~~
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
library("FactoMineR")
 data(decathlon)
 res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
 
 # Screeplot
 fviz_screeplot(res.pca)
}
