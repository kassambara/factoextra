\name{poison}
\alias{poison}
\docType{data}
\title{
Poison
}
\description{
This data is a result from a survey carried out on children of primary school who suffered from food poisoning. They were asked about their symptoms and about what they ate.
}
\usage{data("poison")}
\format{
  A data frame with 55 rows and 15 columns.
}
\references{
This data is from FactoMineR package.
}
\examples{
library(FactoMineR)
data(poison)
res.mca <- MCA(poison, quanti.sup = 1:2, quali.sup=c(3,4), graph = FALSE)
fviz_mca_biplot(res.mca)
}
\keyword{datasets}
