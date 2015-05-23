\name{housetasks}
\alias{housetasks}
\docType{data}
\title{
House tasks contingency table
}
\description{
A data frame containing the frequency of execution of 13 house tasks in the couple. This table is also available in ade4 package.
}
\usage{data("housetasks")}
\format{
  A data frame with 13 observations (house tasks) on the following 4 columns.
  \describe{
    \item{\code{Wife}}{a numeric vector}
    \item{\code{Alternating}}{a numeric vector}
    \item{\code{Husband}}{a numeric vector}
    \item{\code{Jointly}}{a numeric vector}
  }
}
\source{
Kroonenberg, P. M. and Lombardo, R. 1999. Nonsymmetric correspondence analysis: a tool for analysing contingency tables with a dependence structure. Multivariate Behavioral Research, 34, 367-396.
}
\examples{
data(housetasks)
# 1. convert the data as a table
dt <- as.table(as.matrix(housetasks))
# 2. Mosaic plot
library("graphics")
mosaicplot(dt, las=2, shade=TRUE)
}
\keyword{datasets}
