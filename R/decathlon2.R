#' Athletes' performance in decathlon
#'
#' @description
#' Athletes' performance during two sporting meetings
#' @name decathlon2
#' @docType data
#' @usage data("decathlon2")
#' @format
#' A data frame with 27 observations on the following 13 variables.
#' \describe{
#' \item{\code{X100m}}{a numeric vector}
#' \item{\code{Long.jump}}{a numeric vector}
#' \item{\code{Shot.put}}{a numeric vector}
#' \item{\code{High.jump}}{a numeric vector}
#' \item{\code{X400m}}{a numeric vector}
#' \item{\code{X110m.hurdle}}{a numeric vector}
#' \item{\code{Discus}}{a numeric vector}
#' \item{\code{Pole.vault}}{a numeric vector}
#' \item{\code{Javeline}}{a numeric vector}
#' \item{\code{X1500m}}{a numeric vector}
#' \item{\code{Rank}}{a numeric vector corresponding to the rank}
#' \item{\code{Points}}{a numeric vector specifying the point obtained}
#' \item{\code{Competition}}{a factor with levels \code{Decastar} \code{OlympicG}}
#' }
#' 
#' @source
#' This data is a subset of decathlon data in FactoMineR package.
#' @examples
#' \donttest{
#' data(decathlon2)
#' decathlon.active <- decathlon2[1:23, 1:10]
#' res.pca <- prcomp(decathlon.active, scale = TRUE)
#' fviz_pca_biplot(res.pca, data = decathlon.active)
#' }
#' 
NULL