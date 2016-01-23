#' A dataset containing clusters of multiple shapes
#' 
#' @description Data containing clusters of any shapes. Useful for comparing
#' density-based clustering (DBSCAN) and standard partitioning methods such as
#' k-means clustering.
#' @name multishapes
#' @docType data
#' @usage data("multishapes")
#' @format A data frame with 1100 observations on the following 3 variables. 
#'  \describe{
#' \item{\code{x}}{a numeric vector containing the x coordinates of observations}
#' \item{\code{y}}{a numeric vector containing the y coordinates of observations}
#' \item{\code{shape}}{a numeric vector corresponding to the cluster number of each observations.}
#' }
#' @details 
#' The dataset contains 5 clusters and some outliers/noises.
#' 
#' @examples
#' \donttest{
#'data(multishapes)
#'plot(multishapes[,1], multishapes[, 2],
#'     col = multishapes[, 3], pch = 19, cex = 0.8)
#' }
#' 
NULL