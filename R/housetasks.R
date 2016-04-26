#'House tasks contingency table
#'
#'@description A data frame containing the frequency of execution of 13 house
#'  tasks in the couple. This table is also available in ade4 package.
#'@name housetasks
#'@docType data
#'@usage data("housetasks")
#'@format  
#' A data frame with 13 observations (house tasks) on the following 4 columns.
#' \describe{
#'  \item{\code{Wife}}{a numeric vector}
#'  \item{\code{Alternating}}{a numeric vector}
#'  \item{\code{Husband}}{a numeric vector}
#'  \item{\code{Jointly}}{a numeric vector}
#' }
#'@source This data is from FactoMineR package.
#'
#' @examples
#' library(FactoMineR)
#' data(housetasks)
#' res.ca <- CA(housetasks, graph=FALSE)
#' fviz_ca_biplot(res.ca, repel = TRUE)+
#' theme_minimal()
#' 
NULL