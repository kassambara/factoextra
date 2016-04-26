#' @include utilities.R dist.R
NULL
#' Assessing Clustering Tendency
#' 
#' @description Before applying cluster methods, the first step is to assess 
#'   whether the data is clusterable, a process defined as the \strong{assessing
#'   of clustering tendency}. get_clust_tendency() assesses clustering tendency 
#'   using Hopkins' statistic and a visual approach. An ordered dissimilarity 
#'   image (ODI) is shown. Objects belonging to the same cluster are displayed 
#'   in consecutive order using hierarchical clustering. For more details and 
#'   interpretation, see 
#'   \href{http://www.sthda.com/english/wiki/assessing-clustering-tendency-a-vital-issue-unsupervised-machine-learning}{STHDA
#'    website: Assessing clustering tendency}.
#' @param data a numeric data frame or matrix. Columns are variables and rows 
#'   are samples. Computation are done on rows (samples) by default. If you want
#'   to calculate Hopkins statistic on variables, transpose the data before.
#' @param n the number of points selected from sample space which is also the 
#'   number of points selected from the given sample(data).
#' @param graph logical value; if TRUE the ordered dissimilarity image (ODI) is 
#'   shown.
#' @param gradient a list containing three elements specifying the colors for 
#'   low, mid and high values in the ordered dissimilarity image. The element 
#'   "mid" can take the value of NULL.
#' @param seed an integer specifying the seed for random number generator. 
#'   Specify seed for reproducible results.
#' @details
#' 
#' \strong{Hopkins statistic}: If the value of Hopkins statistic is close to
#' zero (far below 0.5), then we can conclude that the dataset is significantly
#' clusterable.
#' 
#' \strong{VAT (Visual Assessment of cluster Tendency)}: The VAT detects the
#' clustering tendency in a visual form by counting the number of square shaped
#' dark (or colored) blocks along the diagonal in a VAT image.
#'  
#' @return A list containing the elements:
#'   
#'   - hopkins_stat for Hopkins statistic value
#'   
#'   - plot for ordered dissimilarity image. This is generated using the 
#'   function \code{\link{fviz_dist}}(dist.obj).
#' @seealso \code{\link{fviz_dist}}
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @examples 
#' data(iris)
#' 
#' # Clustering tendency
#' gradient_col = list(low = "steelblue", high = "white")
#' get_clust_tendency(iris[,-5], n = 50, gradient = gradient_col)
#'    
#' # Random uniformly distributed dataset
#' # (without any inherent clusters)
#' set.seed(123)
#' random_df <- apply(iris[, -5], 2, 
#'                    function(x){runif(length(x), min(x), max(x))}
#'                    )
#' get_clust_tendency(random_df, n = 50, gradient = gradient_col)
#' 
#' @export
get_clust_tendency <- function(data, n, graph = TRUE,
                               gradient = list(low = "red", mid = "white", high = "blue"),
                               seed = 123) 
{
  
  set.seed(seed)
  if (is.data.frame(data)) 
    data <- as.matrix(data)
  if (!(is.matrix(data))) 
    stop("data must be data.frame or matrix")
  if (n >= nrow(data)) 
    stop("n must be no larger than num of samples")
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 package needed for this function to work. Please install it.")
   }
  
  data <- na.omit(data)
  rownames(data) <- paste0("r", 1:nrow(data))
  plot <- NULL
  if(graph){
    plot <- fviz_dist(stats::dist(data), order = TRUE, 
                      show_labels = FALSE, gradient = gradient)
  }
  
  # Hopkins statistic
  p <- apply(data, 2, function(x, n){runif(n, min(x), max(x))}, n)

  
  k <- round(runif(n, 1, nrow(data)))
  q <- as.matrix(data[k, ])
  distp = rep(0, nrow(data))
  distq = 0
  minp = rep(0, n)
  minq = rep(0, n)
  for (i in 1:n) {
    distp[1] <- stats::dist(rbind(p[i, ], data[1, ]))
    minqi <- stats::dist(rbind(q[i, ], data[1, ]))
    for (j in 2:nrow(data)) {
      distp[j] <- stats::dist(rbind(p[i, ], data[j, ]))
      error <- q[i, ] - data[j, ]
      if (sum(abs(error)) != 0) {
        distq <- stats::dist(rbind(q[i, ], data[j, ]))
        if (distq < minqi) 
          minqi <- distq
      }
    }
    minp[i] <- min(distp)
    minq[i] <- minqi
  }
  
  list(hopkins_stat = sum(minq)/(sum(minp) + sum(minq)), plot = plot)
}