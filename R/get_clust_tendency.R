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
#'   \href{http://www.sthda.com/english/articles/29-cluster-validation-essentials/95-assessing-clustering-tendency-essentials/}{STHDA
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
#' 1 (far above 0.5), then we can conclude that the dataset is significantly
#' clusterable. The statistic is calculated using the correct formula from
#' Cross and Jain (1982) with exponent d=D where D is the dimensionality
#' (number of columns) of the data. Under the null hypothesis of spatial
#' randomness, the Hopkins statistic follows a Beta(n, n) distribution.
#'
#' \strong{Note on interpretation}: This function returns the Hopkins statistic H
#' where values close to 1 indicate clusterable data. Some other packages (e.g.,
#' \code{performance::check_clusterstructure}) return 1-H, where values close to
#' 0 indicate clusterability. Always check the documentation of the specific
#' implementation you are using.
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

  data <- na.omit(data)
  rownames(data) <- paste0("r", 1:nrow(data))
  plot <- NULL
  if(graph){
    plot <- fviz_dist(stats::dist(data), order = TRUE, 
                      show_labels = FALSE, gradient = gradient)
  }
  
  # Hopkins statistic
  p <- apply(data, 2, function(x, n){runif(n, min(x), max(x))}, n)

  
  # Use sample() for uniform row selection (fixes biased sampling from PR #133)
  k <- sample(seq_len(nrow(data)), n, replace = TRUE)
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
  
  # Hopkins statistic formula fix: use exponent d=D (dimensionality) as per

  # Cross & Jain (1982) "Measurement of Clustering Tendency" and
  # Wright (2022) "Will the Real Hopkins Statistic Please Stand Up?" R Journal.
  # Previous implementation incorrectly used d=1; correct formula uses d=ncol(data).
  d <- ncol(data)
  list(hopkins_stat = sum(minp^d)/(sum(minp^d) + sum(minq^d)), plot = plot)
}