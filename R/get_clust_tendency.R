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
#'   \href{https://www.datanovia.com/en/lessons/assessing-clustering-tendency/}{STHDA
#'    website: Assessing clustering tendency}.
#' @param data a numeric data frame or matrix. Columns are variables and rows 
#'   are samples. Computation are done on rows (samples) by default. If you want
#'   to calculate Hopkins statistic on variables, transpose the data before.
#' @param n a positive integer specifying the number of points selected from
#'   sample space and from the observed data. Must be smaller than the number
#'   of complete observations.
#' @param graph logical value; if TRUE the ordered dissimilarity image (ODI) is 
#'   shown.
#' @param gradient a list containing three elements specifying the colors for 
#'   low, mid and high values in the ordered dissimilarity image. The element 
#'   "mid" can take the value of NULL.
#' @param seed an integer seed for reproducibility, or NULL to use the current
#'   RNG stream. When non-NULL, the function restores the caller RNG state on
#'   exit.
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
#' \strong{Breaking change}: factoextra uses the corrected Hopkins statistic
#' formula (Wright 2022). Results differ from legacy factoextra and a one-time
#' warning is emitted. Set \code{options(factoextra.warn_hopkins = FALSE)} to
#' silence the warning.
#'
#' For large datasets, nearest-neighbor distances are computed with a low-memory
#' fallback when the full pairwise matrix would exceed
#' \code{getOption("factoextra.hopkins.max_matrix_cells", 2e7)} cells.
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
                               seed = NULL) 
{
  if (is.data.frame(data)) 
    data <- as.matrix(data)
  if (!(is.matrix(data))) 
    stop("data must be data.frame or matrix")
  if (!is.numeric(data))
    stop("data must contain numeric values only")
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1 || n %% 1 != 0)
    stop("n must be a positive integer")

  data <- na.omit(data)
  if (nrow(data) < 2)
    stop("data must contain at least two complete rows")
  if (n >= nrow(data))
    stop("n must be no larger than num of samples")
  rownames(data) <- paste0("r", seq_len(nrow(data)))

  if (!is.null(seed)) {
    has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (has_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      if (!has_seed && exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      } else if (has_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  if (isTRUE(getOption("factoextra.warn_hopkins", TRUE)) &&
      !isTRUE(.factoextra_state$hopkins_warned)) {
    warning(
      "Hopkins statistic uses the corrected formula (Wright 2022); results ",
      "differ from legacy factoextra. Set options(factoextra.warn_hopkins = FALSE) ",
      "to silence this warning.",
      call. = FALSE
    )
    .factoextra_state$hopkins_warned <- TRUE
  }
  plot <- NULL
  if(graph){
    plot <- fviz_dist(stats::dist(data), order = TRUE, 
                      show_labels = FALSE, gradient = gradient)
  }
  
  # Hopkins statistic
  # Sample synthetic points uniformly within the observed ranges.
  mins <- vapply(seq_len(ncol(data)), function(j) min(data[, j]), numeric(1))
  maxs <- vapply(seq_len(ncol(data)), function(j) max(data[, j]), numeric(1))
  p <- vapply(seq_len(ncol(data)), function(j) {
    runif(n, min = mins[j], max = maxs[j])
  }, numeric(n))

  # Use sample() for uniform row selection (fixes biased sampling from PR #133).
  k <- sample(seq_len(nrow(data)), n, replace = TRUE)
  q <- data[k, , drop = FALSE]

  max_matrix_cells <- getOption("factoextra.hopkins.max_matrix_cells", 2e7)
  if(!is.numeric(max_matrix_cells) || length(max_matrix_cells) != 1L ||
     is.na(max_matrix_cells) || max_matrix_cells <= 0){
    max_matrix_cells <- 2e7
  }

  use_vectorized <- (nrow(p) * nrow(data) <= max_matrix_cells) &&
                    (nrow(q) * nrow(data) <= max_matrix_cells)

  euclid_sq <- function(a, b){
    a_sq <- rowSums(a^2)
    b_sq <- rowSums(b^2)
    # Squared Euclidean distances between all rows of a and b.
    pmax(outer(a_sq, b_sq, "+") - 2 * tcrossprod(a, b), 0)
  }

  min_sq_dist <- function(a, b, exclude_identical = FALSE){
    if(use_vectorized){
      d2 <- euclid_sq(a, b)
      if(exclude_identical) d2[d2 <= .Machine$double.eps] <- Inf
      mins <- apply(d2, 1, min)
      mins[!is.finite(mins)] <- 0
      return(mins)
    }

    # Chunked fallback keeps memory bounded while preserving vectorized math.
    chunk_size <- floor(max_matrix_cells / nrow(b))
    if(!is.finite(chunk_size) || chunk_size < 1) chunk_size <- 1
    chunk_size <- as.integer(chunk_size)
    mins <- numeric(nrow(a))
    starts <- seq.int(1L, nrow(a), by = chunk_size)
    for(start in starts){
      end <- min(start + chunk_size - 1L, nrow(a))
      idx <- start:end
      d2 <- euclid_sq(a[idx, , drop = FALSE], b)
      if(exclude_identical) d2[d2 <= .Machine$double.eps] <- Inf
      block_mins <- apply(d2, 1, min)
      block_mins[!is.finite(block_mins)] <- 0
      mins[idx] <- block_mins
    }
    mins
  }

  minp <- sqrt(min_sq_dist(p, data, exclude_identical = FALSE))
  minq <- sqrt(min_sq_dist(q, data, exclude_identical = TRUE))
  
  # Hopkins statistic formula fix: use exponent d=D (dimensionality) as per

  # Cross & Jain (1982) "Measurement of Clustering Tendency" and
  # Wright (2022) "Will the Real Hopkins Statistic Please Stand Up?" R Journal.
  # Previous implementation incorrectly used d=1; correct formula uses d=ncol(data).
  d <- ncol(data)
  list(hopkins_stat = sum(minp^d)/(sum(minp^d) + sum(minq^d)), plot = plot)
}
