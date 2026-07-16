#'Extract and visualize the eigenvalues/variances of dimensions
#'
#'@description Eigenvalues correspond to the amount of the variation explained 
#'  by each principal component (PC). 
#'  
#'  \itemize{ \item get_eig(): Extract the eigenvalues/variances of the
#'  principal dimensions \item fviz_eig(): Plot the eigenvalues/variances
#'  against the number of dimensions \item get_eigenvalue(): an alias of
#'  get_eig() \item fviz_screeplot(): an alias of fviz_eig() }
#'  
#'  These functions support the results of Principal Component Analysis (PCA), 
#'  Correspondence Analysis (CA), Multiple Correspondence Analysis (MCA), Factor Analysis of Mixed Data (FAMD),
#'  Multiple Factor Analysis (MFA) and Hierarchical Multiple Factor Analysis
#'  (HMFA) functions. \code{fviz_eig()} validates \code{ncp},
#'  \code{parallel.iter}, and \code{parallel.seed} before plotting, accepting
#'  integer-like numeric values while still rejecting fractional inputs.
#'  
#'  
#'@param X an object of class PCA, CA, MCA, FAMD, MFA, or HMFA [FactoMineR];
#'  \code{prcomp} or \code{princomp} [stats]; \code{factoextra_pca};
#'  \code{dudi}, \code{pca}, \code{coa}, \code{acm}, \code{between}, or
#'  \code{within} [ade4]; \code{ca} or \code{mjca} [ca];
#'  \code{correspondence} [MASS]; or \code{expoOutput} [ExPosition].
#'@param choice a text specifying the data to be plotted. Allowed values are 
#'  "variance" or "eigenvalue".
#'@param geom a text specifying the geometry to be used for the graph. Allowed 
#'  values are "bar" for barplot, "line" for lineplot or c("bar", "line") to use
#'  both types.
#'@param barfill fill color for bar plot.
#'@param barcolor outline color for bar plot.
#'@param linecolor color for line plot (when geom contains "line").
#'@param ncp a single positive integer specifying the number of dimensions to
#'  be shown. Integer-like numeric values are accepted.
#'@param addlabels logical value. If TRUE, labels are added at the top of bars
#'  or points showing the information retained by each dimension.
#'@param hjust horizontal adjustment of the labels.
#'@param main,xlab,ylab plot main and axis titles.
#'@param parallel logical value. If TRUE, adds a Horn parallel-analysis threshold
#'  curve. Each point is the 95th percentile of the corresponding ordered
#'  eigenvalue from independently simulated reference data. Components whose
#'  eigenvalues exceed their component-specific thresholds are retained by the
#'  parallel-analysis rule. Correlation PCA uses standardized reference data;
#'  covariance PCA uses reconstructed marginal scales. This is available only
#'  when \code{choice = "eigenvalue"} and \code{X} is a sufficiently complete
#'  \code{prcomp} or \code{princomp} object. Mean centering is required;
#'  \code{prcomp} also requires retained scores. Custom scaling and incomplete
#'  covariance decompositions are rejected when their reference distribution
#'  cannot be reconstructed. Default is FALSE.
#'@param parallel.color color of the parallel-analysis threshold curve. Default is "red".
#'@param parallel.lty line type for the parallel-analysis curve. Default is "dashed".
#'@param parallel.iter a single positive integer giving the number of
#'  iterations for parallel analysis simulation. Integer-like numeric values
#'  are accepted. Default is 100.
#'@param parallel.seed NULL or a single non-negative integer seed for
#'  reproducible parallel analysis simulation. If NULL (default), the current
#'  RNG stream is used. Integer-like numeric values are accepted.
#' @inheritParams ggpubr::ggpar
#'@param ... optional arguments to be passed to the function \link[ggpubr]{ggpar}.
#'  
#'@return \itemize{ \item get_eig() (or get_eigenvalue()): returns a data.frame
#'  containing 3 columns: the eigenvalues, the percentage of variance and  the
#'  cumulative percentage of variance retained by each dimension.
#'  \item fviz_eig() (or fviz_screeplot()): returns a ggplot2 }
#'  
#'@author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'@seealso \code{\link{fviz_pca}}, \code{\link{fviz_ca}},
#'  \code{\link{fviz_mca}}, \code{\link{fviz_mfa}}, \code{\link{fviz_hmfa}}
#'@references \url{https://www.sthda.com/english/}
#' @examples
#' # Principal Component Analysis
#' # ++++++++++++++++++++++++++
#' data(iris)
#' res.pca <- prcomp(iris[, -5],  scale = TRUE)
#' 
#' # Extract eigenvalues/variances
#' get_eig(res.pca)
#' 
#' # Default plot
#' fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 85))
#'   
#' # Scree plot - Eigenvalues
#' fviz_eig(res.pca, choice = "eigenvalue", addlabels=TRUE)
#' 
#' # Use only bar  or line plot: geom = "bar" or geom = "line"
#' fviz_eig(res.pca, geom="line")
#'
#' # Parallel analysis (Horn's method) to determine the number of components
#' # Retain components whose eigenvalues exceed their component-specific points
#' fviz_eig(res.pca, choice = "eigenvalue", parallel = TRUE,
#'          addlabels = TRUE, parallel.color = "red",
#'          parallel.iter = 10, parallel.seed = 123)
#'
#' \dontrun{         
#' # Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(housetasks)
#' res.ca <- CA(housetasks, graph = FALSE)
#' get_eig(res.ca)
#' fviz_eig(res.ca, linecolor = "#FC4E07",
#'    barcolor = "#00AFBB", barfill = "#00AFBB")
#' 
#' # Multiple Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mca <- MCA(poison, quanti.sup = 1:2, 
#'               quali.sup = 3:4, graph=FALSE)
#' get_eig(res.mca)
#' fviz_eig(res.mca, linecolor = "#FC4E07",
#'    barcolor = "#2E9FDF", barfill = "#2E9FDF")
#' }
#' 
#'@name eigenvalue
NULL
#' @rdname eigenvalue
#' @export
get_eig<-function(X){
  
  # FactoMineR package
  if(inherits(X, c('PCA', 'CA', 'MCA', 'FAMD', 'MFA', 'HMFA', 'sPCA', 'sCA', 'sMCA', 'sMFA', 'sHMFA'))) eig <- X$eig
  else{
    # stats package
    if(inherits(X, "prcomp") || inherits(X, "princomp")) eig <- (X$sdev)^2
    # user-supplied coordinates wrapped by as_factoextra_pca()
    else if(inherits(X, "factoextra_pca")) eig <- X$eig.values
    # ade4 package (incl. between-class/within-class analyses: bca/wca)
    else if(inherits(X, c("pca", "coa", "acm", "between", "within")) && inherits(X, "dudi")) eig <- X$eig
    # ca package
    else if(inherits(X, 'ca'))  eig <- X$sv^2
    else if(inherits(X, 'mjca')) eig <- X$inertia.e
    # MASS
    else if(inherits(X, 'correspondence'))  eig <- X$cor^2
    # ExPosition package
    else if (inherits(X, "expoOutput")) eig <- X$ExPosition.Data$eigs
    else stop("An object of class : ", paste(class(X), collapse = ", "), 
              " can't be handled by the function get_eigenvalue()")
    
    variance <- eig*100/sum(eig)
    cumvar <- cumsum(variance)
    eig <- data.frame(eigenvalue = eig, variance = variance, 
                      cumvariance = cumvar)
  }
  
  colnames(eig) <- c("eigenvalue", "variance.percent", 
                     "cumulative.variance.percent")
  rownames(eig) <- paste0("Dim.", seq_len(nrow(eig)))
  
  eig 
}

#' @rdname eigenvalue
#' @export
get_eigenvalue <- function(X){
  get_eig(X)
}

.princomp_uses_correlation <- function(X){
  cor_arg <- NULL
  if(!is.null(X[["call"]])) cor_arg <- X[["call"]][["cor"]]
  if(is.null(cor_arg)) return(FALSE)
  if((is.logical(cor_arg) || is.numeric(cor_arg)) &&
     length(cor_arg) == 1L && !is.na(cor_arg) && is.finite(cor_arg) &&
     cor_arg %in% c(0, 1)) return(as.logical(cor_arg))

  # A non-unit stored scale proves that princomp standardized the variables.
  # If every scale is one, a symbolic call (for example cor = use_cor) is
  # ambiguous from the fitted object alone, so do not guess.
  object_scale <- X[["scale"]]
  if(is.numeric(object_scale) && length(object_scale) > 0L &&
     all(is.finite(object_scale)) &&
     any(abs(object_scale - 1) > sqrt(.Machine$double.eps))) return(TRUE)

  stop(
    "Cannot determine whether this princomp object used cor = TRUE or FALSE. ",
    "Refit it with a literal cor argument before requesting parallel analysis."
  )
}

.parallel_analysis_spec <- function(X){
  if(inherits(X, "prcomp")){
    scores <- X[["x"]]
    rotation <- X[["rotation"]]
    if(is.null(scores) || !is.matrix(scores))
      stop("Parallel analysis requires a prcomp object fitted with retx = TRUE.")
    if(is.null(rotation) || !is.matrix(rotation))
      stop("The prcomp object has no usable rotation matrix.")

    complete_scores <- scores[stats::complete.cases(scores), , drop = FALSE]
    n_obs <- nrow(complete_scores)
    n_var <- nrow(rotation)
    eigenvalues <- as.numeric(X[["sdev"]])^2
    n_components <- length(eigenvalues)
    if(identical(X[["center"]], FALSE) || is.null(X[["center"]]))
      stop("Parallel analysis requires a mean-centered prcomp fit.")
    score_tolerance <- sqrt(.Machine$double.eps) *
      max(1, abs(complete_scores))
    if(any(abs(colMeans(complete_scores)) > score_tolerance))
      stop("Parallel analysis requires a prcomp fit centered at the variable means.")
    correlation <- !is.null(X[["scale"]]) && !identical(X[["scale"]], FALSE)

    if(n_components < 1L || any(!is.finite(eigenvalues)) ||
       any(eigenvalues < 0))
      stop("The fitted PCA object has an invalid component spectrum.")

    if(correlation){
      tolerance <- sqrt(.Machine$double.eps) * max(1, n_var)
      if(ncol(rotation) == n_components){
        analyzed_var <- rowSums(sweep(rotation^2, 2, eigenvalues, "*"))
        if(any(abs(analyzed_var - 1) > tolerance))
          stop("Parallel analysis cannot reproduce a prcomp fit using a custom scale vector.")
      } else if(abs(sum(eigenvalues) - n_var) > tolerance){
        stop("Parallel analysis cannot verify correlation scaling for this truncated prcomp fit.")
      }
    }

    marginal_sd <- rep(1, n_var)
    if(!correlation){
      retained <- ncol(rotation)
      if(retained > n_components)
        stop("The prcomp rotation contains more components than its eigenvalues.")

      tolerance <- sqrt(.Machine$double.eps) * max(1, eigenvalues)
      if(retained < n_components &&
         any(eigenvalues[seq.int(retained + 1L, n_components)] > tolerance)){
        stop(
          "Parallel analysis cannot reconstruct marginal variances from this ",
          "truncated covariance prcomp object. Refit without rank truncation or ",
          "use scale. = TRUE."
        )
      }

      weighted_rotation <- sweep(
        rotation^2, 2, eigenvalues[seq_len(retained)], "*"
      )
      marginal_sd <- sqrt(pmax(rowSums(weighted_rotation), 0))
    }

    covariance_divisor <- n_obs - 1L
  }
  else if(inherits(X, "princomp")){
    loadings <- as.matrix(unclass(X[["loadings"]]))
    if(is.null(loadings) || length(dim(loadings)) != 2L)
      stop("The princomp object has no usable loadings matrix.")

    n_obs <- X[["n.obs"]]
    if(is.null(n_obs) && !is.null(X[["scores"]])) n_obs <- nrow(X[["scores"]])
    n_var <- nrow(loadings)
    eigenvalues <- as.numeric(X[["sdev"]])^2
    n_components <- length(eigenvalues)
    correlation <- .princomp_uses_correlation(X)

    if(n_components < 1L || any(!is.finite(eigenvalues)) ||
       any(eigenvalues < 0))
      stop("The fitted PCA object has an invalid component spectrum.")

    marginal_sd <- rep(1, n_var)
    if(!correlation){
      if(ncol(loadings) != n_components)
        stop(
          "Parallel analysis cannot reconstruct marginal variances from this ",
          "incomplete covariance princomp object."
        )
      marginal_sd <- sqrt(pmax(rowSums(sweep(
        loadings^2, 2, eigenvalues, "*"
      )), 0))
    }

    covariance_divisor <- n_obs
  }
  else stop("Parallel analysis supports only prcomp and princomp objects.")

  if(length(n_obs) != 1L || !is.finite(n_obs) || n_obs < 2L ||
     n_obs != round(n_obs))
    stop("Parallel analysis requires a valid observation count of at least two.")
  if(length(n_var) != 1L || !is.finite(n_var) || n_var < 1L ||
     n_var != round(n_var))
    stop("Parallel analysis requires a valid original variable count.")
  if(n_components < 1L || n_components > min(n_obs, n_var) ||
     any(!is.finite(eigenvalues)))
    stop("The fitted PCA object has an invalid component spectrum.")
  if(length(marginal_sd) != n_var || any(!is.finite(marginal_sd)))
    stop("The fitted PCA object has invalid marginal scales.")

  n_thresholds <- min(n_components, n_var, n_obs - 1L)
  list(
    n_obs = as.integer(n_obs), n_var = as.integer(n_var),
    n_components = n_components, n_thresholds = n_thresholds,
    correlation = correlation, marginal_sd = marginal_sd,
    covariance_divisor = covariance_divisor
  )
}

.parallel_analysis_threshold <- function(X, iterations, seed = NULL){
  iterations <- .coerce_integerish(iterations, "iterations")
  if(!is.null(seed))
    seed <- .coerce_integerish(
      seed, "seed", lower = 0L, upper = .Machine$integer.max,
      value_label = "NULL or a single integer value"
    )
  spec <- .parallel_analysis_spec(X)

  simulate <- function(){
    sim_eigs <- matrix(
      NA_real_, nrow = iterations, ncol = spec$n_thresholds
    )
    for(i in seq_len(iterations)){
      random_data <- matrix(
        stats::rnorm(spec$n_obs * spec$n_var),
        nrow = spec$n_obs, ncol = spec$n_var
      )
      if(spec$correlation){
        random_data <- scale(random_data, center = TRUE, scale = TRUE)
        divisor <- spec$n_obs - 1L
      } else {
        random_data <- sweep(random_data, 2, spec$marginal_sd, "*")
        random_data <- scale(random_data, center = TRUE, scale = FALSE)
        divisor <- spec$covariance_divisor
      }
      values <- svd(random_data, nu = 0, nv = 0)$d^2 / divisor
      if(length(values) < spec$n_thresholds ||
         any(!is.finite(values[seq_len(spec$n_thresholds)])))
        stop("A parallel-analysis simulation produced an invalid spectrum.")
      sim_eigs[i, ] <- values[seq_len(spec$n_thresholds)]
    }
    unname(apply(
      sim_eigs, 2, stats::quantile, probs = 0.95, names = FALSE
    ))
  }

  if(is.null(seed)) simulate()
  else .with_preserved_seed(seed, simulate())
}

#' @rdname eigenvalue
#' @export
fviz_eig<-function(X, choice=c("variance", "eigenvalue"), geom=c("bar", "line"),
                  barfill="steelblue", barcolor="steelblue", linecolor = "black",
                  ncp=10, addlabels=FALSE, hjust = 0,
                   main = NULL, xlab = NULL, ylab = NULL,
                  ggtheme = theme_minimal(),
                  parallel = FALSE, parallel.color = "red",
                  parallel.lty = "dashed", parallel.iter = 100,
                  parallel.seed = NULL, ...)
{
  ncp <- .coerce_integerish(ncp, "ncp")

  eig <- get_eigenvalue(X)
  eig <-eig[seq_len(min(ncp, nrow(eig))), , drop=FALSE]
  
  choice <- choice[1]
  if(choice=="eigenvalue") {
    eig <- eig[,1]
    text_labels <- round(eig,1)
    if(is.null(ylab)) ylab <- "Eigenvalue"
  }
  else if(choice=="variance") {
    eig <- eig[,2]
    text_labels <- paste0(round(eig,1), "%")
  }
  else stop("Allowed values for the argument choice are : 'variance' or 'eigenvalue'")
  
  if(length(intersect(geom, c("bar", "line"))) == 0)
    stop("The specified value(s) for the argument geom are not allowed ")
  
  
  
  df.eig <- data.frame(dim = factor(seq_along(eig)), eig=eig )
  
  extra_args <- list(...)
  bar_width <- extra_args$bar_width
  linetype <- extra_args$linetype
  if(is.null(linetype)) linetype <- "solid"

  p <- ggplot(df.eig, aes(dim, eig, group=1 ))
  if("bar" %in% geom) {
    bar_args <- list(stat = "identity", fill = barfill, color = barcolor)
    if(!is.null(bar_width)) bar_args$width <- bar_width
    p <- p + do.call(geom_bar, bar_args)
  }
  if("line" %in% geom) p <- p + geom_line(color = linecolor, linetype = linetype)+
    geom_point(shape=19, color=linecolor)
  if(addlabels) p <- p + geom_text(label = text_labels, vjust=-0.4, hjust = hjust)

  if(!is.null(parallel.seed)){
    parallel.seed <- .coerce_integerish(
      parallel.seed, "parallel.seed", lower = 0L, upper = .Machine$integer.max,
      value_label = "NULL or a single integer value"
    )
  }

  # Add parallel analysis line (Horn's method) if requested
  if(parallel && choice == "eigenvalue") {
    parallel.iter <- .coerce_integerish(parallel.iter, "parallel.iter")
    supported <- (inherits(X, "prcomp") && !is.null(X[["x"]])) ||
      (inherits(X, "princomp") &&
         (!is.null(X[["n.obs"]]) || !is.null(X[["scores"]])))
    if(!supported){
      warning(
        "Parallel analysis requires a prcomp object fitted with retx = TRUE ",
        "or a princomp object with an observation count. Skipping."
      )
    } else {
      parallel_threshold <- .parallel_analysis_threshold(
        X, parallel.iter, parallel.seed
      )
      parallel_threshold <- parallel_threshold[
        seq_len(min(ncp, length(parallel_threshold)))
      ]
      df.parallel <- data.frame(
        dim = factor(seq_along(parallel_threshold)),
        threshold = parallel_threshold
      )
      p <- p + geom_line(
        data = df.parallel,
        aes(x = .data[["dim"]], y = .data[["threshold"]]),
        color = parallel.color, linetype = parallel.lty, linewidth = 0.8
      ) + geom_point(
        data = df.parallel,
        aes(x = .data[["dim"]], y = .data[["threshold"]]),
        color = parallel.color, shape = 4, size = 2
      )
    }
  } else if(parallel && choice != "eigenvalue") {
    warning("Parallel analysis only works with choice = 'eigenvalue'. Skipping.")
  }

  if(is.null(main)) main <- "Scree plot"
  if(is.null(xlab)) xlab <- "Dimensions"
  if(is.null(ylab)) ylab <- "Percentage of explained variances"
  
  p <- p + labs(title = main, x = xlab, y = ylab)
  ggpubr::ggpar(p, ggtheme = ggtheme,  ...)
}


#' @rdname eigenvalue
#' @export 
fviz_screeplot<- function(...){
  fviz_eig(...)
} 
