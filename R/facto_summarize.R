#' @include utilities.R get_pca.R eigenvalue.R
NULL
#' Subset and summarize the output of factor analyses
#' 
#' @description Subset and summarize the results of Principal Component
#'   Analysis (PCA), Correspondence Analysis (CA), Multiple Correspondence
#'   Analysis (MCA), Factor Analysis of Mixed Data (FAMD), Multiple Factor
#'   Analysis (MFA) and Hierarchical Multiple Factor Analysis (HMFA) functions
#'   from several packages. Axis indices are validated before extraction, and
#'   MCA quantitative supplementary summaries inherit the package-level error
#'   raised when that result is unavailable.
#' @param X an object of class PCA, CA, MCA, FAMD, MFA and HMFA [FactoMineR]; prcomp
#'   and princomp [stats]; dudi, pca, coa and acm [ade4]; ca [ca package]; expoOutput [ExPosition].
#' @param element the element to subset from the output. Possible values are
#'   "row" or "col" for CA; "var", "ind", "mca.cor" or "quanti.sup" for MCA;
#'   "var" or "ind" for PCA; and 'quanti.var', 'quali.var', 'quali.sup',
#'   'group' or 'ind' for FAMD, MFA and HMFA.
#' @param result the result to be extracted for the element. Possible values are
#'   any combination of \code{c("coord", "cos2", "contrib")}.
#' @param group.names a vector containing the name of the groups (by default, 
#'   NULL and the groups are named group.1, group.2, and so on).
#' @param node.level a single number indicating the HMFA node level.
#' @param axes a numeric vector specifying the axes of interest. Values must be
#'   positive integer indices within the available dimensions. Default values
#'   are 1:2 for axes 1 and 2.
#' @param select a selection of variables. Allowed values are NULL or a list 
#'   containing the arguments name, cos2 or contrib. Default is list(name = 
#'   NULL, cos2 = NULL, contrib = NULL): \itemize{ \item name: is a character 
#'   vector containing variable names to be selected \item cos2: if cos2 is in 
#'   [0, 1], ex: 0.6, then variables with a cos2 > 0.6 are selected. if cos2 > 
#'   1, ex: 5, then the top 5 variables with the highest cos2 are selected \item
#'   contrib: if contrib > 1, ex: 5, then the top 5 variables with the highest
#'   contributions are selected. }
#' @return A data frame containing the requested coordinates and the cos2 and
#'   contribution metrics aggregated over the requested axes.
#' @details If length(axes) > 1, then the columns contrib and cos2 correspond to
#'   the total contributions and total cos2 of the axes. In this case, the 
#'   column coord is calculated as x^2 + y^2 + ...; x, y, ... are the
#'   coordinates of the points on the specified axes.
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references \url{https://www.sthda.com/english/}
#' @examples
#' # Principal component analysis
#' # +++++++++++++++++++++++++++++
#' data(decathlon2)
#' decathlon2.active <- decathlon2[1:23, 1:10]
#' res.pca <- prcomp(decathlon2.active,  scale = TRUE)
#' 
#' # Summarize variables on axes 1:2
#' facto_summarize(res.pca, "var", axes = 1:2)[,-1]
#' # Select the top 5 contributing variables
#' facto_summarize(res.pca, "var", axes = 1:2,
#'            select = list(contrib = 5))[,-1]
#' # Select variables with cos2 >= 0.6
#' facto_summarize(res.pca, "var", axes = 1:2,
#'            select = list(cos2 = 0.6))[,-1]
#' # Select by names
#' facto_summarize(res.pca, "var", axes = 1:2,
#'      select = list(name = c("X100m", "Discus", "Javeline")))[,-1]
#'            
#' # Summarize individuals on axes 1:2
#' facto_summarize(res.pca, "ind", axes = 1:2)[,-1]
#' 
#' # Correspondence Analysis
#' # ++++++++++++++++++++++++++
#' # Install and load FactoMineR to compute CA
#' # install.packages("FactoMineR")
#' library("FactoMineR")
#' data("housetasks")
#' res.ca <- CA(housetasks, graph = FALSE)
#' # Summarize row variables on axes 1:2
#' facto_summarize(res.ca, "row", axes = 1:2)[,-1]
#' # Summarize column variables on axes 1:2
#' facto_summarize(res.ca, "col", axes = 1:2)[,-1]
#' 
#' # Multiple Correspondence Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mca <- MCA(poison, quanti.sup = 1:2, 
#'               quali.sup = 3:4, graph=FALSE)
#' # Summarize variables on axes 1:2
#' res <- facto_summarize(res.mca, "var", axes = 1:2)
#' head(res)
#' # Summarize individuals on axes 1:2
#' res <- facto_summarize(res.mca, "ind", axes = 1:2)
#' head(res)
#' # Summarize quantitative supplementary variables on axes 1:2
#' res <- facto_summarize(res.mca, "quanti.sup", axes = 1:2)
#' head(res)
#' 
#' # Multiple factor Analysis
#' # +++++++++++++++++++++++++++++++++
#' library(FactoMineR)
#' data(poison)
#' res.mfa <- MFA(poison, group=c(2,2,5,6), type=c("s","n","n","n"),
#'                name.group=c("desc","desc2","symptom","eat"),
#'                num.group.sup=1:2, graph=FALSE)
#' # Summarize categorical variables on axes 1:2
#' res <- facto_summarize(res.mfa, "quali.var", axes = 1:2)
#' head(res)
#' # Summarize individuals on axes 1:2
#' res <- facto_summarize(res.mfa, "ind", axes = 1:2)
#' head(res)
#' @export
facto_summarize <- function(X, element, node.level = 1, group.names, 
                            result = c("coord", "cos2", "contrib"),
                            axes=1:2, select = NULL)
                            
  { 
  # check element
  allowed_elmts <- c("row", "col", "var", "ind", "quanti.var", "quali.var",
                     "quali.sup",
                     "mca.cor", "quanti.sup",  "group", "partial.axes", "partial.node")
  if(!element %in% allowed_elmts) stop("Can't handle element = '", element, "'") 
  if(element %in% c("mca.cor", "quanti.sup")) {
    if(!inherits(X, "MCA")) stop("element = 'mca_cor' is supported only for FactoMineR::MCA().")
    result <- NULL
  }
  if(element == "quali.sup") {
    result <- intersect(result, c("coord", "cos2"))
  }
  
  # Check and get the classe of X
  facto_class <- .get_facto_class(X)
  # Extract the element
  element <- element[1]
  elmt <- switch(facto_class,
                 CA = get_ca(X, element),
                 PCA = get_pca(X, element),
                 MCA = get_mca(X, element),
                 FAMD = get_famd(X, element),
                 MFA = get_mfa(X, element),
                 HMFA = get_hmfa(X, element)
                 )

  requested.metrics <- intersect(result, c("coord", "cos2", "contrib"))
  unavailable <- requested.metrics[vapply(
    requested.metrics, function(metric) is.null(elmt[[metric]]), logical(1)
  )]
  if(length(unavailable))
    stop("'", paste(unavailable, collapse = "', '"),
         "' is not available for element = '", element, "'.", call. = FALSE)
  
  # Check axes
  if(inherits(elmt, "hmfa_partial"))
    ndim <- ncol(elmt[[1]])
  else
    ndim <- ncol(elmt$coord)
  axes <- .validate_axis_indices(axes, ndim = ndim)
  
  # Summarize the result
  res = NULL
  # Fix for PR #143: respect axes parameter for mca.cor and quanti.sup
  if(element %in% c("mca.cor", "quanti.sup")) res <- elmt$coord[, axes, drop = FALSE]
  
  # 1.Extract the coordinates x, y and coord
  # OPTIMIZATION: Replaced apply(dd^2, 1, sum) with rowSums(dd^2)
  # rowSums() is significantly faster than apply() for row summation
  if("coord" %in% result){
    dd <- data.frame(elmt$coord[, axes, drop=FALSE])
    coord <- rowSums(dd^2) # x^2 + y2 + ... (vectorized)
    res = cbind(dd, coord = coord)
  }

  # 2. Extract the cos2
  # OPTIMIZATION: Replaced apply() with rowSums() for multi-axis case
  if("cos2" %in% result){
    cos2 <- elmt$cos2[, axes, drop = FALSE]
    if(length(axes) > 1) {
      cos2 <- rowSums(cos2, na.rm=TRUE)
    } else {
      # Single axis - convert to vector to ensure column name is 'cos2'
      cos2 <- as.vector(cos2)
    }
    res <- cbind(res, cos2 = cos2)
  }

  # 3. Extract the contribution
  # OPTIMIZATION: Replaced t(apply()) with sweep() and rowSums()
  # sweep() for element-wise multiplication, rowSums() for summation
  if("contrib" %in% result){
    contrib <- elmt$contrib[, axes, drop = FALSE]
    if(length(axes) > 1) {
      eig <- get_eigenvalue(X)[axes,1]
      # OPTIMIZED: Adjust variable contributions by the Dimension eigenvalues
      # Using sweep() instead of t(apply()) - much faster
      contrib <- sweep(contrib, 2, eig, "*")
      contrib <- rowSums(contrib) / sum(eig)
    } else {
      # Single axis - convert to vector to ensure column name is 'contrib'
      contrib <- as.vector(contrib)
    }
    res <- cbind(res, contrib = contrib)
  }
  
  # 4.Extract the coordinates x, y and coord partial - MFA
  # OPTIMIZATION: Replaced lapply/do.call with vectorized string operations
  # and apply() with rowSums()
  if("coord.partial" %in% result){
    dd <- data.frame(elmt$coord.partiel[, axes, drop=FALSE])
    rnames <- rownames(dd)
    group.names <- .mfa_group_names(X)
    groupnames <- .split_partial_names(rnames, group.names)
    # OPTIMIZED: Use rowSums instead of apply
    coord.partial <- rowSums(dd^2)
    res.partial <- data.frame(groupnames, dd, coord.partial)
  }
  
  # 5. Extract the coordinates x, y and coord partial - HMFA
  # OPTIMIZATION: Pre-allocate vectors and use rowSums instead of apply
  if("coord.node.partial" %in% result){
    # Select hierarchical node
    node <- as.data.frame(elmt[[node.level]])
    n_rows <- nrow(node)
    n_groups <- length(group.names)
    n_axes <- length(axes)

    name <- rep(rownames(node), n_groups)

    # OPTIMIZED: Pre-allocate matrix instead of growing with cbind
    dd <- matrix(0, nrow = n_rows * n_groups, ncol = n_axes)
    dim.group <- character(n_rows * n_groups)

    # Fill the matrix
    row_idx <- 1
    for(j in seq_len(n_groups)) {
      for(i in seq_along(axes)) {
        dim.name <- paste0("Dim", axes[i], ".", j)
        dd[row_idx:(row_idx + n_rows - 1), i] <- node[, dim.name]
      }
      dim.group[row_idx:(row_idx + n_rows - 1)] <- group.names[j]
      row_idx <- row_idx + n_rows
    }

    colnames(dd) <- paste0("Dim", axes)
    dd <- as.data.frame(dd)

    # OPTIMIZED: Use rowSums instead of apply
    coord.partial <- rowSums(dd^2)
    res.partial <- data.frame(group.name = dim.group, name, dd, coord.partial)
  }
  
  if("coord.node.partial" %in% result) 
    res <- res.partial
  else {
    name <- rownames(elmt$coord)
    if(is.null(name)) name <- as.character(seq_len(nrow(elmt$coord)))
    name <- as.character(name)
    # Disambiguate duplicated category names (e.g. FAMD/MFA qualitative
    # variables sharing factor-level names) so they can be used as row names
    # (#184, #140). No-op when names are already unique.
    name <- .disambiguate_category_names(X, name, element, facto_class)
    res <- cbind.data.frame(name = name, res)
    rownames(res) <- name
    if(!is.null(select) && !is.null(select$name) && .factominer_needs_category_map(facto_class, element)){
      select$name <- map_factominer_legacy_names(X, select$name, element = element)
    }
    if(!is.null(select)) res <- .select(res, select)
    if("coord.partial" %in% result){
    res = list(res = res, res.partial = res.partial)
    }
  }

  res
}


# Per-axis result matrix (element x selected dimensions).
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Reuses the same extractor dispatch as facto_summarize() but returns the raw
# per-dimension cos2/contrib matrix WITHOUT summing across axes (facto_summarize
# collapses the axes into one value per element). Used by the fviz_cos2()/
# fviz_contrib() heat-grid display.
.facto_element_matrix <- function(X, element, result, axes){
  element <- element[1]
  facto_class <- .get_facto_class(X)
  elmt <- switch(facto_class,
                 CA   = get_ca(X, element),
                 PCA  = get_pca(X, element),
                 MCA  = get_mca(X, element),
                 FAMD = get_famd(X, element),
                 MFA  = get_mfa(X, element),
                 HMFA = get_hmfa(X, element))
  m <- elmt[[result]]
  if(is.null(m))
    stop("'", result, "' is not available for element = '", element, "'.", call. = FALSE)
  axes <- .validate_axis_indices(axes, ndim = ncol(elmt$coord))
  m <- m[, axes, drop = FALSE]
  colnames(m) <- paste0("Dim", axes)
  nm <- rownames(elmt$coord)
  if(is.null(nm)) nm <- as.character(seq_len(nrow(m)))
  rownames(m) <- as.character(nm)
  m
}


# Heat-grid (element x dimension) of cos2/contrib values.
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Shared by fviz_cos2()/fviz_contrib() when display = "heatmap". Shows one tile
# per (element, dimension) filled by the cos2/contrib value, with the value
# printed in the tile. Elements are ordered by their total over the shown axes,
# and `top` keeps the leading ones (matching the barplot's top-elements idea).
.fviz_result_heatmap <- function(X, choice, result, axes, top = Inf,
                                 fill = "steelblue", ggtheme = theme_minimal(),
                                 title = NULL, legend.title = result){
  m <- .facto_element_matrix(X, choice, result, axes)
  # order elements by total across the shown axes; keep the top ones
  ord <- order(rowSums(m, na.rm = TRUE), decreasing = TRUE)
  m <- m[ord, , drop = FALSE]
  if(is.finite(top) && top < nrow(m)) m <- m[seq_len(top), , drop = FALSE]

  elements <- rownames(m)
  dims <- colnames(m)
  df <- data.frame(
    name  = factor(rep(elements, times = ncol(m)), levels = rev(elements)),
    dim   = factor(rep(dims, each = nrow(m)), levels = dims),
    value = as.vector(m),
    stringsAsFactors = TRUE
  )
  # cos2 in [0,1] reads better with 2 decimals; contrib (%) with 1.
  digits <- if(identical(result, "cos2")) 2 else 1
  df$label <- ifelse(is.na(df$value), "",
                     formatC(df$value, format = "f", digits = digits))
  # Adaptive label colour: white text on dark tiles so the value stays legible
  # for any `fill` (e.g. "navy"), not just the light default. The tile colour is
  # the white->fill gradient at the value's position; pick white when it's dark.
  rng <- range(df$value, na.rm = TRUE)
  tpos <- if(diff(rng) > 0) (df$value - rng[1]) / diff(rng) else rep(0, nrow(df))
  tpos[is.na(tpos)] <- 0
  tile_rgb <- grDevices::colorRamp(c("white", fill), space = "Lab")(tpos)
  lum <- (0.2126 * tile_rgb[, 1] + 0.7152 * tile_rgb[, 2] + 0.0722 * tile_rgb[, 3]) / 255
  df$txt_col <- ifelse(lum < 0.4, "white", "black")
  p <- ggplot(df, aes(x = .data[["dim"]], y = .data[["name"]], fill = .data[["value"]])) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = .data[["label"]], color = .data[["txt_col"]]), size = 3) +
    scale_color_identity() +
    scale_fill_gradient(low = "white", high = fill, name = legend.title,
                        na.value = "white") +
    labs(title = title, x = "", y = "") +
    ggtheme +
    theme(panel.grid = element_blank())
  p
}
