#' @include utilities.R
NULL

# The Okabe-Ito color-universal-design palette (Okabe & Ito, 2008), the same
# colours base R ships as grDevices::palette.colors("Okabe-Ito"). Reordered for
# categorical use on a light background: the high-contrast hues come first, yellow
# (the palest on white) is pushed late so 3-4 group plots stay crisp, and the grey
# and black neutrals are kept last.
.okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#D55E00", # vermillion
  "#0072B2", # blue
  "#CC79A7", # reddish purple
  "#F0E442", # yellow (palest on white -> late)
  "#999999", # grey
  "#000000"  # black
)

#' Colorblind-safe color palette for factoextra plots
#'
#' @description
#' Returns a colorblind-safe categorical color palette as a character vector of
#' hex codes, to pass explicitly to the \code{palette} argument of the
#' \code{fviz_*()} functions (e.g. \code{fviz_cluster(res, palette =
#' factoextra_palette("okabe"))}). The default is the Okabe-Ito
#' color-universal-design palette, which stays distinguishable under the common
#' forms of color-vision deficiency.
#'
#' The palette is returned as a plain color vector (not a ggplot2 scale), for the
#' existing \code{palette} argument. Use it for \emph{discrete} group coloring
#' (\code{habillage}, a factor \code{col.ind}, or clusters), where it also tints
#' the matching ellipse fills. For a \emph{continuous} metric (e.g.
#' \code{col.ind = "cos2"}) use \code{gradient.cols} instead — a categorical
#' palette does not apply there. It introduces no global option and no hidden
#' state. The colors are ordered so the highest-contrast hues come first; yellow,
#' the palest on a white background, is placed late, so plots with a few groups
#' stay crisp.
#'
#' @param palette name of the palette. Currently \code{"okabe"} (alias
#'   \code{"okabe-ito"}), the Okabe-Ito color-universal-design set.
#' @param n number of colors to return. \code{NULL} (default) returns the whole
#'   palette. If \code{n} is greater than the number of available colors the
#'   palette is recycled (with a message), since recycling defeats the
#'   distinguishability the palette is chosen for.
#'
#' @return A character vector of hex color codes.
#'
#' @details The Okabe-Ito colors are those of \code{grDevices::palette.colors(
#'   palette = "Okabe-Ito")} (here reordered so the vivid hues lead, with grey and
#'   black last). They were designed by Masataka \enc{Okabe}{Okabe} and Kei Ito as
#'   a color-universal-design set, and popularized for figures by Wong (2011).
#'
#' @references
#' Okabe, M. and Ito, K. (2008). \emph{Color Universal Design (CUD): How to make
#' figures and presentations that are friendly to colorblind people.}
#' \url{https://jfly.uni-koeln.de/color/}.
#'
#' Wong, B. (2011). Points of view: Color blindness. \emph{Nature Methods}, 8(6),
#' 441. \doi{10.1038/nmeth.1618}.
#'
#' @seealso \code{\link{theme_factoextra}}
#'
#' @examples
#' # A colorblind-safe categorical palette
#' factoextra_palette("okabe")
#' factoextra_palette("okabe", n = 3)
#'
#' \donttest{
#' # Publication-grade recipe: colorblind-safe groups + a clean theme
#' data(iris)
#' km <- kmeans(scale(iris[, 1:4]), 3, nstart = 25)
#' fviz_cluster(km, data = iris[, 1:4],
#'              palette = factoextra_palette("okabe"),
#'              ggtheme = theme_factoextra())
#' }
#' @export
factoextra_palette <- function(palette = "okabe", n = NULL){
  pal <- match.arg(tolower(palette), c("okabe", "okabe-ito"))
  cols <- .okabe_ito
  if(is.null(n)) return(cols)
  n <- .coerce_integerish(n, "n", lower = 1L)
  if(n <= length(cols)) return(cols[seq_len(n)])
  message("factoextra_palette(): ", n, " colors requested but the palette has ",
          length(cols), "; recycling (colors will repeat and may be hard to tell ",
          "apart).")
  rep_len(cols, n)
}


#' A clean publication theme for factoextra plots
#'
#' @description
#' A ggplot2 theme tuned for the dimension-reduction scatter plots and biplots
#' produced by the \code{fviz_*()} functions: a light, unobtrusive coordinate grid
#' (so points can be read against the axes), a clean background, and de-emphasized
#' axis lines. Pass it through the \code{ggtheme} argument, e.g.
#' \code{fviz_pca_ind(res, ggtheme = theme_factoextra())}, or add it to any
#' returned plot with \code{+ theme_factoextra()}.
#'
#' @param base_size base font size, in points.
#' @param base_family base font family.
#'
#' @return A ggplot2 theme object.
#'
#' @seealso \code{\link{factoextra_palette}}
#'
#' @examples
#' \donttest{
#' data(iris)
#' res.pca <- prcomp(iris[, 1:4], scale. = TRUE)
#' fviz_pca_ind(res.pca, habillage = iris$Species,
#'              palette = factoextra_palette("okabe"),
#'              ggtheme = theme_factoextra())
#'
#' # or add it to an existing plot
#' fviz_pca_var(res.pca) + theme_factoextra()
#' }
#' @export
theme_factoextra <- function(base_size = 12, base_family = ""){
  half_line <- base_size / 2
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # A light coordinate grid (major only) to read points against the axes;
      # this is the deliberate difference from theme_pubr(), which has none.
      panel.grid.major = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_line(color = "grey70", linewidth = 0.3),
      # Dark, legible tick labels (values) and titles for print. theme_minimal's
      # default is a light grey30 at 0.8x, which is faint on the page.
      axis.text        = ggplot2::element_text(color = "grey20", size = ggplot2::rel(0.9)),
      axis.title       = ggplot2::element_text(face = "bold", color = "grey10"),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size * 1.2,
                                               margin = ggplot2::margin(b = half_line)),
      plot.subtitle    = ggplot2::element_text(color = "grey30",
                                               margin = ggplot2::margin(b = half_line)),
      legend.title     = ggplot2::element_text(face = "bold"),
      legend.position  = "right",
      plot.margin      = ggplot2::margin(half_line, half_line, half_line, half_line)
    )
}
