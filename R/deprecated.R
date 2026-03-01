#' @include print.factoextra.R fviz_mfa.R fviz_hmfa.R
NULL
#' Deprecated Functions
#'
#' @description Deprecated functions. Will be removed in a future version.
#'
#' \itemize{
#' \item get_mfa_quanti_var(). Deprecated. Use get_mfa_var(res.mfa, "quanti.var") instead.
#' \item get_mfa_quali_var().  Deprecated. Use get_mfa_var(res.mfa, "quali.var") instead.
#' \item get_mfa_group(). Deprecated. Use get_mfa_var(res.mfa, "group") instead.
#'
#' \item{fviz_mfa_ind_starplot(): Star graph of individuals (draws partial points).
#' Deprecated. Use fviz_mfa_ind(res.mfa, partial = "all") instead.}
#' \item{fviz_mfa_quanti_var(): Graph of quantitative variables. Deprecated. Use fviz_mfa(X, "quanti.var") instead.}
#' \item{fviz_mfa_quali_var(): Graph of qualitative variables. Deprecated. Use fviz_mfa(X, "quali.var") instead.}
#'
#'  \item
#' get_hmfa_quanti_var(). Deprecated. Use get_hmfa_var(res.hmfa, "quanti.var")
#' instead. \item get_hmfa_quali_var().  Deprecated. Use get_hmfa_var(res.hmfa,
#' "quali.var") instead. \item get_hmfa_group(). Deprecated. Use
#' get_hmfa_var(res.hmfa, "group") instead.
#'
#' \item{fviz_hmfa_ind_starplot(): Graph of partial individuals. Deprecated. Use fviz_hmfa_ind(X, partial = "all") instead.}
#' \item{fviz_hmfa_quanti_var(): Graph of quantitative variables. Deprecated. Use fviz_hmfa_var(X, "quanti.var") instead.}
#' \item{fviz_hmfa_quali_var(): Graph of qualitative variables. Deprecated. Use fviz_hmfa_var(X, "quali.var") instead.}
#' \item{fviz_hmfa_group(): Graph of the groups representation. Deprecated. Use fviz_hmfa_var(X, "group") instead.}
#'
#' }
#'
#' @param res.hmfa an object of class HMFA [FactoMineR].
#' @param res.mfa an object of class MFA [FactoMineR].
#' @param X an object of class MFA or HMFA [FactoMineR].
#' @param ... Other arguments.
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @name deprecated
NULL

# Internal helper function for deprecation warnings
# Consolidates duplicated warning+dispatch pattern
# Not exported or documented
.deprecate_warn <- function(old_fn, new_fn) {
  .Deprecated(new_fn, package = "factoextra", old = old_fn)
}

# ============================================================================
# MFA deprecated functions
# ============================================================================

#' @rdname deprecated
#' @export
get_mfa_quanti_var <- function(res.mfa) {
  .deprecate_warn("get_mfa_quanti_var", "get_mfa_var(res.mfa, 'quanti.var')")
  get_mfa_var(res.mfa, "quanti.var")
}

#' @rdname deprecated
#' @export
get_mfa_quali_var <- function(res.mfa) {
  .deprecate_warn("get_mfa_quali_var", "get_mfa_var(res.mfa, 'quali.var')")
  get_mfa_var(res.mfa, "quali.var")
}

#' @rdname deprecated
#' @export
get_mfa_group <- function(res.mfa) {
  .deprecate_warn("get_mfa_group", "get_mfa_var(res.mfa, 'group')")
  get_mfa_var(res.mfa, "group")
}

#' @rdname deprecated
#' @export
fviz_mfa_ind_starplot <- function(X, ...) {
  .deprecate_warn("fviz_mfa_ind_starplot", "fviz_mfa_ind(X, partial = 'all')")
  fviz_mfa_ind(X, partial = "all", ...)
}

#' @rdname deprecated
#' @export
fviz_mfa_group <- function(X, ...) {
  .deprecate_warn("fviz_mfa_group", "fviz_mfa_var(X, 'group')")
  fviz_mfa_var(X, choice = "group", ...)
}

#' @rdname deprecated
#' @export
fviz_mfa_quanti_var <- function(X, ...) {
  .deprecate_warn("fviz_mfa_quanti_var", "fviz_mfa_var(X, 'quanti.var')")
  fviz_mfa_var(X, choice = "quanti.var", ...)
}

#' @rdname deprecated
#' @export
fviz_mfa_quali_var <- function(X, ...) {
  .deprecate_warn("fviz_mfa_quali_var", "fviz_mfa_var(X, 'quali.var')")
  fviz_mfa_var(X, choice = "quali.var", ...)
}

# ============================================================================
# HMFA deprecated functions
# ============================================================================

#' @rdname deprecated
#' @export
get_hmfa_quanti_var <- function(res.hmfa) {
  .deprecate_warn("get_hmfa_quanti_var", "get_hmfa_var(res.hmfa, 'quanti.var')")
  get_hmfa_var(res.hmfa, "quanti.var")
}

#' @rdname deprecated
#' @export
get_hmfa_quali_var <- function(res.hmfa) {
  .deprecate_warn("get_hmfa_quali_var", "get_hmfa_var(res.hmfa, 'quali.var')")
  get_hmfa_var(res.hmfa, "quali.var")
}

#' @rdname deprecated
#' @export
get_hmfa_group <- function(res.hmfa) {
  .deprecate_warn("get_hmfa_group", "get_hmfa_var(res.hmfa, 'group')")
  get_hmfa_var(res.hmfa, "group")
}

#' @rdname deprecated
#' @export
fviz_hmfa_quanti_var <- function(X, ...) {
  .deprecate_warn("fviz_hmfa_quanti_var", "fviz_hmfa_var(X, 'quanti.var')")
  fviz_hmfa_var(X, "quanti.var", ...)
}

#' @rdname deprecated
#' @export
fviz_hmfa_quali_var <- function(X, ...) {
  .deprecate_warn("fviz_hmfa_quali_var", "fviz_hmfa_var(X, 'quali.var')")
  fviz_hmfa_var(X, "quali.var", ...)
}

#' @rdname deprecated
#' @export
fviz_hmfa_ind_starplot <- function(X, ...) {
  .deprecate_warn("fviz_hmfa_ind_starplot", "fviz_hmfa_ind(X, partial = 'all')")
  fviz_hmfa_ind(X, partial = "all", ...)
}

#' @rdname deprecated
#' @export
fviz_hmfa_group <- function(X, ...) {
  .deprecate_warn("fviz_hmfa_group", "fviz_hmfa_var(X, 'group')")
  fviz_hmfa_var(X, choice = "group", ...)
}
