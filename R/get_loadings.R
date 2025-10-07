#' @title Generic Function to Extract Factor Loadings
#'
#' @param x An object from which to extract the factor loadings.
#' @param ... Additional arguments.
#'
#' @return A matrix of factor loadings.
#' @export
get_loadings <- function(x, ...) {
  UseMethod("get_loadings")
}

#' @title Extract Factor Loadings from a \code{mldfm} Object
#'
#' @param x An object of class \code{mldfm}.
#' @param ... Further arguments (ignored).
#'
#' @return A matrix containing the estimated factor loadings.
#'
#' @examples
#' mldfm_result <- mldfm(data = matrix(rnorm(100 * 5), 100, 5), blocks = 1, global = 2)
#' get_loadings(mldfm_result)
#'
#' @export
get_loadings.mldfm <- function(x, ...) {
  stopifnot(inherits(x, "mldfm"))
  x$loadings
}
