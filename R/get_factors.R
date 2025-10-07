#' @title Generic Function to Extract Estimated Factors
#'
#' @param x An object from which to extract the estimated factors.
#' @param ... Additional arguments.
#'
#' @return A matrix of estimated factors.
#' @export
get_factors <- function(x, ...) {
  UseMethod("get_factors")
}

#' @title Extract Estimated Factors from a \code{mldfm} Object
#'
#' @param x An object of class \code{mldfm}.
#' @param ... Further arguments (ignored).
#'
#' @return A matrix containing the estimated factors.
#'
#' @examples
#' mldfm_result <- mldfm(data = matrix(rnorm(100 * 5), 100, 5), blocks = 1, global = 2)
#' get_factors(mldfm_result)
#'
#' @export
get_factors.mldfm <- function(x, ...) {
  stopifnot(inherits(x, "mldfm"))
  x$factors
}
