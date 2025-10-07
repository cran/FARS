#' @title Generic Function to Extract Residuals
#'
#' @param x An object from which to extract the residuals.
#' @param ... Additional arguments (ignored).
#'
#' @return A matrix of residuals.
#' @export
get_residuals <- function(x, ...) {
  UseMethod("get_residuals")
}

#' @title Extract Residuals from a \code{mldfm} Object
#'
#' @param x An object of class \code{mldfm}.
#' @param ... Further arguments (ignored).
#'
#' @return A matrix containing the residuals.
#'
#' @examples
#' mldfm_result <- mldfm(data = matrix(rnorm(100 * 5), 100, 5), blocks = 1, global = 2)
#' get_residuals(mldfm_result)
#'
#' @export
get_residuals.mldfm <- function(x, ...) {
  stopifnot(inherits(x, "mldfm"))
  x$residuals
}
