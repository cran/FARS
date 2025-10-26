#' @title Generic Function to Extract Estimated Factors
#'
#' @param x An object from which to extract the estimated factors.
#' @param ... Additional arguments.
#'
#' @return A matrix of estimated factors.
#' @export
factors <- function(x, ...) {
  UseMethod("factors")
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
#' factors(mldfm_result)
#'
#' @export
factors.mldfm <- function(x, ...) {
  x$factors
}

#' @title Generic Function to Extract Factor Loadings
#'
#' @param x An object from which to extract the factor loadings.
#' @param ... Additional arguments.
#'
#' @return A matrix of factor loadings.
#' @export
loadings <- function(x, ...) {
  UseMethod("loadings")
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
#' loadings(mldfm_result)
#'
#' @export
loadings.mldfm <- function(x, ...) {
  x$loadings
}

#' @title Extract Residuals from a \code{mldfm} Object
#'
#' @param object An object of class \code{mldfm}.
#' @param ... Further arguments (ignored).
#'
#' @return A matrix containing the residuals.
#'
#' @examples
#' mldfm_result <- mldfm(data = matrix(rnorm(100 * 5), 100, 5), blocks = 1, global = 2)
#' residuals(mldfm_result)
#'
#' @method residuals mldfm
#'
#' @export
residuals.mldfm <- function(object, ...) {
  object$residuals
}

#' @title Extract Fitted Values from a \code{mldfm} Object
#'
#' @param object An object of class \code{mldfm}.
#' @param ... Further arguments (ignored).
#'
#' @return A matrix containing the fitted values.
#'
#' @examples
#' mldfm_result <- mldfm(data = matrix(rnorm(100 * 5), 100, 5), blocks = 1, global = 2)
#' fitted(mldfm_result)
#'
#' @method fitted mldfm
#'
#' @export
fitted.mldfm <- function(object, ...) {
  object$fitted
}
