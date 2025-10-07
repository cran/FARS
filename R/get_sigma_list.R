#' @title Generic Function to Get Sigma List
#'
#' @param x An object from which to extract the sigma list.
#' @param ... Additional arguments.
#'
#' @return A list of covariance matrices.
#' @export
get_sigma_list <- function(x, ...) {
  UseMethod("get_sigma_list")
}


#' @title Extract Sigma List from \code{fars_scenario}
#'
#' @description Returns the list of covariance matrices used to construct the ellipsoids.
#'
#' @param x An object of class \code{fars_scenario}.
#' @param ... Additional arguments (ignored).
#' 
#' @return A list of covariance matrices (one per period).
#' @export
get_sigma_list <- function(x, ...) {
  stopifnot(inherits(x, "fars_scenario"))
  x$sigma
}
