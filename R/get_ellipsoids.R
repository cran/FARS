#' @title Generic Function to Extract Ellipsoids
#'
#' @param x An object from which to extract the ellipsoids
#' @param ... Additional arguments.
#'
#' @return A list of matrices defining the ellipsoids at each time.
#' @export
get_ellipsoids <- function(x, ...) {
  UseMethod("get_ellipsoids")
}


#' @title Get Ellipsoids from a \code{fars_scenario} Object.
#'
#' @description Returns the list of ellipsoids from a \code{fars_scenario} object.
#'
#' @param x An object of class \code{fars_scenario}.
#' @param ... Additional arguments (ignored).
#' 
#' @return A list of matrices defining the ellipsoids at each time.
#' @export
get_ellipsoids.fars_scenario <- function(x, ...) {
  stopifnot(inherits(x, "fars_scenario"))
  x$ellipsoids
}
