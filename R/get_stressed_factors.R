#' @title Generic Function to Extract Stressed Factors
#'
#' @param x An object from which to extract the stressed factors.
#' @param ... Additional arguments (ignored).
#'
#' @return A matrix of stressed factors.
#' @export
get_stressed_factors <- function(x, ...) {
  UseMethod("get_stressed_factors")
}
#' @title Extract Stressed Factors from a \code{fars} Object
#'
#' @description Extracts the stressed factors from a \code{fars} object. If stressed factors are not available,
#'             it returns NULL.
#'
#' @param x An object of class \code{fars}.
#' @param ... Additional arguments (ignored).
#'
#' @return A matrix containing the stressed factors if available, otherwise NULL.
#'
#' @examples
#' fars_result <- compute_fars(dep_variable = rnorm(100), factors = matrix(rnorm(100 * 3), ncol = 3))
#' get_stressed_factors(fars_result)  
#'
#' @export
get_stressed_factors.fars <- function(x, ...) {
  stopifnot(inherits(x, "fars"))
  
  # If stressed factors are available
  if (!is.null(x$stressed_factors)) {
    return(x$stressed_factors)
  }
  
  # If stressed factors are not available
  return(NULL)
}
