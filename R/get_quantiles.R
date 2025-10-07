#' @title Generic Function to Extract Quantiles
#'
#' @param x An object from which to extract the quantiles.
#' @param ... Additional arguments (ignored).
#'
#' @return A matrix of quantiles.
#' @export
get_quantiles <- function(x, ...) {
  UseMethod("get_quantiles")
}


#' @title Extract Quantiles from a \code{fars} Object
#'
#' @description Extracts either the non-stressed quantiles or the stressed quantiles from a \code{fars} object, depending on the \code{stressed} parameter.
#' If the requested stressed quantiles are not available, it returns NULL.
#'
#' @param x An object of class \code{fars}.
#' @param stressed Logical. If \code{TRUE}, the function returns the stressed quantiles. If \code{FALSE} (default), it returns the non-stressed quantiles.
#' @param ... Additional arguments (ignored).
#'
#' @return A matrix containing either the non-stressed quantiles or the stressed quantiles, depending on the value of \code{stressed}.
#'         If stressed quantiles are requested but not available, it returns NULL.
#'
#' @examples
#' fars_result <- compute_fars(dep_variable = rnorm(100), factors = matrix(rnorm(100 * 3), ncol = 3))
#' get_quantiles(fars_result)  
#'
#' @export
get_quantiles.fars <- function(x, stressed = FALSE, ...) {
  stopifnot(inherits(x, "fars"))
  
  if (stressed) {
    if (is.null(x$stressed_quantiles)) {
      return(NULL) 
    } else {
      return(x$stressed_quantiles)
    }
  } else {
    return(x$quantiles)
  }
}
