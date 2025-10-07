#' @title Generic Function to Extract a Specific \code{mldfm} Object
#'
#' @param x An object from which to extract a \code{mldfm} Object
#' @param index Integer. The position of the desired model
#' @param ... Additional arguments.
#'
#' @return  A single \code{mldfm} object.
#' @export
get_mldfm_model <- function(x, index, ...) {
  UseMethod("get_mldfm_model")
}

#' @title Extract a Specific \code{mldfm} Object from a \code{mldfm_subsample} Object
#'
#' @description Returns the \code{mldfm} object at the specified position in a \code{mldfm_subsample} object.
#'
#' @param x An object of class \code{mldfm_subsample}.
#' @param index Integer. The position of the desired model (between 1 and \code{n_samples}).
#' @param ... Additional arguments (ignored).
#'
#' @return A single \code{mldfm} object.
#' @export
get_mldfm_model.mldfm_subsample <- function(x, index, ...) {
  stopifnot(inherits(x, "mldfm_subsample"))
  
  n_models <- length(x$models)
  if (!is.numeric(index) || length(index) != 1 || index < 1 || index > n_models) {
    stop(sprintf("`index` must be a single integer between 1 and %d", n_models))
  }
  
  x$models[[index]]
}
