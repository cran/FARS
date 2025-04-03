#' Summary method for fars_density objects
#'
#' Provides summary statistics of the density estimation for each time observation,
#' including the mean, median, and standard deviation of the simulated distribution.
#'
#' @param object An object of class \code{fars_density}.
#' @param ... Additional arguments (ignored).
#'
#' @return A data frame with one row per time observation and columns:
#' \code{Observation}, \code{Mean}, \code{Median}, and \code{StdDev}. 
#' The object is also printed to the console and returned invisibly.
#' 
#' @importFrom stats median
#' 
#' @method summary fars_density
#' @export
summary.fars_density <- function(object, ...) {
  means <- rowMeans(object$distribution)
  medians <- apply(object$distribution, 1, median)
  sds <- apply(object$distribution, 1, sd)
  
  summary_df <- data.frame(
    Observation = seq_along(means),
    Mean = round(means, 4),
    Median = round(medians, 4),
    StdDev = round(sds, 4)
  )
  
  print(summary_df)
  invisible(summary_df)
}
