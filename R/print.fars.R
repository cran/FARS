#' Print method for fars object
#'
#' @description Prints a short summary of the fars object
#'
#' @param x An object of class \code{fars_quantiles}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input object \code{x}, returned invisibly. 
#'
#' @method print fars
#' @export
print.fars <- function(x, ...) {
  cat("Factor-Augmented Quantile Regressions (FARS)\n")
  cat("===========================================\n")
  cat("Forecasted quantiles:\n")
  cat(" - Time periods: ", nrow(x$Quantiles), "\n")
  cat(" - Quantile levels: ", formatC(x$Levels, format = "f", digits = 2), "\n\n")
  
  if (!is.null(x$Stressed_Quantiles)) {
    cat("Stressed quantiles: YES\n")
  } else {
    cat("Stressed quantiles: NO\n")
  }
  invisible(x)
}