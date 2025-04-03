#' Print method for fars_density objects
#'
#' @description Displays a brief summary of the density estimation object produced by the \code{density()} or \code{nl_density()} function.
#'
#' @param x An object of class \code{fars_density}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input object \code{x}, returned invisibly.
#' 
#' @method print fars_density
#' @export
print.fars_density <- function(x, ...) {
  cat("FARS Density\n")
  cat("====================\n")
  cat("Time observations  :", nrow(x$density), "\n")
  cat("Estimation points  :", ncol(x$density), "\n")
  cat("Random samples     :", ncol(x$distribution), "\n")
  cat("Range of x values  : [", min(x$x_vals), ",", max(x$x_vals), "]\n")
  invisible(x)
}

