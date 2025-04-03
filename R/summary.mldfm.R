#' Summary Method for MLDFM Object
#'
#' @description Provides a complete summary of the multilevel dynamic factor model 
#'
#' @param object An object of class \code{mldfm}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input object \code{object}, invisibly. 
#'
#' @method summary mldfm
#' @export
summary.mldfm <- function(object, ...) {
  cat("Summary of Multilevel Dynamic Factor Model (MLDFM)\n")
  cat("===================================================\n")
  
  cat("Number of observations: ", nrow(object$Factors), "\n")
  cat("Total number of factors: ", ncol(object$Factors), "\n")
  cat("Number of nodes: ", length(object$Factors_list), "\n")
  
  if (!is.null(object$Method)) {
    cat("Initialization method: ", object$Method, "\n")
  }
  
  if (!is.null(object$Iterations)) {
    cat("Number of iterations to converge: ", object$Iterations, "\n")
  }
  
  cat("\nFactors per node:\n")
  for (key in names(object$Factors_list)) {
    cat(" -", key, ": ", object$Factors_list[[key]], "factor(s)\n")
  }
  
  if (!is.null(object$Residuals)) {
    rss <- sum(object$Residuals^2)
    avg_rss <- mean(rowSums(object$Residuals^2))
    cat("\nResidual sum of squares (RSS): ", formatC(rss, format = "f", digits = 4), "\n")
    cat("Average RSS per observation: ", formatC(avg_rss, format = "f", digits = 4), "\n")
  }
  
  invisible(object)
}
