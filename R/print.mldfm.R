#' Print Method for MLDFM Object
#'
#' @description Prints a short summary of the multilevel dynamic factor model 
#' 
#' @param x An object of class \code{mldfm}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input object \code{x}, invisibly. 
#'
#' @method print mldfm
#' @export
print.mldfm <- function(x, ...) {
  cat("Multilevel Dynamic Factor Model (MLDFM)\n")
  cat("=======================================\n")
  cat("Time periods:", nrow(x$Factors), "\n")
  cat("Total number of factors:", ncol(x$Factors), "\n")
  cat("Number of nodes:", length(x$Factors_list), "\n\n")
  
  
  
  cat("Factors per node:\n")
  for (key in names(x$Factors_list)) {
    cat(" -", key, ":", x$Factors_list[[key]], "factor(s)\n")
  }
  
  
  invisible(x)
}
