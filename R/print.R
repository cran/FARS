#' Print Method for \code{mldfm} Object
#'
#' @description Prints a short summary of the MLDFM object.
#' 
#' @param x An object of class \code{mldfm}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input \code{mldfm} object, invisibly.
#'
#' @method print mldfm
#' @export
print.mldfm <- function(x, ...) {
  factors <- factors(x)
  
  cat("Multilevel Dynamic Factor Model (MLDFM)\n")
  cat("=======================================\n")
  cat("Number of periods:", nrow(factors), "\n")
  cat("Number of factors:", ncol(factors), "\n")
  cat("Number of nodes  :", length(x$factors_list), "\n")
  
  
  
  cat("\nFactor structure:\n")
  for (key in names(x$factors_list)) {
    cat(" -", key, ": ", x$factors_list[[key]], "factor(s)\n")
  }
  
  
  invisible(x)
}





#' @title Print Method for \code{mldfm_subsample} Object
#'
#' @description Prints a brief summary of the \code{mldfm_subsample} object.
#'
#' @param x An object of class \code{mldfm_subsample}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input object \code{object}, invisibly.
#'
#' @method print mldfm_subsample
#' @export
print.mldfm_subsample <- function(x, ...) {
  
  cat("MLDFM Subsampling\n")
  cat("==========================\n")
  cat("Number of subsamples :", x$n_samples, "\n")
  cat("Sample size fraction :", x$sample_size, "\n")
  if (!is.null(x$seed)) cat("Seed used            :", x$seed, "\n")
  
  models <- get_mldfm_list(x)
  if (length(models) > 0 && inherits(models[[1]], "mldfm")) {
    model1 <- get_mldfm_model(x, 1)
    T_obs <- nrow(model1$residuals)
    N_vars <- ncol(model1$residuals)
    cat("Data dimensions      :", T_obs, "periods,", N_vars, "variables\n")
    
    cat("\nFactor structure:\n")
    f_list <- model1$factors_list
    for (key in names(f_list)) {
      cat(" -", key, ": ", f_list[[key]], "factor(s)\n")
    }
  }
  
  invisible(x)
}





#' @title Print Method for \code{fars} Object
#'
#' @description Prints a short summary of the fars object.
#'
#' @param x An object of class \code{fars}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input \code{fars} object, returned invisibly. 
#'
#' @method print fars
#' @export
print.fars <- function(x, ...) {
  
  cat("Factor-Augmented Quantile Regressions (FARS)\n")
  cat("===========================================\n")
  
  # Summary of forecasted quantiles
  cat("Number of periods: ", x$periods, "\n")
  cat("Number of factors: ", x$n_factors, "\n")
  cat("Lag: ", x$h, "\n")
  cat("Quantile levels: ", formatC(get_quantile_levels(x), format = "f", digits = 2), "\n")
  
  invisible(x)
}





#' @title Print Method for \code{fars_scenario} Object
#'
#' @description Prints a short summary of the FARS scenario object.
#' 
#' @param x An object of class \code{fars_scenario}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input \code{fars_scenario} object, invisibly.
#'
#' @method print fars_scenario
#' @export
print.fars_scenario <- function(x, ...) {
  
  cat("FARS Scenario\n")
  cat("=====================\n")
  cat("Number of periods    :", x$periods, "\n")
  cat("Ellipsoid dimensions :", ncol(x$center), "\n")
  cat("Points per ellipsoid :", x$n_points, "\n")
  cat("Confidence level     :", round(x$alpha * 100), "%\n")
  cat("FPR Gamma            :", ifelse(isTRUE(x$call$fpr), "TRUE", "FALSE"), "\n")
  
  
  invisible(x)
}





#' @title Print Method for \code{fars_density} Object
#'
#' @description Displays a brief summary of the \code{fars_density} object.
#'
#' @param x An object of class \code{fars_density}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input \code{fars_density} object, invisibly.
#' 
#' @method print fars_density
#' @export
print.fars_density <- function(x, ...) {
  cat("FARS Density\n")
  cat("====================\n")
  cat("Time observations  :", nrow(x$density), "\n")
  cat("Estimation points  :", ncol(x$density), "\n")
  cat("Random samples     :", ncol(get_distribution(x)), "\n")
  cat("Support range      : [", min(x$eval_points), ",", max(x$eval_points), "]\n")
  cat("Optimization       :", x$optimization,"\n")
  invisible(x)
}

