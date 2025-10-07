#' @title Summary Method for \code{mldfm} Object
#'
#' @description Provides a complete summary of the MLDFM object.
#'
#' @param object An object of class \code{mldfm}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input \code{mldfm} object, invisibly. 
#'
#' @method summary mldfm
#' @export
summary.mldfm <- function(object, ...) {
  stopifnot(inherits(object, "mldfm"))
  factors <- get_factors(object)
  residuals <- get_residuals(object)
  
  cat("Summary of Multilevel Dynamic Factor Model (MLDFM)\n")
  cat("===================================================\n")
  cat("Number of periods               :", nrow(factors), "\n")
  cat("Number of factors               :", ncol(factors), "\n")
  cat("Number of nodes                 :", length(object$factors_list), "\n")
  
  if (!is.null(object$method)) {
    cat("Initialization method           :", object$method, "\n")
  }
  
  if (!is.null(object$iterations)) {
    cat("Number of iterations to converge:", object$iterations, "\n")
  }
  
  cat("\nFactor structure:\n")
  for (key in names(object$factors_list)) {
    cat(" -", key, ": ", object$factors_list[[key]], "factor(s)\n")
  }
  
  if (!is.null(residuals)) {
    rss <- sum(residuals^2)
    avg_rss <- mean(rowSums(residuals^2))
    cat("\nResidual diagnostics:\n")
    cat(" - Total residual sum of squares (RSS): ", formatC(rss, format = "f", digits = 2), "\n")
    cat(" - Average RSS per time period        : ", formatC(avg_rss, format = "f", digits = 2), "\n")
  }
  
  invisible(object)
}

#' @title Summary Method for \code{mldfm_subsample} Object
#'
#' @description Provides a structured summary of a \code{mldfm_subsample} object.
#'
#' @param object An object of class \code{mldfm_subsample}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input object \code{object}, invisibly.
#'
#' @method summary mldfm_subsample
#' @export
summary.mldfm_subsample <- function(object, ...) {
  stopifnot(inherits(object, "mldfm_subsample"))
  
  cat("Summary of MLDFM Subsampling\n")
  cat("=============================\n")
  cat("Number of subsamples :", object$n_samples, "\n")
  cat("Sample size fraction :", object$sample_size, "\n")
  if (!is.null(object$seed)) cat("Seed used            :", object$seed, "\n")
  
  if (length(object$models) > 0 && inherits(object$models[[1]], "mldfm")) {
    T_obs <- nrow(get_residuals(object$models[[1]]))
    N_vars <- ncol(get_residuals(object$models[[1]]))
    cat("Data dimensions      :", T_obs, "periods,", N_vars, "variables\n")
    
    # Factor structure from first model
    cat("\nFactor structure:\n")
    f_list <- object$models[[1]]$factors_list
    for (key in names(f_list)) {
      cat(" -", key, ": ", f_list[[key]], "factor(s)\n")
    }
    
    # Estimation method 
    cat("\nEstimation method:", object$models[[1]]$method, "\n")
    
    # Iterations 
    iterations <- sapply(object$models, function(m) m$iterations)
    cat("Iterations       : mean =", round(mean(iterations), 2),
        "| min =", min(iterations),
        "| max =", max(iterations), "\n")
    
    # Compute RSS from residuals
    rss_vals <- sapply(object$models, function(m) {
      res <- get_residuals(m)
      sum(res^2, na.rm = TRUE)
    })
    cat("Final RSS        : mean =", round(mean(rss_vals), 2),
        "| min =", round(min(rss_vals), 2),
        "| max =", round(max(rss_vals), 2), "\n")
  }
  
  invisible(object)
}

#' @title Summary Method for \code{fars} Object
#'
#' @description Prints a complete summary of the fars object, including information on estimated quantiles, stressed quantiles,
#' regression coefficients, standard errors, and p-values.
#'
#' @param object An object of class \code{fars}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input \code{fars} object, returned invisibly.
#'
#' @method summary fars
#' @export
summary.fars <- function(object, ...) {
  cat("Factor-Augmented Quantile Regressions (FARS)\n")
  cat("===========================================\n")
  cat("Summary of Quantile Regressions\n\n")
  
  levels <- object$levels
  coeff <- object$coeff
  stderr <- object$std_error
  pval <- object$pvalue
  variables <- rownames(coeff)
  
  for (i in seq_along(levels)) {
    cat(sprintf("-------------------------\n Quantile: %.2f \n-------------------------\n", levels[i]))
    est <- coeff[, i]
    se <- stderr[, i]
    p <- pval[, i]
    
    summary_df <- data.frame(
      Estimate = formatC(est, digits = 3, format = "f"),
      `Std. Error` = formatC(se, digits = 3, format = "f"),
      `P-value` = formatC(p, digits = 3, format = "f"),
      row.names = variables,
      check.names = FALSE
    )
    
    print(summary_df)
    cat("\n")
  }
  
  invisible(object)
}


#' @title Summary Method for \code{fars_scenario} Object
#'
#' @description Provides a detailed summary of the FARS scenario object.
#'
#' @param object An object of class \code{fars_scenario}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input \code{fars_scenario} object, invisibly.
#'
#' @method summary fars_scenario
#' @export
summary.fars_scenario <- function(object, ...) {
  stopifnot(inherits(object, "fars_scenario"))
  
  cat("FARS Scenario Summary\n")
  cat("======================\n")
  cat("Number of periods    :", object$periods, "\n")
  cat("Ellipsoid dimensions :", ncol(object$center), "\n")
  cat("Points per ellipsoid :", object$n_points, "\n")
  cat("Confidence level     :", round(object$alpha * 100), "%\n")
  cat("FPR Gamma            :", ifelse(isTRUE(object$call$fpr), "TRUE", "FALSE"), "\n")
  
  
  center_vals <- as.vector(object$center)
  cat("\nCenter (factor estimates):\n")
  cat("  Mean     :", round(mean(center_vals), 4), "\n")
  cat("  Std. Dev :", round(sd(center_vals), 4), "\n")
  cat("  Min      :", round(min(center_vals), 4), "\n")
  cat("  Max      :", round(max(center_vals), 4), "\n")
  
  
  sigma_vals <- unlist(lapply(object$sigma, function(S) diag(S)))
  cat("\nEllipsoid variability (diagonal of Sigma):\n")
  cat("  Mean     :", round(mean(sigma_vals), 4), "\n")
  cat("  Std. Dev :", round(sd(sigma_vals), 4), "\n")
  cat("  Min      :", round(min(sigma_vals), 4), "\n")
  cat("  Max      :", round(max(sigma_vals), 4), "\n")
  
  invisible(object)
}


#' @title Summary Method for \code{fars_density} Object
#'
#' @description Displays a complete summary of the \code{fars_density} object.
#'
#' @param object An object of class \code{fars_density}.
#' @param ... Additional arguments (ignored).
#'
#' @return The input \code{fars_density} object, invisibly.
#' 
#' @importFrom stats median
#' 
#' @method summary fars_density
#' @export
summary.fars_density <- function(object, ...) {
  stopifnot(inherits(object, "fars_density"))
  
  cat("FARS Density Summary\n")
  cat("=========================\n")
  
  means <- rowMeans(get_distribution(object))
  medians <- apply(get_distribution(object), 1, median)
  sds <- apply(get_distribution(object), 1, sd)
  
  summary_df <- data.frame(
    Mean = round(means, 4),
    Median = round(medians, 4),
    StdDev = round(sds, 4)
  )
  
  print(summary_df)
  invisible(summary_df)
}
