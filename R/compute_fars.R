#' @title Compute Factor Augmented Quantile Regressions 
#'
#' @description Performs quantile regressions of a dependent variable on MLDFM factors.
#' Optionally generates quantile forecasts under stressed scenarios using the ellipsoids.
#'
#' @param dep_variable A numeric vector representing the dependent variable (e.g., GDP growth, inflation).
#' @param factors A matrix or data frame of factor estimates, typically extracted from an MLDFM model.
#' @param h Integer representing the forecast horizon (in time steps) for the quantile regression. Default is 1.
#' @param edge Numeric value specifying the trimming amount applied to the outermost quantiles. Default is 0.05.
#' @param ellipsoids Optional list of matrices (ellips) representing stressed scenario, as returned by \code{get_ellipsoids()}. If provided, the function computes stressed quantiles and stressed factors.
#' @param min Logical. If \code{TRUE} (default), the function performs stepwise minimization. If \code{FALSE}, it performs stepwise maximization.
#' @param qtau Numeric. The quantile level (default is 0.05) used to compute stressed factors via \code{compute_stressed_factors()}. Only used if \code{ellipsoids} is provided.
#'
#' @return An object of class \code{fars}, which is a list containing:
#' \describe{
#'   \item{\code{quantiles}}{Matrix of forecasted quantiles (rows = periods, cols = quantile levels).}
#'   \item{\code{coeff}}{Matrix of quantile regression coefficients for each quantile.}
#'   \item{\code{std_error}}{Matrix of standard errors for each regression coefficient.}
#'   \item{\code{pvalue}}{Matrix of p-values for each regression coefficient.}
#'   \item{\code{levels}}{The list of estimated quantiles.}
#'   \item{\code{qtau}}{The quantile level used to compute stressed factors (if applicable).}
#'   \item{\code{stressed_quantiles}}{Matrix of quantiles under stressed scenarios (only if \code{ellipsoids} is provided).}
#'   \item{\code{stressed_factors}}{Matrix of selected stressed factors (only if \code{ellipsoids} is provided).}
#'   \item{call}{Function call.}
#' }
#' 
#' @examples
#' dep_variable <- rnorm(100)  # A numeric vector
#' data <- matrix(rnorm(100*300), nrow = 100, ncol = 300)
#' block_ind <- c(150, 300)  # Defines 2 blocks
#' global = 1
#' local <- c(1, 1)   
#' mldfm_result <- mldfm(data, blocks = 2, block_ind = block_ind, global = global , local = local)
#' fars_result <- compute_fars(dep_variable, get_factors(mldfm_result), h = 1, edge = 0.05)
#'  
#' @export
compute_fars <- function(dep_variable, 
                         factors, 
                         h = 1, 
                         edge = 0.05, 
                         ellipsoids = NULL, 
                         min = TRUE,
                         qtau = 0.05) {
 
  if (!is.numeric(dep_variable)) stop("dep_variable must be a numeric vector.")
  if (!is.matrix(factors) && !is.data.frame(factors)) stop("factors must be a matrix or data frame.")
  if (length(factors) == 0) stop("factors cannot be empty.")
  if (!is.numeric(h) || h < 1) stop("h must be a positive integer.")
  if (!is.numeric(edge) || edge < 0 || edge > 0.5) stop("edge must be a number between 0 and 0.5.")
  if (!is.null(ellipsoids) && !is.list(ellipsoids)) stop("ellipsoids must be a list of matrices, as returned by get_ellipsoids().")
  if (!is.logical(min)) stop("min must be a single logical value (TRUE or FALSE).")
  if (!is.null(qtau)) {
    if (!is.numeric(qtau) || length(qtau) != 1 || qtau <= 0 || qtau >= 1) {
      stop("qtau must be a single numeric value between 0 and 1 (exclusive).")
    }
  }
  
  
  
  # Prepare levels
  levels <- c(0.00, 0.25, 0.50, 0.75, 1)
  levels[1] <- levels[1]+edge # adjust left edge
  levels[5] <- levels[5]-edge # adjust right edge
  
  
  # Output structures
  quantiles <- matrix(nrow = length(dep_variable), ncol = length(levels))
  stressed_quantiles <- if (!is.null(ellipsoids)) matrix(nrow = length(dep_variable), ncol = length(levels)) else NULL
  coeff_df <- NULL
  pvalue_df <- NULL
  stderr_df <- NULL
  stressed_factors <- NULL
  
  message("Running Factor-Augmented Quantile Regressions (FA-QRs)...")
  
  
  if(!is.null(ellipsoids)){
    stressed_factors <- compute_stressed_factors(dep_variable,factors,ellipsoids,h,qtau,min)
  }
  
  
  # Loop through each quantile and compute Qreg
  for (i in seq_along(levels)) {
    q <- levels[i]

    q_reg_result <- q_reg(dep_variable, factors, stressed_factors, h, q)

    if (!is.null(ellipsoids)) {
      stressed_quantiles[, i] <- q_reg_result$stressed_pred_q
    }

    quantiles[, i] <- q_reg_result$pred_q
    coeff_df <- cbind(coeff_df, round(q_reg_result$coeff,4))
    pvalue_df  <- cbind(pvalue_df, round(q_reg_result$pvalue,4))
    stderr_df <- cbind(stderr_df, round(q_reg_result$stderr, 4))
    
  }

  colnames(coeff_df) <- paste0("q", levels)
  colnames(pvalue_df) <- paste0("q", levels)
  
  
  # Store result
  quantile_levels <- levels  # store adjusted quantiles (with edge)
  
  
  result <- list(
    quantiles = matrix(quantiles,ncol = length(levels)),
    coeff = coeff_df,
    std_error = stderr_df,
    pvalue = pvalue_df,
    levels = quantile_levels,
    call = match.call()  
  )
  
  if (!is.null(ellipsoids)) {
    result$qtau <- qtau
    result$stressed_factors <- matrix(stressed_factors, ncol = ncol(factors))
    result$stressed_quantiles <- matrix(stressed_quantiles, ncol = length(levels))
    
  }
  
  class(result) <- "fars"
  message("Completed")
  
  return(result)

}





