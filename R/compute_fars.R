#' Compute Factor Augmented Quantile Regressions and Stressed Quantiles
#'
#' Performs quantile regressions of a dependent variable on MLDFM-extracted factors.
#' Optionally generates quantile forecasts under stressed scenarios using hyperellipsoids.
#'
#' @param dep_variable A numeric vector representing the dependent variable (e.g., GDP growth, inflation).
#' @param factors A matrix of factor estimates from a \code{mldfm} model.
#' @param h Integer. Forecast horizon (in time steps) for the quantile regression. Default is \code{1}.
#' @param edge Numeric. Trimming amount applied to the outermost quantiles (default \code{0.05}).
#' @param scenario Optional list of matrices representing a stressed scenario, as returned by \code{create_scenario()}.
#' @param min Logical. If \code{TRUE} (default), implement a stepwise minimization. If \code{FALSE}, implement a stepwise maximization. 
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{Quantiles}}{Matrix of forecasted quantiles (rows = time, cols = quantile levels).}
#'   \item{\code{Scenario_Quantiles}}{Matrix of stressed scenario quantiles (same format), returned only if \code{scenario} is provided.}
#'   \item{\code{Coeff}}{Matrix of quantile regression coefficients for each quantile.}
#'   \item{\code{Std. Error}}{Matrix of Std. Error for each regression coefficient.}
#'   \item{\code{Pvalue}}{Matrix of p-values for each regression coefficient.}
#' }
#' 
#' @examples
#' \donttest{
#' dep_variable <- rnorm(100)  # A numeric vector
#' data <- matrix(rnorm(100*300), nrow = 100, ncol = 300)
#' block_ind <- c(150, 300)  # Defines 2 blocks
#' global = 1
#' local <- c(1, 1)   
#' mldfm_result <- mldfm(data, blocks = 2, block_ind = block_ind, global = global , local = local)
#' fars_result <- compute_fars(dep_variable, mldfm_result$Factors, h = 1, edge = 0.05, min = TRUE)
#' }
#'  
#'  
#' @export
compute_fars <- function(dep_variable, 
                         factors, 
                         h = 1, 
                         edge = 0.05, 
                         scenario = NULL, 
                         min = TRUE) {
 
  if (!is.numeric(dep_variable)) stop("dep_variable must be a numeric vector.")
  if (!is.matrix(factors) && !is.data.frame(factors)) stop("factors must be a matrix or data frame.")
  if (!is.numeric(h) || h < 1) stop("h must be a positive integer.")
  if (!is.numeric(edge) || edge < 0 || edge > 0.5) stop("edge must be a number between 0 and 0.5.")
  if (!is.null(scenario) && !is.list(scenario)) stop("scenario must be a list of matrices, as returned by create_scenario().")
  
  # Prepare levels
  levels <- c(0.00, 0.25, 0.50, 0.75, 1)
  levels[1] <- levels[1]+edge # adjust left edge
  levels[5] <- levels[5]-edge # adjust right edge
  
  # Output structures
  Quantiles <- matrix(nrow = length(dep_variable), ncol = length(levels))
  Scenario_Quantiles <- if (!is.null(scenario)) matrix(nrow = length(dep_variable), ncol = length(levels)) else NULL
  coeff_df <- NULL
  pvalue_df <- NULL
  stderr_df <- NULL
  
  message("Running Factor-Augmented Quantile Regressions (FARS)...")
  
  # Loop through each quantile and compute Qreg
  for (i in seq_along(levels)) {
    q <- levels[i]

    QReg_result <- q_reg(dep_variable, factors = factors, scenario = scenario, h=h, QTAU = q, min = min)

    if (!is.null(scenario)) {
      Scenario_Quantiles[, i] <- QReg_result$Scenario_Pred_q
    }

    Quantiles[, i] <- QReg_result$Pred_q
    coeff_df <- cbind(coeff_df, round(QReg_result$Coeff,4))
    pvalue_df  <- cbind(pvalue_df, round(QReg_result$Pvalue,4))
    stderr_df <- cbind(stderr_df, round(QReg_result$StdError, 4))
    
  }

  colnames(coeff_df) <- paste0("q", levels)
  colnames(pvalue_df) <- paste0("q", levels)
  
  
  # Store result
  quantile_levels <- levels  # store adjusted quantiles (with edge)
  
  
  result <- list(
    Quantiles = Quantiles,
    Coeff = coeff_df,
    StdError = stderr_df,
    Pvalue = pvalue_df,
    Levels = quantile_levels
  )
  
  if (!is.null(scenario)) {
    result$Scenario_Quantiles <- Scenario_Quantiles
  }
  
  class(result) <- "fars"
  print(result)
  return(result)

}





