#' @title Extract Conditional Quantile from \code{fars_density} Object
#'
#' @description Computes the conditional quantile (e.g., 5th percentile) from a simulated skew-t distribution,
#' The result corresponds to the risk measure (e.g., Growth-at-Risk, Growth-in-Stress etc.).
#'
#' @param density An object of class \code{fars_density}, which is returned by \code{compute_density()} 
#' @param qtau A numeric value between 0 and 1 indicating the quantile to extract (e.g., 0.05 for the 5th percentile). Default is 0.05.
#'
#' @return A numeric vector of conditional quantiles (one observation for each time period).
#'
#' @examples
#' \donttest{
#' quantiles <- matrix(rnorm(500), ncol = 5)
#' fars_density <- compute_density(quantiles, seed = 42)
#' GaR <- quantile_risk(fars_density, qtau = 0.05)
#' }
#'
#' @importFrom stats quantile
#'
#' @export
quantile_risk <- function(density, qtau = 0.05) {
 
  if (!inherits(density, "fars_density")) stop("'density' must be of class 'fars_density'.")
  if (!is.numeric(qtau) || qtau <= 0 || qtau >= 1) stop("'qtau' must be a numeric value between 0 and 1.")
  
  apply(get_distribution(density), 1, quantile, probs = qtau)
}

