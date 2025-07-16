#' Extract Conditional Quantile from Simulated Densities
#'
#' Computes the conditional quantile (e.g., 5th percentile) from a simulated skew-t distribution,
#' generated via \code{density()} or \code{nl_density()}. The result corresponds to the
#' risk measure (e.g., Growth-at-Risk, Growth-in-Stress etc.).
#'
#' @param density An object of class \code{fars_density} returned by \code{density()} or \code{nl_density()}.
#' @param QTAU A numeric value between 0 and 1 indicating the quantile to extract (e.g., 0.05 for 5th percentile).
#'
#' @return A numeric vector of conditional quantiles (one observation for each time period).
#'
#' @examples
#' \donttest{
#' Quantiles <- matrix(rnorm(500), ncol = 5)
#' fars_density <- compute_density(Quantiles, seed = 42)
#' GaR <- quantile_risk(fars_density, QTAU = 0.05)
#' }
#'
#' @importFrom stats quantile
#'
#' @export
quantile_risk <- function(density, QTAU = 0.05) {
  if (!inherits(density, "fars_density")) stop("'density' must be of class 'fars_density'.")
  if (!is.numeric(QTAU) || QTAU <= 0 || QTAU >= 1) stop("'QTAU' must be a numeric value between 0 and 1.")
  
  apply(density$distribution, 1, quantile, probs = QTAU)
}

