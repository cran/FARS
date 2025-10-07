#' @title Compute Optimal Delta for FPR Gamma Computation
#'
#' @description Computes the optimal threshold level delta.
#'
#' @return A single numeric value representing the optimal threshold level \code{delta}.
#' 
#' @importFrom stats pnorm
#' @keywords internal
#'
compute_optimal_delta <- function(Sigma_eps, Theta, T) {
  N <- nrow(Sigma_eps)
  
  # Standardize the covariances
  z_mat <- abs(Sigma_eps) / sqrt(Theta)
  
  # Compute a0 and a1 
  a0 <- 1 / sqrt(log10(log10(N)))
  a1 <- 2 - min(sqrt(2 + log10(T / N)), 2)
  
  # Range for moderate standardized covariances
  z_range_min <- a1 + a0
  z_range_max <- 2
  
  # Compute M_hat
  upper_tri_indices <- which(upper.tri(z_mat), arr.ind = TRUE)
  z_vals <- z_mat[upper_tri_indices]
  M_hat <- sum(z_vals > z_range_min & z_vals < z_range_max)
  
  # Compute q and V
  q <- N * (N - 1) / 2
  logN <- log10(N)
  Phi <- pnorm
  
  V <- 2 * q * (Phi(2 * sqrt(logN)) - Phi((a1 + a0) * sqrt(logN)))
  
  # Estimate N2
  N2_hat <- max(M_hat - V, sqrt(logN))
  
  # Compute delta_0 
  delta <- sqrt(2 * (2 - log10(N2_hat * logN^(-0.5)) / logN))
  
  return(delta)
}
