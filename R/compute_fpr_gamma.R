#' @title Compute Adaptive Threshold Cross-Sectional Robust Gamma (FPR Gamma)
#' 
#' @description Computes the cross-sectional robust covariance estimator (gamma) based on the Adaptive Thresholding method proposed by Fresoli, Poncela, and Ruiz (2024).
#'
#' @param residuals A \code{T x N} matrix of residualsl.
#' @param loadings A \code{N x K} matrix of factor loadings.
#'
#' @return A \code{K x K} matrix representing the FPR-adjusted covariance of the latent factors.
#'
#' @keywords internal
#' 
compute_fpr_gamma <- function(residuals, loadings) {
  
  
  T <- nrow(residuals)
  N <- ncol(residuals)
  K <- ncol(loadings)
  
  # Sigma_eps
  Sigma_eps <- crossprod(residuals) / T
  
  # Theta
  E2 <- residuals^2
  Theta <- crossprod(E2) / T - Sigma_eps^2
  Theta[Theta < 0] <- 0
  
  # Adaptive threshold
  omega_NT <- 1 / sqrt(N) + sqrt(log10(N) / T)
  delta <- compute_optimal_delta(Sigma_eps, Theta, T)
  C <- delta * omega_NT * sqrt(Theta)
  
  # Thresholded covariance 
  keep <- abs(Sigma_eps) >= C
  S_thresh <- Sigma_eps
  S_thresh[!keep] <- 0
  
  # Gamma
  gamma <- crossprod(loadings, S_thresh %*% loadings) / N
  
  
  gamma
}
