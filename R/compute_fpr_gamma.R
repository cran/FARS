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
  
  # Compute residual covariance matrix sigma_ij
  Sigma_eps <- (1 / T) * t(residuals) %*% residuals  # N x N
  
  # Compute variance of residual product theta_ij
  Theta <- matrix(0, N, N)
  for (i in 1:N) {
    for (j in 1:N) {
      e_prod <- residuals[, i] * residuals[, j]
      sigma_ij <- Sigma_eps[i, j]
      Theta[i, j] <- mean((e_prod - sigma_ij)^2)
    }
  }
  
  # Compute adaptive threshold c_ij
  omega_NT <- 1 / sqrt(N) + sqrt(log10(N) / T)
  delta <- compute_optimal_delta(Sigma_eps, Theta, T) 
    
  
  C <- delta * omega_NT * sqrt(Theta)
  
  # Compute AT-CSR gamma
  gamma <- matrix(0, K, K)
  for (i in 1:N) {
    for (j in 1:N) {
      if (abs(Sigma_eps[i, j]) >= C[i, j]) {
        gamma <- gamma + Sigma_eps[i, j] * (t(loadings[i, , drop = FALSE]) %*% loadings[j, , drop = FALSE])
      }
    }
  }
  
  gamma <- gamma / N
  return(gamma)
}
