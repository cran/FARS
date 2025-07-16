#' Compute Adaptive Threshold Cross-Sectional Robust Gamma (AT-CSR) 
#' Fresoli, Poncela and Ruiz (2024)
#'
#'
#' @keywords internal
#' 
compute_gamma_FPR <- function(Residuals, Loadings) {
  
  
  T <- nrow(Residuals)
  N <- ncol(Residuals)
  K <- ncol(Loadings)
  
  # Compute residual covariance matrix sigma_ij
  Sigma_eps <- (1 / T) * t(Residuals) %*% Residuals  # N x N
  
  # Compute variance of residual product theta_ij
  Theta <- matrix(0, N, N)
  for (i in 1:N) {
    for (j in 1:N) {
      e_prod <- Residuals[, i] * Residuals[, j]
      sigma_ij <- Sigma_eps[i, j]
      Theta[i, j] <- mean((e_prod - sigma_ij)^2)
    }
  }
  
  # Compute adaptive threshold c_ij
  omega_NT <- 1 / sqrt(N) + sqrt(log10(N) / T)
  
  #gamma <- 1  # moderate sparsity
  #delta <- sqrt(2 * (2 - gamma))
  delta <- compute_optimal_delta(Sigma_eps, Theta, T) 
    
  
  C <- delta * omega_NT * sqrt(Theta)
  
  # Compute AT-CSR Gamma
  Gamma <- matrix(0, K, K)
  for (i in 1:N) {
    for (j in 1:N) {
      if (abs(Sigma_eps[i, j]) >= C[i, j]) {
        Gamma <- Gamma + Sigma_eps[i, j] * (t(Loadings[i, , drop = FALSE]) %*% Loadings[j, , drop = FALSE])
      }
    }
  }
  
  Gamma <- Gamma / N
  return(Gamma)
}
