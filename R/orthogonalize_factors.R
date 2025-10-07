
#' @title Orthogonalize Factors
#' @keywords internal
#' 
#'@importFrom stats lm
#' 
orthogonalize_factors <- function(X) {
  n_factors <- ncol(X)  
  
  X <- Re(X)
  
  # Iterative orthogonalization
  for (i in 1:n_factors) {
    # Regress the i-th factor on all other factors (excluding itself)
    other_factors <- X[, -i, drop = FALSE]  
    model <- lm(X[, i] ~ other_factors)
    X[, i] <- residuals(model)
  }
  
  return(X)
}
