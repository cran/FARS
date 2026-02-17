#' @title Apply Identification Constraints to Factors and Loadings
#' 
#' @description Applies identification constraints to factors and loadings.
#' 
#' @param factors A numeric matrix with the current estimate of all factors (T × total factors).
#' @param loadings A numeric matrix with the current estimate of all factor loadings (total factors x N).
#'
#' @return A list containing:
#' \describe{
#'   \item{factors}{A matrix containing identified factors.}
#'   \item{loadings}{A matrix of identified factor loadings.}
#' }
#'
#' @keywords internal
apply_identifications <- function(factors, loadings) {
  
  F <- as.matrix(factors)    # T x r
  L <- as.matrix(loadings)   # r x N
  
  Tn <- nrow(F)
  r  <- ncol(F)
  
  # Gorup factors using mask
  zero_mask <- abs(L) <=  0      # r x N logical
  key <- apply(zero_mask, 1, paste0, collapse = "")  # one key per factor (row)
  blocks <- unname(split(seq_len(r), key))           # indices of factors (1..r)
  
  # H block-diagonal
  H <- diag(r) # start with H = Ir
  
  # Loop over each block one by one
  for (idx in blocks) {
    rb <- length(idx)
    
    Fb <- F[, idx, drop = FALSE]      # T x rb
    Lb <- L[idx, , drop = FALSE]      # rb x N
    
    # Step A: whitening factors in-block: (Fb'Fb)/T = I
    SigmaF <- crossprod(Fb) / Tn      # rb x rb
    eigF <- eigen(SigmaF, symmetric = TRUE)
    
    if (any(eigF$values <= 0)) {
      stop(sprintf("SigmaF not PD in a block (size %d): collinearity/rank issue.", rb))
    }
    
    Hb1 <- eigF$vectors %*% diag(1 / sqrt(eigF$values), rb, rb) %*% t(eigF$vectors)
    
    # After whitening, transform loadings as Lb1 = Hb1^{-1} Lb
    Lb1 <- solve(Hb1, Lb)  # rb x N, solves Hb1 * X = Lb
    
    # Step B: rotate within-block to make (Lb1 Lb1') diagonal
    if (rb > 1) {
      Sb <- Lb1 %*% t(Lb1)                # rb x rb
      Vb <- eigen(Sb, symmetric = TRUE)$vectors
    } else {
      Vb <- matrix(1, 1, 1)
    }
    
    Hb <- Hb1 %*% Vb
    H[idx, idx] <- Hb
  }
  
  # Final rotation
  F_star <- F %*% H
  L_star <- solve(H, L)   # H^{-1} L
  
  list(
    factors = F_star,  # T x r
    loadings = L_star  # r x N
  )
}
