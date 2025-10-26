#' @title Canonical Correlation Analysis for MLDFM
#'
#' @description Performs Canonical Correlation Analysis (CCA) 
#'
#' @param y A numeric matrix containing all variables (T × N).
#' @param Nregio Integer vector specifying the number of variables in each block.
#' @param r_glob Integer. Number of global factors to be extracted.
#' @param r_reg Integer vector. Number of regional (local) factors to be extracted for each block.
#'
#' @return A numeric matrix of estimated factors (T × \code{r_glob}).
#'
#' @keywords internal
canonical_correlation_analysis <- function(y, Nregio, r_glob, r_reg) {
  
  g <- length(Nregio)                     # number of nodes
  RegInd <- c(0, cumsum(Nregio))          # cumulative sum of block size to determine start/end indices
  r <- r_glob + r_reg                     # total number of factors in each node (global + local) 
  rsum <- cumsum(r)                       # cumulative number of factors
  
  # extract regional factors
  fi <- NULL
  
  for (i in 1:g) {
    y_block <- y[, (RegInd[i] + 1):RegInd[i + 1]]
    # compute eigenvectors using crossprod for efficiency
    evec <- eigen(crossprod(y_block), symmetric = TRUE)$vectors
    # concatenate regional factors
    fi <- cbind(fi, y_block %*% evec[, 1:r[i]])
  }
  
  # scale factors by their SD
  fi <- fi / kronecker(matrix(1, nrow = nrow(fi), ncol = 1),
                       t(matrix(sqrt(diag(crossprod(fi))))))
  
  # extract global factors
  fhat <- NULL
  
  for (i in 1:(g - 1)) {
    # compute cross-region local factors covariance
    C <- crossprod(fi[, 1:rsum[1]], fi[, (rsum[i] + 1):rsum[i + 1]])
    # perform eigen-decomposition
    evec <- eigen(crossprod(C), symmetric = TRUE)$vectors
    # extract r_glob principal components
    fhat <- cbind(fhat, fi[, (rsum[i] + 1):rsum[i + 1]] %*% evec[, 1:r_glob])
  }
  
  # compute covariance matrix of fhat factors
  C <- crossprod(fhat)
  # perform PCA
  evec <- eigen(C, symmetric = TRUE)$vectors
  
  # select final global factors
  if ((g - 1) > 1) {
    fhatblock <- fhat %*% evec[, 1:r_glob]
  } else {
    fhatblock <- fhat %*% evec
  }
  
  return(fhatblock)
}
