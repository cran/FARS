#' @title Multi-Level Dynamic Factor Model  - Single Block (DFM)
#'
#' @description Estimates a Dynamic Factor Model (DFM) when only one block is present, using Principal Component Analysis (PCA).
#' 
#' @param data A numeric matrix or data frame containing the time series data (T × N).
#' @param r Integer. Number of factors to extract.
#' 
#' @return A list with elements:
#' \describe{
#'   \item{factors}{Matrix of estimated factors.}
#'   \item{loadings}{Matrix of factor loadings.}
#'   \item{residuals}{Matrix of residuals.}
#'   \item{fitted}{Matrix of fitted values.}
#'   \item{method}{Method used for factor extraction ("PCA").}
#'   \item{iterations}{Number of iterations before convergence (always 0 for single block).}
#'   \item{factors_list}{List of estimated factors for each node.}
#' }
#' 
#' 
#' @importFrom stats prcomp
#'
#' @keywords internal
single_block <- function(data, r) {
  
  # Standardize the data 
  X <- scale(data, center = TRUE, scale = TRUE)
  
  # Dimensions
  T <- nrow(X)
  N <- ncol(X)
  
  # PCA
  pca_res <- prcomp(X, center = FALSE, scale. = FALSE)
  factors_tmp <- pca_res$x[, 1:r, drop = FALSE]             # T × r
  loadings_tmp <- t(pca_res$rotation[, 1:r, drop = FALSE])    # r × N

  # Identifications
  common_component <- factors_tmp %*% loadings_tmp
  pca_result <- prcomp(common_component, center = FALSE, scale. = FALSE)
  factors <- pca_result$x[, 1:r, drop = FALSE]
  factors <- scale(factors, center = TRUE, scale = TRUE)
  loadings <- qr.solve(factors, common_component)

  # Matrix Format
  factors <- unname(as.matrix(factors))
  attr(factors, "scaled:center") <- NULL
  attr(factors, "scaled:scale") <- NULL
  loadings <- unname(as.matrix(loadings))

  # Fitted 
  fitted <- factors %*% loadings
  
  # Residuals 
  residuals <- X - fitted
  
  # Factor structure 
  factors_list <- list()
  factors_list[["1"]] <- r  
  
  iteration <- 0
  
  return(list(
    factors = factors,
    loadings = t(loadings), # N × r
    residuals = residuals,
    fitted = fitted,
    method = "PCA",
    iterations = iteration,
    factors_list = factors_list
  ))
}



