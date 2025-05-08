#' @title Estimate Multilevel Dynamic Factor Model 
#'
#' @description Estimates a multilevel dynamic factor model from time series data. 
#' Supports both single-block and hierarchical multi-block structures with customizable factor extraction settings.
#'
#' @param data A numeric matrix or data frame containing the time series data. Rows represent time points; columns represent observed variables.
#' @param blocks Integer. Number of blocks into which the data is divided.
#' @param block_ind Integer vector. End column indices for each block. Must be of length \code{blocks} and in increasing order.
#' @param r Integer vector of length \code{2^blocks - 1}. Specifies the number of factors for each node in the hierarchical structure.
#' @param method Integer.  Method used to initialize the factors: \code{0} for Canonical Correlation Analysis (CCA), \code{1} for Principal Component Analysis (PCA).
#' @param tol Numeric. The tolerance level for the residual sum of squares (RSS) minimization process. Used as a convergence criterion.
#' @param max_iter Integer. The maximum number of iterations allowed for the RSS minimization process.
#' @param verbose Logical. If \code{TRUE} (default), print a summary of the mldfm.
#'
#' @return An object of class \code{mldfm}, which is a list containing the following components:
#' \describe{
#'   \item{Factors}{Matrix of estimated factors.}
#'   \item{Lambda}{Matrix of factor loadings.}
#'   \item{Residuals}{Matrix of residuals.}
#'   \item{Iterations}{Number of iterations before convergence.}
#'   \item{Factors_list}{List of estimated factors for each node.}
#' }
#'
#' @examples
#' \donttest{
#' data <- matrix(rnorm(1000), nrow = 100, ncol = 519)
#' block_ind <- c(63, 311, 519)  # Defines 3 blocks
#' r <- c(1, 1, 1, 1, 1, 1, 1)   # 2^3 - 1 = 7 nodes
#' result <- mldfm(data, blocks = 3, block_ind = block_ind, r = r)
#' summary(result)
#'}
#'
#' @export
mldfm <- function(data, blocks = 1, block_ind = NULL, r = c(1), method = 0, tol = 1e-6, max_iter = 1000, verbose = TRUE) {
  
  # Argument checks
  if (!is.matrix(data) && !is.data.frame(data)) stop("data must be a matrix or data frame.")
  if (!is.numeric(blocks) || length(blocks) != 1) stop("blocks must be a single numeric value.")
  if (is.null(block_ind) || length(block_ind) != blocks) stop("block_ind must be provided and must have length equal to the number of blocks.")
  if (!is.numeric(r) || length(r) != (2^blocks - 1)) stop("r must be a numeric vector of length 2^blocks - 1.")
  if (!is.numeric(tol) || tol <= 0) stop("tol must be a positive numeric value.")
  if (!is.numeric(max_iter) || max_iter < 1) stop("max_iter must be a positive integer.")
  if (!method %in% c(0, 1)) stop("method must be 0 (CCA) or 1 (PCA).")
  
  
  data <- as.matrix(data)
  
  if (blocks == 1) {
    result <- single_block(data, r = r)
  } else if (blocks > 1) {
    result <- multiple_blocks(data, r = r, block_ind = block_ind, tol = tol, max_iter = max_iter, method = method)
  } else {
    stop("Invalid number of blocks.")
  }
  
  output <- list(
    Factors = result$Factors,
    Lambda = result$Lambda,
    Residuals = result$Residuals,
    Method = result$Method,
    Iterations = result$Iterations,
    Factors_list = result$Factors_list
  )
  
  class(output) <- "mldfm"
  if (verbose) {
    summary(output)
  }
  return(output)
  
}


