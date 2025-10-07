#' @title Multi-Level Dynamic Factor Model (MLDFM)
#'
#' @description Estimates a Multi-Level Dynamic Factor Modelfrom time series data. 
#' Supports both Single-bBlock (DFM) and hierarchical Multi-Block (MLDFM) structures with customizable factor extraction settings.
#'
#' @param data A numeric matrix or data frame containing the time series data. Rows represent time points; columns represent observed variables.
#' @param blocks Integer. Number of blocks into which the data is divided.
#' @param block_ind Integer vector. End column indices for each block. Must be of length \code{blocks} and in increasing order.
#' @param global Integer. Number of global factors extracted from the entire dataset.
#' @param local Integer vector of length \code{blocks}. Specifies the number of local factors for each block.
#' @param middle_layer Named list. Each name is a string specifying a group of blocks (e.g., \code{"1-3"} or \code{"2-3"}), and each value is the number of factors to extract.
#' @param method Integer.  Method used to initialize the factors: \code{0} for Canonical Correlation Analysis (CCA), \code{1} for Principal Component Analysis (PCA).
#' @param tol Numeric. The tolerance level for the residual sum of squares (RSS) minimization process. Used as a convergence criterion.
#' @param max_iter Integer. The maximum number of iterations allowed for the RSS minimization process.
#' @param verbose Logical. If \code{TRUE} (default), print a summary of the mldfm.
#'
#' @return An object of class \code{mldfm}, which is a list containing:
#' \describe{
#'   \item{factors}{Matrix of estimated factors.}
#'   \item{loadings}{Matrix of factor loadings.}
#'   \item{residuals}{Matrix of residuals.}
#'   \item{method}{Initialization method used (CCA or PCA).}
#'   \item{iterations}{Number of iterations before convergence.}
#'   \item{factors_list}{List of estimated factors for each node.}
#'   \item{call}{Function call.}
#' }
#'
#' @examples
#' mldfm_result <- mldfm(data = matrix(rnorm(100 * 5), 100, 5), blocks = 1, global = 2)
#'
#' @export
#' 
#' 
#' 
mldfm <- function(data, blocks = 1, block_ind = NULL, global = 1, local = NULL, middle_layer = NULL, method = 0, tol = 1e-6, max_iter = 1000, verbose = TRUE) {
  
  # Argument checks
  if (!is.matrix(data) && !is.data.frame(data)) stop("data must be a matrix or data frame.")
  if (!is.numeric(blocks) || length(blocks) != 1) stop("blocks must be a single numeric value.")
  if (blocks > 1 && (is.null(block_ind) || length(block_ind) != blocks)) {
    stop("block_ind must be provided and must have length equal to the number of blocks (when blocks > 1).")
  }
  if (!is.numeric(global) || length(global) != 1 || global < 1) stop("global must be a single numeric value greater than zero")
  if (!is.null(local)) {
    if (!is.numeric(local) || length(local) != blocks) {
      stop("local must be a numeric vector of length equal to the number of blocks, or NULL.")
    }
  }
  if (!is.numeric(tol) || tol <= 0) stop("tol must be a positive numeric value.")
  if (!is.numeric(max_iter) || max_iter < 1) stop("max_iter must be a positive integer.")
  if (!method %in% c(0, 1)) stop("method must be 0 (CCA) or 1 (PCA).")
  
  
  data <- as.matrix(data)
  
  if (blocks == 1) {
    result <- single_block(data, r = global)
  } else if (blocks > 1) {
    result <- multiple_blocks(data, global = global, local = local, middle_layer = middle_layer , block_ind = block_ind, tol = tol, max_iter = max_iter, method = method)
  } else {
    stop("Invalid number of blocks.")
  }
  
  structure(
    list(
      factors      = result$factors,
      loadings     = result$loadings,
      residuals    = result$residuals,
      method       = result$method,
      iterations   = result$iterations,
      factors_list = result$factors_list,
      call         = match.call()
    ),
    class = "mldfm"
  )
  
}


