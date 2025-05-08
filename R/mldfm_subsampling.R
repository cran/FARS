#' Subsampling Procedure for MLDFM Estimation
#'
#' Repeatedly applies the MLDFM estimation to randomly drawn subsamples of the input data.
#'
#' @param data A numeric matrix or data frame containing the time series data. Rows represent time points; columns represent observed variables.
#' @param blocks Integer. The number of blocks into which the data is divided.
#' @param block_ind A vector of integers indicating the end index of each block. Must be of length \code{blocks} and in increasing order. Required if \code{blocks > 1}.
#' @param r A vector of integers specifying the number of factors to extract for each node in the block hierarchy. Its length must equal \code{2^blocks - 1}, corresponding to all nodes in the hierarchical tree.
#' @param method Integer. The method used to initialize the factors: \code{0} for Canonical Correlation Analysis (CCA), \code{1} for Principal Component Analysis (PCA).
#' @param tol Numeric. The tolerance level for the residual sum of squares (RSS) minimization process. Used as a convergence criterion.
#' @param max_iter Integer. The maximum number of iterations allowed for the RSS minimization process.
#' @param n_samples Number of subsamples to generate.
#' @param sample_size Proportion of the original sample to retain (e.g., 0.9 for 90%).
#' @param seed Optional integer. Seed for reproducibility of the subsampling process. If \code{NULL}, random draws will differ each run.
#'
#' @return A list of \code{mldfm} objects, one for each subsample.
#' 
#' @examples
#' \donttest{
#' data <- matrix(rnorm(1000), nrow = 100, ncol = 100)
#' block_ind <- c(50,100)  # Defines 3 blocks
#' r <- c(1, 1, 1)   # 2^2 - 1 = 3 nodes
#' result <- mldfm_subsampling(data, blocks = 2, block_ind = block_ind, r = r, 
#' n_samples = 100, sample_size = 0.9)
#' }
#' 
#' @export
mldfm_subsampling <- function(data, blocks = 1, block_ind = NULL, r = c(1), 
                              method = 0, tol = 1e-6, max_iter = 1000, 
                              n_samples = 10, sample_size = 0.9, seed = NULL) {
  
  
  
  # Argument checks
  if (!is.matrix(data) && !is.data.frame(data)) stop("data must be a matrix or data frame.")
  if (!is.numeric(blocks) || length(blocks) != 1) stop("blocks must be a single numeric value.")
  if (is.null(block_ind) || length(block_ind) != blocks) stop("block_ind must be provided and must have length equal to the number of blocks.")
  if (!is.numeric(r) || length(r) != (2^blocks - 1)) stop("r must be a numeric vector of length 2^blocks - 1.")
  if (!is.numeric(tol) || tol <= 0) stop("tol must be a positive numeric value.")
  if (!is.numeric(max_iter) || max_iter < 1) stop("max_iter must be a positive integer.")
  if (!method %in% c(0, 1)) stop("method must be 0 (CCA) or 1 (PCA).")
  if (!is.numeric(n_samples) || n_samples < 1) stop("n_samples must be a positive integer.")
  if (!is.numeric(sample_size) || sample_size <= 0 || sample_size > 1) stop("sample_size must be a number in (0, 1].")
  
  
  n_obs <- nrow(data)
  result <- vector("list", n_samples)
  
  message(paste0("Generating ", n_samples, " subsamples..."))
  
  for (i in 1:n_samples) {
    
    # Compute subsample 
    sub_sample_result <- compute_subsample(data, 
                                   block_ind = block_ind, 
                                   n = blocks,
                                   r = r,
                                   sample_size,
                                   seed = if (!is.null(seed)) seed + i - 1 else NULL )
    
    # Call MLDFM on subsample
    mldfm_result <- mldfm(sub_sample_result$sample_data, 
                          blocks = blocks, 
                          block_ind = sub_sample_result$sample_block_ind,
                          r = r, 
                          method = method, 
                          tol = tol, 
                          max_iter = max_iter,
                          verbose = FALSE)
    
    # Store results
    result[[i]] <- mldfm_result
  }
  
  # Final recap message
  message("\nSubsampling completed.")
  message(paste("Number of subsamples generated:", n_samples))
  
  return(result)
}
  
