#' @title Multi-level Dynamic Factor Model - Multiple Blocks (MLDFM)
#'
#' @description Estimates a Multi-Level Dynamic Factor Model (MLDFM) using Sequential Least Squares Estimation approach
#' 
#' @param data A numeric matrix or data frame containing the time series data (T × N).
#' @param block_ind Integer vector. End column indices for each block. Must be of length \code{blocks} and in increasing order.
#' @param global Integer. Number of global factors extracted from the entire dataset.
#' @param local Integer vector of length \code{blocks}. Specifies the number of local factors for each block.
#' @param middle_layer Named list. Each name is a string specifying a group of blocks (e.g., \code{"1-3"} or \code{"2-3"}), and each value is the number of factors to extract.
#' @param method Integer.  Method used to initialize the factors: \code{0} for Canonical Correlation Analysis (CCA), \code{1} for Principal Component Analysis (PCA).
#' @param tol Numeric. The tolerance level for the residual sum of squares (RSS) minimization process. Used as a convergence criterion.
#' @param max_iter Integer. The maximum number of iterations allowed for the RSS minimization process.
#'
#' @return A list with elements:
#' \describe{
#'   \item{factors}{Matrix of estimated factors.}
#'   \item{loadings}{Matrix of factor loadings.}
#'   \item{residuals}{Matrix of residuals.}
#'   \item{fitted}{Matrix of fitted values.}
#'   \item{method}{Initialization method used (CCA or PCA).}
#'   \item{iterations}{Number of iterations before convergence.}
#'   \item{factors_list}{List of estimated factors for each node.}
#' }
#' 
#' @keywords internal
multiple_blocks<-function(data, global, local, middle_layer, block_ind, tol, max_iter, method){
 
  # Standardize the original data
  data <- scale(data,TRUE,TRUE)
 
  # Initialize 
  num_blocks <- length(block_ind) # Number of blocks
  num_obs <- nrow(data) # Total number of observations
  
  # Define block ranges and count the number of variables in each block
  ranges <- list()
  num_vars <- numeric(length(block_ind))  
 
  for (i in 1:length(block_ind)) {
    if (i == 1) {
      ranges[[i]] <- 1:block_ind[i]
    } else {
      ranges[[i]] <- (block_ind[i - 1] + 1):block_ind[i]
    }
    num_vars[i] <- length(ranges[[i]]) 
  }
  
  # Define factor list structure
  r_list <- build_factor_structure(global = global, 
                                   local = local, 
                                   middle_layer = middle_layer, 
                                   num_blocks = num_blocks)
  
  # --- Step 1: Initial factor extraction ---
  init_res <- compute_initial_factors(
    data = data,
    num_vars = num_vars,
    num_obs = num_obs,
    num_blocks = num_blocks,
    ranges = ranges,
    r_list = r_list,
    method = method
  )
  factor_list <- init_res$factor_list
  
  # --- Step 2: Iterative optimization ---
  RSS_previous <- Inf
  iteration <- 0
  loadings_list <- list()
  
  repeat {
    iteration <- iteration + 1
    if (iteration > max_iter) break
    
    # Update loadings
    loadings_res <- compute_loadings(data, num_blocks, ranges, r_list, factor_list, loadings_list)
    loadings <- loadings_res$loadings
    loadings_list <- loadings_res$loadings_list
    
    # Update factors
    LtL <- tcrossprod(loadings)                    # r x r
    LDt <- loadings %*% t(data)                    # r x T
    final_factors <- t(qr.solve(LtL, LDt))         # T x r
    
    # Update factor list
    factor_list <- update_factor_list(factor_list, final_factors, r_list)
    
    # Compute RSS and check convergence
    residuals <- data - final_factors %*% loadings
    RSS_new <- Re(sum(residuals^2))
    
  
    if ((log(RSS_previous) - log(RSS_new)) < tol) break  # Converged
    RSS_previous <- RSS_new

  }
  
  # --- Step 3: Identification ---
  id_res <- apply_identifications(data, num_blocks, ranges, r_list, final_factors, factor_list, loadings_list)
  orthogonal_factors <- id_res$final_factors
  factor_list <- id_res$factor_list
  loadings <- id_res$loadings
  
  # Fitted
  fitted <- orthogonal_factors %*% loadings
  
  # Final residuals
  residuals <- data - fitted
  
  # Drop column names
  orthogonal_factors <- unname(orthogonal_factors)
  
  # Return results
  return(list(
    factors = orthogonal_factors,
    loadings = t(loadings),
    residuals = residuals,
    fitted = fitted,
    method = if (method == 0) "CCA" else "PCA",
    iterations = iteration,
    factors_list = r_list
  ))
}


