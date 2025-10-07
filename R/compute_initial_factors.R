#' @title Compute Initial Factors for Multi-Level Dynamic Factor Model
#' 
#' @description Computes the initial set of global, middle-layer, and local factors using either PCA or CCA.
#' 
#' @param data A numeric matrix or data frame containing the time series data (T × N).
#' @param num_vars Integer vector. Number of variables in each block.
#' @param num_obs Integer. Number of time observations (T).
#' @param num_blocks Integer. Number of blocks.
#' @param ranges A list of vectors with the column indices for each block.
#' @param r_list A named list specifying the number of factors to extract for each node (from \code{build_factor_structure}).
#' @param method Integer. Method for factor extraction: \code{0} = Canonical Correlation Analysis (CCA), \code{1} = Principal Component Analysis (PCA).
#'
#'@return A list with two elements:
#' \describe{
#'   \item{initial_factors}{Matrix of all initial factors (T × total number of factors).}
#'   \item{factor_list}{Named list of factors for each node in the hierarchy.}
#' }
#' 
#' @importFrom utils combn
#' @importFrom stats prcomp
#'
#' @keywords internal
compute_initial_factors <- function(data, num_vars, num_obs, num_blocks, ranges, r_list, method) {
  
  # Initialize
  factor_list <- list()
  initial_factors <- matrix(nrow = num_obs, ncol = 0)
  
  
  # --- Step 1: Global factors ---
  global_key <- paste(seq(1, num_blocks), collapse = "-")  
  number_of_factor <- r_list[[global_key]]
  
  
  if (method == 0){
    # Canonical Correlation Analysis
    global_factors <- canonical_correlation_analysis(data, num_vars, number_of_factor, rep(1, num_blocks))
  }else{
    # Principal Component Analysis
    pca_result <- prcomp(data, scale. = FALSE)
    global_factors <- pca_result$x[, 1:number_of_factor]
  }
  
  global_factors<- scale(global_factors,TRUE,TRUE)
  
  # Store global factors
  factor_list[[global_key]] <- global_factors  
  initial_factors <- cbind(initial_factors, global_factors)
    
  
  # --- Step 2: Lower level factors ---
  for (key in names(r_list)) {
    if (key == global_key) next
    
    combination <- as.numeric(unlist(strsplit(key, "-")))
    
    # Step 2a: Initialize residuals using blocks data
    residuals <- do.call(cbind, lapply(combination, function(idx) data[, ranges[[idx]]]))

    # Step 2b: Remove higher-level factors (orthogonal projection)
    level <- num_blocks
    while (level > length(combination)) {
      upper_factors <- get_level_factors(factor_list, combination, level)
      if(!is.null(upper_factors)){
        ols_result <- beta_ols(upper_factors, residuals)
        residuals <- residuals - upper_factors %*% ols_result
      }
      level <- level - 1
    }

    # Step 2c: Compute new factors 
    number_of_factor <- r_list[[key]]
    n_sep <- stringr::str_count(key, "-")
    
    if (n_sep>0 && method == 0) {
      # Use CCA for intermediate levels
      factors <- canonical_correlation_analysis(residuals, num_vars[combination], number_of_factor, rep(1, num_blocks))
    }else{
      # Use PCA
      pca_result <- prcomp(residuals, scale. = FALSE)
      factors <- pca_result$x[, 1:number_of_factor]
      
    }
    
    factors<- scale(factors,TRUE,TRUE)
    
    # Step 2d: Store factors
    factor_list[[key]] <- factors  
    initial_factors <- cbind(initial_factors, factors)
  }


  return(list(
    initial_factors = initial_factors,
    factor_list = factor_list
  ))
}

