#' @title Compute Loadings 
#'
#' @description Computes the full loadings matrix for the Multi-Level Dynamic Factor Model.
#' 
#' @param data A numeric matrix or data frame containing the time series data (T × N).
#' @param num_blocks Integer. Number of blocks.
#' @param ranges A list of vectors with the column indices for each block.
#' @param r_list A named list specifying the number of factors to extract for each node (from \code{build_factor_structure}).
#' @param factor_list A named list of factors extracted at each node (from \code{compute_initial_factors}).
#' @param loadings_list A named list to which updated loadings will be appended for each node.
#'
#' @return A list with:
#' \describe{
#'   \item{loadings}{A numeric matrix (total factors × total variables) with estimated loadings.}
#'   \item{loadings_list}{An updated named list of loadings for each node.}
#' }
#'
#' @importFrom utils combn
#'
#' @keywords internal
compute_loadings <- function(data, num_blocks, ranges, r_list, factor_list, loadings_list) {
  

  # Initialize 
  num_factors <- sum(unlist(r_list))
  loadings_matrix <- matrix(0, nrow = num_factors, ncol =  ncol(data))
  
  counter <- 1
  
  # --- Step 1: Global level ---
  global_key <- paste(seq(1, num_blocks), collapse = "-")
  global_factors <- factor_list[[global_key]]
  
  global_loadings <- beta_ols(global_factors, data)
  loadings_list[[global_key]] <- global_loadings
  
  combination <- seq(1, num_blocks)
  loadings_matrix[counter:(counter + r_list[[global_key]] - 1), unlist(ranges[combination])] <- global_loadings
  counter <- counter + r_list[[global_key]]
  
  
  # --- Step 2: Lower levels ---
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
    
    # Step 2c: Regress residuals on current node factors to compute loadings
    factors <- factor_list[[key]]
    loadings <- beta_ols(factors, residuals)
    loadings_list[[key]] <- loadings
    
    loadings_matrix[counter:(counter + r_list[[key]] - 1), unlist(ranges[combination])] <- loadings
    counter <- counter + r_list[[key]]
    
    
  }
  
  return(list(
    loadings = loadings_matrix,
    loadings_list = loadings_list
  ))
}

