#' Compute Loadings (Lambda Matrix)
#'
#' Computes the full loadings matrix (Lambda) for the multilevel dynamic factor model,
#' including global and lower-level block-specific loadings, by regressing observed data
#' on previously extracted factors.
#'
#'
#' @importFrom utils combn
#'
#'
#' @keywords internal
compute_lambda <- function(Yorig, num_blocks, ranges, num_factors, r, Factor_list,Loadings_list) {
  
  # Initialize 
  Lambda <- matrix(0, nrow = num_factors, ncol =  ncol(Yorig))
  
  r_index <- 1
  counter <- 1
  
  # --- Step 1: GLOBAL LEVEL ---
  key <- paste(seq(1, num_blocks), collapse = "-")
  GlobalFactors <- Factor_list[[key]]
  
  GlobalLoadings <- beta_ols(GlobalFactors, Yorig)
  Loadings_list[[key]] <- GlobalLoadings
  
  combination <- seq(1, num_blocks)
  Lambda[counter:(counter+r[r_index]-1), unlist(ranges[combination])] <- GlobalLoadings
  counter <- counter + r[r_index]
  
  
  # --- Step 2: LOWER LEVELS ---
  for (i in 1:(num_blocks-1)) {
    k <-  num_blocks - i
    combinations_matrix <- t(combn(num_blocks,k))
    
    for (j in 1:nrow(combinations_matrix)) {
      combination <- combinations_matrix[j,]
      
      r_index <- r_index + 1
      
      if (r[r_index] == 0) next  # Skip if no factors in the node
      
      # Step 2a: Initialize residuals using blocks data
      Residuals <- do.call(cbind, lapply(combination, function(idx) Yorig[, ranges[[idx]]]))
      
      
      # Step 2b: Remove higher-level factors (orthogonal projection)
      level <- num_blocks
      while (level > length(combination)) {
        Factors <- get_factors(Factor_list, combination, level)
        if(!is.null(Factors)){
          ols_result <- beta_ols(Factors, Residuals)
          Residuals <- Residuals - Factors %*% ols_result
        }
        level <- level - 1
      }
      
      # Step 2c: Regress residuals on current node factors to compute loadings
      key <- paste(combination, collapse = "-")
      Factors <- Factor_list[[key]]
      
      Loadings <- beta_ols(Factors, Residuals)
      Loadings_list[[key]] <- Loadings
      
      Lambda[counter:(counter+r[r_index]-1), unlist(ranges[combination])] <- Loadings
      counter <- counter + r[r_index]
      
    }
  }
  
  return(list(
    Lambda = Lambda,
    Loadings_list = Loadings_list
  ))
}

