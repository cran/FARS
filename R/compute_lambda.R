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
compute_lambda <- function(Yorig, num_blocks, ranges, r_list, Factor_list, Loadings_list) {
  

  # Initialize 
  num_factors <- sum(unlist(r_list))
  Lambda <- matrix(0, nrow = num_factors, ncol =  ncol(Yorig))
  
  counter <- 1
  
  # --- Step 1: GLOBAL LEVEL ---
  global_key <- paste(seq(1, num_blocks), collapse = "-")
  GlobalFactors <- Factor_list[[global_key]]
  
  GlobalLoadings <- beta_ols(GlobalFactors, Yorig)
  Loadings_list[[global_key]] <- GlobalLoadings
  
  combination <- seq(1, num_blocks)
  Lambda[counter:(counter+r_list[[global_key]]-1), unlist(ranges[combination])] <- GlobalLoadings
  counter <- counter + r_list[[global_key]]
  
  
  # --- Step 2: LOWER LEVELS ---
  for (key in names(r_list)) {
    if (key == global_key) next
    
    
    combination <- as.numeric(unlist(strsplit(key, "-")))
    
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
    Factors <- Factor_list[[key]]
    
    Loadings <- beta_ols(Factors, Residuals)
    Loadings_list[[key]] <- Loadings
    
    Lambda[counter:(counter+r_list[[key]]-1), unlist(ranges[combination])] <- Loadings
    counter <- counter + +r_list[[key]]
    
    
  }
  
  return(list(
    Lambda = Lambda,
    Loadings_list = Loadings_list
  ))
}

