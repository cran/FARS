#' Multi-Level Dynamic Factor Model - Multiple blocks
#'
#' @keywords internal


# Internal helper function to compute beta OLS
beta_ols <- function(X, Y) {
  #solve(t(X) %*% X) %*% t(X) %*% Y
  inv_X <- ginv(t(X) %*% X)
  inv_X %*% t(X) %*% Y
}

# Internal function: Multiple-blocks MLDFM computation
multiple_blocks<-function(Yorig, global, local, middle_layer, block_ind, tol, max_iter, method){

 
  # Standardize the original data
  Yorig <- scale(Yorig,TRUE,TRUE)
 
  # Initialize 
  results <- list()  # List to save results
  num_blocks <- length(block_ind) # Number of blocks
  num_obs <- nrow(Yorig) # Total number of observations

  
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
  r_list <- build_factor_structure(global = global, local = local, middle_layer = middle_layer, num_blocks)
  
  
  
  
  # --- STEP 1: INITIAL FACTORS ---
  init_res <- compute_initial_factors(
    Yorig, num_vars, num_obs, num_blocks,
    ranges, r_list, method
  )
  InitialFactors <- init_res$InitialFactors
  Factor_list <- init_res$Factor_list
  
  
  
  
  # --- STEP 2: ITERATIVE OPTIMIZATION ---
  RSS_previous <- Inf
  iteration <- 0
  Loadings_list <- list()
  
  repeat {
    iteration <- iteration + 1
    if (iteration > max_iter) break
    
    # Compute/Update Lambda
    L_res <- compute_lambda(Yorig, num_blocks, ranges, r_list, Factor_list, Loadings_list)
    
    Lambda <- L_res$Lambda
    Loadings_list <- L_res$Loadings_list
    
    # Update factors
    FinalFactors <- t(solve(Lambda %*% t(Lambda)) %*% Lambda %*% t(Yorig))
    # inv_LtL <- ginv(Lambda %*% t(Lambda))
    # FinalFactors <- t(inv_LtL %*% Lambda %*% t(Yorig))
    
    # Update factor list
    Factor_list <- update_factor_list(Factor_list, FinalFactors, r_list)
    
    # Compute RSS and check convergence
    FinalResiduals <- Yorig - FinalFactors %*% Lambda
    RSS_new <- sum(FinalResiduals^2)
    
    
    RSS_new <- Re(RSS_new)
    RSS_previous <- Re(RSS_previous)
    
    
    if ((log(RSS_previous) - log(RSS_new)) < tol) break  # Converged
    
    RSS_previous <- RSS_new

  }
  
  
  # --- STEP 3: IDENTIFICATION ---
  Id_res <- apply_identifications(
    Yorig, num_blocks, ranges, r_list,
    FinalFactors, Factor_list, Loadings_list
  )
  orthogonal_FinalFactors <- Id_res$FinalFactors
  Factor_list <- Id_res$Factor_list
  Lambda <- Id_res$Lambda

  
  # Final residuals
  Residuals <- Yorig - orthogonal_FinalFactors %*% Lambda
  
  # Drop column names
  orthogonal_FinalFactors <- unname(orthogonal_FinalFactors)
  
  # Collect results
  results$Factors <- orthogonal_FinalFactors
  results$Lambda <- t(Lambda)
  results$Residuals <- Residuals
  if(method == 0){
    results$Method <- "CCA"
  }else{
    results$Method <- "PCA"
  }
  results$Iterations <- iteration
  results$Factors_list <- r_list
  
 
  return(results)
}


