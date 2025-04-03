#' Compute Initial Factors (Multilevel Dynamic Factor Model)
#'
#' @importFrom utils combn
#' @importFrom stats prcomp
#'
#' @keywords internal
compute_initial_factors <- function(Yorig, num_vars, num_obs, num_blocks, ranges, num_factors, r, method) {
  
  # Initialize
  Factor_list <- list()
  InitialFactors <- matrix(nrow = num_obs, ncol = 0)  
  
  
  # --- STEP 1: GLOBAL FACTORS ---
  r_index <- 1 
  number_of_factor <- r[r_index] 
  
  if (method == 0){
    # Canonical Correlation Analysis
    GlobalFactors <- canonical_correlation_analysis(Yorig, num_vars, number_of_factor, rep(1, num_blocks))
  }else{
    # Principal Component Analysis
    pca_result <- prcomp(Yorig, scale. = FALSE)
    GlobalFactors <- pca_result$x[, 1:number_of_factor]
  }
  
  GlobalFactors<- scale(GlobalFactors,TRUE,TRUE)
  
  # Store global factors
  key <- paste(seq(1, num_blocks), collapse = "-")  
  Factor_list[[key]] <- GlobalFactors  
  InitialFactors <- cbind(InitialFactors, GlobalFactors)
    
  
  # --- STEP 2: LOWER-LEVEL FACTORS ---
  for (i in 1:(num_blocks-1)) {
    k <-  num_blocks - i
    combinations_matrix <- t(combn(num_blocks,k))
    
    for (j in 1:nrow(combinations_matrix)) {
      combination <- combinations_matrix[j, ]
      
    
      r_index <- r_index + 1
      if (r[r_index] == 0) next  # Skip if no factors to extract in this node
      
      
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
      
      
      # Step 2c: Compute new factors 
      number_of_factor <- r[r_index] 
      if (i < num_blocks - 1 && method == 0) {
        # Use CCA for intermediate levels
        Factors <- canonical_correlation_analysis(Residuals, num_vars[combination], number_of_factor, rep(1, num_blocks))
      }else{
        # Use PCA
        pca_result <- prcomp(Residuals, scale. = FALSE)
        Factors <- pca_result$x[, 1:number_of_factor]

      }
      
      Factors<- scale(Factors,TRUE,TRUE)
     
      # Step 2d: Store factors
      key <- paste(combination, collapse = "-")
      Factor_list[[key]] <- Factors  
      InitialFactors <- cbind(InitialFactors, Factors)
    }
    
  }
  
  
  return(list(
    InitialFactors = InitialFactors,
    Factor_list = Factor_list
  ))
}

