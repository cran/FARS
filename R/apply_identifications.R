#' Apply Identification Constraints to Factors and Loadings
#' 
#' @keywords internal
apply_identifications <- function(Yorig, num_blocks, ranges, r_list, 
                                  currentFactors, Factor_list, Loadings_list) {
  
 
  num_factors <- sum(unlist(r_list))
  T_obs <- nrow(Yorig)
  FinalFactors <- matrix(nrow = T_obs, ncol = 0)  
  
  
  # --- Step 1: Orthogonalize current factors (Identifiation III) ---
  orthogonal_Factors <- orthogonalize_factors(currentFactors) 
  #orthogonal_Factors <- qr.Q(qr(currentFactors))
  Factor_list <- update_factor_list(Factor_list, orthogonal_Factors, r_list)


  # Recompute loadings based on orthogonal factors
  L_res <- compute_lambda(Yorig,num_blocks,ranges,r_list,Factor_list,Loadings_list)
  Loadings_list <- L_res$Loadings_list
  
  
  # Initialize new Lambda
  Lambda <- matrix(0, nrow = num_factors, ncol =  ncol(Yorig))
  
  r_index <- 1
  counter <- 1
  
  # --- Step 2: Identification I and II on Global Factors ---
  global_key <- paste(seq(1, num_blocks), collapse = "-")
  GlobalFactors <- Factor_list[[global_key]]
  GlobalLoadings <- Loadings_list[[global_key]]
  
  # Apply identifications I and II
  CommonComponent <- GlobalFactors %*% GlobalLoadings
  pca_result <- prcomp(CommonComponent, center = FALSE, scale. = FALSE)
  GlobalFactors_new <- pca_result$x[, 1:ncol(GlobalFactors), drop = FALSE]
  GlobalFactors_new<- scale(GlobalFactors_new,TRUE,TRUE)
  GlobalLoadings_new <- qr.solve(GlobalFactors_new, CommonComponent)

  # Update outputs
  FinalFactors <- cbind(FinalFactors, GlobalFactors_new)
  Factor_list[[global_key]] <- GlobalFactors_new
  Loadings_list[[global_key]] <- GlobalLoadings_new
  combination <- seq(1, num_blocks)
  Lambda[counter:(counter+r_list[[global_key]]-1), unlist(ranges[combination])] <- GlobalLoadings_new
  counter <- counter + r_list[[global_key]]
  
  # Check identifications
  # print(key)
  # check_identification_condition_1(GlobalFactors_new)
  # check_identification_condition_2(t(GlobalLoadings_new))
  
  # --- Step 3: Identification I and II on Lower Level Factors ---
  for (key in names(r_list)) {
    if (key == global_key) next
    
    combination <- as.numeric(unlist(strsplit(key, "-")))
    
    # Get residuals after removing higher-level structure
    Residuals <- do.call(cbind, lapply(combination, function(idx) Yorig[, ranges[[idx]]]))
    
    level <- num_blocks
    while (level > length(combination)) {
      Factors <- get_factors(Factor_list, combination, level)
      if(!is.null(Factors)){
        ols_result <- beta_ols(Factors, Residuals)
        Residuals <- Residuals - Factors %*% ols_result
      }
      level <- level - 1
    }
    
    # Extract current node factors
    key <- paste(combination, collapse = "-")
    Factors <- Factor_list[[key]]
    Loadings <- Loadings_list[[key]]
    
    # Apply identifications I and II
    CommonComponent <- Factors %*% Loadings
    pca_result <- prcomp(CommonComponent, center = FALSE, scale. = FALSE)
    Factors_new <- pca_result$x[, 1:ncol(Factors), drop = FALSE]  # 59 x r
    Factors_new<- scale(Factors_new,TRUE,TRUE)
    Loadings_new <- qr.solve(Factors_new, CommonComponent)
    
    # Update
    FinalFactors <- cbind(FinalFactors, Factors_new)
    Factor_list[[key]] <- Factors_new
    Loadings_list[[key]] <- Loadings_new
    Lambda[counter:(counter+r_list[[key]]-1), unlist(ranges[combination])] <- Loadings_new
    counter <- counter + r_list[[key]]
    
    # Check identifications
    # print(key)
    # check_identification_condition_1(Factors_new)
    # check_identification_condition_2(t(Loadings_new))
  }
  
  return(list(
    Factor_list = Factor_list,
    FinalFactors = FinalFactors,
    Lambda = Lambda
  ))
}

