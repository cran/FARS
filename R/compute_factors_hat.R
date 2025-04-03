#' Compute Factors_hat from Observed Data and Loadings
#'
#' Projects observed data onto the estimated loading space to reconstruct \code{Factors_hat}
#' for each combination of blocks in the multilevel structure.
#'
#' @keywords internal
compute_factors_hat<- function(Yorig, ranges, Final_list,Loadings_list) {
  
  # Initialize
  num_obs <- nrow(Yorig)
  Factors_hat <- matrix(nrow = num_obs, ncol = 0)
  
  for (key in names(Final_list)){
    # extract combination of the node
    combination <- as.numeric(unlist(strsplit(key, "-")))
    
    # Extract node data
    Block <- do.call(cbind, lapply(combination, function(idx) Yorig[, ranges[[idx]]]))
    
    # Extract Loadings
    Loadings <- Loadings_list[[key]]
    N <- ncol(Loadings)
    
    # Compute Factors Hat
    Factors_hat_block<-(1/N)*Block%*%t(Loadings)
    
    # Append
    Factors_hat <- cbind(Factors_hat, Factors_hat_block)
    
  }
 
  return(Factors_hat)
}

