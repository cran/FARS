# Help function 
beta_ols <- function(X, Y) {
  solve(t(X) %*% X) %*% t(X) %*% Y
}
#' Create Stressed Scenarios
#'
#' Constructs confidence regions (hyperellipsoids) for the factor space based on a central MLDFM estimate
#' and a set of subsampled estimates. These regions capture estimation uncertainty and are used to simulate
#' stresses scenarios.
#'
#' @param model An object of class \code{mldfm}, containing the factor estimates.
#' @param subsamples A list of \code{mldfm} objects returned from \code{mldfm_subsampling}.
#' @param data A numeric matrix or data frame containing the time series data. Rows represent time points; columns represent observed variables.
#' @param block_ind A vector of integers indicating the end index of each block. Must be of length \code{blocks} and in increasing order. Required if \code{blocks > 1}.
#' @param alpha Numeric. Confidence level (level of stress) for the hyperellipsoid (e.g., 0.95).
#'
#' @return A list of matrices representing the hyperellipsoid points for each time observation.
#' 
#' @examples
#' \donttest{
#' data <- matrix(rnorm(1000), nrow = 100, ncol = 519)
#' block_ind <- c(63, 311, 519)  # Defines 3 blocks
#' r <- c(1, 1, 1, 1, 1, 1, 1)   # 2^3 - 1 = 7 nodes
#' mldfm_result <- mldfm(data, blocks = 3, block_ind = block_ind, r = r)
#' mldfm_subsampling_result <- mldfm_subsampling(data, blocks = 3, block_ind = block_ind, r = r, 
#' n_samples = 100, sample_size = 0.9)
#' scenario <- create_scenario(mldfm_result, mldfm_subsampling_result, data, block_ind, alpha = 0.95)
#' }
#'
#' @import ellipse
#' @import SyScSelection
#'
#' @export
create_scenario <- function(model, subsamples, data, block_ind, alpha=0.95) {
  
  
  if (!inherits(model, "mldfm")) stop("model must be an object of class 'mldfm'.")
  if (!is.list(subsamples)) stop("subsamples must be a list of 'mldfm' objects.")
  if (!all(sapply(subsamples, function(obj) inherits(obj, "mldfm")))) {
    stop("All elements in subsamples must be of class 'mldfm'.")
  }
  if (!is.matrix(data) && !is.data.frame(data)) stop("data must be a matrix or data frame.")
  if (is.null(block_ind)) stop("block_ind must be provided when blocks.")
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a numeric value in (0, 1).")
  }
  
  data <- scale(data,TRUE,TRUE)
  
  # Extract model factors
  Factors <- model$Factors
  Factors_list <- model$Factors_list
  
  #Extraxt subsample (factors and factors_hat)
  Factors_samples <- lapply(subsamples, function(obj) obj$Factors) 
  Factors_hat_samples <- lapply(subsamples, function(obj) obj$Factors_hat)
  
  
  
  n_obs <- nrow(Factors)
  tot_n_factors <-  ncol(Factors)
  n_samples <- length(subsamples)
  
  
  # Define block ranges and count the number of variables in each range
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
  
  
  message(paste0("Constructing scenario using ", length(subsamples), 
                 " subsamples and alpha = ", alpha, "..."))
  
  
  # Set ellipsoid center for each obs
  CenterHE_matrix <- Factors
  
  # Initialize sigma 
  Sigma_list <- list()
  
  for (obs in 1:n_obs) {
    
    # initialize Sigma diagonal for current obs
    Sigma_diag <- c()
    factor_index <- 1
    
    Sigma <- matrix(0, nrow = tot_n_factors, ncol = tot_n_factors)
    
    # loop over  factors
    for (key in names(Factors_list)){
      
      # Extract data
      combination <- as.numeric(unlist(strsplit(key, "-")))
      Block <- do.call(cbind, lapply(combination, function(idx) data[, ranges[[idx]]]))
      n_factors <- Factors_list[[key]]
    
      # Extract corresponding factors
      Facts <- Factors[,factor_index:(factor_index+n_factors-1)]
      
      # Compute Loadings and Residuals
      Loads <- beta_ols(Facts, Block)
      Resid <- Block - Facts %*% Loads
      
      # Compute Factors Hat
      N <- ncol(Loads)
      Facts_hat<-(1/N)*Block%*%t(Loads)
      
      # Compute number of variables
      n_var <- ncol(Loads)
      
      # Compute the inverse of the loading matrix
      inv_Loads <- solve((Loads %*% t(Loads)) / n_var)
      
      # Compute Gamma
      Gamma <- matrix(0, nrow = ncol(Facts_hat), ncol = ncol(Facts_hat))
      for(v in 1:n_var){
        term <- (Loads[,v] %*% t(Loads[,v]))*(Resid[obs,v]^2)
        Gamma <- Gamma + term
      }
      
      Gamma <- Gamma / n_var
      
      
      # Compute Sigma
      term2 <- matrix(0, nrow = ncol(Facts_hat), ncol = ncol(Facts_hat))
      for(s in 1:n_samples){
        # Extract sample's Factors
        Factors_hat_s <- Factors_hat_samples[[s]]
        Facts_hat_s <- Factors_hat_s[,factor_index:(factor_index+n_factors-1)]
        Facts_hat_s <- as.matrix(Facts_hat_s) # impose matrix structure 
        
        # Extract current obs
        Facts_hat_s_obs <- Facts_hat_s[obs,]
        Facts_hat_obs <- Facts_hat[obs,]
        
        # Compute diff
        diff <- Facts_hat_s_obs - Facts_hat_obs
        term2 <- term2 + (diff %*% t(diff)) 
        
      }
      
      Sig <- inv_Loads %*% ((term2/n_samples)+Gamma) %*% inv_Loads
      size <- ncol(Sig)
      
      Sigma[factor_index:(factor_index + size - 1), factor_index:(factor_index + size - 1)] <- Sig
      
      
      factor_index <- factor_index + n_factors
    
    }
   
    #Sigma <- diag(Sigma_diag)
    #Sigma <- Sig
   
    # Store the sigma matrix for this observation
    Sigma_list[[obs]] <- Sigma
  
   
  }
  
  # Hyperellipsoids 
  Hyperellipsoids <- list()
  
  # Loop over each observation and compute the hyperellipsoid
  for (obs in 1:n_obs) {
    
    center_obs <- CenterHE_matrix[obs,]
    sigma_obs <- Sigma_list[[obs]]     
    
    calpha <- sizeparam_normal_distn(alpha, d=n_factors)  # Size parameter 
    #calpha <- qchisq(alpha, df = tot_n_factors)
    
    
    if(tot_n_factors > 2){
      # more than 2 dimensions
      hellip <- hyperellipsoid(center_obs, solve(sigma_obs), calpha)
      Hyperellipsoids[[obs]] <- t(hypercube_mesh(8,hellip,TRUE))
    }else{
      # 2D ellips
      Hyperellipsoids[[obs]] <- ellipse(sigma_obs, centre=center_obs, level = alpha, npoints = 300)
    }
    
  }
  message("Scenario construction completed.")
  return(Hyperellipsoids)
}
