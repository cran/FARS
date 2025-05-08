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
#' data <- matrix(rnorm(100*300), nrow = 100, ncol = 300)
#' block_ind <- c(150, 300)  # Defines 3 blocks
#' r <- c(1, 1, 1)   # 2^2 - 1 = 4 nodes
#' mldfm_result <- mldfm(data, blocks = 2, block_ind = block_ind, r = r)
#' mldfm_subsampling_result <- mldfm_subsampling(data, blocks = 2, block_ind = block_ind, r = r, 
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
  Loadings <- model$Lambda
  #Loadings <-t((1/nrow(Factors))*t(Factors)%*%data)
  Residuals <- model$Residuals
  #Residuals<-data-Factors%*%t(Loadings)
  Factors_list <- model$Factors_list
  
 
  #Extraxt subsample 
  Factors_samples <- lapply(subsamples, function(obj) obj$Factors) 
  
  # Initialize
  n_obs <- nrow(Factors)
  n_var <- nrow(Loadings)
  tot_n_factors <-  ncol(Factors)
  n_samples <- length(subsamples)
  n_var_sample <- nrow(subsamples[[1]]$Lambda)
  
  
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
    
    
    # Compute Gamma
    Gamma <- matrix(0, nrow = ncol(Factors), ncol = ncol(Factors))
    for(v in 1:n_var){
      term <- (Loadings[v,] %*% t(Loadings[v,]))*(Residuals[obs,v]^2)
      
      Gamma <- Gamma + term
    }
    
    Gamma <- Gamma / n_var
    
    
    # Compute the inverse of the loading matrix
    inv_Loads <- solve((t(Loadings) %*% Loadings) / n_var)
      
    # Compute Sigma
    term2 <- matrix(0, nrow = ncol(Factors), ncol = ncol(Factors))
    for(s in 1:n_samples){
      Factors_sample <- Factors_samples[[s]]
      Factors_sample <- as.matrix(Factors_sample)
      
      
      
      # Align signs to the original factors
      for (ff in 1:tot_n_factors) {
        inv_sample <- (-1) * Factors_sample[, ff]
        dif1 <- sum(abs(Factors[, ff] - Factors_sample[, ff]))
        dif2 <- sum(abs(Factors[, ff] - inv_sample))

        if (dif2 < dif1) {
          Factors_sample[, ff] <- inv_sample
        }
      }

      
      
      Factors_obs <- Factors[obs,,drop = FALSE]
      Factors_sample_obs <- Factors_sample[obs,,drop = FALSE]
      
     
      diff <- Factors_sample_obs - Factors_obs
      term2 <- term2 + (t(diff) %*% diff) 
      
    }
    
   
    #Sig <- inv_Loads %*% ((term2/n_samples)+Gamma) %*% inv_Loads
    #Sig <- (1/n_var) * inv_Loads %*%  Gamma %*% inv_Loads + (n_var_sample/(n_var*n_samples)*term2)
    Sig <-  inv_Loads %*%  Gamma %*% inv_Loads + (n_var_sample/(n_var*n_samples)*term2)
    
    Sigma_list[[obs]] <- Sig
  }
 
   
  # Hyperellipsoids 
  Hyperellipsoids <- list()
  
  # Loop over each observation and compute the hyperellipsoid
  for (obs in 1:n_obs) {
    
    center_obs <- CenterHE_matrix[obs,]
    sigma_obs <- Sigma_list[[obs]]    
    
   
    
    calpha <- sizeparam_normal_distn(alpha, d=tot_n_factors)  # Size parameter 
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
