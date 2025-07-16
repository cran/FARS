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
#' @param alpha Numeric. Confidence level (level of stress) for the hyperellipsoid (e.g., 0.95).
#' @param fpr Logical. If TRUE, uses the Adaptive Threshold Cross-Sectional Robust (FPR) Gamma as in Fresoli, Poncela and Ruiz (2024); otherwise, uses the standard time-varying (NG) Gamma.
#'
#' @return A list of matrices representing the hyperellipsoid points for each time observation.
#' 
#' @examples
#' \donttest{
#' data <- matrix(rnorm(100*300), nrow = 100, ncol = 300)
#' block_ind <- c(150, 300)  # Defines 2 blocks
#' global = 1
#' local <- c(1, 1)   
#' mldfm_result <- mldfm(data, blocks = 2, block_ind = block_ind, 
#' global = global, local = local)
#' mldfm_subsampling_result <- mldfm_subsampling(data, blocks = 2, 
#' block_ind = block_ind, global = global, 
#' local = local, n_samples = 100, sample_size = 0.9)
#' scenario <- create_scenario(mldfm_result, mldfm_subsampling_result, 
#' alpha = 0.95)
#' }
#'
#' @import ellipse
#' @import SyScSelection
#' @importFrom stats qnorm 
#'
#' @export
create_scenario <- function(model, subsamples, alpha=0.95, fpr = FALSE) {
  
  
  if (!inherits(model, "mldfm")) stop("model must be an object of class 'mldfm'.")
  if (!is.list(subsamples)) stop("subsamples must be a list of 'mldfm' objects.")
  if (!all(sapply(subsamples, function(obj) inherits(obj, "mldfm")))) {
    stop("All elements in subsamples must be of class 'mldfm'.")
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a numeric value in (0, 1).")
  }
  

  # Extract model factors
  Factors <- model$Factors
  Loadings <- model$Lambda
  Residuals <- model$Residuals
  Factors_list <- model$Factors_list
  
 
  #Extraxt subsample 
  Factors_samples <- lapply(subsamples, function(obj) obj$Factors) 
  
  # Initialize
  n_obs <- nrow(Factors)
  n_var <- nrow(Loadings)
  tot_n_factors <-  ncol(Factors)
  n_samples <- length(subsamples)
  n_var_sample <- nrow(subsamples[[1]]$Lambda)
  
  
  message(paste0("Constructing scenario using ", length(subsamples), 
                 " subsamples and alpha = ", alpha))
  message("Using ", ifelse(fpr, "FPR Gamma", "standard time-varying Gamma"))
  message("... ")
  
  
  # Set ellipsoid center for each obs
  CenterHE_matrix <- Factors
  
  # Compute the inverse of the loading matrix
  inv_Loads <- solve((t(Loadings) %*% Loadings) / n_var)
  
  # Initialize sigma 
  Sigma_list <- list()
  
  # Compute FPR Gamma
  if(fpr){
    Gamma <- compute_gamma_FPR(Residuals, Loadings)
  }
 
  
  for (obs in 1:n_obs) {
    
    # initialize Sigma diagonal for current obs
    Sigma_diag <- c()
    factor_index <- 1
    Sigma <- matrix(0, nrow = tot_n_factors, ncol = tot_n_factors)
    
    
    # Compute normal gamma Gamma
    if(!fpr){
      Gamma <- matrix(0, nrow = ncol(Factors), ncol = ncol(Factors))
      for(v in 1:n_var){
        term <- (Loadings[v,] %*% t(Loadings[v,]))*(Residuals[obs,v]^2)
        
        Gamma <- Gamma + term
      }
      
      Gamma <- Gamma / n_var
    }
   
      
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
    
    Sig <- (1/n_var) * inv_Loads %*%  Gamma %*% inv_Loads + (n_var_sample/(n_var*n_samples)*term2) # maldonado and ruiz

    Sigma_list[[obs]] <- Sig
  }
 
   
  # Hyperellipsoids 
  Hyperellipsoids <- list()
  
  # Loop over each observation and compute the hyperellipsoid
  for (obs in 1:n_obs) {
    
    center_obs <- CenterHE_matrix[obs,]
    sigma_obs <- Sigma_list[[obs]]    
    
    calpha <- sizeparam_normal_distn(alpha, d=tot_n_factors)  # Size parameter 

    # if(tot_n_factors > 2){
    #   # more than 2 dimensions
    #   hellip <- hyperellipsoid(center_obs, solve(sigma_obs), calpha)
    #   Hyperellipsoids[[obs]] <- t(hypercube_mesh(8,hellip,TRUE))
    # }else{
    #   # 2D ellips
    #   Hyperellipsoids[[obs]] <- ellipse(sigma_obs, centre=center_obs, level = alpha, npoints = 300)
    # }
    
    if (tot_n_factors > 2) {
      # More than 2 dimensions
      hellip <- hyperellipsoid(center_obs, solve(sigma_obs), calpha)
      Hyperellipsoids[[obs]] <- t(hypercube_mesh(8, hellip, TRUE))
    } else if (tot_n_factors == 2) {
      # 2D ellipse
      Hyperellipsoids[[obs]] <- ellipse(sigma_obs, centre = center_obs, level = alpha, npoints = 300)
    } else if (tot_n_factors == 1) {
      # 1D confidence interval
      se <- sqrt(sigma_obs[1, 1])  
      z_alpha <- qnorm((1 + alpha) / 2)  
      lower <- center_obs - z_alpha * se
      upper <- center_obs + z_alpha * se
      Hyperellipsoids[[obs]] <- matrix(c(lower, upper), ncol = 1)  # 2row matrix: lower and upper 
    }
    
    
    
  }
  message("Scenario construction completed.")
  return(Hyperellipsoids)
}
