#' @title Create Stressed Scenarios 
#' 
#' @description Constructs hyperellipsoids around MLDFM factor estimates using subsampling. These regions capture estimation uncertainty and allow for the simulation of stressed scenarios.
#'
#' @param model An object of class \code{mldfm}, containing the factor estimates.
#' @param subsamples An object of class \code{mldfm_subsample}, returned by \code{mldfm_subsampling}.
#' @param alpha Numeric. Confidence level for the hyperellipsoids (e.g., 0.95).
#' @param fpr Logical. If \code{TRUE}, uses FPR Gamma (Fresoli, Poncela, Ruiz, 2024); otherwise, uses standard time-varying Gamma.
#'
#' @return An object of class \code{fars_scenario}, which is a list containing:
#' \describe{
#'   \item{ellipsoids}{List of matrices defining the hyperellipsoids at each time.}
#'   \item{center}{Matrix of factor estimates (centers of the ellipsoids).}
#'   \item{sigma}{List of covariance matrices used to define the ellipsoids.}
#'   \item{periods}{Number of time observations.}
#'   \item{n_points}{Number of points used to define each ellipsoid.}
#'   \item{alpha}{Confidence level for the hyperellipsoids.}
#'   \item{call}{Function call.}
#' }
#' 
#' 
#' @examples
#' \donttest{
#' data <- matrix(rnorm(100*300), nrow = 100, ncol = 300)
#' block_ind <- c(150, 300)  
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
  if (!inherits(subsamples, "mldfm_subsample")) stop("subsamples must be an object of class 'mldfm_subsample'.")
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) stop("alpha must be a numeric value in (0, 1).")
  if (!is.logical(fpr) || length(fpr) != 1) stop("fpr must be a logical value (TRUE or FALSE).")
  
  
  # Extract model info
  factors <- get_factors(model)
  loadings <- get_loadings(model)
  residuals <- get_residuals(model)
  
  #Extraxt subsamples factors
  subsamples_list <- get_mldfm_list(subsamples)
  factors_samples <- lapply(subsamples_list, get_factors)
  
  
  # Initialize
  n_obs <- nrow(factors)
  n_var <- nrow(loadings)
  tot_n_factors <-  ncol(factors)
  n_samples <- length(factors_samples)
  n_var_sample <- nrow(get_loadings(subsamples_list[[1]]))
  
  
  message(paste0("Constructing scenario using ", n_samples, " subsamples, alpha = ", 
                 alpha, " and ", ifelse(fpr, "FPR Gamma", "standard time-varying Gamma"), "..."))
  
  
  
  # Set ellipsoid center for each obs
  center_matrix <- factors
  
  # Compute the inverse of the loading matrix
  inv_loadings <- solve(crossprod(loadings) / n_var)
  
  # Initialize sigma 
  sigma_list <- vector("list", n_obs)
  
  # Compute FPR gamma if needed
  if(fpr){
    gamma <- compute_fpr_gamma(residuals, loadings)
  }
 
  for (obs in 1:n_obs) {
    # Compute normal gamma 
    if(!fpr){
      d <- residuals[obs, ]^2
      gamma <- crossprod(loadings, loadings * d) / n_var
    }
      
    # Compute Sigma
    term2 <- matrix(0, nrow = tot_n_factors, ncol = tot_n_factors)
    for(s in 1:n_samples){
      factors_sample <- factors_samples[[s]]
      factors_sample <- as.matrix(factors_sample)
      
      # Align signs to the original factors
      for (ff in 1:tot_n_factors) {
        inv_sample <- (-1) * factors_sample[, ff]
        diff1 <- sum(abs(factors[, ff] - factors_sample[, ff]))
        diff2 <- sum(abs(factors[, ff] - inv_sample))

        if (diff2 < diff1) {
          factors_sample[, ff] <- inv_sample
        }
      }

      
      factors_obs <- factors[obs,,drop = FALSE]
      factors_sample_obs <- factors_sample[obs,,drop = FALSE]
      
     
      diff <- factors_sample_obs - factors_obs
      term2 <- term2 + (t(diff) %*% diff) 
      
    }
    
    sigma <- (1/n_var) * inv_loadings %*%  gamma %*% inv_loadings + (n_var_sample/(n_var*n_samples)*term2) # maldonado and ruiz
    sigma_list[[obs]] <- sigma
  }
 
   
  # hyper_ellipsoids 
  hyper_ellipsoids <- vector("list", n_obs)
  
  # Loop over each observation and compute the hyperellipsoid
  for (obs in 1:n_obs) {
    
    center_obs <- center_matrix[obs,]
    sigma_obs <- sigma_list[[obs]]    
    
    calpha <- sizeparam_normal_distn(alpha, d=tot_n_factors)  # Size parameter 

    if (tot_n_factors > 2) {
      # More than 2 dimensions
      h_ellip <- hyperellipsoid(center_obs, solve(sigma_obs), calpha)
      hyper_ellipsoids[[obs]] <- t(hypercube_mesh(8, h_ellip, TRUE))
    } else if (tot_n_factors == 2) {
      # 2D ellipse
      hyper_ellipsoids[[obs]] <- ellipse(sigma_obs, centre = center_obs, level = alpha, npoints = 300)
    } else if (tot_n_factors == 1) {
      # 1D confidence interval
      se <- sqrt(sigma_obs[1, 1])  
      z_alpha <- qnorm((1 + alpha) / 2)  
      lower <- center_obs - z_alpha * se
      upper <- center_obs + z_alpha * se
      hyper_ellipsoids[[obs]] <- matrix(c(lower, upper), ncol = 1)  # 2row matrix: lower and upper 
    }
    
    
    
  }
  message("Scenario construction completed.")
  
  structure(
    list(
      ellipsoids = hyper_ellipsoids,
      center     = center_matrix,
      sigma      = sigma_list,
      periods    = length(hyper_ellipsoids),
      n_points   = nrow(hyper_ellipsoids[[1]]),
      alpha      = alpha,
      call       = match.call()
    ),
    class = "fars_scenario"
  )
  
  
}
