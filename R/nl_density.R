#' @title Compute Skew-t Densities from Quantiles  (Non-Linear Optimization)
#'
#' @description Computes the skew-t density from a matrix of quantiles using non-linear optimization method.
#'
#' @param quantiles A matrix of quantiles. Each row represents a time observation, and each column corresponds to a quantile level.
#' @param levels A numeric vector of the quantile levels corresponding to the columns of the quantile matrix (default: c(0.05, 0.25, 0.50, 0.75, 0.95)).
#' @param est_points Integer. The number of evaluation points for the estimated density (default: 512).
#' @param random_samples Integer. The number of random samples to draw from the fitted skew-t distribution (default: 5000).
#' @param support Numeric vector of length 2. Defines the lower and upper limits of the density evaluation range. Used with \code{est_points} to create the evaluation grid. Default: \code{c(-10, 10)}.
#' @param seed Optional integer to set the random seed for reproducibility.
#' 
#' @return 
#' An object of class \code{"fars_density"}, which is a list containing:
#' \describe{
#'   \item{density}{A matrix of estimated densities for each time period (rows) across estimation points (columns).}
#'   \item{distribution}{A matrix of random draws from the fitted skew-t distribution for each time period.}
#'   \item{optimization}{The optimization method used (either 'nloptr' or 'optim').}
#'   \item{eval_points}{The sequence of evaluation points used to compute the density. Useful for plotting.}
#'}
#'
#' @import sn
#' @import nloptr
#' @importFrom stats prcomp qnorm optim
#'
#' @keywords internal
nl_density <- function(quantiles, 
                    levels = c(0.05, 0.25, 0.50, 0.75, 0.95), 
                    est_points = 512, 
                    random_samples = 5000,
                    support = c(-10,10),
                    seed = NULL) {
  
  
  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  
  # Initialize variables
  n_obs = nrow(quantiles) 
  density_matrix <- matrix(NA, nrow = n_obs, ncol = est_points) # density matrix
  distribution <- matrix(NA, nrow = n_obs, ncol = random_samples) # skew-t distribution 

  message("Estimating skew-t densities from forecasted quantiles...")
  
  # Support
  eval_points <- seq(support[1], support[2], length.out = est_points)
  
  for (tt in 1:n_obs) {
    
    # Initial values
    iqn <- qnorm(0.75) - qnorm(0.25) # Interquartile range of standard normal distribution
    l0 <- quantiles[tt, 3]  # Location
    s0 <- log(max(1, (quantiles[tt, 4] - quantiles[tt, 2]) / iqn)) # Log of scale to use with exp
    sh0 <- 0 # Shape

    # Starting parameters for optimization
    x0 <- c(l0, s0, sh0)
    
    # LB <- c(l0 - 10, -Inf, -Inf)
    # UB <- c(l0 + 20, Inf, Inf)
    
    
    LB <- c(l0 - 3, -Inf, -Inf)
    UB <- c(l0 + 3, Inf, Inf)


   
    # Objective function
    objective_fn <- function(x) {
      transformed_values <- qst(levels, xi = x[1], omega = exp(x[2]), alpha = tanh(x[3]))
      return(sum((quantiles[tt, ] - transformed_values)^2))
    }
    
   
    
    # Non-linear optimization using nloptr
    result <- nloptr::nloptr(
      x0 = x0,
      eval_f = objective_fn,
      lb = LB,
      ub = UB,
      opts = list(
        algorithm = "NLOPT_LN_SBPLX", 
        xtol_rel = 1.0e-8
      )
    )
    
    # Extract optimized parameters 
    xi_opt <- result$solution[1]
    omega_opt <- exp(result$solution[2]) 
    alpha_opt <- tanh(result$solution[3]) 
    
    # Generate and store samples from skew-t
    distribution[tt, ] <- sn::rst(n = random_samples, 
                                  xi = xi_opt, 
                                  omega = omega_opt, 
                                  alpha = alpha_opt)
    
    # Evaluate density
    density_matrix[tt, ] <- dst(eval_points, 
                                xi = xi_opt, 
                                omega = omega_opt, 
                                alpha = alpha_opt)
  }
  
  output <- list(
    optimization = "Non-linear",
    density = density_matrix, 
    distribution = distribution,
    eval_points = eval_points)
  
  class(output) <- "fars_density"
 
  
  return(output)
}
