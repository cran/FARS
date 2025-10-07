#' @title Compute Skew-t Densities from Quantiles  (Linear Optimization)
#'
#' @description Computes the skew-t density from a matrix of quantiles using linear optimization method.
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
#' @importFrom stats qnorm optim
#' 
#' @keywords internal
l_density <- function(quantiles, 
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
  
  for (tt in 1:n_obs){
    
    
    # Initial restrictions
    iqn <- qnorm(0.75)-qnorm(0.25) # Interquartile range of standard normal distr
    l0 <- quantiles[tt,3]  # Location
    s0 <- (quantiles[tt,4] - quantiles[tt,2]) / iqn # Scale
    
    sh0 <- 0 # Shape
    

    #LB = c(l0-10, 1, -100)
    #UB = c(l0+20, 50, 100)
    
    LB = c(l0 - 3 * s0, 0.1, -5)  
    UB = c(l0 + 3 * s0, 5 * s0, 5)  
    
    
    # LB = c(l0 - 5 * s0, max(0.1, s0/5), -20)
    # UB = c(l0 + 5 * s0, s0*5, 20)
    
    
    
    fit <- optim(
      par = c(l0, s0, sh0),
      fn=function(x) {
        sum((quantiles[tt,] - qst(levels, xi = x[1], omega = x[2], alpha = x[3]))^2)
      }, 
      method = "L-BFGS-B",
      lower = LB,
      upper = UB
      )
    

    # Generate and store samples from skew-t
    distribution[tt,] <- rst(n = random_samples, 
                             xi = fit$par[1], 
                             omega = fit$par[2], 
                             alpha = fit$par[3])  # dp = NULL
  
    # Evaluate density
    density_matrix[tt, ] <- dst(eval_points, 
                                xi = fit$par[1], 
                                omega = fit$par[2], 
                                alpha = fit$par[3])
    
    
  }
  
  output <- list(
    optimization = "Linear",
    density = density_matrix, 
    distribution = distribution,
    eval_points = eval_points)
  
  class(output) <- "fars_density"
  
  
  return(output)
  
}





