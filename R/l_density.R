#' Compute Skew-t Densities from Forecasted Quantiles  (Linear Optimization)
#'
#' @import sn
#' @importFrom stats prcomp qnorm optim
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
  x_vals <- seq(support[1], support[2], length.out = est_points)
  
  for (tt in 1:n_obs){
    
    
    # Initial restrictions
    iqn <- qnorm(0.75)-qnorm(0.25) # Interquartile range of standard normal distr
    l0 <- quantiles[tt,3]  # Location
    s0 <- (quantiles[tt,4] - quantiles[tt,2]) / iqn # Scale
    sh0 <- 0 # Shape
    

    LB = c(l0-10, 1, -100)
    UB = c(l0+20, 50, 100)
    
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
    density_matrix[tt, ] <- dst(x_vals, 
                                xi = fit$par[1], 
                                omega = fit$par[2], 
                                alpha = fit$par[3])
    
    
  }
  
  output <- list(
    optimization = "Linear",
    density = density_matrix, 
    distribution = distribution,
    x_vals = x_vals)
  
  class(output) <- "fars_density"
  
  
  return(output)
  
}





