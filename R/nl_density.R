#' Compute Skew-t Densities from Forecasted Quantiles (Nonlinear Optimization)
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
  x_vals <- seq(support[1], support[2], length.out = est_points)
  
  for (tt in 1:n_obs) {
    
    # Initial values
    iqn <- qnorm(0.75) - qnorm(0.25) # Interquartile range of standard normal distribution
    l0 <- quantiles[tt, 3]  # Location
    s0 <- log(max(1, (quantiles[tt, 4] - quantiles[tt, 2]) / iqn)) # Log of scale to use with exp
    sh0 <- 0 # Shape

    # Starting parameters for optimization
    x0 <- c(l0, s0, sh0)
    LB <- c(l0 - 10, -Inf, -Inf)
    UB <- c(l0 + 20, Inf, Inf)

    
   
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
    density_matrix[tt, ] <- dst(x_vals, 
                                xi = xi_opt, 
                                omega = omega_opt, 
                                alpha = alpha_opt)
  }
  
  output <- list(
    optimization = "Non-linear",
    density = density_matrix, 
    distribution = distribution,
    x_vals = x_vals)
  
  class(output) <- "fars_density"
 
  
  return(output)
}
