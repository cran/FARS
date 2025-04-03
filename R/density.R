#' Compute Skew-t Densities from Forecasted Quantiles
#'
#' Fits a skew-t distribution to a set of quantile forecasts using linear optimization
#'
#' @param quantiles A matrix of forecasted quantiles. Each row is a time observation; each column a quantile level.
#' @param levels A numeric vector of the quantile levels corresponding to the columns of the quantile matrix (default: c(0.05, 0.25, 0.50, 0.75, 0.95)).
#' @param est_points Integer. Number of evaluation points for the estimated density (default: 512).
#' @param random_samples Integer. Number of random samples to draw from the fitted skew-t distribution (default: 5000).
#' @param seed Optional integer to set the random seed for reproducibility (default: NULL).
#' 
#' @return An object of class \code{"fars_density"}, which is a list containing the following components:
#' \describe{
#'   \item{density}{A matrix of estimated densities for each time period (rows) across estimation points (columns).}
#'   \item{distribution}{A matrix of random draws from the fitted skew-t distribution for each time period.}
#'   \item{x_vals}{The sequence of evaluation points used to compute the density. Useful for plotting.}
#'}
#'
#' @examples
#' \donttest{
#' 
#' Quantiles <- matrix(rnorm(500, mean = 0, sd = 1), nrow = 100, ncol = 5)
#' Levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
#' density_result <- density(Quantiles,
#'                           levels = Levels,
#'                           est_points = 512,
#'                           random_samples = 100000,
#'                           seed = 42)
#'}
#'
#' @import sn
#' @importFrom stats prcomp qnorm optim
#' @export
density <- function(quantiles, 
                    levels = c(0.05, 0.25, 0.50, 0.75, 0.95), 
                    est_points = 512, 
                    random_samples = 5000,
                    seed = NULL) {
 
  # Argument checks
  if (!is.matrix(quantiles)) stop("'quantiles' must be a matrix.")
  if (!is.numeric(levels) || length(levels) != ncol(quantiles)) stop("'levels' must be a numeric vector of same length as number of quantile columns.")
  if (!is.numeric(est_points) || est_points < 1) stop("'est_points' must be a positive integer.")
  if (!is.numeric(random_samples) || random_samples < 1) stop("'random_samples' must be a positive integer.")
  
  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  # Initialize variables
  n_obs = nrow(quantiles) 
  density_matrix <- matrix(NA, nrow = n_obs, ncol = est_points) # density matrix
  distribution <- matrix(NA, nrow = n_obs, ncol = random_samples) # skew-t distribution 
  
  message("Estimating skew-t densities from forecasted quantiles...")
  
  for (tt in 1:n_obs){
    
    
    # Initial values
    iqn <- qnorm(0.75)-qnorm(0.25) # Interquartile range of standard normal distr
    l0 <- quantiles[tt,3]  # Location
    s0 <- (quantiles[tt,4] - quantiles[tt,2]) / iqn # Scale
    sh0 <- 0 # Shape
    
    
    LB = c(l0-10, 1, -100) 
    UB = c(l0+20, 50, 100)
    
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
    x_vals <- seq(-30, 10, length.out = est_points)
    density_matrix[tt, ] <- dst(x_vals, 
                                xi = fit$par[1], 
                                omega = fit$par[2], 
                                alpha = fit$par[3])
    
    
  }
  
  output <- list(
    density = density_matrix, 
    distribution = distribution,
    x_vals = x_vals)
  
  class(output) <- "fars_density"
  print(output)
  
  return(output)
  
}





