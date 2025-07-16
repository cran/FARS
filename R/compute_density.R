#' Compute Skew-t Densities from Forecasted Quantiles
#'
#'
#' @param quantiles A matrix of forecasted quantiles. Each row is a time observation; each column a quantile level.
#' @param levels A numeric vector of the quantile levels corresponding to the columns of the quantile matrix (default: c(0.05, 0.25, 0.50, 0.75, 0.95)).
#' @param est_points Integer. Number of evaluation points for the estimated density (default: 512).
#' @param random_samples Integer. Number of random samples to draw from the fitted skew-t distribution (default: 5000).
#' @param support Numeric vector of length 2. Defines the lower and upper limits of the density evaluation range. Used with \code{est_points} to create the evaluation grid. Default: \code{c(-30, 10)}.
#' @param nl Logical. If \code{TRUE}, uses nonlinear optimization via \code{nloptr}; if \code{FALSE}, uses linear optimization via \code{optim}. Default: \code{FALSE}.
#' @param seed Optional integer to set the random seed for reproducibility (default: NULL).
#' 
#' @return An object of class \code{"fars_density"}, which is a list containing the following components:
#' \describe{
#'   \item{density}{A matrix of estimated densities for each time period (rows) across estimation points (columns).}
#'   \item{distribution}{A matrix of random draws from the fitted skew-t distribution for each time period.}
#'   \item{optimization}{The optimization method implemented.}
#'   \item{x_vals}{The sequence of evaluation points used to compute the density. Useful for plotting.}
#'}
#'
#' @examples
#' \donttest{
#' 
#' Quantiles <- matrix(rnorm(500, mean = 0, sd = 1), nrow = 100, ncol = 5)
#' density_result <- compute_density(Quantiles, seed = 42)
#'}
#'
#' @import sn
#' @importFrom stats prcomp qnorm optim
#' @export
compute_density <- function(quantiles, 
                    levels = c(0.05, 0.25, 0.50, 0.75, 0.95), 
                    est_points = 512, 
                    random_samples = 5000,
                    support = c(-10,10),
                    nl = FALSE,
                    seed = NULL) {
 
  # Argument checks
  if (!is.matrix(quantiles)) stop("'quantiles' must be a matrix.")
  if (!is.numeric(levels) || length(levels) != ncol(quantiles)) stop("'levels' must be a numeric vector of same length as number of quantile columns.")
  if (!is.numeric(est_points) || est_points < 1) stop("'est_points' must be a positive integer.")
  if (!is.numeric(random_samples) || random_samples < 1) stop("'random_samples' must be a positive integer.")
  if (!is.numeric(support) || length(support) != 2 || support[1] >= support[2]) {
    stop("'support' must be a numeric vector of length 2 with increasing values: c(lower, upper).")
  }
  
  
  # Call linear or nonlinear version
  if (nl) {
    output <- nl_density(quantiles = quantiles,
                         levels = levels,
                         est_points = est_points,
                         random_samples = random_samples,
                         support = support,
                         seed = seed)
  } else {
    output <- l_density(quantiles = quantiles,
                        levels = levels,
                        est_points = est_points,
                        random_samples = random_samples,
                        support = support,
                        seed = seed)
  }
  
  message("Completed")
  return(output)
  
}





