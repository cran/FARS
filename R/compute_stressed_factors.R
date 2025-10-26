#' @title Compute Stressed Factors
#'
#' @description Computes stressed factors.
#'
#' @param dep_variable Numeric vector of length T representing the dependent variable (e.g., GDP growth, inflation).
#' @param factors Numeric matrix or data frame of dimension T x r, containing factor estimates.
#' @param ellipsoids List of matrices. Each matrix represents a stressed ellipsoid for a given time period. 
#' @param h Integer (>= 1). Lag order used in the regression (default = 1)
#' @param qtau Numeric between 0 and 1. The quantile level used in quantile regression (default = 0.05).
#' @param direction Character, either "min" or "max". Determines whether to minimize or maximize
#'
#' @return A numeric matrix of dimension T x r containing the stressed factors for each time period.
#'
#' @examples
#' set.seed(42)
#' T <- 100; r <- 3
#' Y <- rnorm(T)
#' F <- matrix(rnorm(T * r), T, r)          
#' E <- replicate(T, matrix(rnorm(50 * r), nrow = 50, ncol = r), simplify = FALSE)
#' stressed_factors <- compute_stressed_factors(Y, F, E, h = 1, qtau = 0.05, direction = "min")
#' 
#' @import quantreg
#' @importFrom stats as.formula
#' 
#' @export
compute_stressed_factors <- function(dep_variable, 
                                     factors, 
                                     ellipsoids, 
                                     h = 1, 
                                     qtau = 0.05, 
                                     direction = c("min", "max")) {
  
  # Check parameters
  direction <- match.arg(direction)
  
  if (!is.numeric(dep_variable)) stop("dep_variable must be numeric.")
  if (is.data.frame(factors)) factors <- as.matrix(factors)
  if (!is.matrix(factors)) stop("factors must be a matrix or data.frame.")
  Tn <- length(dep_variable)
  if (nrow(factors) != Tn) stop("factors must have T rows (same as dep_variable).")
  r <- ncol(factors)
  if (r < 1) stop("factors must have at least one column.")
  if (!is.numeric(h) || h < 1) stop("h must be integer >= 1.")
  if (!(qtau > 0 && qtau < 1)) stop("qtau must be in (0,1).")
  if (!is.list(ellipsoids) || length(ellipsoids) < 1) {
    stop("ellipsoids must be a non-empty list.")
  }
  
  # Lag variables
  Y <- dep_variable
  LagY <- c(rep(NA_real_, h), dep_variable[1:(Tn - h)])
  LagF <- rbind(
    matrix(NA_real_, nrow = h, ncol = r),
    factors[1:(Tn - h), , drop = FALSE]
  )
  
  X_full <- cbind(Intercept = 1, LagY = LagY, LagF)
  idx <- stats::complete.cases(X_full)  
  X <- X_full[idx, , drop = FALSE]
  y <- Y[idx]
  
  # Quantile regression
  fit <- quantreg::rq.fit(x = X, y = y, tau = qtau)
  
  # Minimization/maximization
  beta <- as.numeric(fit$coefficients)
  beta0 <- beta[1]
  beta1 <- beta[2]
  beta_f <- beta[-c(1,2)]     
  
  stressed_factors <- matrix(NA_real_, nrow = Tn, ncol = r)
  
  for (tt in 1:Tn) {
    ellips <- ellipsoids[[tt]]
    if (!is.matrix(ellips)) {
      ellips <- as.matrix(ellips)
    }
    
    pred <- as.numeric(beta0 + beta1 * Y[tt] + ellips %*% beta_f)
    pick <- if (direction == "min") which.min(pred) else which.max(pred)
    stressed_factors[tt,] <- ellips[pick,]
  }
  
  return(stressed_factors)
  
}