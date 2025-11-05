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
create_scenario <- function(model, subsamples, alpha = 0.95, fpr = FALSE) {
  
  if (!inherits(model, "mldfm")) stop("model must be an object of class 'mldfm'.")
  if (!inherits(subsamples, "mldfm_subsample")) stop("subsamples must be an object of class 'mldfm_subsample'.")
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) stop("alpha must be a numeric value in (0, 1).")
  if (!is.logical(fpr) || length(fpr) != 1) stop("fpr must be a logical value (TRUE or FALSE).")
  
  # Extract model information
  factors   <- factors(model)         # T x K
  loadings  <- loadings(model)        # N x K
  residuals <- residuals(model)       # T x N
  
  # Extract subsample factors
  subsamples_list <- get_mldfm_list(subsamples)
  factors_samples <- lapply(subsamples_list, factors)
  
  # Dimensions
  n_obs         <- nrow(factors)
  n_var         <- nrow(loadings)
  tot_n_factors <- ncol(factors)
  n_samples     <- length(factors_samples)
  n_var_sample  <- nrow(loadings(subsamples_list[[1]]))
  
  message(paste0(
    "Constructing scenario using ", n_samples,
    " subsamples, alpha = ", alpha, " and ",
    ifelse(fpr, "FPR Gamma", "standard time-varying Gamma"), "..."
  ))
  
  # Center of each hyperellipsoid
  center_matrix <- factors
  
  # Compute (L'L / N)^(-1)
  inv_loadings <- n_var * chol2inv(chol(crossprod(loadings)))
  
  
  # Align factor signs once per subsample
  align_one <- function(Fs, F0) {
    sgn <- sign(colSums(F0 * Fs))
    sgn[sgn == 0] <- 1L
    Fs * rep(sgn, each = nrow(Fs))
  }
  factors_samples <- lapply(factors_samples, align_one, F0 = factors)
  
  # Initialize sigma list
  sigma_list <- vector("list", n_obs)
  
  # Precompute FPR gamma if required
  if (fpr) {
    gamma_fpr <- compute_fpr_gamma(residuals, loadings)
  }
  
  # Loop over time observations
  for (obs in seq_len(n_obs)) {
    
    # Compute gamma_t
    if (!fpr) {
      d <- residuals[obs, ]^2
      gamma <- crossprod(loadings * rep(sqrt(d), times = 1L)) / n_var
    } else {
      gamma <- gamma_fpr
    }
    
    # Compute term2 (empirical uncertainty from subsamples)
    term2 <- matrix(0, nrow = tot_n_factors, ncol = tot_n_factors)
    f_ref <- center_matrix[obs, , drop = FALSE]
    for (s in seq_len(n_samples)) {
      f_s_obs <- factors_samples[[s]][obs, , drop = FALSE]
      diff    <- f_s_obs - f_ref
      term2   <- term2 + tcrossprod(drop(diff))
    }
    
    # Total covariance: Maldonado & Ruiz 
    sigma <- (1 / n_var) * (inv_loadings %*% gamma %*% inv_loadings) +
      (n_var_sample / (n_var * n_samples)) * term2
    
    sigma_list[[obs]] <- sigma
  }
  
  # Build hyperellipsoids
  hyper_ellipsoids <- vector("list", n_obs)
  calpha <- sizeparam_normal_distn(alpha, d = tot_n_factors)
  
  for (obs in seq_len(n_obs)) {
    center_obs <- center_matrix[obs, ]
    sigma_obs  <- sigma_list[[obs]]
    
    if (tot_n_factors > 2) {
      Prec <- chol2inv(chol(sigma_obs))
      h_ellip <- hyperellipsoid(center_obs, Prec, calpha)
      hyper_ellipsoids[[obs]] <- t(hypercube_mesh(8, h_ellip, TRUE))
    } else if (tot_n_factors == 2) {
      hyper_ellipsoids[[obs]] <- ellipse(sigma_obs, centre = center_obs, level = alpha, npoints = 300)
    } else {
      se     <- sqrt(sigma_obs[1, 1])
      z_alph <- qnorm((1 + alpha) / 2)
      lower  <- center_obs - z_alph * se
      upper  <- center_obs + z_alph * se
      hyper_ellipsoids[[obs]] <- matrix(c(lower, upper), ncol = 1)
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