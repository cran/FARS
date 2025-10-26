#' @title Compute Factor Augmented Quantile Regressions 
#'
#' @description Performs quantile regressions of a dependent variable on factors estimates.
#'
#' @param dep_variable Numeric vector of length T representing the dependent variable (e.g., GDP growth, inflation).
#' @param factors Numeric matrix or data frame of dimension T x r, containing factor estimates.
#' @param h Integer (>= 1). Lag order used in the regression (default = 1)
#' @param edge Numeric value specifying the trimming amount applied to the outermost quantiles. Default is 0.05.
#'
#' @return An object of class \code{fars}, which is a list containing:
#' \describe{
#'   \item{\code{models}}{List of five S3 \code{quantreg::rq} fitted objects named \code{tau_0.xx}.}
#'   \item{\code{h}}{The forecast horizon used.}
#'   \item{\code{levels}}{The vector of quantile levels effectively estimated
#'     (\code{c(edge, 0.25, 0.50, 0.75, 1 - edge)}).}
#'.  \item{\code{periods}}{Integer. The number of fitted periods}
#'   \item{\code{n_factors}}{Integer. The number of factors included in the regression.}
#'   \item{\code{call}}{The matched function call.}
#' }
#' 
#' @examples
#' set.seed(123)
#' T <- 100; r <- 3
#' Y <- rnorm(T)
#' F <- matrix(rnorm(T * r), T, r)      
#' fars_result <- compute_fars(dep_variable = Y, factors = F, h = 1, edge = 0.05)
#' @export
compute_fars <- function(dep_variable, 
                         factors, 
                         h = 1, 
                         edge = 0.05) {
 
  
  # Check parameters
  if (!is.numeric(dep_variable)) stop("dep_variable must be numeric.")
  if (is.data.frame(factors)) factors <- as.matrix(factors)
  if (!is.matrix(factors)) stop("factors must be a matrix or data.frame.")
  Tn <- length(dep_variable)
  if (nrow(factors) != Tn) stop("factors must have T rows (same as dep_variable).")
  r <- ncol(factors)
  if (r < 1) stop("factors must have at least one column.")
  if (!is.numeric(h) || h < 1) stop("h must be integer >= 1.")
  if (!is.numeric(edge) || length(edge) != 1 || edge <= 0 || edge >= 0.25) stop("edge must be a number in (0, 0.25)")
  
  # Prepare levels
  levels <- c(edge, 0.25, 0.50, 0.75, 1 - edge)
  
  # Lag variables
  Y <- dep_variable
  LagY <- c(rep(NA_real_, h), dep_variable[1:(Tn - h)])
  LagF <- rbind(
    matrix(NA_real_, nrow = h, ncol = r),
    factors[1:(Tn - h), , drop = FALSE]
  )
  
  dfX <- data.frame(
    LagY = LagY,
    LagF
  )
  colnames(dfX) <- c("LagY", paste0("F", seq_len(r)))
 
  idx <- stats::complete.cases(dfX, Y)
  df <- cbind.data.frame(y = Y[idx], dfX[idx, , drop = FALSE])

  
  message("Running Factor-Augmented Quantile Regressions (FA-QRs)...")
  # Loop through each quantile and compute Qreg
  rq_models <- vector("list", length(levels))
  #names(rq_models) <- paste0("tau_", format(levels, nsmall = 2))
  #names(rq_models) <- levels
  
  for (i in seq_along(levels)) {
    tau_i <- levels[i]
    rq_models[[i]] <- quantreg::rq(y ~ ., tau = tau_i, data = df)
  }

  
  # Store result
  result <- list(
    models = rq_models,
    periods = nrow(df),
    n_factors = r,
    h = h,
    levels = levels,
    call = match.call()
  )
  
  
  class(result) <- "fars"
  message("Completed")
  
  return(result)

}





