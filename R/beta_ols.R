#' @title Efficient OLS Estimation
#'
#' @description Computes OLS coefficients using \code{.lm.fit} for computational efficiency.
#' If \code{Y} is a matrix, applies OLS column-wise.
#'
#' @param X A numeric matrix of regressors (T × k).
#' @param Y A numeric matrix or vector of responses (T × N or T × 1).
#'
#' @return A numeric matrix of coefficients.
#'
#' @importFrom stats .lm.fit
#'
#' @keywords internal
beta_ols <- function(X, Y) {
  if (is.vector(Y)) {
    return(matrix(stats::.lm.fit(X, Y)$coefficients, ncol = 1))
  } else if (is.matrix(Y)) {
    return(apply(Y, 2, function(y) stats::.lm.fit(X, y)$coefficients))
  } else {
    stop("Y must be a numeric vector or matrix.")
  }
}
