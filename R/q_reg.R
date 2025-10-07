#' @title Quantile Regression 
#'
#' @description Estimates quantile regressions of a dependent variable on dynamic factors.
#' Optionally computes projections under stressed scenarios using the provided stressed factors.
#'
#' @param dep_variable A numeric vector representing the dependent variable (e.g., GDP growth, inflation).
#' @param factors A matrix of factor estimates from an MLDFM model, with rows corresponding to time periods and columns corresponding to factors.
#' @param stressed_factors An optional matrix of stressed factors. If provided, the function computes projections under the stressed scenario.
#' @param h Integer representing the forecast horizon (in time steps) for the quantile regression.
#' @param qtau Numeric. The quantile level used in quantile regression.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{pred_q}}{The quantile regression predictions based on the estimated factors.}
#'   \item{\code{coeff}}{The regression coefficients for each variable in the model.}
#'   \item{\code{pvalues}}{The p-values for each regression coefficient.}
#'   \item{\code{stderr}}{The standard errors for each regression coefficient.}
#'   \item{\code{stressed_pred_q}}{The quantile predictions under the stressed scenario, only if \code{stressed_factors} is provided.}
#' }
#'
#' @import quantreg
#' @importFrom stats as.formula
#' 
#' @keywords internal
q_reg <- function(dep_variable, factors, stressed_factors = NULL,  h=1,  qtau=0.05) {
  
  
  t <- nrow(factors)
  r <- ncol(factors)

  
  # Prepare regression data
  Y <- dep_variable
  LagY<-shift(Y,h)
  
  shifted_factors <- matrix(NA, nrow = nrow(factors), ncol = r)
  for (i in 1:r) {
    shifted_factors[,i] <- shift( factors[,i],h)
  }

  # Build regression dataset
  reg_data <- data.frame(Y = Y, LagY = LagY)
  reg_data <- cbind(reg_data, shifted_factors)  
  names(reg_data)[1:2] <- c("Y", "LagY")
  new_factor_names <- paste("factor", 1:r, sep = "")  
  names(reg_data)[3:(2 + r)] <- new_factor_names  
  factor_names_concat <- paste(new_factor_names, collapse = " + ")
  
  # Build formula
  formula <- as.formula(paste("Y ~ LagY", factor_names_concat, sep = " + "))

  # qreg
  fit_q <- rq(formula, tau = qtau, data = reg_data)
  summary_fit <- summary(fit_q, se = "ker",covariance=TRUE)
  coefficients <- summary_fit$coefficients[, 1]
  pvalues <- summary_fit$coefficients[, 4] 
  std_errors <- summary_fit$coefficients[, 2]
  
  
 
  # Predict quantile with estimated factors
  pred_q <- coefficients[1] + coefficients[2] * Y + as.numeric(factors %*% coefficients[3:(2 + r)])
    
  
  # Return here if strssed quantiles are not needed
  if (is.null(stressed_factors)) {
    return(list(pred_q = pred_q, coeff = coefficients, pvalues = pvalues, stderr = std_errors))
  }
  
  
  # Predict with stressed factors (if provided)
  stressed_pred_q <- coefficients[1] + coefficients[2] * Y + as.numeric(stressed_factors %*% coefficients[3:(2 + r)])
  
  # return
  return(list(pred_q = pred_q, coeff = coefficients, pvalues = pvalues, 
              stderr = std_errors, stressed_pred_q = stressed_pred_q))
  
}


#' Shift a time series vector
#' @keywords internal
shift <- function(x, lag) {
  n <- length(x)
  xnew <- rep(NA, n)
  if (lag < 0) {
    xnew[1:(n-abs(lag))] <- x[(abs(lag)+1):n]
  } else if (lag > 0) {
    xnew[(lag+1):n] <- x[1:(n-lag)]
  } else {
    xnew <- x
  }
  return(xnew)
}
