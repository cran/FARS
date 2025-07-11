#' Quantile Regression with Stressed Scenario Projection
#'
#' Estimates quantile regressions of a dependent variable on dynamic factors.
#'
#' @import quantreg
#' @importFrom stats as.formula
#' 
#' @keywords internal
#' 
q_reg <- function(dep_variable, factors, Stressed_Factors = NULL,  h=1,  QTAU=0.05) {
  
  
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
  fit_q <- rq(formula, tau = QTAU, data = reg_data)
  summary_fit <- summary(fit_q, se = "ker",covariance=TRUE)
  coefficients <- summary_fit$coefficients[, 1]
  pvalues <- summary_fit$coefficients[, 4] 
  std_errors <- summary_fit$coefficients[, 2]
  
  
 
  # Predict quantile with estimated factors
  Pred_q <- coefficients[1] + coefficients[2] * Y + as.numeric(factors %*% coefficients[3:(2 + r)])
    
  
  # Return here if strssed quantiles are not needed
  if (is.null(Stressed_Factors)) {
    return(list(Pred_q = Pred_q, Coeff = coefficients, Pvalues = pvalues, StdError = std_errors))
  }
  
  
  # Predict with stressed factors (if provided)
  Stressed_Pred_q <- coefficients[1] + coefficients[2] * Y + as.numeric(Stressed_Factors %*% coefficients[3:(2 + r)])
  
  # return
  return(list(Pred_q = Pred_q, Coeff = coefficients, Pvalues = pvalues, 
              StdError = std_errors, Stressed_Pred_q = Stressed_Pred_q))
  
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
