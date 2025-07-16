#' Compute Stressed Factors
#'
#' @import quantreg
#' @importFrom stats as.formula
#' 
#' @keywords internal
#' 
compute_stressed_factors <- function(dep_variable, factors, scenario, h, QTAU, min) {
  
  
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
 
  
  # Initialize stressed factor matrix
  stressed_factors <- matrix(NA, nrow = t, ncol = r)
  
  # Loop over time to compute stressed factors
  for (tt in 1:t){
    ellips <- scenario[[tt]]
    pred <- coefficients[1] + coefficients[2] * Y[tt] + ellips %*% coefficients[3:(2 + r)]
    index <- if (min) which.min(pred) else which.max(pred)
    stressed_factors[tt, ] <- ellips[index, ]
    
  }
   
  
  return(stressed_factors)

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
