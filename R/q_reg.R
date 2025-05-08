#' Quantile Regression with Stressed Scenario Projection
#'
#' Estimates quantile regressions of a dependent variable on dynamic factors.
#' If a \code{scenario} is provided (e.g., a list of ellipsoids from \code{create_scenario()}),
#' the function computes the minimum (or maximum) projected quantile values under stress.
#'
#' @import quantreg
#' @importFrom stats as.formula
#' 
#' @keywords internal
#' 
q_reg <- function(dep_variable, factors, h=1,  QTAU=0.05,  scenario = NULL, min = TRUE) {
  
  
  t <- nrow(factors)
  r <- ncol(factors)

  
  index_list <- c(
    1308, 2172, 2172, 2748, 2748, 2748, 2026, 1310, 2172, 1884,
    1844, 1844, 2132, 732, 1844, 1844, 1818, 1811, 734, 1844,
    1844, 2132, 1844, 1884, 1884, 1884, 1884, 1884, 1884, 1884,
    1884, 1884, 1738, 1884, 2132, 1884, 1844, 1844, 1844, 782,
    1844, 1844, 1844, 1844, 1844, 1844, 1844, 1844, 1844, 1844,
    1844, 1844, 1844, 1844, 1836, 1844, 1844, 1836, 1844
  )
  
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
  Pred_q <- as.numeric(coefficients[1])
  Pred_q <- Pred_q + as.numeric(coefficients[2]) * Y[] 
  for (i in 1:r) {
    Pred_q <- Pred_q + as.numeric(coefficients[i+2]) * factors[, i]
  }
    
  
  # Return here is scenario quantiles are not needed
  if (is.null(scenario)) {
    return(list(Pred_q = Pred_q, Coeff = coefficients, Pvalues = pvalues, StdError = std_errors))
  }
  
  
  # qreg scenario
  Scenario_Pred_q <- vector(mode = "numeric", length = t)
  
  for (tt in 1:t){
    ellips <- scenario[[tt]]
    points <- nrow(ellips)
    pred <- vector(mode = "numeric", length = points)
    
    for(p in 1:points){
      pred[p] <- as.numeric(coefficients[1])
      pred[p] <- pred[p] + as.numeric(coefficients[2]) * Y[tt] 
      
      for (j in 1:r) {
        pred[p] <- pred[p] + as.numeric(coefficients[j+2]) * ellips[p,j]
      }
    }
    
    
    if(min == TRUE){
      value <- min(pred)
      #index <- which.min(pred)
    }else{
      value <- max(pred)
    }
    
    Scenario_Pred_q[tt] <- value
  }
    
 
  # return
  return(list(Pred_q = Pred_q, Coeff = coefficients, Pvalues = pvalues, 
              StdError = std_errors, Scenario_Pred_q = Scenario_Pred_q))

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
