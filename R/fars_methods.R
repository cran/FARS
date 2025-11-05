#' @title Generic Function to Extract Quantile Levels
#'
#' @param x An object from which to extract the quantile levels
#' @param ... Additional arguments.
#'
#' @return A vector of quantile levels.
#' @export
get_quantile_levels <- function(x, ...) {
  UseMethod("get_quantile_levels")
}

#' @title Extract Quantile Levels from a \code{fars} Object
#'
#' @description Returns the quantile levels from an object of class \code{fars}.
#'
#' @param x An object of class \code{fars}, typically the result of a computation such as \code{compute_fars}.
#' @param ... Additional arguments (ignored).
#'
#' @return 
#' A vector of quantile levels stored within the \code{fars} object.
#' 
#' @examples
#' fars_result <- compute_fars(dep_variable = rnorm(100), 
#'                             factors = matrix(rnorm(100 * 3), ncol = 3))
#' get_quantile_levels(fars_result)  
#'
#' @export
get_quantile_levels.fars <- function(x, ...) {
  return(x$levels)
}



#' @title Generic Function to Extract a Specific \code{rq} Object
#'
#' @param x An object from which to extract a \code{rq} Object
#' @param tau Numeric scalar in (0, 1) indicating the desired quantile.
#' @param ... Additional arguments.
#'
#' @return  A single \code{rq} object.
#' @export
get_rq_model <- function(x, tau, ...) {
  UseMethod("get_rq_model")
}

#' @title Extract a Specific \code{rq} Object from a \code{fars} Object
#'
#' @description Returns the \code{rq} fit stored inside a \code{fars} object at the requested quantile.
#'
#' @param x An object of class \code{mldfm_subsample}.
#' @param tau Numeric scalar in (0, 1) indicating the desired quantile.
#' @param ... Additional arguments (ignored).
#'
#' @return A single \code{rq} object.
#' 
#' @examples
#' fars_result <- compute_fars(dep_variable = rnorm(100), 
#'                             factors = matrix(rnorm(100 * 3), ncol = 3))
#' get_rq_model(fars_result, tau = 0.05)  
#' 
#' @export
get_rq_model.fars <- function(x, tau, ...) {
  
  if (!is.numeric(tau) || length(tau) != 1 || tau <= 0 || tau >= 1) {
    stop("tau must be a numeric scalar in (0, 1).")
  }
  
  levels <- get_quantile_levels(x)
  if (tau %in% levels) {
    idx <- which(levels == tau)
    return(x$models[[idx]])
  } else {
    stop("Requested tau not found in fars object levels.")
  }
  
}


#' @title Fitted Values for \code{fars} Object
#'
#' @description Returns a matrix of fitted values from all quantile regressions stored in a \code{fars} object.
#'
#' @param object An object of class \code{fars}.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric matrix with one column per quantile level and as many rows as observations used in the fit.
#'
#' @examples
#' fars_result <- compute_fars(dep_variable = rnorm(100), 
#'                             factors = matrix(rnorm(100 * 3), ncol = 3))
#' fitted(fars_result)
#' 
#' @method fitted fars
#' @export
fitted.fars <- function(object, ...) {
  levels <- get_quantile_levels(object)
  models <- object$models
  
  fitted_mat <- sapply(models, fitted)
  colnames(fitted_mat) <- formatC(levels, format = "f", digits = 2)
  
  fitted_mat
}

#' @title Residuals for \code{fars} Object
#'
#' @description Returns a matrix of residuals from all quantile regressions stored in a \code{fars} object.
#'
#' @param object An object of class \code{fars}.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric matrix with one column per quantile level and as many rows as observations used in the fit.
#'
#' @examples
#' fars_result <- compute_fars(dep_variable = rnorm(100), 
#'                             factors = matrix(rnorm(100 * 3), ncol = 3))
#' residuals(fars_result)
#'
#' @method residuals fars
#' @export
residuals.fars <- function(object, ...) {
  levels <- get_quantile_levels(object)
  models <- object$models
  
  residuals_mat <- sapply(models, residuals)
  colnames(residuals_mat) <- formatC(levels, format = "f", digits = 2)
  
  residuals_mat
}


#' @title Coefficients for \code{fars} Object
#'
#' @description Returns a matrix of estimated coefficients from all quantile regressions stored in a \code{fars} object.
#'
#' @param object An object of class \code{fars}.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric matrix with one column per quantile level and one row per coefficient.
#' Column names correspond to quantile levels (e.g. \code{0.05}, \code{0.25}, ...),
#' and row names to coefficient names.
#'
#' @examples
#' fars_result <- compute_fars(dep_variable = rnorm(100), 
#'                             factors = matrix(rnorm(100 * 3), ncol = 3))
#' coef(fars_result)
#'
#' @method coef fars
#' @export
coef.fars <- function(object, ...) {
  levels <- get_quantile_levels(object)
  models <- object$models
  
  # Extract coefficients from each rq model
  coef_mat <- sapply(models, coef)
  
  # Add column names for quantile levels
  colnames(coef_mat) <- formatC(levels, format = "f", digits = 2)
  
  round(coef_mat,3)
}

#' @title Predict Method for \code{fars} Object
#'
#' @description
#' Computes predictions from all quantile regressions in a \code{fars} object.
#' \code{newdata} must contain (in this order) the lagged dependent variable column
#' followed by the factor columns. Column names are generated internally as
#' \code{LagY}, \code{F1}, \code{F2}, ..., \code{Fr}.
#'
#' @param object An object of class \code{fars}.
#' @param newdata A matrix or data frame with one column for the lagged dependent variable
#'   and \code{r} columns for the factors (same \code{r} used in \code{compute_fars()}).
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric matrix with one column per quantile level and one row per observation in \code{newdata}.
#'
#' @method predict fars
#' @export
predict.fars <- function(object, newdata, ...) {
  models <- object$models
  levels <- get_quantile_levels(object)
  
  # number of factors 
  r <- length(coef(models[[1]])) - 2L
  
  # assign expected names: LagY, F1..Fr
  if (is.matrix(newdata)) newdata <- as.data.frame(newdata)
  colnames(newdata) <- c("LagY", paste0("F", seq_len(r)))
  
  # predict for each rq model and bind as matrix
  pred_mat <- sapply(models, function(m) predict(m, newdata = newdata))
  pred_mat <- as.matrix(pred_mat)
  colnames(pred_mat) <- formatC(levels, format = "f", digits = 2)
  pred_mat
}

#' @title Log-Likelihoods for \code{fars} Object
#'
#' @description Returns the log-likelihood for each quantile regression stored
#' in a \code{fars} object.
#'
#' @param object An object of class \code{fars}.
#' @param ... Additional arguments (ignored).
#'
#' @return A named numeric vector with one element per quantile level.
#'
#' @examples
#' fars_result <- compute_fars(dep_variable = rnorm(100),
#'                             factors = matrix(rnorm(100 * 3), ncol = 3))
#' logLik(fars_result)
#'
#' @method logLik fars
#' @export
logLik.fars <- function(object, ...) {
  levels <- get_quantile_levels(object)
  models <- object$models
  
  ll_vec <- sapply(models, function(m) as.numeric(logLik(m)))
  names(ll_vec) <- formatC(levels, format = "f", digits = 2)
  
  round(ll_vec, 3)
}


