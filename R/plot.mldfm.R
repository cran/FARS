#' Plot method for MLDFM object
#'
#' Dispatches to specific plot functions for factors, loadings, or residuals.
#'
#' @param x An object of class \code{mldfm}.
#' @param which What to plot: one of \code{"factors"} (default), \code{"loadings"}, or \code{"residuals"}.
#' @param dates Optional vector of dates (as \code{Date} or \code{zoo::yearqtr}) to use for the x-axis. If not provided, a simple index (1:N) is used.
#' @param var_names Optional vector of variable names to label loadings and residual axis.
#' @param \dots Additional arguments (ignored)
#' 
#' @return No return value. Called for plots generation.
#' 
#' @method plot mldfm
#' @export
plot.mldfm <- function(x, which = "factors", dates = NULL, var_names = NULL, ...) {
  
 
  which <- match.arg(tolower(which), c("factors", "loadings", "residuals"))

  switch(which,
         "factors"   = plot_factors.mldfm(x, dates = dates, ...),
         "loadings"  = plot_loadings.mldfm(x, var_names = var_names, ...),
         "residuals" = plot_residuals.mldfm(x, var_names = var_names, ...)
  )
}
