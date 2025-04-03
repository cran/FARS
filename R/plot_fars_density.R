#' Plot method for fars_density objects
#'
#' Plots the evolution of the estimated density over time as a 3D surface.
#'
#' @param x An object of class \code{fars_density}.
#' @param time_index Optional vector for the time axis (default is 1:nrow).
#' @param ... Additional arguments passed to the plot function. (ignored)
#'
#' @return An interactive plot of class \code{plotly}. 
#' 
#' @import ggplot2
#' 
#' @method plot fars_density
#' @export
plot.fars_density <- function(x, time_index = NULL, ...) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for 3D plotting. Please install it via install.packages('plotly').")
  }
  
  # Extract components
  density_matrix <- x$density
  x_vals <- x$x_vals
  n_time <- nrow(density_matrix)
  n_points <- ncol(density_matrix)
  
  # Time axis
  if (is.null(time_index)) {
    time_index <- seq_len(n_time)
  }
  
  # Create meshgrid
  z_matrix <- density_matrix
  x_axis <- x_vals
  y_axis <- time_index
  
  # Create plot
  plotly::plot_ly(
    x = ~x_axis,
    y = ~y_axis,
    z = ~z_matrix,
    type = "surface",
    colorscale = "Viridis"
  ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "Evaluation points"),
        yaxis = list(title = "Time"),
        zaxis = list(title = "Density"),
        camera = list(eye = list(x = 1.25, y = -1.25, z = 1))
      ),
      title = "Density over Time"
    )
}
