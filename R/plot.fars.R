#' Plot Method for fars Object
#'
#' @description Generates line plots of forecasted quantiles from a FARS object. 
#' If a stressed scenario is available, it is plotted in a separate panel.
#'
#' @param x An object of class \code{fars}.
#' @param dates Optional vector of dates (as \code{Date} or \code{zoo::yearqtr}) to use for the x-axis. 
#' If not provided, a simple index is used.
#' @param ... Additional arguments (currently ignored).
#'
#' @return No return value. Called for plot generation.
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @method plot fars
#' @export
plot.fars <- function(x, dates = NULL, ...) {
  if (!inherits(x, "fars")) stop("Object must be of class 'fars'.")
  
  # Prepare data
  quantiles <- x$Quantiles
  scenario <- x$Scenario_Quantiles
  levels <- x$Levels
  
  if (is.null(dates)) {
    dates <- 1:nrow(quantiles)
  }
  
  # --- Forecasted Quantiles ---
  df <- as.data.frame(quantiles)
  colnames(df) <- paste0("Q", levels)
  df$Time <- dates
  df_long <- reshape2::melt(df, id.vars = "Time", variable.name = "Quantile", value.name = "Value")
  
  
  Time <- Value <- Quantile <- NULL
  
  p_main <- ggplot(df_long, aes(x = Time, y = Value, color = Quantile)) +
    geom_line(size = 1) +
    labs(title = "Quantiles",
         y = "Predicted Value", x = "Time") +
    theme_minimal()
  
  print(p_main)
  
  # --- Stressed Quantiles (if available) ---
  if (!is.null(scenario)) {
    df_s <- as.data.frame(scenario)
    colnames(df_s) <- paste0("Q", levels)
    df_s$Time <- dates
    df_s_long <- reshape2::melt(df_s, id.vars = "Time", variable.name = "Quantile", value.name = "Value")
    
    p_stress <- ggplot(df_s_long, aes(x = Time, y = Value, color = Quantile)) +
      geom_line(size = 1) +
      labs(title = "Scenario Quantiles",
           y = "Predicted Value", x = "Time") +
      theme_minimal()
    
    print(p_stress)
  }
}
