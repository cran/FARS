#' Plot Residuals from MLDFM
#'
#' @import ggplot2
#' @importFrom stats cor
#'
#' @keywords internal
plot_residuals.mldfm <- function(x, var_names = NULL, ...) {
  Residuals <- x$Residuals
  n_vars <- ncol(Residuals)
  
  # Assign variable names
  country_names <- if (is.null(var_names)) {
    paste0("Var", seq_len(n_vars))
  } else {
    var_names
  }
  
  # Compute Correlation matrix
  corr_matrix <- cor(Residuals)
  rownames(corr_matrix) <- country_names
  colnames(corr_matrix) <- country_names
  
  # Convert to long format
  corr_df <- as.data.frame(as.table(corr_matrix))
  colnames(corr_df) <- c("Country1", "Country2", "Correlation")
  
  Country1 <- Country2 <- Correlation <- NULL # Setting the variables to NULL first
  
  # Plot
  g <- ggplot(corr_df, aes(x = Country1, y = Country2, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 0.1) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1), name = "Correlation") +
    labs(title = "Residuals",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(g)
}
