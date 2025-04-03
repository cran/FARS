
#' Plot Loadings from MLDFM
#'
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @importFrom forcats fct_rev
#'
#'
#' @keywords internal
plot_loadings.mldfm <- function(MLDFM_result, var_names = NULL, ...) {
  
  
  Factors <- MLDFM_result$Factors
  Lambda <- MLDFM_result$Lambda
  Residuals <- MLDFM_result$Residuals
  
  # MSE CI
  t<-nrow(Residuals)
  N<-ncol(Residuals)
  
  # Create loadings data frame 
  loadings <- as.data.frame(Lambda)
  
  
  # Extract Factors names
  keys <- names(MLDFM_result$Factors_list)
  values <- unlist(MLDFM_result$Factors_list) 
  transformed_keys <- c()
  
  for (i in seq_along(keys)) {
    
    clean_key <- paste0("F", gsub("-", "", keys[i]))
    if(values[i]>1){
      repeated_keys <- paste0(clean_key,"n",seq_len(values[i]))
    }else{
      repeated_keys <- clean_key
    }
    transformed_keys <- c(transformed_keys, repeated_keys)
  }
  colnames(loadings)<-transformed_keys
  
  
  # Set variables' names
  if (is.null(var_names)) {
    loadings$Variables <- paste0("Var", 1:nrow(loadings))
  }else{
    loadings$Variables <- var_names
  }
  loadings <- loadings[, c("Variables", setdiff(names(loadings), "Variables"))]
  
  Variables <- Factor <- Loading <- SE <- Loading_lower <- Loading_upper <- NULL
  
  # Pivot
  loadings_long <- loadings %>%
    pivot_longer(cols = -Variables, names_to = "Factor", values_to = "Loading")
  
  # Add standard errors and confidence intervals 
  loadings_se <- apply(Residuals, 2, sd) / sqrt(nrow(Residuals))
  
  loadings_long <- loadings_long %>%
    mutate(SE = rep(loadings_se, times = length(unique(Factor)))) %>%
    mutate(Loading_lower = Loading - 1.96 * SE,
           Loading_upper = Loading + 1.96 * SE)
  
  
  
  
  # Create the list of plots
  plot_list <- list()
  unique_factors <- unique(loadings_long$Factor)  
  
  # Define axis limits
  y_min <- -1
  y_max <- 1
  
  

  for (factor_name in unique_factors) {
    data_plot <- loadings_long
    
    data_plot <- data_plot %>%
      filter(Factor == factor_name & Loading != 0) %>%
      mutate(Variables = factor(Variables, levels = unique(Variables)))  
    
    p <- ggplot(data = data_plot, aes(x = fct_rev(Variables), y = Loading)) +
      geom_bar(stat = "identity", alpha = 0.7, aes(fill = "bar")) +
      geom_hline(yintercept = 0, color = "red") +
      geom_errorbar(aes(ymin = Loading_lower, ymax = Loading_upper),
                    width = 0.5, color = "black", alpha = 1, size = 0.2) +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(limits = c(y_min, y_max)) +
      scale_fill_manual(values = c("grey")) +
      ggtitle(factor_name)
    
      plot_list[[length(plot_list) + 1]] <- p
  }
  
  
  
  return(plot_list)
  
  
  
}



