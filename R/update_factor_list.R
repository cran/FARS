#' Update Factor List from a given Factor Matrix
#' 
#' @keywords internal
update_factor_list <- function(Factor_list, FinalFactors, r_list) {
  
  col_start <- 1
  
  # Update 
  for (key in names(Factor_list)) {
    
    n_factors <- r_list[[key]]
    col_end <- col_start + n_factors - 1
    
    Factor_list[[key]] <- FinalFactors[, col_start:col_end, drop = FALSE]
    
    col_start <- col_end + 1
  }
  
  return(Factor_list)
}

