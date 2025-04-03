#' Update Factor List from a given Factor Matrix
#' 
#' @keywords internal
update_factor_list <- function(Factor_list, FinalFactors, r) {
  
  r_index <- 1
  col_start <- 1
  filtered_r <- r[r != 0] # skip node without factors
  
  # Update 
  for (key in names(Factor_list)) {
    
    n_factors <- filtered_r[r_index]
    col_end <- col_start + n_factors - 1
    
    Factor_list[[key]] <- FinalFactors[, col_start:col_end, drop = FALSE]
    
    col_start <- col_end + 1
    r_index <- r_index + 1
  }
  
  return(Factor_list)
}

