#' @title Update Factor List 
#'
#' @description Updates the list of factors at each hierarchical node by slicing from a matrix containing all final factors.
#'
#' @param factor_list A named list of factors for each node in the hierarchy.
#' @param final_factors A numeric matrix containing all factors (T × total number of factors).
#' @param r_list A named list specifying the number of factors for each node (from \code{build_factor_structure}).
#'
#' @return An updated named list of factors.
#'
#' @keywords internal
update_factor_list <- function(factor_list, final_factors, r_list) {
  
  col_start <- 1
  
  # Update 
  for (key in names(factor_list)) {
    
    n_factors <- r_list[[key]]
    col_end <- col_start + n_factors - 1
    
    factor_list[[key]] <- final_factors[, col_start:col_end, drop = FALSE]
    
    col_start <- col_end + 1
  }
  
  return(factor_list)
}

