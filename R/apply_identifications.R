#' @title Apply Identification Constraints to Factors and Loadings
#' 
#' @description Applies identification constraints to factors and loadings.
#' 
#' @param data A numeric matrix or data frame containing the observed time series (T × N).
#' @param num_blocks Integer. The number of blocks.
#' @param ranges A list of vectors with the column indices corresponding to each block.
#' @param r_list A named list specifying the number of factors for each node (from \code{build_factor_structure}).
#' @param current_factors A numeric matrix with the current estimate of all factors (T × total factors).
#' @param factor_list A named list of factors per node (from \code{compute_initial_factors}).
#' @param loadings_list A named list of loadings per node.
#'
#' @return A list containing:
#' \describe{
#'   \item{factor_list}{An updated named list of identified factors.}
#'   \item{final_factors}{A matrix containing identified factors.}
#'   \item{loadings}{A matrix of identified factor loadings.}
#' }
#'
#' @keywords internal
apply_identifications <- function(data, num_blocks, ranges, r_list,
                                  current_factors, factor_list, loadings_list) {
  
 
  num_factors <- sum(unlist(r_list))
  t_obs <- nrow(data)
  final_factors <- matrix(nrow = t_obs, ncol = 0)
  
  
  # --- Step 1: Orthogonalize current factors (Identifiation III) ---
  orthogonal_factors <- orthogonalize_factors(current_factors)
  #orthogonal_factors <- qr.Q(qr(current_factors))
  factor_list <- update_factor_list(factor_list, orthogonal_factors, r_list)
  
  # Recompute loadings based on orthogonal factors
  l_res <- compute_loadings(data, num_blocks, ranges, r_list, factor_list, loadings_list)
  loadings_list <- l_res$loadings_list
  
  
  # Initialize new loadings_matrix
  loadings_matrix <- matrix(0, nrow = num_factors, ncol = ncol(data))
  counter <- 1
  
  # --- Step 2: Identification I and II on Global Factors ---
  global_key <- paste(seq(1, num_blocks), collapse = "-")
  global_factors <- factor_list[[global_key]]
  global_loadings <- loadings_list[[global_key]]
  
  # Apply identifications I and II
  common_component <- global_factors %*% global_loadings
  pca_result <- prcomp(common_component, center = FALSE, scale. = FALSE)
  global_factors_new <- pca_result$x[, 1:ncol(global_factors), drop = FALSE]
  global_factors_new <- scale(global_factors_new, TRUE, TRUE)
  global_loadings_new <- qr.solve(global_factors_new, common_component)

  # Update outputs
  final_factors <- cbind(final_factors, global_factors_new)
  factor_list[[global_key]] <- global_factors_new
  loadings_list[[global_key]] <- global_loadings_new
  combination <- seq(1, num_blocks)
  loadings_matrix[counter:(counter + r_list[[global_key]] - 1), unlist(ranges[combination])] <- global_loadings_new
  counter <- counter + r_list[[global_key]]
  
  # Check identifications
  # print(global_key)
  # check_identification_condition_1(global_factors_new)
  # check_identification_condition_2(t(global_loadings_new))
  
  # --- Step 3: Identification I and II on Lower Level Factors ---
  for (key in names(r_list)) {
    if (key == global_key) next
    
    combination <- as.numeric(unlist(strsplit(key, "-")))
    
    # Get residuals after removing higher-level structure
    residuals <- do.call(cbind, lapply(combination, function(idx) data[, ranges[[idx]]]))
    
    level <- num_blocks
    while (level > length(combination)) {
      upper_factors <- get_level_factors(factor_list, combination, level)
      if(!is.null(upper_factors)){
        ols_result <- beta_ols(upper_factors, residuals)
        residuals <- residuals - upper_factors %*% ols_result
      }
      level <- level - 1
    }
    
    # Extract current node factors
    factors <- factor_list[[key]]
    loadings <- loadings_list[[key]]
    
    # Apply identifications I and II
    common_component <- factors %*% loadings
    pca_result <- prcomp(common_component, center = FALSE, scale. = FALSE)
    factors_new <- pca_result$x[, 1:ncol(factors), drop = FALSE]
    factors_new <- scale(factors_new, TRUE, TRUE)
    loadings_new <- qr.solve(factors_new, common_component)
    
    # Update
    final_factors <- cbind(final_factors, factors_new)
    factor_list[[key]] <- factors_new
    loadings_list[[key]] <- loadings_new
    loadings_matrix[counter:(counter + r_list[[key]] - 1), unlist(ranges[combination])] <- loadings_new
    counter <- counter + r_list[[key]]
    
    # Check identifications
    # print(key)
    # check_identification_condition_1(factors_new)
    # check_identification_condition_2(t(loadings_new))
  }
  
  return(list(
    factor_list = factor_list,
    final_factors = final_factors,
    loadings = loadings_matrix
  ))
}

