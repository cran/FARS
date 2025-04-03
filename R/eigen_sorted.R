#' Eigen Decomposition (Descending Order)
#'
#' Computes the eigenvalues and eigenvectors of a symmetric matrix, returning them in descending order (default behavior of `eigen()`).
#'
#' @keywords internal
#' 
eigen_sorted <- function(matrix_y) {
  # Compute eigenvalues and eigenvectors
  eigen_result <- eigen(matrix_y)
  
  # Extract eigenvalues and eigenvectors (descending order)
  eigenvalues <- eigen_result$values
  eigenvectors <- eigen_result$vectors
  
  # Sort (ascending order) 
  # sorted_indices <- order(eigenvalues)
  # sorted_eigenvalues <- eigenvalues[sorted_indices]
  # sorted_eigenvectors <- eigenvectors[, sorted_indices]
  
  # Return as list
  return(list(eigenvalues = eigenvalues, eigenvectors = eigenvectors))
  #return(list(eigenvalues = sorted_eigenvalues, eigenvectors = sorted_eigenvectors))
}
