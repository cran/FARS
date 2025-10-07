#' @title Eigen Decomposition 
#'
#' @description Computes the eigenvalues and eigenvectors of a symmetric matrix using the base \code{eigen()} function.
#'
#' @param matrix_y A symmetric numeric matrix.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{eigenvalues}{A numeric vector of eigenvalues in descending order.}
#'   \item{eigenvectors}{A matrix whose columns are the eigenvectors corresponding to the eigenvalues.}
#' }
#'
#' @keywords internal
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
