#' @title Compute Subsample of Data by Block
#' 
#' @description Selects a random subset of columns within each block of the input data matrix.
#' 
#' @param data A numeric matrix or data frame containing the original data. 
#' @param block_ind Integer vector specifying the end indices of each block. If \code{NULL}, assumes a single block spanning all columns.
#' @param n Integer specifying the number of blocks.
#' @param sample_size Numeric between 0 and 1 specifying the proportion of columns to sample within each block. Defaults to 1 (all columns).
#' @param seed Optional integer. Seed for reproducibility of the column sampling. If \code{NULL}, sampling is random.
#'
#'@return A list with two elements:
#' \describe{
#'   \item{sample_data}{A numeric matrix combining the sampled columns from all blocks.}
#'   \item{sample_block_ind}{An integer vector containing the indices corresponding to the sampled data.}
#' }
#' 
#' @keywords internal
compute_subsample <- function(data, block_ind, n,sample_size = 1, seed = NULL) {
  
  # Block index in case there is just one block
  if (is.null(block_ind)) {
    block_ind <- ncol(data)
  }
  
  # Set seed once to ensure reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  new_data <- list()
  new_block_ind <- numeric(n)  #block indices for the sample data
  
  for (i in seq_len(n)) {
    # Define the start and end indices for each block 
    start_idx <- ifelse(i == 1, 1, block_ind[i-1] + 1)
    end_idx <- block_ind[i]
    
    # Extract the current block 
    block_data <- data[, start_idx:end_idx, drop = FALSE]
    
    # Randomly select the columns from the current block
    n_vars <- ncol(block_data)
    selected_cols <- sample(n_vars, size = round(n_vars*sample_size,0)
                            ,replace =FALSE ,prob = NULL)
    
    # Store the reduced block data
    new_data[[i]] <- block_data[, selected_cols,drop = FALSE]
    
    # Update the new block index
    if (i == 1) {
      new_block_ind[i] <- ncol(new_data[[i]])
    } else {
      new_block_ind[i] <- new_block_ind[i-1] + ncol(new_data[[i]])
    }
  }
  
  # Combine all the new block data into a single dataframe
  final_data <- do.call(cbind, new_data)
  
  return(list(sample_data = final_data, sample_block_ind = new_block_ind))
}