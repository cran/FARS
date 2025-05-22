#' Internal helper to compute a column-wise subsample of the data by block
#'
#' @keywords internal
compute_subsample <- function(data, block_ind, n,sample_size = 1, seed = NULL) {
  
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