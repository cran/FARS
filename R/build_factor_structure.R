#' @title Build Factor Structure for Multi-level Dynamic Factor Model
#'
#' @description Constructs a named list defining the factor structure across global, middle-layer, and local levels,
#' 
#' @param global Integer. Number of global factors extracted from the entire dataset.
#' @param local Integer vector of length \code{blocks}. Specifies the number of local factors for each block.
#' @param middle_layer Named list. Each name is a string specifying a group of blocks (e.g., \code{"1-3"} or \code{"2-3"}), and each value is the number of factors to extract.
#' @param num_blocks Integer. Number of blocks into which the data is divided.
#' 
#' @return A named list where each name corresponds to a block or group of blocks (e.g., "1", "1-3"),
#' and the value is the number of factors associated with that node.
#' 
#' @keywords internal
build_factor_structure <- function(global = 1, local = NULL, middle_layer = NULL, num_blocks = NULL) {
  if (is.null(num_blocks)) {
    num_blocks <- length(local)
  }
  
  final_list <- list()
  
  # Add global factor
  global_key <- paste0(seq_len(num_blocks), collapse = "-")
  final_list[[global_key]] <- global
  
  # Add sorted middle-layer factors
  if (!is.null(middle_layer) && length(middle_layer) > 0 && num_blocks > 2) {
    sorted_middle_keys <- names(middle_layer)[order(
      -sapply(strsplit(names(middle_layer), "-"), length),  # longest first
      sapply(strsplit(names(middle_layer), "-"), function(x) paste(sprintf("%02d", as.integer(x)), collapse = ""))
    )]
    for (key in sorted_middle_keys) {
      final_list[[key]] <- middle_layer[[key]]
    }
  }
  
  # Add local factors
  if (!is.null(local)) {
    for (i in seq_along(local)) {
      if (local[i] > 0) {
        final_list[[as.character(i)]] <- local[i]
      }
    }
  }
  
  return(final_list)
}
