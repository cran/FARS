#' Create the key value pair system to track the factors
#'
#' @keywords internal
#' 
# Function to build the final structure
build_factor_structure <- function(global = 1, local = NULL, middle_layer = NULL, num_blocks = NULL) {
  if (is.null(num_blocks)) {
    num_blocks <- length(local)
  }
  
  Final_list <- list()
  
  # Add global factor
  global_key <- paste0(seq_len(num_blocks), collapse = "-")
  Final_list[[global_key]] <- global
  
  # Add sorted middle-layer factors
  if (!is.null(middle_layer) && length(middle_layer) > 0 && num_blocks > 2) {
    sorted_middle_keys <- names(middle_layer)[order(
      -sapply(strsplit(names(middle_layer), "-"), length),  # longest first
      sapply(strsplit(names(middle_layer), "-"), function(x) paste(sprintf("%02d", as.integer(x)), collapse = ""))
    )]
    for (key in sorted_middle_keys) {
      Final_list[[key]] <- middle_layer[[key]]
    }
  }
  
  # Add local factors
  if (!is.null(local)) {
    for (i in seq_along(local)) {
      if (local[i] > 0) {
        Final_list[[as.character(i)]] <- local[i]
      }
    }
  }
  
  return(Final_list)
}
