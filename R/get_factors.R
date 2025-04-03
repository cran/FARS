#' Extract Factors at a Given Hierarchical Level
#'
#' @keywords internal
#' 
get_factors <- function(Factor_list, combination, level) {

  matched <- list()

  for (key in names(Factor_list)) {
    key_elements <- as.integer(strsplit(key, "-")[[1]])

    # Check if this key corresponds to the requested level and includes all in combination
    if (length(key_elements) == level && all(combination %in% key_elements)) {
      matched[[length(matched) + 1]] <- Factor_list[[key]]
    }
  }

  # Combine matched factors if any found
  if (length(matched) > 0) {
    return(do.call(cbind, matched))
  } else {
    return(NULL)
  }
}
